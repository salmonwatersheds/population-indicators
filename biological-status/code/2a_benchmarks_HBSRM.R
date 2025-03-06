
#'******************************************************************************
#' The goal of the script is to calculate the benchmarks from the HBSE modelling
#' work done in HBSRM.R. 
#' 
#' Files imported:
#' - cuspawnerabundance (from database)
#' - conservationunits_decoder (from database)
#' - REGION_SPECIES_HBSRM_posteriors_priorShift.rds (created in 1a_HBSRM.R)
#' - REGION_SPECIES_SR_matrices.rds (created in 1a_HBSRM.R)
#' 
#' Files produced: 
#' - figures/REGION_SPECIES_CU_benchmark_posteriors.jpeg
#' - output/REGION_SPECIES_benchmarks_summary_HBSRM.csv
#' - output/REGION_SPECIES_biological_status_HBSRM.csv
#'
#' Notes:
#' - a lot of the code originates from:
#' Fraser_VIMI/analysis/fraser-status/HBM and status/1_hbm-sr-model-interp.R
#' 
#'******************************************************************************
#
rm(list = ls())
graphics.off()

# reset the wd to head using the location of the current script
path <- rstudioapi::getActiveDocumentContext()$path
dirhead <- "population-indicators"
path_ahead <- sub(pattern = paste0("\\",dirhead,".*"),replacement = "", x = path)
wd_head <- paste0(path_ahead,dirhead)
setwd(wd_head)

# Now import functions related to directories.
# Note that the script cannot be called again once the directory is set to the 
# subdirectory of the project (unless setwd() is called again).
source("code/functions_set_wd.R")
source("code/functions_general.R")
source("code/colours.R")

# return the name of the directories for the different projects:
subDir_projects <- subDir_projects_fun()

wds_l <- set_working_directories_fun(subDir = subDir_projects$biological_status,
                                     Export_locally = F)
wd_head <- wds_l$wd_head
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS
wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# wd_output <- gsub("/output","/output_NORMAL_DIST",wd_output)

# The datasets to input were outputted by other scripts 
wd_data_input <- paste0(wd_output,"/intermediate")

# Import functions for this specific project
source("Code/functions.R")

# Load packages
library(R2jags)  # Provides wrapper functions to implement Bayesian analysis in JAGS.  ??? needed ?
library(modeest) # Provides estimators of the mode of univariate data or univariate distributions. ??? needed ?

# option to export the figures
print_fig <- T

# Import species names and acronyms
species_acronym_df <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

#' Import the cuspawnerabundance.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' To calculating current spawner abundance for biostatus assessment
fromDatabase <- update_file_csv <- F

cuspawnerabundance <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[2],
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)

#' Import the conservationunits_decoder.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

#------------------------------------------------------------------------------#
# Selection of region(s) and species and last year for current spawner abundance
#------------------------------------------------------------------------------#

# Choosing the region
# BSC: This will have to eventually be automatized and eventually allows for 
# multiple regions to be passed on.
region <- regions_df$Fraser
region <- regions_df$Yukon
region <- regions_df$Haida_Gwaii

# multiple regions:
region <- c(
  regions_df$Columbia,
  regions_df$Transboundary,
  regions_df$VIMI)

region <- regions_df$VIMI

# all the regions
region <- as.character(regions_df[1,])
#region <- region[region != "Columbia"]
# region <- gsub(" ","_",region)
  
# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
cond_sp <- species_acronym_df$species_qualified_simple == "CK" | 
  species_acronym_df$species_qualified_simple == "SE"
cond_sp <- species_acronym_df$species_qualified_simple == "SE"
species <- species_acronym_df$species_name_simple[cond_sp] |> unique()

# For all species
species <- species_acronym_df$species_name_simple |> unique()

#' Last year to calculate current spawner abundance.
#' If NA then the current spawner abundance is calculated considering the range 
#' between the last year with available data (e.g., yr_last) and
#' yr_last - generation length + 1.
#' If not NA (e.g., yr_last = 2021), current spawner abundance is calculated 
#' in the range yr_last : (yr_last - generation length + 1) even if there is no 
#' data available in more recent years.
yearCurrentAbundance <- NA # was 2021

options(warn = 0)  # warnings are stored until the top level function returns (default)

# 
for(i_rg in 1:length(region)){
  
  # i_rg <- 3
  
  if(region[i_rg] == "Vancouver Island & Mainland Inlets"){
    regionName <- "VIMI"
  }else{
    regionName <- gsub(" ","_",region[i_rg])
  }
  
  # Get all the species for which _posteriors_priorShift datasets are available
  files_list <- list.files(wd_data_input)
  files_s <- files_list[grepl(pattern = "_posteriors_priorShift",files_list)]
  files_s <- files_s[grepl(pattern = regionName,files_s)]
  
  # Remove the "cyclic" pattern for now because cyclic CUs have not been integrated to this main workflow yet 
  # cond <- grepl("cyclic",files_s)
  # files_s <- files_s[!cond]
  
  species_acro <- unique(sub("_HBSRM_posteriors_priorShift.*", "", files_s))
  species_acro <- gsub(pattern = paste0(regionName,"_"), replacement = "", x = species_acro)
  
  species_rg <- sapply(X = species_acro,FUN = function(sp){
    cond <- species_acronym_df$species_qualified_simple == sp
    if(sp == "SE_cyclic"){
      cond <- species_acronym_df$species_qualified_simple == "SE"
    }
    return(unique(species_acronym_df$species_name_simple[cond]))
  })
  
  # Print a message for the species for which there is no data
  cond <- species_rg %in% species
  species_rg <- species_rg[cond]
  species_acro <- names(species_rg)
  if(length(species_rg) == 0){
    
    print(paste0("There is no posterior files in ",region[i_rg]," for ",paste(species[cond],collapse = ", ")))
    
  }else{
    
    for(i_sp in 1:length(species_acro)){
      # i_sp <- 3
      
      # Import the HBSRM outputs, i.e., the posterior distributions of:
      # - mu_a and sigma_a: with CU-level intrinsic productivity ai ~ N(mu_a,sigma_a)
      # - with bi the CU-level density dependence parameter bi ~ logN(log(1/Smaxi),sigma_bi), with Smaxi being the max(S) of that CU i
      # in the datasets "ma_a" = "ma_a", "sigma_a" = "sd_a", "sigma_bi" = "sd[i]"
      post <- readRDS(paste0(wd_data_input,"/",regionName,"_",species_acro[i_sp],
                             "_HBSRM_posteriors_priorShift.rds"))
      
      # Import the S and R matrices used for fitting the HBSR model:
      # BSC: the wd here will eventually have to be set to the final repo for the 
      # exported datasets.
      SRm <- readRDS(paste0(wd_data_input,"/",regionName,"_",species_acro[i_sp],
                            "_SR_matrices.rds"))
      
      CUs <- colnames(SRm$R)
      nCUs <-length(CUs)
      
      # Find the species_name for these CUs
      if(species_acro[i_sp] == "SE_cyclic"){
        cond <- species_acronym_df$species_qualified_simple == "SE"
      }else{
        cond <- species_acronym_df$species_qualified_simple == species_acro[i_sp]
      }
      speciesHere <- unique(species_acronym_df$species_name[cond])
      
      # find the corresponding cuid 
      cuids <- sapply(CUs,function(cu){
        cond_cu <- conservationunits_decoder$cu_name_pse == cu
        cond_rg <- conservationunits_decoder$region == region[i_rg]
        cond_sp <- conservationunits_decoder$species_name %in% speciesHere
        val <- conservationunits_decoder$cuid[cond_rg & cond_sp & cond_cu]
        if(length(val) > 1){ # in case of pooled CUs
          val <- conservationunits_decoder$pooledcuid[cond_rg & cond_sp & cond_cu]
          val <- unique(val)
        }
        return(val)
      })
      
      cond_sa_rg_sp <- cuspawnerabundance$region == region[i_rg] &
        cuspawnerabundance$species_name %in% speciesHere
      
      cond_cud_rg_sp <- conservationunits_decoder$region == region[i_rg] &
        conservationunits_decoder$species_name%in% speciesHere

      # nb of chains
      nchains <- length(post) # 6 chains
      # parameter names
      pnames <- colnames(post[[1]])
      
      #-----------------------------------------------------------------------------#
      # Calculate benchmarks
      #-----------------------------------------------------------------------------#
      
      # Unlist different chains of the posterior
      # 6 chains x nb iteration (?) x nb parameters
      post.arr <- array(
        data = NA, 
        dim = c(nchains, nrow(post[[1]]), ncol(post[[1]])), 
        dimnames = list(paste0("chain", 1:length(post)), NULL, pnames))
      
      for(i in 1:length(post)){  # for each chain
        post.arr[i, , ] <- post[[i]]
      }
      
      # Calculate benchmarks for all mcmc draws to account for correlation between a and b
      # nb CUs x nb different parameters (i.e., 5) x nb chains x nb iterations
      SR_bench <- array(
        data = NA,
        dim = c(nCUs, 5, length(post), nrow(post[[1]])),
        dimnames = list(CUs, 
                        c("a", "b", "sig", "Smsy", "Sgen"), 
                        paste0("chain", 1:length(post)), 
                        NULL))
      
      for(i in 1:nCUs){
        if(nCUs == 1){
          SR_bench[i, "a", , ] <- post.arr[, , which(pnames == "a")]      # matrix nb chains x nb mcmc draws --> all the values for that parameter
          SR_bench[i, "b", , ] <- post.arr[, , which(pnames == "b")]
          SR_bench[i, "sig", , ] <- post.arr[, , which(pnames == "sd")]   # sigma_bi
        }else{
          SR_bench[i, "a", , ] <- post.arr[, , which(pnames == paste0("a[", i, "]"))]
          SR_bench[i, "b", , ] <- post.arr[, , which(pnames == paste0("b[", i, "]"))]
          SR_bench[i, "sig", , ] <- post.arr[, , which(pnames == paste0("sd[", i, "]"))]
        }
      }
      
      # Calculate Smsy & Sgen (this takes a few mins...think of vectorizing/parallelizing)
      # Uses 1_functions.R which is different from previous versions by estimating Smsy
      # directly using the methods of Scheuerell (2016).
      for(i in 1:nCUs){
        # i <- 1
        # i <- 3   # issue with Sgen in Fraser CO CU nb 3
        for(j in 1:length(post)){ # for each chain
          # j <- 1
          # Smsy (function can handle vectors)
          SR_bench[i, "Smsy", j, ] <- calcSmsy(a = SR_bench[i, "a", j, ], 
                                               b = SR_bench[i, "b", j, ])
          
          # Sgen (function not currently set up to handle vectors..think of updating this)
          for(k in 1:nrow(post[[1]])){   # for each mcmc draw
            # k <- 1
            SR_bench[i, "Sgen", j, k] <- calcSgen(
              Sgen.hat = 0.5 * SR_bench[i, "Smsy", j, k], 
              theta = c(
                a = SR_bench[i, "a", j, k], 
                b = SR_bench[i, "b", j, k],
                sig = SR_bench[i, "sig", j, k]),
              Smsy = SR_bench[i, "Smsy", j, k])
          }
        }
      }
      
      # median(SR_bench[3,"Sgen",,],na.rm = T)
      
      #------------------------------------------------------------------------------#
      # Plot the benchmark posteriors
      #------------------------------------------------------------------------------#
      
      # data frame that will contain the biological status expressed as probabilities
      # for each of the three levls (i.e., red, amber and green) both with 
      # upper threshold Smsy and 80% of Smsy
      biologicalStatus_region_species_df <- NULL
      
      # data frame that will contain the benchmark central values and CI for the two
      # methods used (median and quantile and HPD)
      benchSummary_region_species_df <- NULL
      
      for(i in 1:nCUs){
        
        # i <- 1
        # i <- which(cuids == 185)
        # i <- which(CUs == "Kitsumkalum")
        
        #----------------------
        #' biological status probability with the average spawner abundance over
        #' the last generation
        #----------------------
        
        CUname <- gsub(pattern = "_",replacement = " ",x = CUs[i])   # DFO Cu name --> not anymore it is pse now and gsub might not be needed anymore
        
        cond_sa_cu <- cuspawnerabundance$cuid ==  cuids[i]
        spawnerAbundance <- cuspawnerabundance$estimated_count[cond_sa_cu]
        names(spawnerAbundance) <- cuspawnerabundance$year[cond_sa_cu]
        
        cond_cud_cu <- conservationunits_decoder$cuid == cuids[i]

        if(!any(cond_cud_cu)){
          print("This CUS is not found in conservationunits_decoder: BREAK")
          print(paste(region[i_rg],species[i_sp],CUs[i_cu]))
          cat("\n")
          break
        }else if(sum(cond_cud_cu) > 1){
          print("There are multiple CUs with that name who don't have the same pooledcuid, TO CHECK BREAK")
          break
        }
        
        # keep track of the different versions of the CU names
        cu_name_dfo <- conservationunits_decoder$cu_name_dfo[cond_cud_cu]
        
        CU_genLength <- conservationunits_decoder$gen_length[cond_cud_cu]
        CU_genLength_available <- TRUE
        
        if(is.na(CU_genLength)){
          # find the mean over the CUs of the same species: NOT USED
          # conservationunits_decoder_sp <- conservationunits_decoder[conservationunits_decoder$species_abbr %in% speciesName,]
          # CU_genLength <- round(mean(conservationunits_decoder_sp$gen_length))
          
          # From Tech-Report: 
          #' "Where CU-specific data on age-at-return are unavailable, we assume 
          #' generation lengths of 
          #' - 5 years for Chinook CUs, 
          #' - 4 years for coho CUs, 
          #' - 4 years for chum CUs, 
          #' - 4 years for sockeye CUs"   --> BUT 5 years for the Cus in the northen region
          #' - Pink salmon have a consistent 2-year age-at-return and because 
          #' even- and odd-year lineages are considered separate CUs, the most 
          #' recent spawner abundance is simply the most recent year’s estimated 
          #' spawner abundance for this species
          cond <- generationLengthEstiamte_df$species %in% speciesName # generationLengthEstiamte_df is created in functions_general.R
          CU_genLength <- generationLengthEstiamte_df$genLength[cond]
          CU_genLength_available <- FALSE
          print(paste("No generation length for:",region[i_rg],species[i_sp],CUname))
        }
        
        # Calculate current spawner abundance:
        # yearCurrentAbundance should be NA so it is calculated from the most 
        # recent year with data
        csa_df <- current_spawner_abundance_fun(cuids = cuids[i], 
                                                cuspawnerabundance = cuspawnerabundance, 
                                                yearCurrentAbundance = yearCurrentAbundance, 
                                                CU_genLength = CU_genLength)
        
        yrInitial <- csa_df$yr_withData_start
        yrFinal <-  csa_df$yr_withData_end
        currentSpawnerData_available <- csa_df$curr_spw_available
        spawnerAbundance_lastGen_m <- csa_df$curr_spw_abun
        spawnerAbundance_lastGen_dataPointNb <- csa_df$dataPointNb
        currentSpawnerData_availableRecentEnough <- csa_df$curr_spw_availableRecentEnough
        
        # determine the number of time this CUs fall under the Red, Amber and Green 
        # status over all the simulations
        if(currentSpawnerData_available & currentSpawnerData_availableRecentEnough){
          
          status_Smsy <- status_Smsy80 <- c()
          for(j in 1:length(post)){ # for each chain
            # j <- 1
            for(k in 1:nrow(post[[1]])){   # for each mcmc draw
              # k <- 1
              LB_Sgen <- SR_bench[i, "Sgen", j, k]    # i corresponds to the CU
              UB_Smsy <- SR_bench[i, "Smsy", j, k]
              UB_Smsy80 <- UB_Smsy * .8
              
              if(!is.na(LB_Sgen) & !is.na(UB_Smsy)){
                if(spawnerAbundance_lastGen_m <= LB_Sgen){
                  status_Smsy <- c(status_Smsy,'red')
                  status_Smsy80 <- c(status_Smsy80,"red")
                }else if(spawnerAbundance_lastGen_m <= UB_Smsy80){
                  status_Smsy <- c(status_Smsy,'amber')
                  status_Smsy80 <- c(status_Smsy80,"amber")
                }else if(spawnerAbundance_lastGen_m <= UB_Smsy){
                  status_Smsy <- c(status_Smsy,'amber')
                  status_Smsy80 <- c(status_Smsy80,"green")
                }else{
                  status_Smsy <- c(status_Smsy,'green')
                  status_Smsy80 <- c(status_Smsy80,"green")
                }
              }else{
                status_Smsy <- c(status_Smsy,NA)
                status_Smsy80 <- c(status_Smsy80,NA)
              }
            }
          }
          status_Smsy <- status_Smsy[!is.na(status_Smsy)]
          status_Smsy80 <- status_Smsy80[!is.na(status_Smsy80)]
          
          status_Smsy_prob <- round(table(factor(status_Smsy,levels = c("red","amber","green")))/length(status_Smsy)*100,4)
          status_Smsy80_prob <- round(table(factor(status_Smsy80,levels = c("red","amber","green")))/length(status_Smsy80)*100,4)
          
          comment <- "Biostatus calculated"
          
        }else{
          
          status_Smsy_prob <- status_Smsy80_prob <- rep(NA,3)
          names(status_Smsy_prob) <- names(status_Smsy80_prob) <- c("red","amber","green")
          
          if(!currentSpawnerData_available){
            comment <- "No estimated_count data in cuspawnerabundance.csv"
          }else if(!currentSpawnerData_availableRecentEnough){
            comment <- paste0("Not recent enough data: last year with data is ",yrFinal," while generation length = ",CU_genLength," years and current year = ",yearCurrentAbundance)
          }
        }
        
        species_name <- conservationunits_decoder$species_name[cond_cud_cu]
        species_qualified <- conservationunits_decoder$species_abbr[cond_cud_cu]
        
        # TEMPORARY
        # simplify species_name as in PSE data meeting December 11 2024
        # To remove eventually when conservationunits_decoder$species_name 
        # changed as well.
        if(grepl("[s|S]ockeye",species_name)){
          
          species_name <- "Sockeye"
          
        }else if(grepl("Pink",species_name)){

          species_name <- "Pink"
          
        }
        
        # Record the probabilities:
        biologicalStatus_df <- data.frame(region = region[i_rg],
                                          species_name = species_name,
                                          species_qualified = species_qualified,
                                          cuid = cuids[i],
                                          cu_name_pse = CUs[i],
                                          cu_name_dfo = cu_name_dfo,
                                          current_spawner_abundance = spawnerAbundance_lastGen_m,
                                          yr_withData_end = yrFinal,
                                          yr_withData_start = yrInitial,
                                          yr_end_imposed = yearCurrentAbundance,
                                          genLength = CU_genLength,
                                          genLength_available = CU_genLength_available,
                                          genLength_dataPointNb = spawnerAbundance_lastGen_dataPointNb,
                                          status_Smsy_red   = status_Smsy_prob["red"],
                                          status_Smsy_amber = status_Smsy_prob["amber"],
                                          status_Smsy_green = status_Smsy_prob["green"],
                                          status_Smsy80_red   = status_Smsy80_prob["red"],
                                          status_Smsy80_amber = status_Smsy80_prob["amber"],
                                          status_Smsy80_green = status_Smsy80_prob["green"],
                                          comment = comment)
        
        
        # add it to biologicalStatus_region_species_df
        if(is.null(biologicalStatus_region_species_df)){
          biologicalStatus_region_species_df <- biologicalStatus_df
        }else{
          biologicalStatus_region_species_df <- rbind(biologicalStatus_region_species_df,
                                                      biologicalStatus_df)
        }
        
        #' Obtain the benchamrks CI
        #' - Median and 95% quantiles
        #' - highest posterior density (HPD) and HPD interval
        benchSummary <- list(
          Sgen = rbind(
            medQuan = medQuan(SR_bench[i, "Sgen", , ]),
            HPD = HPD(SR_bench[i, "Sgen", , ])),
          Smsy = rbind(
            medQuan = medQuan(SR_bench[i, "Smsy", , ]),
            HPD = HPD(SR_bench[i, "Smsy", , ]))
        )
        
        # Report the benchmark values and CI
        benchSummary_df <- data.frame(region = rep(region[i_rg],4),
                                      species_name = rep(species_name,4),
                                      species_qualified = rep(species_qualified,4),
                                      cuid = rep(cuids[i],4),
                                      cu_name_pse = rep(CUs[i],4),
                                      benchmark = c(rep(names(benchSummary)[1],2),
                                                    rep(names(benchSummary)[2],2)),
                                      method = rep(rownames(benchSummary[[1]]),2))
        
        benchSummary_df$m <- c(benchSummary$Sgen[,"m"],benchSummary$Smsy[,"m"])
        benchSummary_df$CI025 <- c(benchSummary$Sgen[,2],benchSummary$Smsy[,2])
        benchSummary_df$CI975 <- c(benchSummary$Sgen[,3],benchSummary$Smsy[,3])
        benchSummary_df$comment = comment
        
        # add it to benchSummary_region_species_df
        if(is.null(benchSummary_region_species_df)){
          benchSummary_region_species_df <- benchSummary_df
        }else{
          benchSummary_region_species_df <- rbind(benchSummary_region_species_df,
                                                  benchSummary_df)
        }
        
        #----------------------
        # Plot
        #----------------------
        
        if(print_fig){
          CUhere <- gsub(pattern = "/",'-',CUs[i])  # in case "/" is in the CU's name
          CUhere <- gsub(pattern = "\\(","",CUhere)
          CUhere <- gsub(pattern = "\\)","",CUhere)
          
          if(nchar(CUhere) > 25){   # the figure can't print if the number of characters is too large
            CUhere <- substr(x = CUhere,start =  1,stop = 25) 
          }
          
          pathFile <- paste0(wd_figures,"/",regionName,"_",species_acro[i_sp],"_",CUhere,
                             "_benchmark_posteriors.jpeg")
          
          # pdf(file = pathFile, width = 8.5, height = 11)
          # pdf(file = pathFile, width = 15, height = 7.5)
          jpeg(file = pathFile, width = 30, height = 14, units = "cm", res = 300)
        }
        
        layout(matrix(data = 1:2, nrow = 1))
        par(mar = c(4.5,4.5,4,2))
        
        # max spawners for that CU for plotting purposes
        maxS <- max(SRm$S[, i], na.rm = TRUE) * 1.5
        maxR <- max(SRm$R[, i], na.rm = TRUE)
        
        #----------------------
        # Plot SR relationship
        #----------------------
        # Create dummy vector of spawner abundance
        dummy_spawners <- seq(0, maxS, length.out = 200)
        
        # Calculate posterior prediction for recruits based on a and b
        dummy_recruits_full <- array(NA, dim = c(200, nchains*nrow(post[[1]])))
        for(j in 1:200){
          # R = S*exp(a - b*S)
          dummy_recruits_full[j, ] <- dummy_spawners[j] * exp(c(SR_bench[i, "a", , ]) - 
                                                                c(c(SR_bench[i, "b", , ])) * 
                                                                dummy_spawners[j])
        }
        
        # Summarize predicted recruits using original method and HPD for comparison
        dummy_recruits <- list(
          medQuan = apply(dummy_recruits_full, 1, medQuan),
          HPD = apply(dummy_recruits_full, 1, HPD, xmax = maxS)
        )
        
        # Plot SR relationship with predicted recruits and 95% CI
        plot(x = dummy_spawners, y = dummy_recruits[[1]][1,], type = "n", 
             xlim = c(0, maxS/1.5), ylim = c(0, maxR), 
             xlab = "Spawners", ylab = "Recruits", bty = "l")
        mtext(side = 3, adj = 0, line = 2.5, paste(CUs[i],cuids[i],sep = " - "))
        mtext(side = 3, adj = 0, line = 1.2, 
              paste0("(",paste(range(row.names(SRm$S)[!is.na(SRm$S[,i])]),collapse = " - "),")"))
        
        points(SRm$S[, i], SRm$R[, i])
        for(j in 1:2){
          lines(dummy_spawners, dummy_recruits[[j]][1, ], lty = c(2,1)[j], lwd = 2)
          lines(dummy_spawners, dummy_recruits[[j]][2, ], lty = c(2,1)[j])
          lines(dummy_spawners, dummy_recruits[[j]][3, ], lty = c(2,1)[j])
        }
        
        #----------------------
        # Plot benchmarks
        #----------------------
        
        # u <- par('usr')
        for(j in 1:2){    # median, HPD
          # j <- 1
          for(k in 1:2){  # Sgen, Smsy
            # k <- 1
            abline(v = benchSummary[[k]][j, 1], lty = c(2,1)[j], 
                   col = status_cols[c('red', 'green')[k]])
            # abline(v = benchSummary[[k]][j, 2], lty = c(2,1)[j], col = statusCols[c('r', 'g')[k]])
            # abline(v = benchSummary[[k]][j, 3], lty = c(2,1)[j], col = statusCols[c('r', 'g')[k]])
            # y <- u[4] + c(c(0.05, 0.02)[k] + c(0, 0.05)[j])*(u[4]- u[3])
            y <-  maxR * (1 + c(0, 0.02)[j] + c(-0.1, 0)[k])
            
            points(x = benchSummary[[k]][j, 1],y = y, 
                   col = status_cols[c('red', 'green')[k]], pch = 19, xpd = NA)
            segments(x0 = benchSummary[[k]][j, 2], x1 = benchSummary[[k]][j, 3], 
                     y0 = y, y1 = y, 
                     col = status_cols[c('red', 'green')[k]], 
                     lty = c(2,1)[j], lwd = 2, xpd = NA)
          }
        }
        
        legend("topright", lty = c(2,1, NA, NA), pch = c(NA, NA, 19, 19), 
               col = c(1, 1, status_cols['red'], status_cols['green']), 
               legend = c("median and quantiles", "HPD", "Sgen", "Smsy"), 
               # bg = "white",
               bty = "n")
        
        # Histograms of posterior mcmc draws:
        Sgen <- SR_bench[i, "Sgen", , ]
        Sgen <- Sgen[which(Sgen <= maxS)]
        
        Smsy <- SR_bench[i, "Smsy", , ]
        Smsy <- Smsy[which(Smsy <= maxS)]
        
        dens <- list(density(SR_bench[i, "Sgen", , ], from = 0, to = maxS/1.5, na.rm = T), # BSC: I had to add na.rm = T
                     density(SR_bench[i, "Smsy", , ], from = 0, to = maxS/1.5, na.rm = T))
        
        par(mar = c(4.5,4.5,4,.5))
        
        # If there are Sgen and Smsy values < Smax:
        if(length(Sgen) > 0 & length(Smsy) > 0){
          
          h1 <- hist(x = Sgen, col = paste0(status_cols['red'], 50), border = NA,
                     breaks = seq(0, maxS, maxS/50), xlim = c(0, maxS), main = "", 
                     freq = FALSE, xlab = "Benchmarks")
          h2 <- hist(x = Smsy, col = paste0(status_cols['green'], 50), border = NA, 
                     breaks = seq(0, maxS, maxS/50), add = TRUE, 
                     freq = FALSE)
          
          # Plot density lines and benchmark estimates
          u <- par('usr')
          for(k in 1:2){   # median, HPD
            lines(dens[[k]], lwd = 2, col = status_cols[c('red', 'green')[k]])
            for(j in 1:2){ # Sgen, Smsy
              abline(v = benchSummary[[k]][j, 1], col = status_cols[c('red', 'green')[k]], 
                     lty = c(2,1)[j])
              # abline(v = benchSummary[[k]][j, 2:3], col = statusCols[c('r', 'g')[k]], lty = c(2,1)[j])
              # y <- u[4] + c(c(0.05, 0.02)[k] + c(0, 0.05)[j])*(u[4]- u[3])
              y <- u[4] + c(c(-0.2,-0.1)[k] + c(0, 0.02)[j])*(u[4]- u[3])
              points(x = benchSummary[[k]][j, 1],y = y, col = status_cols[c('red', 'green')[k]],
                     pch = 19, xpd = NA)
              segments(x0 = benchSummary[[k]][j, 2], x1 = benchSummary[[k]][j, 3], 
                       y0 = y, y1 = y, 
                       col = status_cols[c('red', 'green')[k]], lty = c(2,1)[j], lwd = 2, xpd = NA)
            }
          }
          
          # plot current spawner abundance
          abline(v = spawnerAbundance_lastGen_m, y0 = y, y1 = y, col = "black", lwd = 2)
          mtext(side = 3, adj = 0, line = 1.5, 
                paste0("(Curr. spawner abund. ",yrInitial,"-",yrFinal,")"))
          
          
        }else{
          
          commentHere <- paste0("maxS = ",maxS,
                                "; min(Sgen) = ",round(min(SR_bench[i, "Sgen",,]),1),
                                "; min(Smsy) = ",round(min(SR_bench[i, "Smsy",,]),1))
          
          plot.new()
          legend("center",c("ERROR",
                            paste0("maxS = ",maxS),
                            paste0("min(Sgen) = ",round(min(SR_bench[i, "Sgen",,]),1)),
                            paste0("min(Smsy) = ",round(min(SR_bench[i, "Smsy",,]),1))))
          
          if(biologicalStatus_df$comment == ""){
            biologicalStatus_df$comment <- commentHere
          }else{
            biologicalStatus_df$comment <- paste(biologicalStatus_df$comment,
                                                 commentHere, sep = " ; ERROR: ")
          }
        }
        # # Add legend
        # plot(1,1,"n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        # legend("top", fill = c(statusCols['r'], statusCols['g']), legend = c("Sgen", "Smsy"), bty = "n", border = NA, cex = 1.5)
        # legend("center", lty = c(2,1), title = "Posterior summary", legend = c("median", "HPD"), bty = "n", border = NA, cex = 1.5)
        
        # Add spawner points along bottom for reference
        #points(x = d$Esc[d$CU == keepCU[i]], y = rep(0, length(which(d$CU == keepCU[i]))))
        
        if(print_fig){
          dev.off()
        }

      } # end of for each CU
      
      print(paste0("*** ",region[i_rg],"_",species_acro[i_sp]," done ***"))
      
      write.csv(x = benchSummary_region_species_df, 
                file = paste0(wd_output,"/intermediate/",regionName,"_",species_acro[i_sp],"_benchmarks_summary_HBSRM.csv"),
                row.names = F,)
      
      write.csv(x = biologicalStatus_region_species_df, 
                file = paste0(wd_output,"/intermediate/",regionName,"_",species_acro[i_sp],"_biological_status_HBSRM.csv"),
                row.names = F)

    } # end of for each species
  }
} # end of for each region


