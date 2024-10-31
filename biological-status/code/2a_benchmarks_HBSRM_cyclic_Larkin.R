
#'******************************************************************************
#' The goal of the script is to calculate the benchmarks from the HBSE modelling
#' work done in HBSRM.R. 
#' 
#' Files imported:
#' - cuspawnerabundance (from database)
#' - conservationunits_decoder (from database)
#' - REGION_SPECIES_cyclic_Larkin_DIC_HBSRM_posteriors_priorShift.rds (created in 1a_HBSRM.R)
#' - REGION_SPECIES_SR_matrices.rds (created in 1a_HBSRM.R)
#' - 
#' 
#' Files produced: 
#' - figures/REGION_SPECIES_CU_cuid_Larkin_benchmark_posteriors.jpeg
#' - output/REGION_SPECIES_cyclic_Larkin_benchmarks_summary_HBSRM.csv
#' - output/REGION_SPECIES_cyclic_Larkin_biological_status_HBSRM.csv
#' - output/REGION_SPECIES_cyclic_Larkin_alphas_HBSRM.csv
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
wd_data_input <- wd_output

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
species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "CK"],    
             species_acronym_df$species_name[species_acronym_df$species_acro == "SX"])

species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "PK"])

# If we do not specify the species: all the species that have a _SRdata files are 
# returned: 
# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- T

#' Last year to calculate current spawner abundance.
#' If NA then the current spawner abundance is calculated considering the range 
#' between the last year with available data (e.g., yr_last) and
#' yr_last - generation length + 1.
#' If not NA (e.g., yr_last = 2021), current spawner abundance is calculated 
#' in the range yr_last : (yr_last - generation length + 1) even if there is no 
#' data available in more recent years.
yearCurrentAbundance <- NA # was 2021

#'* Select the cyclic CUs *

cond_cycl <- grepl("cyclic",conservationunits_decoder$cu_name_pse)
cuid_cycl <- conservationunits_decoder$cuid[cond_cycl]
conservationunits_decoder[cond_cycl,]

region <- unique(conservationunits_decoder$region[cond_cycl])
species <- unique(conservationunits_decoder$species_name[cond_cycl])

#'* Select the Larkin best model(s) or the Ricker; NOT USED ANY MORE
# model_choice <- c("Ricker","Larkin")[2]

options(warn = 0)  # warnings are stored until the top level function returns (default)

# 
for(i_rg in 1:length(region)){
  
  # i_rg <- 1
  
  region_i <- gsub("_"," ",region[i_rg])
  if(region_i == "Central coast"){    # might not be needed anymore
    region_i <- "Central Coast"
  }
  
  if(region[i_rg] == "Vancouver Island & Mainland Inlets"){
    regionName <- "VIMI"
  }else{
    regionName <- regionName <- gsub(" ","_",region[i_rg])
  }
  
  # Get all the species for which _posteriors_priorShift datasets are available
  if(species_all){
    files_list <- list.files(paste0(wd_data_input,"/intermediate"))
    files_s <- files_list[grepl(pattern = "cyclic_Larkin_DIC_HBSRM_posteriors_priorShift",files_list)]
    files_s <- files_s[grepl(pattern = regionName,files_s)]
    species_acro <- unique(sub("_cyclic_Larkin_DIC_HBSRM_posteriors_priorShift.*", "", files_s))
    species_acro <- gsub(pattern = paste0(regionName,"_"), replacement = "", x = species_acro)
    
    species <- sapply(X = species_acro,FUN = function(sp){
      species_acronym_df$species_name[species_acronym_df$species_acro == sp][1]
    })
    
  }else{
    species_acro <- sapply(X = species,FUN = function(sp){
      species_acronym_df$species_acro[species_acronym_df$species_name == sp][1]
    }) |> unique()
  }
  
  if(length(species) == 0){
    
    print(paste0("*** There is no data in region ",region[i_rg]," ***"))
    
  }else{
    
    for(i_sp in 1:length(species_acro)){
      # i_sp <- 1
      
      cond <- species_acronym_df$species_acro == species_acro[i_sp]
      speciesHere <- species_acronym_df$species_name[cond]
      
      cond <- cuspawnerabundance$region == region_i &
        cuspawnerabundance$species_name %in% speciesHere
      cuspawnerabundance_rg_sp <- cuspawnerabundance[cond,]
      
      cond <- conservationunits_decoder$region == region_i &
        conservationunits_decoder$species_name%in% speciesHere
      conservationunits_decoder_rg_sp <- conservationunits_decoder[cond,]
      
      # Import the HBSRM outputs, i.e., the posterior distributions of:
      # - mu_a and sigma_a: with CU-level intrinsic productivity ai ~ N(mu_a,sigma_a)
      # - with bi the CU-level density dependence parameter bi ~ logN(log(1/i),sigma_bi), with i being the max(S) of that CU i
      # in the datasets "ma_a" = "ma_a", "sigma_a" = "sd_a", "sigma_bi" = "sd[i]"
      post_l <- readRDS(paste0(wd_data_input,"/intermediate/",regionName,"_",species_acro[i_sp],
                             "_cyclic_Larkin_DIC_HBSRM_posteriors_priorShift.rds"))
      
      # There are potentially multiple best models for cyclic CUs, select the one
      # with delta DIC = 0
      #' TODO: what else to do? Compare the benchmark stability between the best model
      #' as in Grant et al 2017 ?
      #' --> combine the distributions of the model parameters? Use DIC-informed weights?
      
      # Import the S and R matrices used for fitting the HBSR model:
      # BSC: the wd here will eventually have to be set to the final repo for the 
      # exported datasets.
      SRm <- readRDS(paste0(wd_data_input,"/intermediate/",regionName,"_",species_acro[i_sp],
                            "_cyclic_SR_matrices.rds"))
      
      # Find the nb of CUs
      # CUs <- read.csv(paste0(wd_data_input,"/",region[i_rg],"_",species[i_sp],"_CUs_names.csv"),
      #                 header = T,stringsAsFactors = F)
      # CUs <- CUs$CU
      CUs <- colnames(SRm$R)
      nCUs <- length(CUs)
      
      # find the corresponding cuid
      cuids <- sapply(X = CUs, function(cu){
        cond <- conservationunits_decoder$cu_name_pse == cu
        return(conservationunits_decoder$cuid[cond])
      })
      
      # data frame that will contain the biological status expressed as probabilities
      # for each of the three levls (i.e., red, amber and green) both with 
      # upper threshold Smsy and 80% of Smsy
      biologicalStatus_rg_sp_df <- NULL
      
      # data frame that will contain the benchmark central values and CI for the two
      # methods used (median and quantile and HPD)
      benchSummary_rg_sp_df <- NULL
      
      alpha_rg_sp_df <- NULL
      
      for(cu in CUs){
        
        # cu <- CUs[2]
        # cu <- CUs[which(cuids == 732)]
        post <- post_l[[cu]]
        
        # Select the model within 5 DIC_delta
        DICs_here <- post$DICs
        DICs_here <- DICs_here[DICs_here$delta <= 5,]
        
        # Add Rc_max and associated convergence conclusion
        convergence_df <- post_l[[cu]]$convergence_prior_DIC
        DICs_here$Rc_max <- sapply(DICs_here$models, function(m){
          cond_m <- convergence_df$model == m
          return(convergence_df$Rc_max[cond_m])
        })
        
        # If more than one choice, remove the model that did not converge:
        # If that remove all the best model then do not do.
        cond <- DICs_here$Rc_max <= 1.1
        if(any(cond)){
          DICs_here <- DICs_here[cond,]
        }
        
        # Select the most complex model among the best models
        
        # If more than one choice, remove the Ricker model if present because it
        # is the simplest model of all
        if(nrow(DICs_here) > 1){
          cond <- DICs_here$models == "Ricker"
          DICs_here <- DICs_here[!cond,]
        }
        
        # If still more than one choice, keep the model with the longest name
        # e.g. Larkin_123 over Larkin_23
        if(nrow(DICs_here) > 1){
          nb_char <- nchar(DICs_here$models)
          cond <- nb_char == max(nb_char)
          DICs_here <- DICs_here[cond,]
        }
        
        # If still more than one choice, keep the model with the smallest parameter
        # indice
        # e.g. Larkin_13 over Larkin_23
        if(nrow(DICs_here) > 1){
          models_here <- DICs_here$models
          models_here <- gsub("Larkin_","",models_here) |> as.numeric()
          cond <- models_here == min(models_here)
          DICs_here <- DICs_here[cond,]
        }
        
        if(nrow(DICs_here) > 1){
          print("still more than one best model BREAK")
          break
        }
        
        m_best <- DICs_here$models[1]
        
        post <- post[[m_best]]
        
        # return the convergence table
        cond <- convergence_df$model == m_best
        convergence_df <- convergence_df[cond,]
        
        # nb of chains
        nchains <- length(post) # 6 chains 
        
        # parameter names
        pnames <- colnames(post[[1]])
        
        cond <- conservationunits_decoder$cu_name_pse == cu
        species_name <- conservationunits_decoder$species_name[cond]
        cuid <- conservationunits_decoder$cuid[cond]
        
        #-----------------------------------------------------------------------------#
        # Calculate benchmarks for each year
        #-----------------------------------------------------------------------------#
        
        # Unlist different chains of the posterior
        # 6 chains x nb iteration (?) x nb parameters
        post.arr <- array(
          data = NA, 
          dim = c(nchains, nrow(post[[1]]), ncol(post[[1]])), # nb chains, nb samples, nb parameters
          dimnames = list(paste0("chain", 1:length(post)), NULL, pnames))
        
        for(chain in 1:length(post)){  # for each chain
          post.arr[chain, , ] <- post[[chain]]
        }
        
        # Calculate benchmarks for all mcmc draws to account for correlation between a and b
        # nb CUs x nb different parameters (i.e., 5) x nb chains x nb iterations
        dimnames <- post[[1]] |> colnames()
        if(grepl("Larkin",m_best)){
          b_parameters <- gsub("Larkin_","",m_best) |> strsplit(split = "") |> unlist()
          b_parameters <- paste0("b",b_parameters)
          b_parameters <- c("b",b_parameters)
        }else{ # Ricker model
          b_parameters <- "b"
        }
        
        # find the minimum and maximum years with data in SRm
        yr_range <- apply(SRm$S,2,function(c){
          r <- rownames(SRm$S)[!is.na(c)] |> as.numeric()
          return(range(r))
        })
        yr_range <- yr_range[,which(cu == CUs)] # simplify
        yr_max <- max(yr_range)
        yr_min <- min(yr_range)
        
        # create array to contain all the values of the parameters and Sgen and Smsy
        # for each chain, samples and last 4 yours
        SR_bench <- array(
          data = NA,
          dim = c(4 + length(b_parameters) + 1,  # a, b, b1, b2, b3, sig, Smsy, Sgen, a_Larkin
                  length(yr_min:yr_max),     # nb years
                  length(post),              # nb chains
                  nrow(post[[1]])),          # nb draws
          dimnames = list(c("a", b_parameters, "sig", "Smsy", "Sgen","a_Larkin"),
                          yr_min:yr_max,
                          paste0("chain", 1:length(post)), 
                          NULL))
        
        for(yr in yr_min:yr_max){
          # yr <- yr_min
          i_yr <- which(yr == yr_min:yr_max)
          
          SR_bench[ "a", i_yr, , ] <- post.arr[, , which(pnames == "a")]      # matrix nb chains x nb mcmc draws --> all the values for that parameter
          SR_bench[ "b", i_yr, , ] <- post.arr[, , which(pnames == "b")]
          SR_bench[ "sig", i_yr, , ] <- post.arr[, , which(pnames == "sd")]   # sigma_bi
          if("b1" %in% b_parameters){
            SR_bench[ "b1", i_yr, , ] <- post.arr[, , which(pnames == "b1")]
          }
          if("b2" %in% b_parameters){
            SR_bench[ "b2", i_yr, , ] <- post.arr[, , which(pnames == "b2")]
          }
          if("b3" %in% b_parameters){
            SR_bench[ "b3", i_yr, , ] <- post.arr[, , which(pnames == "b3")]
          }
        }
        
        # Calculate Smsy & Sgen (this takes a few mins...think of vectorizing/parallelizing)
        # Uses 1_functions.R which is different from previous versions by estimating Smsy
        # directly using the methods of Scheuerell (2016). UPDATE COMMENT
        # Return values for S_t-1, S_t-2 and S_t-3
        S_here <- SRm$S[,cu]
        while(is.na(tail(S_here,1))){   # remove the NAs from the tail
          S_here <- S_here[-length(S_here)]
        }
        while(is.na(head(S_here,1))){   # remove the NAs from the head
          S_here <- S_here[-1]
        }
        
        # for each possible complete cycle from last year with data in S_here
        yr_max_here <- max(names(S_here) |> as.numeric())
        # yr_min_here <- min(names(S_here) |> as.numeric()) + length(b_parameters) - 1  # for the entier time series
        yr_min_here <- yr_max_here - 3   # only consider the last 4-year cycle instead
        
        for(yr in yr_max_here:yr_min_here){
          # yr <- yr_max_here
          i_yr <- which(names(S_here) == yr)
          if("b1" %in% b_parameters){
            St1 <- S_here[i_yr - 1]
          }
          if("b2" %in% b_parameters){
            St2 <- S_here[i_yr - 2]
          }
          if("b3" %in% b_parameters){
            St3 <- S_here[i_yr - 3]
          }
          
          alpha_chain_c <- c()
          for(chain in 1:length(post)){ # for each chain
            # chain <- 1
            # Smsy (function can handle vectors)
            
            # Calculate alpha = a - b1*St-1 - b2*St-2 - b3*St-3 (as in p. 8 of Grant et al. 2020)
            alpha_chain <-  SR_bench[ "a", which(yr == yr_min:yr_max),chain, ] # - b1*St1 - b2*St2 - b3*St3
            if("b1" %in% b_parameters){
              alpha_chain <- alpha_chain - SR_bench[ "b1", which(yr == yr_min:yr_max), chain, ]*St1
            }
            if("b2" %in% b_parameters){
              alpha_chain <- alpha_chain - SR_bench[ "b2", which(yr == yr_min:yr_max), chain, ]*St2
            }
            if("b3" %in% b_parameters){
              alpha_chain <- alpha_chain - SR_bench[ "b3", which(yr == yr_min:yr_max), chain, ]*St3
            }
            
            SR_bench[ "Smsy", which(yr == yr_min:yr_max),chain, ] <- calcSmsy(a = alpha_chain,               # SR_bench[ "a", chain, ], 
                                                                              b = SR_bench[ "b", which(yr == yr_min:yr_max), chain, ])
            
            SR_bench[ "a_Larkin", which(yr == yr_min:yr_max),chain, ] <- alpha_chain
            
            if(all(is.na(SR_bench[ "Smsy", which(yr == yr_min:yr_max),chain, ]))){
              print("Only NA for Smsy")
              break
            }
            
            # Sgen (function not currently set up to handle vectors..think of updating this)
            for(draw in 1:nrow(post[[1]])){   # for each mcmc draw
              # draw <- 1
              Sgen_here <- calcSgen(
                Sgen.hat = 0.5 * SR_bench[ "Smsy", which(yr == yr_min:yr_max),chain, draw], 
                theta = c(
                  a = alpha_chain[draw],                # SR_bench[ "a", chain, draw], 
                  b = SR_bench[ "b", which(yr == yr_min:yr_max), chain, draw],
                  sig = SR_bench[ "sig", which(yr == yr_min:yr_max),chain, draw]),
                Smsy = SR_bench[ "Smsy", which(yr == yr_min:yr_max),chain, draw])
              
              SR_bench[ "Sgen", which(yr == yr_min:yr_max),chain, draw] <- Sgen_here
            }
            
            alpha_chain_c <- c(alpha_chain_c,alpha_chain)
            
            if(all(is.na(SR_bench[ "Sgen", which(yr == yr_min:yr_max),chain, ]))){
              print("Only NA for Sgen")
              break
            }
          } # for each chain chain
          
          alpha_stats <- quantile(x = alpha_chain_c, probs = c(.025, .5, .975))
          a_stats <- quantile(x = SR_bench["a", which(yr == yr_min:yr_max),,], 
                              probs = c(.025, .5, .975))
          
          alpha_df <- data.frame(region = region[i_rg],
                                 species_name = species_name,
                                 cu_name_pse = cu,
                                 cuid = cuid, 
                                 year = yr,
                                 a_median = round(a_stats["50%"],4),
                                 a_CI025 = round(a_stats["2.5%"],5),
                                 a_CI975 = round(a_stats["97.5%"],5),
                                 a_Larkin_median = round(alpha_stats["50%"],4),
                                 a_Larkin_CI025 = round(alpha_stats["2.5%"],5),
                                 a_Larkin_CI975 = round(alpha_stats["97.5%"],5),
                                 model = m_best)
          
          if(is.null(alpha_rg_sp_df)){
            alpha_rg_sp_df <- alpha_df
          }else{
            alpha_rg_sp_df <- rbind(alpha_rg_sp_df,alpha_df)
          }
          
        } # for each year 
        
        #----------------------
        #' biological status probability with the average spawner abundance over
        #' the last generation
        #----------------------
        
        # CUname <- gsub(pattern = "_",replacement = " ",x = CUs[i])   # DFO Cu name --> not anymore it is pse now and gsub might not be needed anymore
        
        cond <- cuspawnerabundance_rg_sp$cu_name_pse == cu
        spawnerAbundance <- cuspawnerabundance_rg_sp$estimated_count[cond]
        names(spawnerAbundance) <- cuspawnerabundance_rg_sp$year[cond]
        
        cond <- conservationunits_decoder_rg_sp$cuid %in% cuid
        conservationunits_decoder_rg_sp_cu <- conservationunits_decoder_rg_sp[cond,]
        
        if(nrow(conservationunits_decoder_rg_sp_cu) == 0){
          print("This CUS is not found in conservationunits_decoder: BREAK")
          print(paste(region[i_rg],species[i_sp],CUs[i]))
          cat("\n")
          break
        }else if(nrow(conservationunits_decoder_rg_sp_cu) > 1){
          if(length(unique(conservationunits_decoder_rg_sp_cu$pooledcuid)) > 1){ # if == 1 there are all the same CUs for PSF
            print("There are multiple CUs with that name who don't have the same pooledcuid, BREAK")
            print(paste(region[i_rg],species[i_sp],CUs[i]))
            print(conservationunits_decoder_rg_sp_cu)
            cat("\n")
            break
          }
          # conservationunits_decoder_rg_sp_cu <- conservationunits_decoder_rg_sp_cu[1,,drop = F]
        }
        
        # keep track of the different versions of the CU names
        # CUname_pse <- conservationunits_decoder_rg_sp_cu$cu_name_pse
        # CUname_dfo <- conservationunits_decoder_rg_sp_cu$cu_name_dfo
        
        CU_genLength <- conservationunits_decoder_rg_sp_cu$gen_length[1]
        CU_genLength_available <- TRUE
        
        #
        # Return values for S_t-1, S_t-2 and S_t-3
        S_here <- SRm$S[,cu]
        while(is.na(tail(S_here,1))){   # remove the NAs from the tail
          S_here <- S_here[-length(S_here)]
        }
        while(is.na(head(S_here,1))){   # remove the NAs from the head
          S_here <- S_here[-1]
        }
        
        # Determine the years over which to calculate the benchmarks:
        yr_max_here <- max(names(S_here) |> as.numeric())
        
        # for each possible complete cycle from last year with data in S_here
        # yr_min_here <- min(names(S_here) |> as.numeric()) + length(b_parameters) - 1
        # alternative simpler: only consider the last cycle:
        yr_min_here <- yr_max_here - 3   # only consider the last 4-year cycle instead
        
        yrInitial_c <- c()
        yrFinal_c <- c()
        currentSpawnerData_available_c <- c()
        spawnerAbundance_lastGen_m_c <- c()
        spawnerAbundance_lastGen_dataPointNb_c <- c()
        currentSpawnerData_availableRecentEnough_c <- c()
        status_Smsy_prob_c <- c()
        status_Smsy80_prob_c <- c()
        for(yr in yr_max_here:yr_min_here){
          # yr <- yr_max_here
          # Calculate current spawner abundance:
          # yearCurrentAbundance should be NA so it is calculated from the most 
          # recent year with data
          
          yrInitial <- yrFinal <- yr
          cond_yr <- cuspawnerabundance$year == yr
          currentSpawnerData_available <- T
          cond_cuid <- cuspawnerabundance$cuid == cuids[which(cu == CUs)]
          spawnerAbundance_lastGen_m <- cuspawnerabundance$estimated_count[cond_yr & cond_cuid]
          spawnerAbundance_lastGen_dataPointNb <- 1
          currentSpawnerData_availableRecentEnough <- T
          
          # determine the number of time this CUs fall under the Red, Amber and Green 
          # status over all the simulations
          if(currentSpawnerData_available & currentSpawnerData_availableRecentEnough){
            
            status_Smsy <- status_Smsy80 <- c()
            for(chain in 1:length(post)){ # for each chain
              # chain <- 1
              for(draw in 1:nrow(post[[1]])){   # for each mcmc draw
                # draw <- 1
                LB_Sgen <- SR_bench[ "Sgen", which(yr == yr_min:yr_max), chain, draw]    # i corresponds to the CU
                UB_Smsy <- SR_bench[ "Smsy", which(yr == yr_min:yr_max), chain, draw]
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
            
            comment <- ""
            
          }else{
            
            status_Smsy_prob <- status_Smsy80_prob <- rep(NA,3)
            names(status_Smsy_prob) <- names(status_Smsy80_prob) <- c("red","amber","green")
            
            if(!currentSpawnerData_available){
              comment <- "Only NAs in cuspawnerabundance.csv for this CU"
            }else if(!currentSpawnerData_availableRecentEnough){
              comment <- paste0("Not recent enough data: last year with data is ",yrFinal," while generation length = ",CU_genLength," years and current year = ",yearCurrentAbundance)
            }
          }
          
          yrInitial_c <- c(yrInitial_c,yrInitial)
          yrFinal_c <- c(yrFinal_c,yrFinal)
          currentSpawnerData_available_c <- c(currentSpawnerData_available_c,currentSpawnerData_available)
          spawnerAbundance_lastGen_m_c <- c(spawnerAbundance_lastGen_m_c,spawnerAbundance_lastGen_m)
          spawnerAbundance_lastGen_dataPointNb_c <- c(spawnerAbundance_lastGen_dataPointNb_c,spawnerAbundance_lastGen_dataPointNb)
          currentSpawnerData_availableRecentEnough_c <- c(currentSpawnerData_availableRecentEnough_c,currentSpawnerData_availableRecentEnough)
          status_Smsy_prob_c <- c(status_Smsy_prob_c,status_Smsy_prob)
          status_Smsy80_prob_c <- c(status_Smsy80_prob_c,status_Smsy80_prob)
          
        } # for each year
        
        # Record the probabilities:
        biologicalStatus_df <- data.frame(region = region[i_rg],
                                          species = species[i_sp],
                                          cuid = cuids[which(cu == CUs)],
                                          # CU = CUs[which(cu == CUs)],
                                          cu_name_pse = cu,
                                          # cu_name_dfo = CUname_dfo,
                                          year = yr_max_here:yr_min_here,
                                          current_spawner_abundance = spawnerAbundance_lastGen_m_c,
                                          yr_withData_end = yrFinal_c,
                                          yr_withData_start = yrInitial_c,
                                          yr_end_imposed = yearCurrentAbundance,
                                          genLength = CU_genLength,
                                          genLength_available = CU_genLength_available,
                                          genLength_dataPointNb = spawnerAbundance_lastGen_dataPointNb_c,
                                          status_Smsy_red   = status_Smsy_prob_c[names(status_Smsy_prob_c) == "red"],
                                          status_Smsy_amber = status_Smsy_prob_c[names(status_Smsy_prob_c) == "amber"],
                                          status_Smsy_green = status_Smsy_prob_c[names(status_Smsy_prob_c) == "green"],
                                          status_Smsy80_red   = status_Smsy80_prob_c[names(status_Smsy80_prob_c) == "red"],
                                          status_Smsy80_amber = status_Smsy80_prob_c[names(status_Smsy80_prob_c) == "amber"],
                                          status_Smsy80_green = status_Smsy80_prob_c[names(status_Smsy80_prob_c) == "green"],
                                          model = m_best,
                                          comment = comment)
        
        
        # add it to biologicalStatus_rg_sp_df
        if(is.null(biologicalStatus_rg_sp_df)){
          biologicalStatus_rg_sp_df <- biologicalStatus_df
        }else{
          biologicalStatus_rg_sp_df <- rbind(biologicalStatus_rg_sp_df,
                                                      biologicalStatus_df)
        }
        
        #' Obtain the benchamrks CI
        #' - Median and 95% quantiles
        #' - highest posterior density (HPD) and HPD interval
        for(yr in yr_max_here:yr_min_here){
          # yr <- yr_max_here
          
          cond_Sgen_NA <- all(is.na(SR_bench[ "Sgen", which(yr == yr_min:yr_max), , ]))
          cond_Smsy_NA <- all(is.na(SR_bench[ "Smsy", which(yr == yr_min:yr_max), , ]))
          
          if(cond_Sgen_NA){
            Sgen <-  rbind(rep(NA,3),rep(NA,3))
            rownames(Sgen) <- c("medQuan","HPD")
            colnames(Sgen) <- c("m","mCI",NA)
          }else{
            Sgen <- rbind(
              medQuan = medQuan(SR_bench[ "Sgen", which(yr == yr_min:yr_max), , ]),
              HPD = HPD(SR_bench[ "Sgen", which(yr == yr_min:yr_max), , ]))
          }
          
          if(cond_Smsy_NA){
            Smsy <-  rbind(rep(NA,3),rep(NA,3))
            rownames(Smsy) <- c("medQuan","HPD")
            colnames(Smsy) <- c("m","mCI",NA)
          }else{
            Smsy <- rbind(
              medQuan = medQuan(SR_bench[ "Smsy", which(yr == yr_min:yr_max), , ]),
              HPD = HPD(SR_bench[ "Smsy", which(yr == yr_min:yr_max), , ]))
          }
          
          benchSummary <- list(Sgen,Smsy)
          names(benchSummary) <- c("Sgen","Smsy")
          
          # Report the benchmark values and CI
          benchSummary_df <- data.frame(region = rep(region[i_rg],4),
                                        species = rep(species[i_sp],4),
                                        cuid = rep(cuids[which(cu == CUs)],4),
                                        cu_name_pse = rep(cu,4),
                                        year = yr,
                                        benchmark = c(rep(names(benchSummary)[1],2),
                                                      rep(names(benchSummary)[2],2)),
                                        method = rep(rownames(benchSummary[[1]]),2))
          
          benchSummary_df$m <- c(benchSummary$Sgen[,"m"],benchSummary$Smsy[,"m"])
          benchSummary_df$CI025 <- c(benchSummary$Sgen[,2],benchSummary$Smsy[,2])
          benchSummary_df$CI975 <- c(benchSummary$Sgen[,3],benchSummary$Smsy[,3])
          benchSummary_df$model <- m_best
          
          # add it to benchSummary_rg_sp_df
          if(is.null(benchSummary_rg_sp_df)){
            benchSummary_rg_sp_df <- benchSummary_df
          }else{
            benchSummary_rg_sp_df <- rbind(benchSummary_rg_sp_df,
                                                    benchSummary_df)
          }
        } # for each year
        
        #----------------------
        # Plot
        #----------------------
        
        i_cu <- which(cu == CUs)
        
        cond_cuid <- benchSummary_rg_sp_df$cuid == cuids[i_cu]
        cond_HPD <- benchSummary_rg_sp_df$method == "HPD"
        cond_Sgen <- benchSummary_rg_sp_df$benchmark == "Sgen"
        cond_Smsy <- benchSummary_rg_sp_df$benchmark == "Smsy"
        
        model <- benchSummary_rg_sp_df$model[cond_cuid] |> unique()
        
        # if(Smax > max(SRm$S[, i], na.rm = TRUE) * 3){
        #   Smax <- max(SRm$S[, i], na.rm = TRUE) * 3
        # }
        
        yr_max_here <- names(SRm$S[,i_cu][!is.na(SRm$S[,i_cu])]) |> as.numeric() |> max()
        
        # if(m_best == "Ricker"){
        #   # for Ricker we only calculate the bencmarks for the most recent year
        #   # because the model was fitted on the SR data up to that year (the same
        #   # model should not be applied to previous years as with the Larkin model as
        #   # the latter one accounts for temporal dependence in S)
        #   yr_min_here <- yr_max_here
        # }else{
        #   # for each possible complete cycle from last year with data in S_here
        #   yr_min_here <- min(names(S_here) |> as.numeric()) + length(b_parameters) - 1
        #   # alternative simpler: only consider the last cycle:
        #   yr_min_here <- yr_max_here - 3   # only consider the last 4-year cycle instead
        # }
        
        yr_min_here <- yr_max_here - 3 # only consider the last 4-year cycle instead
        
        if(print_fig){
          CUhere <- gsub(pattern = "/",'-',cu)  # in case "/" is in the CU's name
          CUhere <- gsub(pattern = "\\(","",CUhere)
          CUhere <- gsub(pattern = "\\)","",CUhere)
          
          if(nchar(CUhere) > 35){   # the figure can't print if the number of characters is too large
            CUhere <- substr(x = CUhere,start =  1,stop = 25) 
          }
          
          CUhere <- gsub(" ","_",CUhere)
          
          pathFile <- paste0(wd_figures,"/",regionName,"_",species_acro[i_sp],"_",
                             CUhere,"_",cuids[i_cu],"_","Larkin","_benchmark_posteriors.jpeg")
          
          if(length(yr_max_here:yr_min_here) == 1){
            width <- 30
            height <- 14
          }else{
            coef <- .6
            width <- 30 * coef
            height <- 14 * 3 * coef
          }
          
          jpeg(file = pathFile, width = width, height = height, units = "cm", res = 300)
        }
        
        # if(model_choice == "Ricker"){
        #   heights <- c()
        # }else{
        #   heights <- c(1.1,1,1,1.2)
        # }
        
        heights <- c(1.1,1,1,1.2)
        
        layout(matrix(data = 1:(length(yr_max_here:yr_min_here)*2), 
                      nrow = length(yr_max_here:yr_min_here), 
                      byrow = T), heights = heights)
        
        
        # Determine the Smax and Rmax among the different years
        dummy_spawners_l <- list()
        dummy_recruits_full_l <- list()
        Smax_c <- c()
        Rmax_c <- c()
        i <- 1
        for(yr in yr_max_here:yr_min_here){
          # yr <- yr_max_here
          
          cond_yr <- benchSummary_rg_sp_df$year == yr
          
          # Create dummy vector of spawner abundance
          Smax <- max(c(SRm$S[,i_cu],
                        benchSummary_rg_sp_df$CI975[cond_cuid & cond_yr]),
                      na.rm = T) 
          
          dummy_spawners <- seq(1, Smax, length.out = 200)
          
          # Calculate posterior prediction for recruits based on a and b
          dummy_recruits_full <- array(NA, dim = c(200, nchains*nrow(post[[1]])))
          for(j in 1:200){
            # R = S*exp(a - b*S)
            # a <- SR_bench[ "a", which(yr == yr_min:yr_max), , ]
            a <- SR_bench[ "a_Larkin", which(yr == yr_min:yr_max), , ]
            b <- SR_bench[ "b", which(yr == yr_min:yr_max), , ]
            dummy_recruits_full[j, ] <- dummy_spawners[j] * exp(a - b * dummy_spawners[j])
          }
          
          # Summarize predicted recruits using original method and HPD for comparison
          dummy_recruits <- list(
            medQuan = apply(dummy_recruits_full, 1, medQuan),
            HPD = apply(dummy_recruits_full, 1, HPD, xmax = Smax)
          )
          
          Rmax <-  max(c(SRm$R[,i_cu],dummy_recruits$medQuan[3,],dummy_recruits$HPD[3,]),
                       na.rm = T)
          
          dummy_spawners_l[[i]] <- dummy_spawners
          dummy_recruits_full_l[[i]] <- dummy_recruits_full
          Smax_c[i] <- Smax
          Rmax_c[i] <- Rmax
          
          i <- i + 1
        }
        
        Smax <- max(Smax_c)
        Rmax <- max(Rmax_c)
        dummy_spawners <- seq(1, Smax, length.out = 200)
        
        for(yr in yr_max_here:yr_min_here){
          # yr <- yr_max_here
          
          #----------------------
          # Plot SR relationship
          #----------------------
          
          side1 <- side3 <- 1
          main <- ""
          xaxt <- "n"
          xlab_SR <- xlab_bench <- ""
          if(yr == yr_max_here){ # top panels
            side3 <- 3
            main <- paste(cu,cuids[i_cu],model, sep = " - ")
          }
          if(yr == yr_min_here){
            side1 <- 4.5
            xaxt <- "s"
            xlab_SR <- "Spawners"
            xlab_bench <- "Benchmarks"
          }
          
          par(mar = c(side1,4.5,side3,.5))
          
          # Plot SR relationship with predicted recruits and 95% CI
          S <- dummy_spawners
          R <- dummy_recruits$medQuan["m",]
          ylab <- "Recruits"
          
          plot(x = S, y = R, type = "n", xlim = c(0, Smax), ylim = c(0, Rmax), 
               xlab = xlab_SR, ylab = ylab, bty = "l", xaxt = xaxt, 
               main = main)
          
          if(yr != yr_min_here){
            axis(side = 1, labels = NA)
          }
          
          points(x = SRm$S[, i_cu], y = SRm$R[, i_cu])
          
          for(j in 1:2){ # medQuan, HPD
            lines(S, R, lty = c(2,1)[j], lwd = 2) # median
            lines(S, dummy_recruits[[j]][2, ], lty = c(2,1)[j]) # lower CL
            lines(S, dummy_recruits[[j]][3, ], lty = c(2,1)[j]) # Upper CL
          }
          
          # Plot benchmarks:
          cond_yr <- benchSummary_rg_sp_df$year == yr
          Sgen_80Smsy <- benchSummary_rg_sp_df$m[cond_cuid & cond_yr & cond_HPD]
          Sgen_80Smsy[2] <- Sgen_80Smsy[2] * .8 # 80% Smsy

          abline(v = Sgen_80Smsy, col = status_cols[c('red', 'green')], lwd = 2)
          
          y <- Rmax * c(.98, 1.02)
          
          points(x = Sgen_80Smsy, y = y, col = status_cols[c('red', 'green')], 
                 pch = 19, xpd = NA)
          
          bench_CIL <- c(benchSummary_rg_sp_df$CI025[cond_cuid & cond_yr & cond_HPD & cond_Sgen],
                         benchSummary_rg_sp_df$CI025[cond_cuid & cond_yr & cond_HPD & cond_Smsy] * .8)
          bench_CIU <- c(benchSummary_rg_sp_df$CI975[cond_cuid & cond_yr & cond_HPD & cond_Sgen],
                         benchSummary_rg_sp_df$CI975[cond_cuid & cond_yr & cond_HPD & cond_Smsy] * .8)

          segments(x0 = bench_CIL, x1 = bench_CIU, y0 = y, y1 = y, 
                   col = status_cols[c('red', 'green')], lwd = 2, xpd = NA)
          
          legend("bottomright", lty = c(2,1, NA, NA), pch = c(NA, NA, 19, 19), 
                 col = c(1, 1, status_cols['red'], status_cols['green']), 
                 legend = c("median and quantiles", "HPD", "Sgen", "80% Smsy"), 
                 bty = 'n')
          
          #----------------------
          # Histograms of posterior mcmc draws:
          #----------------------
          
          Sgen <- SR_bench[ "Sgen", which(yr == yr_min:yr_max), , ]
          # Sgen <- Sgen[which(Sgen <= Smax)]
          
          Smsy80 <- SR_bench[ "Smsy",  which(yr == yr_min:yr_max),, ] * 0.8
          # Smsy <- Smsy[which(Smsy <= Smax)]
          
          cond_cuid_here <- biologicalStatus_rg_sp_df$cuid == cuids[i_cu]
          cond_yr_here <- biologicalStatus_rg_sp_df$year == yr
          csa <- biologicalStatus_rg_sp_df$current_spawner_abundance[cond_cuid_here & cond_yr_here]

          #Smax <- max(c(Sgen,Smsy80,csa), na.rm = T)
          #Smax_hist <- Smax * .5
          Smax_hist <- max(c(SR_bench["Sgen",,,],SR_bench["Smsy",,,]*0.8,csa), na.rm = T)
          
          # If there are Sgen and Smsy values < :
          if(length(Sgen) > 0 & length(Smsy80) > 0){
            
            main <- ""
            col_main <- "black"
            if(yr == yr_max_here){ # top panels
              main <- paste0("prCV = ",convergence_df$prCV,
                             " ; Rc_max = ",round(convergence_df$Rc_max,2))
              if(convergence_df$Rc_max > 1.1){
                col_main <- "red"
              }
            }
            
            h1 <- hist(x = Sgen, breaks = seq(0, Smax_hist, Smax_hist/50), plot = F)
            h2 <- hist(x = Smsy80, breaks = seq(0, Smax_hist, Smax_hist/50), plot = F)
            
            # h1 <- hist(x = Sgen, plot = F)
            # h2 <- hist(x = Smsy, plot = F)
            
            if(max(h1$density) > max(h2$density)){
              hist(x = Sgen, col = paste0(status_cols['red'], 50), border = NA,
                   breaks = seq(0, Smax_hist, Smax_hist/50), 
                   xlim = c(0, Smax_hist), main = main, col.main = col_main,
                   freq = FALSE, xaxt = xaxt, xlab = xlab_bench)
              hist(x = Smsy80, col = paste0(status_cols['green'], 50), border = NA, 
                   breaks = seq(0, Smax_hist, Smax_hist/50), 
                   main = "", add = T, freq = FALSE, 
                   xaxt = xaxt, xlab = xlab_bench)
            }else{
              hist(x = Smsy80, col = paste0(status_cols['green'], 50), border = NA, 
                   breaks = seq(0, Smax_hist, Smax_hist/50), 
                   main = main,  col.main = col_main, freq = FALSE,
                   xaxt = xaxt, xlab = xlab_bench)
              hist(x = Sgen, col = paste0(status_cols['red'], 50), border = NA,
                   breaks = seq(0, Smax_hist, Smax_hist/50), 
                   xlim = c(0, Smax_hist), main = "", 
                   freq = FALSE, add = T, xaxt = xaxt, xlab = xlab_bench)
            }
            
            if(yr != yr_min_here){
              axis(side = 1, labels = NA)
            }
            
            # Plot density lines and benchmark estimates
            dens <- list(density(Sgen,from = 0, to = Smax_hist, na.rm = T),
                         density(Smsy80,from = 0, to = Smax_hist, na.rm = T))
            
            lines(dens[[1]], lwd = 2, col = status_cols[c('red', 'green')[1]])
            lines(dens[[2]], lwd = 2, col = status_cols[c('red', 'green')[2]])
            
            abline(v = Sgen_80Smsy, col = status_cols[c('red', 'green')], lwd = 2)
            
            #abline(v = c(median(Sgen),median(Smsy80)), col = status_cols[c('red', 'green')], lwd = 2)
            
            y <- max(c(h1$density,h2$density)) * c(.98, 1.02)
            points(x = Sgen_80Smsy, y = y, col = status_cols[c('red', 'green')], pch = 19, xpd = NA)
            segments(x0 = bench_CIL, x1 = bench_CIU, y0 = y, y1 = y, 
                     col = status_cols[c('red', 'green')], lwd = 2, xpd = NA)
            
            legend("topright", legend = yr, cex = 1.5, bty = 'n')
            
            legend("right","80% Smsy",bty = "n", text.col = status_cols[c('green')])
            
          }else{
            
            # commentHere <- paste0("Smax = ",Smax,
            #                       "; min(Sgen) = ",round(min(SR_bench[ "Sgen",,]),1),
            #                       "; min(Smsy) = ",round(min(SR_bench[ "Smsy",,]),1))
            
            plot.new()
            legend("center",c("ERROR",
                              paste0("Smax = ",round(Smax,1)),
                              paste0("min(Sgen) = ",round(min(SR_bench[ "Sgen", which(yr == yr_min:yr_max),,]),1)),
                              paste0("min(Smsy) = ",round(min(SR_bench[ "Smsy", which(yr == yr_min:yr_max),,]),1))))
            
            legend("topright", legend = yr, cex = 1.5, bty = 'n')
            
            # if(biologicalStatus_df$comment == ""){
            #   biologicalStatus_df$comment <- commentHere
            # }else{
            #   biologicalStatus_df$comment <- paste(biologicalStatus_df$comment,
            #                                        commentHere, sep = " ; ERROR: ")
            # }
          }
          
          # plot current spawner abundance 
          abline(v = csa, lwd = 2)
          
          # show the years in current generation
          yr_end <- biologicalStatus_rg_sp_df$yr_withData_end[cond_cuid_here & cond_yr_here]
          yr_start <- biologicalStatus_rg_sp_df$yr_withData_start[cond_cuid_here & cond_yr_here]
          yr_start_end <- paste(unique(c(yr_start,yr_end)), collapse = " - ")
          y_max <- max(c(h1$density,h2$density)) * 1
          #text(x = csa, y = y_max, labels = yr_start_end, pos = 4)
          
          # # Add legend
          # plot(1,1,"n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
          # legend("top", fill = c(statusCols['r'], statusCols['g']), legend = c("Sgen", "Smsy"), bty = "n", border = NA, cex = 1.5)
          # legend("center", lty = c(2,1), title = "Posterior summary", legend = c("median", "HPD"), bty = "n", border = NA, cex = 1.5)
          
          # Add spawner points along bottom for reference
          #points(x = d$Esc[d$CU == keepCU[i]], y = rep(0, length(which(d$CU == keepCU[i]))))
          
        } # for each yr
        
        if(print_fig){
          dev.off()
        }
        
      } # for each CU

      print(paste0("*** ",region[i_rg],"_",species_acro[i_sp]," done ***"))
      
      write.csv(x = benchSummary_rg_sp_df, 
                file = paste0(wd_output,"/intermediate/",regionName,"_",species_acro[i_sp],"_cyclic_","Larkin","_benchmarks_summary_HBSRM.csv"),
                row.names = F,)
      
      write.csv(x = biologicalStatus_rg_sp_df, 
                file = paste0(wd_output,"/intermediate/",regionName,"_",species_acro[i_sp],"_cyclic_","Larkin","_biological_status_HBSRM.csv"),
                row.names = F)
      
      write.csv(x = alpha_rg_sp_df, 
                file = paste0(wd_output,"/intermediate/",regionName,"_",species_acro[i_sp],"_cyclic_","Larkin","_alphas_HBSRM.csv"),
                row.names = F)
      

    } # end of for each species
  }
} # end of for each region


