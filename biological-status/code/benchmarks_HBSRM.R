
#'******************************************************************************
#' The goal of the script is to calculate the benchmarks from the HBSE modelling
#' work done in HBSRM.R. 
#' 
#' Files imported (from ):
#' - 
#' 
#' Files produced: 
#' - figures/region_species_CU_benchmark_posteriors.jpeg
#' - output/region_species_benchmarks_summary.csv
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
source("functions_set_wd.R")

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

# The datasets to input were outputted by other scripts 
wd_data_input <- wd_output

# Import functions for this specific project
source("Code/functions.R")

# Load packages
library(R2jags)  # Provides wrapper functions to implement Bayesian analysis in JAGS.  ??? needed ?
library(modeest) # Provides estimators of the mode of univariate data or univariate distributions. ??? needed ?

# option to export the figures
print_fig <- F

# Paths to the repositories containing the run reconstruction datasets for each 
# region.
wd_data_regions <- wd_data_regions_fun(wd_root = wd_X_Drive1_PROJECTS)

# Import species names and acronyms
species_acronym <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

# import the conservationunits_decoder.csv (to access generation length)
# To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- read.csv(paste(wd_pop_indic_data_input_dropbox,"conservationunits_decoder.csv",sep = "/"),
                                      header = T)

#------------------------------------------------------------------------------#
# Selection of region(s) and species
#------------------------------------------------------------------------------#

# Choosing the region
# BSC: This will have to eventually be automatized and eventually allows for 
# multiple regions to be passed on.
region <- regions_df$Fraser
region <- regions_df$Yukon
region <- regions_df$Nass

# multiple regions:
region <- c(
  regions_df$Fraser,
  regions_df$Yukon,
  regions_df$Nass)

region <- c(
  regions_df$Central_coast,
  regions_df$Haida_Gwaii,
  regions_df$Skeena)

# all the regions
region <- as.character(regions_df[1,])
region <- region[region != "Columbia"]

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
species <- c(
             species_acronym$Sockeye,    
             species_acronym$Pink,
             species_acronym$Coho
             #species_acronym$Cutthroat,
             #species_acronym$Chum
             )

# If we do not specify the species: all the species that have a _SRdata files are 
# returned: 
# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- TRUE

for(i_rg in 1:length(region)){
  
  # i_rg <- 1
  
  if(species_all){
    files_list <- list.files(wd_data_input)
    files_s <- files_list[grepl(pattern = "_posteriors_priorShift",files_list)]
    files_s <- files_s[grepl(pattern = region[i_rg],files_s)]
    species <- unique(sub("_posteriors_priorShift.*", "", files_s))
    species <- gsub(pattern = paste0(region[i_rg],"_"), replacement = "", x = species)
  }
  
  # BSC: if case species is not NULL, write code to remove species for which there
  # is not dataset for that specific region
  
  for(i_sp in 1:length(species)){
    
    # i_sp <- 1
    
    # Import the HBSRM outputs, i.e., the posterior distributions of:
    # - mu_a and sigma_a: with CU-level intrinsic productivity ai ~ N(mu_a,sigma_a)
    # - with bi the CU-level density dependence parameter bi ~ logN(log(1/Smaxi),sigma_bi), with Smaxi being the max(S) of that CU i
    # in the datasets "ma_a" = "ma_a", "sigma_a" = "sd_a", "sigma_bi" = "sd[i]"
    post <- readRDS(paste0(wd_data_input,"/",region[i_rg],"_",species[i_sp],"_posteriors_priorShift.rds"))
    
    # find the nb of CUs
    CUs <- read.csv(paste0(wd_data_input,"/",region[i_rg],"_",species[i_sp],"_CUs_names.csv"),
                    header = T,stringsAsFactors = F)
    CUs <- CUs$CU
    nCUs <-length(CUs)
    
    nchains <- length(post) # 6 chains
    # parameter names
    pnames <- colnames(post[[1]])
    
    #-----------------------------------------------------------------------------#
    # Check convergence of chains
    #-----------------------------------------------------------------------------#
    
    # The Gelman-Rubin metric : R_hat ; if R_hat < 1.1 all is good
    r.hat <- gelman.diag(x = post, multivariate = F)
    if(sum(r.hat[[1]][, 1] > 1.1, na.rm = T) > 0){
      warning(paste("Some convergence issues for parameters: \n", 
                    paste0(region[i_rg]," - ",species[i_sp]," - ",pnames[which(r.hat[[1]][, 1] > 1.1)])))
      print(r.hat)
    }
    if(sum(is.na(r.hat[[1]][, 1])) > 0){  # BSC: I added that for Fraser PK 
      warning(paste("Some convergence issues for parameters: \n", 
                    paste0(region[i_rg]," - ",species[i_sp]," - ",pnames[which(is.na(r.hat[[1]][, 1]))])))
      print(r.hat)
    }
    
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
    
    # library(MCMCglmm) # For poster.mode function
    
    # Import the S and R matrices used for fitting the HBSR model:
    # BSC: the wd here will eventually have to be set to the final repo for the 
    # exported datasets.
    SRm <- readRDS(paste0(wd_data_input,"/",region[i_rg],"_",species[i_sp],"_SR_matrices.rds"))
    
    # Compare median/quantiles (medQuan) and HPD/HPDI (HPD)
    # if(print_fig){
    #   pathFile <- paste0(wd_figures,"/",region,"_",species[i_sp],"_benchmark_posteriors.pdf")
    #   pdf(file = pathFile, width = 8.5, height = 11)
    # }
    
    # data frame that will contain the benchmark central values and CI for the two
    # methods used (median and quantile and HPD)
    benchSummary_region_species_df <- NULL
    
    # data frame that will contain the biological status expressed as probabilities
    # for each of the three levls (i.e., red, amber and green) both with 
    # upper threshold Smsy and 80% of Smsy
    biologicalStatus_region_species_df <- NULL
    
    statusCols <- c(g = "#8EB687", a = "#DFD98D", r = "#9A3F3F")  # BSC: those are not colour blind friendly
    # par(mfrow = c(3,2), mar = c(4, 4, 5, 1), oma = c(3,3,1,0))
    # layout(matrix(data = 1:(nCUs * 2), nrow = nCUs, byrow = T))
    for(i in 1:nCUs){
      
      # i <- 1
      
      #----------------------
      # biological status probability with the average spawner abundance over the last generation
      #----------------------
      
      spawnerAbundance <- SRm$S[, i]
      
      CUname <- gsub(pattern = "_",replacement = " ",x = CUs[i])   # DFO Cu name
      
      # quick fixes for dealing with ALL the variations in the CUs names
      CUname <- CU_name_variations_fun(CUname = CUname,
                                       spawnerAbundance = spawnerAbundance,
                                       speciesAcronym =  species[i_sp])

      speciesName <- species[i_sp]
      if(speciesName[1] == "SX"){
        speciesName <- c(speciesName,"SEL","SER")
        # cf. unique(conservationunits_decoder$species_abbr)
      }else if(speciesName[1] == "CN"){
        speciesName <- c(speciesName,"CK")
      }else if(speciesName[1] == "PK"){
        speciesName <- c(speciesName,"PKO","PKE")
      }
      
      conservationunits_decoder_cut <- conservationunits_decoder[conservationunits_decoder$cu_name_pse %in% CUname &
                                                                   conservationunits_decoder$species_abbr %in% speciesName,]
      
      # conservationunits_decoder_cut <- conservationunits_decoder[conservationunits_decoder$cu_name_dfo %in% CUname &           # USE cu_name_dfo and not cu_name_pse !!!
      #                                                              conservationunits_decoder$species_abbr %in% speciesName,]
      
      if(nrow(conservationunits_decoder_cut) == 0){
        print("This CUS is not found in conservationunits_decoder:")
        print(paste(region[i_rg],species[i_sp],CUname))
        cat("\n")
      }else if(nrow(conservationunits_decoder_cut) > 1){
        if(length(unique(conservationunits_decoder_cut$pooledcuid)) > 1){ # if == 1 there are all the same CUs for PSF
          print("There are multiple CUs with that name who don't have the same pooledcuid, the 1st row is used")
          print(paste(region[i_rg],species[i_sp],CUname))
          print(conservationunits_decoder_cut)
          cat("\n")
        }
        conservationunits_decoder_cut <- conservationunits_decoder_cut[1,,drop = F]
      }
      
      # keep track of the different version of the CU names
      CUname_pse <- conservationunits_decoder_cut$cu_name_pse
      CUname_dfo <- conservationunits_decoder_cut$cu_name_dfo

      CU_genLegth <- conservationunits_decoder_cut$gen_length[1]
      CU_genLegth_available <- TRUE
      
      if(is.na(CU_genLegth)){
        # find the mean over the CUs of the same species: NOT USED
        # conservationunits_decoder_sp <- conservationunits_decoder[conservationunits_decoder$species_abbr %in% speciesName,]
        # CU_genLegth <- round(mean(conservationunits_decoder_sp$gen_length))
        
        # From Tech-Report: 
        #' "Where CU-specific data on age-at-return are unavailable, we assume 
        #' generation lengths of 
        #' - 5 years for Chinook CUs, 
        #' - 4 years for coho CUs, 
        #' - 4 years for chum CUs, 
        #' - 4 years for sockeye CUs"
        #' - Pink salmon have a consistent 2-year age-at-return and because 
        #' even- and odd-year lineages are considered separate CUs, the most 
        #' recent spawner abundance is simply the most recent year’s estimated 
        #' spawner abundance for this species
        CU_genLegth <- generationLengthEstiamte_df$genLength[generationLengthEstiamte_df$species %in% speciesName]
        CU_genLegth_available <- FALSE
      }
      
      # remove NAs at the tail and return the most recent year from which the 
      # avergae spawner abundance is calculated:
      while(is.na(tail(spawnerAbundance,1))){
        spawnerAbundance <- spawnerAbundance[-length(spawnerAbundance)]
      }
      yrFinal <- as.numeric(names(spawnerAbundance)[length(spawnerAbundance)])
      
      # calculate the geometric mean over the last generation
      spawnerAbundance_lastGen <- tail(spawnerAbundance,CU_genLegth)
      spawnerAbundance_lastGen_m <- mean_geom_fun(x = spawnerAbundance_lastGen)
      spawnerAbundance_lastGen_dataPointNb <- sum(!is.na(spawnerAbundance_lastGen))
      
      # determine the number of time this CUs fall under the Red, Amber and Green 
      # status over all the simulations
      status_Smsy <- status_Smsy80 <- c()
      for(j in 1:length(post)){ # for each chain
        # j <- 1
        for(k in 1:nrow(post[[1]])){   # for each mcmc draw
          # k <- 1
          LB_Sgen <- SR_bench[i, "Sgen", j, k]
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
      
      biologicalStatus_df <- data.frame(region = region[i_rg],
                                        species = species[i_sp],
                                        CU = CUs[i],
                                        CU_pse = CUname_pse,
                                        CU_dfo = CUname_dfo,
                                        year_last = yrFinal,
                                        genLength = CU_genLegth,
                                        genLength_available = CU_genLegth_available,
                                        genLength_dataPointNb = spawnerAbundance_lastGen_dataPointNb,
                                        status_Smsy_red   = status_Smsy_prob["red"],
                                        status_Smsy_amber = status_Smsy_prob["amber"],
                                        status_Smsy_green = status_Smsy_prob["green"],
                                        status_Smsy80_red   = status_Smsy80_prob["red"],
                                        status_Smsy80_amber = status_Smsy80_prob["amber"],
                                        status_Smsy80_green = status_Smsy80_prob["green"])

      
      if(is.null(biologicalStatus_region_species_df)){
        biologicalStatus_region_species_df <- biologicalStatus_df
      }else{
        biologicalStatus_region_species_df <- rbind(biologicalStatus_region_species_df,
                                                biologicalStatus_df)
      }

      #----------------------
      # Plot
      #----------------------
      
      if(print_fig){
        CUhere <- gsub(pattern = "/",'-',CUs[i])  # in case "/" is in the CU's name
        pathFile <- paste0(wd_figures,"/",region[i_rg],"_",species[i_sp],"_",CUhere,
                           "_benchmark_posteriors.jpeg")
        
        # pdf(file = pathFile, width = 8.5, height = 11)
        # pdf(file = pathFile, width = 15, height = 7.5)
        jpeg(file = pathFile, width = 30, height = 14, units = "cm", res = 300)
      }
      
      layout(matrix(data = 1:2, nrow = 1))
      par(mar = c(4.5,4.5,4,2))
      
      # max spawners for that CU for plotting purposes
      maxS <- max(SRm$S[, i], na.rm = TRUE) * 1.5
      
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
           xlim = c(0, maxS/1.5), ylim = c(0, max(SRm$R[, i], na.rm = TRUE)), 
           xlab = "Spawners", ylab = "Recruits", bty = "l")
      mtext(side = 3, adj = 0, line = 2.5, CUs[i])
      points(SRm$S[, i], SRm$R[, i])
      for(j in 1:2){
        lines(dummy_spawners, dummy_recruits[[j]][1, ], lty = c(2,1)[j], lwd = 2)
        lines(dummy_spawners, dummy_recruits[[j]][2, ], lty = c(2,1)[j])
        lines(dummy_spawners, dummy_recruits[[j]][3, ], lty = c(2,1)[j])
      }
      
      #----------------------
      # Plot benchmarks
      #----------------------
      benchSummary <- list(
        Sgen = rbind(
          medQuan = medQuan(SR_bench[i, "Sgen", , ]),
          HPD = HPD(SR_bench[i, "Sgen", , ])),
        Smsy = rbind(
          medQuan = medQuan(SR_bench[i, "Smsy", , ]),
          HPD = HPD(SR_bench[i, "Smsy", , ]))
      )
      
      u <- par('usr')
      for(j in 1:2){    # median, CI
        for(k in 1:2){  # Sgen, Smsy
          abline(v = benchSummary[[k]][j, 1], lty = c(2,1)[j], 
                 col = statusCols[c('r', 'g')[k]])
          # abline(v = benchSummary[[k]][j, 2], lty = c(2,1)[j], col = statusCols[c('r', 'g')[k]])
          # abline(v = benchSummary[[k]][j, 3], lty = c(2,1)[j], col = statusCols[c('r', 'g')[k]])
          y <- u[4] + c(c(0.05, 0.02)[k] + c(0, 0.05)[j])*(u[4]- u[3])
          points(x = benchSummary[[k]][j, 1],y = y, 
                 col = statusCols[c('r', 'g')[k]], pch = 19, xpd = NA)
          segments(x0 = benchSummary[[k]][j, 2], x1 = benchSummary[[k]][j, 3], 
                   y0 = y, y1 = y, 
                   col = statusCols[c('r', 'g')[k]], 
                   lty = c(2,1)[j], lwd = 2, xpd = NA)
        }}
      
      legend("topright", lty = c(2,1, NA, NA), pch = c(NA, NA, 19, 19), 
             col = c(1, 1, statusCols['r'], statusCols['g']), 
             legend = c("median and quantiles", "HPD", "Sgen", "Smsy"), bg = "white")
      
      
      # Histograms of posterior mcmc draws:
      Sgen <- SR_bench[i, "Sgen", , ]
      Sgen <- Sgen[which(Sgen <= maxS)]
      
      Smsy <- SR_bench[i, "Smsy", , ]
      Smsy <- Smsy[which(Smsy <= maxS)]
      
      dens <- list(density(SR_bench[i, "Sgen", , ], from = 0, to = maxS/1.5, na.rm = T), # BSC: I had to add na.rm = T
                   density(SR_bench[i, "Smsy", , ], from = 0, to = maxS/1.5, na.rm = T))
      
      
      h1 <- hist(x = Sgen, col = paste0(statusCols['r'], 50), border = NA,
                 breaks = seq(0, maxS, maxS/50), xlim = c(0, maxS), main = "", 
                 freq = FALSE, xlab = "Benchmarks")
      h2 <- hist(x = Smsy, col = paste0(statusCols['g'], 50), border = NA, 
                 breaks = seq(0, maxS, maxS/50), add = TRUE, 
                 freq = FALSE)
      
      # Plot density lines and benchmark estimates
      u <- par('usr')
      for(k in 1:2){
        lines(dens[[k]], lwd = 2, col = statusCols[c('r', 'g')[k]])
        for(j in 1:2){
          abline(v = benchSummary[[k]][j, 1], col = statusCols[c('r', 'g')[k]], 
                 lty = c(2,1)[j])
          # abline(v = benchSummary[[k]][j, 2:3], col = statusCols[c('r', 'g')[k]], lty = c(2,1)[j])
          
          y <- u[4] + c(c(0.05, 0.02)[k] + c(0, 0.05)[j])*(u[4]- u[3])
          points(x = benchSummary[[k]][j, 1],y = y, col = statusCols[c('r', 'g')[k]],
                 pch = 19, xpd = NA)
          segments(x0 = benchSummary[[k]][j, 2], x1 = benchSummary[[k]][j, 3], 
                   y0 = y, y1 = y, 
                   col = statusCols[c('r', 'g')[k]], lty = c(2,1)[j], lwd = 2, xpd = NA)
        }
      }
      
      if(print_fig){
        dev.off()
      }
      # Add spawner points along bottom for reference
      #points(x = d$Esc[d$CU == keepCU[i]], y = rep(0, length(which(d$CU == keepCU[i]))))
      
      benchSummary_df <- data.frame(region = rep(region[i_rg],4),
                                    species = rep(species[i_sp],4),
                                    CU = rep(CUs[i],4),
                                    benchmark = c(rep(names(benchSummary)[1],2),rep(names(benchSummary)[2],2)),
                                    method = rep(rownames(benchSummary[[1]]),2))
      
      benchSummary_df$m <- c(benchSummary$Sgen[,"m"],benchSummary$Smsy[,"m"])
      benchSummary_df$CI025 <- c(benchSummary$Sgen[,2],benchSummary$Smsy[,2])
      benchSummary_df$CI975 <- c(benchSummary$Sgen[,3],benchSummary$Smsy[,3])
      
      if(is.null(benchSummary_region_species_df)){
        benchSummary_region_species_df <- benchSummary_df
      }else{
        benchSummary_region_species_df <- rbind(benchSummary_region_species_df,
                                                benchSummary_df)
      }
    } # end of for each CU
    
    print(paste0("*** ",region[i_rg],"_",species[i_sp]," done ***"))
    
    write.csv(x = benchSummary_region_species_df, 
              file = paste0(wd_output,"/",region[i_rg],"_",species[i_sp],"_benchmarks_summary.csv"),
              row.names = F)
    
    write.csv(x = biologicalStatus_region_species_df, 
              file = paste0(wd_output,"/",region[i_rg],"_",species[i_sp],"_biological_status.csv"),
              row.names = F)

    # # Add legend
    # plot(1,1,"n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    # legend("top", fill = c(statusCols['r'], statusCols['g']), legend = c("Sgen", "Smsy"), bty = "n", border = NA, cex = 1.5)
    # legend("center", lty = c(2,1), title = "Posterior summary", legend = c("median", "HPD"), bty = "n", border = NA, cex = 1.5)
  }
}








