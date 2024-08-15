

#'******************************************************************************
#' The goal of the script is to conduct a hierarchical Bayesian spawner recruits 
#' (HBSR) R analysis for all regions and conservation units.
#' Code adapted from Korman and English (2013).
#' 
#' Files imported (from dropbox):
#' - recruitsperspawner.csv (from database)
#' - conservationunits_decoder (from database)
#' - data/priors_HBSRmodel.csv (created in checks_fixes.R which assemble the 
#' SRdata.txt files present in the "HBM and status" subfolders in each 
#' region-specific folders in dropbox)
#' 
#' Files produced: 
#' - output/REGION_SPECIES_SR_matrices.rds
#' - output/REGION_SPECIES_HBSRM_posteriors_priorShift.rds
#' 
#'******************************************************************************

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

# Import functions for this specific project
source("Code/functions.R")

# Load packages
library(R2jags)  # Provides wrapper functions to implement Bayesian analysis in JAGS.
library(modeest) # Provides estimators of the mode of univariate data or univariate distributions. ??? needed ?

# Paths to the repositories containing the run reconstruction datasets for each 
# region.
wd_data_regions <- wd_data_regions_fun(wd_root = wd_X_Drive1_PROJECTS)

# Import species names and acronyms
species_acronym_df <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

#' Import the recruitsperspawner.csv from population-indicators/data_input or 
#' download it from the PSF database
fromDatabase <- update_file_csv <- F

recruitsperspawner <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[3],
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
# Selection of region(s) and species
#------------------------------------------------------------------------------#

# option to show the SR plot
show_figures <- F

# Choosing the region(s)
region <- regions_df$Fraser
region <- regions_df$Yukon
region <- regions_df$Nass
region <- regions_df$Central_coast
region <- regions_df$Columbia     # BSC: no _RS data file
region <- regions_df$Haida_Gwaii
region <- regions_df$Skeena

# multiple regions:
region <- c(
  regions_df$Columbia,
  regions_df$Transboundary,
  regions_df$VIMI)

# all the regions
region <- as.character(regions_df[1,])
# region <- region[region != "Columbia"]

# **** BSC: issues to solve with *** DELETE CHUNK eventually
# region <- regions_df$Fraser
# species <- species_acronym$Pink   # one CU: Fraser River (odd)

# region <- regions_df$Nass
# species <- species_acronym$Pink    # two CUs: "Nass-Skeena_Estuary_Even"      "Nass_Portland_Observatory_odd"
# ****

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "CK"],    
             species_acronym_df$species_name[species_acronym_df$species_acro == "SX"])

# If we do not specify the species: all the species that have a _SRdata files are 
# returned: 
# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- TRUE

#' Import the prior values for the HBSR model parameters prSmax and prCV that are
#' used in HBSRM.R (the file is created in checks_fixes.R and contains the values 
#' of these priors that were originally contained in SRdata.txt files that are 
#' found in the "HBM and status" subfolders in each region-specific folders.
priors_HBSRmodel <- read.csv(paste0(wd_data,"/priors_HBSRmodel.csv"),header = T)


#'* Select the cyclic CUs *

cond_cycl <- grepl("cyclic",conservationunits_decoder$cu_name_pse)
cuid_cycl <- conservationunits_decoder$cuid[cond_cycl]
conservationunits_decoder[cond_cycl,]

region <- unique(conservationunits_decoder$region[cond_cycl])
species <- unique(conservationunits_decoder$species_name[cond_cycl])



# Set first brood year, "-99" for no constraint
FBYr <- -99

# Set minimum nb of SR data points required to be included in the analysis
MinSRpts <- 3 

# Set the HBSR number of simulations, burning runs and chains:
n.iter <- 10000 # 100000  # --> 10000 only QUESTION
n.burnin <- 3000 # 5000  # 
n.chains <- 6     # 

# options(warn=1)  # print warnings as they occur
options(warn = 2)  # treat warnings as errors

#----------------------------------------------------------------------------#
# Read in Stock-Recruit Data, run the HBSR model and output parameter estimates
#----------------------------------------------------------------------------#

for(i_rg in 1:length(region)){
  # i_rg <- 1
  
  recruitsperspawner_rg <- recruitsperspawner[recruitsperspawner$region == region[i_rg],]
  
  species_acro <- sapply(X = species,FUN = function(sp){
    species_acronym_df$species_acro[species_acronym_df$species_name == sp]
  })
  
  regionName <- region[i_rg]
  if(region[i_rg] == "Vancouver Island & Mainland Inlets"){
    regionName <- "VIMI"
  }
  
  if(sum(!is.na(recruitsperspawner_rg$spawners)) == 0 | sum(!is.na(recruitsperspawner_rg$recruits)) == 0){
    
    print(paste0("*** There is no data in recruitsperspawner.csv for salmon in ",region[i_rg]," ***"))
    
  }else{
    
    for(i_sp in 1:length(unique(species_acro))){
      
      # i_sp <- 1
      
      speciesAcroHere <- unique(species_acro)[i_sp]
      speciesHere <- species_acronym_df$species_name[species_acronym_df$species_acro %in% speciesAcroHere]
      
      cond <- recruitsperspawner_rg$species_name %in% speciesHere & 
        recruitsperspawner_rg$cuid %in% cuid_cycl
      recruitsperspawner_rg_sp <- recruitsperspawner_rg[cond,]
      
      cond <- conservationunits_decoder$region == region[i_rg] &
        conservationunits_decoder$species_name %in% speciesHere &
        conservationunits_decoder$cuid %in% cuid_cycl
      conservationunits_decoder_rg_sp <- conservationunits_decoder[cond,]
      
      print(paste0("*** Plot for: ",region[i_rg]," - ",speciesAcroHere," - cyclic ***"))
      

      #'* Define the spawner (S) and recruits (R) matrices *
      
      # organize the data into a year x CU for R and S:
      CUs <- unique(recruitsperspawner_rg_sp$cu_name_pse)
      CUs_cuid <- sapply(X = CUs,
                         FUN = function(cu){unique(recruitsperspawner_rg_sp$cuid[recruitsperspawner_rg_sp$cu_name_pse == cu])})
      # unique(recruitsperspawner_rg_sp[,c("cu_name_pse","cuid")])
      nCUs <- length(CUs)
      Yrs <- min(recruitsperspawner_rg_sp$year):max(recruitsperspawner_rg_sp$year)
      nYrs <- length(Yrs)
      
      S <- R <- matrix(nrow = nYrs, ncol = nCUs, dimnames = list(Yrs,CUs))
      for(j in 1:nCUs){
        # j <- 1
        dj <- subset(recruitsperspawner_rg_sp,cu_name_pse == CUs[j])
        S[as.character(dj$year),j] <- dj$spawners # dj$Esc
        R[as.character(dj$year),j] <- dj$recruits # dj$Rec
      }
      
      # remove the row with NAs in S but not R and vice versa of a same CU
      # NOTE: TO NOT DO BECAUSE THE LARKIN MODEL USED S VALUES UP TO 3 YEARS IN
      # THE PAST. IT DOES NOT DO ANYTHING HERE BECAUSE THE DATA HAVE NO GAPS.
      SR_l <- cuSR_removeNA_fun(R = R, S = S)
      R <- SR_l$R
      S <- SR_l$S
      
      # replace 0s by 1 to avoid the lm(log(R/S)~ S) to crash
      R <- apply(X = R,MARGIN = 2,FUN = function(c){
        # c <- R[,3]
        out <- c
        out[which(out == 0)] <- 1
        return(out)
      })
      S <- apply(X = S,MARGIN = 2,FUN = function(c){
        # c <- R[,3]
        out <- c
        out[which(out == 0)] <- 1
        return(out)
      })
      
      #' filter CUs with less than MinSRpts data points 
      CuToRemove <- c()
      for(j in 1:ncol(S)){
        # j <- 1
        CUHere <- colnames(S)[j]
        if(sum(!is.na(S[,CUHere])) < MinSRpts | sum(!is.na(R[,CUHere])) < MinSRpts){
          CuToRemove <- c(CuToRemove,CUHere)
        }
      }
      S <- S[,!colnames(S) %in% CuToRemove, drop = F]
      R <- R[,!colnames(R) %in% CuToRemove, drop = F]
      CUs <- CUs[!CUs %in% CuToRemove]
      CUs_cuid <- sapply(X = CUs,FUN = function(cu){
        unique(recruitsperspawner_rg_sp$cuid[recruitsperspawner_rg_sp$cu_name_pse == cu])
      })
      nCUs <- length(CUs)
      SR_l$R <- R
      SR_l$S <- S
      
      #'* Set priors on b: prSmax and prCV *
      # select the prior for these CUs and check those that do not have values
      cond_cuid <- priors_HBSRmodel$cuid %in% cuid_cycl
      priors_HBSRmodel_rg_sp <- priors_HBSRmodel[cond_cuid,]
      
      # NOTE: THERE IS NO PRIOR FOR THESE CYCLIC CUs; so define the priors here 
      # as a TEMPORARY solution
      
      # Define corresponding parameters to feed the HBSRM:
      prSmax <- apply(S,2,FUN = function(c){max(c, na.rm = T) * 3}) # as in p. 8 in (Grant et al. 2020)
      prCV <- rep(5,nCUs)
      prmub <- log(1/prSmax)    # convert mean prior on Smax to log b for winbugs model
      prtaub <- prmub^2/prCV^2
      
      saveRDS(SR_l,
              file = paste0(wd_output,"/",gsub(" ","_",regionName),"_",speciesAcroHere,"cyclic_SR_matrices.rds"))
      
      #### Estimate a and b by linreg and plot
      if(show_figures){
        if(.Platform$OS.type == "windows"){      # BSC: what to do with Linux? : x11()?
          windows()                              # Also do we want it to open in a new window vs in Rstudio?
        }else{
          quartz()
        }
      }
      
      # LnRS <- log(R/S)   # BSC: now is created inside the function with R and S to limit the number of parameters to pass in
      # inipars <- LinReg(Nyrs,LNRS,S,R,StNames)
      # Display the RS plot and output the estimated parameter values:
      inipars <- linRegRicker_fun(S = S, R = R, plot_figures = show_figures, 
                                  verbose = F)

      #------------------------------------------------------------------------------#
      #'*  Bayes model defined with rjags - Ricker model *
      #------------------------------------------------------------------------------#
      
      # Replace NAs with a value so that the model can run. Note that obs_lnRS still
      # has NAs, so the it does not matter what pred_lnRS is for these cases because
      # they are not considered in the likelihood calcutation (due to NAs in obs_lnRS).
      S[is.na(S)] <- -99    # could put any value
      nYrs <- nrow(S)
      
      # jags inputs:
      jags.data <- list(
        nCUs = nCUs,
        nYrs = nYrs,
        # rowsToKeep = rowsToKeep,
        obs_lnRS = log(R/S),  # data on observed log(recruits/spawners) - matrix nYrs x nCU
        S = S,         # data on observed spawners - matrix nYrs x nCU 
        # ln_Smsr = log(1/inipars$b),
        prmub = prmub, # prior on mu for b
        prtaub = prtaub # prior on tau for b
      ) # cov - not sure what this is
      
      # what is sd?  --> the sd for the likelihood --> not in the tech report
      # why sd_bi are not estimated?
      # why taking the exp(sd) in Sgen.optim()?
      
      # The name of the different model to pass to the HBSRM_JAGS_fun function:
      hbsrm <-       c("Ricker",
                       "Larkin_123",
                       "Larkin_1","Larkin_2","Larkin_3",
                       "Larkin_12","Larkin_13","Larkin_23")

      jags_m_l <- list()
      count <- 1
      for(m in hbsrm){
        # m <- hbsrm[2]
        
        if(grepl("Larkin",m)){
          m_nb <- sapply(1:3,function(nb){grepl(nb,m)})
          par_Larkin <- c("b1","b2","b3")[m_nb]
   
        }else{ # Ricker model
          par_Larkin <- NULL
        }

        jags.parms <- c("a", "b", par_Larkin,
                        "sd",     # the sd for the loglikelihood function
                        "mu_a", "sd_a")
        
        jags_m_l[[count]] <- HBSRM_JAGS_fun(model_name = m,
                                            modelFilename = NA,
                                            jags.data = jags.data, 
                                            jags.parms = jags.parms, 
                                            n.thin = 10,
                                            n.burnin = n.burnin,
                                            n.chains = n.chains)
        count <- count + 1
      }
      
      names(jags_m_l) <- hbsrm
      
      # Model selection using DIC:
      # models within 5 delta DIC from the best fit are equivalent (Pestal et al. 2012)
      DICs <- sapply(jags_m_l,FUN = function(l){l$jagsfit.p$BUGSoutput["DIC"]}) |> unlist()
      DICs <- sort(DICs)
      DICs <- data.frame(models = names(DICs),
                         DIC = DICs)
      rownames(DICs) <- NULL
      DICs$delta <- DICs$DIC - min(DICs$DIC)
      DICs$models <- gsub(".DIC","",DICs$models)
      
      # Check the Trances and density distributions 
      #jags_m_l$Larkin_full$post |> plot()
      
      # Save the posterio densities of the best models
      m_best <- DICs$models[DICs$delta < 5]
      post <- lapply(m_best,FUN = function(m){return(jags_m_l[[m]]$post)})
      names(post) <- m_best
      
      post_export <- post
      post_export$DICs <- DICs
      
      # plot <- plot(post)
      
      saveRDS(post_export,
              file = paste0(wd_output,"/",gsub(" ","_",regionName),"_",speciesAcroHere,"_cyclic_Larkin_DIC_HBSRM_posteriors_priorShift.rds"))
      
      ##### INFERENCE #####
      
      # Gelman and Rubin's convergence diagnostic
      # remove the "deviance" column because it is not a model parameter
      convDiagnostic_combine <- NULL
      for(m in m_best){
        for(l in 1:length(post[[m]])){
          # l <- 1
          post[[m]][[l]] <- post[[m]][[l]][,colnames(post[[m]][[l]]) != "deviance"] 
        }
        convDiagnostic <- as.data.frame(gelman.diag(post[[m]], multivariate = F)$psrf)
        convDiagnostic$parameter <- rownames(convDiagnostic)
        convDiagnostic$cu_name_pse <- NA
        convDiagnostic$cuid <- NA
        convDiagnostic$region <- NA
        convDiagnostic$species_name <- NA
        convDiagnostic$species_acro <- NA
        convDiagnostic$nb <- NA
        convDiagnostic$prSmax <- convDiagnostic$prCV <- NA
        for(r in 1:nrow(convDiagnostic)){
          # r <- 1
          nbHere <- rownames(convDiagnostic)[r]
          nbHere <- sub(".*\\[", "", nbHere) # remove everything before "[" including "["
          nbHere <- sub("\\].*", "", nbHere) #
          
          if(!nbHere %in% c("mu_a",'sd_a')){
            if(length(CUs) == 1){  # there is not number associated with the parameter, e.g. "a" and not "a[1]"
              nbHere <- 1
            }else{
              nbHere <- as.numeric(nbHere)
            }
            CUHere <- CUs[nbHere]
            cuidHere <- CUs_cuid[nbHere]
            sp_nameHere <- conservationunits_decoder$species_name[conservationunits_decoder$cuid == CUs_cuid[nbHere]]
            prSmaxHere <- prSmax[nbHere]
            prCVHere <- prCV[nbHere]
          }else{
            nbHere <- CUHere <- cuidHere <- prSmaxHere <- prCVHere <- sp_nameHere <- ""
          }
          
          regionHere <- region[i_rg]
          sp_acroHere <- speciesAcroHere
          
          convDiagnostic$cu_name_pse[r] <- CUHere
          convDiagnostic$cuid[r] <- cuidHere
          convDiagnostic$region[r] <- regionHere
          convDiagnostic$species_name[r] <- sp_nameHere
          convDiagnostic$species_acro[r] <- sp_acroHere
          convDiagnostic$nb[r] <- nbHere
          convDiagnostic$prSmax[r] <- prSmaxHere
          convDiagnostic$prCV[r] <- prCVHere
        }
        
        rownames(convDiagnostic) <- NULL
        
        # order rows:
        for(char in c("a\\[","b\\[","sd\\[")){
          # char <- "a\\["
          dfToSort <- convDiagnostic[grepl(char,convDiagnostic$parameter) & !is.na(convDiagnostic$cu_name_pse),]
          dfToSort$nb <- as.numeric(dfToSort$nb)
          dfToSort <- dfToSort[order(dfToSort$nb),]
          convDiagnostic[grepl(char,convDiagnostic$parameter) & !is.na(convDiagnostic$cu_name_pse),] <- dfToSort
        }
        
        convDiagnostic <- convDiagnostic[,c("region","species_name","cuid","cu_name_pse","species_acro",
                                            "parameter","Point est.","Upper C.I.",
                                            "prSmax","prCV")]
        
        convDiagnostic$model <- m
        
        if(is.null(convDiagnostic_combine)){
          convDiagnostic_combine <- convDiagnostic
        }else{
          convDiagnostic_combine <- rbind(convDiagnostic_combine,convDiagnostic)
        }
      }

      write.csv(convDiagnostic_combine,
                file = paste0(wd_output,"/",gsub(" ","_",regionName),"_",speciesAcroHere,"_cyclic_Larkin_DIC_HBSRM_convDiagnostic.csv"),
                row.names = F)
      
      post_m <- as.matrix(post$Larkin_13, chain = F)
      post_m <- post_m[,colnames(post_m) != "deviance"] 
      model.probs <- round(cbind(est = colMeans(post_m),
                                 sd = apply(post_m,2,sd),
                                 ci = t(apply(post_m,2,quantile,c(.025,.975)))),
                           digits = 8)
      # model.probs
    } # species loop
  }  # if there is data for this region
} # region loop


