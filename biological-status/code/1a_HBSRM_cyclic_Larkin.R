

#'******************************************************************************
#' The goal of the script is to conduct a hierarchical Bayesian spawner recruits 
#' (HBSR) R analysis for individual cyclic CU using the Larkin modelling approach
#' as descrined in (Grant et al. 2020).
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
#' - output/REGION_SPECIES_cyclic_Larkin_DIC_HBSRM_posteriors_priorShift.rds
#' - output/REGION_SPECIES_cyclic_Larkin_DIC_HBSRM_convDiagnostic.csv
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

# The Gelman and Rubin's convergence of parameters Rc
# if Rc > Rc_max_cut --> prCV = 1 instead of 10 
Rc_max_cut <- 1.1 # 1.2

# Set first brood year, "-99" for no constraint
FBYr <- -99

# Set minimum nb of SR data points required to be included in the analysis
MinSRpts <- 3 

# Set the HBSR number of simulations, burning runs and chains:
n.iter <- 100000 # 100000
n.burnin <- 6000 # 3000 # 
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
      cuids <- sapply(X = CUs,
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
      cuids <- sapply(X = CUs,FUN = function(cu){
        unique(recruitsperspawner_rg_sp$cuid[recruitsperspawner_rg_sp$cu_name_pse == cu])
      })
      nCUs <- length(CUs)
      SR_l$R <- R
      SR_l$S <- S
      
      # Export the SR matrices
      saveRDS(SR_l,
              file = paste0(wd_output,"/intermediate/",gsub(" ","_",regionName),"_",speciesAcroHere,"_cyclic_SR_matrices.rds"))
      
      #------------------------------------------------------------------------------#
      #'*  Bayes model defined with rjags - Ricker model *
      #------------------------------------------------------------------------------#
      
      #'* Set priors on b: prSmax and prCV *
      # select the prior for these CUs and check those that do not have values
      # cond_cuid <- priors_HBSRmodel$cuid %in% cuid_cycl
      # priors_HBSRmodel_rg_sp <- priors_HBSRmodel[cond_cuid,]
      # NOTE: THERE IS NO PRIOR FOR THESE CYCLIC CUs; so define the priors here 
      # as a TEMPORARY solution
      
      #' Define the prSmax and prCV following Korman and English 2013:
      #' https://salmonwatersheds.ca/document_library_files/lib_318.pdf
      #' - prSmax = the average escapement
      #' - prCV = 10 (highly uninformative) or 1 (minimal informative) (Steph:
      #' "they start with uninformative (CV = 10), and then only use CV = 1 
      #' for those CUs that had convergence issues).
      prSmax <- apply(SR_l$S,2,function(s){return(mean(s,na.rm = T))})
      prmub <- log(1/prSmax)    # convert mean prior on Smax to log b for winbugs model
      prCV <- rep(10,nCUs) #    
      prtaub <- prmub^2/prCV^2 # --> b[i] ~ dlnorm(prmub[i], prtaub[i])
      
      # Replace NAs with a value so that the model can run. Note that obs_lnRS still
      # has NAs, so the it does not matter what pred_lnRS is for these cases because
      # they are not considered in the likelihood calcutation (due to NAs in obs_lnRS).
      S[is.na(S)] <- -99    # could put any value
      nYrs <- nrow(S)
      
      # The name of the different model to pass to the HBSRM_JAGS_fun function:
      hbsrm <-       c("Ricker",
                       "Larkin_123",
                       "Larkin_1","Larkin_2","Larkin_3",
                       "Larkin_12","Larkin_13","Larkin_23")
      
      # List that will contain the best models and the Ricker model (if not in the 
      # set of best models) and the DIC table of all the model.
      # For each CU
      post_export_l <- list()
      priors_convergence_df <- NULL
      for(cu in CUs){
        # cu <- CUs[4]
        
        print(paste0("* CU: ",cu))
        
        S_cu <- S[,colnames(S) == cu, drop = F]
        R_cu <- S[,colnames(R) == cu, drop = F]
        
        # what is sd?  --> the sd for the likelihood --> not in the tech report
        # why sd_bi are not estimated?
        # why taking the exp(sd) in Sgen.optim()?
        
        jags_m_l <- list()
        count <- 1
        priors_convergence_m_df <- NULL
        for(m in hbsrm){
          # m <- hbsrm[1]
          
          print(paste0("** Model: ",m))
          print("")
          
          if(grepl("Larkin",m)){
            m_nb <- sapply(1:3,function(nb){grepl(nb,m)})
            par_Larkin <- c("b1","b2","b3")[m_nb]
            
          }else{ # Ricker model
            par_Larkin <- NULL
          }
          
          jags.parms <- c("a", "b", par_Larkin,
                          "sd",     # the sd for the loglikelihood function
                          "mu_a", "sd_a")
          
          # record the prior values
          prSmax_here <- prSmax[which(cu == CUs)]
          prmub_here <- prmub[which(cu == CUs)]
          prCV_here <- prCV[which(cu == CUs)]
          prtaub_here <- prtaub[which(cu == CUs)]
          
          # jags inputs:
          jags.data <- list(
            nCUs = 1, # nCUs,
            nYrs = nYrs,
            # rowsToKeep = rowsToKeep,
            obs_lnRS = log(R_cu/S_cu),  # data on observed log(recruits/spawners) - matrix nYrs x nCU
            S = S_cu,         # data on observed spawners - matrix nYrs x nCU 
            # ln_Smsr = log(1/inipars$b),
            prmub = prmub_here, # prior on mu for b
            prtaub = prtaub_here # prior on tau for b
          ) # cov - not sure what this is
          
          jags_m_here <- HBSRM_JAGS_fun(model_name = m,
                                        modelFilename = NA,
                                        jags.data = jags.data, 
                                        jags.parms = jags.parms, 
                                        n.thin = 10,
                                        n.burnin = n.burnin,
                                        n.chains = n.chains)
          print("")
          
          # check convergence of parameters with Gelman and Rubin's Rc
          convergence <- gelman.diag(jags_m_here$post)$psrf
          cond_deviance <- rownames(convergence) == "deviance"
          convergence <- convergence[!cond_deviance,] # remove the "deviance" column because it is not a model parameter
          if(any(convergence[,"Point est."] > Rc_max_cut)){
            
            prCV_here <- 1  # use minimally informative prior instead of less informative 10 
            prtaub_here <- prmub_here^2/prCV_here^2
            jags.data$prtaub <- prtaub_here
            
            print(convergence)
            print(paste0("Running model with prCV = 1 instead of 10 because Rc > ",Rc_max_cut,":"))
            print("")
            
            jags_m_here <- HBSRM_JAGS_fun(model_name = m,
                                          modelFilename = NA,
                                          jags.data = jags.data, 
                                          jags.parms = jags.parms, 
                                          n.thin = 10,
                                          n.burnin = n.burnin,
                                          n.chains = n.chains)
            print("")
            
            convergence <- gelman.diag(jags_m_here$post)$psrf
            cond_deviance <- rownames(convergence) == "deviance"
            convergence <- convergence[!cond_deviance,]
            
            print("Resulting convergence Rc:")
            print(convergence)
            if(any(convergence[,"Point est."] > Rc_max_cut)){
              print("!!!There is still convergence issue!!!")
            }
            print("")
          }
          
          # NOTE: DIC might not be able to compare models with different prCV
          # To investigate
          
          jags_m_l[[count]] <- jags_m_here
          
          priors_convergence_here <- data.frame(region = region[i_rg], 
                                                species_name = species[i_sp], 
                                                cu_name_pse = cu,
                                                cuid = cuids[which(cu == CUs)],
                                                model = m,
                                                prSmax = prSmax_here,
                                                prmub = prmub_here,
                                                prCV = prCV_here,
                                                prtaub = prtaub_here,
                                                Rc_max = max(convergence[,"Point est."]),
                                                DIC_delta = NA)
        
          if(is.null(priors_convergence_m_df)){
            priors_convergence_m_df <- priors_convergence_here
          }else{
            priors_convergence_m_df <- rbind(priors_convergence_m_df,
                                             priors_convergence_here)
          }
          
          count <- count + 1
          
        } # for each model
        
        names(jags_m_l) <- hbsrm
        
        row.names(priors_convergence_m_df) <- NULL
        
        #' Model selection using using DIC:
        #' 1) retain only the model that converged with prCV = 10
        #'    if none --> keep all the models and go to 2)
        #' 2) calculate DIC to the remaining models and retain those within 
        #'    5 delta DIC
        
        #' 1) Retain the models that converged with prCV = 10 (the non-informative prior)
        priors_convergence_m_df$exclude <- F
        cond <- priors_convergence_m_df$prCV == 10
        if(any(cond)){
          priors_convergence_m_df$exclude[!cond] <- T
        }
        
        #' 2) Model selection using DIC:
        # models within 5 delta DIC from the best fit are equivalent (Pestal et al. 2012)
        models_kept <- priors_convergence_m_df$model[!priors_convergence_m_df$exclude]
        DICs <- sapply(jags_m_l[models_kept],FUN = function(l){l$jagsfit.p$BUGSoutput["DIC"]}) |> unlist()
        DICs <- sort(DICs)
        DICs <- data.frame(models = names(DICs),
                           DIC = DICs)
        rownames(DICs) <- NULL
        DICs$delta <- DICs$DIC - min(DICs$DIC)
        DICs$models <- gsub(".DIC","",DICs$models)
        
        # sort and fill priors_convergence_m_df
        priors_convergence_m_df$DIC_delta <- sapply(X = priors_convergence_m_df$model, 
                                                    FUN = function(m){
                                                      cond <- DICs$models == m
                                                      if(any(cond)){
                                                        out <- DICs$delta[cond]
                                                      }else{
                                                        out <- NA
                                                      }
                                                      return(out)
                                                    })
        priors_convergence_m_df <- priors_convergence_m_df[order(priors_convergence_m_df$DIC_delta),]
        
        if(is.null(priors_convergence_df)){ # not really needed
          priors_convergence_df <- priors_convergence_m_df
        }else{
          priors_convergence_df <- rbind(priors_convergence_df,
                                         priors_convergence_m_df)
        }
        
        # Check the Trances and density distributions 
        #jags_m_l$Larkin_full$post |> plot()
        
        # Save the posterio densities of the best models
        # m_best <- DICs$models[DICs$delta < 5 | DICs$models == "Ricker"]
        m_best <- DICs$models[DICs$delta < 5 & !is.na(DICs$delta)]
        post <- lapply(m_best,FUN = function(m){return(jags_m_l[[m]]$post)})
        names(post) <- m_best
        
        post_export <- post
        post_export$DICs <- DICs
        post_export$convergence_prior_DIC <- priors_convergence_m_df
        
        post_export_l[[which(cu == CUs)]] <- post_export
      }
      names(post_export_l) <- CUs
      
      # priors_convergence_df # just to see all the models

      # plot <- plot(post)
      file <- paste0(wd_output,"/intermediate/",gsub(" ","_",regionName),"_",speciesAcroHere,
                     "_cyclic_Larkin_DIC_HBSRM_posteriors_priorShift.rds")
      saveRDS(post_export_l, file = file)
      
      ##### INFERENCE #####
      
      # To check convergence visually
      # gelman.plot(post[[1]])
      
      # Gelman and Rubin's convergence diagnostic
      # remove the "deviance" column because it is not a model parameter
      # Brooks and Gelman (1998) suggest that diagnostic Rc > 1.2 for any of the
      # model parameters should indicate non-convergence [...] In practice, a more
      # stringent rule of Rc < 1.1 is often used.
      # https://www.stata.com/features/overview/gelman-rubin-convergence-diagnostic/
      convDiagnostic_combine <- NULL
      for(cu in CUs){
        # cu <- CUs[1]
        # cu <- "Shuswap-Early Summer (cyclic)"
        
        m_best <- names(post_export_l[[cu]])
        m_best <- m_best[! m_best %in% c("DICs","convergence_prior_DIC")]
        post <- post_export_l[[cu]][m_best]
        
        for(m in m_best){
          # m <- m_best[1]
          
          # remove parameter "deviance"
          for(l in 1:length(post[[m]])){ # nb of chains
            # l <- 1
            post[[m]][[l]] <- post[[m]][[l]][,colnames(post[[m]][[l]]) != "deviance"] 
          }
          
          convDiagnostic <- as.data.frame(gelman.diag(post[[m]], multivariate = F)$psrf)
          convDiagnostic$parameter <- rownames(convDiagnostic)
          convDiagnostic$region <- region[i_rg]
          cond_cuid <- conservationunits_decoder$cuid == cuids[which(cu == CUs)]
          convDiagnostic$species_name <- conservationunits_decoder$species_name[cond_cuid]
          convDiagnostic$cu_name_pse <- cu
          convDiagnostic$cuid <- cuids[which(cu == CUs)]
          convDiagnostic$species_acro <- speciesAcroHere
          convDiagnostic$model <- m
          convDiagnostic$prSmax <- prSmax[which(cu == CUs)]
          cond <- priors_convergence_df$cuid ==  cuids[which(cu == CUs)] & priors_convergence_df$model == m
          convDiagnostic$prCV <- priors_convergence_df$prCV[cond]
          convDiagnostic$DIC_delta <- round(priors_convergence_df$DIC_delta[cond],3)
          convDiagnostic$converged <- all(convDiagnostic$`Point est.` < Rc_max_cut)
          
          rownames(convDiagnostic) <- NULL
          
          # order rows:
          # for(char in c("a\\[","b\\[","sd\\[")){
          #   # char <- "a\\["
          #   dfToSort <- convDiagnostic[grepl(char,convDiagnostic$parameter) & !is.na(convDiagnostic$cu_name_pse),]
          #   dfToSort$nb <- as.numeric(dfToSort$nb)
          #   dfToSort <- dfToSort[order(dfToSort$nb),]
          #   convDiagnostic[grepl(char,convDiagnostic$parameter) & !is.na(convDiagnostic$cu_name_pse),] <- dfToSort
          # }
          
          convDiagnostic <- convDiagnostic[,c("region","species_name","cuid","cu_name_pse","species_acro",
                                              "parameter","Point est.","Upper C.I.","converged",
                                              "prSmax","prCV","DIC_delta")]
          
          if(is.null(convDiagnostic_combine)){
            convDiagnostic_combine <- convDiagnostic
          }else{
            convDiagnostic_combine <- rbind(convDiagnostic_combine,convDiagnostic)
          }
        }
      }
 
      write.csv(convDiagnostic_combine,
                file = paste0(wd_output,"/intermediate/",gsub(" ","_",regionName),"_",speciesAcroHere,"_cyclic_Larkin_DIC_HBSRM_convDiagnostic.csv"),
                row.names = F)
      
      convDiagnostic_combine[convDiagnostic_combine$`Point est.` > 1.2,]
      convDiagnostic_combine[convDiagnostic_combine$`Point est.` > 1.1,]
      
      # post_m <- as.matrix(post$Larkin_13, chain = F)
      # post_m <- post_m[,colnames(post_m) != "deviance"] 
      # model.probs <- round(cbind(est = colMeans(post_m),
      #                            sd = apply(post_m,2,sd),
      #                            ci = t(apply(post_m,2,quantile,c(.025,.975)))),
      #                      digits = 8)
      # model.probs
    } # species loop
  }  # if there is data for this region
} # region loop

gelman.plot(post[[1]])

