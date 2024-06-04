

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
#' - output/REGION_SPECIES_priorShift.rds
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
  
  species <- unique(recruitsperspawner_rg$species_name)
  
  species <- species[species != "Steelhead"] # QUESTION: is it still relevant?
  
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
      
      recruitsperspawner_rg_sp <- recruitsperspawner_rg[recruitsperspawner_rg$species_name %in% speciesHere,]
      
      conservationunits_decoder_rg_sp <- conservationunits_decoder[conservationunits_decoder$region == region[i_rg] &
                                                                     conservationunits_decoder$species_name %in% speciesHere,]
      
      print(paste0("*** Plot for: ",region[i_rg]," - ",speciesAcroHere," ***"))
      
      # Import the priors and counts from the SRdata.txt file. The function retain 
      # CUs with at least MinSRpts nb of data points and update their names in case 
      # the CUID and not the name was used.
      # d_old <- SRdata_fun(path_file = fndata[i_sp], wd_data = wd_data, MinSRpts = MinSRpts)
      # d_prior <- d_old$priors
      # d_old <- d_old$counts
      # CUs_old <- unique(d_old$CU)
      # nCUs_old <- length(CUs_old)
      # Yrs_old <- min(d_old$BY):max(d_old$BY)
      # nYrs_old <- length(Yrs_old)
      
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
        
        # S[1:Nyrs[j],j] <- d1$Esc      # BSC: previous code
        # R[1:Nyrs[j],j] <- d1$Rec
      }
      
      # S_old <- R_old <- matrix(nrow = nYrs_old, ncol = nCUs_old, dimnames = list(Yrs_old,CUs_old))
      # for(j in 1:nCUs_old){
      #   # j <- 1
      #   # previous code
      #   dj_old <- subset(d_old,CU == CUs_old[j])
      #   S_old[as.character(dj_old$BY),j] <- dj_old$Esc
      #   R_old[as.character(dj_old$BY),j] <- dj_old$Rec
      #   
      #   # S[1:Nyrs[j],j] <- d1$Esc      # BSC: previous code
      #   # R[1:Nyrs[j],j] <- d1$Rec
      # }
      
      # col <- 4
      # cbind(S[,col],R[,col])#[28:nrow(S),]
      # cbind(S_old[,col],R_old[,col])
      
      # save the S and R matrix
      # SR_l <- list(S,R)
      # names(SR_l) <- c("S","R")
      
      # remove the row with NAs in S but not R and vice versa of a same CU
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
      priors_HBSRmodel_rg_sp <- priors_HBSRmodel[priors_HBSRmodel$region == region[i_rg] &
                                                   priors_HBSRmodel$species %in% speciesHere,]
      
      CUs_priors <- data.frame(region = region[i_rg],
                               species_acro = unique(species_acro)[i_sp],
                               species = NA,
                               cuid = NA,
                               cu_pse = CUs,
                               prSmax = NA,
                               prCV = NA)
      
      for(r in 1:nrow(CUs_priors)){
        # r <- 1
        cond <- conservationunits_decoder_rg_sp$cu_name_pse == CUs_priors$cu_pse[r]
        cuidHere <- conservationunits_decoder_rg_sp$cuid[cond]
        speciesHere <- conservationunits_decoder_rg_sp$species_name[cond]
        
        # in case multiple instances are returned (happens with pooled CUs)
        if(length(cuidHere) > 1){

          cuidPooledHere <- conservationunits_decoder_rg_sp$pooledcuid[cond]
          
          if(length(unique(cuidPooledHere)) == 1){
            cuidHere <- unique(cuidPooledHere)
            speciesHere <- unique(speciesHere)

          }else{
            print(paste0("There are multiple different CUs here: i_rg: ",i_rg,
                         " ; i_sp: ",i_sp," ; r: ",r))
            print(conservationunits_decoder_rg_sp[cond,])
          }
        }
        
        cond <- priors_HBSRmodel_rg_sp$cu_name_pse == CUs_priors$cu_pse[r]
        prSmaxHere <- priors_HBSRmodel_rg_sp$prSmax[cond]
        prCVHere <- priors_HBSRmodel_rg_sp$prCV[cond]

        if(length(prSmaxHere) == 0){
          prSmaxHere <- prCVHere <- NA
        }
        
        CUs_priors$prSmax[r] <- prSmaxHere
        CUs_priors$prCV[r] <- prCVHere
        CUs_priors$species[r] <- speciesHere
        CUs_priors$cuid[r] <- cuidHere
      }
      
      #' if certain CUs do not have prior estimate, check why, eventually remove
      #' them if appropriate (e.g. they are cyclic) or find a value if possible.
      CuToRemove <- c()
      if(sum(is.na(CUs_priors$prSmax)) > 0){
        
        CUs_priors_noData <- CUs_priors[is.na(CUs_priors$prSmax),]
        CuToRemove <- CUs_priors_noData$cu_pse[grepl("cyclic",CUs_priors_noData$cu_pse)]
        
        CUs_priors_noData <- CUs_priors_noData[!CUs_priors_noData$cu_pse %in% CuToRemove,]
        
        if(nrow(CUs_priors_noData) > 0){
          
          #' Define the prSmax and prCV following Korman and English 2013:
          #' https://salmonwatersheds.ca/document_library_files/lib_318.pdf
          #' - prSmax = the average escapement
          #' - prCV = 10 (highly uninformative) or 1 (minimal informative) (Steph:
          #' "they start with uninformative (CV = 10), and then only use CV = 1 
          #' for those CUs that had convergence issues).
          
          for(r in 1:nrow(CUs_priors_noData)){
            # r <- 1
            CUHere <- CUs_priors_noData$cu_pse[r]
            SHere <- S[,CUHere]
            CUs_priors$prSmax[CUs_priors$cu_pse == CUHere] <- mean(SHere,na.rm = T)
            CUs_priors$prCV[CUs_priors$cu_pse == CUHere] <- 10
          }
        }
      }
      #' filter CUs again
      S <- S[,!colnames(S) %in% CuToRemove, drop = F]
      R <- R[,!colnames(R) %in% CuToRemove, drop = F]
      CUs <- CUs[!CUs %in% CuToRemove]
      CUs_cuid <- sapply(X = CUs,FUN = function(cu){
        unique(recruitsperspawner_rg_sp$cuid[recruitsperspawner_rg_sp$cu_name_pse == cu])
      })
      nCUs <- length(CUs)
      SR_l$R <- R
      SR_l$S <- S
      CUs_priors <- CUs_priors[!CUs_priors$cu_pse %in% CuToRemove,]
      
      # Define corresponding parameters to feed the HBSRM:
      # prSmax <- d_prior$prSmax
      # prCV <- d_prior$prCV
      prSmax <- CUs_priors$prSmax
      prCV <- CUs_priors$prCV
      prmub <- log(1/prSmax)    # convert mean prior on Smax to log b for winbugs model
      # prtaub <- 1/prCV^2				# convert from cv to tau --> BSC: should be prtaub <- prmub^2/prCV^2 ???
      prtaub <- prmub^2/prCV^2
      
      saveRDS(SR_l,
              file = paste0(wd_output,"/",gsub(" ","_",regionName),"_",speciesAcroHere,"_SR_matrices.rds"))
      
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
      # BSC: linRegRicker_fun() was LinReg() and estSR()
      # ***SP: Check with Eric about priors on b. ***
      
      # BSC: this is temporary: see if it is possible to treat each CU separately in the
      # JAGS chunk in case there are NAs that differ among CU.
      # rowToKeep <- apply(X = S, MARGIN = 1, FUN = function(x){sum(is.na(x)) == 0})
      # S <-  S[rowToKeep,,drop = F]
      # rowToKeep <- apply(X = R, MARGIN = 1, FUN = function(x){sum(is.na(x)) == 0})
      # R <-  R[rowToKeep,,drop = F]
      
      # Yrs <- as.numeric(rownames(R))
      # nYrs <- length(Yrs)
      
      # LnRS <- log(R/S)
      
      #------------------------------------------------------------------------------#
      #  Bayes model defined with rjags
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
      
      jags.parms <- c("a", "b", 
                      "sd",     # the sd for the loglikelihood function
                      "mu_a", "sd_a")
      
      # what is sd?  --> the sd for the likelihood --> not in the tech report
      # why sd_bi are not estimated?
      # why taking the exp(sd) in Sgen.optim()?
      
      # Definition of the model:
      modelFilename = "Bayes_SR_model.txt"
      cat("
        model{
        
        	# Hyper priors
        	log_mu_a ~ dnorm(0.5, 1.0E-6) # SP: Note that this parameter was confusing as it was defined as mu_a previously; changed this.
        	mu_a <- exp(log_mu_a)
        	tau_a ~ dgamma(0.5, 0.5) 
        	sd_a <- pow(tau_a, -0.5) # = sigma_a = sqrt(1/tau_a)
        	
        	for(i in 1:nCUs) {	# For each CU, draw estimates from hyperdistribution
        	
        		# a[i] ~ dlnorm(log_mu_a, tau_a) # Hyper distribution on alpha --> dlnorm(mu_a,tau_a) instead ??? 
        		# a[i] ~ dlnorm(mu_a, tau_a) # TOCHANGE: CONFUSION WITH ALPHA = exp(a) --> a[i] ~ dnorm(mu_a, tau_a) ???
        		
        		a[i] ~ dnorm(mu_a, tau_a) # --> FINAL CALL from Population meeting 30/01/2024.
        		
        		b[i] ~ dlnorm(prmub[i], prtaub[i])	# prior on CU-dependent b
        		
        		# Getting very broad estimates of b, try to contstrain by fitting
        		# S at max recruits (Smsr) instead of b and truncating at 1e10
        		# Smsr[i] ~ dlnorm(ln_Smsr[i], pow(1, -2)) T(0, 1e10)
        		# b[i] <- 1/Smsr[i]
        		
        		# BSC: sigma for the model likelihood estimation
        		sd[i] ~ dunif(0.05, 10)
        		tau[i] <- pow(sd[i], -2)	
        	}
        	
        	for(i in 1:nCUs){
        		for(j in 1:nYrs){
        		
        		 	# Model prediction for log R/S based on estimated parameters
        		 	pred_lnRS[j, i] <- a[i] - b[i] * S[j, i]
        		 	
        		 	# Likelihood
        		 	obs_lnRS[j, i] ~ dnorm(pred_lnRS[j, i], tau[i])
        		}
        	}
        }", fill = TRUE, file = modelFilename)
      
      # Run Model
      print("Running Parallel")
      # **SP: Why have this message when it's not actually running in parallel?
      # **BSC: TODO: get the parallele to work on windows, MAC and Linuxf
      # https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
      
      ptm = proc.time()
      
      jagsfit.p <- jags(data = jags.data,  
                        parameters.to.save = jags.parms,
                        n.thin = 10,                     # thinning rate
                        n.iter = n.iter,  # n.iter 100000
                        model.file = modelFilename, 
                        n.burnin = n.burnin, # 5000
                        n.chains = n.chains) # 6
      
      endtime <- proc.time()-ptm
      endtime[3]/60
      
      post <- as.mcmc(jagsfit.p)
      
      # plot <- plot(post)
      
      # BSC: it is exported in /Output for now but these should be exported someWhere
      # else becaue they are probably too big for github.
      saveRDS(post,
              file = paste0(wd_output,"/",gsub(" ","_",regionName),"_",speciesAcroHere,"_HBSRM_posteriors_priorShift.rds"))
      
      # save the name of the corresponding CUs:
      #' TODO: remove as this CSV might/should not be used if future, these CUs names
      #' should be taken from the REGION_SPECIESACRO__SR_matrices.rds instead to 
      #' reduce the risk of mistakes.
      CUs_df <- data.frame(CU = CUs)
      # write.csv(x = CUs_df,
      #           file = paste0(wd_output,"/",gsub(" ","_",regionName),"_",speciesAcroHere,"HBSR_CUs_names.csv"), 
      #           row.names = F)
      
      ##### INFERENCE #####
      
      # Gelman and Rubin's convergence diagnostic
      # remove the "deviance" column because it is not a model parameter
      for(l in 1:length(post)){
        # l <- 1
        post[[l]] <- post[[l]][,colnames(post[[l]]) != "deviance"] 
      }
      convDiagnostic <- as.data.frame(gelman.diag(post, multivariate = F)$psrf)
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

      write.csv(convDiagnostic,
                file = paste0(wd_output,"/",gsub(" ","_",regionName),"_",speciesAcroHere,"_HBSRM_convDiagnostic.csv"),
                row.names = F)
      
      
      post_m <- as.matrix(post, chain = F)
      post_m <- post_m[,colnames(post_m) != "deviance"] 
      model.probs <- round(cbind(est = colMeans(post_m),
                                 sd = apply(post_m,2,sd),
                                 ci = t(apply(post_m,2,quantile,c(.025,.975)))),
                           digits = 8)
      # model.probs
    } # species loop
  }  # if there is data for this region
} # region loop


