

#'******************************************************************************
#' The goal of the script is to conduct a hierarchical Bayesian spawner recruits 
#' (HBSR) R analysis for all regions and conservation units.
#' Code adpated from Korman and English (2013).
#' 
#' Files imported (from dropbox):
#' - species_SRdata_date.txt --> TO UPDATE
#' 
#' Files produced: 
#' - output/region_species_SR_matrices.rds
#' - output/region_posteriors_priorShift.rds
#' - output/region_species_CUs_names.csv
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

# The datasets to input were outputted by other scripts 
wd_data_input <- wd_output

# Import functions for this specific project
source("Code/functions.R")

# Load packages
library(R2jags)  # Provides wrapper functions to implement Bayesian analysis in JAGS.  ??? needed ?
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
fromDatabase <- F
update_recruitsperspawner_csv <- F

recruitsperspawner <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[3],
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

# Set first brood year, "-99" for no constraint
FBYr <- -99

# Set minimum nb of SR data points required to be included in the analysis
MinSRpts <- 3 

#----------------------------------------------------------------------------#
# Read in Stock-Recruit Data, run the HBSR model and output parameter estimates
#----------------------------------------------------------------------------#

for(i_rg in 1:length(region)){
  
  # i_rg <- 7
  
  #*** OLD CODE BELOW TO REMOVE EVENTUALLY ***

  # set the path of the input data sets for that specific region
  # wd_data_input <- paste0(wd_data_regions[,region[i_rg]])
  
  # Returns a list with the species and the corresponding path of the _SRdata files
  # (the most up to date)
  # fndata <- SRdata_path_species_fun(wd = wd_data_input,
  #                                   species = species,
  #                                   species_all = species_all)
  
  # species <- fndata$species  # species is updated is was NULL or certain species do not have a file
  # fndata <- fndata$SRdata
  
  #*** OLD CODE above TO REMOVE EVENTUALLY ***
  
  recruitsperspawner_rg <- recruitsperspawner[recruitsperspawner$region == region[i_rg],]
  
  species <- unique(recruitsperspawner_rg$species_name)
  
  species <- species[species != "Steelhead"]
  
  species_acro <- sapply(X = species,FUN = function(sp){
    species_acronym_df$species_acro[species_acronym_df$species_name == sp]
    })
  
  regionName <- region[i_rg]
  if(region[i_rg] == "Vancouver Island & Mainland Inlets"){
    regionName <- "VIMI"
  }
  
  # recruitsperspawner[recruitsperspawner$species_name == "Coho" & 
  #                      recruitsperspawner$region == "Fraser",]
  # unique(recruitsperspawner$species_name)
  # unique(recruitsperspawner$region)
  if(sum(!is.na(recruitsperspawner_rg$spawners)) == 0 | sum(!is.na(recruitsperspawner_rg$recruits)) == 0){
    
    print(paste0("*** There is no data in recruitsperspawner.csv for salmon in ",region[i_rg]," ***"))
    
  }else{
    
    for(i_sp in 1:length(unique(species_acro))){
      
      # i_sp <- 4
      speciesAcroHere <- unique(species_acro)[i_sp]
      speciesHere <- species_acronym_df$species_name[species_acronym_df$species_acro %in% speciesAcroHere]
      
      recruitsperspawner_rg_sp <- recruitsperspawner_rg[recruitsperspawner_rg$species_name %in% speciesHere,]
      
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
      
      # organize the data into a year x CU for R and S:
      CUs <- unique(recruitsperspawner_rg_sp$cu_name_pse)
      CUs_cuid <- sapply(X = CUs,FUN = function(cu){unique(recruitsperspawner_rg_sp$cuid[recruitsperspawner_rg_sp$cu_name_pse == cu])})
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
      #' TODO: looks like this did not work: the_SR_matrices.rds still contains CUs with only NAs, whose names are not in the filtered CUs
      #' This is dealt with in benchamrks_HBSRM.R but still need to be addressed here.
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
      
      # nameFile <- paste0(gsub(" ","_",region[i_rg]),"_",
      #                    gsub(" ","_",species[i_sp]),"_",
      #                    species_acro[i_sp],"_SR_matrices.rds")
      SR_l$R <- R
      SR_l$S <- S
      
      saveRDS(SR_l,
              file = paste0(wd_output,"/",gsub(" ","_",regionName),"_",speciesAcroHere,"_SR_matrices.rds"))
      
      # Set priors on b:
      # Previous method using the values in the _SRdata.txt file
      # prSmax <- d_prior$prSmax
      # prCV <- d_prior$prCV
      # prmub <- log(1/prSmax)    # convert mean prior on Smax to log b for winbugs model
      # prtaub <- 1/prCV^2				# convert from cv to tau
      
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
      inipars <- linRegRicker_fun(S = S, R = R, plot_figures = show_figures)
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
        ln_Smsr = log(1/inipars$b))
      # prmub = prmub, # prior on mu for b
      # prtaub = prtaub #prior on tau for b
      # ) # cov - not sure what this is
      
      jags.parms <- c("a", "b", "sd", "mu_a", "sd_a")
      
      # Definition of the model:
      modelFilename = "Bayes_SR_model.txt"
      cat("
    model{
    
    	# Hyper priors
    	log_mu_a ~ dnorm(0.5, 1.0E-6) # SP: Note that this parameter was confusing as it was defined as mu_a previously; changed this.
    	mu_a <- exp(log_mu_a)
    	tau_a ~ dgamma(0.5, 0.5) 
    	sd_a <- pow(tau_a, -0.5)
    	
    	for(i in 1:nCUs) {	# For each CU, draw estimates from hyperdistribution
    	
    		a[i] ~ dlnorm(log_mu_a, tau_a) # Hyper distribution on alpha
    		
    		# b[i] ~ dlnorm(prmub[i], prtaub[i])	# prior on CU-dependent b
    		# Getting very broad estimates of b, try to contstrain by fitting
    		# S at max recruits (Smsr) instead of b and truncating at 1e10
    		
    		Smsr[i] ~ dlnorm(ln_Smsr[i], pow(1, -2)) T(0, 1e10)
    		b[i] <- 1/Smsr[i]
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
                        n.iter = 100000, 
                        model.file = modelFilename, 
                        n.burnin = 5000, 
                        n.chains = 6)
      
      endtime <- proc.time()-ptm
      endtime[3]/60
      
      post <- as.mcmc(jagsfit.p)
      
      # BSC: it is exported in /Output for now but these should be exported someWhere
      # else becaue they are probably too big for github.
      saveRDS(post,
              file = paste0(wd_output,"/",gsub(" ","_",regionName),"_",speciesAcroHere,"_posteriors_priorShift.rds"))
      
      # save the name of the corresponding CUs:
      #' TODO: remove as this CSV might/should not be used if future, these CUs names
      #' should be taken from the REGION_SPECIESACRO__SR_matrices.rds instead to 
      #' reduce the risk of mistakes.
      CUs_df <- data.frame(CU = CUs)
      write.csv(x = CUs_df,
                file = paste0(wd_output,"/",gsub(" ","_",regionName),"_",speciesAcroHere,"_CUs_names.csv"), 
                row.names = F)
      
      ##### INFERENCE ##### BSC: is that useful?
      mypost <- as.matrix(post, chain = F)
      gelman.diag(post, multivariate = F)
      model.probs <- round(cbind(est = colMeans(mypost),
                                 sd = apply(mypost,2,sd),
                                 ci = t(apply(mypost,2,quantile,c(.025,.975)))),
                           digits = 8)
      model.probs
    } # species loop
  }  # if there is data for this region
} # region loop



