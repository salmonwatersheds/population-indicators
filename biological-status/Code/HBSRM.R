
# Hierarchical Bayesian SR analysis for all regions and conservation units.
# Code adpated from Korman and English (2013).

rm(list = ls())
graphics.off()

# Set directory to /biological-status
if(!grepl(pattern = "biological-status", x = getwd())){
  setwd(dir = paste0(getwd(),"/biological-status"))
}

# Import functions
source("Code/functions.R")

# Load packages
library(R2jags)  # Provides wrapper functions to implement Bayesian analysis in JAGS.
library(modeest) # Provides estimators of the mode of univariate data or univariate distributions.

# Define subdirectories:
wd_Code <- paste0(getwd(),"/Code")
wd_Data <- paste0(getwd(),"/Data")
wd_Figures <- paste0(getwd(),"/Figures")
wd_Output <- paste0(getwd(),"/Output")

# BSC: this below need to be automatized, which is tricky because our names are 
# placed in the dropbox path that leads to the datasets...
wd_Data_input_root <- "C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS"

# Paths to the repositories containing the run reconstruction datasets for each 
# region.
wd_data_regions <- wd_data_regions_fun(wd_root = wd_Data_input_root)

# Import species names and acronyms
species_acronym <- species_acronym_fun()

# Import region names
regions <- regions_fun()

#------------------------------------------------------------------------------#
# User choices
#------------------------------------------------------------------------------#

# Choosing the region
# BSC: This will have to eventually be automatized and eventually allows for 
# multiple regions to be passed on.
region <- regions$Fraser
region <- regions$Yukon
region <- regions$Nass

# **** BSC: issues to solve with *** DELETE CHUNK eventually
# region <- regions$Fraser
# Species <- species_acronym$Pink   # one CU: Fraser River (odd)

# region <- regions$Nass
# Species <- species_acronym$Pink    # two CUs: "Nass-Skeena_Estuary_Even"      "Nass_Portland_Observatory_odd"
# ****

# set the path of the input data sets for that specific region
wd_Data_input <- paste0(wd_data_regions[,region])

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set Species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
Species <- c(species_acronym$Sockeye,    
             species_acronym$Pink,
             species_acronym$Cutthroat,
             species_acronym$Chum)

# If we do not specify the species:
Species <- NULL

# Returns a list with the species and the corresponding path of the _SRdata files
# (the most up to date)
fndata <- SRdata_path_Species_fun(wd = wd_Data_input, Species = Species)

Species <- fndata$Species  # species is updated is was NULL or certain species do not have a file
fndata <- fndata$SRdata

# Set first brood year, "-99" for no constraint
FBYr <- -99

# Set minimum # of SR data points required to be included in the analysis
MinSRpts <- 3 

#----------------------------------------------------------------------------#
# Read in Stock-Recruit Data, run the HBSR model and output parameter estimates
#----------------------------------------------------------------------------#

for(i in 1:length(Species)){
  
  # i <- 1
  
  print(paste0("*** Plot printer for: ",
               colnames(species_acronym[, species_acronym == Species[i],drop=F]),
               " (",Species[i],") ***"))
  
  # Import the priors and counts from the SRdata.txt file. The function retain 
  # CUs with at least MinSRpts nb of data points and update their names in case 
  # the CUID and not the name was used.
  d <- SRdata_fun(path_file = fndata[i], wd_Data = wd_Data, MinSRpts = MinSRpts)
  d_prior <- d$priors
  d <- d$counts

  # organize the data into a year x CU for R and S:
  CUs <- unique(d$CU)
  nCUs <- length(CUs)
  Yrs <- min(d$BY):max(d$BY)
  nYrs <- length(Yrs)

  # unique(d[d$CU == CUs[1],]$BY)
  # unique(d[d$CU == CUs[2],]$BY)
  
  S <- R <- matrix(nrow = nYrs, ncol = nCUs, dimnames = list(Yrs,CUs))
  for(j in 1:nCUs){
    dj <- subset(d,CU == CUs[j])
    S[as.character(dj$BY),j] <- dj$Esc  
    R[as.character(dj$BY),j] <- dj$Rec
    
    # S[1:Nyrs[j],j] <- d1$Esc      # BSC: previous code
    # R[1:Nyrs[j],j] <- d1$Rec
  }
  
  # save the S and R matrix
  SR_l <- list(S,R)
  names(SR_l) <- c("S","R")
  saveRDS(SR_l,
          file = paste0("Output/",region,"_",Species[i],"_SR_matrices.rds"))
  
  # Set priors on b:
  # Previous method using the values in the _SRdata.txt file
  # prSmax <- d_prior$prSmax
  # prCV <- d_prior$prCV
  # prmub <- log(1/prSmax)    # convert mean prior on Smax to log b for winbugs model
  # prtaub <- 1/prCV^2				# convert from cv to tau
  
  #### Estimate a and b by linreg and plot
  if(.Platform$OS.type == "windows"){      # BSC: what to do with Linux? : x11()?
    windows()                              # Also do we want it to open in a new window vs in Rstudio?
  }else{
    quartz()
  }

  # LnRS <- log(R/S)   # BSC: now is created inside the function with R and S to limit the number of parameters to pass in
  # inipars <- LinReg(Nyrs,LNRS,S,R,StNames)
  # Display the RS plot and output the estimated parameter values:
  inipars <- linRegRicker_fun(S = S, R = R, plot_figures = T)
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
                    n.iter = 6000, #100000, 
                    model.file = modelFilename, 
                    n.burnin = 2000, #5000, 
                    n.chains = 6)
  
  endtime <- proc.time()-ptm
  endtime[3]/60
  
  post <- as.mcmc(jagsfit.p)
  
  # BSC: it is exported in /Output for now but these should be exported someWhere
  # else becaue they are probably too big for github.
  saveRDS(post,
          file = paste0("Output/",region,"_",Species[i],"_posteriors_priorShift.rds"))
  
  # save the name of the corresponding CUs:
  CUs_df <- data.frame(CU = CUs)
  write.csv(x = CUs_df,
            file = paste0("Output/",region,"_",Species[i],"_CUs_names.csv"), 
            row.names = F)

  ##### INFERENCE ##### BSC: is that useful?
  mypost <- as.matrix(post, chain = F)
  gelman.diag(post, multivariate = F)
  model.probs <- round(cbind(est = colMeans(mypost),
                             sd = apply(mypost,2,sd),
                             ci = t(apply(mypost,2,quantile,c(.025,.975)))),
                       digits = 8)
  model.probs
}

