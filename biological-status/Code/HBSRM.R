
# Hierarchical Bayesian SR analysis for all regions and conservation units.
# Code adpated from Korman and English (2013).

rm(list=ls())
graphics.off()

# Set directory to /biological-status
setwd(dir = paste0(getwd(),"/biological-status"))

# Import functions
source("Code/linear_SR_func.R")

library(R2jags)  # Provides wrapper functions to implement Bayesian analysis in JAGS.
library(modeest) # Provides estimators of the mode of univariate data or univariate distributions.

# option to export the figures
print_fig <- F

# Define subdirectories:
wd_Code <- paste0(getwd(),"/Code")
wd_Data <- paste0(getwd(),"/Data")
wd_Figures <- paste0(getwd(),"/Figures")
wd_Output <- paste0(getwd(),"/Output")

# BSC: this below need to be automatized, which is tricky because our names are 
# placed in the dropbax path that leads to the datasets...
wd_Data_input_root <- "C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS"

# BSC: we probably will need to organize these datasets better
wd_data_regions <- data.frame(
    Central_coast = paste0(wd_Data_input_root,"/1_Active/Central Coast PSE/analysis/central-coast-status/HBM and status"),
    Columbia = paste0(wd_Data_input_root,"/1_Active/Columbia/data & analysis/analysis/columbia-status"), # BSC: ? no HBM and status folder... to check at some point
    Fraser = paste0(wd_Data_input_root,"/Fraser_VIMI/analysis/fraser-status/HBM and status"),
    Haida_Gwaii = paste0(wd_Data_input_root,"/1_Active/Haida Gwaii PSE/Data & Assessments/haida-gwaii-status/HBM and status"),
    Nass = paste0(wd_Data_input_root,"/Nass/assessments/2_population/nass-status/HBM and status"),
    Skeena = paste0(wd_Data_input_root,"/Skeena Updates/skeena-status/HBM and status"),
    Yukon = paste0(wd_Data_input_root,"/1_Active/Yukon/Data & Assessments/yukon-status/HBM-and-status"))

# BSC: figure out what these are exactly
species_acronym <- data.frame(
  Chinook = "CK",
  Chum = "CM",         # to check
  Coho = "CO",         # to check
  Pink = "PK",
  Sockeye = 'SX',
  Steelheqd = "SH",   # to check
  Cutthroat = "CT")   # to check

# To be sure that the name of the region is spelled correctly 
regions <- data.frame(
  Central_coast = 'Central_coast',
  Columbia = "Columbia", 
  Fraser = 'Fraser',
  Haida_Gwaii = 'Haida_Gwaii',
  Nass = 'Nass',
  Skeena = 'Skeena',
  Yukon = 'Yukon')

# Choosing the region
# BSC: This will have to eventually be automatized and eventually allows for 
# multiple regions to be passed on.
region <- regions$Fraser
region <- regions$Yukon

# set the path of the input data sets for that specific region
wd_Data_input <- paste0(wd_data_regions[,region])

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set Species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
Species <- c(species_acronym$Sockeye,    
             species_acronym$Pink,
             species_acronym$Chum)

# If we do not specify the species:
Species <- NULL

# Import the most recent individual fish counts for the region and species selected.
# Select the most recent version of the files.
files_list <- list.files(wd_Data_input)

# In the case we did not specify the Species, find those that have data:
if(is.null(Species)){                               
  files_s <- files_list[grepl(pattern = "_SRdata_",files_list)]
  # get the species present:
  Species <- unique(sub("_SRdata_.*", "", files_s))
}

# Return the corresponding files, selecting the most recent ones eventually:
fndata <- sapply(X = Species, FUN = function(s){
  # s <- Species[3]
  files_s <- files_list[grepl(pattern = paste0(s,"_SRdata_"),files_list)]
  
  # if multiple files, select the one with the most recent date modified:
  if(length(files_s) > 1){
    files_dates <- file.info(paste(wd_Data_input,files_s,sep="/"))$mtime
    files_s <- files_s[files_dates == max(files_dates)]
  }
  # if no file is present --> NA
  if(length(files_s) == 0){
    files_s <- NA
    print(paste0("Species ",s," does not have a file."))
  }
  return(files_s)
})

fndata <- rbind(fndata)           # coerce into an array
fndata <- fndata[!is.na(fndata)]  # remove species with that do not have file.

# Get the full path:
fndata <- paste(wd_Data_input,fndata,sep = "/")

FBYr <- -99   # set first brood year, "-99" for no constraint
MinSRpts <- 3 # set minimum # of SR data points required to be included in the analysis

for(i in 1:length(Species)){
  
  #----------------------------------------------------------------------------#
  # Read in Stock-Recruit Data, run the HBSR model and output parameter estimates ----
  #----------------------------------------------------------------------------#
  
  print(paste0("*** Plot printer for: ",
               colnames(species_acronym[, species_acronym == Species[i],drop=F]),
               " (",Species[i],") ***"))
  
  MaxStocks <- scan(file = fndata[i], nlines = 1, skip = 1)
  
  # First pass through to get list of stocks/CUs and determine how many years of SR 
  # points for each. 
  # Only retain the CUs with at least MinSRpts.
  # Import the fish counts for R and S per year for each CU
  d0 <- read.table(file = fndata[i], header = T, skip = 3 + MaxStocks, 
                   fill = TRUE, stringsAsFactors = FALSE)
  d0$BY <- as.numeric(d0$BY) # brood year
  
  # Import the prSmax and prCV for each CU
  d0_prior <- read.table(file = fndata[i], header = T, skip = 2, nrows = MaxStocks)
  # **SP: These priors differ among stocks. Where do they come from?
  
  d <- subset(d0, is.na(Rec) == F & is.na(Esc) == F & BY >= FBYr & Esc > 0)
  
  StNames <- as.character(unique(d$CU))     # name of CUs; as.character() is used because certain CUs have a number for name
  StNames <- unique(d$CU)
  
  # in case CUID is given, replace it by the corresponding name of the CU(s)
  areThereCUID <- suppressWarnings(!is.na(as.numeric(StNames)))
  if(sum(areThereCUID) > 0){
    CUIDsHere <- as.numeric(StNames[areThereCUID])
    CUIDs <- read.csv(paste0(wd_Data,"/appendix1.csv"),header = T, stringsAsFactors = F)
    CUIDs_cut <- CUIDs[CUIDs$CUID %in% CUIDsHere,]
    CUIDsHere_names <- CUIDs_cut[order(CUIDsHere),]$Conservation.Unit  # to preserve the order because %in% does not 
    StNames[areThereCUID] <- CUIDsHere_names
    
    # update d and 
    for(j in 1:length(CUIDsHere)){
      d$CU[d$CU == CUIDsHere[j]] <- CUIDsHere_names[j]
      d0_prior$CU[d0_prior$CU == CUIDsHere[j]] <- CUIDsHere_names[j]
    }
  }
  
  Nyrs <- tapply(X = d, INDEX = d$CU, FUN = nrow) # nb of year per CU
  
  # ti check the range of years for each CU:
  # tapply(X = d$BY, INDEX = d$CU, FUN = function(x){range(x)})
  
  
  # retain CUs with enough data points
  StNames <- StNames[which(Nyrs >= MinSRpts)]
  
  # update objects
  d <- subset(d, CU %in% StNames)
  Nstocks <- length(StNames)
  Nyrs <- Nyrs[StNames]
  
  # organize the data into a year x CU for R and S: 
  # S <- R <- matrix(nrow = max(Nyrs),ncol = Nstocks)    # BSC: previous code
  S <- R <- matrix(nrow = length(min(d$BY):max(d$BY)),ncol = Nstocks)
  colnames(S) <- colnames(R) <- StNames
  rownames(S) <- rownames(R) <- min(d$BY):max(d$BY)
  for(j in 1:Nstocks){
    d1 <- subset(d,CU == StNames[j])
    S[as.character(d1$BY),j] <- d1$Esc  # BSC: to address SP 's comment below
    R[as.character(d1$BY),j] <- d1$Rec
    
    # S[1:Nyrs[j],j] <- d1$Esc      # BSC: previous code
    # R[1:Nyrs[j],j] <- d1$Rec
  }
  
  #**SP: We have to be a bit careful with this format, because S-R pairs from 
  #* different years are slotted into the same row. For example, if CU#1 has 
  #* data for 1980-2000 and CU #2 has data for 1970-1990 those will both be in 
  #* rows 1-21. There's no problem if we're not accounting for any temporal
  #* covariation among stocks, but this format will have to be reconsidered if
  #* any complexity is added to the model (autocorrelation, temporal covariation)
  
  # Set priors on b:
  # only keep the retained CUs
  d1_prior <- subset(d0_prior,CU %in% StNames)
  prSmax <- d1_prior$prSmax
  prCV <- d1_prior$prCV
  
  prmub <- log(1/prSmax)    # convert mean prior on Smax to log b for winbugs model
  prtaub <- 1/prCV^2				# convert from cv to tau
  
  #### Estimate a and b by linreg and plot
  if(.Platform$OS.type == "windows"){      # BSC: what to do with Linux? : x11()?
    windows()                              # Also do we want it to open in a new window vs in Rstudio?
  }else{
    quartz()
  }

  # LNRS <- log(R/S)   # BSC: now be created inside the function with R and S to limit the number of parameters to pass in
  # inipars <- LinReg(Nyrs,LNRS,S,R,StNames)
  # Display the RS plot and output the estimated parameter values:
  inipars <- LinReg(S,R)
  
  # BSC: this is temporary: see if it is possible to treat eah CU separately in the
  # JAGS chunk in case there are NAs that differ among CU.
  rowToKeep <- apply(X = S, MARGIN = 1, FUN = function(x){sum(is.na(x)) == 0})
  S <-  S[rowToKeep,,drop = F]
  rowToKeep <- apply(X = R, MARGIN = 1, FUN = function(x){sum(is.na(x)) == 0})
  R <-  R[rowToKeep,,drop = F]
  
  LNRS <- log(R/S)
  
  #------------------------------------------------------------------------------#
  #  Bayes model
  #------------------------------------------------------------------------------#
  modelFilename = "Bayes_SR_model.txt"
  cat("
  model{
  
    #Hyper priors
    mu_a ~ dnorm(0.5, 1.0E-6)
    tau_a ~ dgamma(0.5, 0.5)
    sd_a <- pow(tau_a, -0.5)
  
    for(i in 1:Nstocks) {	
  
  	  a[i] ~ dlnorm(mu_a, tau_a) #Hyper distribution on alpha
  
  	  b[i] ~ dlnorm(prmub[i], prtaub[i])I(1.0E-5,)	#prior on stock-independent b
  	  # **SP: Why is b truncated to be greater than 1.0E-5?
  
  	  sd[i] ~ dunif(0.05, 10)
  	  # **SP: Why start at 0.05? Could the sd not be less?
  	
  	  # tau[i] <- pow(sd[i], -0.5)
  	  # **SP: This should be -2 instead of -0.5
      tau[i] <- pow(sd[i], -2)
  	
    }
  
    for(i in 1:Nstocks) {	
    	for(j in 1:Nyrs[i]) {
    		LNRS[j, i] ~ dnorm(Pred[j, i], tau[i])
    		Pred[j, i] <- a[i] - b[i]*S[j, i]
    	}
    }
  }
  ", fill=TRUE, file=modelFilename)
  
  #------------------------------------------------------------------------------#
  #  Jags inputs
  #------------------------------------------------------------------------------#
  # jags.data = list("Nstocks","Nyrs","LNRS","S","prmub","prtaub","cov")
  #**SP: What is cov? Don't see it defined above or used in the model.
  
  # **SP: AFAIK you need a list of the data, not just a list of the data names. I.e.,
  jags.data <- list(
  	Nstocks = Nstocks,
  	Nyrs = Nyrs,
  	LNRS = LNRS,
  	S = S,
  	prmub = prmub,
  	prtaub = prtaub #,
  	# cov = cov
  )
  jags.parms = c("a","b","b2","sd","mu_a","sd_a","mu_b2","sd_b2")
  #** SP: What are b2? mub2? etc.?
  jags.parms = c("a","b","sd","mu_a","sd_a")
  
  #------------------------------------------------------------------------------#
  #   Run Model
  #------------------------------------------------------------------------------#
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
  mypost <- as.matrix(post, chain=F)
  
  #**SP: This is not in fact running in parallel.
  
  ##### INFERENCE #####
  gelman.diag(post, multivariate = F)
  model.probs <- round(cbind(est = colMeans(mypost),
                             sd = apply(mypost,2,sd),
                             ci = t(apply(mypost,2,quantile,c(.025,.975)))),
                       digits = 8)
  model.probs
  
  # BSC: it is now exported in /Output
  # BSC: Where these should be outputed? They are probably too big for github
  write.table(mypost,file = paste0("Output/",region,".",Species[i],".post.out"),
              col.names = T,row.names = F)
  
# }

  #------------------------------------------------------------------------------#
  # Plot SR relationship ----
  #------------------------------------------------------------------------------#
  
  # BSC: it is possible to group this above code with the one below in the same loop.
  # Now it is grouped.
  # Note that keeping everything in one loop is simpler because we filter the 
  # _SPregion <- regions$Fraser...csv datasets and eventually updated the names 
  # in CUs. How come this is not done again here by the way?

# for(i in 1:length(Species)){
  
  # i <- 1
  # Import the ...
  # could also simply use mypost
  post <- read.table(file = paste0(wd_Output,"/",region,".",Species[i],".post.out"),
                     header = T)
  
  # maximum nb of CUs
  # MaxStocks <- scan(file = fndata[i],nlines = 1,skip = 1)
  
  # for each CU..
  CUs <- unique(d$CU)
  for(i_cu in 1:length(CUs)){
    
    # i_cu <- 1
    CU <-CUs[i_cu]
    d_sub <- subset(x = d, subset = CU == CU)
    
    # alpha <- sx.post[,16]; beta <- sx.post[,65] # ?! what are those big numbers in there?
    alpha <- post[,i_cu]
    beta <- post[,i_cu + length(unique(d$CU))]
    
    # 
    spw <- seq(0,max(d_sub$Esc, na.rm = T),100)
    
    # 
    recruits <- array(NA, dim = c(1,length(spw), 10000))
    
    # 
    for(j in 1:10000){
      iter <- sample(length(alpha),1)
      recruits[,,j] <- spw * exp(alpha[iter] - beta[iter] * spw)
    }
    
    # 
    rec.m <- apply(recruits,c(1,2),median)/1000
    rec.u <- apply(recruits,c(1,2),quantile,probs = 0.975)/1000
    rec.l <- apply(recruits,c(1,2),quantile,probs = 0.025)/1000
    
    if(print_fig){
      # jpeg("LongSR.jpg", width=8, height=4,units="in",res=200)
      # BSC: not sure if we want to export the figures in the github repo.
      jpeg(paste0(wd_Figures,"/",region,"_",Species[i],"_",unique(d$CU)[i_cu],".jpg"), 
           width = 8, height = 4,units = "in",res = 200)
    }
    
    par(mfrow = c(1,1),mar = c(1.5,2.5,1,1.5),oma = c(2,2,2,1), new = F)
    plot(x = d_sub$Esc / 1000,y = d_sub$Rec/1000,
         bty='l', xlab="", ylab="", xaxt="n", yaxt="n", col = "white")
    axis(1)
    axis(2,las = 2)
    # mtext("Sockeye - Long",3,cex=0.85,line=1)
    species_full <- colnames(species_acronym)[which(species_acronym == Species[i])]
    mtext(paste0(species_full," - ",CU),3,cex=0.85,line=1)
    #
    polygon(x = c(spw/1000,rev(spw/1000)),y = c(rec.u,rev(rec.l)),
            col = grey(0.9),border = NA)
    # 
    points(x = d_sub$Esc / 1000,y = d_sub$Rec/1000)
    #
    lines(x = spw/1000, y = rec.m, lwd = 2)
    
    # adjustcolor(grey(0.9), alpha.f = 0.9)
    abline(v =41.4,col="red",lwd=2)
    abline(v =29.8,col="red",lty=2)
    abline(v =76.7,col="red",lty=2)
    
    abline(v=82.8,col="dark green",lwd=2)
    abline(v=59.7,col="dark green",lty=2)
    abline(v=153.4,col="dark green",lty=2)
    mtext("Spawners (000s)",1,outer=T,line=1)
    mtext("Recruits (000s)",2,outer=T,line=1)
    
    if(print_fig){
      dev.off()
    }
    
  }
  
  
  # obtain the CUs names:
  unique(sp.rs$CU)
  
  
  # sockeye
  #long
  Long.cu <- subset(sx.rs, CU=="Long")
  
  #owikeno
  Owikeno.cu <- subset(sx.rs, CU=="Owikeno")
  
  #backland
  Backland.cu <- subset(sx.rs, CU=="Backland")
  
  #backland
  Canoona.cu
  
  
}






