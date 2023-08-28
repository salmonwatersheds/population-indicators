
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

#---------------------------------------------------------------#
# 1. Read in Stock-Recruit Data 
#---------------------------------------------------------------#

for(i in 1:length(fndata)){
  
  print(paste0("Plot printer for: ",
               colnames(species_acronym[, species_acronym == Species[i],drop=F]),
               " (",Species[i],")"))
  
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
  
  Nstocks <- length(StNames)  # nb of CUs
  Nyrs <- tapply(X = d, INDEX = d$CU, FUN = nrow) # nb of year per CU
  
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
  inipars <- LinReg(S,R)
}





