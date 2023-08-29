
# Function which computes linear regression estimates of SR parameters: a, b, sigma
# spawners # matrix of spawners with columns = CUs and rows = years
# recruits # matrix of recruits with columns = CUs and rows = years
# CUnames # List of CU names corresponding to columns
estSR <- function(spawners, recruits, CUnames){
  
  nCU <- dim(spawners)[2]
  
  logRS <- log(recruits/spawners)
  
  a <- rep(NA, nCU) # Vector to store productivity parameter
  b <- rep(NA, nCU)	# Vector to store density-dependence parameter
  sig <- rep(NA, nCU) # Vector to store estimates of sigma
  
  # ngrows=4;ngcol=trunc(Nstocks/ngrows)+1
  
  for (i in 1:nCU) { # For each CU
    
    # Fit simple linearized Ricker model
    reg <- lm(logRS[ ,i] ~ spawners[ ,i])
    
    # Extract fitted parameters
    a[i] <- reg$coefficients[1]
    b[i] <- abs(reg$coefficients[2])
    
    sig[i] <- summary(reg)$sigma
  }
  
  return(list(a = a, b = b, sig = sig))
}

# Function that returns the name of the regions. This is to ensure that no spelling
# mistakes are make.
regions_fun <- function(){
  
  regions <- data.frame(
    Central_coast = 'Central_coast',
    Columbia = "Columbia", 
    Fraser = 'Fraser',
    Haida_Gwaii = 'Haida_Gwaii',
    Nass = 'Nass',
    Skeena = 'Skeena',
    Yukon = 'Yukon')
  
  return(regions)
}

# Function that returns a data frame of the fish species names (as column names)
# and corresponding acronym.
# BSC: figure out what these are exactly --> to double check
species_acronym_fun <- function(){
  
  # species_acronym <- data.frame(
  #   name = c("Chinook","Chum","Coho","Pink","Sockeye","Steelhead","Cutthroat"),
  #   acronym = c("CK","CM","CO","PK","SX","SH","CT"))
  
  species_acronym <- data.frame(
    Chinook = "CK",
    Chum = "CM",         # to check
    Coho = "CO",         # to check
    Pink = "PK",
    Sockeye = 'SX',
    Steelheqd = "SH",   # to check
    Cutthroat = "CT")   # to check
  
  return(species_acronym)
}

# Function that returns a list of two data frame from the _SRdata.txt for a given 
# species in a given region. The path of the 
# path_file <- fndata[i]  # the path of the _SRdata.text file
# MinSRpts: the minimum number of year (= data points) required 
# wd_Data: the biological-status/Data folder 
SRdata_fun <- function(path_file, wd_Data, MinSRpts = 3){
  
  # BSC: I replaced "stock" with "CU" in the code?
  CUs_nb <- scan(file = path_file, nlines = 1, skip = 1)
  
  # Import the prSmax and prCV for each CU
  d_prior <- read.table(file = path_file, header = T, skip = 2, nrows = CUs_nb)
  # **SP: These priors differ among stocks. Where do they come from?
  
  # Import the fish counts for R and S per year for each CU
  d0 <- read.table(file = path_file, header = T, skip = 3 + CUs_nb, 
                   fill = TRUE, stringsAsFactors = FALSE)
  d0$BY <- as.numeric(d0$BY) # brood year
  
  # Only retain rows with values
  d <- subset(x = d0, subset =  !is.na(Rec) & !is.na(Esc) & BY >= FBYr & Esc > 0) # BSC: why  Esc > 0  and not >= 0?
  
  # get the names of the CUs
  StNames <- as.character(unique(d$CU)) # name of CUs; as.character() is used because certain CUs have a number for name
  StNames <- unique(d$CU)
  
  # in case the CUID is given, replace it by the corresponding name of the CU(s)
  areThereCUID <- suppressWarnings(!is.na(as.numeric(StNames)))
  if(sum(areThereCUID) > 0){
    CUIDsHere <- as.numeric(StNames[areThereCUID])
    # Import Appendix 1: table from the tech report:
    CUIDs <- read.csv(paste0(wd_Data,"/appendix1.csv"),header = T, 
                      stringsAsFactors = F)
    # filter/subset
    CUIDs_cut <- CUIDs[CUIDs$CUID %in% CUIDsHere,]
    
    # preserve the order because %in% does not
    CUIDsHere_names <- CUIDs_cut[order(CUIDsHere),]$Conservation.Unit   
    StNames[areThereCUID] <- CUIDsHere_names
    
    # update d and d0_prior
    for(j in 1:length(CUIDsHere)){
      d$CU[d$CU == CUIDsHere[j]] <- CUIDsHere_names[j]
      d_prior$CU[d_prior$CU == CUIDsHere[j]] <- CUIDsHere_names[j]
    }
  }
  
  # nb of year per CU
  Nyrs <- tapply(X = d, INDEX = d$CU, FUN = nrow) 
  
  # to check the range of years for each CU:
  # tapply(X = d$BY, INDEX = d$CU, FUN = function(x){range(x)})
  
  # retain CUs with enough data points (i.e., Nyrs >= MinSRpts)
  StNames <- StNames[which(Nyrs >= MinSRpts)]
  
  # update objects
  d <- subset(d, CU %in% StNames)
  d_prior <- subset(d_prior, CU %in% StNames)
  
  # 
  output <- list(d,d_prior)
  names(output) <- c("counts","priors")
  
  return(output)
}

# Function that returns the input _SRdata.txt file names from there region-specific 
# repository, which is defined by wd.
# wd <- wd_Data_input
SRdata_path_Species_fun <- function(wd, Species = NULL){
  
  # Import the most recent individual fish counts for the region and species selected.
  # Note that the region path should be contained in wd already.
  files_list <- list.files(wd)
  
  # In the case we did not specify the Species, find those that have data:
  if(is.null(Species)){                           
    files_s <- files_list[grepl(pattern = "_SRdata",files_list)]
    # get the species present:
    Species <- unique(sub("_SRdata.*", "", files_s))
  }
  
  # Return the corresponding files, selecting the most recent ones eventually:
  SRdata <- sapply(X = Species, FUN = function(s){
    # s <- Species[3]
    files_s <- files_list[grepl(pattern = paste0(s,"_SRdata"),files_list)]
    
    # if multiple files, select the one with the most recent date modified:
    if(length(files_s) > 1){
      files_dates <- file.info(paste(wd,files_s,sep="/"))$mtime
      files_s <- files_s[files_dates == max(files_dates)]
    }
    # if no file is present --> NA
    if(length(files_s) == 0){
      files_s <- NA
      print(paste0("Species ",s," does not have a file."))
    }
    return(files_s)
  })
  
  SRdata <- rbind(SRdata)           # coerce into an array
  SRdata <- SRdata[!is.na(SRdata)]  # remove species with that do not have file.
  
  # Get the full path:
  SRdata <- paste(wd_Data_input,SRdata,sep = "/")
  
  output <- list(Species,SRdata)
  names(output) <- c("Species","SRdata")
    
  return(output)
}

# Function that returns a data frame with the path leading to the repository where 
# input data (i.e., run reconstructions) is located for each region.
# wd_Data_input_root is the root directory common to all the regions.
# BSC: we probably will need to organize these datasets better
wd_data_regions_fun <- function(wd_root = ""){
  
  wd_data_regions <- data.frame(
    Central_coast = paste0(wd_root,"/1_Active/Central Coast PSE/analysis/central-coast-status/HBM and status"),
    Columbia = paste0(wd_root,"/1_Active/Columbia/data & analysis/analysis/columbia-status"), # BSC: ? no HBM and status folder... to check at some point
    Fraser = paste0(wd_root,"/Fraser_VIMI/analysis/fraser-status/HBM and status"),
    Haida_Gwaii = paste0(wd_root,"/1_Active/Haida Gwaii PSE/Data & Assessments/haida-gwaii-status/HBM and status"),
    Nass = paste0(wd_root,"/Nass/assessments/2_population/nass-status/HBM and status"),
    Skeena = paste0(wd_root,"/Skeena Updates/skeena-status/HBM and status"),
    Yukon = paste0(wd_root,"/1_Active/Yukon/Data & Assessments/yukon-status/HBM-and-status"))
  
  return(wd_data_regions)
}



