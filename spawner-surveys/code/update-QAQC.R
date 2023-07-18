###############################################################################
# Code that reviews new outputs of spawner-surveys and compared to what is
# currently in the database and flags any discrepancies
###############################################################################

library(dplyr)
# library(RPostgreSQL)

###############################################################################
# Specify region
###############################################################################
# To speed up the code, you can specify what region you want to QA/QC

regions <- c("Skeena", "Nass", "Central Coast", "Fraser","Vancouver Island & Mainland Inlets", "Haida Gwaii", "Columbia", "Yukon", "Transboundary")

region_num <- 
  as.numeric(readline("Select a region to QA/QC, or press return to check all regions.\nAvailable regions are:\n 1 = Skeena\n2 = Nass\n3 = Central Coast\n4 = Fraser\n5 = VIMI\n6 = Haida Gwaii\n7 = Columbia\n8 = Yukon\n9 = Transboundary\n"))

if(is.na(region_num)){
  region_num <- c(1:9)
}

region <- regions[region_num]     
# ###############################################################################
# # Pull current spawner survey data from the database
# ###############################################################################
# 
# #------------------------------------------------------------------------------
# # Create connection to postreSQL salmonwatersheds database
# #------------------------------------------------------------------------------
# 
# # Note: connecting to the database requires access to be granted by Katy and
# # your IP to be registered. access may be denied if your IP address has changed
# # (common with dynamic IPs, e.g., whenever router is restarted).
# 
# # Enter credentials
# dsn_database <- "salmondb_prod"   # Specify the name of your Database
# dsn_hostname <- "data.salmonwatersheds.ca"  # Specify host name e.g.:"aws-us-east-1-portal.4.dblayer.com"
# dsn_port <- "5432"                # Specify your port number. e.g. 98939
# dsn_uid <- "salmonwatersheds"         # Specify your username. e.g. "admin"
# 
# # STOP: need to enter password before proceeding to connect
# dsn_pwd <- readline(prompt="Enter database password: " )    # Specify your password. e.g. "xxx"
# 
# # Establish connection to database
# tryCatch({
#   drv <- dbDriver("PostgreSQL")
#   print("Connecting to Database…")
#   connec <- dbConnect(drv,
#                       dbname = dsn_database,
#                       host = dsn_hostname,
#                       port = dsn_port,
#                       user = dsn_uid,
#                       password = dsn_pwd)
#   print("Database Connected!")
# },
# error=function(cond) {
#   print("Unable to connect to Database.")
# })
# 
# #------------------------------------------------------------------------------
# # Pull spawner survey table (this takes a minute - lots of data!)
# #------------------------------------------------------------------------------
# 
# # Import
# spawner_surveys <- dbGetQuery(
#   conn = connec,
#   statement = "SELECT * FROM appdata.vwdl_streamspawnersurveys_output
# 	WHERE "
# )

###############################################################################
# Find spawner survey datasets for region and import
###############################################################################

# Extract root for specific user
root <- paste(strsplit(getwd(), "/")[[1]][1:7], collapse = "/")

# Set path to output files for the selected region
if(region == "Skeena"){
  path <- "Skeena Updates/skeena-status/Output"
} else if(region == "Nass"){
  path <- "Nass/assessments/2_population/nass-status/Output"
  # Note: previous assessments are in Nass/assessments/2_population/results/final analysis from BC/Output
} else if(region == "Central Coast"){
  path <- "1_Active/Central Coast PSE/analysis/central-coast-status/Output"
} else if(region == "Haida Gwaii"){
  path <- "1_Active/Haida Gwaii PSE/Data & Assessments/haida-gwaii-status/Output"
} else if(region == "Fraser"){
  path <- "Fraser_VIMI/analysis/fraser-status/Output"
} else if(region == "Vancouver Island & Mainland Inlets"){
  path <- "Fraser_VIMI/analysis/vimi-status/Output"
  # Note: Previous assessments are in Fraser_VIMI/analysis/SCVI status assessment/Output
} else if(region == "Columbia"){
  path <- "1_Active/Columbia/data & analysis/analysis/columbia-status/Output"
} else if(region == "Transboundary"){
  path <- "1_Active/Transboundary/Data & Assessments/transboundary-status/Output"
} else if(region == "Yukon"){
  path <- "1_Active/Yukon/Data & Assessments/yukon-status/Output"
  # Note these files are named differently, with "Yukon" on the end; need to change
}

# Get file names for output datasets and identify newest and previous files
z <- list.files(path = paste0(root, "/", path))

zz <- z[grep(pattern = "dataset_1part2", z)]

z.date <- unlist(lapply(strsplit(zz, split = ".csv"), strsplit, split = "dataset_1part2."))
z.date <- as.Date(z.date[which(z.date != "")], format = "%b%d%Y")

newFile <- zz[which(order(z.date, decreasing = TRUE) == 1)]
if(length(z.date) == 1){
  stop("No old data to compare to.")
} else {
  
oldFile <- zz[which(order(z.date, decreasing = TRUE) == 2)]

newData <- read.csv(paste0(root, "/", path, "/", newFile))
oldData <- read.csv(paste0(root, "/", path, "/", oldFile))


###############################################################################
# Run checks and store warnings
###############################################################################

warnings <- c(NA)

#------------------------------------------------------------------------------
# Check that headers are the same
#------------------------------------------------------------------------------

if(sum(c(
  names(newData) %in% names(oldData) == FALSE, 
  names(oldData) %in% names(newData) == FALSE)) > 0){
     warnings <- c(warnings, "Headers don't match.")
     warning("Headers don't match.\n")
   } 

#------------------------------------------------------------------------------
# Check that number of streams is the same
#------------------------------------------------------------------------------

if(length(unique(newData$streamname)) != length(unique(oldData$streamname))){
  # If there are more streams in the newData
  if(length(unique(newData$streamname)) > length(unique(oldData$streamname))){
    warning(paste0("New data has more stream names (", length(unique(newData$streamname)), ") than old data (", length(unique(oldData$streamname)),"). See addedStreams.\n"))
    warnings <- c(warnings, paste0("New data has more stream names (", length(unique(newData$streamname)), ") than old data (", length(unique(oldData$streamname)),"). See addedStreams."))
    
    # Extract added stream names
    addedStreams <- sort(unique(newData$streamname)[which(unique(newData$streamname) %in% oldData$streamname == FALSE)])
    
  } else {
    
    warnings <- c(warnings, paste0("New data has fewer stream names (", length(unique(newData$streamname)), ") than old data (", length(unique(oldData$streamname)),"). See missingStreams."))
    warning(paste0("New data has fewer stream names (", length(unique(newData$streamname)), ") than old data (", length(unique(oldData$streamname)),"). See missingStreams.\n"))
    
    # Extract missing stream names
    missingStreams <- sort(unique(oldData$streamname)[which(unique(oldData$streamname) %in% newData$streamname == FALSE)])
  }
  
}

#------------------------------------------------------------------------------
# Check that number of streamid is the same
#------------------------------------------------------------------------------
if(length(unique(newData$streamid)) != length(unique(oldData$streamid))){
  # If there are more streams in the newData
  if(length(unique(newData$streamid)) > length(unique(oldData$streamid))){
    warning(paste0("New data has more streamid (", length(unique(newData$streamid)), ") than old data (", length(unique(oldData$streamid)),"). See addedStreamid.\n"))
    warnings <- c(warnings, paste0("New data has more streamid (", length(unique(newData$streamid)), ") than old data (", length(unique(oldData$streamid)),"). See addedStreamid."))
    
    # Extract added stream names
    addedStreamid <- sort(unique(newData$streamid)[which(unique(newData$streamid) %in% oldData$streamid == FALSE)])
    
  } else {
    
    warnings <- c(warnings, paste0("New data has fewer streamid (", length(unique(newData$streamid)), ") than old data (", length(unique(oldData$streamid)),"). See missingStreamid."))
    warning(paste0("New data has fewer streamid (", length(unique(newData$streamid)), ") than old data (", length(unique(oldData$streamid)),"). See missingStreamid.\n"))
    
    # Extract missing stream names
    missingStreamid <- sort(unique(oldData$streamid)[which(unique(oldData$streamid) %in% newData$streamid == FALSE)])
  }
  
}


#------------------------------------------------------------------------------
# Check for duplicate years
#------------------------------------------------------------------------------
allData <- full_join(newData[, which(names(newData) != "X")], oldData[, which(names(oldData) != "X")])

# Create function that checks that there are no duplicate years for a given streamid
# if there are duplicate years, function will return FALSE (i.e., not unique)
checkUnique <- function(x){
  length(unique(x)) == length(x)
}

# Apply function by streamid
dum <- tapply(allData$year, allData$streamid, checkUnique)

if(sum(dum == FALSE) > 0){ # If there are duplicates...
  duplicateYear_streamid <- as.numeric(names(dum)[which(dum == FALSE)])

  warning("Duplicate years for single streamid. See duplicateYear_streamid.\n")
  warnings <- c(warnings, "Duplicate years for single streamid. See duplicateYear_streamid.")
  
  # sort(allData$year[allData$streamid == duplicateYear_streamid[1]])
  # x <- allData[allData$streamid == duplicateYear_streamid[1], ]
  # x[order(x$year),]
} else {
  cat("\nAll common years match, no duplicate data.\n\n")
}
}
