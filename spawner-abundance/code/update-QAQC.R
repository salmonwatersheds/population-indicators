###############################################################################
# Code that reviews new outputs of spawner-abundance and compared to what is
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

zz <- z[grep(pattern = "dataset_1part1", z)]

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
# Check that number of CUs is the same
#------------------------------------------------------------------------------

if(length(unique(newData$CUID)) != length(unique(oldData$CUID))){
  # If there are more CUs in the newData
  if(length(unique(newData$streamname)) > length(unique(oldData$streamname))){
    warning(paste0("New data has more CUs (", length(unique(newData$CUID)), ") than old data (", length(unique(oldData$CUID)),"). See addedCUs.\n"))
    warnings <- c(warnings, paste0("New data has more CUs (", length(unique(newData$CUID)), ") than old data (", length(unique(oldData$CUID)),"). See addedCUs."))
    
    # Extract added stream names
    addedCUs <- sort(unique(newData$CUID)[which(unique(newData$CUID) %in% oldData$CUID == FALSE)])
    
  } else {
    
    warnings <- c(warnings, paste0("New data has fewer CUs (", length(unique(newData$CUID)), ") than old data (", length(unique(oldData$CUID)),"). See missingCUs."))
    warning(paste0("New data has fewer CUs (", length(unique(newData$CUID)), ") than old data (", length(unique(oldData$CUID)),"). See missingCUs.\n"))
    
    # Extract missing stream names
    missingCUs <- sort(unique(oldData$CUID)[which(unique(oldData$CUID) %in% newData$CUID == FALSE)])
  }
  
}


#------------------------------------------------------------------------------
# Check that LGL.counts match
#------------------------------------------------------------------------------
allLGL.counts <- full_join(newData[!is.na(newData$LGL.counts), which(names(newData) %in% c("CUID", "Year", "LGL.counts"))], oldData[!is.na(oldData$LGL.counts), which(names(oldData) %in% c("CUID", "Year", "LGL.counts"))])

# Create function that checks that there are no duplicate years for a given streamid
# if there are duplicate years, function will return FALSE (i.e., not unique)
checkUnique <- function(x){
  length(unique(x)) == length(x)
}

# Apply function by streamid
dum <- tapply(allLGL.counts$Year, allLGL.counts$CUID, checkUnique)

if(sum(dum == FALSE) > 0){ # If there are duplicates...
  duplicateLGL_CUID <- as.numeric(names(dum)[which(dum == FALSE)])

  warning("Duplicate years of LGL.counts for single CU. See duplicateLGL_CUID\n")
  warnings <- c(warnings, "Duplicate years for single CU. See duplicateLGL_CUID")
  
  # sort(allData$year[allData$streamid == duplicateYear_streamid[1]])
  # x <- allData[allData$streamid == duplicateYear_streamid[1], ]
  # x[order(x$year),]
} 

#------------------------------------------------------------------------------
# Check that NuSEDS.counts.by.CU match
#------------------------------------------------------------------------------
allNuSEDS.counts.by.CU <- full_join(newData[!is.na(newData$LGL.counts), which(names(newData) %in% c("CUID", "Year", "NuSEDS.counts.by.CU"))], oldData[!is.na(oldData$LGL.counts), which(names(oldData) %in% c("CUID", "Year", "NuSEDS.counts.by.CU"))])


# Apply function by CUID
dum <- tapply(allNuSEDS.counts.by.CU$Year, allNuSEDS.counts.by.CU$CUID, checkUnique)

if(sum(dum == FALSE) > 0){ # If there are duplicates...
  duplicateNuSEDS_CUID <- as.numeric(names(dum)[which(dum == FALSE)])
  
  warning("Duplicate years of NuSEDS.counts.by.CU for single CU. See duplicateNuSEDS_CUID\n")
  warnings <- c(warnings, "Duplicate years of NuSEDS.counts.by.CU for single CU. See duplicateNuSEDS_CUID")
  
} 

#------------------------------------------------------------------------------
# Check that Total.run match
#------------------------------------------------------------------------------
allTotal.run <- full_join(newData[!is.na(newData$LGL.counts), which(names(newData) %in% c("CUID", "Year", "Total.run"))], oldData[!is.na(oldData$LGL.counts), which(names(oldData) %in% c("CUID", "Year", "Total.run"))])

# Apply function by CUID
dum <- tapply(allTotal.run$Year, allTotal.run$CUID, checkUnique)

if(sum(dum == FALSE) > 0){ # If there are duplicates...
  duplicateTotal.run_CUID <- as.numeric(names(dum)[which(dum == FALSE)])
  
  warning("Duplicate years of Total.run for single CU. See duplicateTotal.run_CUID\n")
  warnings <- c(warnings, "Duplicate years of Total.run for single CU. See duplicateTotal.run_CUID")
  
} 

} # End if there is old data
