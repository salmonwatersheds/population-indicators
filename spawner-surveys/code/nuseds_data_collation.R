

#'******************************************************************************
#' The goal of the script is to import, clean and format NuSEDS data.
#' Script based on Emma Atkinson's previous version (26-Mar-2019).
#' 
#' Files imported (from dropbox):
#' - 
#' 
#' Files produced: 
#' - 
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
source("functions_set_wd.R")

# return the name of the directories for the different projects:
subDir_projects <- subDir_projects_fun()

wds_l <- set_working_directories_fun(subDir = subDir_projects$spawner_surveys,
                                     Export_locally = F)
wd_head <- wds_l$wd_head
wd_project <- wds_l$wd_project
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS

wd_references_dropbox <- paste(wd_X_Drive1_PROJECTS,
                               wds_l$wd_project_dropbox,
                               "references",sep="/")

wd_data_dropbox <- paste(wd_X_Drive1_PROJECTS,
                         wds_l$wd_project_dropbox,
                         "data",sep="/")

wd_documents <- paste(wd_project,"documents",sep="/")

# Loading packages 
library(plyr)
library(dplyr)
library(tibble)
library(scales)
library(ggplot2)
library(readxl)
library(reshape2)
library(stringr)


# Functions ------
# TODO: place in a functions.R script

#' Compares the column names of two dataframes and print the differences if any.
compareColNamesDF_fun <- function(DF1,DF2){
  colnames1 <- colnames(DF1)
  colnames2 <- colnames(DF2)
  if(!identical(colnames1, colnames2)){
    print("Column names only in first dataframe:")
    toPrint <- colnames1[!colnames1 %in% colnames2]
    print(toPrint)
    cat("\n")
    print("Column names only in second dataframe:")
    toPrint <- colnames2[!colnames2 %in% colnames1]
    print(toPrint)
    cat("\n")
    print("Column names in both dataframes:")
    toPrint <- colnames2[colnames2 %in% colnames1]
    print(toPrint)
    cat("\n")
  }else{
    print("Column names are identical.")
  }
}

# Import datasets -----

# Create a dataframe to keep track of the different datasets and objects
datasets_df <- data.frame(object = rep(NA,10),
                          dataset_original = rep(NA,10), 
                          dataset_new = rep(NA,10),
                          comments = rep(NA,10))
nrow <- 1

#' ** Import ??? **
# ??? what is this file?
# it is in: Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/Fraser_VIMI/analysis/Compilation/Reference
refdat <- read.delim(paste(wd_references_dropbox,"NCC_Streams_13March2016_KKE.txt",sep="/"),
                     header=TRUE, na.string="")

# Set up column names for empty destination dataframe
names <- c(names(refdat)[1:45], c(1926:2021))
# View(refdat)

datasets_df$object[nrow] <- "refdat"
datasets_df$dataset_original[nrow] <- "NCC_Streams_13March2016_KKE.txt"
datasets_df$dataset_new[nrow] <- NA
datasets_df$comments[nrow] <- "?"
nrow <- nrow + 1


#' ** Import the NuSEDS data **
#' either from the API if the data has been updated since
#' the last download, or from wd_data 
#' TODO: find a way to check the date of last modification from API and compare it
#' to the local version of the file, then decide what to do. Packages and functions
#' to use:
#' - file.info()$mtime : file (last) modification date
#' - httr::HEAD(API_NuSEDS)$date: that does not work with the API_NuSEDS...

updateNuSEDSFile <- F

if(updateNuSEDSFile){
  
  options(timeout = 190)
  API_NuSEDS <- "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/c48669a3-045b-400d-b730-48aafe8c5ee6/attachments/All%20Areas%20NuSEDS.csv"
  nusedsFileName <- paste0(wd_data_dropbox,"/all_areas_nuseds_", strftime(Sys.Date(), format = "%Y%m%d"), ".csv")
  download.file(url = API_NuSEDS, destfile = nusedsFileName)
  
}else{
  
  pattern <- "all_areas_nuseds_"
  z <- list.files(path = wd_data_dropbox)
  zz <- z[grep(pattern = pattern, z)]
  z.date <- unlist(lapply(strsplit(zz, split = ".csv"), strsplit, split = pattern))
  z.date <- as.Date(z.date[which(z.date != "")], format = "%Y%m%d")
  
  nusedsFileName <- zz[which(order(z.date, decreasing = TRUE) == 1)]
  nusedsFileName <- paste(wd_data_dropbox,nusedsFileName,sep="/")
  
}

nuseds <- read.csv(nusedsFileName, header = T)

# TODO: implement comparison with previous version. Below is a provisory solution.
nuseds_old <- read.csv(paste(wd_data_dropbox,"All Areas NuSEDS.csv",sep="/"),
                       header=TRUE, stringsAsFactors=FALSE)

compareColNamesDF_fun(nuseds,nuseds_old)

datasets_df$object[nrow] <- "nuseds"
datasets_df$dataset_original[nrow] <- "All Areas NuSEDS.csv"
datasets_df$dataset_new[nrow] <- "all_areas_nuseds_20231017.csv"
datasets_df$comments[nrow] <- "NuSEDS data, column names are identical"
nrow <- nrow + 1


#' ** Import the NuSEDS list of CUs **
# ???: where is it coming from?
# ???: what's the goal?
# DFO provided files matching streams and Nuseds to full CU index --> API!

updateNuSEDSFile <- F

if(updateNuSEDSFile){
  
  url_CU_sites <-"https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/c48669a3-045b-400d-b730-48aafe8c5ee6/attachments/conservation_unit_system_sites.csv"
  CU_SiteFileName <- paste0(wd_data_dropbox,"/conservation_unit_system_sites_", strftime(Sys.Date(), format = "%Y%m%d"), ".csv")
  download.file(url = url_CU_sites, destfile = CU_SiteFileName)
  
}else{
  
  pattern <- "conservation_unit_system_sites_"
  z <- list.files(path = wd_data_dropbox)
  zz <- z[grep(pattern = pattern, z)]
  z.date <- unlist(lapply(strsplit(zz, split = ".csv"), strsplit, split = pattern))
  z.date <- as.Date(z.date[which(z.date != "")], format = "%Y%m%d")
  
  nusedsFileName <- zz[which(order(z.date, decreasing = TRUE) == 1)]
  nusedsFileName <- paste(wd_data_dropbox,nusedsFileName,sep="/")
  
}

cu.sites <- read.csv(nusedsFileName, header=TRUE, stringsAsFactors=FALSE)

# TODO: implement comparison with previous version. Below is a provisory solution.
cu.sites_old <- read.csv(paste(wd_data_dropbox,"conservation_unit_system_sitesJul2023.csv",sep="/"),
                         header=TRUE, stringsAsFactors=FALSE)

compareColNamesDF_fun(cu.sites,cu.sites_old)

datasets_df$object[nrow] <- "cu.sites"
datasets_df$dataset_original[nrow] <- "conservation_unit_system_sitesJul2023.csv"
datasets_df$dataset_new[nrow] <- "conservation_unit_system_sites_20231017.csv"
datasets_df$comments[nrow] <- "There are 10 more columns in the original dataset"
nrow <- nrow + 1

# If we decide to keep the older version
# cu.sites <- cu.sites_old
# names(cu.sites)[1] <- "ID"


#' ** Import PSF list of CUs **
# ???: where is it coming from?
# ???: what's the goal?
# CU list provided by Katy at some point, other version of data/conservation-units.csv, 
# BUT check the different, contain generation length?
psf.cu <- read.csv(paste(wd_data_dropbox,"PSF_master_CU_list.csv",sep="/"),
                   header=TRUE, stringsAsFactors = FALSE)

psf.cu2 <- read.csv(paste(wd_data_dropbox,"conservation-units.csv",sep="/"),
                    header=TRUE, stringsAsFactors = FALSE)

compareColNamesDF_fun(psf.cu,psf.cu2)
# --> many colnames are in capital letters in psf.cu and not in psf.cu2
psf.cu$CUID
psf.cu2$cuid[! psf.cu2$cuid %in% psf.cu$CUID]  # conservation-units.csv has way more CUs BUT
psf.cu$CUID[! psf.cu$CUID %in% psf.cu2$cuid]   # 936 is missing in conservation-units.csv

datasets_df$object[nrow] <- "psf.cu"
datasets_df$dataset_original[nrow] <- "PSF_master_CU_list.csv"
datasets_df$dataset_new[nrow] <- "conservation-units.csv"
datasets_df$comments[nrow] <- "There are much more CUs in the new df but CU 936 is missing. Colnames are capitalized in older file"
nrow <- nrow + 1


#' ** Import WHAT IS IT ??? **
# ???: where is it coming from?
# ???: what's the goal?
# ??? that is specific to VIMI Fraser right?
vimi.sites <- read.delim(paste(wd_data_dropbox,"ssp.streams_20230719.txt",sep = "/"),
                         header=TRUE, stringsAsFactors=FALSE)
colnames(vimi.sites)[colnames(vimi.sites) == "streamname"] <- "SYSTEM_SITE"

datasets_df$object[nrow] <- "vimi.sites"
datasets_df$dataset_original[nrow] <- "ssp.streams_20230719.txt"
datasets_df$dataset_new[nrow] <- NA
datasets_df$comments[nrow] <- "?"
nrow <- nrow + 1


# BSC: return all the rows in vimi.sites without a match in cu.sites
# ??? what is that for? Just a check up?
da <- anti_join(vimi.sites, cu.sites, by = "SYSTEM_SITE")

# Compare NuSEDS CUs with PSF list of CUs - make sure that the CUs we are keeping
# are the right ones. There are some discrepancies where CUs listed as "current"
# in PSF list have been binned in NuSEDS list.
unique(cu.sites$AREA) # ???
unique(cu.sites_old$AREA)

# Filter site info for CUs in south coast regions.
# nuseds.cus <- cu.sites # no need to create nuseds.cus so I replaced it by cu.sites in the code below

# colToKeep <- colnames(cu.sites)[c(13,18,20)] # bod practice
colToKeep <- c("CU_NAME","CU_TYPE","FULL_CU_IN")

# Which CUs in the NuSEDS "CU_Sites" file are labeled "Bin"?
# what is "Bin"?
# unique(cu.sites[cu.sites$CU_NAME %in% c(str_subset(unique(cu.sites$CU_NAME), "Bin")),c(13,18,20)])
# unique(cu.sites[cu.sites$CU_NAME %in% c(str_subset(unique(cu.sites$CU_NAME), "BIN")),c(13,18,20)]) # BSC: !!!
unique(cu.sites[grepl("BIN",cu.sites$CU_NAME),colToKeep])

# Which are labeled "Extirpated"? #
# unique(cu.sites[cu.sites$CU_NAME %in% c(str_subset(unique(cu.sites$CU_NAME), "Extirpated")),c(13,18,20)])
# unique(cu.sites[cu.sites$CU_NAME %in% c(str_subset(unique(cu.sites$CU_NAME), "EXTIRPATED")),c(13,18,20)])  # BSC: !!!
unique(cu.sites[grepl("EXTIRPATED",cu.sites$CU_NAME),colToKeep])

# For the populations which are Binned in NuSEDs, we have DIFFERENT CU indices from the PSF list #
# Which of these CU indices are present in the NuSEDS CU list? #
CU_Bin_Ind <- c("SEL-06-910", "SEL-06-911", "SEL-10-913","SEL-13-008", "SER-101", "SEL-13-025")
#unique(cu.sites[cu.sites$FULL_CU_IN %in% c("SEL-06-910", "SEL-06-911", "SEL-10-913","SEL-13-008", "SER-101", "SEL-13-025"), c(13,18,20)])
unique(cu.sites[cu.sites$FULL_CU_IN %in% CU_Bin_Ind, colToKeep])

# Try looking up binned CU names in NuSEDS list #
# unique(cu.sites[cu.sites$CU_ACRO %in% c(str_subset(unique(cu.sites$CU_ACRO), "Seton-L")),c(13,18,20)])
# unique(cu.sites[cu.sites$CU_ACRO %in% c(str_subset(unique(cu.sites$CU_ACRO), "Nadina/Francois-ES")),c(13,18,20)])
# unique(cu.sites[cu.sites$CU_ACRO %in% c(str_subset(unique(cu.sites$CU_ACRO), "NBarriere-ES")),c(13,18,20)])
unique(cu.sites[grepl("Seton-L",cu.sites$CU_ACRO),colToKeep])
unique(cu.sites[grepl("Nadina/Francois-ES",cu.sites$CU_ACRO),colToKeep])
unique(cu.sites[grepl("NBarriere-ES",cu.sites$CU_ACRO),colToKeep])


# Remove rows for Atlantic, Steelhead, and Kokanee #
nuseds <- filter(nuseds, SPECIES %in% c("Chum", "Chinook", "Coho", "Pink", "Sockeye"))
cu.sites <- filter(cu.sites, SPECIES_QUALIFIED %in% c("CM", "CK", "CO", "PKE", "PKO", "SEL", "SER"))
# unique(nuseds$SPECIES)
# unique(cu.sites$SPECIES_QUALIFIED)

# Define variables to include in MAX_ESTIMATE
var_in_MAX_ESTIMATE <- c("NATURAL_ADULT_SPAWNERS", 
                         "NATURAL_JACK_SPAWNERS", 
                         "NATURAL_SPAWNERS_TOTAL", 
                         "ADULT_BROODSTOCK_REMOVALS", 
                         "JACK_BROODSTOCK_REMOVALS", 
                         "TOTAL_BROODSTOCK_REMOVALS", 
                         "OTHER_REMOVALS", 
                         "TOTAL_RETURN_TO_RIVER")

# Calculate MAX_ESTIMATE
# (dat is the NuSEDS data)
nuseds$MAX_ESTIMATE <- apply(nuseds[, var_in_MAX_ESTIMATE], 1, max, na.rm = TRUE)
nuseds[sapply(nuseds, is.infinite)] <- NA


BRUNO IS HEAR

# --- Part A: Organize stream/CU data from CU System Sites spreadsheet --- #
# --- Code adapted from LGL script: "UpdateNCCStreams.fn.R" (see "nccdbv2-master" folder) --- #

# - Step 1: Add NCCDB SpeciesId based on SPECIES_QUALIFIED - #
spp.code <- cu.sites$SPECIES_QUALIFIED
spp.code <- as.character(spp.code)

spp.code.qual <- c("CK", "CM", "CO", "PKE", "PKO", "SEL", "SER")

# Check to ensure NuSEDS SPECIES_QUALIFIED codes have not changed
check <- all(spp.code %in% spp.code.qual)
if (!check) { 
  stop("'SPECIES_QUALIFIED' codes have changed.")
}

spp.lookup <-  c("CN", "CM", "CO", "PKE", "PKO",  "SX",  "SX")  # SpeciesId
names(spp.lookup) <-spp.code.qual

# Convert codes
result <- spp.lookup[as.character(spp.code)]

# check to ensure all species code conversions worked.
if (any(is.na(result))) {
  warning("Missing SPECIES_QUALIFIED conversion")
}

head(result)
cu.sites$SpeciesId <- result

# Ensure we have a 1:1 mapping of SPECIES_QUALIFIED to SpeciesId
if (!all(table(unique(cu.sites[c("SPECIES_QUALIFIED", "SpeciesId")])$SPECIES_QUALIFIED) == 1)) {
  stop("Error converting SPECIES_QUALIFIED")
}

# - Step 2: Calculate stream escapement - #
# - Code adapted from LGL script: "CalculateStreamEscapement.fn.R" - #

# Compute escapement #

# Settings #
legacy = FALSE
zeros = FALSE
ncc.only = FALSE

meta.data = TRUE
na.rm = FALSE

# Step 1: Use UNSPECIFIED_RETURNS -----------------------------------------
adult.returns <- data.frame(
  Id =  nuseds[[c("ACT_ID")]],
  PopId = nuseds[[c("POP_ID")]],
  Year =   nuseds[[c("ANALYSIS_YR")]],
  nuseds[c("AREA", "SPECIES", "ADULT_PRESENCE")],
  Returns = nuseds[["NATURAL_ADULT_SPAWNERS"]],
  Source = "NATURAL_ADULT_SPAWNERS",
  stringsAsFactors = FALSE
)

# Return if we have a value for every stream
any.return <- !is.na(nuseds[["NATURAL_ADULT_SPAWNERS"]])
# if (!any(any.return == FALSE)) return(adult.returns)

# Step 2 ------------------------------------------------------------------
# otherwise, we need to calculate the adult spawners - get the [Natural_Adult_Spawners]
adult.returns[["Source"]][!any.return] <- NA
adult.returns[["Spawners"]][!any.return] <- nuseds[["NATURAL_SPAWNERS_TOTAL"]][!any.return]
adult.returns[["SpawnersSource"]][!any.return] <- "NATURAL_SPAWNERS_TOTAL"

check2 <- is.na(nuseds[["NATURAL_ADULT_SPAWNERS"]]) & !any.return
if (any(check2)) {
  adult.returns[["SpawnersSource"]][!any.return & check2] <- NA
  adult.returns[["Spawners"]][check2] <- nuseds[["NATURAL_SPAWNERS_TOTAL"]][check2]
  adult.returns[["SpawnersSource"]][check2] <- "NATURAL_SPAWNERS_TOTAL"
}
adult.returns[["SpawnersSource"]][is.na(adult.returns[["Spawners"]])] <- NA


d1<- filter(adult.returns, PopId=='52625')
# Broodstock --------------------------------------------------
# 4) calculate brood stock - use [Adult_Broodstock_Removals]
# 5) if there is no value in [Adult_Broodstock_Removals], then use the value in [Total_Broodstock_Removals]
adult.returns[["Broodstock"]][!any.return] <- nuseds[['ADULT_BROODSTOCK_REMOVALS']][!any.return]
adult.returns[["BroodstockSource"]][!any.return] <- 'ADULT_BROODSTOCK_REMOVALS'

check3 <- is.na(adult.returns[["Broodstock"]]) & !any.return
if (any(check3)) {
  adult.returns[["Broodstock"]][check3] <- nuseds[["TOTAL_BROODSTOCK_REMOVALS"]][check3]
  adult.returns[["BroodstockSource"]][check3] <- "TOTAL_BROODSTOCK_REMOVALS"
}

adult.returns[["BroodstockSource"]][is.na(adult.returns[["Broodstock"]])] <- NA

d1<- filter(adult.returns, PopId=='52625')

# Removals ----------------------------------------------------------------
adult.returns[['Removals']] <- nuseds[[toupper("OTHER_REMOVALS")]]
adult.returns[['RemovalsSource']] <-  toupper("OTHER_REMOVALS")
adult.returns[["RemovalsSource"]][is.na(adult.returns[["Removals"]])] <- NA

d1<- filter(adult.returns, PopId=='52625')
# Calculate Returns ------------------------------------------------
adult.returns[["Returns"]][!any.return] <- apply(
  X = adult.returns[!any.return, c("Spawners","Broodstock", "Removals")],
  MARGIN = 1, 
  FUN = sum, 
  na.rm=TRUE
)  

adult.returns[["Source"]][!any.return] <- apply(adult.returns[c("SpawnersSource","BroodstockSource", "RemovalsSource")], 1, function(x) paste(na.omit(x),collapse=" + "))[!any.return]

d1<- filter(adult.returns, PopId=='3119')
# Set as NA rather than zero if no info in any column
check4 <- apply(
  X = adult.returns[!any.return, c("Spawners","Broodstock", "Removals")],
  MARGIN = 1, 
  FUN = function(x) all(is.na(x))
) 
adult.returns[["Returns"]][!any.return][check4] <- NA

d1<- filter(adult.returns, PopId=='52625')
# Check final source ---------------------------------------------
# 7) if we still don't have a value, use either [Tot_adult_ret_river] or [Total_return_to_river]
no.return <- is.na(adult.returns[["Returns"]])
if (any(no.return)) {
  adult.returns[["Returns"]][no.return] <- nuseds[[toupper("TOTAL_RETURN_TO_RIVER")]][no.return]
  adult.returns[["Source"]][no.return] <- toupper("TOTAL_RETURN_TO_RIVER")
}

no.return <- is.na(adult.returns[["Returns"]])
if (any(no.return)) {
  adult.returns[["Returns"]][no.return] <- nuseds[[toupper("TOTAL_RETURN_TO_RIVER")]][no.return]
  adult.returns[["Source"]][no.return] <- toupper("TOTAL_RETURN_TO_RIVER")
}

d1<- filter(adult.returns, PopId=='52625')
# OPTIONAL: MAX_ESTIMATE when all else fails  ---------------------------
# Finally, when none of the other methods are available #
#if (!legacy) {
#no.return <- is.na(adult.returns[["Returns"]])
#adult.returns[["Returns"]][no.return] <- nuseds[[toupper("max_estimate")]][no.return]
# adult.returns[["Source"]][no.return] <- toupper("max_estimate")
#} 

# Cleanup  ----------------------------------------------------------------
# Add the MAX_ESTIMATE field
adult.returns$MAX_ESTIMATE <-  nuseds[[toupper("max_estimate")]]

# Reset Source if no Returns provided
no.return <- is.na(adult.returns[["Returns"]])
adult.returns[["Source"]][no.return] <- NA

d1<- filter(adult.returns, PopId=='52625')
# Zero Counts -------------------------------------------------------------
if (zeros) {
  # browser()
  message("NuSEDS ADULT_PRESENCE == 'NONE OBSERVED' used as zero counts for escapement." )
  no.adult <- no.return & nuseds$ADULT_PRESENCE == 'NONE OBSERVED'
  adult.returns[["Returns"]][no.adult] <- 0
  adult.returns[["Source"]][no.adult] <- 'NONE OBSERVED'
}

# browser()
# Add NCC Salmon Database Fields ------------------------------------------
# @TODO - could use general ConvertSpeciesCode 

# Reference table for species codes #
spp.tab <- data.frame(
  nccsdb   = c( "CM",       "CN",   "CO",     NA, "PKE", "PKO",      "SX",      "SX"),   # NCC Salmon Database (NCCSDB) Designation 
  nuseds   = c("Chum", "Chinook", "Coho", "Pink",    NA,    NA, "Sockeye", "Sockeye"),     # NuSEDS
  cu_sites = c(  "CM",      "CK",   "CO",     NA,  "PKE", "PKO",    "SEL",     "SER"), # Conservation Unit Sites
  stringsAsFactors = FALSE
)

Convert2StatArea <- function(area){
  StatArea <- as.character(area)
  StatArea[area %in% c("3A", "3B")] <- 3
  StatArea[area %in% c("4A", "4B", "4C", "4D")] <- 4
  StatArea[area %in% c("1", "2W", "3", "4", "5", "6", "7", "8", "9")] <- paste0("0", StatArea[area %in% c("1", "2W", "3", "4", "5", "6", "7", "8", "9")])
  return(StatArea)
}

adult.returns <- within( 
  data = merge(
    x = adult.returns,
    y = data.frame(SpeciesName = spp.tab$nuseds, SpeciesId = spp.tab$nccsdb),
    by.x="SPECIES",
    by.y = "SpeciesName",
    all.x=TRUE   # LEFT JOIN
  ),
  {
    SpeciesId[SPECIES == "Pink" & Year %% 2 == 0] <- "PKE"  # Even pink years 
    SpeciesId[SPECIES == "Pink" & Year %% 2 != 0] <- "PKO"  # Odd pink years 
    IndexId <- paste(SpeciesId, PopId, sep="_")
    StatArea <- Convert2StatArea(AREA)
  }
)

d1<- filter(adult.returns, PopId=='52625')

adult.returns <- distinct(adult.returns)

# Subset and Sort -------------------------------------------------------
fields <- c("Id", "SpeciesId", "IndexId", "PopId", "Year", "StatArea", "Returns")
if (meta.data) {
  fields <- c(fields, setdiff(colnames(adult.returns), fields))
}

adult.returns <- adult.returns[fields]

# Sort results
adult.returns <- adult.returns[order(adult.returns$IndexId, adult.returns$Year), ]


# Return ------------------------------------------------------------------
if (na.rm) {
  escapement <- (subset(adult.returns, !is.na(Returns)))
} else {
  escapement <- (adult.returns)
}

# Summarize escapment statistics for each stream
# First, make corrections for populations with discrepancies in area assignments #
# These errors become apparent when merging data frames later on #

escapement[escapement$IndexId=="CO_46240",]$StatArea = "29"
escapement[escapement$IndexId=="PKO_51094",]$StatArea = "12"
escapement[escapement$IndexId=="SX_45495",]$StatArea = "120"

esc.summary <- ddply(escapement, c("IndexId", "SpeciesId", "PopId", "StatArea"), summarise,
                     nrecs  = sum(!is.na(AREA)),   # Total number of NuSEDS records
                     nnumest  = sum(!is.na(Returns)),
                     nins  = sum(ADULT_PRESENCE %in% c("NONE OBSERVED", "PRESENT")),
                     npres = sum(ADULT_PRESENCE %in% c("PRESENT")),
                     pinsrec = (sum(ADULT_PRESENCE %in% c("NONE OBSERVED", "PRESENT")))/(sum(!is.na(AREA))),
                     ppres_ins = sum(ADULT_PRESENCE %in% c("PRESENT"))/sum(ADULT_PRESENCE %in% c("NONE OBSERVED", "PRESENT")),
                     pest_pres = sum(!is.na(Returns))/sum(ADULT_PRESENCE %in% c("PRESENT")),
                     Escapement.total = sum(Returns, na.rm=TRUE),
                     Escapement.avg = mean(Returns, na.rm=TRUE),
                     Escapement.sd   = sd(Returns, na.rm=TRUE),
                     Escapement.se   = sd(Returns, na.rm=TRUE) / sqrt(nnumest),
                     Year.start = min(Year, na.rm=TRUE),
                     Year.end = max(Year, na.rm=TRUE)
)

d1<- filter(esc.summary, PopId=='52625')

# append available meta data
stream.list <- within(
  data = merge(
    x = esc.summary,
    y = cu.sites,
    by.x = c("PopId","SpeciesId"),
    by.y = c("POP_ID","SpeciesId"),
    all.x = TRUE
  ),
  {
    # List of streams with confirmed escapement, used for calculations
    Active <- Escapement.total > 0   
    # List of st
    NoEscapement <- !is.na(Escapement.total) & Escapement.total == 0
    Surveyed <- !is.na(Escapement.avg)
    CU <- paste0(SpeciesId, "_", CU_INDEX)
    SITE_ID <- ID
  }
)


# Stream List Filtering and Fixes  -------------------------------------------------
# Drop streams without CU_INDEX values
# drop.cuindex <- subset(stream.list, is.na(CU_INDEX))
# message(nrow(drop.cuindex), " potential NCC streams were dropped from the NCC Salmon database master stream listing because no CU_INDEX value was available.")
# stream.list <- subset(stream.list, !is.na(CU_INDEX))

# Fix CU_NAME field that may have "<<VREQ[Bin]>>"
# EA: I think we want to filter out binned CUs?
# stream.list$CU_NAME <- str_replace(stream.list$CU_NAME, "<<VREQ\\[Bin\\]>>", "")

# browser()
if (legacy){
  colnames(stream.list) <- str_replace(colnames(stream.list), "^TYPE$", "CU_TYPE")
}

# Select particular fields #
# fields <- c('SpeciesId', "IndexId", "PopId",  "Records", "Surveys", "Active", "SYSTEM_SITE", "StatArea",
#             "CU", "CU_NAME",  "CU_ACRO", "CU_LATITUDE", "CU_LONGITUDE",
#             "SPECIES_QUALIFIED", "YLAT", "XLONG", "FAZ_ACRO", "MAZ_ACRO", "JAZ_ACRO",
#             "SITE_ID",  "GFE_ID", "NUMBER_OF_SITES", "CU_TYPE",
#             "SBJ_ID", "IS_INDICATOR", "OL_GRP_NM", "OL_GRP_N", "AREA", "ISENH", "COMMENTS", "GFE_ID_IN_NUSEDS", "POP_ID_IN_NUSEDS","CMNT", "EFFECTIVE_DT")
# 
# if (!all(check <- fields %in% colnames(stream.list))) {
#   stop("Final streams listing missing fields:", paste(fields[!check], collapse=", "))
# } 
# # View(stream.list[c(fields)])
# 
# stream.list <- stream.list[fields]

# CHECK Record Count -----------------------------------------------------
# Ensure our record count is accurate
check <- merge(
  x = stream.list[c("IndexId", "nrecs")],
  y = as.data.frame(table(escapement$IndexId)),
  by.x = "IndexId",
  by.y = "Var1"
)

if (!all(check$nrecs == check$Freq)) stop("Record count error")
check[check$nrecs != check$Freq,]

# Check for duplications
piv <- table(stream.list$IndexId)

# Print which IndexIds have duplicates
piv[piv > 1]

#remove <- c("CO_46240", "PKO_51094", "SX_45495") # Temporary fix for populations with duplicates (from earlier check)
#remove2 <- c("SX_43790", "SX_47590", "SX_49234") # Temporary fix for populations with duplicates caught during transposing
# 
# streams <- filter(stream.list, !(IndexId %in% remove))
# streams <- filter(streams, !(IndexId %in% remove2))

streams <- stream.list

# CHECK: CU indicator ER --------------------------------------------------
sa.cu.lk <- ddply(
  .data = subset(streams, Active=TRUE),
  .variables =   c("SpeciesId", "CU", "CU_NAME"), 
  .fun = summarise, 
  StatArea = paste(sort(unique(StatArea)), collapse=", ")
)

# --- Part 2: Formatting output data frame with CU info and escapement info --- #

# z <- filter(escapement, !(IndexId %in% remove))
# z <- filter(z, !(IndexId %in% remove2))

z <- escapement

id <- unique(z$IndexId)


#z[which(z$Returns==0),]$Returns <- "ZERO"

# zz <- z[,-c(1,10:18)]
# zz <- z[,c(3,5,7)]

# Widen data frame #
zz <- dcast(melt(z[,c(3,5,7)], id.vars = c("IndexId", "Year")),
            IndexId + variable ~ Year,
            value.var = "value",
            fun.aggregate=sum, # Come back to this, currently won't let me widen data without aggregation function
            fill = 0.12345) # Filler to make sure can distinguish true zeros

#duplicates <- which(zz[,3:ncol(zz)][1] > 1)
zz <- zz[,-2]

# Putting together extra info for final data frame #
names(zz)
names(streams)
names(sa.cu.lk)

# Merging dataframes - THIS IS WHEN TO REMOVE BINNED CUs #
data <- merge(streams, zz, by="IndexId")

# Updating "SpeciesId" for sockeye populations with river/lake specification #
data$SpeciesId <- as.character(data$SpeciesId)

data$SpeciesId <- ifelse(!is.na(data$SPECIES_QUALIFIED) & data$SPECIES_QUALIFIED == "SEL", "SEL", data$SpeciesId)
data$SpeciesId <- ifelse(!is.na(data$SPECIES_QUALIFIED) & data$SPECIES_QUALIFIED == "SER", "SER", data$SpeciesId)

# Cleaning up data #
data$IndexId <- paste(data$SpeciesId, data$PopId,sep="_")
data$CU_INDEX <- as.character(gsub("\\D", "", data$CU_INDEX))

data$CU_fname <- paste(data$SpeciesId, data$CU_NAME, sep="::")
data$CU_facro <- paste(data$SpeciesId, data$CU_ACRO, sep="::")
data$CU_findex <- data$FULL_CU_IN

# Set all zero escapement values to NA #
data[,56:157][data[,56:157] == 0.12345] <- " "
data[,56:157][is.na(data[,56:157])] <- " "

# Check out CUs that are no longer current # 
bin <- filter(data, CU_TYPE %in% c("bin", "Deleted", "Bin", "VREQ[Bin]", "VREQ[Extirpated]", "Extirpated"))
length(unique(bin$CU_findex))

bin1 <- unique(bin$CU_findex)
bin2 <- (unique(cu.sites[cu.sites$CU_TYPE %in% c("bin", "Deleted", "Bin", "VREQ[Bin]", "VREQ[Extirpated]", "Extirpated"),]$FULL_CU_IN))
bin3 <- (unique(bb.cu[bb.cu$CU_type %in% c("bin", "Deleted", "Bin", "VREQ[Bin]", "VREQ[Extirpated]", "Extirpated"),]$CU_findex))
setdiff(bin2, bin3)

# Put together final data frame #
Z.fields <- c("PopId","SpeciesId","GFE_ID","SYSTEM_SITE","Y_LAT","X_LONGT",
              "FAZ_ACRO","MAZ_ACRO","JAZ_ACRO","CU_fname","CU_facro","CU_findex",
              "CU_NAME","CU_ACRO","CU_INDEX","ISENH","IS_INDICATOR","nrecs","nins",
              "npres","nnumest","pinsrec","ppres_ins","pest_pres","OL_GRP_NM","StatArea","IndexId",
              "NoEscapement")

Z <- data[,Z.fields]
Z <- cbind(Z, data[,56:157])

# Add extra columns #
length <- length(Z$IndexId)
extras <- data.frame(ID = rep(NA, length),
                     Indicator = rep(NA, length),
                     Source = rep("NuSEDS", length),
                     popMAP = rep(NA, length),
                     EXPN = rep(NA, length),
                     Area = Z$StatArea,
                     IsInFiltEsc = rep(NA, length),
                     fr_timing = rep(NA, length),
                     Fraser_mnemonic = rep(NA, length),
                     CD_findex.name = rep(NA, length),
                     Reviewer = rep(NA, length),
                     QA = rep(NA, length),
                     Method = rep(NA, length),
                     Race = rep(NA, length),
                     WildCode = rep(NA, length),
                     WildRigor = rep(NA, length),
                     PopId = rep(NA, length),
                     SpeciesId2 = rep(NA, length))

# Put it all together #                     
ZZ <- cbind(Z, extras)

final.fields <- c("ID","PopId","Indicator","SpeciesId","GFE_ID",
                  "SYSTEM_SITE","Source","popMAP","EXPN","Y_LAT",
                  "X_LONGT","FAZ_ACRO","MAZ_ACRO","JAZ_ACRO","CU_fname",
                  "CU_facro","CU_findex","CU_NAME","CU_ACRO","CU_INDEX",
                  "Area","ISENH","IS_INDICATOR","IsInFiltEsc","nrecs","nins",
                  "npres","nnumest","pinsrec","ppres_ins","pest_pres","fr_timing",
                  "OL_GRP_NM","Fraser_mnemonic","CD_findex.name","StatArea","Reviewer",
                  "QA","Method","Race","WildCode","WildRigor","IndexId","PopId","SpeciesId2",
                  "NoEscapement")

# All the columns in the right order #
final <- ZZ[,final.fields]
final <- cbind(final,Z[,c(29:130)])

# Remove populations with no escapement data at all #
final <- filter(final, !(NoEscapement))
final <- final[,-46]

# Rename the columns per reference data frame #
colnames(final)[1:45] <- colnames(refdat)[1:45]


# Write to file #
setwd(dir.out)

#hardcode fixes to nuseds

#babine/onerka
final$CU_findex[final$POP_ID==49379] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49384] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49354] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==45452] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49389] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49394] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49404] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49419] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49399] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49424] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49434] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49439] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48599] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48674] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==45462] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48684] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48064] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48069] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48074] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48094] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48099] <- "SEL-21-02-EW"

#nilkitkwa
final$CU_findex[final$POP_ID==49369] <- "SEL-21-02-LW"
final$CU_findex[final$POP_ID==49374] <- "SEL-21-02-LW"
final$CU_findex[final$POP_ID==49359] <- "SEL-21-02-LW"
final$CU_findex[final$POP_ID==49364] <- "SEL-21-02-LW"
final$CU_findex[final$POP_ID==49457] <- "SEL-21-02-LW"

# tahlo/morrison
final$CU_findex[final$POP_ID==49409] <- "SEL-21-02-MW"
final$CU_findex[final$POP_ID==49414] <- "SEL-21-02-MW"

#babine enhanced
final$CU_findex[final$POP_ID==3237] <- "SEL-21-02-F"
final$CU_findex[final$POP_ID==45467] <- "SEL-21-02-F"
final$CU_findex[final$POP_ID==45472] <- "SEL-21-02-F"
final$CU_findex[final$POP_ID==3238] <- "SEL-21-02-F"
final$CU_findex[final$POP_ID==45482] <- "SEL-21-02-F"

### bella coola chum
final$CU_findex[final$POP_ID==3119] <- "CM-16"
final$CU_findex[final$POP_ID==51771] <- "CM-16"
final$CU_findex[final$POP_ID==51772] <- "CM-16"
final$CU_findex[final$POP_ID==3143] <- "CM-16"
final$CU_findex[final$POP_ID==3122] <- "CM-16"
final$CU_findex[final$POP_ID==3125] <- "CM-16"
final$CU_findex[final$POP_ID==3138] <- "CM-16"
final$CU_findex[final$POP_ID==3128] <- "CM-16"
final$CU_findex[final$POP_ID==51778] <- "CM-16"




d1<- filter(final, POP_ID=='3119')

d1<- filter(nuseds, WATERBODY=='AIRPORT SIDE CHANNEL')


write.csv(final, "NuSEDS_escapement_data_collated_20230818.csv", row.names=FALSE)

# Notes:
# Remove CU's which are "BIN" at this stage? 










