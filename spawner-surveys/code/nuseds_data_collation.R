

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

# NOTE (to remove eventually): original script is:
# 1_nuseds_data_collationJun72023.R in:
# \X Drive\1_PROJECTS\1_Active\Fraser_VIMI\analysis\Compilation\Code

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

wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# Loading packages 
library(plyr)
library(dplyr)
library(tibble)
library(scales)
library(ggplot2)
library(readxl)
library(reshape2)
library(stringr)

#
# Functions ------
source("code/functions.R")

#
# Import datasets -----

#' ** Import reference file for formatting output **
#'This file is only used to rename the columns of the final dataframe at the end 
#' (i.e., NuSEDS_escapement_data_collated_date.csv).
# Origin in Dropbox: /X Drive/1_PROJECTS/Fraser_VIMI/analysis/Compilation/Reference
template_df <- read.delim(paste(wd_references_dropbox,"NCC_Streams_13March2016_KKE.txt",sep="/"),
                     header=TRUE, na.string="")

# Set up column names for empty destination dataframe
names <- c(names(template_df)[1:45], c(1926:2021))
# View(template_df)

#' ** Import the NuSEDS data (all_areas_nuseds) **

#' Imort the dataframe of the NuSEDS datasets of interest:
NuSEDS_datasets_names <- NuSEDS_datasets_names_fun()

# Import the all_areas_nuseds data:
all_areas_nuseds <- datasets_NuSEDS_fun(name_dataSet = NuSEDS_datasets_names$all_areas_nuseds, 
                                        from_NuSEDS_website = F, 
                                        wd = wd_data_dropbox)

colnames(all_areas_nuseds)
# View(all_areas_nuseds)

# cf. references/useds_report_definitions.xlsx for the definitions of terms:
unique(all_areas_nuseds$POPULATION)    # Default naming originates from previous databases =  stream name + subdistrict + species + run type. This is the most important piece of data that all the other SEN data fields refers to.
unique(all_areas_nuseds$POP_ID)        # population ID
unique(all_areas_nuseds$GFE_ID)        # stream ID
unique(all_areas_nuseds$WATERBODY)     # name of the waterbody or portion of a waterbody that bounds the population as shown on any given SEN.
unique(all_areas_nuseds$WATERBODY_ID)  # 
unique(all_areas_nuseds$ACT_ID)        # primary key for SEN

# TODO: implement comparison with previous version. Below is a temporary solution.
# all_areas_nuseds_old <- read.csv(paste(wd_data_dropbox,"All Areas NuSEDS.csv",sep="/"),
#                        header=TRUE, stringsAsFactors=FALSE)
# 
# compareColNamesDF_fun(all_areas_nuseds,all_areas_nuseds_old)

#' ** Import the NuSEDS list of CUs (conservation_unit_system_sites): **
# ???: what's the goal?
# DFO provided files matching streams and Nuseds to full CU index 
conservation_unit_system_sites <- datasets_NuSEDS_fun(name_dataSet = NuSEDS_datasets_names$conservation_unit_system_sites, 
                                                      from_NuSEDS_website = F, 
                                                      wd = wd_data_dropbox)

colnames(conservation_unit_system_sites)
unique(conservation_unit_system_sites$SPECIES_QUALIFIED)
# View(conservation_unit_system_sites)

# cf. references/conservation_unit_report_definitions.csv for definitions:
unique(conservation_unit_system_sites$CU_NAME)        # The assigned name of the Conservation Unit. Note that this name does not identify the species.
unique(conservation_unit_system_sites$CU_ACRO)        # the CUs' acronyms
unique(conservation_unit_system_sites$CU_INDEX)       # = "species code" + "Conservation Unit Index" (?) NOTE from Cathy: old code that should not be trusted 
unique(conservation_unit_system_sites$CU_TYPE)        # 
unique(conservation_unit_system_sites$SYSTEM_SITE)    # 
unique(conservation_unit_system_sites$WATERSHED_CDE)  # 45 digit hierarchical provincial code unique to the waterbody and its watershed  
unique(conservation_unit_system_sites$FULL_CU_IN)     #' The full index of the CU including the species qualifier (SPECIES_QUALIFIED), e.g. CK-01 *** to compare with the PSF "CU_INDEX" or "cu_index" ***
unique(conservation_unit_system_sites$POP_ID)         # A unique numeric code identifying the population
range(conservation_unit_system_sites$POP_ID)          # so different from the number in cuid or CU_FULL_IN
unique(conservation_unit_system_sites$GFE_ID)         # Numeric code identifying the waterbody. From NUSEDS with some additions and modifications. Same as Stream_Id
unique(conservation_unit_system_sites$FWA_WATERSHED_CDE) 
unique(conservation_unit_system_sites$SPECIES_QUALIFIED)   # This is an Conservation Unit acronym used to describe the species of salmon for which the escapement estimate is for, eg:  CK - Chinook Salmon CM - Chum Salmon CO - Coho Salmon PKE - Even Year Pink Salmon PKO - Odd Year Pink Salmon SEL - Lake Type Sockeye Salmon SER - River or Ocean Type Sockeye Salmon  

# TODO: implement comparison with previous version. Below is a provisory solution.
# conservation_unit_system_sites_old <- read.csv(paste(wd_data_dropbox,"conservation_unit_system_sitesJul2023.csv",sep="/"),
#                          header=TRUE, stringsAsFactors=FALSE)
# names(conservation_unit_system_sites_old)[1] <- "ID"
# unique(conservation_unit_system_sites_old$ID)  # QUESTION: there are only NAs ???
# 
# compareColNamesDF_fun(conservation_unit_system_sites,conservation_unit_system_sites_old)

#' TODO: check if the new version works despite missing certain columns. If it 
#' does not work, figure out what column(s) is needed and then ask how it was 
#' obtained.

# If we decide to keep the older version, do:
# conservation_unit_system_sites <- conservation_unit_system_sites_old
# names(conservation_unit_system_sites)[1] <- "ID"
# unique(conservation_unit_system_sites_old[,1])

colnames(all_areas_nuseds)[colnames(all_areas_nuseds) %in% colnames(conservation_unit_system_sites)]
# "WATERSHED_CDE" "POP_ID"        "GFE_ID"

#' ** Import PSF list of CUs **

#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()
# ???: what's the goal?

conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = F,
                                                   update_file_csv = F,
                                                   wd = wd_pop_indic_data_input_dropbox)

unique(conservationunits_decoder$cu_index)   # Katy: carried through from earlier versions of CUs. I would not rely on the values in the CU_INDEX field to match to FULL_CU_IN in 
unique(conservationunits_decoder$cuid)       # The PSF unique identifier
unique(conservationunits_decoder$cu_name_pse)    # 
unique(conservationunits_decoder$cu_name_dfo)   
unique(conservationunits_decoder$region)     # SCVI = ???
unique(conservationunits_decoder$cu_type)    # same as the DFO conservation_unit_system_sites$CU_TYPE   

conservationunits_decoder[conservationunits_decoder$cuid == 936,] # CU that disappeared from older dataset (see google doc) --> TODO: check with Eric and Katy to make sure

#' ** Import WHAT IS IT ??? TO FINISH AT HOME CHECK IF THIS IS THE STREAM DATA FROM DATABASE**
# ???: where is it coming from? --> streamspawnersurveys_output to check 
# ???: what's the goal?
# List of CUs for the VIMI Fraser???
# Katy: "this file contains stream IDs in the Skeena. why it's tagged with vimi I do not know."
# vimi.sites <- read.delim(paste(wd_data_dropbox,"ssp.streams_20230719.txt",sep = "/"),
#                          header=TRUE, stringsAsFactors=FALSE)
# colnames(vimi.sites)[colnames(vimi.sites) == "streamname"] <- "SYSTEM_SITE"
# head(vimi.sites)
# 
# streamspawnersurveys_output <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[4],
#                                                      fromDatabase = F,
#                                                      update_file_csv = F,
#                                                      wd = wd_pop_indic_data_input_dropbox)
# head(streamspawnersurveys_output)


# CHECKS -------
#
#'* CHECK 1 ??? QUESTION: is that a QA/QC here below? *

# BSC: return all the rows in vimi.sites without a match in conservation_unit_system_sites
# QUESTIONS: what is that for? Just a check up? What do we do with these CUs?
# da <- anti_join(vimi.sites, conservation_unit_system_sites, by = "SYSTEM_SITE")
# da

#'* CHECK 2 ??? * 
#' Compare NuSEDS CUs (i.e. conservation_unit_system_sites) with PSF list of CUs 
#' (i.e., conservationunits_decoder) - make sure that the CUs we are keeping are
#' the right ones. There are some discrepancies where CUs listed as "current" in
#' PSF list have been binned in NuSEDS list.
#' QUESTION: what to do in this case?


# Filter site info for CUs in south coast regions.

# colToKeep <- colnames(conservation_unit_system_sites)[c(13,18,20)] # bad practice
colToKeep <- c("CU_NAME","CU_TYPE","FULL_CU_IN")

# Which CUs in the NuSEDS "CU_Sites" file are labeled "Bin"?
conservation_unit_system_sites_bin <- unique(conservation_unit_system_sites[conservation_unit_system_sites$CU_TYPE == "Bin",colToKeep])
conservation_unit_system_sites_bin <- unique(conservation_unit_system_sites[grepl("[B|b]in",conservation_unit_system_sites$CU_TYPE),colToKeep])
conservation_unit_system_sites_bin

# Which are labeled "Extirpated"?
conservation_unit_system_sites_extir <- unique(conservation_unit_system_sites[conservation_unit_system_sites$CU_TYPE == "Extirpated",colToKeep])
conservation_unit_system_sites_extir <- unique(conservation_unit_system_sites[grepl("[E|e]xtirpated",conservation_unit_system_sites$CU_TYPE),colToKeep])
conservation_unit_system_sites_extir

# For the populations which are Binned in NuSEDs, we have DIFFERENT CU indices from the PSF list #
# Which of these CU indices are present in the NuSEDS CU list? #
CU_Bin_Ind <- c("SEL-06-910", "SEL-06-911", "SEL-10-913","SEL-13-008", "SER-101", "SEL-13-025") # QUESTION: are those CU indices from PSF?
conservationunits_decoder[conservationunits_decoder$CU_TYPE == "Bin",]  # BSC: these are different, I'm so confused...
conservationunits_decoder[conservationunits_decoder$CU_INDEX %in% conservation_unit_system_sites_bin$FULL_CU_IN,]  # QUESTION: I don't know how to match these two datasets...
conservationunits_decoder2[conservationunits_decoder2$CU_INDEX %in% conservation_unit_system_sites_bin$FULL_CU_IN,c("Region","CU_INDEX","cu_name_dfo","cu_name_pse","pooledcuid","CUID")]

conservationunits_decoder2[conservationunits_decoder2$CU_INDEX %in% conservation_unit_system_sites_extir$FULL_CU_IN,c("Region","CU_INDEX","cu_name_dfo","cu_name_pse","pooledcuid","CUID")]

unique(conservationunits_decoder$CUID)
unique(conservation_unit_system_sites_bin$CU_NAME) # transform to  lower case and remove "<<BIN>>" ?

conservationunits_decoder[conservationunits_decoder$CU_INDEX %in% CU_Bin_Ind,] # QUESTION: one is misisng + plus I thought CU_INDEX was not reliable

unique(conservationunits_decoder$CU_INDEX)
unique(conservationunits_decoder2$cu_index)
unique(conservation_unit_system_sites_bin$FULL_CU_IN)
unique(conservation_unit_system_sites$FULL_CU_IN)

unique(conservation_unit_system_sites$FULL_CU_IN)
unique(conservation_unit_system_sites_old$FULL_CU_IN)

unique(conservation_unit_system_sites_old$POP_ID_IN_NUSEDS)

conservation_unit_system_sites_bin

#unique(conservation_unit_system_sites[conservation_unit_system_sites$FULL_CU_IN %in% c("SEL-06-910", "SEL-06-911", "SEL-10-913","SEL-13-008", "SER-101", "SEL-13-025"), c(13,18,20)])
unique(conservation_unit_system_sites[conservation_unit_system_sites$FULL_CU_IN %in% CU_Bin_Ind, colToKeep])

# Try looking up binned CU names in NuSEDS list # QUESTION: try to do what? there is not correction being done
# unique(conservation_unit_system_sites[conservation_unit_system_sites$CU_ACRO %in% c(str_subset(unique(conservation_unit_system_sites$CU_ACRO), "Seton-L")),c(13,18,20)])
# unique(conservation_unit_system_sites[conservation_unit_system_sites$CU_ACRO %in% c(str_subset(unique(conservation_unit_system_sites$CU_ACRO), "Nadina/Francois-ES")),c(13,18,20)])
# unique(conservation_unit_system_sites[conservation_unit_system_sites$CU_ACRO %in% c(str_subset(unique(conservation_unit_system_sites$CU_ACRO), "NBarriere-ES")),c(13,18,20)])
unique(conservation_unit_system_sites[grepl("Seton-L",conservation_unit_system_sites$CU_ACRO),colToKeep])
unique(conservation_unit_system_sites[grepl("Nadina/Francois-ES",conservation_unit_system_sites$CU_ACRO),colToKeep])
unique(conservation_unit_system_sites[grepl("NBarriere-ES",conservation_unit_system_sites$CU_ACRO),colToKeep])

#
# CHECKS END ------

# Remove rows for Atlantic, Steelhead, and Kokanee #
sp_salmon_detail <- c("Chum", "Chinook", "Coho", "Pink even","Pink odd","Sockeye lake","Sockeye river")
sp_salmon <-        c("Chum", "Chinook", "Coho", "Pink","Pink","Sockeye","Sockeye")
sp_salmon_acro <-     c("CM", "CK", "CO", "PKE", "PKO", "SEL", "SER")
sp_salmon_acro_ncc <- c("CM", "CN", "CO", "PKE", "PKO",  "SX",  "SX")  # SpeciesId ; NCC Salmon Database (NCCSDB) Designation 

sp_salmon_names_acro_df <- data.frame(name = sp_salmon,
                                      name_detail = sp_salmon_detail,
                                      acronym = sp_salmon_acro,
                                      acronym_ncc = sp_salmon_acro_ncc)

all_areas_nuseds <- filter(all_areas_nuseds, !SPECIES %in% c("Steelhead","Atlantic","Kokanee"))

conservation_unit_system_sites <- filter(conservation_unit_system_sites, 
                                         SPECIES_QUALIFIED %in% sp_salmon_names_acro_df$acronym)

# unique(all_areas_nuseds$SPECIES)
# unique(conservation_unit_system_sites$SPECIES_QUALIFIED)

# Part A: Organize stream/CU data from CU System Sites spreadsheet (???) ----
# Code adapted from LGL script: "UpdateNCCStreams.fn.R" (see "nccdbv2-master" folder)

# - Step 1: Add NCCDB SpeciesId based on SPECIES_QUALIFIED - #
# spp.code <- conservation_unit_system_sites$SPECIES_QUALIFIED # CU acronym used to describe the species of salmon for which the escapement estimate is for, eg:  CK - Chinook Salmon CM - Chum Salmon CO - Coho Salmon PKE - Even Year Pink Salmon PKO - Odd Year Pink Salmon SEL - Lake Type Sockeye Salmon SER - River or Ocean Type Sockeye Salmon  
# spp.code <- as.character(spp.code)
# spp.code.qual <- c("CK", "CM", "CO", "PKE", "PKO", "SEL", "SER") # needed? --> use sp_salmon_acro instead

# Check to ensure NuSEDS SPECIES_QUALIFIED codes have not changed
#check <- all(spp.code %in% spp.code.qual)
check <- all(conservation_unit_system_sites$SPECIES_QUALIFIED %in% sp_salmon_names_acro_df$acronym)
if(!check){ 
  stop("conservation_unit_system_sites$SPECIES_QUALIFIED codes have changed.")
  out <- conservation_unit_system_sites$SPECIES_QUALIFIED[! conservation_unit_system_sites$SPECIES_QUALIFIED %in% sp_salmon_names_acro_df$acronym]
  print(out)
}

# convert SPECIES_QUALIFIED to SpeciesId, i.e. the acronyms used in NCC Salmon Database (NCCSDB) Designation
conservation_unit_system_sites$SpeciesId <- NA
for(a in unique(conservation_unit_system_sites$SPECIES_QUALIFIED)){
  # a <- unique(conservation_unit_system_sites$SPECIES_QUALIFIED)[1]
  a_ncc <- sp_salmon_names_acro_df$acronym_ncc[sp_salmon_names_acro_df$acronym == a]
  conservation_unit_system_sites$SpeciesId[conservation_unit_system_sites$SPECIES_QUALIFIED == a] <- a_ncc
}

# check to ensure all species code conversions worked.
if(any(is.na(conservation_unit_system_sites$SpeciesId))){
  warning("Missing SPECIES_QUALIFIED conversion")
}

# spp.lookup <-  c("CN", "CM", "CO", "PKE", "PKO",  "SX",  "SX")  # SpeciesId ; NCC Salmon Database (NCCSDB) Designation 
# names(spp.lookup) <- spp.code.qual
# 
# # Convert codes
# result <- spp.lookup[as.character(spp.code)]
# spp.lookup[spp.code]
# 
# # check to ensure all species code conversions worked.
# if(any(is.na(result))) {
#   warning("Missing SPECIES_QUALIFIED conversion")
# }

# head(result)
# unique(result)
# conservation_unit_system_sites$SpeciesId <- result  # TODO: change name to speciesID_NCCDB for instance --> NO cause I think the name is used in PSE (?) + skip the creation of the object results

# Ensure we have a 1:1 mapping of SPECIES_QUALIFIED to SpeciesId (i.e. sp_salmon_acro with sp_salmon_acro_ncc)
if(!all(table(unique(conservation_unit_system_sites[c("SPECIES_QUALIFIED", "SpeciesId")])$SPECIES_QUALIFIED) == 1)){
  stop("Error converting SPECIES_QUALIFIED")
}

# - Step 2: Calculate stream escapement - #
# - Code adapted from LGL script: "CalculateStreamEscapement.fn.R" - #
# TODO: explain the method

# Compute escapement #

# Settings #
legacy = FALSE    # ???
zeros = FALSE     # NuSEDS ADULT_PRESENCE == 'NONE OBSERVED' used as zero counts for escapement.
ncc.only = FALSE  # ???"
meta.data = TRUE  # ???
na.rm = FALSE     # if True, escapement does not contains rows with NAs for Returns

# Step 1: Use UNSPECIFIED_RETURNS (??? we use NATURAL_ADULT_SPAWNERS here) 
escapement <- data.frame(
  Id =  all_areas_nuseds$ACT_ID,                             # This is the primary key for the SEN (from references/nuseds_report_definitions.csv)
  PopId = all_areas_nuseds$POP_ID,                           # A unique numeric code identifying the population
  Year =  all_areas_nuseds$ANALYSIS_YR,
  all_areas_nuseds[c("AREA", "SPECIES", "ADULT_PRESENCE")],
  stringsAsFactors = FALSE
)

# Return if we have a value for every stream
escapement$Returns <- all_areas_nuseds$NATURAL_ADULT_SPAWNERS         # All salmon that have reached maturity, excluding jacks (jacks are salmon that have matured at an early age).
escapement$Source <- "NATURAL_ADULT_SPAWNERS"                        # this field will change below depending on data availability and origin
NATURAL_ADULT_SPAWNERS_any <- !is.na(all_areas_nuseds$NATURAL_ADULT_SPAWNERS)
escapement$Source[!NATURAL_ADULT_SPAWNERS_any] <- NA

#' Step 2: define the sum of:
#' - "Spawner" = NATURAL_SPAWNERS_TOTAL +
#' - "Broodstock" = ADULT_BROODSTOCK_REMOVALS (or TOTAL_BROODSTOCK_REMOVALS if not available) + 
#' - "Removals" = OTHER_REMOVALS

# Spawners:
escapement$Spawners <- all_areas_nuseds$NATURAL_SPAWNERS_TOTAL
spawners_any <- !is.na(escapement$Spawners)
escapement$SpawnersSource[!NATURAL_ADULT_SPAWNERS_any & spawners_any] <- "NATURAL_SPAWNERS_TOTAL"

# Broodstock:
escapement$Broodstock <- all_areas_nuseds$ADULT_BROODSTOCK_REMOVALS
ADULT_BROODSTOCK_REMOVALS_any <- !is.na(all_areas_nuseds$ADULT_BROODSTOCK_REMOVALS)
TOTAL_BROODSTOCK_REMOVALS_any <- !is.na(all_areas_nuseds$TOTAL_BROODSTOCK_REMOVALS)
toReplace <- !ADULT_BROODSTOCK_REMOVALS_any & TOTAL_BROODSTOCK_REMOVALS_any
escapement$Broodstock[toReplace] <- all_areas_nuseds$TOTAL_BROODSTOCK_REMOVALS[toReplace]
escapement$BroodstockSource[!NATURAL_ADULT_SPAWNERS_any & ADULT_BROODSTOCK_REMOVALS_any] <- 'ADULT_BROODSTOCK_REMOVALS'
escapement$BroodstockSource[toReplace] <- 'TOTAL_BROODSTOCK_REMOVALS'

# Removals:
escapement$Removals <- all_areas_nuseds$OTHER_REMOVALS
OTHER_REMOVALS_any <- !is.na(all_areas_nuseds$OTHER_REMOVALS)
escapement$RemovalsSource[!NATURAL_ADULT_SPAWNERS_any & OTHER_REMOVALS_any] <- "OTHER_REMOVALS"

# Calculate Returns when !NATURAL_ADULT_SPAWNERS_any as the sum of what other 
# sources of data is available:
escapement$Returns[!NATURAL_ADULT_SPAWNERS_any] <- apply(
  X = escapement[!NATURAL_ADULT_SPAWNERS_any, c("Spawners","Broodstock","Removals")],
  MARGIN = 1, 
  FUN = sum, 
  na.rm = TRUE
)

escapement$Source[!NATURAL_ADULT_SPAWNERS_any] <- apply(
  X = escapement[!NATURAL_ADULT_SPAWNERS_any,c("SpawnersSource","BroodstockSource","RemovalsSource")], 
  MARGIN = 1, 
  FUN = function(x){
    paste(na.omit(x),collapse=" + ")}
)

# Set as NA rather than zero if no info in any column (the na.rm = T above produced)
# 0s when only NAs were available).
check4 <- apply(
  X = escapement[!NATURAL_ADULT_SPAWNERS_any, c("Spawners","Broodstock", "Removals")],
  MARGIN = 1, 
  FUN = function(x){all(is.na(x))}
) 
escapement$Returns[!NATURAL_ADULT_SPAWNERS_any][check4] <- NA

# Check final source: TOTAL_RETURN_TO_RIVER

# 7) if we still don't have a value, use either [Tot_adult_ret_river] or [Total_return_to_river]
returns_any <- !is.na(escapement$Returns)
TOTAL_RETURN_TO_RIVER_any <- !is.na(all_areas_nuseds$TOTAL_RETURN_TO_RIVER)
escapement$Returns[!returns_any & TOTAL_RETURN_TO_RIVER_any] <- all_areas_nuseds$TOTAL_RETURN_TO_RIVER[!returns_any & TOTAL_RETURN_TO_RIVER_any]
escapement$Source[!returns_any & TOTAL_RETURN_TO_RIVER_any] <- "TOTAL_RETURN_TO_RIVER"

#escapement[!is.na(escapement$Spawners) & !is.na(escapement$Broodstock),]

# OPTIONAL: MAX_ESTIMATE when all else fails  ??? QUESTION: to remove then? Do we even need MAX_ESTIMATE?
# Finally, when none of the other methods are available #
#if (!legacy) {
#no.return <- is.na(escapement[["Returns"]])
#escapement[["Returns"]][no.return] <- all_areas_nuseds[[toupper("max_estimate")]][no.return]
# escapement[["Source"]][no.return] <- toupper("max_estimate")
#} 


#' Define variables to include in MAX_ESTIMATE. MAX_ESTIMATE is the maximum estimate
#' of all these fields.
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
escapement$MAX_ESTIMATE <- apply(all_areas_nuseds[,var_in_MAX_ESTIMATE], 1, 
                                 max, na.rm = TRUE)

# Replace infinite values by NA
escapement[sapply(escapement, is.infinite)] <- NA

# Zero Counts -------------------------------------------------------------

if(zeros){
  # browser()
  message("NuSEDS ADULT_PRESENCE == 'NONE OBSERVED' used as zero counts for escapement.")
  no.adult <- !returns_any & all_areas_nuseds$ADULT_PRESENCE == 'NONE OBSERVED'
  escapement$Returns[no.adult] <- 0
  escapement$Source[no.adult] <- 'NONE OBSERVED'
}

# browser()

# Add NCC Salmon Database Fields: SpeciesName and SpeciesId---------------------
# @TODO - could use general ConvertSpeciesCode 

# Reference table for species codes #
# spp.tab <- data.frame(
#   nccsdb   = c( "CM",       "CN",   "CO",     NA, "PKE", "PKO",      "SX",      "SX"),   # NCC Salmon Database (NCCSDB) Designation 
#   nuseds   = c("Chum", "Chinook", "Coho", "Pink",    NA,    NA, "Sockeye", "Sockeye"),     # NuSEDS
#   cu_sites = c(  "CM",      "CK",   "CO",     NA,  "PKE", "PKO",    "SEL",     "SER"), # Conservation Unit Sites
#   stringsAsFactors = FALSE
# )

sp_salmon_names_acro_df$acronym_ncc # = spp.tab$nccsdb
sp_salmon_names_acro_df$name # = spp.tab$nuseds

unique(escapement$SPECIES)
unique(all_areas_nuseds$SPECIES)

# escapement <- merge(x = escapement,
#                     y = data.frame(SpeciesName = sp_salmon_names_acro_df$name, 
#                                    SpeciesId = sp_salmon_names_acro_df$acronym_ncc),
#                     by.x = "SPECIES",
#                     by.y = "SpeciesName",
#                     all.x = TRUE)
# 
# escapement$SpeciesId[escapement$SPECIES == "Pink" & escapement$Year %% 2 == 0] <- "PKE"  # Even pink years
# escapement$SpeciesId[escapement$SPECIES == "Pink" & escapement$Year %% 2 != 0] <- "PKO"  # Odd pink years
# escapement$IndexId <- paste(escapement$SpeciesId, escapement$PopId, sep = "_")
# escapement$StatArea <- Convert2StatArea(escapement$AREA)
  
escapement <- within(
  data = merge(
    x = escapement,
    y = data.frame(SpeciesName = sp_salmon_names_acro_df$name, 
                   SpeciesId = sp_salmon_names_acro_df$acronym_ncc),
    by.x ="SPECIES",
    by.y = "SpeciesName",
    all.x = TRUE   # LEFT JOIN
  ),
  {
    SpeciesId[SPECIES == "Pink" & Year %% 2 == 0] <- "PKE"  # Even pink years
    SpeciesId[SPECIES == "Pink" & Year %% 2 != 0] <- "PKO"  # Odd pink years
    IndexId <- paste(SpeciesId, PopId, sep="_")
    StatArea <- Convert2StatArea(AREA)                      # AREA is the subdistrict
  }
)

head(escapement)
colnames(escapement)
unique(escapement$SpeciesId)

# Remove duplicated rows
nrow(escapement) # 561548
escapement <- distinct(escapement)
nrow(escapement) # 411886

#
# Subset and Sort -------------------------------------------------------

# Define the field for the meta data (???)
fields <- c("Id", "SpeciesId", "IndexId", "PopId", "Year", "StatArea", "Returns")
if(meta.data){
  # BSC: concatenate fields with column names in escapement that are not in fields
  fields <- c(fields, setdiff(colnames(escapement), fields)) # setdiff(x,y) is the same as: x[! x %in% y]
}

# Re-order columns
escapement <- escapement[,fields]

# Sort results, 1st by IndexId, then Year:
escapement <- escapement[order(escapement$IndexId, escapement$Year), ]

# Return ??? ------------------------------------------------------------------
if(na.rm){
  escapement <- subset(escapement, !is.na(Returns))
}

# Summarize escapement statistics for each stream

# First, make corrections for populations with discrepancies in area assignments #
# These errors become apparent when merging data frames later on #
escapement[escapement$IndexId == "CO_46240",]$StatArea <- "29"
escapement[escapement$IndexId == "PKO_51094",]$StatArea <- "12"  # BSC: there is one ""
escapement[escapement$IndexId == "SX_45495",]$StatArea <- "120"  # BSC: already "120"

# BSC: calculate different statistics per group of unique combination of the .variables
# IndexId is = to paste(SpeciesId, PopId, sep="_")
# SpeciesId is = to sp_salmon_names_acro_df$acronym_ncc = the NCC Salmon Database (NCCSDB) Designation
# PopId is = to the all_areas_nuseds$POP_ID
# StatArea is = to Convert2StatArea(AREA), with AREA the subdistrict
esc.summary <- ddply(.data = escapement,
                     .variables = c("IndexId", "SpeciesId", "PopId", "StatArea"), 
                     .fun = summarise,
                     nrecs  = sum(!is.na(AREA)),   # Total number of NuSEDS records
                     nnumest  = sum(!is.na(Returns)), # number of observations
                     nins  = sum(ADULT_PRESENCE %in% c("NONE OBSERVED", "PRESENT")),  # nb of observations "inspected" (?)
                     npres = sum(ADULT_PRESENCE %in% c("PRESENT")),                   # nb observations with presence
                     pinsrec = (sum(ADULT_PRESENCE %in% c("NONE OBSERVED", "PRESENT")))/(sum(!is.na(AREA))), # proportion of observation inspected overall
                     ppres_ins = sum(ADULT_PRESENCE %in% c("PRESENT"))/sum(ADULT_PRESENCE %in% c("NONE OBSERVED", "PRESENT")), # proportion observations presence in the inspected ones
                     pest_pres = sum(!is.na(Returns))/sum(ADULT_PRESENCE %in% c("PRESENT")), # average returns per observation (?)
                     Escapement.total = sum(Returns, na.rm=TRUE),
                     Escapement.avg = mean(Returns, na.rm=TRUE),
                     Escapement.sd   = sd(Returns, na.rm=TRUE),
                     Escapement.se   = sd(Returns, na.rm=TRUE) / sqrt(nnumest),
                     Year.start = min(Year, na.rm=TRUE),
                     Year.end = max(Year, na.rm=TRUE)
)

nrow(esc.summary) # 11475

# BSC: check for duplicated rows
nrow(unique(esc.summary[,c("PopId","SpeciesId")])) # 11474 vs. 11475
rowDupli <- which(duplicated(esc.summary[,c("PopId","SpeciesId")]))
PopId_SpeciesId <- esc.summary[rowDupli,c("PopId","SpeciesId")]
esc.summary[esc.summary$PopId == PopId_SpeciesId$PopId & 
              esc.summary$SpeciesId == PopId_SpeciesId$SpeciesId,]

all_areas_nuseds$AREA[all_areas_nuseds$SPECIES == "Chinook" & 
                        all_areas_nuseds$POP_ID == 47367]


#' BSC: how can those be the same population?
#' Any way, it looks like there is no data for StatArea = 110:

all_areas_nuseds[all_areas_nuseds$SPECIES == "Chinook" & 
                   all_areas_nuseds$POP_ID == 47367 &
                   all_areas_nuseds$AREA == 110,]

all_areas_nuseds[all_areas_nuseds$SPECIES == "Chinook" & 
                   all_areas_nuseds$POP_ID == 47367 &
                   all_areas_nuseds$AREA == "29I",]

escapement[escapement$IndexId == "CN_47367",]

#' BSC: remove data with AREA == "29I" because it contains not data:
toRemove <- escapement$IndexId == "CN_47367" & escapement$AREA == "29I"
escapement <- escapement[!toRemove,]
toRemove <- esc.summary$IndexId == "CN_47367" & esc.summary$StatArea == "29I"
esc.summary <- esc.summary[!toRemove,]


# append available meta data
# BSC: that does not work...
stream.list <- within(
  data = merge(
    x = esc.summary,
    y = conservation_unit_system_sites,
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
    SITE_ID <- NA # ID  # QUESTION: this is conservation_unit_system_sites_old$X.ID. which contains only NAs and is not present in conservation_unit_system_sites ?! What's the point of it?
  }
)

nrow(stream.list) # 11477 --> there are two extra rows --> find them

# check for duplicates
nrow(unique(stream.list[,c("PopId","SpeciesId")])) # 11474 vs. 11477
rowDupli <- which(duplicated(stream.list[,c("PopId","SpeciesId")]))
PopId_SpeciesId <- stream.list[rowDupli,c("PopId","SpeciesId")]
for(i in 1:length(rowDupli)){
  out <- stream.list[stream.list$PopId == PopId_SpeciesId$PopId[i] & 
                       stream.list$SpeciesId == PopId_SpeciesId$SpeciesId[i],]
  colNo <- c("FWA_WATERSHED_CDE","WATERSHED_CDE","EFFECTIVE_DT","CMNTS")
  colYes <- colnames(stream.list)[! colnames(stream.list) %in% colNo]
  print(out[,colYes])
  print("***")
}

#' BSC: it look like for these populations (except CN_47367 above), these are 
#' duplicates because they have the same counts but location differ slightly.
#' - CN_7479: SYSTEM_SITE = BRIDGE RIVER vs. HIUIHILL CREEK
#' - SX_45525: SYSTEM_SITE = NADINA CHANNEL Artificial vs. NADINA RIVER
#' - CN_47367: same as above --> UPDATE: it was removed in esc.summary so does not appear here anymore

#' TODO: decide which duplicate row to remove --> see if matters in the end.

#
# Stream List Filtering and Fixes  ---------------------------------------------
# Drop streams without CU_INDEX values
# drop.cuindex <- subset(stream.list, is.na(CU_INDEX))
# message(nrow(drop.cuindex), " potential NCC streams were dropped from the NCC Salmon database master stream listing because no CU_INDEX value was available.")
# stream.list <- subset(stream.list, !is.na(CU_INDEX))

# Fix CU_NAME field that may have "<<VREQ[Bin]>>"
# EA: I think we want to filter out binned CUs?
# stream.list$CU_NAME <- str_replace(stream.list$CU_NAME, "<<VREQ\\[Bin\\]>>", "")

# browser()
if(legacy){
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
# check <- merge(
#   x = stream.list[c("IndexId", "nrecs")],
#   y = as.data.frame(table(escapement$IndexId)), #  IndexId = paste(SpeciesId, PopId, sep="_")
#   by.x = "IndexId",
#   by.y = "Var1"       # i.e., IndexId
# )

#' BSC: same as above but easier to track the origin of the variables.
#' Also, should this not be done before creating stream.list ???
check <- merge(
  x = esc.summary[c("IndexId", "nrecs")],       #  nrecs  = sum(!is.na(AREA)) per unique combination of c("IndexId", "SpeciesId", "PopId", "StatArea")
  y = as.data.frame(table(escapement$IndexId)), #  IndexId = paste(SpeciesId, PopId, sep="_")
  by.x = "IndexId",
  by.y = "Var1"       # i.e., IndexId
)

if(!all(check$nrecs == check$Freq)){  # BSC:  nrecs  = sum(!is.na(AREA))
  print(check[check$nrecs != check$Freq,])
  stop("Record count error")
} 
check[check$nrecs != check$Freq,]

# Check for duplications
piv <- table(stream.list$IndexId)
piv[piv > 1] # Print which IndexIds have duplicates
stream.list[duplicated(stream.list$IndexId),c("PopId","SpeciesId","IndexId")]

# remove <- c("CO_46240", "PKO_51094", "SX_45495") # Temporary fix for populations with duplicates (from earlier check)
# remove2 <- c("SX_43790", "SX_47590", "SX_49234") # Temporary fix for populations with duplicates caught during transposing

# streams <- filter(stream.list, !(IndexId %in% remove))
# streams <- filter(streams, !(IndexId %in% remove2))

# streams <- stream.list # BSC: no t doing this , creates too many object, makes the script not tractable 

# CHECK: CU indicator ER --------------------------------------------------
# BSC: indicate all the AREAs for each CU
sa.cu.lk <- ddply(
  .data = subset(stream.list, Active = TRUE),
  .variables =   c("SpeciesId", "CU", "CU_NAME"), 
  .fun = summarise, 
  StatArea = paste(sort(unique(StatArea)), collapse=", ")
)

sa.cu.lk$StatArea[1]

# QUESTION: What are we checking here? is not used after.

# --- Part 2: Formatting output data frame with CU info and escapement info --- #

# z <- filter(escapement, !(IndexId %in% remove))
# z <- filter(z, !(IndexId %in% remove2))

# z <- escapement      #  BSC: commented out

# id <- unique(z$IndexId) # BSC: commented out until I see what it is used for.

#z[which(z$Returns==0),]$Returns <- "ZERO"

# zz <- z[,-c(1,10:18)]
# zz <- z[,c(3,5,7)]

# Widen data frame #
# zz <- dcast(melt(z[,c(3,5,7)], id.vars = c("IndexId", "Year")), # BSC Bad practices
# zz <- dcast(melt(z[,c("IndexId","Year","Returns")], 
#                  id.vars = c("IndexId", "Year")),  # wide to long format
#             IndexId + variable ~ Year,
#             value.var = "value",
#             fun.aggregate=sum, # Come back to this, currently won't let me widen data without aggregation function
#             fill = 0.12345) # Filler to make sure can distinguish true zeros

escapement_wide <- dcast(melt(escapement[,c("IndexId","Year","Returns")], 
                              id.vars = c("IndexId", "Year")),  # wide to long format
                         IndexId + variable ~ Year,
                         value.var = "value",
                         fun.aggregate=sum, # Come back to this, currently won't let me widen data without aggregation function
                         fill = 0.12345) # Filler to make sure can distinguish true zeros

duplicates <- which(escapement_wide[,3:ncol(escapement_wide)][1] > 1) # BSC: ?!
escapement_wide$IndexId[duplicated(escapement_wide$IndexId)] # BSC
# zz <- zz[,-2] # remove column variable, bad practice
escapement_wide <- escapement_wide[,colnames(escapement_wide) != "variable"]

# Putting together extra info for final data frame #
names(escapement_wide)
names(stream.list)
names(sa.cu.lk) # BSC: not used

# Merging dataframes - THIS IS WHEN TO REMOVE BINNED CUs #
escapement_streams <- merge(stream.list, 
                            escapement_wide, 
                            by = "IndexId")
# data <- merge(stream.list, escapement_wide, by = "IndexId")

# Updating "SpeciesId" for sockeye populations with river/lake specification #
# i.e., "SX" --> "SEL" or "SER"
escapement_streams$SpeciesId <- as.character(escapement_streams$SpeciesId)

escapement_streams$SpeciesId <- ifelse(!is.na(escapement_streams$SPECIES_QUALIFIED) &
                                      escapement_streams$SPECIES_QUALIFIED == "SEL", 
                                    "SEL", escapement_streams$SpeciesId)

escapement_streams$SpeciesId <- ifelse(!is.na(escapement_streams$SPECIES_QUALIFIED) & 
                                      escapement_streams$SPECIES_QUALIFIED == "SER", 
                                    "SER", escapement_streams$SpeciesId)

# escapement_streams[escapement_streams$SpeciesId == "SX",][,c("SpeciesId","SPECIES_QUALIFIED")]

# Cleaning up data #
# escapement_streams$IndexId <- paste(escapement_streams$SpeciesId, escapement_streams$PopId,sep="_") # BSC: already done above for escapement

# remove all non-digit characters in CU_INDEX
escapement_streams$CU_INDEX <- as.character(gsub("\\D","", escapement_streams$CU_INDEX))

escapement_streams$CU_fname <- paste(escapement_streams$SpeciesId,
                                     escapement_streams$CU_NAME, sep="::")
escapement_streams$CU_facro <- paste(escapement_streams$SpeciesId,
                                     escapement_streams$CU_ACRO, sep="::")
escapement_streams$CU_findex <- escapement_streams$FULL_CU_IN

# Set all zero escapement values to NA #
# BSC: bad practices TODO: correct --> it does not work with the new conservation_unit_system_sites because certain columns are missing
# escapement_streams[,56:157][escapement_streams[,56:157] == 0.12345] <- " "
# escapement_streams[,56:157][is.na(escapement_streams[,56:157])] <- " "

col_dates <- colnames(escapement_streams)[colnames(escapement_streams) %in% as.character(1500:3000)]
escapement_streams[,col_dates][escapement_streams[,col_dates] == 0.12345] <- " "
escapement_streams[,col_dates][is.na(escapement_streams[,col_dates])] <- " "

# Check out CUs that are no longer current # BSC: AND DO WHAT?! these bin, bin1 and bin2 are not used 
noLongerCurrent <- c("bin","Bin","Deleted","VREQ[Bin]","VREQ[Extirpated]","Extirpated")
unique(escapement_streams$CU_TYPE)
bin <- filter(escapement_streams, CU_TYPE %in% noLongerCurrent)
length(unique(bin$CU_findex))

bin1 <- unique(bin$CU_findex)
bin2 <- unique(conservation_unit_system_sites$FULL_CU_IN[conservation_unit_system_sites$CU_TYPE %in% noLongerCurrent])
# bin3 <- unique(bb.cu$CU_findex[bb.cu$CU_type %in% noLongerCurrent]) # BSC: ?!
# setdiff(bin2, bin3)
setdiff(bin1, bin2)

# Put together final data frame #
fields <- c("PopId","SpeciesId","GFE_ID","SYSTEM_SITE","Y_LAT","X_LONGT",
            "FAZ_ACRO","MAZ_ACRO","JAZ_ACRO","CU_fname","CU_facro","CU_findex",
            "CU_NAME","CU_ACRO","CU_INDEX",
            # "ISENH",
            "IS_INDICATOR","nrecs","nins",
            "npres","nnumest","pinsrec","ppres_ins","pest_pres",
            # "OL_GRP_NM", # Group of statistical areas (https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/359596.pdf)
            "StatArea","IndexId","NoEscapement")

# BSC: what are the fields "ISENH" and "OL_GRP_NM"?

fields[! fields %in% colnames(escapement_streams)] # "ISENH"     "OL_GRP_NM"
c("ISENH","OL_GRP_NM")[! c("ISENH","OL_GRP_NM") %in% colnames(all_areas_nuseds)]
c("ISENH","OL_GRP_NM")[! c("ISENH","OL_GRP_NM") %in% colnames(conservation_unit_system_sites)]
c("ISENH","OL_GRP_NM")[! c("ISENH","OL_GRP_NM") %in% colnames(template_df)]

# re-order columns
# escapement_streams <- cbind(escapement_streams, escapement_streams[,56:157])
escapement_streams <- cbind(escapement_streams[,fields], escapement_streams[,col_dates])

# Add extra columns #
length <- length(escapement_streams$IndexId)
extras <- data.frame(ID = rep(NA, length),
                     Indicator = rep(NA, length),
                     Source = rep("NuSEDS", length),
                     popMAP = rep(NA, length),
                     EXPN = rep(NA, length),
                     Area = escapement_streams$StatArea,
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
escapement_streams <- cbind(escapement_streams, extras)

final.fields <- c("ID","PopId","Indicator","SpeciesId","GFE_ID",
                  "SYSTEM_SITE","Source","popMAP","EXPN","Y_LAT",
                  "X_LONGT","FAZ_ACRO","MAZ_ACRO","JAZ_ACRO","CU_fname",
                  "CU_facro","CU_findex","CU_NAME","CU_ACRO","CU_INDEX",
                  "Area",
                  # "ISENH",
                  "IS_INDICATOR","IsInFiltEsc","nrecs","nins",
                  "npres","nnumest","pinsrec","ppres_ins","pest_pres","fr_timing",
                  # "OL_GRP_NM",
                  "Fraser_mnemonic","CD_findex.name","StatArea","Reviewer",
                  "QA","Method","Race","WildCode","WildRigor","IndexId","PopId","SpeciesId2",
                  "NoEscapement")

# All the columns in the right order #
# final <- ZZ[,final.fields]
# final <- cbind(final,escapement_streams[,c(29:130)])
final <- cbind(escapement_streams[,final.fields],escapement_streams[,col_dates])
final.fields[!final.fields %in% colnames(escapement_streams)] # "ISENH"     "OL_GRP_NM"

# Remove populations with no escapement data at all #
final <- filter(final, !(NoEscapement))
final <- final[,colnames(final) != "NoEscapement"]
# remove ???
# final <- final[,-46] # BSC: SpeciesId2 ??? --> NoEscapement (?)
colnames(final)[!colnames(final) %in% colnames(template_df)]

# Rename the columns per reference data frame #
# colnames(final)[1:45] <- colnames(template_df)[1:45] # BSC: better edit the column names as it is more transparent
colnames(final[1:45])
colnames(template_df[1:46])

colnames(final)[colnames(final) %in% c("PopId")] <- "POP_ID"

# ???
unique(final$SpeciesId)
unique(template_df$SPP) 
unique(final$SpeciesId2)
unique(template_df$SpeciesId)

colnames(final)[colnames(final) %in% c("SYSTEM_SITE")] <- "SYS_NM"
tolower(final$SYSTEM_SITE)[tolower(final$SYSTEM_SITE) %in% tolower(template_df$SYS_NM)]
tolower(final$SYSTEM_SITE)[!tolower(final$SYSTEM_SITE) %in% tolower(template_df$SYS_NM)]

colnames(final)[colnames(final) %in% c("Source")] <- "source"

colnames(final)[colnames(final) %in% c("Y_LAT")] <- "yLAT"
colnames(final)[colnames(final) %in% c("X_LONGT")] <- "xLONG"

colnames(final)[colnames(final) %in% c("FAZ_ACRO","MAZ_ACRO","JAZ_ACRO")] <- tolower(colnames(final)[colnames(final) %in% c("FAZ_ACRO","MAZ_ACRO","JAZ_ACRO")])

colnames(final)[colnames(final) %in% c("CU_NAME")] <- "CU_name"
colnames(final)[colnames(final) %in% c("CU_ACRO")] <- "CU_acro"
colnames(final)[colnames(final) %in% c("CU_INDEX")] <- "CU_index"

# ???
unique(template_df$SEP_ENH) # "N" "Y"

colnames(final)[colnames(final) %in% c("IS_INDICATOR")] <- "IsIndicator"

colnames(final)[colnames(final) %in% c("pinsrec")] <- "pins_rec"

colnames(final)[colnames(final) %in% c("IS_INDICATOR")] <- "IsIndicator"

# ???
unique(template_df$OL_GRP_NM)

colnames(final)[colnames(final) %in% c("PopId.1")] <- "PopId"


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

d1<- filter(all_areas_nuseds, WATERBODY=='AIRPORT SIDE CHANNEL')


write.csv(final, "NuSEDS_escapement_data_collated_20230818.csv", row.names=FALSE)

# Notes:
# Remove CU's which are "BIN" at this stage? 










