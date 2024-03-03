

#'******************************************************************************
#' The goal of the script is to import, clean and format NuSEDS data.
#' Script based on Emma Atkinson's previous version (26-Mar-2019).
#' 
#' Files imported (from dropbox):
#' - 
#' 
#' Files produced: 
#' - 
#' 

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
library(viridis)
library(parallel)

#
# Functions ------
source("code/functions.R")

#
# Import datasets -----

#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

#' Import the recruitsperspawner.csv from population-indicators/data_input or 
#' download it from the PSF database
fromDatabase <- F
update_file_csv <- F

#' Import the conservationunits_decoder.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

#' Import streamlocationids to obtain the streamID 
streamlocationids <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[8],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

streamlocationids$sys_nm <- tolower(streamlocationids$sys_nm)
streamlocationids$cu_name_pse <- tolower(streamlocationids$cu_name_pse)

cuids <- unique(streamlocationids$cuid)
streamlocationids$species_name <- NA
for(c in cuids){
  cond <- conservationunits_decoder$cuid == c
  if(sum(cond) > 0){
    streamlocationids$species_name[streamlocationids$cuid == c] <- conservationunits_decoder$species_name[cond]
  }
}

# several cuids in streamlocationids are not in conservationunits_decoder
cuids_focal <- unique(streamlocationids$cuid[!streamlocationids$cuid %in% conservationunits_decoder$cuid])
cuids_focal
streamlocationids[streamlocationids$cuid %in% cuids_focal,]
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1708456276544449?thread_ts=1682606819.442789&cid=CJ5RVHVCG
#' 936 is from the Village Bay CU in VIMI. It's extinct but should still be in the database
#' 755 and 936 were removed based on this PSAC feedback
#' 531 was removed based on Technical Working Group feedback (can't be sockeye there because there is an impassable falls)
#' 416 and 417 have been added to the database.

#' Import the dataframe of the NuSEDS datasets of interest:
NuSEDS_datasets_names <- NuSEDS_datasets_names_fun()

#' ** Import the NuSEDS data (all_areas_nuseds) **

# Import the all_areas_nuseds data:
all_areas_nuseds <- datasets_NuSEDS_fun(name_dataSet = NuSEDS_datasets_names$all_areas_nuseds, 
                                        from_NuSEDS_website = F, 
                                        wd = wd_data_dropbox)

colnames(all_areas_nuseds)
# View(all_areas_nuseds)


#' Remove duplicated rows:
dupli <- all_areas_nuseds %>%
  dplyr::group_by(SPECIES, POP_ID, GFE_ID, ANALYSIS_YR,
                  NATURAL_ADULT_SPAWNERS,NATURAL_JACK_SPAWNERS,        
                  NATURAL_SPAWNERS_TOTAL, ADULT_BROODSTOCK_REMOVALS, 
                  JACK_BROODSTOCK_REMOVALS, TOTAL_BROODSTOCK_REMOVALS,    
                  OTHER_REMOVALS, TOTAL_RETURN_TO_RIVER) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

dupli

toRemove_r <- which(duplicated(all_areas_nuseds[,c("SPECIES", "POP_ID", "GFE_ID", "ANALYSIS_YR",
                                     "NATURAL_ADULT_SPAWNERS","NATURAL_JACK_SPAWNERS",        
                                     "NATURAL_SPAWNERS_TOTAL", "ADULT_BROODSTOCK_REMOVALS", 
                                     "JACK_BROODSTOCK_REMOVALS", "TOTAL_BROODSTOCK_REMOVALS",    
                                     "OTHER_REMOVALS", "TOTAL_RETURN_TO_RIVER")]))

all_areas_nuseds <- all_areas_nuseds[-toRemove_r,]


#' ** Import the NuSEDS list of CUs (conservation_unit_system_sites): **
# DFO provided files matching streams and Nuseds to full CU index 
conservation_unit_system_sites <- datasets_NuSEDS_fun(name_dataSet = NuSEDS_datasets_names$conservation_unit_system_sites, 
                                                      from_NuSEDS_website = F, 
                                                      wd = wd_data_dropbox)

#' check for duplicated rows: --> NONE
dupli <- conservation_unit_system_sites %>%
  dplyr::group_by(SPECIES_QUALIFIED,POP_ID,GFE_ID) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

dupli


#' ** Import the definition of the different fields of these two datasets **
fields_def <- nuseds_fields_definitions_fun(wd_references = wd_references_dropbox)
fields_def$all_areas_nuseds$AREA
fields_def$cu_system_sites$`Waterbody Name`

#' ** Import  list for the fields in NUSEDS and CUSS that are associated to unique IndexId and GFE_ID **
fields_l <- fields_IndexId_GFE_ID_fun(all_areas_nuseds = all_areas_nuseds,
                                      conservation_unit_system_sites = conservation_unit_system_sites)

#' ** Import PSF list of CUs **
#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = F,
                                                   update_file_csv = F,
                                                   wd = wd_pop_indic_data_input_dropbox)

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

# Add or edit certain fields in all_areas_nuseds & conservation_unit_system_sites -----

# rename the year column
colnames(all_areas_nuseds)[colnames(all_areas_nuseds) == "ANALYSIS_YR"] <- "Year"

# add the field SPECIES to conservation_unit_system_sites
conservation_unit_system_sites$SPECIES <- NA
species <- c("Coho","Chinook","Pink","Pink","Chum","Sockeye","Sockeye")
sp_acronym_q <- c("CO","CK","PKE","PKO","CM","SEL","SER")
for(spq in sp_acronym_q){
  # spq <- sp_acronym_q[1]
  condition <- conservation_unit_system_sites$SPECIES_QUALIFIED == spq
  sp_Here <- unique(species[spq == sp_acronym_q])
  conservation_unit_system_sites$SPECIES[condition] <- sp_Here
}
unique(conservation_unit_system_sites$SPECIES)

#' Create the field "species_acronym_ncc" (previously "speciesId")
#' i.e., CN, SX instead of CK and SER or SEL in the SPECIES_QUALIFIED.
#' There is no information about the spawning habitat in all_areas_nuseds, 
#' contrary to in conservation_unit_system_sites. We consequently have to create
#' the field species_acronym_ncc in both dataset to be able to merge them after.
#' (Note that there is information
#' about the rearing locations the fieldsWATERBODY, GAZETTED_NAME, LOCAL_NAME_1,
#' LOCAL_NAME_2, POPULATION).
all_areas_nuseds$species_acronym_ncc <- conservation_unit_system_sites$species_acronym_ncc <- NA
species <- c("Coho","Chinook","Pink","Chum","Sockeye")
species_acronym_ncc <- c("CO","CN","PK","CM","SX")
for(sp in species){
  # sp <- species[3]
  sp_acroHere <- species_acronym_ncc[sp == species]
  condition <- all_areas_nuseds$SPECIES == sp
  all_areas_nuseds$species_acronym_ncc[condition] <- sp_acroHere
  condition <- conservation_unit_system_sites$SPECIES == sp
  conservation_unit_system_sites$species_acronym_ncc[condition] <- sp_acroHere
  
  # For Pink, add E and O for Even and Odd, respectively
  if(sp == "Pink"){
    all_areas_nuseds$species_acronym_ncc[all_areas_nuseds$SPECIES == sp & 
                                           all_areas_nuseds$Year %% 2 == 0] <- "PKE"
    all_areas_nuseds$species_acronym_ncc[all_areas_nuseds$SPECIES == sp & 
                                           all_areas_nuseds$Year %% 2 != 0] <- "PKO"
    
    condition <- conservation_unit_system_sites$SPECIES_QUALIFIED == "PKE"
    conservation_unit_system_sites$species_acronym_ncc[condition] <- "PKE"
    
    condition <- conservation_unit_system_sites$SPECIES_QUALIFIED == "PKO"
    conservation_unit_system_sites$species_acronym_ncc[condition] <- "PKO"
  }
}
unique(all_areas_nuseds$species_acronym_ncc)
unique(conservation_unit_system_sites$species_acronym_ncc)

#' Add the field IndexId = species_acronym_ncc + POP_ID
all_areas_nuseds$IndexId <- paste(all_areas_nuseds$species_acronym_ncc,
                                  all_areas_nuseds$POP_ID,sep="_")

conservation_unit_system_sites$IndexId <- paste(conservation_unit_system_sites$species_acronym_ncc,
                                                conservation_unit_system_sites$POP_ID,sep="_")

#'* Add the field StatArea to all_areas_nuseds_all * 
#' What's the goal?

all_areas_nuseds$StatArea <- Convert2StatArea(all_areas_nuseds$AREA)

# Make corrections for populations with discrepancies in area assignments #
# These errors become apparent when merging data frames later on #
all_areas_nuseds[all_areas_nuseds$IndexId == "CO_46240",]$StatArea <- "29"    # vs. "29J" "29K"
all_areas_nuseds[all_areas_nuseds$IndexId == "PKO_51094",]$StatArea <- "12"  # BSC: there is one ""
all_areas_nuseds[all_areas_nuseds$IndexId == "SX_45495",]$StatArea <- "120"  # BSC: already "120"

#'* Determine "Returns" (i.e. number fish) in all_areas_nuseds (NOT USED - TO REMOVED?) *
#' "Return" will be the column that contains the final fish count. Priority of the
#' fields to population Returns:
#'1) NATURAL_ADULT_SPAWNERS, if not available:
#'2) sum of 
#'  - NATURAL_SPAWNERS_TOTAL
#'  - ADULT_BROODSTOCK_REMOVALS (or TOTAL_BROODSTOCK_REMOVALS if not available)
#'  - OTHER_REMOVALS
#'3) TOTAL_RETURN_TO_RIVER

# Add "Return" which will contain the final number of fish
all_areas_nuseds$Returns <- all_areas_nuseds$NATURAL_ADULT_SPAWNERS         # All salmon that have reached maturity, excluding jacks (jacks are salmon that have matured at an early age).
all_areas_nuseds$Source <- "NATURAL_ADULT_SPAWNERS"                        # this field will change below depending on data availability and origin
NATURAL_ADULT_SPAWNERS_any <- !is.na(all_areas_nuseds$NATURAL_ADULT_SPAWNERS)
all_areas_nuseds$Source[!NATURAL_ADULT_SPAWNERS_any] <- NA

#' Define the sum of:
#' - "Spawner" = NATURAL_SPAWNERS_TOTAL +
#' - "Broodstock" = ADULT_BROODSTOCK_REMOVALS (or TOTAL_BROODSTOCK_REMOVALS if not available) + 
#' - "Removals" = OTHER_REMOVALS

# Spawners:
all_areas_nuseds$Spawners <- all_areas_nuseds$NATURAL_SPAWNERS_TOTAL
spawners_any <- !is.na(all_areas_nuseds$Spawners)
all_areas_nuseds$SpawnersSource[!NATURAL_ADULT_SPAWNERS_any & spawners_any] <- "NATURAL_SPAWNERS_TOTAL"

# Broodstock:
all_areas_nuseds$Broodstock <- all_areas_nuseds$ADULT_BROODSTOCK_REMOVALS
ADULT_BROODSTOCK_REMOVALS_any <- !is.na(all_areas_nuseds$ADULT_BROODSTOCK_REMOVALS)
TOTAL_BROODSTOCK_REMOVALS_any <- !is.na(all_areas_nuseds$TOTAL_BROODSTOCK_REMOVALS)
toReplace <- !ADULT_BROODSTOCK_REMOVALS_any & TOTAL_BROODSTOCK_REMOVALS_any
all_areas_nuseds$Broodstock[toReplace] <- all_areas_nuseds$TOTAL_BROODSTOCK_REMOVALS[toReplace]
all_areas_nuseds$BroodstockSource[!NATURAL_ADULT_SPAWNERS_any & ADULT_BROODSTOCK_REMOVALS_any] <- 'ADULT_BROODSTOCK_REMOVALS'
all_areas_nuseds$BroodstockSource[toReplace] <- 'TOTAL_BROODSTOCK_REMOVALS'

# Removals:
all_areas_nuseds$Removals <- all_areas_nuseds$OTHER_REMOVALS
OTHER_REMOVALS_any <- !is.na(all_areas_nuseds$OTHER_REMOVALS)
all_areas_nuseds$RemovalsSource[!NATURAL_ADULT_SPAWNERS_any & OTHER_REMOVALS_any] <- "OTHER_REMOVALS"

# Calculate Returns when !NATURAL_ADULT_SPAWNERS_any as the sum of what other 
# sources of data is available:
all_areas_nuseds$Returns[!NATURAL_ADULT_SPAWNERS_any] <- apply(
  X = all_areas_nuseds[!NATURAL_ADULT_SPAWNERS_any, c("Spawners","Broodstock","Removals")],
  MARGIN = 1, 
  FUN = sum, 
  na.rm = TRUE
)

all_areas_nuseds$Source[!NATURAL_ADULT_SPAWNERS_any] <- apply(
  X = all_areas_nuseds[!NATURAL_ADULT_SPAWNERS_any,c("SpawnersSource","BroodstockSource","RemovalsSource")], 
  MARGIN = 1, 
  FUN = function(x){
    paste(na.omit(x),collapse=" + ")}
)

# Set as NA rather than zero if no info in any column (the na.rm = T above produced)
# 0s when only NAs were available).
allNAs <- apply(
  X = all_areas_nuseds[!NATURAL_ADULT_SPAWNERS_any, c("Spawners","Broodstock","Removals")],
  MARGIN = 1, 
  FUN = function(x){all(is.na(x))}
) 
all_areas_nuseds$Returns[!NATURAL_ADULT_SPAWNERS_any][allNAs] <- NA

# Use TOTAL_RETURN_TO_RIVER if we still don't have a value
returns_any <- !is.na(all_areas_nuseds$Returns)
TOTAL_RETURN_TO_RIVER_any <- !is.na(all_areas_nuseds$TOTAL_RETURN_TO_RIVER)
toReplace <- !returns_any & TOTAL_RETURN_TO_RIVER_any
all_areas_nuseds$Returns[toReplace] <- all_areas_nuseds$TOTAL_RETURN_TO_RIVER[toReplace]
all_areas_nuseds$Source[toReplace] <- "TOTAL_RETURN_TO_RIVER"

#
#'* Determine "MAX_ESTIMATE" in all_areas_nuseds *

#' MAX_ESTIMATE is the maximum estimate of all these fields:
var_in_MAX_ESTIMATE <- c("NATURAL_ADULT_SPAWNERS", 
                         "NATURAL_JACK_SPAWNERS",        # Same question below
                         "NATURAL_SPAWNERS_TOTAL", 
                         "ADULT_BROODSTOCK_REMOVALS", 
                         "JACK_BROODSTOCK_REMOVALS",     # QUESTION: why considered here and not above?
                         "TOTAL_BROODSTOCK_REMOVALS",    
                         "OTHER_REMOVALS", 
                         "TOTAL_RETURN_TO_RIVER")

all_areas_nuseds$MAX_ESTIMATE <- apply(all_areas_nuseds[,var_in_MAX_ESTIMATE], 1,
                                       max, na.rm = TRUE)

all_areas_nuseds$MAX_ESTIMATE[is.infinite(all_areas_nuseds$MAX_ESTIMATE)] <- NA

# View(all_areas_nuseds[is.na(all_areas_nuseds$Returns) & !is.na(all_areas_nuseds$JACK_BROODSTOCK_REMOVALS),])

# plot(log(all_areas_nuseds$Returns) ~ log(all_areas_nuseds$MAX_ESTIMATE))
# abline(a = 0, b = 1)

#'* Get the GFE_IDs in NUSEDS that are not in CUSS *
#' Several series in NUSEDS not in CUSS will be added to CUSS, for some of the 
#' the GFE_ID is not in CUSS and so the coordinate are not available.
#' --> use the GFE_ID_nuseds_notCuss_df to specify the GFE_IDs for which must find
#' coordinates.

# GFE_IDs in NUSEDS not in CUSS:
GFE_ID_nuseds <- unique(all_areas_nuseds$GFE_ID)
GFE_ID_cuss <- unique(conservation_unit_system_sites$GFE_ID)
GFE_ID_nuseds_notCuss <- GFE_ID_nuseds[!GFE_ID_nuseds %in% GFE_ID_cuss]
length(GFE_ID_nuseds_notCuss)
waterbody_nuseds_notCuss <- sapply(X = GFE_ID_nuseds_notCuss, 
                                   FUN = function(gfeid){
                                     cond <- all_areas_nuseds$GFE_ID == gfeid
                                     return(unique(all_areas_nuseds$WATERBODY[cond]))
                                   })

GFE_ID_nuseds_notCuss_df <- data.frame(GFE_ID = GFE_ID_nuseds_notCuss,
                                    WATERBODY = waterbody_nuseds_notCuss,
                                    need_coordinates = "no")

#
# CHECKS on all_areas_nuseds and conservation_unit_system_sites ---------

#' * 1) Remove the IndexId & GFE_ID time series with only NAs and/or 0s*
all_areas_nuseds_all <- all_areas_nuseds
nrow(all_areas_nuseds_all) # 412493

conservation_unit_system_sites_all <- conservation_unit_system_sites
nrow(conservation_unit_system_sites_all) # 7145

detectCores()
detectCores(logical = FALSE)
cores_nb <- 10
all_areas_nuseds <- remove_series_nodata_nuseds_parallel_fun(all_areas_nuseds = all_areas_nuseds,
                                                             zeros_too = T, 
                                                             cores_nb = cores_nb)
nrow(all_areas_nuseds) # 309648
nrow(all_areas_nuseds)/nrow(all_areas_nuseds_all) # .75

# Record the series that were removed and why:
IndexId_GFE_ID_all <- unique(all_areas_nuseds_all[,c("IndexId","GFE_ID")])
nrow(IndexId_GFE_ID_all) # 11553

IndexId_GFE_ID <- unique(all_areas_nuseds[,c("IndexId","GFE_ID")])
nrow(IndexId_GFE_ID) # 7062

IndexId_GFE_ID_all <- paste(IndexId_GFE_ID_all$IndexId,IndexId_GFE_ID_all$GFE_ID,sep = "&")
IndexId_GFE_ID <- paste(IndexId_GFE_ID$IndexId,IndexId_GFE_ID$GFE_ID,sep = "&")

IndexId_GFE_ID_removed_c <- IndexId_GFE_ID_all[! IndexId_GFE_ID_all %in% IndexId_GFE_ID]
length(IndexId_GFE_ID_removed_c) # 4491
IndexId_GFE_ID_removed <- data.frame(IndexId = rep(NA,length(IndexId_GFE_ID_removed_c)),
                                     GFE_ID = rep(NA,length(IndexId_GFE_ID_removed_c)))

IndexId_GFE_ID_removed$IndexId <- sapply(X = IndexId_GFE_ID_removed_c, FUN = function(s){
  # s <- IndexId_GFE_ID_removed[1]
  return(strsplit(x = s, split = "&")[[1]][1])
  })

IndexId_GFE_ID_removed$GFE_ID <- sapply(X = IndexId_GFE_ID_removed_c, FUN = function(s){
  # s <- IndexId_GFE_ID_removed[1]
  return(strsplit(x = s, split = "&")[[1]][2])
})

removed_all <- IndexId_GFE_ID_removed
removed_all$dataset <- "all_areas_nuseds"
removed_all$comment <- "Only NAs and/or 0s for MAX_ESTIMATE"

head(removed_all)

# check
i <- 3243
plot_IndexId_GFE_ID_fun(IndexIds = removed_all$IndexId[i],
                        GFE_IDs = removed_all$GFE_ID[i],
                        all_areas_nuseds = all_areas_nuseds_all)

#'* ) Remove these series from conservation_units_system_sites *

#' We should not remove series in CUSS at this stage that have only NAs and/or 0s
#' in NUSEDS because these series could be alternative series for the series 
#' in NUSEDS that do not appear in CUSS.
#' It is only once series in NUSEDS absent in CUSS were found alternative for that 
#' one should remove series in CUSS that have only NAs and/or 0s in NUSEDS.


#' * 2) Check all the IndexId - GFE_ID series in NUSEDS but not in CUSS  *
#' Look for each IndexId & GFE_ID in conservation_unit_system_sites:
#' - 1) check if there are multiple GFE_IDs associated
#'      if yes: trouble shoot manually;
#' - 2) else look if there is a time series with the iid & its GFE_ID ('gfeid') in 
#'      all_areas_nuseds;
#'      if yes: all good;
#' - 3) esle: that could be due to either (i) a typo in the IndexId or
#'      (ii) a typo in the GFE_ID. The the rest of the code looks for potential
#'      alternative series in NUSEDS with either a different IndexId (but with 
#'      the same species) or a different GFE_ID. Alternative series identified 
#'      NOT present in CUSS are kept, the ones present are removed.
#' The function returns a simple dataframe with the IndexId and GFE_ID concerned
#' and associated comment and eventual potential alternative series that have to 
#' be checked manually after.

# graphics.off()

colNuSEDS <- c("SPECIES","IndexId","GFE_ID","WATERBODY","Year","MAX_ESTIMATE")

detectCores()
detectCores(logical = FALSE)
cores_nb <- 10
trackRecord <- cuss_nuseds_match_parallel_fun(conservation_unit_system_sites = conservation_unit_system_sites,
                                              all_areas_nuseds = all_areas_nuseds, 
                                              cores_nb = cores_nb)
head(trackRecord)
nrow(trackRecord) # 7145 same as CUSS

unique(trackRecord$comment)

comment <- "In CUSS: there are multiple GFE_IDs for"
comment <- "Alternative series:"
comment <- "There is no alternative series in all_areas_nuseds"
trackRecord[grepl(comment,trackRecord$comment),]

#'** 2.1) Manual fix: Alternative series **
#'Go over all these cases and decide on the fixes.
comment <- "Alternative series:"
trackRecord_cuss_alternative <- trackRecord[grepl(comment,trackRecord$comment),]
nrow(trackRecord_cuss_alternative) # 4
trackRecord_cuss_alternative

# ***
r <- 1
d <- trackRecord_cuss_alternative[r,,drop = F]
series_alternative <- d$comment
series_alternative <- gsub("Alternative series: ","",series_alternative)
series_alternative <- strsplit(series_alternative, split = ", ")[[1]]
series_alternative <- strsplit(series_alternative," & ")

iids <- sapply(X = series_alternative, function(c){c[1]})
gfeids <- sapply(X = series_alternative, function(c){c[2]})

d <- data.frame(IndexId = iids,
                GFE_ID = gfeids)

main <- paste0("Alternative series for ",
               trackRecord_cuss_alternative$IndexId[r],
               " & ",
               trackRecord_cuss_alternative$GFE_ID[r])
plot_IndexId_GFE_ID_fun(IndexIds = d$IndexId, 
                        GFE_IDs = d$GFE_ID, 
                        all_areas_nuseds = all_areas_nuseds_all, 
                        main = main)

# These series have different Run numbers:
cond <- all_areas_nuseds$IndexId %in% c("CN_46842","CN_46841")
unique(all_areas_nuseds$POPULATION[cond])

cond <- all_areas_nuseds$POPULATION == "Chilcotin River (Williams Lake Area) Chinook Run 2"
unique(all_areas_nuseds$IndexId[cond])

#' TODO: replace CN_46842 & GFE_ID = 2463 and by CN_46841 & GFE_ID = 285 by 
#' CN_46842 & GFE_ID = 285 in all_area_nuseds.

#' conversation about merging them:
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1707929645199719?thread_ts=1707771319.134789&cid=CJ5RVHVCG 
removed <- data.frame(IndexId = c("CN_46842","CN_46841"),
                      GFE_ID = c(2463,285))
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Alternative series for CN_46842 & GFE_ID = 285"
removed_all <- rbind(removed_all,removed)

#' CN_46842 & GFE_ID = 2463 --> CN_46842 & GFE_ID = 285
#' --> change of GFE_ID:
all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "CN_46842",
                                                IndexId_alter = "CN_46842",
                                                GFE_ID_focal = 2463,
                                                GFE_ID_alter = 285,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)


#' CN_46841 & GFE_ID = 285 --> CN_46842 & GFE_ID = 285
#' --> change IndexId
all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "CN_46841",
                                                IndexId_alter = "CN_46842",
                                                GFE_ID_focal = 285,
                                                GFE_ID_alter = 285,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

# ***
r <- 2
d <- trackRecord_cuss_alternative[r,,drop = F]
series_alternative <- d$comment
series_alternative <- gsub("Alternative series: ","",series_alternative)
series_alternative <- strsplit(series_alternative, split = ", ")[[1]]
series_alternative <- strsplit(series_alternative," & ")

iids <- sapply(X = series_alternative, function(c){c[1]})
gfeids <- sapply(X = series_alternative, function(c){c[2]})

d <- data.frame(IndexId = iids,
                GFE_ID = gfeids)

main <- paste0("Alternative series for ",
               trackRecord_cuss_alternative$IndexId[r],
               " & ",
               trackRecord_cuss_alternative$GFE_ID[r])
plot_IndexId_GFE_ID_fun(IndexIds = d$IndexId, 
                        GFE_IDs = d$GFE_ID, 
                        all_areas_nuseds = all_areas_nuseds_all, 
                        main = main)

#' TODO: we remove PKE_42409 & GFE_ID = 1490 from conservation_unit_system_sites
#' and PKE_54793433 & GFE_ID = 1490 from all_area_nuseds.
removed <- data.frame(IndexId = c("PKE_42409","PKE_54793433"),
                      GFE_ID = c(1490,1490))
removed$dataset <- c("conservation_unit_system_sites","all_areas_nuseds")
removed$comment <- c("Removed because not in all_area_nuseds and alternative series PKE_54793433 & GFE_ID = 1490 ahs only one data point as was removed",
                     "Only 1 data point(s) and not in conservation_unit_system_sites")

removed_all <- rbind(removed_all,removed)

condition <- conservation_unit_system_sites$IndexId == removed$IndexId[1] &
  conservation_unit_system_sites$GFE_ID == removed$GFE_ID[1]
conservation_unit_system_sites[condition,]
conservation_unit_system_sites <- conservation_unit_system_sites[!condition,]
nrow(conservation_unit_system_sites) # 7144

condition <- all_areas_nuseds$IndexId == removed$IndexId[2] &
  all_areas_nuseds$GFE_ID == removed$GFE_ID[2]
all_areas_nuseds[condition,colNuSEDS]
all_areas_nuseds <- all_areas_nuseds[!condition,]
nrow(all_areas_nuseds) # 309638

# ***
r <- 3
d <- trackRecord_cuss_alternative[r,,drop = F]
series_alternative <- d$comment
series_alternative <- gsub("Alternative series: ","",series_alternative)
series_alternative <- strsplit(series_alternative, split = ", ")[[1]]
series_alternative <- strsplit(series_alternative," & ")

iids <- sapply(X = series_alternative, function(c){c[1]})
gfeids <- sapply(X = series_alternative, function(c){c[2]})

d <- data.frame(IndexId = iids,
                GFE_ID = gfeids)

main <- paste0("Alternative series for ",
               trackRecord_cuss_alternative$IndexId[r],
               " & ",
               trackRecord_cuss_alternative$GFE_ID[r])
plot_IndexId_GFE_ID_fun(IndexIds = d$IndexId, 
                        GFE_IDs = d$GFE_ID, 
                        all_areas_nuseds = all_areas_nuseds, 
                        main = main)

# These GFE_ID correspond to the same river:
cond <- conservation_unit_system_sites$GFE_ID %in% c(2463,285)
unique(conservation_unit_system_sites$SYSTEM_SITE[cond])
# "CHILCOTIN RIVER - LOWER" "CHILCOTIN RIVER"         "CHILCOTIN RIVER"

#' TODO: replace CO_46835 & 2463 by CO_46835 & 285 in all_area_nuseds.
#' --> change GFE_ID:

removed <- data.frame(IndexId = c("CO_46835"),
                      GFE_ID = 2463)
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Alternative series for CO_46835 & GFE_ID = 285"
removed_all <- rbind(removed_all,removed)

all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "CO_46835",
                                                IndexId_alter = "CO_46835",
                                                GFE_ID_focal = 2463,
                                                GFE_ID_alter = 285,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

# ***
r <- 4
d <- trackRecord_cuss_alternative[r,,drop = F]
series_alternative <- d$comment
series_alternative <- gsub("Alternative series: ","",series_alternative)
series_alternative <- strsplit(series_alternative, split = ", ")[[1]]
series_alternative <- strsplit(series_alternative," & ")

iids <- sapply(X = series_alternative, function(c){c[1]})
gfeids <- sapply(X = series_alternative, function(c){c[2]})

d <- data.frame(IndexId = iids,
                GFE_ID = gfeids)

main <- paste0("Alternative series for ",
               trackRecord_cuss_alternative$IndexId[r],
               " & ",
               trackRecord_cuss_alternative$GFE_ID[r])
plot_IndexId_GFE_ID_fun(IndexIds = d$IndexId, 
                        GFE_IDs = d$GFE_ID, 
                        all_areas_nuseds = all_areas_nuseds, 
                        main = main)

#' these series have the same CU_NAME:
cond <- conservation_unit_system_sites$IndexId %in% c("SX_7763","SX_47954")
conservation_unit_system_sites$CU_NAME[cond]
# "WIDGEON" "WIDGEON"

#' TODO: replace SX_47954 & 21 by SX_7763 & 21 in all_area_nuseds.
#' --> change IndexId
removed <- data.frame(IndexId = c("SX_47954"),
                      GFE_ID = 21)
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Alternative series for SX_7763 & GFE_ID = 21"
removed_all <- rbind(removed_all,removed)

all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "SX_47954",
                                                IndexId_alter = "SX_7763",
                                                GFE_ID_focal = 21,
                                                GFE_ID_alter = 21,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

#
#'** 2.2) Manual fix: no alternative series in nuseds **
comment <- "There is no alternative series in all_areas_nuseds"
trackRecord_cuss_noAlernative <- trackRecord[grepl(comment,trackRecord$comment),]
trackRecord_cuss_noAlernative
nrow(trackRecord_cuss_noAlernative) # 254
#' TODO: remove them from cuss and check if they were in all_areas_nuseds_all. If
#' yes comment = "Only NAs and/or 0s in nuseds", else "time series not present in
#' nuseds a and not alternative.
for(r in 1:nrow(trackRecord_cuss_noAlernative)){
  # r <- 1
  iid <- trackRecord_cuss_noAlernative$IndexId[r]
  gfeid <- trackRecord_cuss_noAlernative$GFE_ID[r]
  
  # check if the series already exists in removed_all
  cond <- removed_all$IndexId == iid & removed_all$GFE_ID == gfeid
  if(nrow(removed_all[cond,]) > 0){ # edit removed_all
    removed_all$dataset[cond] <- "both"
    print(paste("Series",iid,"-",gfeid,"was in nuseds with only NAs"))
    
  }else{ # add row to removed_all
    removed <- data.frame(IndexId = iid,
                          GFE_ID = gfeid,
                          dataset = "conservation_unit_system_sites")
    removed$comment <- "Not in all_area_nuseds and no alternative series"
    removed_all <- rbind(removed_all,removed)
    print(paste("Series",iid,"-",gfeid,"was not nuseds"))
  }
  
  # remove series from conservation_unit_system_sites
  cond <- conservation_unit_system_sites$IndexId == iid &
    conservation_unit_system_sites$GFE_ID == gfeid
  conservation_unit_system_sites <- conservation_unit_system_sites[!cond,]
}

nrow(conservation_unit_system_sites) # 6890

# all but one of the series are in all_areas_nuseds_all
cond <- removed_all$dataset == "both"
sum(cond) # 253 (vs. 254)

# the only series in CUSS not in all_areas_nuseds_all
cond <- grepl("Not in all_area_nuseds and no alternative series",removed_all$comment)
removed_all[cond,]
# IndexId GFE_ID                        dataset                                           comment
# SX_2167    150 conservation_unit_system_sites Not in all_area_nuseds and not alternative series

cond <- conservation_unit_system_sites_all$IndexId == "SX_2167"
conservation_unit_system_sites_all$CU_NAME[cond] # "UPPER FRASER"

#
#'** 2.3) Manual fix: there are multiple GFE_IDs for a population in cuss **
comment <- "In CUSS: there are multiple GFE_IDs for"
trackRecord_cuss_multi_fgeid <- trackRecord[grepl(comment,trackRecord$comment),]
trackRecord_cuss_multi_fgeid
# IndexIds: 
unique(trackRecord_cuss_multi_fgeid$IndexId)
# "CN_7479"  "SX_45525"

#' Case with CN_7479:
#'    -  series CN_7479 & GF_ID = 133 is not in all_areas_nuseds_all 
#'    (nor in all_area_nuseds) as shown by its absence in the figure below.
#'    TODO: remove CN_7479 & GF_ID = 133 from CUSS
iid <- unique(trackRecord_cuss_multi_fgeid$IndexId)[1]
gfeids <- trackRecord_cuss_multi_fgeid$GFE_ID[1:2]
plot_IndexId_GFE_ID_fun(IndexIds = iid, 
                        all_areas_nuseds = all_areas_nuseds_all)

cond <- conservation_unit_system_sites$IndexId == "CN_7479" & 
  conservation_unit_system_sites$GFE_ID == 133

conservation_unit_system_sites <- conservation_unit_system_sites[!cond,]
nrow(conservation_unit_system_sites) # 6889

removed <- data.frame(IndexId = "CN_7479",
                      GFE_ID = 133,
                      dataset = "conservation_unit_system_sites")
removed$comment <- "Not in all_area_nuseds and no alternative series"
removed_all <- rbind(removed_all,removed)


#' Case with SX_45525:
#' - there is no "Nadina channel" in streamlocationids (the red series) and seems
#' that the abundances were summed in the PSE.
#' TODO: create a new row in streamlocationids for the red series.
iid <- unique(trackRecord_cuss_multi_fgeid$IndexId)[2]
gfeids <- trackRecord_cuss_multi_fgeid$GFE_ID[trackRecord_cuss_multi_fgeid$IndexId == iid]
plot_IndexId_GFE_ID_fun(IndexIds = iid, 
                        all_areas_nuseds = all_areas_nuseds_all)

POP_WB <- lapply(X = c(2444,303), FUN = function(x){
  cond <- all_areas_nuseds$IndexId == iid & all_areas_nuseds$GFE_ID == x
  pop <- unique(all_areas_nuseds$POPULATION[cond])
  wb <-  unique(all_areas_nuseds$WATERBODY[cond])
  out <- data.frame(POPULATION = pop,
                    WATERBODY = wb)
  return(out)
})
POP_WB <- do.call(rbind,POP_WB)
legend("top",c("WATERBODY:",POP_WB$WATERBODY),bty = 'n')
#legend("topright",c("POPULATION:",GFE_IDs),bty = 'n')

cond <- grepl("[N|n]adina",streamlocationids$sys_nm) & grepl("[S|s]ockeye",streamlocationids$species_name)
streamlocationids[cond,]

# they have the sane CU_NAME
sapply(X = c(2444,303),FUN = function(x){
  cond <- conservation_unit_system_sites$IndexId == iid &
    conservation_unit_system_sites$GFE_ID == x
  out <- conservation_unit_system_sites$CU_NAME[cond]
  return(out)
})

# check coordinates --> there are different
sapply(X = c(2444,303),FUN = function(x){
  cond <- conservation_unit_system_sites$IndexId == iid &
    conservation_unit_system_sites$GFE_ID == x
  out <- conservation_unit_system_sites[cond,c("Y_LAT","X_LONGT")]
  return(out)
})

# check fpop size for the blue (to see if correspond to values in the PSE)
cond <- all_areas_nuseds$IndexId == "SX_45525" & all_areas_nuseds$GFE_ID == 303
all_areas_nuseds[cond,c("Year","MAX_ESTIMATE")][order(all_areas_nuseds[cond,c("Year")]),]

toAdd <- data.frame(IndexId = "SX_45525",
                    GFE_ID = 2444,
                    dataset = "streamlocationids",
                    CU_NAME = "NADINA/FRANCOIS-EARLY SUMMER TIMING",
                    comment = "In both CUSS and NUSEDS but not in the PSE; but series SX_45525 - 303 is.")

toAdd_all <- toAdd

#
#'* 3) Check all the IndexId - GFE_ID series in NUSEDS but not in CUSS *

series_nuseds <- paste(all_areas_nuseds$IndexId,
                       all_areas_nuseds$GFE_ID,sep = "&")
series_nuseds <- unique(series_nuseds)
length(series_nuseds) # 7060

series_cuss <- paste(conservation_unit_system_sites$IndexId,
                     conservation_unit_system_sites$GFE_ID,sep = "&")
series_cuss <- unique(series_cuss)
length(series_cuss) # 6889

#' Now all the series in CUSS are in all_areas_nuseds.
series_cuss[! series_cuss %in% series_nuseds]

# Number of series in NUSEDS not in CUSS:
series_Nuseds_noCuss <- series_nuseds[! series_nuseds %in% series_cuss]
series_Nuseds_noCuss <- series_Nuseds_noCuss[order(series_Nuseds_noCuss)]
length(series_Nuseds_noCuss) # 171

#' 
iids <- sapply(X = series_Nuseds_noCuss,FUN = function(s){
  return(strsplit(s,"&")[[1]][1])
})

gfeids <- sapply(X = series_Nuseds_noCuss,FUN = function(s){
  return(strsplit(s,"&")[[1]][2])
})

trackRecord_nuseds <- data.frame(IndexId = iids,
                                 GFE_ID = gfeids)
trackRecord_nuseds$nb_dataPt <- NA
trackRecord_nuseds$alternative_IndexId <- NA
trackRecord_nuseds$alternative_GFE_ID <- NA
trackRecord_nuseds$alternative_IndexId_track <- NA
trackRecord_nuseds$alternative_GFE_ID_track <- NA
# trackRecord_nuseds$only_0s <- NA
rownames(trackRecord_nuseds) <- NULL

for(i in 1:nrow(trackRecord_nuseds)){
  # i <- 35
  #' 2. is there a GFE_ID + species in cuss
  iid <- trackRecord_nuseds$IndexId[i]
  speciesAcro <- strsplit(iid,split = "_")[[1]][1]
  gfeid <- trackRecord_nuseds$GFE_ID[i]
  
  # plot_IndexId_GFE_ID_fun(IndexIds = iid,
  #                         all_areas_nuseds = all_areas_nuseds)
  # legend("topright", paste("Series not in CUSS"), bty = 'n')
  
  # Find the number of data points 
  cond <- all_areas_nuseds$IndexId == iid &
    all_areas_nuseds$GFE_ID == gfeid
  nuseds_cut <- all_areas_nuseds[cond,]
  max_esti <- nuseds_cut$MAX_ESTIMATE
  trackRecord_nuseds$nb_dataPt[i] <- sum(!is.na(nuseds_cut$MAX_ESTIMATE))
  # max_esti <- max_esti[!is.na(max_esti)]
  # if(all(max_esti == 0)){
  #   trackRecord_nuseds$only_0s[i] <- T
  # }
  
  #' Check in NUSEDS for alternative series that are in CUSS with the same IndexId
  #' but different GFE_ID.
  cond <- all_areas_nuseds$IndexId == iid & all_areas_nuseds$GFE_ID != gfeid
  nuseds_here <- all_areas_nuseds[cond,]
  
  alternative_GFE_ID <- c()
  alternative_GFE_ID_track <- c()
  if(nrow(nuseds_here) == 0){
    alternative_GFE_ID <- "none"
    
  }else{
    
    # look if the these alternative series are in CUSS:
    gfeids <- unique(nuseds_here$GFE_ID)
    for(gfeid_i in gfeids){
      # gfeid_i <- gfeids[2]
      cond <- conservation_unit_system_sites$IndexId == iid &
        conservation_unit_system_sites$GFE_ID == gfeid_i
      
      # if the alternative series is in CUSS
      if(any(cond)){
        alternative_GFE_ID <- c(alternative_GFE_ID,
                                conservation_unit_system_sites$GFE_ID[cond])
        
      }else{ # in that case the altenative series has to be in trackRecord_nuseds
        cond <- trackRecord_nuseds$IndexId == iid &
          trackRecord_nuseds$GFE_ID == gfeid_i
        
        alternative_GFE_ID_track <- c(alternative_GFE_ID_track,
                                      trackRecord_nuseds$GFE_ID[cond])
      }
    }
    if(length(alternative_GFE_ID) == 0){
      alternative_GFE_ID <- "none"
    }
  }
  
  #' Check in NUSEDS for alternative series that are in CUSS with the same GFE_ID 
  #' and species but different GFE_ID.
  cond <- all_areas_nuseds$species_acronym_ncc == speciesAcro & 
    all_areas_nuseds$GFE_ID == gfeid &
    all_areas_nuseds$IndexId != iid
  nuseds_here <- all_areas_nuseds[cond,]
  
  alternative_IndexId <- c()
  alternative_IndexId_track <- c()
  if(nrow(nuseds_here) == 0){
    alternative_IndexId <- "none"
    
  }else{
    # look if the these alternative series are in CUSS:
    iids <- unique(nuseds_here$IndexId)
    for(iid_i in iids){
      # iid_i <- iids[1]
      cond <- conservation_unit_system_sites$IndexId == iid_i &
        conservation_unit_system_sites$GFE_ID == gfeid
      
      # if the alternative series is in CUSS
      if(any(cond)){
        alternative_IndexId <- c(alternative_IndexId,
                                 conservation_unit_system_sites$IndexId[cond])
        
      }else{ # in that case the altenative series has to be in trackRecord_nuseds
        cond <- trackRecord_nuseds$IndexId == iid_i &
          trackRecord_nuseds$GFE_ID == gfeid
        
        alternative_IndexId_track <- c(alternative_IndexId_track,
                                       trackRecord_nuseds$IndexId[cond])
      }
    }
    if(length(alternative_IndexId) == 0){
      alternative_IndexId <- "none"
    }
  }
  trackRecord_nuseds$alternative_GFE_ID[i] <- paste(alternative_GFE_ID,collapse = " ; ")
  trackRecord_nuseds$alternative_GFE_ID_track[i] <- paste(alternative_GFE_ID_track,collapse = " ; ")
  trackRecord_nuseds$alternative_IndexId[i] <- paste(alternative_IndexId,collapse = " ; ")
  trackRecord_nuseds$alternative_IndexId_track[i] <- paste(alternative_IndexId_track,collapse = " ; ")
  # trackRecord_nuseds[i,]
}

#View(trackRecord_nuseds)
head(trackRecord_nuseds)
nrow(trackRecord_nuseds) # 171

hist(trackRecord_nuseds$nb_dataPt)
nrow(trackRecord_nuseds[trackRecord_nuseds$nb_dataPt > 3,]) # 77

#' ** 3.1) Remove series number of data points <= 3 **
#' It is not worth making a guess to save three data points.
cond_3 <- trackRecord_nuseds$nb_dataPt <= 3
trackRecord_nuseds_3 <- trackRecord_nuseds[cond_3,]
nrow(trackRecord_nuseds_3) # 94

i <- 94
plot_IndexId_GFE_ID_fun(IndexIds = trackRecord_nuseds_3$IndexId[i],
                        GFE_IDs = trackRecord_nuseds_3$GFE_ID[i],
                        all_areas_nuseds = all_areas_nuseds_all)

removed <- trackRecord_nuseds_3[,c("IndexId","GFE_ID")]
removed$dataset <- "all_areas_nuseds"
removed$comment <- paste("Only",trackRecord_nuseds_3$nb_dataPt,
                         "data point(s) and not in conservation_unit_system_sites")

all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                           toRemove = removed, 
                                           fields = c("IndexId","GFE_ID"))
nrow(all_areas_nuseds) # 308251

removed_all <- rbind(removed_all,removed)
nrow(removed_all) # 4593

#
#' ** 3.2) Alternative IndexId AND no alternative GFE_ID **
#' Strategy: the action depends on the % of data points that are:
#' - complementary (i.e. no duplicate no conflict): merge if 100%
#'    - if tie with multiple alternatives: set aside
#' - duplicate (i.e. same value for same year): remove if 100%
#' - conflictual (i.e. same value same year): put aside
cond <- trackRecord_nuseds$alternative_IndexId != "none" &
  trackRecord_nuseds$alternative_GFE_ID == "none" & 
  !cond_3
trackRecord_nuseds_iid <- trackRecord_nuseds[cond,]
nrow(trackRecord_nuseds_iid) # 6
trackRecord_nuseds_iid

removed <- removed_all[F,]

set_aside <- list()
count_set_aside <- 1
for(i in 1:nrow(trackRecord_nuseds_iid)){
  # i <- 5
  iid_focal <- trackRecord_nuseds_iid$IndexId[i]
  iid_alter <- strsplit(trackRecord_nuseds_iid$alternative_IndexId[i]," ; ")[[1]]
  gfeid <- trackRecord_nuseds_iid$GFE_ID[i]
  
  # compare the series
  cond <- all_areas_nuseds$IndexId == iid_focal & 
    all_areas_nuseds$GFE_ID == gfeid
  series_focal <- all_areas_nuseds$MAX_ESTIMATE[cond]
  names(series_focal) <- all_areas_nuseds$Year[cond]
  
  # 
  POPULATION_focal <- unique(all_areas_nuseds$POPULATION[cond])
  POPULATION_alter <- c()
  
  comparison_series <- data.frame(IndexId = iid_alter, 
                                  GFE_ID = gfeid)
  comparison_series$complementary <- NA
  comparison_series$duplicate <- NA
  comparison_series$conflict <- NA
  comparison_series$decision <- NA
  
  for(iid in iid_alter){
    # iid <- iid_alter[1]
    cond <- all_areas_nuseds$IndexId == iid & 
      all_areas_nuseds$GFE_ID == gfeid[1]
    series_comp <- all_areas_nuseds$MAX_ESTIMATE[cond]
    names(series_comp) <- all_areas_nuseds$Year[cond]
    comparison <- compare_series_fun(series_focal,series_comp,percentage = T)
    
    comparison_series$nb_dataPt[which(iid == iid_alter)] <- comparison$nb_dataPt
    comparison_series$complementary[which(iid == iid_alter)] <- comparison$complementary
    comparison_series$duplicate[which(iid == iid_alter)] <- comparison$duplicate
    comparison_series$conflict[which(iid == iid_alter)] <- comparison$conflict
    
    POPULATION_here <- unique(all_areas_nuseds$POPULATION[cond])
    POPULATION_alter <- c(POPULATION_alter,POPULATION_here)
  }
  
  removed_here <- NULL
  #' 1) If the series is 100% duplicated with another: remove
  if(any(comparison_series$duplicate == 100)){
    cond <- comparison_series$duplicate == 100
    iid_replacement <- comparison_series$IndexId[cond]
    gfeid_replacement <- comparison_series$GFE_ID[cond]
    
    removed_here <- removed_all[1,]
    removed_here$IndexId <- iid_focal
    removed_here$GFE_ID <- gfeid
    removed_here$dataset <- "all_areas_nuseds"
    comment <- paste("Duplicate of series IndexId =",iid_replacement,"& GFE_ID =",
                     gfeid_replacement)
    removed_here$comment <- comment
    
    main <- "Removed because duplicated"
    
    #' Do the change in NUSEDS: only change the IndexId are related fields (not 
    #' the GFE_ID ones because here GFE_IDs are the same).
    all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                               toRemove = removed_here, 
                                               fields = c("IndexId","GFE_ID"))
    
    #' 2) if a series is 100 % compatible with one and only one alternative: merge.
    #' If with more than one: set aside.
  }else if(any(comparison_series$complementary == 100)){
    
    if(sum(comparison_series$complementary == 100) == 1){
      cond <- comparison_series$complementary == 100
      iid_merge <- comparison_series$IndexId[cond]
      gfeid_merge <- comparison_series$GFE_ID[cond]
      
      removed_here <- removed_all[1,]
      removed_here$IndexId <- iid_focal
      removed_here$GFE_ID <- gfeid
      removed_here$dataset <- "all_areas_nuseds"
      comment <- paste("Merged to series IndexId =",iid_merge,"& GFE_ID =",
                       gfeid_merge,"because 100% compatible")
      removed_here$comment <- comment
      
      main <- paste("Merged to",iid_merge,"-",gfeid_merge)
      
    }else{
      set_aside[[count_set_aside]] <- comparison_series
      names(set_aside)[count_set_aside] <- paste("IndexId =",iid_focal,"& GFE_ID =",
                                                 gfeid)
      count_set_aside <- count_set_aside + 1
      main <-"Set aside all alternative series are 100% compatible"
    }
  }else{
    main <- "Set aside for now"
    set_aside[[count_set_aside]] <- comparison_series
    names(set_aside)[count_set_aside] <- paste("IndexId =",iid_focal,"& GFE_ID =",
                                               gfeid)
    count_set_aside <- count_set_aside + 1
    
  }
  
  #
  plot_IndexId_GFE_ID_fun(IndexIds = c(iid_focal,iid_alter),
                          GFE_IDs = rep(trackRecord_nuseds_iid$GFE_ID[i],
                                        length(iid_alter) + 1),
                          all_areas_nuseds = all_areas_nuseds_all, 
                          main = main)
  legend("top",c("focal",rep("alterntive",length(POPULATION_alter))),col = NA,lwd = 1,bty = "n")
  legend("topright",c(POPULATION_focal,POPULATION_alter),pch = NA, bty = "n")
  legend("bottomleft",paste("i =",i),bty = "n")
  
  print(comparison_series)
  print("***")
  
  if(!is.null(removed_here)){
    removed <- rbind(removed,removed_here)
  }
}

#' Case with CN_3331 (i = 1): UNKNOWN TIMING
#' Temporal fields show contradictions.
#' TODO: remove because they cannot be attributed to either alternative series
iid <- c("CN_3331") # ,"CN_3334")
gfeid <- trackRecord_nuseds_iid$GFE_ID[trackRecord_nuseds_iid$IndexId == iid]
cond <- all_areas_nuseds$IndexId == iid & all_areas_nuseds$GFE_ID == gfeid
unique(all_areas_nuseds[cond,c("START_DTT","END_DTT")]) # all year
unique(all_areas_nuseds[cond,c("STREAM_ARRIVAL_DT_FROM","STREAM_ARRIVAL_DT_TO")]) # spring
unique(all_areas_nuseds[cond,c("PEAK_SPAWN_DT_FROM","PEAK_SPAWN_DT_TO")]) # summer/fall
unique(all_areas_nuseds[cond,c("END_SPAWN_DT_FROM","END_SPAWN_DT_TO")])   # fall

toRemove <- data.frame(IndexId = iid, 
                       GFE_ID = gfeid, 
                       dataset = "all_areas_nuseds")
toRemove$comment <- paste("Unkown timing, not in conservation_unit_system_sites; potential alternative series:",
                          trackRecord_nuseds_iid$alternative_IndexId[1])
removed_all <- rbind(removed_all,toRemove)

cond <- all_areas_nuseds$IndexId == iid & 
  all_areas_nuseds$GFE_ID == gfeid
all_areas_nuseds <- all_areas_nuseds[!cond,]
nrow(all_areas_nuseds) # 308232

#' Case with CN_3334 (i = 2): UNKNOWN TIMING
#' Temporal fields show contradictions.
#' TODO: remove because they cannot be attributed to either alternative series
iid <- c("CN_3334")
gfeid <- trackRecord_nuseds_iid$GFE_ID[trackRecord_nuseds_iid$IndexId == iid]
cond <- all_areas_nuseds$IndexId == iid & all_areas_nuseds$GFE_ID == gfeid
unique(all_areas_nuseds[cond,c("START_DTT","END_DTT")]) # all year
unique(all_areas_nuseds[cond,c("STREAM_ARRIVAL_DT_FROM","STREAM_ARRIVAL_DT_TO")]) # spring
unique(all_areas_nuseds[cond,c("PEAK_SPAWN_DT_FROM","PEAK_SPAWN_DT_TO")]) # summer/fall
unique(all_areas_nuseds[cond,c("END_SPAWN_DT_FROM","END_SPAWN_DT_TO")])   # fall

toRemove <- data.frame(IndexId = iid, 
                       GFE_ID = gfeid,
                       dataset = "all_areas_nuseds")
toRemove$comment <- paste("Unkown timing, not in conservation_unit_system_sites; potential alternative series:",
                          trackRecord_nuseds_iid$alternative_IndexId[2])
removed_all <- rbind(removed_all,toRemove)

cond <- all_areas_nuseds$IndexId == iid & 
  all_areas_nuseds$GFE_ID == trackRecord_nuseds_iid$GFE_ID[trackRecord_nuseds_iid$IndexId == iid]
all_areas_nuseds <- all_areas_nuseds[!cond,]
nrow(all_areas_nuseds) # 308196

#' Case with CN_39983 (i = 3):
#' It is not in the PSE
#' TODO: add to CUSS and then streamlocationids
trackRecord_nuseds_iid[3,]
iid <- trackRecord_nuseds_iid[3,]$IndexId
gfeid <- trackRecord_nuseds_iid[3,]$GFE_ID

# add to CUSS
cuss_new <- CUSS_newRow_fun(IndexId = iid, 
                            GFE_ID = gfeid,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId ="CN_39983",
                    GFE_ID = 1204,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = NA,
                    comment = "Series not in CUSS and not merged with CN_39983 - 1204 because of different timing")

toAdd_all <- rbind(toAdd_all,toAdd)

#' check if GFE_ID is not already in CUSS to specify if it needs to be attributed 
#' coordinates in GFE_ID_nuseds_notCuss_df
gfeid <- 1204
cond <- conservation_unit_system_sites_all$GFE_ID == gfeid
if(sum(cond) == 0){
  cond <- GFE_ID_nuseds_notCuss_df$GFE_ID == gfeid
  GFE_ID_nuseds_notCuss_df$need_coordinates[cond] <- 'yes'
}

#' Case with CN_7809 (i = 4):
#' CN_7809 is Summer run type, while CN_48442 is Run 1 --> not contradictory.
#' But series with CN_7809 is much longer and recent than the CN_48442 one.
#' Looks like the two series are merged in the PSE (look for CU "Okanagan").
#' TODO: change series CN_48442 - 442 in CUSS for CN_7809 - 442.
#' --> change IndexId in CUSS
i <- 4
iid <- trackRecord_nuseds_iid$IndexId[i]
iid_alter <- trackRecord_nuseds_iid$alternative_IndexId[i]
gfeid <- trackRecord_nuseds_iid$GFE_ID[i]

sapply(X = c(iid,iid_alter),function(pop){
  cond <- all_areas_nuseds$IndexId == pop
  return(unique(all_areas_nuseds$RUN_TYPE[cond])) # ""  "1"
})

conservationunits_decoder[conservationunits_decoder$cu_name_dfo == "OKANAGAN_1.x",]
conservationunits_decoder[conservationunits_decoder$cu_name_pse == "Okanagan River",]
conservationunits_decoder[grepl("Okanagan",conservationunits_decoder$cu_name_pse),]

toRemove <- data.frame(IndexId = "CN_48442", 
                       GFE_ID = 442, 
                       dataset = "conservation_unit_system_sites")
toRemove$comment <- paste0("Merged to series IndexId = ","CN_7809"," - GFE_ID = ",442,", which was added to CUSS")

removed_all <- rbind(removed_all,toRemove)

#
toAdd <- data.frame(IndexId ="CN_7809",
                    GFE_ID = 442,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = conservation_unit_system_sites$CU_NAME[conservation_unit_system_sites$IndexId == "CN_48442"],
                    comment = "Series not in CUSS and not merged with CN_39983 - 1204 because of different timing")

toAdd_all <- rbind(toAdd_all,toAdd)

# edit indexId - related fields in conservation_unit_system_sites 
conservation_unit_system_sites <- fields_edit_NUSEDS_CUSS_fun(edit_CUSS = T, 
                                                IndexId_focal = "CN_48442",
                                                IndexId_alter = "CN_7809",
                                                GFE_ID_focal = 442,
                                                GFE_ID_alter = 442,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

# merge series in all_areas_nuseds by changing CN_48442 for CN_7809
all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "CN_48442",
                                                IndexId_alter = "CN_7809",
                                                GFE_ID_focal = 442,
                                                GFE_ID_alter = 442,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

#' Case with CO_7776 (i = 5):
#' TODO: removed (already done in for loop) because 100% duplicated

#' Case with SX_46308 (i = 6):
#' The series are different run type and the focal one has values close to 0s
#' TODO: remove it
i <- 6
trackRecord_nuseds_iid[i,]
iid <- trackRecord_nuseds_iid$IndexId[i]
iid_alter <- trackRecord_nuseds_iid$alternative_IndexId[i]
gfeid <- trackRecord_nuseds_iid$GFE_ID[i]

toRemove <- data.frame(IndexId = iid, 
                       GFE_ID = gfeid, 
                       dataset = "all_areas_nuseds")
toRemove$comment <- paste("Not in conservation_unit_system_sites; no alternative series; almost only 0s")

removed_all <- rbind(removed_all,toRemove)

all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds,
                                           toRemove = toRemove, 
                                           fields = c("IndexId","GFE_ID"))
nrow(all_areas_nuseds) # 308176

cond <- all_areas_nuseds_all$IndexId == iid & all_areas_nuseds_all$GFE_ID == gfeid
all_areas_nuseds_all$MAX_ESTIMATE[cond]

#
#' ** 3.3) Alternative GFE_ID without or without alternative IndexId **
cond <- trackRecord_nuseds$alternative_GFE_ID != "none" &
  # trackRecord_nuseds$alternative_IndexId == "none" &
  !cond_3
trackRecord_nuseds_gfeid <- trackRecord_nuseds[cond,]
trackRecord_nuseds_gfeid <- trackRecord_nuseds_gfeid[order(trackRecord_nuseds_gfeid$IndexId),]
trackRecord_nuseds_gfeid
nrow(trackRecord_nuseds_gfeid) # 34 32 26
trackRecord_nuseds_gfeid$i <- NA

#' There several instances where a same IndexId appears multiple time (i.e., with
#' a different GFE_ID) but the same GFE_ID is proposed as an alternative.
#' --> consider those in group.
IndexId_GFE_ID_alternative <- unique(paste(trackRecord_nuseds_gfeid$IndexId,
                                           trackRecord_nuseds_gfeid$alternative_GFE_ID,
                                           sep = "&"))
length(IndexId_GFE_ID_alternative) # 22 20 18

# change order based on GFE_ID_alternative
GFE_ID_alternative <- sapply(X = IndexId_GFE_ID_alternative, 
                             FUN = function(x){strsplit(x = x, split = "&")[[1]][2]})

IndexId_GFE_ID_alternative <- IndexId_GFE_ID_alternative[order(GFE_ID_alternative)]

series_compare <- list()
series_MAX_ESTIMATE <- list()
for(i in 1:length(IndexId_GFE_ID_alternative)){
  # i <- 10
  
  iid <- strsplit(IndexId_GFE_ID_alternative[i],"&")[[1]][1]
  gfeid_alter <- strsplit(IndexId_GFE_ID_alternative[i],"&")[[1]][2]
  
  cond <- trackRecord_nuseds_gfeid$IndexId == iid
  gfeids <- trackRecord_nuseds_gfeid$GFE_ID[cond]
  
  trackRecord_nuseds_gfeid$i[cond] <- i
  
  # check if there is also potential alternative IndexId
  cond <- trackRecord_nuseds_gfeid$IndexId == iid & 
    trackRecord_nuseds_gfeid$alternative_GFE_ID == gfeid_alter
  trackRecord_nuseds_gfeid_cut <- trackRecord_nuseds_gfeid[cond,]
  
  if(any(trackRecord_nuseds_gfeid_cut$alternative_IndexId != "none")){
    m <- matrix(1:2, ncol = 1)
  }else{
    m <- matrix(1)
  }
  
  # case with the alternative GFE_ID
  cond <- all_areas_nuseds$IndexId == iid & all_areas_nuseds$GFE_ID == gfeid_alter
  series_alter <- all_areas_nuseds$MAX_ESTIMATE[cond]
  names(series_alter) <- all_areas_nuseds$Year[cond]
  
  compare <- data.frame(IndexId = rep(iid,length(gfeids)),
                        GFE_ID = gfeids,
                        IndexId_alter = rep(NA,length(gfeids)),
                        GFE_ID_alter = rep(gfeid_alter,length(gfeids)))
  
  compare2 <- sapply(X = gfeids, FUN = function(g){
    cond <- all_areas_nuseds$IndexId == iid & all_areas_nuseds$GFE_ID == g
    series_focal <- all_areas_nuseds$MAX_ESTIMATE[cond]
    names(series_focal) <- all_areas_nuseds$Year[cond]
    out <- compare_series_fun(series_focal = series_focal, 
                              series_compare = series_alter,
                              percentage = T)
    return(out)
  })
  compare2 <- as.data.frame(t(compare2))
  
  compare <- cbind(compare,compare2)
  rownames(compare) <- NULL
  
  # keep the MAX_ESTIMATE for checking out later
  series_df <- data.frame(year = as.numeric(names(series_alter)),
                          s = series_alter)
  names(series_df)[names(series_df) == "s"] <- paste(iid,gfeid_alter,sep="-")
  for(g in gfeids){
    cond <- all_areas_nuseds$IndexId == iid & all_areas_nuseds$GFE_ID == g
    series_df_here <- data.frame(year = all_areas_nuseds$Year[cond],
                                 s = all_areas_nuseds$MAX_ESTIMATE[cond])
    names(series_df_here)[names(series_df_here) == "s"] <- paste(iid,g,sep = "-")
    
    series_df <- base::merge(x = series_df, y =  series_df_here, 
                             by = "year", all = T)
  }
  series_df$sum <- rowSums(series_df[colnames(series_df) != "year"], na.rm = T)
  
  series_MAX_ESTIMATE[[i]] <- series_df
  names(series_MAX_ESTIMATE)[i] <- paste0("i=",i)
  
  series_compare[[i]] <- compare
  names(series_compare)[i] <- paste0("i=",i)
  
  # in case these alternative IndexId:
  if(any(trackRecord_nuseds_gfeid_cut$alternative_IndexId != "none")){
    
    # compare:
    cond <- trackRecord_nuseds_gfeid_cut$alternative_IndexId != "none"
    trackRecord_nuseds_gfeid_cut_cut <- trackRecord_nuseds_gfeid_cut[cond,]
    
    compare_iid <- data.frame(IndexId = trackRecord_nuseds_gfeid_cut_cut$IndexId,
                              GFE_ID = trackRecord_nuseds_gfeid_cut_cut$GFE_ID,
                              IndexId_alter = trackRecord_nuseds_gfeid_cut_cut$alternative_IndexId,
                              GFE_ID_alter = rep(NA,nrow(trackRecord_nuseds_gfeid_cut_cut)))
    
    compare_iid2 <- sapply(X = 1:nrow(trackRecord_nuseds_gfeid_cut_cut), 
                           FUN = function(z){
                             # z <- 1
                             cond <- all_areas_nuseds$IndexId == trackRecord_nuseds_gfeid_cut_cut$IndexId[z] &
                               all_areas_nuseds$GFE_ID == trackRecord_nuseds_gfeid_cut_cut$GFE_ID[z] # == gfeid
                             series_focal <- all_areas_nuseds$MAX_ESTIMATE[cond]
                             names(series_focal) <- all_areas_nuseds$Year[cond]
                             
                             cond <- all_areas_nuseds$IndexId == trackRecord_nuseds_gfeid_cut_cut$alternative_IndexId[z] &
                               all_areas_nuseds$GFE_ID == trackRecord_nuseds_gfeid_cut_cut$GFE_ID[z] # == gfeid
                             series_alter <- all_areas_nuseds$MAX_ESTIMATE[cond]
                             names(series_alter) <- all_areas_nuseds$Year[cond]
                             out <- compare_series_fun(series_focal = series_focal, 
                                                       series_compare = series_alter,
                                                       percentage = T)
                             return(out)
                           })
    compare_iid2 <- as.data.frame(t(compare_iid2))
    
    compare_iid <- cbind(compare_iid,compare_iid2)
    rownames(compare_iid) <- NULL
    
    compare <- rbind(compare,compare_iid)
    series_compare[[i]] <- compare
    
    # time series:
    for(r in 1:nrow(trackRecord_nuseds_gfeid_cut_cut)){
      # r <- 1
      cond <- all_areas_nuseds$IndexId == trackRecord_nuseds_gfeid_cut_cut$alternative_IndexId[r] &
        all_areas_nuseds$GFE_ID == trackRecord_nuseds_gfeid_cut_cut$GFE_ID[r] # gfeid
      series_df_here <- data.frame(year = all_areas_nuseds$Year[cond],
                                   s = all_areas_nuseds$MAX_ESTIMATE[cond])
      names(series_df_here)[names(series_df_here) == "s"] <- paste(trackRecord_nuseds_gfeid_cut_cut$alternative_IndexId[r],
                                                                   trackRecord_nuseds_gfeid_cut_cut$GFE_ID[r],
                                                                   sep = "-")
      
      series_df <- base::merge(x = series_df, y =  series_df_here, 
                               by = "year", all = T)
      series_MAX_ESTIMATE[[i]] <- series_df
    }
  }
  
  ylim <- range(series_df[,!colnames(series_df) %in% c("year","sum")], na.rm = T)
  xlim <- range(series_df$year)
  
  print(paste("*** i =",i,"***"))
  print(compare)
  
  # plot
  layout(m)
  par(mar =c(4.5,4.5,.5,.5))
  
  WATERBODYs <- sapply(X = c(gfeid_alter,gfeids),FUN = function(g){
    cond <- all_areas_nuseds$GFE_ID == g
    return(unique(all_areas_nuseds$WATERBODY[cond]))
  })
  
  cond <- all_areas_nuseds$IndexId == iid
  POPULATION <- unique(all_areas_nuseds$POPULATION[cond])
  
  plot_IndexId_GFE_ID_fun(IndexIds = rep(iid,length(gfeids) + 1),
                          GFE_IDs = c(gfeid_alter,gfeids),
                          all_areas_nuseds = all_areas_nuseds, 
                          Xlim = xlim, Ylim = ylim)
  legend("topleft","                                  (alternative)", col = NA, pch = 16, lwd = 2, bty = "n")
  legend("top",c("POPULATION",POPULATION),bty = "n")
  legend("topright",WATERBODYs,bty = "n")
  legend("bottomleft",paste("i =",i),bty = "n")
  
  # case with alternative IndexId:
  if(any(trackRecord_nuseds_gfeid_cut$alternative_IndexId != "none")){
    
    plot_IndexId_GFE_ID_fun(IndexIds = c(trackRecord_nuseds_gfeid_cut_cut$alternative_IndexId,
                                         trackRecord_nuseds_gfeid_cut_cut$IndexId),
                            GFE_IDs = c(trackRecord_nuseds_gfeid_cut_cut$GFE_ID,trackRecord_nuseds_gfeid_cut_cut$GFE_ID),
                            all_areas_nuseds = all_areas_nuseds,
                            Xlim = xlim, Ylim = ylim)
    
    POPULATION <- sapply(X = c(trackRecord_nuseds_gfeid_cut_cut$alternative_IndexId,
                               trackRecord_nuseds_gfeid_cut_cut$IndexId),
                         FUN = function(g){
                           cond <- all_areas_nuseds$IndexId == g
                           return(unique(all_areas_nuseds$POPULATION[cond]))
                         })
    
    legend("topleft",rep("                                  (alternative)",nrow(trackRecord_nuseds_gfeid_cut_cut)),
           col = NA, pch = 16, lwd = 2, bty = "n")
    legend("topright",rep(WATERBODYs[trackRecord_nuseds_gfeid_cut_cut$GFE_ID],nrow(trackRecord_nuseds_gfeid_cut_cut)), bty = "n")
    legend("top",POPULATION, bty = "n")
    legend("bottomleft",paste("i =",i),bty = "n")
  }
}

#' Cases with SOMASS-SPROAT-GC SYSTEM  (i = 1 to 4) ***
#' --> do we create a data point to WATERHED ABOVE STAMP FALLS in CUSS then create a streamID --> get lat long OR GFE_ID (if possible)
#' 
#' CM (i = 1) --> merge potentially but check streamID --> only a few make it up the falls --> keep separate 
#' CN (i = 2) --> combine red a and green and remove blue --> SUM ALL OF THEM because most individual chinook would pass the falls
#' PK (i = 3) --> ?
#' SX (i = 4) --> top plot: don't mix, bottom: combine series and FLAG TO BRUCE cf. email forwarded
i <- 1:4
lapply(X = i, FUN = function(i){series_compare[[i]]})
series_MAX_ESTIMATE[1]

cond <- grepl("somass",streamlocationids$sys_nm)
streamlocationids[cond,]

series_MAX_ESTIMATE[2]

#' CM (i = 1) keep separate because only a few make it up the falls
#' Need to add the time series to CUSS --> new GFE_ID
i <- 1
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]

cuss_new <- CUSS_newRow_fun(IndexId = "CM_3305", 
                            GFE_ID = 11486,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

# the GFE_ID is not in CUSS so the GIS information has to be found by hand:
cuss_new$Y_LAT
cuss_new$X_LONGT

#' Take GIS coordinates from Google map just above the Stamp falls (on the left of 
#' the Stamp river fish ladder):
#' Google uses the World Geodetic System WGS84 standard. --> same as in CUSS
cuss_new$Y_LAT <- 49.332224
cuss_new$X_LONGT <- -124.919717

# add to conservation_unit_system_sites
conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId = trackRecord_nuseds_gfeid$IndexId[cond_i],
                    GFE_ID = trackRecord_nuseds_gfeid$GFE_ID[cond_i],
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = NA,
                    comment = "Series not in CUSS, not merged to altenative series IndexId = CM_3305 & GHE_ID = 11485 because upper location; GFE_ID not in CUSS so coordinates were define by hand")

toAdd$CU_NAME <- cuss_new$CU_NAME

toAdd_all <- rbind(toAdd_all,toAdd)

#' check if GFE_ID is not already in CUSS to specify if it needs to be attributed 
#' coordinates in GFE_ID_nuseds_notCuss_df
gfeid <- 11486
cond <- conservation_unit_system_sites_all$GFE_ID == gfeid
if(sum(cond) == 0){
  cond <- GFE_ID_nuseds_notCuss_df$GFE_ID == gfeid
  GFE_ID_nuseds_notCuss_df$need_coordinates[cond] <- 'yes'
}


#' CN (i = 2) 
#' --> add green to CUSS
#' --> remove blue because points are in conflict with green and there 
#' are only four of them.
# obtain a new CUSS row:
i <- 2
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]

cuss_new <- CUSS_newRow_fun(IndexId = "CN_3306", 
                            GFE_ID = 11486,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

# add to conservation_unit_system_sites
conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId = "CN_3306",
                    GFE_ID = 11486,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = NA,
                    comment = "Series not in CUSS, not merged to altenative series INdexId = CN_3306 & GHE_ID = 11485 because upper location; GFE_ID not in CUSS so coordinates were define by hand")

toAdd$CU_NAME <- cuss_new$CU_NAME

toAdd_all <- rbind(toAdd_all,toAdd)

#' check if GFE_ID is not already in CUSS to specify if it needs to be attributed 
#' coordinates in GFE_ID_nuseds_notCuss_df
gfeid <- 11486
cond <- conservation_unit_system_sites_all$GFE_ID == gfeid
if(sum(cond) == 0){
  cond <- GFE_ID_nuseds_notCuss_df$GFE_ID == gfeid
  GFE_ID_nuseds_notCuss_df$need_coordinates[cond] <- 'yes'
}

#' remove blue series
removed <- data.frame(IndexId = "CN_3306",
                      GFE_ID = 11488)
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Series not in CUSS only 4 data points; not merged to potential series IndexId = CN_3306 & GFE_ID = 11485"
removed_all <- rbind(removed_all,removed)

all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                           toRemove = removed, 
                                           fields = c("IndexId","GFE_ID"))

#' PKO (i = 3)
#' --> add blue to CUSS
i <- 3
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]

cuss_new <- CUSS_newRow_fun(IndexId = "PKO_3304",
                            GFE_ID = 11486,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

# add to conservation_unit_system_sites
conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId = "PKO_3304",
                    GFE_ID = 11486,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = NA,
                    comment = "Series not in CUSS, not merged to altenative series INdexId = PKO_3304 & GHE_ID = 11485 because upper location; GFE_ID not in CUSS so coordinates were define by hand")

toAdd$CU_NAME <- cuss_new$CU_NAME

toAdd_all <- rbind(toAdd_all,toAdd)


#' SX (i = 4)
#' In bottom plot:
#' --> merge SX_3302 - 3416 (black) to SX_3310 - 3416 (red) in NUSEDS
#' --> merge SX_3302 - 3444 (blue)  to SX_3325 - 3444 (green) in NUSEDS
i <- 4 
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
 
all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "SX_3302",
                                                IndexId_alter = "SX_3310",
                                                GFE_ID_focal = 3416,
                                                GFE_ID_alter = 3416,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "SX_3302",
                                                IndexId_alter = "SX_3325",
                                                GFE_ID_focal = 3444,
                                                GFE_ID_alter = 3444,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

removed <- data.frame(IndexId = c("CN_3306","SX_3302"),
                      GFE_ID = c(3416,3444))
removed$dataset <- "all_areas_nuseds"
removed$comment <- c("Series not in CUSS, merged to alternative series IndexId = SX_3310 & GFE_ID = 3416",
                     "Series not in CUSS, merged to alternative series IndexId = SX_3325 & GFE_ID = 3444")
removed_all <- rbind(removed_all,removed)

#' Cases with ALOUETTE RIVER (i = 5)
#' those are probably summed up -> a bit higher in PSE
#' In bottom plot: merge blue to red
#' In top plot: create new series in CUSS for black and blue
i <- 5
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]

series_MAX_ESTIMATE[i]

#' In bottom plot: merge blue to red
#' CM_47925 --> CM_47928
removed <- data.frame(IndexId = c("CM_47925"),
                      GFE_ID = c(15))
removed$dataset <- "all_areas_nuseds"
removed$comment <- c("Series not in CUSS, merged to alternative series IndexId = CM_47928 & GFE_ID = 15")
removed_all <- rbind(removed_all,removed)

all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "CM_47925",
                                                IndexId_alter = "CM_47928",
                                                GFE_ID_focal = 15,
                                                GFE_ID_alter = 15,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

#' In top plot: create new series in CUSS for black and blue
#' - black: CM_47925 - 31516 --> GFE_ID not in CUSS, need to find the coordinates TODO
#' - blue:  CM_47925 - 31740 --> GFE_ID not in CUSS, need to find the coordinates TODO
cuss_new <- CUSS_newRow_fun(IndexId = "CM_47925",
                            GFE_ID = 31516,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

cond <- conservation_unit_system_sites_all$GFE_ID == 31516
sum(cond) 
cuss_new$SYSTEM_SITE # MILLIONAIRE CREEK

cuss_new$Y_LAT
cuss_new$X_LONGT

# add to conservation_unit_system_sites
conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId = "CM_47925",
                    GFE_ID = 31516,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = cuss_new$CU_NAME,
                    comment = "Series not in CUSS, not merged to altenative series IndexId = CM_47925 & GHE_ID = 14 because different location; GFE_ID not in CUSS so coordinates were define by hand")

toAdd_all <- rbind(toAdd_all,toAdd)

#' check if GFE_ID is not already in CUSS to specify if it needs to be attributed 
#' coordinates in GFE_ID_nuseds_notCuss_df
gfeid <- 31516
cond <- conservation_unit_system_sites_all$GFE_ID == gfeid
if(sum(cond) == 0){
  cond <- GFE_ID_nuseds_notCuss_df$GFE_ID == gfeid
  GFE_ID_nuseds_notCuss_df$need_coordinates[cond] <- 'yes'
}

#
cuss_new <- CUSS_newRow_fun(IndexId = "CM_47925",
                            GFE_ID = 31740,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

cond <- conservation_unit_system_sites_all$GFE_ID == 31740
sum(cond) 
cuss_new$SYSTEM_SITE # LATIMER CREEK

cuss_new$Y_LAT
cuss_new$X_LONGT

# add to conservation_unit_system_sites
conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId = "CM_47925",
                    GFE_ID = 31740,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = cuss_new$CU_NAME,
                    comment = "Series not in CUSS, not merged to altenative series IndexId = CM_47925 & GHE_ID = 14 because different location; GFE_ID not in CUSS so coordinates were define by hand")

toAdd_all <- rbind(toAdd_all,toAdd)

#' check if GFE_ID is not already in CUSS to specify if it needs to be attributed 
#' coordinates in GFE_ID_nuseds_notCuss_df
gfeid <- 31740
cond <- conservation_unit_system_sites_all$GFE_ID == gfeid
if(sum(cond) == 0){
  cond <- GFE_ID_nuseds_notCuss_df$GFE_ID == gfeid
  GFE_ID_nuseds_notCuss_df$need_coordinates[cond] <- 'yes'
}

#' Cases with SWIFT RIVER / COTTONWOOD RVER (i = 6)
#' Fraser
#' --> combined in the PSE same in the PSE: Middle Fraser River Spring 
#' TODO: merge the the blue series to the red
#' --> change GFE_ID 2464 to 142
i <- 6
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i]

series_MAX_ESTIMATE[i]

all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "CN_47277",
                                                IndexId_alter = "CN_47277",
                                                GFE_ID_focal = 2464,
                                                GFE_ID_alter = 142,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

removed <- data.frame(IndexId = c("CN_47277"),
                      GFE_ID = c(2464))
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Series not in CUSS, merged to series CN_47277 & GFE_ID = 142"
removed_all <- rbind(removed_all,removed)
nrow(removed_all) # 4602

#' Cases with NICOLA RIVER (i = 7)
#' TODO: 
#' - merge green to blue --> GFE_ID from 2461 to 213 
#' - create a new series in CUSS for the blue because the dam is closer to the 
#' UPPER NICOLA RIVER and shoud consequently not be merge to lower parts of the 
#' river. The dam is even closer to Clapperton Creek, which the time series is 
#' shown below. But its POP_ID is different so merging is excluded.
i <- 7
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i]

series_MAX_ESTIMATE[i]

cond <- grepl("CLAPPERTON",all_areas_nuseds$WATERBODY)
sum(cond)
unique(all_areas_nuseds$IndexId[cond])
unique(all_areas_nuseds$GFE_ID[cond])
plot_IndexId_GFE_ID_fun(IndexIds = c("CO_46170","CO_44965"),
                        GFE_IDs = c(54282,2370),
                        all_areas_nuseds = all_areas_nuseds)

#' - merge green to blue --> GFE_ID from 2461 to 213 in NUSEDS
all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "CO_46170",
                                                IndexId_alter = "CO_46170",
                                                GFE_ID_focal = 2461,
                                                GFE_ID_alter = 213,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

removed <- data.frame(IndexId = c("CO_46170"),
                      GFE_ID = c(2461))
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Series not in CUSS, merged to series CO_46170 & GFE_ID = 213"
removed_all <- rbind(removed_all,removed)

#' - create a new series in CUSS for the blue:
cuss_new <- CUSS_newRow_fun(IndexId = "CO_46170",
                            GFE_ID = 54282,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

cuss_new$Y_LAT
cuss_new$X_LONGT

# add to conservation_unit_system_sites
conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId ="CO_46170",
                    GFE_ID = 54282,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = cuss_new$CU_NAME,
                    comment = "Series not in CUSS and not merged with CO_46170 - 213 because of different location")

toAdd_all <- rbind(toAdd_all,toAdd)

#' check if GFE_ID is not already in CUSS to specify if it needs to be attributed 
#' coordinates in GFE_ID_nuseds_notCuss_df
gfeid <- 54282
cond <- conservation_unit_system_sites_all$GFE_ID == gfeid
if(sum(cond) == 0){
  cond <- GFE_ID_nuseds_notCuss_df$GFE_ID == gfeid
  GFE_ID_nuseds_notCuss_df$need_coordinates[cond] <- 'yes'
}

#' Cases with STEON RIVER - CN (i = 8)
#' blue is actually a duplicate of the red in lower plot --> remove from nuseds
i <- 8
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i]
series_compare[[i]]
series_MAX_ESTIMATE[i]

removed <- data.frame(IndexId = c("CN_2178"),
                      GFE_ID = 129)
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Series not in CUSS, duplicates of series IndexId = CN_47189 & GFE_ID = 129"
removed_all <- rbind(removed_all,removed)

all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                           toRemove = removed, 
                                           fields = c("IndexId","GFE_ID"))

#' Cases with STEON RIVER / PORTAGE CREEK - CO (i = 9)
#' All are in the Seton River near Lillooets
#' - top plot: DO NOT merge blue to red because there is a conflict in 2013
#'   --> create a new row in CUSS
#' - bottom plot: merge blue to red --> change IndexId from CO_44539 to CO_47183 in NUSEDS
i <- 9
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_IndexId[cond_i]


#' - top plot: create a new row in CUSS for CO_44539 - 2451 (2451 is already in CUSS)
sum(conservation_unit_system_sites_all$GFE_ID == 2451)

cuss_new <- CUSS_newRow_fun(IndexId = "CO_44539",
                            GFE_ID = 2451,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId ="CO_44539",
                    GFE_ID = 2451,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = cuss_new$CU_NAME,
                    comment = "Series not in CUSS and not merged with CO_44539 - 2476 because of one conflictual data point")

toAdd_all <- rbind(toAdd_all,toAdd)

#' check if GFE_ID is not already in CUSS to specify if it needs to be attributed 
#' coordinates in GFE_ID_nuseds_notCuss_df
gfeid <- 2451
cond <- conservation_unit_system_sites_all$GFE_ID == gfeid
if(sum(cond) == 0){
  cond <- GFE_ID_nuseds_notCuss_df$GFE_ID == gfeid
  GFE_ID_nuseds_notCuss_df$need_coordinates[cond] <- 'yes'
}


#' - bottom plot: merge blue to red --> change IndexId from CO_44539 to CO_47183 
#' in NUSEDS
all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "CO_44539",
                                                IndexId_alter = "CO_47183",
                                                GFE_ID_focal = 129,
                                                GFE_ID_alter = 129,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

removed <- data.frame(IndexId = c("CO_44539"),
                      GFE_ID = c(129))
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Series not in CUSS, merged to series CO_47183 & GFE_ID = 129"
removed_all <- rbind(removed_all,removed)


#' Cases with THOMPSON RIVER CO (i = 10)
#' --> add these series to CUSS --> new GFE_IDs
i <- 10
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_IndexId[cond_i]

for(gfeid in trackRecord_nuseds_gfeid$GFE_ID[cond_i]){
  
  cuss_new <- CUSS_newRow_fun(IndexId = "CO_46582",
                              GFE_ID = gfeid,
                              conservation_unit_system_sites = conservation_unit_system_sites,
                              all_areas_nuseds = all_areas_nuseds)
  
  print(cuss_new[,c("GFE_ID","SYSTEM_SITE")])
  
  conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                          cuss_new)
  
  toAdd <- data.frame(IndexId ="CO_46582",
                      GFE_ID = gfeid,
                      dataset = "conservation_unit_system_sites",
                      CU_NAME = cuss_new$CU_NAME,
                      comment = "Series not in CUSS and not merged with CO_46582 - 256 because of different location")
  
  toAdd_all <- rbind(toAdd_all,toAdd)
  
  #' check if GFE_ID is not already in CUSS to specify if it needs to be attributed 
  #' coordinates in GFE_ID_nuseds_notCuss_df
  cond <- conservation_unit_system_sites_all$GFE_ID == gfeid
  if(sum(cond) == 0){
    cond <- GFE_ID_nuseds_notCuss_df$GFE_ID == gfeid
    GFE_ID_nuseds_notCuss_df$need_coordinates[cond] <- 'yes'
  }
}


#' Cases with BARRIER RIVER (i = 11)
#' TODO:
#' - bottom plot: merge blue to red --> change IndexId in NUSEDS 
#' - top plot: create new series in CUSS for green
i <- 11
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_IndexId[cond_i]

#' - bottom plot: merge blue to red --> change IndexId in NUSEDS 
all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "CO_46602",
                                                IndexId_alter = "CO_46632",
                                                GFE_ID_focal = 2746,
                                                GFE_ID_alter = 2746,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

removed <- data.frame(IndexId = c("CO_46602"),
                      GFE_ID = c(2746))
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Series not in CUSS, merged to series CO_46632 & GFE_ID = 2746"
removed_all <- rbind(removed_all,removed)

#' - top plot: create new series in CUSS for green
cuss_new <- CUSS_newRow_fun(IndexId = "CO_46602",
                            GFE_ID = 212716981,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId ="CO_46602",
                    GFE_ID = 212716981,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = cuss_new$CU_NAME,
                    comment = "Series not in CUSS and not merged with CO_46632 - 258 because of different location")

toAdd_all <- rbind(toAdd_all,toAdd)

#' check if GFE_ID is not already in CUSS to specify if it needs to be attributed 
#' coordinates in GFE_ID_nuseds_notCuss_df
gfeid <- 212716981
cond <- conservation_unit_system_sites_all$GFE_ID == gfeid
if(sum(cond) == 0){
  cond <- GFE_ID_nuseds_notCuss_df$GFE_ID == gfeid
  GFE_ID_nuseds_notCuss_df$need_coordinates[cond] <- 'yes'
}

#' Cases with FENNELL CREEK / SASKUM CREEK (i = 12, 13)
#' TODO: merge for both --> change GFE_ID in NUSEDS
i <- 12
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_IndexId[cond_i]

all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "SX_3416",
                                                IndexId_alter = "SX_3416",
                                                GFE_ID_focal = 2746,
                                                GFE_ID_alter = 261,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

removed <- data.frame(IndexId = c("SX_3416"),
                      GFE_ID = c(2746))
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Series not in CUSS, merged to series SX_3416 & GFE_ID = 261"
removed_all <- rbind(removed_all,removed)


i <- 13
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_IndexId[cond_i]

all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "CO_46632",
                                                IndexId_alter = "CO_46632",
                                                GFE_ID_focal = 261,
                                                GFE_ID_alter = 2746,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

removed <- data.frame(IndexId = c("CO_46632"),
                      GFE_ID = c(261))
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Series not in CUSS, merged to series CO_46632 & GFE_ID = 2746"
removed_all <- rbind(removed_all,removed)


#' Cases with BLUE RIVER (i = 14)
#' series were added up in the PSE,
#' TODO:
#' - merge blue to red --> change GFE_ID
#' - create new series in CUSS for green
i <- 14
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_IndexId[cond_i]
series_MAX_ESTIMATE[i]
series_compare[[i]]

#' - merge blue to red --> change GFE_ID
all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
                                                IndexId_focal = "CO_46795",
                                                IndexId_alter = "CO_46795",
                                                GFE_ID_focal = 33103,
                                                GFE_ID_alter = 281,
                                                all_areas_nuseds = all_areas_nuseds,
                                                conservation_unit_system_sites = conservation_unit_system_sites)

removed <- data.frame(IndexId = c("CO_46795"),
                      GFE_ID = c(33103))
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Series not in CUSS, merged to series CO_46795 & GFE_ID = 281"
removed_all <- rbind(removed_all,removed)

#' - create new series in CUSS for green
cuss_new <- CUSS_newRow_fun(IndexId = "CO_46795",
                            GFE_ID = 1921661712,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId ="CO_46795",
                    GFE_ID = 1921661712,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = cuss_new$CU_NAME,
                    comment = "Series not in CUSS and not merged to CO_46795 & GFE_ID = 281 because of different location")

toAdd_all <- rbind(toAdd_all,toAdd)

#' check if GFE_ID is not already in CUSS to specify if it needs to be attributed 
#' coordinates in GFE_ID_nuseds_notCuss_df
gfeid <- 1921661712
cond <- conservation_unit_system_sites_all$GFE_ID == gfeid
if(sum(cond) == 0){
  cond <- GFE_ID_nuseds_notCuss_df$GFE_ID == gfeid
  GFE_ID_nuseds_notCuss_df$need_coordinates[cond] <- 'yes'
}


#' Case 1 with CARIBOO RIVER (i = 15)
#' CN_46891 - 2467 is 100% duplicated with CN_46891 - 290
#' TODO: remove from NUSEDS
i <- 15
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_IndexId[cond_i]
series_compare[[i]]

removed <- data.frame(IndexId = c("CN_46891"),
                      GFE_ID = 2467)
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Duplicates of series IndexId = CN_46891 & GFE_ID = 290"
removed_all <- rbind(removed_all,removed)

all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                           toRemove = removed, 
                                           fields = c("IndexId","GFE_ID"))

#' Case 2 with CARIBOO RIVER (i = 16):
#' alternative CN_46892 - 290
#' This is a complicated case that was spotted by looking at the time series:
#' In the figure below:
#' - top plot: the alternative series has 3 points duplicating with the focal series
#' - bottom plot: the same alternative series has a duplicated point with another series
#' TODO: 
#' - remove the 5th point in the alternative series (IndexId = CN_46892 & GFE_ID = 290),
#' - then merge the focal series (CN_46892 - 2466) to it, taking care of the duplicated
#' points (i.e. removing them)

layout(matrix(1:2,nrow = 2))
plot_IndexId_GFE_ID_fun(IndexIds = "CN_46892",all_areas_nuseds = all_areas_nuseds)
legend("top",c(NA,"focal","alternative"),bty = 'n')
plot_IndexId_GFE_ID_fun(GFE_IDs = 290, species_acro = "CN",
                        all_areas_nuseds = all_areas_nuseds)

# alternative series:
cond_CN_46892_290 <- all_areas_nuseds$IndexId == "CN_46892" & 
  all_areas_nuseds$GFE_ID == 290
nuseds_CN_46892_290 <- all_areas_nuseds[cond_CN_46892_290,]

# focal series
cond_CN_46892_2466 <- all_areas_nuseds$IndexId == "CN_46892" & 
  all_areas_nuseds$GFE_ID == 2466
nuseds_CN_46892_2466 <- all_areas_nuseds[cond_CN_46892_2466,]

# remove the 5th point in nuseds_CN_46892_290, which is also the highest
r_toremove <- which(nuseds_CN_46892_290$MAX_ESTIMATE == max(nuseds_CN_46892_290$MAX_ESTIMATE, na.rm = T))
nuseds_CN_46892_290 <- nuseds_CN_46892_290[-r_toremove,]

# remove rows in nuseds_CN_46892_2466 with duplicated values in nuseds_CN_46892_290
colSelect <- c("Year","MAX_ESTIMATE")
data <- merge(nuseds_CN_46892_2466[,colSelect],
              nuseds_CN_46892_290[,colSelect],
              by = "Year", all.x = T)
data <- data[!is.na(data$MAX_ESTIMATE.x) &
               !is.na(data$MAX_ESTIMATE.y) &
               data$MAX_ESTIMATE.x == data$MAX_ESTIMATE.y,]

cond <- ! nuseds_CN_46892_2466$MAX_ESTIMATE %in% data$MAX_ESTIMATE.x
nuseds_CN_46892_2466 <- nuseds_CN_46892_2466[cond,]
nuseds_CN_46892_2466[,c("Year","MAX_ESTIMATE")]

# remove rows in nuseds_CN_46892_2466 with NAs for MAX_ESTIMATE in years already 
# present in nuseds_CN_46892_290 (to avoid creating duplicated years)
data <- merge(nuseds_CN_46892_2466[,colSelect],
              nuseds_CN_46892_290[,colSelect],
              by = "Year", all.x = T)
data <- data[!is.na(data$MAX_ESTIMATE.x),]
cond <- nuseds_CN_46892_2466$Year %in% data$Year
nuseds_CN_46892_2466 <- nuseds_CN_46892_2466[cond,]
nuseds_CN_46892_2466[,c("Year","MAX_ESTIMATE")]

# edit nuseds_CN_46892_2466 with new GFE_ID-related fields
for(f in fields_l$NUSEDS$GFE_ID){
  nuseds_CN_46892_2466[,f] <- unique(nuseds_CN_46892_290[,f])
}

#' edit data in all_areas_nuseds
all_areas_nuseds <- all_areas_nuseds[!(cond_CN_46892_290 | cond_CN_46892_2466),]
all_areas_nuseds <- rbind(all_areas_nuseds,
                          nuseds_CN_46892_2466,
                          nuseds_CN_46892_290)

layout(matrix(1))
plot_IndexId_GFE_ID_fun(IndexIds = "CN_46892",all_areas_nuseds = all_areas_nuseds)

# edit removed_all
removed <- data.frame(IndexId = c("CN_46892"),
                      GFE_ID = c(2466))
removed$dataset <- "all_areas_nuseds"
removed$comment <- "Merged to series CN_46892 & GFE_ID = 290 with which 3 data points were duplicated; one was duplicated with CN_46891 & GFE_ID = 290"
removed_all <- rbind(removed_all,removed)


#' Cases with PHILLIPS RIVER & CLEAR WATER CREEK (VIMI) (i = 17 to 22)
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1708020689543579?thread_ts=1707771319.134789&cid=CJ5RVHVCG
#' TODO: create a new series for all of them in CUSS
for(i in 17:22){
  cond_i <- trackRecord_nuseds_gfeid$i == i
  
  cuss_new <- CUSS_newRow_fun(IndexId = trackRecord_nuseds_gfeid$IndexId[cond_i],
                              GFE_ID = trackRecord_nuseds_gfeid$GFE_ID[cond_i],
                              conservation_unit_system_sites = conservation_unit_system_sites,
                              all_areas_nuseds = all_areas_nuseds)
  
  conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                          cuss_new)
  
  toAdd <- data.frame(IndexId = trackRecord_nuseds_gfeid$IndexId[cond_i],
                      GFE_ID = 1204,
                      dataset = trackRecord_nuseds_gfeid$GFE_ID[cond_i],
                      CU_NAME = cuss_new$CU_NAME,
                      comment = paste0("Series not in CUSS and not merged with ",
                                       trackRecord_nuseds_gfeid$IndexId[cond_i]," - ",
                                       trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i],
                                       " because of different location"))
  
  toAdd_all <- rbind(toAdd_all,toAdd)
  
  #' check if GFE_ID is not already in CUSS to specify if it needs to be attributed 
  #' coordinates in GFE_ID_nuseds_notCuss_df
  gfeid <- trackRecord_nuseds_gfeid$GFE_ID[cond_i]
  cond <- conservation_unit_system_sites_all$GFE_ID == gfeid
  if(sum(cond) == 0){
    cond <- GFE_ID_nuseds_notCuss_df$GFE_ID == gfeid
    GFE_ID_nuseds_notCuss_df$need_coordinates[cond] <- 'yes'
  }
}

#
#' ** 3.4) Alternative GFE_ID without or without alternative IndexId **
cond <- trackRecord_nuseds$alternative_GFE_ID == "none" &
  trackRecord_nuseds$alternative_IndexId == "none" &
  !cond_3
trackRecord_nuseds_nocuss <- trackRecord_nuseds[cond,]
nrow(trackRecord_nuseds_nocuss) # 37

#' TODO: delete them from NUSEDS because we cannot attribute them a CU_NAME.

for(r in 1:nrow(trackRecord_nuseds_nocuss)){
  # r <- 1
  
  # check if IndexId is in CUSS
  cond <- conservation_unit_system_sites$IndexId == trackRecord_nuseds_nocuss$IndexId[r]
  if(sum(cond) > 0){
    IndexId_inCuss <- T
  }else{
    IndexId_inCuss <- F
  }
  
  # check if GFE_ID is in CUSS
  cond <- conservation_unit_system_sites$GFE_ID == trackRecord_nuseds_nocuss$GFE_ID[r] &
    grepl(strsplit(x = trackRecord_nuseds_nocuss$IndexId[r],split = "_")[[1]][1],
          conservation_unit_system_sites$IndexId)
    
  if(sum(cond) > 0){
    GFE_ID_inCuss <- T
  }else{
    GFE_ID_inCuss <- F
  }
  
  plot_IndexId_GFE_ID_fun(IndexIds = trackRecord_nuseds_nocuss$IndexId[r],
                          GFE_IDs = trackRecord_nuseds_nocuss$GFE_ID[r],
                          all_areas_nuseds = all_areas_nuseds)
  legend("bottomleft",paste("n =",r),bty = "n")
  legend("top",c(paste("IndexId is in CUSS:",IndexId_inCuss),
                 paste("GFE_ID is in CUSS:",GFE_ID_inCuss)),bty = "n")
  
  #
  removed <- data.frame(IndexId = trackRecord_nuseds_nocuss$IndexId[r],
                        GFE_ID = trackRecord_nuseds_nocuss$GFE_ID[r])
  removed$dataset <- "all_areas_nuseds"
  removed$comment <- "Series not in CUSS, neither IndexId nor GFE_ID are in CUSS"
  removed_all <- rbind(removed_all,removed)
  
  #
  all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                             toRemove = removed, 
                                             fields = c("IndexId","GFE_ID"))
}

nrow(all_areas_nuseds) # 307217 307244


# Check - normally all series in NUSEDS and in CUSS now:
series_nuseds <- paste(all_areas_nuseds$IndexId,
                       all_areas_nuseds$GFE_ID,sep = "&")
series_nuseds <- unique(series_nuseds)
length(series_nuseds) # 6910

series_cuss <- paste(conservation_unit_system_sites$IndexId,
                     conservation_unit_system_sites$GFE_ID,sep = "&")
series_cuss <- unique(series_cuss)
length(series_cuss) # 6910

#' Now all the series in CUSS are in all_areas_nuseds.
series_cuss[! series_cuss %in% series_nuseds]

# Number of series in NUSEDS not in CUSS:
series_Nuseds_noCuss <- series_nuseds[! series_nuseds %in% series_cuss]
series_Nuseds_noCuss <- series_Nuseds_noCuss[order(series_Nuseds_noCuss)]
length(series_Nuseds_noCuss) # 0

#' Export the series in NUSEDS with IndexIds and GFE_IDs not in CUSS with 
#' info on the number of data points and POPULATION
#' slack: https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1709250704372429

cond <- trackRecord_nuseds$alternative_GFE_ID == "none" &
  trackRecord_nuseds$alternative_IndexId == "none"

trackRecord_toExport <- trackRecord_nuseds[cond,c("IndexId","GFE_ID","nb_dataPt")]

trackRecord_toExport$POP_ID <- apply(X = trackRecord_toExport, 1, 
                                     FUN = function(r){
                                       iid <- r["IndexId"]
                                       popid <- strsplit(x = iid, split = "_")[[1]][2]
                                       return(popid)
                                     })

trackRecord_toExport$POPULATION <- apply(X = trackRecord_toExport, 1, 
                                         FUN = function(r){
                                           iid <- r["IndexId"]
                                           gfeid <- r["GFE_ID"]
                                           cond <- all_areas_nuseds_all$IndexId == iid &
                                             all_areas_nuseds_all$GFE_ID == gfeid
                                           return(unique(all_areas_nuseds_all$POPULATION[cond]))
                                         })

# write.csv(trackRecord_toExport,paste0(wd_output,"/series_inNUSEDS_noInCUSS.csv"), 
#           row.names = F)

#
# Merge NUSEDS AND CUSS -----
#
col_common <- c("IndexId","POP_ID","GFE_ID")

col_nuseds <- c("SPECIES","WATERBODY","AREA","Year","MAX_ESTIMATE"
                # "ENUMERATION_METHODS",            # these must be kept out because they differ between years
                # "ESTIMATE_CLASSIFICATION",
                # "ESTIMATE_METHOD"
                )

col_cuss <- c("SPECIES_QUALIFIED","CU_NAME","CU_TYPE","FAZ_ACRO","JAZ_ACRO","MAZ_ACRO",
              "FULL_CU_IN","SYSTEM_SITE","Y_LAT","X_LONGT","IS_INDICATOR",
              "CU_LAT","CU_LONGT")

# transform all_areas_nuseds to a wide format
library(tidyr)
nuseds_long <- all_areas_nuseds[,c(col_common,col_nuseds)] %>% 
  pivot_wider(names_from = "Year",values_from = "MAX_ESTIMATE",names_sort = T)

nrow(all_areas_nuseds) # 307217
nrow(nuseds_long)      # 6909 
nrow(conservation_unit_system_sites)  # 6909

nuseds_final <- base::merge(y = nuseds_long, 
                            x = conservation_unit_system_sites[,c(col_common,col_cuss)], 
                            by = col_common, 
                            all.y = T)

# Updates FULL_CU_IN:
full_cu_l <- update_for_FULL_CU_IN_l()

for(i in 1:length(full_cu_l)){
  # i <- 1
  FULL_CU_IN_here <- names(full_cu_l)[i]
  pop_ids <- full_cu_l[[i]]
  cond <- nuseds_final$POP_ID %in% pop_ids
  nuseds_final$FULL_CU_IN[cond] <- FULL_CU_IN_here
}

# rename fields
nuseds_final


col_old <- c("Y_LAT","X_LONG",)


#
# 

# 

# Check on INdexIds with mutliple FGE_IDs in NUSEDS NOT USED ANYMORE -----

#'* 3) CHECK: association between GFE_ID and IndexId in all_areas_nuseds *
#' It should be a ONE TO MANY relationship.
#' - For all_areas_nuseds: 
#'    - 1) check that for each IndexId there is an unique GFE_ID, if not, trouble
#'    shoot.
#'    - 2) check that there is not duplicated multiple data points for a same year,
#'    if that's the case, trouble shoot.
#' - 

IndexId_GFE_ID <- unique(all_areas_nuseds[,c("IndexId","GFE_ID")])
sum(duplicated(IndexId_GFE_ID$IndexId)) # 33 67 69 # was 79 --> not normal
sum(duplicated(IndexId_GFE_ID$GFE_ID)) # 4628 4708 4749 # was 9049 --> normal
IndexId_toCheck <- unique(IndexId_GFE_ID$IndexId[duplicated(IndexId_GFE_ID$IndexId)])
length(IndexId_toCheck) # 21 40 41 # was 46

IndexId_GFE_ID_dupli <- NULL
layout(matrix(1))
par(mar = c(4.5,4.5,.5,.5))
for(iid_i in 1:length(IndexId_toCheck)){
  # iid_i <- 6
  iid <- IndexId_toCheck[iid_i]
  
  # print(count <- count + 1)
  all_areas_nuseds_cut <- all_areas_nuseds[all_areas_nuseds$IndexId == iid,]
  
  #' find the % of points overlapping with the other time series in 
  #' all_areas_nuseds_cut (see application in for loop below)
  all_areas_nuseds_cut_noNA <- all_areas_nuseds_cut[!is.na(all_areas_nuseds_cut$MAX_ESTIMATE),] # common NAs are not considered as duplicates
  row_dupli <- duplicated(all_areas_nuseds_cut_noNA[,c("MAX_ESTIMATE","Year")])
  dupli <- all_areas_nuseds_cut_noNA[row_dupli,c("MAX_ESTIMATE","Year"),drop = F]
  
  # 1) check if there is a unique GFE_ID or not
  gfe_id <- unique(all_areas_nuseds_cut$GFE_ID)
  
  #' If there are more than one gfe_ID, look at the data. Possible cases:
  #' - there are multiple time series --> check in all_areas_nuseds to see if 
  #' there is a field that can help solving the issue. 
  #' - Only a few data points have been attributed the wrong GFE_ID or IndexId
  
  output <- data.frame(IndexId = iid,
                       GFE_ID = gfe_id,
                       nb_dataPt = NA,
                       nb_dataPt_overlap = NA,           # percentage of data points overlapping with others
                       nb_dataPt_overlapPercent = NA,
                       action = NA, 
                       iid_i = iid_i,
                       WATERBODY = NA,
                       SYSTEM_SITE = NA,
                       CU_NAME = NA,
                       CU_TYPE = NA,
                       method = NA,
                       comment = NA)
  
  yrs <- (min(all_areas_nuseds_cut$Year, na.rm = T)-10):max(all_areas_nuseds_cut$Year)
  
  # method
  output$method <- sapply(X = gfe_id, FUN = function(gfeid){
    # gfeid <- gfe_id[2]
    condition <- all_areas_nuseds_cut$GFE_ID == gfeid
    out <- unique(all_areas_nuseds_cut$ENUMERATION_METHODS[condition])
    out <- paste(out, collapse = " ; ")
    return(out)
  })
  
  # WATERBODY
  output$WATERBODY <- sapply(X = gfe_id, FUN = function(gfeid){
    # gfeid <- gfe_id[1]
    condition <- all_areas_nuseds_cut$GFE_ID == gfeid
    out <- unique(all_areas_nuseds_cut$WATERBODY[condition])
    if(length(out) > 1){
      print(paste("Multiple values for WATERBODY at iid_i = ",iid_i))
    }
    return(out)
  })
  
  # SYSTEM_SITE (in conservation_unit_system_sites)
  output$SYSTEM_SITE <- sapply(X = gfe_id, FUN = function(gfeid){
    # gfeid <- gfe_id[1]
    condition <- conservation_unit_system_sites$GFE_ID == gfeid & 
      conservation_unit_system_sites$IndexId == iid
    out <- unique(conservation_unit_system_sites$SYSTEM_SITE[condition])
    if(length(out) > 1){
      print(paste("Multiple values for SYSTEM_SITE at iid_i = ",iid_i))
    }else if(length(out) == 0){
      out <- NA
    }
    return(out)
  })
  
  #
  output$CU_NAME <- sapply(X = gfe_id, FUN = function(gfeid){
    # gfeid <- gfe_id[1]
    condition <- conservation_unit_system_sites$GFE_ID == gfeid & 
      conservation_unit_system_sites$IndexId == iid
    out <- unique(conservation_unit_system_sites$CU_NAME[condition])
    if(length(out) > 1){
      print(paste("Multiple values for CU_NAME at iid_i = ",iid_i))
    }else if(length(out) == 0){
      out <- NA
    }
    return(out)
  })
  
  output$CU_TYPE <- sapply(X = gfe_id, FUN = function(gfeid){
    # gfeid <- gfe_id[1]
    condition <- conservation_unit_system_sites$GFE_ID == gfeid & 
      conservation_unit_system_sites$IndexId == iid
    out <- unique(conservation_unit_system_sites$CU_TYPE[condition])
    return(out)
  })
  
  # comment 1: there is no MAX_ESTIMATE data at all
  comments_l <- list()
  c_i <- 1
  comments_l[[c_i]] <- rep("",length(gfe_id))
  names(comments_l)[c_i] <- "Data_any"
  
  # comment 2: either the IndexId is not present in conservation_unit_system_sites, 
  # or it is but it is associated with another(s) GFE_ID
  c_i <- c_i + 1
  comments_l[[c_i]] <- rep("",length(gfe_id))
  
  # if series are not in CUSS:
  if(all(is.na(output$SYSTEM_SITE))){
    condition <- conservation_unit_system_sites$IndexId == iid
    gfeId_cuss <- conservation_unit_system_sites$GFE_ID[condition]
    
    # 
    if(length(gfeId_cuss) == 0){
      comments_l[[c_i]] <- rep("IndexId not present in conservation_unit_system_sites",
                               length(gfe_id))
    }else{
      comments_l[[c_i]] <- rep(paste("Different GFE_ID in conservation_unit_system_sites:",gfeId_cuss),
                               length(gfe_id))
    }
  }
  names(comments_l)[c_i] <- "SYSTEM_SITE"
  
  # If there are only NAs for MAX_ESTIMATE
  if(sum(!is.na(all_areas_nuseds_cut$MAX_ESTIMATE)) == 0){
    
    # output$comment <- "only NAs for field MAX_ESTIMATE"
    comments_l$Data_any <- rep("No data for this population",length(gfe_id))
    
    output$nb_dataPt <- 0
    output$action <- "remove"
    
  }else{
    
    plot_IndexId_GFE_ID_fun(IndexIds = iid, all_areas_nuseds = all_areas_nuseds)
    legend("topright",paste("iid_i =",iid_i),bty = "n")
    legend("top",c("SYSTEM_SITE",output$SYSTEM_SITE), bty = 'n')
    
    for(gfeid in gfe_id){
      # gfeid <- gfe_id[1]
      
      dataHere <- all_areas_nuseds_cut[all_areas_nuseds_cut$GFE_ID == gfeid,]
      dataHere <- dataHere[order(dataHere$Year),c("MAX_ESTIMATE","WATERBODY","Year")]
      
      nb_dataPt <-  sum(!is.na(dataHere$MAX_ESTIMATE))
      output$nb_dataPt[output$GFE_ID == gfeid] <- nb_dataPt
      
      # if there is not MAX_ESTIMATE data:
      if(sum(!is.na(dataHere$MAX_ESTIMATE)) == 0){
        output$action[output$GFE_ID == gfeid] <- "remove"
        output$comment[output$GFE_ID == gfeid] <- "only NAs for field MAX_ESTIMATE"
        
        # if there is MAX_ESTIMATE data:
      }else{
        # output$action[output$GFE_ID == gfeid] <- T
        
        # check if there are duplicated years in this time series
        if(sum(duplicated(dataHere$Year)) > 0){
          print("There are duplicated year here:")
          yr_dupli <- dataHere$Year[duplicated(dataHere$Year)]
          print(dataHere[dataHere$Year %in% yr_dupli,])
        }
        
        # check if there are duplicated MAX_ESTIMATE - Year with the other time series
        # in all_areas_nuseds_cut:
        if(nrow(dupli) > 0){
          
          # are there data points in dupli
          merge <- merge(x = dataHere[,c("MAX_ESTIMATE","Year")],
                         y = dupli)
          nb_dupli <- nrow(merge)
          
          if(nb_dupli > 0){
            output$nb_dataPt_overlap[output$GFE_ID == gfeid] <- nb_dupli
            percent <- round(nb_dupli / nb_dataPt * 100,2)
            output$nb_dataPt_overlapPercent[output$GFE_ID == gfeid] <- percent
          }
          
        }else{
          output$nb_dataPt_overlap[output$GFE_ID == gfeid] <- 0
        }
      }
    } # end of for(gfeid in gfe_id)
  } # end of else if(there is no data)
  
  output$comment <- sapply(X = 1:nrow(output), FUN = function(r){
    # r <- 2
    commentsHere <- sapply(X = comments_l, FUN = function(c){
      return(c[r])
    })
    commentsHere <- commentsHere[commentsHere != ""]
    if(length(commentsHere) == 1){
      out <- commentsHere
    }else if(length(commentsHere) > 1){
      out <- paste(commentsHere, collapse = " ; ")
    }else{
      out <- ""
    }
    return(out)
  })
  
  if(is.null(rep(IndexId_GFE_ID_dupli,2))){
    IndexId_GFE_ID_dupli <- output
  }else{
    IndexId_GFE_ID_dupli <- rbind(IndexId_GFE_ID_dupli,output)
  }
}

# View(IndexId_GFE_ID_dupli)
head(IndexId_GFE_ID_dupli)
nrow(IndexId_GFE_ID_dupli) # 107

IndexId_GFE_ID_dupli[IndexId_GFE_ID_dupli$nb_dataPt_overlap > 0 & !is.na(IndexId_GFE_ID_dupli$nb_dataPt_overlap),]

IndexId_GFE_ID_dupli$iid_i[IndexId_GFE_ID_dupli$IndexId == "SX_45525"]

IndexId_GFE_ID_dupli[IndexId_GFE_ID_dupli$IndexId == "CM_47925",]
IndexId_GFE_ID_dupli[grepl("46602",IndexId_GFE_ID_dupli$IndexId),]
IndexId_GFE_ID_dupli[grepl("46582",IndexId_GFE_ID_dupli$IndexId),]

# write.csv(IndexId_GFE_ID_dupli,paste0(wd_output,"/IndexId_GFE_ID_missmatched.csv"),
#           row.names = F)

iid_i <- 2
IndexId_GFE_ID_dupli[IndexId_GFE_ID_dupli$iid_i == iid_i,]
plot_IndexId_GFE_ID_fun(IndexIds = IndexId_GFE_ID_dupli$IndexId[IndexId_GFE_ID_dupli$iid_i == iid_i][1],
                        all_areas_nuseds = all_areas_nuseds)
legend("topright",paste("iid_i =",iid_i),bty = "n")

plot_IndexId_GFE_ID_COMBO_fun(iid_i = 35, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

#'** Case 1: all/most the data points overlap with other time series **
condition <- IndexId_GFE_ID_dupli$nb_dataPt_overlap > 0 &
  !is.na(IndexId_GFE_ID_dupli$nb_dataPt_overlap)
unique(IndexId_GFE_ID_dupli$iid_i[condition])
#' iid_i: 28, 30, and also 35 (5, 13, 16 are not clear overlaps)
#' Check the series kept is in CUSS

#' - looking at iid_i 28 (related to 30):
#'    - 3rd plot: the two blue data points with IndexId = CN_46891 & GFE_ID = 2466 
#'      overlap with the red time series of IndexId = CN_46892 & GFE_ID = 2466 
#'      TODO: remove the two data points.
plot_IndexId_GFE_ID_COMBO_fun(iid_i = 28, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

removed <- data.frame(IndexId = c("CN_46891"),
                      GFE_ID = 2466)
removed$dataset <- "all_areas_nuseds"
removed$comment <- "The two points are duplicates of series IndexId = CN_46892 & GFE_ID = 2466"
removed_all <- rbind(removed_all,removed)

condition <- all_areas_nuseds$IndexId == removed$IndexId &
  all_areas_nuseds$GFE_ID == removed$GFE_ID
colNuSEDS <- c("SPECIES","IndexId","GFE_ID","WATERBODY","Year","MAX_ESTIMATE")
all_areas_nuseds[condition,colNuSEDS]

all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                           toRemove = removed, 
                                           fields = c("IndexId","GFE_ID"))
nrow(all_areas_nuseds) # 309643

#' - looking at iid_i 28 (related to 30): continue:
#'   - bottom plot: the blue series (IndexId = CN_46891 & GFE_ID = 2467) overlaps 
#'     with the red one (IndexId = CN_46891 & GFE_ID = 290)
#'     TODO: remove the blue time series
plot_IndexId_GFE_ID_COMBO_fun(iid_i = 28, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

removed <- data.frame(IndexId = c("CN_46891"),
                      GFE_ID = 2467)
removed$dataset <- "all_areas_nuseds"
removed$comment <- "the 5 points are duplicates of series IndexId = CN_46891 & GFE_ID = 290"
removed_all <- rbind(removed_all,removed)

condition <- all_areas_nuseds$IndexId == removed$IndexId &
  all_areas_nuseds$GFE_ID == removed$GFE_ID
all_areas_nuseds[condition,colNuSEDS]

all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                           toRemove = removed, 
                                           fields = c("IndexId","GFE_ID"))
nrow(all_areas_nuseds) # 309638

#' - looking at iid_i 30 (related to 28): 
#'    - bottom plot: the blue data points (IndexId = CN_46892 & GFE_ID = 290)  
#'      overlaps with the red time series (IndexId = CN_46892 & GFE_ID = 2466), 
#'      except for the 5th point, which overlaps with the red series in the plot 
#'      above (IndexId = CN_46891 & GFE_ID = 290)
#'      TODO: 
#'        - in blue series in the bottom plot: add the 3rd and 4th point to 
#'          the red series (IndexId = CN_46892 & GFE_ID = 2466), discard the other
#'          points.
plot_IndexId_GFE_ID_COMBO_fun(iid_i = 30,
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

condition_CN_46892_290 <- all_areas_nuseds$IndexId == "CN_46892" & 
  all_areas_nuseds$GFE_ID == 290
condition_CN_46892_2466 <- all_areas_nuseds$IndexId == "CN_46892" & 
  all_areas_nuseds$GFE_ID == 2466
condition_CN_46891_290 <- all_areas_nuseds$IndexId == "CN_46891" & 
  all_areas_nuseds$GFE_ID == 290

# the blue time series in the bottom plot:
data <- all_areas_nuseds[condition_CN_46892_290,colNuSEDS]
data$action <- NA

# Remove data points overlapping with the red time series in bottom plot or being NAs
data$action <- sapply(X = 1:nrow(data), FUN = function(r){
  # r <- 9
  yr_here <- data$Year[r]
  maxEstim_here <- data$MAX_ESTIMATE[r]
  
  if(is.na(maxEstim_here)){
    out <- "remove"
  }else{
    # look if value is in red time series bottom plot:
    notIn_CN_46892_2466 <- all_areas_nuseds[condition_CN_46892_2466,]$Year == yr_here &
      all_areas_nuseds[condition_CN_46892_2466,]$MAX_ESTIMATE == maxEstim_here &
      !is.na(all_areas_nuseds[condition_CN_46892_2466,]$MAX_ESTIMATE)
    
    notIn_CN_46891_290 <- all_areas_nuseds[condition_CN_46891_290,]$Year == yr_here &
      all_areas_nuseds[condition_CN_46891_290,]$MAX_ESTIMATE == maxEstim_here &
      !is.na(all_areas_nuseds[condition_CN_46891_290,]$MAX_ESTIMATE)
    
    if(any(notIn_CN_46892_2466)){ # if overlap with red time series in bottom plot
      out <- "remove"
    }else if(any(notIn_CN_46891_290)){ # if it overlaps with red time series in 2nd plot
      out <- "remove"
    }else{
      out <- "add"
    }
  }
  return(out)
})

#' Add the two data points from series IndexId = CN_46892 & GFE_ID = 290 to 
#' series IndexId = CN_46892 & GFE_ID = 2466
data_add <- data[data$action == "add",]
for(r in 1:nrow(data_add)){
  # r <- 1
  nuseds_CN_46892_290 <- all_areas_nuseds[condition_CN_46892_290,]
  condition <- nuseds_CN_46892_290$Year == data_add$Year[r]
  row_new <- nuseds_CN_46892_290[condition,]
  row_new$GFE_ID <- 2466
  row_new$WATERBODY <- unique(all_areas_nuseds$WATERBODY[condition_CN_46892_2466])
  all_areas_nuseds <- rbind(all_areas_nuseds,row_new)
}

nrow(all_areas_nuseds) # 

# Now remove the series IndexId = CN_46892 & GFE_ID = 290
removed <- data.frame(IndexId = c("CN_46892"),
                      GFE_ID = 290)
removed$dataset <- "all_areas_nuseds"
removed$comment <- "3 data points are duplicates of series IndexId = CN_46892 & GFE_ID = 2466, two data points were added to the latter, remaining one is duplicate of series IndexId = CN_46891 & GFE_ID = 290"
removed_all <- rbind(removed_all,removed)

condition <- all_areas_nuseds$IndexId == removed$IndexId &
  all_areas_nuseds$GFE_ID == removed$GFE_ID
all_areas_nuseds[condition,colNuSEDS]

all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                           toRemove = removed, 
                                           fields = c("IndexId","GFE_ID"))
nrow(all_areas_nuseds) # 309631

#' - looking at iid_i 35: 
#'    - the point in IndexId = CN_46801 & GFE_ID = 33103 is a duplicate of series
#'    series IndexId = CN_46801 & GFE_ID = 281
#'    TODO: remove the point
plot_IndexId_GFE_ID_COMBO_fun(iid_i = 35, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

IndexId_GFE_ID_dupli$iid_i[IndexId_GFE_ID_dupli$IndexId == "CN_46801"]

removed <- data.frame(IndexId = c("CN_46801"),
                      GFE_ID = 33103)
removed$dataset <- "all_areas_nuseds"
removed$comment <- "the point is a duplicate of series IndexId = CN_46801 & GFE_ID = 281"
removed_all <- rbind(removed_all,removed)

condition <- all_areas_nuseds$IndexId == removed$IndexId &
  all_areas_nuseds$GFE_ID == removed$GFE_ID
all_areas_nuseds[condition,colNuSEDS]

all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                           toRemove = removed, 
                                           fields = c("IndexId","GFE_ID"))
nrow(all_areas_nuseds) # 309630 310523

#'** Case 2: WATERBODY != SYSTEM_SITE match ** 
#' 39, 40
#' - for 39: 
#'    TODO: we simply remove the one blue data point in bottom plot (is is not in
#'    cuss)
cond <- IndexId_GFE_ID_dupli$WATERBODY != IndexId_GFE_ID_dupli$SYSTEM_SITE &
  !is.na(IndexId_GFE_ID_dupli$WATERBODY) & 
  !is.na(IndexId_GFE_ID_dupli$SYSTEM_SITE)
IndexId_GFE_ID_dupli[cond,]

plot_IndexId_GFE_ID_COMBO_fun(iid_i = 39, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

# check if the series is in cuss --> it is not
cond <- conservation_unit_system_sites$IndexId == "CM_47907" &
  conservation_unit_system_sites$GFE_ID == 5
conservation_unit_system_sites[cond,1:5]

removed <- data.frame(IndexId = c("CM_47907"),
                      GFE_ID = 5)
removed$dataset <- "all_areas_nuseds"
removed$comment <- "There is only one data point and the series is not in CUSS ; close to series IndexId = CM_47907 & GFE_ID = 7"
removed_all <- rbind(removed_all,removed)

condition <- all_areas_nuseds$IndexId == removed$IndexId &
  all_areas_nuseds$GFE_ID == removed$GFE_ID
all_areas_nuseds[condition,colNuSEDS]

all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                           toRemove = removed, 
                                           fields = c("IndexId","GFE_ID"))
nrow(all_areas_nuseds) # 309629 310522

#' - for 40: the blue data point in the bottom panel (IndexId = SX_45182 & GFE_ID = 1327)
#'   seems to belong to the red series in middle plot (IndexId = SX_52310 & GFE_ID = 1327)
#'   Additionally, the series (IndexId = SX_45182 & GFE_ID = 1327) but both 
#'   (IndexId = SX_45182 & GFE_ID = 7990630) and (IndexId = SX_52310 & GFE_ID = 1327) are. 
#'   TODO: move the blue dot to series IndexId = SX_45182 & GFE_ID = 7990630 because
#'   (i) it is the same species and (ii) the series exists in CUSS.
plot_IndexId_GFE_ID_COMBO_fun(iid_i = 40,
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

cond <- conservation_unit_system_sites$IndexId == "SX_45182"
conservation_unit_system_sites$GFE_ID[cond]
# --> IndexId = SX_45182 & GFE_ID = 1327 is not in CUSS
# --> IndexId = SX_45182 & GFE_ID = 7990630 is in CUSS

cond <- conservation_unit_system_sites$GFE_ID == 1327
conservation_unit_system_sites$IndexId[cond]
# --> IndexId = SX_52310 & GFE_ID = 1327 is in CUSS

removed <- data.frame(IndexId = c("SX_45182"),
                      GFE_ID = 1327)
removed$dataset <- "all_areas_nuseds"
removed$comment <- "The one data point moved to series IndexId = SX_45182 & GFE_ID = 7990630"
removed_all <- rbind(removed_all,removed)

condition_SX_45182_1327 <- all_areas_nuseds$IndexId == "SX_45182" &
  all_areas_nuseds$GFE_ID == 1327
all_areas_nuseds[condition_SX_45182_1327,]

# Get SYSTEM_SITE for GFE_ID = 7990630
cond <- conservation_unit_system_sites$IndexId == "SX_45182" & 
  conservation_unit_system_sites$GFE_ID == 7990630
SYSTEM_SITE <- conservation_unit_system_sites$SYSTEM_SITE[cond] # "BEDWELL SYSTEM"

# all_areas_nuseds[condition_SX_45182_1327,]$IndexId <- "SX_52310"
all_areas_nuseds[condition_SX_45182_1327,]$GFE_ID <- 7990630
all_areas_nuseds[condition_SX_45182_1327,]$WATERBODY <- SYSTEM_SITE

#'** Case 3: there is only one SYSTEM_SITE matching the WATERBODIES ** 
#' 1, 2, 4, 8, 11, 13, 14, 16, 17, 20, 21, 23, 24, 25, 27, 31, 32, 
#' with only one or two data points for certain series: 7, 9, 12, 15, 22, 37, 39, 41
#' with clear continuity: 3, 5, 10, 18, 26, 35
#' to show: 1, 5, 23
#' TODO: nothing for now
cond <- is.na(IndexId_GFE_ID_dupli$SYSTEM_SITE)
unique(IndexId_GFE_ID_dupli$iid_i[cond])
plot_IndexId_GFE_ID_COMBO_fun(iid_i = 23, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

#'** Case 4: the population is not present in conservation_unit_system_sites **
#' 19, 29
#' TODO: remove? --> YES, spillover from a CU?...
IndexId_GFE_ID_dupli[grepl("IndexId not present in conservation_unit_system_sites",
                           IndexId_GFE_ID_dupli$comment),]
plot_IndexId_GFE_ID_COMBO_fun(iid_i = 19, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

# OLD fixes not implemented for Case 4 ------
plot_IndexId_GFE_ID_COMBO_fun(iid_i = 37, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

#' Check in the cases where there are only a few data points:
#' - for 7: this data point is not a duplicate. We could either remove it or add 
#'   it to the other time series.
#'   TODO: add it to other series (IndexId = CN_48448 & GFE_ID = 443)
condition <- IndexId_GFE_ID_dupli$iid_i == 7 & is.na(IndexId_GFE_ID_dupli$SYSTEM_SITE)
is_MAX_ESTIMATE_duplicate_fun(IndexId = IndexId_GFE_ID_dupli$IndexId[condition], 
                              GFE_ID = GFE_ID <- IndexId_GFE_ID_dupli$GFE_ID[condition], 
                              all_areas_nuseds = all_areas_nuseds)

removed <- data.frame(IndexId = c("CN_48448"),
                       GFE_ID = c("19723"))
removed$comment <- "the data point was added to series IndexId = CN_48448 & GFE_ID = 443"
removed_all <- rbind(removed_all,removed)

condition <- all_areas_nuseds$IndexId == removed$IndexId &
  all_areas_nuseds$GFE_ID == removed$GFE_ID
all_areas_nuseds[condition,colNuSEDS]

all_areas_nuseds$GFE_ID[condition] <- 443

#' - for 9:
#'    - in top panel the two blue points (IndexId = CN_50619 & GFE_ID = 824) are
#'      duplicates of the red time series (IndexId = CN_50618 & GFE_ID = 824).
#'      The series exist in conservation_unit_system_sites with the same CU describers
#'      TODO: remove the blue points from both all_areas_nuseds and 

removed <- data.frame(IndexId = c("CN_50619"),
                       GFE_ID = c("824"))
removed$comment <- "the two points are (close) duplicates of series IndexId = CN_50618 & GFE_ID = 824"
removed_all <- rbind(removed_all,removed)

condition1 <- conservation_unit_system_sites$IndexId == "CN_50619" &
  conservation_unit_system_sites$GFE_ID == removed$GFE_ID
condition2 <- conservation_unit_system_sites$IndexId == "CN_50618" &
  conservation_unit_system_sites$GFE_ID == removed$GFE_ID
rbind(conservation_unit_system_sites[condition1,],
      conservation_unit_system_sites[condition2,])

condition <- all_areas_nuseds$IndexId == removed$IndexId &
  all_areas_nuseds$GFE_ID == removed$GFE_ID
all_areas_nuseds[condition,colNuSEDS]

condition <- all_areas_nuseds$IndexId =="CN_50618" &
  all_areas_nuseds$GFE_ID == removed$GFE_ID &
  all_areas_nuseds$Year %in% c(1996,2001)
all_areas_nuseds[condition,colNuSEDS]

overlap_two_timeSeries_fun(IndexIds = c("CN_50618","CN_50619"),
                           GFE_IDs = 824, 
                           all_areas_nuseds = all_areas_nuseds)

all_areas_nuseds <- remove_rows_fields_fun(dataframe = all_areas_nuseds, 
                                           removed = removed)
nrow(all_areas_nuseds) # 412345

conservation_unit_system_sites <- remove_rows_fields_fun(dataframe = conservation_unit_system_sites, 
                                                         removed = removed)
nrow(conservation_unit_system_sites) # 7143

#'    - in the 3rd panel: the red point (IndexId = CN_50618 & GFE_ID = 446068199)
#'      has the WATERBODY = "Clearwater Creek", and it is CLEARWATER CREEK in series
#'      second panel (IndexId = CN_50618 & GFE_ID = 446068199)
#'      TODO: move the one point to the other series

removed <- data.frame(IndexId = c("CN_50618"),
                       GFE_ID = c("446068199"))
removed$comment <- "the one point was moved to series IndexId = CN_50618 & GFE_ID = 2618"
removed_all <- rbind(removed_all,removed)

condition <- all_areas_nuseds$IndexId == removed$IndexId &
  all_areas_nuseds$GFE_ID == removed$GFE_ID
all_areas_nuseds[condition,colNuSEDS]

all_areas_nuseds$GFE_ID[condition] <- 2618

#' - for 12:
#'    - in bottom plot: the blue series (IndexId = CN_3333 & GFE_ID = 1194) does not
#'      exist in conservation_unit_system_sites, but the red series in top panel
#'      does
#'      TODO: place the two blue points in series IndexId = CN_3333 & GFE_ID = 23735
conservation_unit_system_sites[conservation_unit_system_sites$SYSTEM_SITE == "NANAIMO RIVER",]
removed <- data.frame(IndexId = c("CN_3333"),
                       GFE_ID = c("1194"))
removed$comment <- "the two points were moved to series IndexId = CN_3333 & GFE_ID = 23735"
removed_all <- rbind(removed_all,removed)

condition <- all_areas_nuseds$IndexId == removed$IndexId &
  all_areas_nuseds$GFE_ID == removed$GFE_ID
all_areas_nuseds[condition,colNuSEDS]

all_areas_nuseds$GFE_ID[condition] <- 23735

NOTE WATERBODY MUST BE UPDATED TOO !!!

#' - for 37:
#'    - in bottom plot: the blue point (IndexId = PKO_51094 & GFE_ID = 489440637)
#'      seem to be part of the other (red) series (IndexId = PKO_51094 & GFE_ID = 872).
#'      GFE_ID = 489440637 is not in conservation_unit_system_sites.
#'      TODO: move the blue point in other time series:
unique(conservation_unit_system_sites[conservation_unit_system_sites$GFE_ID == 489440637,]$SYSTEM_SITE)
removed <- data.frame(IndexId = c("PKO_51094"),
                       GFE_ID = c("489440637"))
removed$comment <- "the one point was moved to series IndexId = PKO_51094 & GFE_ID = 872"
removed_all <- rbind(removed_all,removed)

condition <- all_areas_nuseds$IndexId == removed$IndexId &
  all_areas_nuseds$GFE_ID == removed$GFE_ID
all_areas_nuseds[condition,colNuSEDS]

all_areas_nuseds$GFE_ID[condition] <- 872

#' - for 39:

BRUNO IS HERE

unique(conservation_unit_system_sites[conservation_unit_system_sites$GFE_ID == 2518,]$SYSTEM_SITE)


#' Case 4: WATERBODY and SYSTEM_SITE match
#' 6
#' TODO: delete or combine the data? --> SUM?
plot_IndexId_GFE_ID_COMBO_fun(iid_i = 19, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

condition <- IndexId_GFE_ID_dupli$iid_i == 6
overlap_two_timeSeries_fun(IndexIds = unique(IndexId_GFE_ID_dupli$IndexId[condition]),
                           GFE_IDs = unique(IndexId_GFE_ID_dupli$GFE_ID[condition]),
                           all_areas_nuseds = all_areas_nuseds)

IndexId_GFE_ID_dupli[IndexId_GFE_ID_dupli$IndexId == "SX_45524",]







#' Case 7: IndexId is present in conservation_unit_system_sites but under a 
#' different GFE_ID
#' NA
IndexId_GFE_ID_dupli[grepl("ifferent GFE_ID in conservation_unit_system_sites",
                           IndexId_GFE_ID_dupli$comment),]





#' See if I can figure out how to spot IndexIds typos: a same population was given
#' the wrong IndexId, which would explain why certain IndexIds have multiple 
#' GFE_ID:

iid_i <- 6
plot_IndexId_GFE_ID_COMBO_fun(iid_i = iid_i, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

d <- all_areas_nuseds[all_areas_nuseds$IndexId == "SX_45525",]

conservation_unit_system_sites[conservation_unit_system_sites$IndexId == "SX_45524",]



#' Comments:
#' - top panel: looks like it could be the 1st part of the series for SX_3302
#' - duplicate time series for SX_3302 --> one of them if wrong
#' SOLUTION?:
#' - SX_3302 in GFE_ID 3444 is in fact SX_3325?
#' - SX_3302 in GFE_ID 3416 is in fact SX_3310?

iid_i <- 36
plot_IndexId_GFE_ID_COMBO_fun(iid_i = iid_i, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)

iid_i <- 23
plot_IndexId_GFE_ID_COMBO_fun(iid_i = iid_i, 
                              IndexId_GFE_ID_dupli = IndexId_GFE_ID_dupli, 
                              all_areas_nuseds = all_areas_nuseds)





# check WATERBODY - GFE_ID match:
d <- unique(all_areas_nuseds[,c("AREA","GFE_ID","WATERBODY")])
GFE_ID_dupli <- d$GFE_ID[duplicated(d$GFE_ID)]
GFE_ID_dupli

WATERBODY_dupli <- d$WATERBODY[duplicated(d$WATERBODY)]
WATERBODY_dupli

dd <- d[d$WATERBODY %in% WATERBODY_dupli,]
dd[order(dd$WATERBODY),]

all_areas_nuseds$AREA


fields_def$all_areas_nuseds


#' Check if the IndexId, GFE_ID and the combination between the two is 
#' present in conservation_unit_system_sites:
IndexId_GFE_ID_dupli$IndexId_in_cuss <- F
IndexId_GFE_ID_dupli$GFE_ID_in_cuss <- F
IndexId_GFE_ID_dupli$IndexId_GFE_ID_in_cuss <- F

condition <- IndexId_GFE_ID_dupli$IndexId %in% conservation_unit_system_sites$IndexId
IndexId_GFE_ID_dupli$IndexId_in_cuss[condition] <- T

condition <- IndexId_GFE_ID_dupli$GFE_ID %in% conservation_unit_system_sites$GFE_ID
IndexId_GFE_ID_dupli$GFE_ID_in_cuss[condition] <- T

condition <- sapply(X = 1:nrow(IndexId_GFE_ID_dupli), FUN = function(r){
                     # r <- 1
                     merge <- merge(x = IndexId_GFE_ID_dupli[r,c("IndexId","GFE_ID")],
                                    y = conservation_unit_system_sites[,c("IndexId","GFE_ID")])
                     out <- nrow(merge) == 1
                   })
IndexId_GFE_ID_dupli$GFE_ID_in_cuss[condition] <- T


View(IndexId_GFE_ID_dupli)

hist(all_areas_nuseds$GFE_ID)
hist(IndexId_GFE_ID_dupli$GFE_ID)

iid_i <- 7
iid <- unique(IndexId_GFE_ID_dupli$IndexId[IndexId_GFE_ID_dupli$iid_i == iid_i])
gfeid <- IndexId_GFE_ID_dupli$GFE_ID[IndexId_GFE_ID_dupli$iid_i == iid_i]
dataCheck <- lapply(X = gfeid, FUN = function(gfeid){
  out <- all_areas_nuseds[all_areas_nuseds$IndexId == iid & 
                            all_areas_nuseds$GFE_ID == gfeid,]
  return(out)
})
dataCheck <- do.call(rbind,dataCheck)
View(dataCheck)


d1 <- all_areas_nuseds[]

fields_def$all_areas_nuseds$GFE_ID

duplicated_pop_nused

duplicated_pop_nused_df <- NULL
for(iid in duplicated_pop_nused){
  # iid <- duplicated_pop_nused[3]
  all_areas_nuseds_cut <- all_areas_nuseds[all_areas_nuseds$IndexId == iid,]
  colSelect <- c("IndexId","WATERBODY","GFE_ID")
  data_unique <- unique(all_areas_nuseds_cut[,colSelect])
  
  # case where is is duplicated dates but one unique location:
  if(nrow(data_unique) == 1){
    
    yr_duplicated <- all_areas_nuseds_cut$Year[duplicated(all_areas_nuseds_cut$Year)]
    
    print("Only location but duplicated dates:")
    print(all_areas_nuseds_cut[all_areas_nuseds_cut$Year %in% yr_duplicated,])
    
    data_unique$year_start <- min(all_areas_nuseds_cut$Year)
    data_unique$year_end <- max(all_areas_nuseds_cut$Year)
    
  }else{ # case where there are multiple locations for a single IndexId
    
    year_range_m <- sapply(X = data_unique$GFE_ID, FUN = function(gfe){
      # gfe <- data_unique$GFE_ID[1]
      dataHere <- all_areas_nuseds_cut[all_areas_nuseds_cut$GFE_ID == gfe,]
      years <- dataHere$Year
      yr_range <- range(years,na.rm = T)
      if(sum(duplicated(years)) > 0){
        print(paste("Duplicated years for pop:",iid,", GFE_ID:",gfe))
      }
      return(yr_range)
    })
    data_unique$year_start <- year_range_m[1,]
    data_unique$year_end <- year_range_m[2,]
  }
  
  if(is.null(duplicated_pop_nused_df)){
    duplicated_pop_nused_df <- data_unique
  }else{
    duplicated_pop_nused_df <- rbind(duplicated_pop_nused_df,data_unique)
  }
  
  # print(unique(all_areas_nuseds_cut[condition,colSelect]))
}

#
#' ** CHECK: NOT USED association between POPULATION and IndexId in all_areas_nuseds **
#' NOT NEEDED PER SE BECAUSE POPULATION is not to be used as it is not a reliable
#' field.
#' It should be a ONE TO ONE relationship, except for Pink were a same 
#' POPULATION can be assocated to two Pink one, one Even, one Odd.
IndexId_POPULATION <- unique(all_areas_nuseds[,c("IndexId","POPULATION")])
sum(duplicated(IndexId_POPULATION$IndexId))    # 0
sum(duplicated(IndexId_POPULATION$POPULATION)) # 1764
duplicated_Pop <- IndexId_POPULATION$POPULATION[duplicated(IndexId_POPULATION$POPULATION)]
#' Many of these correspond to Pink Odd vs. Even population, which is normal. 

#' Check if for the Pink, it is systematically a one Even and one Odd, flag if not.
IndexId_POPULATION_Pink <- IndexId_POPULATION[grepl("[P|p]ink",IndexId_POPULATION$POPULATION),]
duplicated_Pop_Pink <- duplicated_Pop[grepl("[P|p]ink",duplicated_Pop)]
for(pop in duplicated_Pop_Pink){
  dataHere <- IndexId_POPULATION_Pink[IndexId_POPULATION_Pink$POPULATION == pop,]
  
  PKO <- any(sapply(X = 1:nrow(dataHere),
                    FUN = function(r){grepl("PKO",dataHere[r,]$IndexId)}))
  PKE <- any(sapply(X = 1:nrow(dataHere),
                    FUN = function(r){grepl("PKE",dataHere[r,]$IndexId)}))
  
  if(!(PKO & PKE)){
    print(dataHere)
  }
}
# All good.

#' Check if there are non-pink populations:
duplicated_Pop <- duplicated_Pop[!grepl("[P|p]ink",duplicated_Pop)]
IndexId_POP_dupli <- IndexId_POPULATION[IndexId_POPULATION$POPULATION %in% duplicated_Pop,]
IndexId_POP_dupli <- IndexId_POP_dupli[order(IndexId_POP_dupli$POPULATION),]
IndexId_POP_dupli

#' For each POPULATION, plot the population dynamics of the different IndexId.
#' In each case, retain the population that has the longest time series and 
#' remove the other one (we can see that there is barely any data for them).
IndexIdToRremove <- c()
layout(matrix(1:length(unique(IndexId_POP_dupli$POPULATION)),ncol = 2))
par(mar = c(4.5,4.5,.5,.5))
for(pop in unique(IndexId_POP_dupli$POPULATION)){
  # pop <- unique(IndexId_POP_dupli$POPULATION)[1]
  nusedsHere <- all_areas_nuseds[all_areas_nuseds$POPULATION == pop,]
  IndexIdHere <- unique(nusedsHere$IndexId)
  yr_min <- min(nusedsHere$Year)
  yr_max <- max(nusedsHere$Year)
  yrs <- yr_min:yr_max
  pop_max <- max(nusedsHere$Returns, na.rm = T)
  
  plot(NA, xlim = c(yr_min,yr_max), ylim = c(0,pop_max), 
       ylab = "Returns", xlab = "Years", main = "")
  series <- list()
  count <- 1
  for(iid in IndexIdHere){
    s_yr <- nusedsHere[nusedsHere$IndexId == iid, c("Year","Returns")]
    if(length(yrs[!yrs %in% s_yr$Year]) > 0){
      s_yr_add <- data.frame(Year = yrs[!yrs %in% s_yr$Year],
                             Returns = NA)
      s_yr <- rbind(s_yr,s_yr_add)
    }
    s_yr <- s_yr[order(s_yr$Year),]
    series[[count]] <- s_yr
    count <- count + 1
  }
  cols <- viridis(n = length(IndexIdHere))
  for(i in 1:length(series)){
    lines(x = yrs, y = series[[i]]$Returns, lty = 1, lwd = 3, col = cols[i])
  }
  legend("topright",IndexIdHere, col = cols, bty = "n", lwd = 3)
  legend("topleft",pop, bty = "n")
  
  # select the population to remove as a function of the length of the data
  dataPtsNb <- sapply(X = series, function(s){sum(!is.na(s$Returns))})
  toKeep <- dataPtsNb == max(dataPtsNb)
  IndexIdToRremove <- c(IndexIdToRremove,
                        IndexIdHere[!toKeep])
}

# Add CM_40836 because the data is insufficient --> DO NOT DO IT
IndexIdToRremove <- c(IndexIdToRremove,"CM_40836")




# Check 



sum(is.na(all_areas_nuseds$WATERBODY))

# looks like all_areas_nuseds$WATERBODY == conservation_unit_system_sites$SYSTEM_SITE
sites_SYSTEM_SITE <- unique(conservation_unit_system_sites$SYSTEM_SITE)
nused_WATERBODY <- unique(all_areas_nuseds$WATERBODY)
sum(nused_WATERBODY %in% sites_SYSTEM_SITE) / length(nused_WATERBODY)
sum(sites_SYSTEM_SITE %in% nused_WATERBODY) / length(sites_SYSTEM_SITE)

# For those site/waterbody that don't match, is it because of populations are not
# present in either dataset?
SYSTEM_SITE_notIn_WATERBODY <- sites_SYSTEM_SITE[! sites_SYSTEM_SITE %in% nused_WATERBODY]
WATERBODY_notIn_SYSTEM_SITE <- nused_WATERBODY[! nused_WATERBODY %in% sites_SYSTEM_SITE]
sort(SYSTEM_SITE_notIn_WATERBODY)
sort(WATERBODY_notIn_SYSTEM_SITE)

SYSTEM_SITE_notIn_WATERBODY_IndexId <- sapply(X = SYSTEM_SITE_notIn_WATERBODY, 
                                              FUN = function(ss){
                                                dataset <- conservation_unit_system_sites[conservation_unit_system_sites$SYSTEM_SITE == ss,]
                                                out <- unique(dataset$IndexId)
                                              })

WATERBODY_notIn_SYSTEM_SITE_IndexId <- sapply(X = WATERBODY_notIn_SYSTEM_SITE, 
                                              FUN = function(wb){
                                                dataset <- escapement[all_areas_nuseds$WATERBODY == wb,] # rows match in escapement and all_areas_nuseds 
                                                out <- unique(dataset$IndexId)
                                              })

#' Look for each of the populations associated with these miss-matched site
#' if (1) the it is present in the other dataset and if that's the case (2) find 
#' the name of the siwze to see if there is an typo error or not.

unlist(SYSTEM_SITE_notIn_WATERBODY_IndexId)

sites_missmatches <- data.frame(IndexId = )

for(pops_i in 1:length(SYSTEM_SITE_notIn_WATERBODY_IndexId)){
  #' pops_i <- 1
  pops <- SYSTEM_SITE_notIn_WATERBODY_IndexId[[pops_i]]
  syst_site_here <- names(SYSTEM_SITE_notIn_WATERBODY_IndexId)[pops_i]
  for(pop in pops){
    # pop <- pops[1]
    sapply(X = , FUN = function(){
      
    })
    
    
    
  }
  
  print(pops)
  
}




conservation_unit_system_sites$SPECIES

Check NuSEDS All Areas to see what the unique POP_ID is for the SYSTEM_SITE and Species combo. Seems likely that this is an error in the conservation_unit_system_sites.



# Check which population match or not between the two datasets:
IndexId_nused <- unique(escapement$IndexId)
length(IndexId_nused) # 11474
IndexId_sites <- unique(conservation_unit_system_sites$IndexId)
length(IndexId_sites) # 7143





View(all_areas_nuseds[escapement$SPECIES_QUALIFIED == "SE",])

conservation_unit_system_sites$SPECIES_QUALIFIED
conservation_unit_system_sites$POP_ID
conservation_unit_system_sites$SPECIES_QUALIFIED


escapement$IndexId <- paste(escapement$sp, PopId, sep="_")
StatArea <- Convert2StatArea(AREA)                      # AREA is the subdistrict; # QUESTION: why doing this?

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
removed <- escapement$IndexId == "CN_47367" & escapement$AREA == "29I"
escapement <- escapement[!removed,]
removed <- esc.summary$IndexId == "CN_47367" & esc.summary$StatArea == "29I"
esc.summary <- esc.summary[!removed,]

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
  colNo <- c("FWA_WATERSHED_CDE","WATERSHED_CDE","EFFECTIVE_DT","CMNTS","PopId","SpeciesId",
             "nrecs","nnumest","nins","npres","pinsrec","ppres_ins","pest_pres",
             "Escapement.avg","Escapement.sd","Escapement.se","Year.start","Year.end",
             "CU_TYPE","CU_INDEX","FULL_CU_IN","SBJ_ID","IS_INDICATOR","SITE_ID",
             "CU","Surveyed","NoEscapement","Active","SPECIES_QUALIFIED",
             "FAZ_ACRO","MAZ_ACRO","JAZ_ACRO","CU_NAME","CU_ACRO")
  colYes <- colnames(stream.list)[! colnames(stream.list) %in% colNo]
  print(out[,colYes])
  print("***")
}

conservation_unit_system_sites[conservation_unit_system_sites$POP_ID == 7479,]$SYSTEM_SITE

all_areas_nuseds
c("SPECIES","POP_ID")

conservation_unit_system_sites$SpeciesId

#' SP: Check NuSEDS All Areas to see what the unique POP_ID is for the SYSTEM_SITE
#' and Species combo. Seems likely that this is an error in the conservation_unit_system_sites.

conservation_unit_system_sites[conservation_unit_system_sites$POP_ID == 45525,]$SYSTEM_SITE



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

colnames(all_areas_nuseds)[grepl(pattern = "ENH",colnames(all_areas_nuseds))]
colnames(all_areas_nuseds)[grepl(pattern = "enh",colnames(all_areas_nuseds))]
colnames(conservation_unit_system_sites)[grepl(pattern = "ENH",colnames(conservation_unit_system_sites))]
colnames(conservation_unit_system_sites)[grepl(pattern = "enh",colnames(conservation_unit_system_sites))]


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
                     SpeciesId2 = rep(NA, length))  # BSC: ?! there is no SpeciesId2 field in template_df

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
# setwd(dir.out)

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

date <- Sys.time()
date <- substr(x = date,start = 1, stop = 10)
date <- gsub("-","",date)
# write.csv(final, "NuSEDS_escapement_data_collated_20230818.csv", row.names=FALSE)
write.csv(final,paste0(wd_output,"/NuSEDS_escapement_data_collated_",date,".csv"),
                       row.names = FALSE)

# Notes:
# Remove CU's which are "BIN" at this stage? 










