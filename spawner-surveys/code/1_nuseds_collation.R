

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

# Loading packages & functions 
library(tidyr)
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

source("code/functions.R")

options(digits = 9)

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

nrow(unique(conservation_unit_system_sites[,c("SPECIES_QUALIFIED","POP_ID","SYSTEM_SITE")])) # 7145
nrow(unique(conservation_unit_system_sites[,c("SPECIES_QUALIFIED","POP_ID","SYSTEM_SITE","GFE_ID")])) # 7145

conservation_unit_system_sites$GFE_ID

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

#
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
# Fixes on all_areas_nuseds and conservation_unit_system_sites ---------

all_areas_nuseds_all <- all_areas_nuseds
nrow(all_areas_nuseds_all) # 412492

conservation_unit_system_sites_all <- conservation_unit_system_sites
nrow(conservation_unit_system_sites_all) # 7145

#'* Fix: coordinates of locations conservation_unit_system_sites *
#' There are multiple locations in conservation_unit_system_sites that have 
#' different names and GFE_ID but exactly the same geo coordinates. 
#' This causes an issue in the next data processing process when trying to 
#' provide streamid for the PSE.
#' The goal is to estimate the coordinates of the locations by hand using 
#' - google map
#' - https://maps.gov.bc.ca/ess/hm/imap4m

fields_def$cu_system_sites$X_LONGT

conservation_unit_system_sites$X_LONGT <- round(conservation_unit_system_sites $X_LONGT,6)
conservation_unit_system_sites$Y_LAT <- round(conservation_unit_system_sites $Y_LAT,6)

locations <- unique(conservation_unit_system_sites [,c("GFE_ID","X_LONGT","Y_LAT")])
nrow(locations) # 2312
locations$GFE_ID[duplicated(locations$GFE_ID)] # 0

coord_duplicated <- locations[,c("X_LONGT","Y_LAT")][duplicated(locations[,c("X_LONGT","Y_LAT")]),]
nrow(coord_duplicated) # 21

GFE_ID_duplicated <- c()
for(r in 1:nrow(coord_duplicated)){
  cond <- locations$X_LONGT == coord_duplicated$X_LONGT[r] &
    locations$Y_LAT == coord_duplicated$Y_LAT[r]
  GFE_ID_duplicated <- c(GFE_ID_duplicated,locations$GFE_ID[cond])
}

length(GFE_ID_duplicated) # 44
GFE_ID_duplicated <- unique(GFE_ID_duplicated)
length(GFE_ID_duplicated) # 41

cond <- conservation_unit_system_sites $GFE_ID %in% GFE_ID_duplicated

col <- c("GFE_ID","SYSTEM_SITE","Y_LAT","X_LONGT")
locations_duplicated <- unique(conservation_unit_system_sites[cond,col])
nrow(locations_duplicated) # 41

#' Sort the dataset and group per coordinates
locations_duplicated <- locations_duplicated_group_fun(locations_duplicated)

# Fill the coordinate appropriately in each case:
i <- 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(51.344756,-119.797289)
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(49.090741,-121.531078)  # not sure where the channel so I picked a location a little bit above the creek mouth
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 1
X_Y <- c(49.107077, -121.636804)  #
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(49.169178, -122.191740)  # could not check if the creek is called Hawkins
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(52.720154, -120.867586)  # could not found the beach so I picked a location just beside it
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]
i_toChange <- 3
X_Y <- c(52.746393, -120.837075)  # could not found the beach so I picked a location just beside it
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

cond <- conservation_unit_system_sites$SYSTEM_SITE == "BAXTER BEACH"
cond <- conservation_unit_system_sites$SYSTEM_SITE == "BEAR BEACH - SHORE"
cond <- conservation_unit_system_sites$SYSTEM_SITE == "BETTY FRANK'S - SHORE"
cond <- conservation_unit_system_sites$SYSTEM_SITE %in% c("BAXTER BEACH",
                                                          "BEAR BEACH - SHORE",
                                                          "BETTY FRANK'S - SHORE")
conservation_unit_system_sites[cond,]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(49.740213, -122.147390)  # 
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

cond <- conservation_unit_system_sites$SYSTEM_SITE == "LILLOOET RIVER"
conservation_unit_system_sites[cond,]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 1
X_Y <- c(50.680016, -121.929476)  # moved to CAYOOSH CREEK's mouth
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(49.626847, -122.671116)  # moved north a little bit
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(49.111242, -123.168505)  # just besid it 
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(51.745035, -122.413504)  # just above it 
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(49.294603, -124.879074)  # moved to the cross between the two rivers
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(49.201198, -125.512450)  # placed it above
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 1
X_Y <- c(54.445124, -125.458809)  # moved above the weir
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 1
X_Y <- c(52.378979, -126.581766)  # no idea where these two creeks are exactly
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

cond <- conservation_unit_system_sites$SYSTEM_SITE == "BETTY FRANK'S - SHORE"
cond <- conservation_unit_system_sites$SYSTEM_SITE %in% c("FORESTRY CREEK",
                                                          "MARTY'S CREEK")
conservation_unit_system_sites[cond,]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(50.342682, -127.437795)  # I don't know what is the difference between "creek" and "system"
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(55.081340, -125.560056)  # 
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(54.816010, -126.163338)  # 
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 1
X_Y <- c(52.225501, -127.598108)  # moved it just a little nord
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 1
X_Y <- c(52.044541, -128.068576)  # 
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 1
X_Y <- c(52.877146, -132.000700)  # 
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

# Replace these values in conservation_unit_system_sites

conservation_unit_system_sites$coordinates_changed <- F

for(l in unique(locations_duplicated$group)){
  # l <- unique(locations_duplicated$group)[1]
  cond_l <- locations_duplicated$group == l & !is.na(locations_duplicated$Y_LAT_new)
  GFE_ID <- locations_duplicated$GFE_ID[cond_l]
  SYSTEM_SITE <- locations_duplicated$SYSTEM_SITE[cond_l]
  Y_LAT_new <- locations_duplicated$Y_LAT_new[cond_l]
  X_LONGT_new <- locations_duplicated$X_LONGT_new[cond_l]
  
  for(i in 1:length(GFE_ID)){
    cond_cuss <- conservation_unit_system_sites$GFE_ID == GFE_ID[i] & 
      conservation_unit_system_sites$SYSTEM_SITE == SYSTEM_SITE[i]
    
    conservation_unit_system_sites$Y_LAT[cond_cuss] <- Y_LAT_new[i]
    conservation_unit_system_sites$X_LONGT[cond_cuss] <- X_LONGT_new[i]
    conservation_unit_system_sites$coordinates_changed[cond_cuss] <- T
  }
}

# check if there is no more duplicated coordinates
locations <- unique(conservation_unit_system_sites [,c("GFE_ID","X_LONGT","Y_LAT")])
nrow(locations) # 2333
coord_duplicated <- locations[,c("X_LONGT","Y_LAT")][duplicated(locations[,c("X_LONGT","Y_LAT")]),]
nrow(coord_duplicated) # 0


#
#' * 1) Remove the IndexId & GFE_ID time series in all_areas_nuseds with only NAs and/or 0s*

detectCores()
detectCores(logical = FALSE)
cores_nb <- 10
all_areas_nuseds <- remove_series_nodata_nuseds_parallel_fun(all_areas_nuseds = all_areas_nuseds,
                                                             zeros_too = T, 
                                                             cores_nb = cores_nb)
nrow(all_areas_nuseds) # 309647
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

nrow(removed_all) # 4491

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


#' * 2) Fix IndexId - GFE_ID series in CUSS that are not in NUSEDS  *
#' Look for each IndexId & GFE_ID series in conservation_unit_system_sites:
#' - 1) check if there are multiple GFE_IDs associated
#'      if yes: trouble shoot manually;
#' - 2) else look if there is a time series with the iid & its GFE_ID ('gfeid') in 
#'      all_areas_nuseds;
#'      if yes: all good;
#' - 3) else: that could be due to either (i) a typo in the IndexId or
#'      (ii) a typo in the GFE_ID. The the rest of the code looks for potential
#'      alternative series in NUSEDS with either a different IndexId (but with 
#'      the same species) or a different GFE_ID. Alternative series identified 
#'      NOT present in CUSS are kept, the ones present are removed.

#' The function returns a simple dataframe with the IndexId and GFE_ID concerned
#' and associated comment and eventual potential alternative series that have to 
#' be checked manually after.

# graphics.off()

colNuSEDS <- c("SPECIES","IndexId","GFE_ID","WATERBODY","Year","MAX_ESTIMATE")

# merge all_areas_nuseds and conservation_unit_system_sites by IndexId and GFE_ID
# and keep the time series that do not have a match
detectCores()
detectCores(logical = FALSE)
cores_nb <- 10
trackRecord <- cuss_nuseds_match_parallel_fun(conservation_unit_system_sites = conservation_unit_system_sites,
                                              all_areas_nuseds = all_areas_nuseds, 
                                              cores_nb = cores_nb)
head(trackRecord)
nrow(trackRecord)                    # 7145 same as CUSS
nrow(conservation_unit_system_sites) # 7145
sum(trackRecord$in_nused == "no")    # n = 259

# check the series related to different comments:
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

#' TODO: replace CN_46842 & GFE_ID = 2463 and CN_46841 & GFE_ID = 285 by 
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
nrow(all_areas_nuseds) # 309637

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
#' nuseds and not alternative.
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

# they have the same CU_NAME
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

# check pop size for the blue (to see if correspond to values in the PSE)
cond <- all_areas_nuseds$IndexId == "SX_45525" & all_areas_nuseds$GFE_ID == 303
all_areas_nuseds[cond,c("Year","MAX_ESTIMATE")][order(all_areas_nuseds[cond,c("Year")]),]

toAdd <- data.frame(IndexId = "SX_45525",
                    GFE_ID = 2444,
                    dataset = "streamlocationids",
                    CU_NAME = "NADINA/FRANCOIS-EARLY SUMMER TIMING",
                    comment = "In both CUSS and NUSEDS but not in the PSE; but series SX_45525 - 303 is.")

added_all <- toAdd

#
#'* 3) Check all the IndexId - GFE_ID series in NUSEDS but not in CUSS *
#' Look for each IndexId & GFE_ID series in NUSEDS that are not in CUSS
#' 1) remove series with <= 3 data points (n = 94)
#' 2) look if there are alternative series in NUSEDS that are also in CUSS
#'  - if yes then check if they can be merged
#'    - if they cannot -->  add the series to CUSS
#'  - if no --> add the series to CUSS

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
trackRecord_nuseds$alternative_IndexId_track <- NA # to retain the alternative IndexId not retained because series is not in CUSS
trackRecord_nuseds$alternative_GFE_ID_track <- NA # to retain the alternative GFE_ID not retained because series is not in CUSS
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
  
  #' 1) Look in NUSEDS for alternative series that are in CUSS with the same IndexId
  #' but different GFE_ID.
  cond <- all_areas_nuseds$IndexId == iid & all_areas_nuseds$GFE_ID != gfeid
  nuseds_here <- all_areas_nuseds[cond,]
  
  alternative_GFE_ID <- c()
  alternative_GFE_ID_track <- c()
  
  # there is no alternative series in NUSEDS
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
        
      }else{ # in that case the alternative series has to be in trackRecord_nuseds
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
  
  #' 2) Look in NUSEDS for alternative series that are in CUSS with the same GFE_ID 
  #' and species but different IndexId
  cond <- all_areas_nuseds$species_acronym_ncc == speciesAcro & 
    all_areas_nuseds$GFE_ID == gfeid &
    all_areas_nuseds$IndexId != iid
  nuseds_here <- all_areas_nuseds[cond,]
  
  alternative_IndexId <- c()
  alternative_IndexId_track <- c()
  
  # there is no alternative series in NUSEDS
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
        
      }else{ # in that case the alternative series has to be in trackRecord_nuseds
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
nrow(all_areas_nuseds) # 308250

removed_all <- rbind(removed_all,removed)
nrow(removed_all) # 4591

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
nrow(all_areas_nuseds) # 308195

#' Case with CN_39983 (i = 3):
#' It is not in the PSE
#' TODO: add to CUSS. Note that the POP_ID is not in CUSS nor in the PSE (or decoder)
#' There is consequently no CU associated to it.
#' But it is very close to the Nanaimo river for which there is a Chinook summer 
#' (i.e. ) for which the Chemainus river is included as a spawning location
#'  https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1709659637606539
trackRecord_nuseds_iid[3,]
iid <- trackRecord_nuseds_iid[3,]$IndexId
gfeid <- trackRecord_nuseds_iid[3,]$GFE_ID

# add to CUSS
cuss_new <- CUSS_newRow_fun(IndexId = iid, 
                            GFE_ID = gfeid,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

# CN_39983 is not in CUSS --> so it is not associated with a CU
cond <- conservation_unit_system_sites_all$IndexId == "CN_39983"
conservation_unit_system_sites_all$CU_NAME[cond]

# Look in the decoder --> the Summer one is not in there, only the Fall one.
cond <- grepl("[C|c]hemainus",conservationunits_decoder$cu_name_dfo)
conservationunits_decoder[cond,]

# attribute the East Vancouver Island-Georgia Strait (Summer 4-1) CU:
cond <- conservationunits_decoder$cu_name_pse == "East Vancouver Island-Georgia Strait (Summer 4-1)"
cu_decoder_here <- conservationunits_decoder[cond,]

#' find the CU-related information for East Vancouver Island-Georgia Strait 
#' (Summer 4-1) in CUSS:
cond <- conservation_unit_system_sites_all$CU_NAME == toupper(cu_decoder_here$cu_name_dfo) # does not work
cond <- grepl(toupper("East Vancouver Island-Georgia Strait"),conservation_unit_system_sites_all$CU_NAME) & 
  conservation_unit_system_sites_all$SPECIES_QUALIFIED == "CK"
conservation_unit_system_sites_all[cond,]

cu_fields <- c("CU_ACRO","CU_INDEX","CU_LAT","CU_LONGT","CU_NAME","CU_TYPE",
               "FULL_CU_IN","FAZ_ACRO","MAZ_ACRO","JAZ_ACRO")

cu_fields_info <- unique(conservation_unit_system_sites_all[cond,cu_fields])

for(f in colnames(cu_fields_info)){
  cuss_new[,f] <- cu_fields_info[,f]
}

conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId ="CN_39983",
                    GFE_ID = 1204,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = NA,
                    comment = "Series not in CUSS; POP_ID not in CUSS but attributed to CU_NAME: EAST VANCOUVER ISLAND-GEORGIA STRAIT_SU_0.3")

added_all <- rbind(added_all,toAdd)

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

added_all <- rbind(added_all,toAdd)

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
length(IndexId_GFE_ID_alternative) # 22

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
#' CN (i = 2) --> combine red and green and remove blue --> SUM ALL OF THEM because most individual chinook would pass the falls
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
#' The coordinates below are given latter in the process.
# cuss_new$Y_LAT <- 49.332224
# cuss_new$X_LONGT <- -124.919717

# add to conservation_unit_system_sites
conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId = trackRecord_nuseds_gfeid$IndexId[cond_i],
                    GFE_ID = trackRecord_nuseds_gfeid$GFE_ID[cond_i],
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = NA,
                    comment = "Series not in CUSS, not merged to altenative series IndexId = CM_3305 & GHE_ID = 11485 because upper location; GFE_ID not in CUSS so coordinates were define by hand")

toAdd$CU_NAME <- cuss_new$CU_NAME

added_all <- rbind(added_all,toAdd)

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

added_all <- rbind(added_all,toAdd)

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

added_all <- rbind(added_all,toAdd)


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

added_all <- rbind(added_all,toAdd)

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

added_all <- rbind(added_all,toAdd)

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
#' CORRECTION: we do not do the change above but instead create a new row in 
#' conservation_unit_system_sites
i <- 6
cond_i <- trackRecord_nuseds_gfeid$i == i
trackRecord_nuseds_gfeid$IndexId[cond_i]
trackRecord_nuseds_gfeid$GFE_ID[cond_i]
trackRecord_nuseds_gfeid$alternative_GFE_ID[cond_i]

series_MAX_ESTIMATE[i]

# all_areas_nuseds <- fields_edit_NUSEDS_CUSS_fun(edit_NUSEDS = T, 
#                                                 IndexId_focal = "CN_47277",
#                                                 IndexId_alter = "CN_47277",
#                                                 GFE_ID_focal = 2464,
#                                                 GFE_ID_alter = 142,
#                                                 all_areas_nuseds = all_areas_nuseds,
#                                                 conservation_unit_system_sites = conservation_unit_system_sites)
# 
# removed <- data.frame(IndexId = c("CN_47277"),
#                       GFE_ID = c(2464))
# removed$dataset <- "all_areas_nuseds"
# removed$comment <- "Series not in CUSS, merged to series CN_47277 & GFE_ID = 142"
# removed_all <- rbind(removed_all,removed)
# nrow(removed_all) # 4602

# add to CUSS
cuss_new <- CUSS_newRow_fun(IndexId = "CN_47277", 
                            GFE_ID = 2464,
                            conservation_unit_system_sites = conservation_unit_system_sites,
                            all_areas_nuseds = all_areas_nuseds)

# TEMPORARY, WAIT TO GET THE VALUES FROM WU
# --> Message from Wu Zhipeng from 04/04/2024:
# there is no geop coordinates for GFE_ID 2464. 
# The coordinate below are at the mouth of Cottonwood river, which is what 
# X_LONGT and Y_LAT should represent.
cuss_new$X_LONGT <- -122.610584
cuss_new$Y_LAT <- 53.119965
cuss_new$coordinates_changed <- T

conservation_unit_system_sites <- rbind(conservation_unit_system_sites,
                                        cuss_new)

toAdd <- data.frame(IndexId ="CN_47277",
                    GFE_ID = 2464,
                    dataset = "conservation_unit_system_sites",
                    CU_NAME = cuss_new$CU_NAME,
                    comment = "GFE_ID not in CUSS")

added_all <- rbind(added_all,toAdd)



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

added_all <- rbind(added_all,toAdd)

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

added_all <- rbind(added_all,toAdd)

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
  
  added_all <- rbind(added_all,toAdd)
  
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

added_all <- rbind(added_all,toAdd)

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

added_all <- rbind(added_all,toAdd)

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
  
  added_all <- rbind(added_all,toAdd)
  
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
#' ** 3.4) No alternative series **
cond <- trackRecord_nuseds$alternative_GFE_ID == "none" &
  trackRecord_nuseds$alternative_IndexId == "none" &
  !cond_3
trackRecord_nuseds_nocuss <- trackRecord_nuseds[cond,]
nrow(trackRecord_nuseds_nocuss) # 37

#' TODO: delete them from NUSEDS because we cannot attribute them a CU_NAME.
#' TODO: in future we could spend some time finding the CU name for the series 
#' with many points

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

nrow(all_areas_nuseds) # 307217


# Check - normally all series in NUSEDS and in CUSS now:
series_nuseds <- paste(all_areas_nuseds$IndexId,
                       all_areas_nuseds$GFE_ID,sep = "&")
series_nuseds <- unique(series_nuseds)
length(series_nuseds) # 6911

series_cuss <- paste(conservation_unit_system_sites$IndexId,
                     conservation_unit_system_sites$GFE_ID,sep = "&")
series_cuss <- unique(series_cuss)
length(series_cuss) # 6911

#' Now all the series in CUSS are in all_areas_nuseds.
series_cuss[! series_cuss %in% series_nuseds]

# Number of series in NUSEDS not in CUSS:
series_Nuseds_noCuss <- series_nuseds[! series_nuseds %in% series_cuss]
series_Nuseds_noCuss <- series_Nuseds_noCuss[order(series_Nuseds_noCuss)]
length(series_Nuseds_noCuss) # 0

#
# Define geo-spatial coordinates in CUSS where there are missing ----
#' They are missing because the corresponding series were added to CUSS and their
#' GFE_IDs were not already present in CUSS.

# Wu Zhipend from DFO email us a file with (some) geospatial info: list 1:
# emails from 
gfe_ids_extra1 <- read_excel(path = paste0(wd_data_dropbox,"/DFO_GFE_IDs_list_1.xlsx"),
                             sheet = 1)

gfe_ids_extra1 <- gfe_ids_extra1[,c("ID","X_LONGT","Y_LAT")]

gfe_ids_extra2 <- read_excel(path = paste0(wd_data_dropbox,"/DFO_GFE_IDs_list_2.xlsx"),
                             sheet = 1)

gfe_ids_extra2 <- gfe_ids_extra2[,c("GFE_ID_FROM","X_LONGT","Y_LAT")]

gfe_ids_extra <- merge(x = gfe_ids_extra1,
                       y = gfe_ids_extra2,
                       by.x = "ID",by.y = "GFE_ID_FROM", 
                       all = T)

gfe_ids_extra$X_LONGT <- apply(X = gfe_ids_extra, 1, FUN = function(r){
  out <- r[c("X_LONGT.x","X_LONGT.y")][!is.na(r[c("X_LONGT.x","X_LONGT.y")])]
  if(length(out) == 0){
    out <- NA
  }
  return(out)
}) %>% unlist()

gfe_ids_extra$Y_LAT <- apply(X = gfe_ids_extra, 1, FUN = function(r){
  out <- r[c("Y_LAT.x","Y_LAT.y")][!is.na(r[c("Y_LAT.x","Y_LAT.y")])]
  if(length(out) == 0){
    out <- NA
  }
  return(out)
})

gfe_ids_extra <- gfe_ids_extra[,c("ID","X_LONGT","Y_LAT")]

#' For  WATERSHED ABOVE STAMP FALLS:
#' Take GIS coordinates from Google map just above the Stamp falls (on the left of 
#' the Stamp river fish ladder):
#' Google uses the World Geodetic System WGS84 standard. --> same as in CUSS
GFE_ID <- 11486
cond <- conservation_unit_system_sites$GFE_ID == GFE_ID
conservation_unit_system_sites$SYSTEM_SITE[cond]
cond <- gfe_ids_extra$ID == GFE_ID
gfe_ids_extra$Y_LAT[cond] <- 49.332224
gfe_ids_extra$X_LONGT[cond] <- -124.919717

# retrieve the gfeids without geospatial info from conservation_unit_system_sites
cond <- is.na(conservation_unit_system_sites$Y_LAT)
gfeids_noCoord <- unique(conservation_unit_system_sites[cond,c("GFE_ID","SYSTEM_SITE")])

m <- merge(x = gfeids_noCoord,
           y = gfe_ids_extra[c("ID","X_LONGT","Y_LAT")], 
           by.x = "GFE_ID", by.y = "ID",
           all.x = T)
m
# there is no missing coordinate :-)

for(r in 1:nrow(m)){
  # r <- 1
  cond <- conservation_unit_system_sites$GFE_ID == m$GFE_ID[r]
  conservation_unit_system_sites$Y_LAT[cond] <- m$Y_LAT[r]
  conservation_unit_system_sites$X_LONGT[cond] <- m$X_LONGT[r]
  conservation_unit_system_sites$coordinates_changed[cond] <- T
}


# Check for locations with different SYSTEM_SITE but same coordinates (like initially)
conservation_unit_system_sites_copy <- conservation_unit_system_sites
# conservation_unit_system_sites <- conservation_unit_system_sites_copy

conservation_unit_system_sites$X_LONGT <- round(as.numeric(conservation_unit_system_sites$X_LONGT),6)
conservation_unit_system_sites$Y_LAT <- round(as.numeric(conservation_unit_system_sites$Y_LAT),6)

locations <- unique(conservation_unit_system_sites[,c("GFE_ID","X_LONGT","Y_LAT")])
nrow(locations) # 2312
locations$GFE_ID[duplicated(locations$GFE_ID)] # 0

coord_duplicated <- locations[,c("X_LONGT","Y_LAT")][duplicated(locations[,c("X_LONGT","Y_LAT")]),]
nrow(coord_duplicated) # 7

GFE_ID_duplicated <- c()
for(r in 1:nrow(coord_duplicated)){
  cond <- locations$X_LONGT == coord_duplicated$X_LONGT[r] &
    locations$Y_LAT == coord_duplicated$Y_LAT[r]
  GFE_ID_duplicated <- c(GFE_ID_duplicated,locations$GFE_ID[cond])
}

length(GFE_ID_duplicated) # 34
GFE_ID_duplicated <- unique(GFE_ID_duplicated)
length(GFE_ID_duplicated) # 10

cond <- conservation_unit_system_sites $GFE_ID %in% GFE_ID_duplicated
col <- c("GFE_ID","SYSTEM_SITE","Y_LAT","X_LONGT")
locations_duplicated <- unique(conservation_unit_system_sites[cond,col])
nrow(locations_duplicated) # 10
locations_duplicated

#' Sort the dataset and group per coordinates
locations_duplicated <- locations_duplicated_group_fun(locations_duplicated)
locations_duplicated

# Fill the coordinate appropriately in each case:
i <- 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(52.112252,-119.289006)  # moved it up
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
# cf. p 31 in https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40888721.pdf
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(51.593091, -119.701668)  # moved roughly betwen mouth and UPPER NORTH
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]
i_toChange <- 3
X_Y <- c(52.345236, -119.177796)  # moved above MILEDGE CREEK
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]
i_toChange <- 4
X_Y <- c(51.598602, -119.889197)  # moved way North to near BIRCH ISLAND
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]
i_toChange <- 5
X_Y <- c(52.281312, -119.173514)  # moved to MILEDGE CREEK
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]
i_toChange <- 6
X_Y <- c(51.579465, -119.810014)  # moved to PiG Channel according to pdf map
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

i <- i + 1
letter <- unique(locations_duplicated$group)[i]
cond <- locations_duplicated$group == letter
locations_duplicated[cond,]
i_toChange <- 2
X_Y <- c(50.162828, -120.665360) # location of the dam East of Merritt
locations_duplicated$Y_LAT_new[cond][i_toChange] <- X_Y[1]
locations_duplicated$X_LONGT_new[cond][i_toChange] <- X_Y[2]

# Edit conservation_unit_system_sites
for(l in unique(locations_duplicated$group)){
  # l <- unique(locations_duplicated$group)[1]
  cond <- locations_duplicated$group == l & !is.na(locations_duplicated$Y_LAT_new)
  GFE_ID <- locations_duplicated$GFE_ID[cond]
  Y_LAT_new <- locations_duplicated$Y_LAT_new[cond]
  X_LONGT_new <- locations_duplicated$X_LONGT_new[cond]
  for(i in 1:length(GFE_ID)){
    # i <- 1
    cond <- conservation_unit_system_sites$GFE_ID == GFE_ID[i]
    conservation_unit_system_sites$X_LONGT[cond] <- X_LONGT_new[i]
    conservation_unit_system_sites$Y_LAT[cond] <- Y_LAT_new[i]
    conservation_unit_system_sites$coordinates_changed[cond] <- T
  }
}

# check
locations <- unique(conservation_unit_system_sites[,c("GFE_ID","X_LONGT","Y_LAT")])
nrow(locations) # 2312
locations$GFE_ID[duplicated(locations$GFE_ID)] # 0

coord_duplicated <- locations[,c("X_LONGT","Y_LAT")][duplicated(locations[,c("X_LONGT","Y_LAT")]),]
nrow(coord_duplicated) # 0

# check 
sum(is.na(conservation_unit_system_sites$coordinates_changed))

#
# Export cleaned all_areas_nuseds and conservation_unit_system_sites -----

#' Export NUSEDS:
write.csv(all_areas_nuseds,paste0(wd_output,"/all_areas_nuseds_cleaned.csv"), 
          row.names = F)

#' Export CUSS:
write.csv(conservation_unit_system_sites,paste0(wd_output,"/conservation_unit_system_sites_cleaned.csv"), 
          row.names = F)

#
# Merge NUSEDS AND CUSS and do some edits -----
#

all_areas_nuseds <- read.csv(paste0(wd_output,"/all_areas_nuseds_cleaned.csv"),
                             header = T)

conservation_unit_system_sites <- read.csv(paste0(wd_output,"/conservation_unit_system_sites_cleaned.csv"),
                                           header = T)

nrow(unique(conservation_unit_system_sites[,c("SPECIES_QUALIFIED","POP_ID","SYSTEM_SITE")])) # 6911
nrow(unique(conservation_unit_system_sites[,c("SPECIES_QUALIFIED","POP_ID","SYSTEM_SITE","GFE_ID")])) # 6911


#'* Merge NUSEDS with CUSS *
#'
col_common <- c("IndexId","POP_ID","GFE_ID")

col_nuseds <- c("SPECIES","WATERBODY","AREA","Year","MAX_ESTIMATE",
                #"ENUMERATION_METHODS",            # 
                "ESTIMATE_CLASSIFICATION",
                "ESTIMATE_METHOD",
                "GAZETTED_NAME",
                "LOCAL_NAME_1",
                "LOCAL_NAME_2"
                )

col_cuss <- c("SPECIES_QUALIFIED","CU_NAME","CU_TYPE","FAZ_ACRO","JAZ_ACRO","MAZ_ACRO",
              "FULL_CU_IN","SYSTEM_SITE","Y_LAT","X_LONGT","IS_INDICATOR",
              "CU_LAT","CU_LONGT","coordinates_changed")

# Transform all_areas_nuseds to a wide format (NOT ANYMORE)
# nuseds_long <- all_areas_nuseds[,c(col_common,col_nuseds)] %>% 
#   pivot_wider(names_from = "Year",values_from = "MAX_ESTIMATE",names_sort = T)
# 
# nrow(all_areas_nuseds) # 307217
# nrow(nuseds_long)      # 6910
# nrow(conservation_unit_system_sites)  # 6910
# 
# nuseds_final <- base::merge(y = nuseds_long, 
#                             x = conservation_unit_system_sites[,c(col_common,col_cuss)], 
#                             by = col_common, 
#                             all.y = T)

nuseds_final <- base::merge(x = all_areas_nuseds[,c(col_common,col_nuseds)], 
                            y = conservation_unit_system_sites[,c(col_common,col_cuss)], 
                            by = col_common, 
                            all.x = T)

nrow(nuseds_final)     # 307217
nrow(all_areas_nuseds) # 307217

#'* Rename fields *
# Older file for comparison:
# wd_here <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Fraser_VIMI/analysis/Compilation/Results")
# nusedsPrevious <- read.csv(paste0(wd_here,"/NuSEDS_escapement_data_collated_20230818.csv"),header = T)

# field_toChange <- c("SYSTEM_SITE",
#                     "IS_INDICATOR",
#                     "SPECIES_QUALIFIED",
#                     "Y_LAT","X_LONGT",
#                     "AREA",
#                     "MAZ_ACRO","FAZ_ACRO","JAZ_ACRO",
#                     "CU_NAME"
#                     #"SPECIES"
#                     )
# 
# fields_new <- c("SYS_NM",
#                 "IsIndicator",
#                 "species_abbr",
#                 "yLAT","xLONG",
#                 "Area",
#                 "maz_acro","faz_acro","jaz_acro",
#                 "CU_name"
#                 #"species_abbr"
#                 )
# 
# for(i in 1:length(field_toChange)){
#   names(nuseds_final)[names(nuseds_final) == field_toChange[i]] <- fields_new[i]
# }

#' * add "X" in from of the year columns *
#' cond <- grepl("[1|2]",colnames(nuseds_final))
#' col_yrs <-  colnames(nuseds_final)[cond]
#' colnames(nuseds_final)[cond] <- paste0("X",col_yrs)
#' col_yrs <-  colnames(nuseds_final)[cond]

#'* remove IndexId and SPECIES * 
# nuseds_final <- nuseds_final[,! colnames(nuseds_final) %in% c("IndexId","SPECIES")]

#'* Replace 0s by NAs *
#' There are cases where 0s means 0s and cases where they mean NAs (for instance)
#' someone assess multiple populations of salmon at a given time that falls 
#' outside the migratory period of some of the assess population. 
#' In future we should try to discriminate the true 0s from the NA ones.
#' cf. Population "2024 Population Analysis running notes" at March 19 for 
#' corresponding documentation. 
#' https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing
cond <- nuseds_final$MAX_ESTIMATE == 0 & !is.na(nuseds_final$MAX_ESTIMATE)
sum(cond) # 2996
sum(cond)/nrow(nuseds_final) * 100
nuseds_final$MAX_ESTIMATE[cond] <- NA

#
# Additional fix: when a CU have multiple IndexId/POP_ID in a same GFE_ID ------
#' During the next phase (when attributing PFS cuid, pointid and streamid) I 
#' noticed many cases where a same CU in a same location has multiple series (i.e. POP_ID)
#' and that among those there are clear duplicates or single data point that 
#' are not worth keeping.
#' Rules:
#' - Case 1: if one data point
#'    - if complementary --> merge to longer series
#'    - if conflict or duplicate --> remove
#' - Case 2: the shorter series is 100% duplicated --> removed
#' - Case 3: on the rest of the series, 
#'    - points that are conflictual or duplicated are added
#'    - points that are complementary are merged
#' 

# date <- "20240307"
# nuseds_final <- read.csv(paste0(wd_output,"/NuSEDS_escapement_data_collated_",date,".csv"),
#                          header = T)

# removed_all <- read.csv(paste0(wd_output,"/series_removed.csv"),header = T)
# head(removed_all)
# unique(removed_all$comment)

removed_all_new <- removed_all[NULL,]

fields_l <- fields_IndexId_GFE_ID_fun(all_areas_nuseds = all_areas_nuseds,
                                      conservation_unit_system_sites = conservation_unit_system_sites)

fields_IndexId <- unique(c(fields_l$NUSEDS$IndexId,fields_l$CUSS$IndexId))
fields_IndexId <- fields_IndexId[fields_IndexId %in% colnames(nuseds_final)]

nuseds_CU_GFI_ID <- unique(nuseds_final[,c("SPECIES_QUALIFIED","CU_NAME","GFE_ID")])
nrow(nuseds_CU_GFI_ID) # 6848

count <- 1
for(r in 1:nrow(nuseds_CU_GFI_ID)){
  # r <- 3540
  CU_NAME_here <- nuseds_CU_GFI_ID$CU_NAME[r]
  GFE_ID_here <- nuseds_CU_GFI_ID$GFE_ID[r]
  SPECIES_QUALIFIED_here <- nuseds_CU_GFI_ID$SPECIES_QUALIFIED[r]
  
  cond_nuseds <- nuseds_final$CU_NAME == CU_NAME_here &
    nuseds_final$GFE_ID == GFE_ID_here &
    nuseds_final$SPECIES_QUALIFIED == SPECIES_QUALIFIED_here
  
  IndexId_here <- unique(nuseds_final$IndexId[cond_nuseds])
  
  if(length(IndexId_here) > 1){
    
    plot_IndexId_GFE_ID_fun(IndexIds = IndexId_here,
                            GFE_IDs = rep(GFE_ID_here,length(IndexId_here)),
                            all_areas_nuseds = nuseds_final)
    legend("top",legend = paste0("CU_NAME_here = ",CU_NAME_here),bty = 'n')
    legend("topright",legend = c(paste0("count = ",count),
                                 paste0(", r = ",r)),bty = 'n')
    
    cond_nuseds_1 <- cond_nuseds & nuseds_final$IndexId == IndexId_here[1]
    cond_nuseds_2 <- cond_nuseds & nuseds_final$IndexId == IndexId_here[2]
    series_1 <- nuseds_final$MAX_ESTIMATE[cond_nuseds_1]
    names(series_1) <- nuseds_final$Year[cond_nuseds_1]
    series_2 <- nuseds_final$MAX_ESTIMATE[cond_nuseds_2]
    names(series_2) <- nuseds_final$Year[cond_nuseds_2]
    
    # the focal series should be the shortest one
    if(length(series_1) > length(series_2)){
      IndexId_focal <- IndexId_here[2]
      IndexId_compare <- IndexId_here[1]
      series_focal <- series_2
      series_compare <- series_1
      cond_focal <- cond_nuseds_2
      cond_compare <- cond_nuseds_1
    }else{
      IndexId_focal <- IndexId_here[1]
      IndexId_compare <- IndexId_here[2]
      series_focal <- series_1
      series_compare <- series_2
      cond_focal <- cond_nuseds_1
      cond_compare <- cond_nuseds_2
    }
    
    comparison <- compare_series_fun(series_focal = series_focal,
                                     series_compare = series_compare)
    
    # series_compare[order(as.numeric(names(series_compare)))]
    
    # some extra cases spotted by eye
    delete_exception <- IndexId_focal == "CN_50619" & GFE_ID_here == 824 # here one data point is a duplicate, the other one almost is
    correct_exception <- IndexId_focal == "CM_50537" & GFE_ID_here == 816
    
    # Case 1
    if(comparison$nb_dataPt == 1){
      
      # merge the focal series to series_compare
      if(comparison$complementary == 1){
        
        removed <- data.frame(IndexId = IndexId_focal,
                              GFE_ID = GFE_ID_here)
        removed$dataset <- "nuseds_final"
        removed$comment <- paste0("The one data point was merged to series ",IndexId_compare," & GFE_ID = ",GFE_ID_here," as it is the same CU: ",CU_NAME_here)
        removed_all_new <- rbind(removed_all_new,removed)
        
        # 1st edit IndexId related fields in nuseds_final
        # 2nd remove the row with only NA
        cond_focal_NA <- cond_focal & is.na(nuseds_final$MAX_ESTIMATE)
        cond_focal_NAno <-  cond_focal & !is.na(nuseds_final$MAX_ESTIMATE)
        for(f in fields_IndexId){
          # f <- fields_IndexId[1]
          nuseds_final[cond_focal_NAno,f] <- unique(nuseds_final[cond_compare,f])
        }
        nuseds_final <- nuseds_final[!cond_focal_NA,]
        
        plot_IndexId_GFE_ID_fun(IndexIds = IndexId_here,
                                GFE_IDs = rep(GFE_ID_here,length(IndexId_here)),
                                all_areas_nuseds = nuseds_final)
        legend("top",legend = paste0("CU_NAME_here = ",CU_NAME_here),bty = 'n')
        legend("topright",legend = "MERGED",bty = 'n')
        
      }else{ # discard it
        
        nuseds_final <- nuseds_final[!cond_focal,]
        
        removed <- data.frame(IndexId = IndexId_focal,
                              GFE_ID = GFE_ID_here)
        removed$dataset <- "nuseds_final"
        removed$comment <- paste0("The series had only one point in confict or duplicating the series ",IndexId_compare," & GFE_ID = ",GFE_ID_here," of the same CU: ",CU_NAME_here)
        removed_all_new <- rbind(removed_all_new,removed)
        
        plot_IndexId_GFE_ID_fun(IndexIds = IndexId_here,
                                GFE_IDs = rep(GFE_ID_here,length(IndexId_here)),
                                all_areas_nuseds = nuseds_final)
        legend("top",legend = paste0("CU_NAME_here = ",CU_NAME_here),bty = 'n')
        legend("topright",legend = "DELETED",bty = 'n')
      }
      
    }else if(comparison$nb_dataPt == comparison$duplicate | delete_exception){ # case 2 --> remove the short series that is 100% duplicated
      
      nuseds_final <- nuseds_final[!cond_focal,]
      
      removed <- data.frame(IndexId = IndexId_focal,
                            GFE_ID = GFE_ID_here)
      removed$dataset <- "nuseds_final"
      removed$comment <- paste0("The series was duplicating the series ",IndexId_compare," & GFE_ID = ",GFE_ID_here," of the same CU: ",CU_NAME_here)
      removed_all_new <- rbind(removed_all_new,removed)
      
      
      plot_IndexId_GFE_ID_fun(IndexIds = IndexId_here,
                              GFE_IDs = rep(GFE_ID_here,length(IndexId_here)),
                              all_areas_nuseds = nuseds_final)
      legend("top",legend = paste0("CU_NAME_here = ",CU_NAME_here),bty = 'n')
      legend("topright",legend = "DELETED",bty = 'n')
      
    }else if(correct_exception){
      
      # 1st find the 9 data points that are duplicated with series_compare
      # return the years with matching abundances
      years_toRemove <- c()
      for(i in 1:length(series_focal)){
        # i <- 1
        yr_here <- names(series_focal)[i]
        val_compare <- series_compare[as.character(yr_here)]
        if(!is.na(val_compare) & !is.na(series_focal[i])){
          if(series_focal[i] == val_compare){
            years_toRemove <- c(years_toRemove,yr_here)
          }
        }
      }
      years_toRemove <- as.numeric(years_toRemove)
      years_toRemove <- years_toRemove[years_toRemove < 1960] # there is one duplicated points for later years but it looks legit
      
      cond_focal_YrtoRemove <- cond_focal & nuseds_final$Year %in% years_toRemove
      
      # 2nd merge the rest of the series but summing the data
      # change the field in the focal series
      for(f in fields_IndexId){
        # f <- fields_IndexId[1]
        nuseds_final[cond_focal,f] <- unique(nuseds_final[cond_compare,f])
      }
      # sum MAX_ESTIMATE in duplicated year
      cond_sum <- (cond_focal & !cond_focal_YrtoRemove) | cond_compare
      yr_duplicated <- nuseds_final[cond_sum,]$Year[duplicated(nuseds_final[cond_sum,]$Year)]
      for(yr in yr_duplicated){
        # yr <- yr_duplicated[1]
        cond_here <- (cond_focal | cond_compare) & nuseds_final$Year == yr
        vals <- nuseds_final$MAX_ESTIMATE[cond_here]
        if(any(!is.na(vals))){
          val <- sum(vals,na.rm = T)
          nuseds_final$MAX_ESTIMATE[cond_here] <- val
        }
      }
      
      # 3rd remove the row with duplicated data from 1st step and the rows with 
      # duplicated years in the focal from 2nd step
      cond_toDelete <- cond_focal_YrtoRemove | (cond_focal & nuseds_final$Year %in% yr_duplicated)
      nuseds_final <- nuseds_final[!cond_toDelete,]
      
      removed <- data.frame(IndexId = IndexId_focal,
                            GFE_ID = GFE_ID_here)
      removed$dataset <- "nuseds_final"
      removed$comment <- paste0("We removed the duplicated data points and merged by summing the rest of them to series ",IndexId_compare," & GFE_ID = ",GFE_ID_here," of the same CU: ",CU_NAME_here)
      removed_all_new <- rbind(removed_all_new,removed)
      
      plot_IndexId_GFE_ID_fun(IndexIds = IndexId_here,
                              GFE_IDs = rep(GFE_ID_here,length(IndexId_here)),
                              all_areas_nuseds = nuseds_final)
      legend("top",legend = paste0("CU_NAME_here = ",CU_NAME_here),bty = 'n')
      legend("topright",legend = "DELETED & MERGED",bty = 'n')
      

    }else{ # Case 3
      
      plot_IndexId_GFE_ID_fun(IndexIds = IndexId_here,
                              GFE_IDs = rep(GFE_ID_here,length(IndexId_here)),
                              all_areas_nuseds = nuseds_final)
      legend("top",legend = paste0("CU_NAME_here = ",CU_NAME_here),bty = 'n')
      
      if("CN_2483" %in% IndexId_here & GFE_ID_here == 142){
        
        # - 1) merge CN_47277 - 142 to CN_2483 - 142
        # - 2) remove the point CN_47278 - 142
        
        # 1) 
        # change the IndexId of focal series to the compared one
        cond_focal <- nuseds_final$IndexId == "CN_47277" & 
          nuseds_final$GFE_ID == 142
        cond_compare <- nuseds_final$IndexId == "CN_2483" & 
          nuseds_final$GFE_ID == 142
        
        for(f in fields_IndexId){
          # f <- fields_IndexId[1]
          nuseds_final[cond_focal,f] <- unique(nuseds_final[cond_compare,f])
        }
        
        removed <- data.frame(IndexId = "CN_47277",
                              GFE_ID = 142)
        removed$dataset <- "nuseds_final"
        removed$comment <- paste0("Series merged to series ",IndexId_compare,
                                  " & GFE_ID = ",GFE_ID_here," of the same CU: ",
                                  CU_NAME_here)
        removed_all_new <- rbind(removed_all_new,removed)
        
        # 2)
        cond_toRemove <- nuseds_final$IndexId == "CN_47278" & 
          nuseds_final$GFE_ID == 142
        
        nuseds_final <- nuseds_final[!cond_toRemove,]
        
        removed <- data.frame(IndexId = "CN_47278",
                              GFE_ID = 142)
        removed$dataset <- "nuseds_final"
        removed$comment <- paste0("The series had only one point in confict or duplicating the series ",
                                  IndexId_compare," & GFE_ID = ",GFE_ID_here," of the same CU: ",CU_NAME_here)
        removed_all_new <- rbind(removed_all_new,removed)
        
        plot_IndexId_GFE_ID_fun(IndexIds = IndexId_here,
                                GFE_IDs = rep(GFE_ID_here,length(IndexId_here)),
                                all_areas_nuseds = nuseds_final)
        legend("top",legend = paste0("CU_NAME_here = ",CU_NAME_here),bty = 'n')
        legend("topright",legend = "MERGED & DELETED",bty = 'n')
        
        
      }else{
        
        # 1st change the IndexId of focal series to the compared one
        for(f in fields_IndexId){
          # f <- fields_IndexId[1]
          nuseds_final[cond_focal,f] <- unique(nuseds_final[cond_compare,f])
        }
        
        # 2nd sum MAX_ESTIMATE values for duplicated years
        yr_duplicated <- nuseds_final[cond_focal | cond_compare,]$Year[duplicated(nuseds_final[cond_focal | cond_compare,]$Year)]
        for(yr in yr_duplicated){
          # yr <- yr_duplicated[1]
          cond_here <- (cond_focal | cond_compare) & nuseds_final$Year == yr
          vals <- nuseds_final$MAX_ESTIMATE[cond_here]
          if(any(!is.na(vals))){
            val <- sum(vals,na.rm = T)
            nuseds_final$MAX_ESTIMATE[cond_here] <- val
          }
        }
        
        # 3rd remove the rows with duplicated years in the focal
        cond_toDelete <- cond_focal & nuseds_final$Year %in% yr_duplicated
        nuseds_final <- nuseds_final[!cond_toDelete,]
        

        plot_IndexId_GFE_ID_fun(IndexIds = IndexId_here,
                                GFE_IDs = rep(GFE_ID_here,length(IndexId_here)),
                                all_areas_nuseds = nuseds_final)
        legend("top",legend = paste0("CU_NAME_here = ",CU_NAME_here),bty = 'n')
        legend("topright",legend = "MERGED",bty = 'n')
        
        removed <- data.frame(IndexId = IndexId_focal,
                              GFE_ID = GFE_ID_here)
        removed$dataset <- "nuseds_final"
        removed$comment <- paste0("Series merged by summing to series ",IndexId_compare," & GFE_ID = ",GFE_ID_here," of the same CU: ",CU_NAME_here)
        removed_all_new <- rbind(removed_all_new,removed)
        
      }
    }
    count <- count + 1
  }
}

removed_all <- rbind(removed_all,removed_all_new)
removed_all <- unique(removed_all)

#
# Additional fix: check duplicated years for a same IndexId/POP_ID - GFE_ID series ------
#' If there are duplicated years:
#' - if it is with NAs --> remove the duplicated row with NAs
#' - if it is not with NA: deal with it (should not have happened)

nrow(nuseds_final) # 306833

CU_GFE_ID <- unique(nuseds_final[,c("SPECIES_QUALIFIED","CU_NAME","GFE_ID")])
nrow(CU_GFE_ID) # 6848

count_show <- 1
for(r in 1:nrow(CU_GFE_ID)){
  # r <- 1
  cond <- nuseds_final$SPECIES_QUALIFIED == CU_GFE_ID$SPECIES_QUALIFIED[r] &
    nuseds_final$CU_NAME == CU_GFE_ID$CU_NAME[r] &
    nuseds_final$GFE_ID == CU_GFE_ID$GFE_ID[r]
  
  yr_dupli <- nuseds_final$Year[cond][duplicated(nuseds_final$Year[cond])]
  IndexIds <- unique(nuseds_final$IndexId[cond])
  
  # if there are duplicated years
  if(length(yr_dupli) > 0){
    
    # if these duplicated years occur for a same IndexId
    if(length(IndexIds) == 1){
      
      # for each duplicated year, check if there is only one MAX_ESTIMATE value 
      # and the duplicated years have NAs
      for(yr in yr_dupli){
        # yr <- yr_dupli[1]
        cond_yr <- cond & nuseds_final$Year == yr
        MAX_ESTIMATE_here <- nuseds_final$MAX_ESTIMATE[cond_yr]
        
        if(sum(!is.na(MAX_ESTIMATE_here)) == 1){
          #
          cond_yr_NA <- cond_yr & is.na(nuseds_final$MAX_ESTIMATE)
          print("Row removed:")
          print(nuseds_final[cond_yr_NA,c("SPECIES_QUALIFIED","CU_NAME","GFE_ID","Year","MAX_ESTIMATE")])
          
          nuseds_final <- nuseds_final[!cond_yr_NA,]
          
        }else{ # problematic cases to flag and fix eventually
          
          if(sum(!is.na(MAX_ESTIMATE_here)) == 0){
            print("Duplicated years occur for a same IndexId and has only NAs")
            
          }else{
            print("Duplicated years occur for a same IndexId and has more than one value")
          }
          print(nuseds_final[cond,][cond_yr,c("SPECIES_QUALIFIED","CU_NAME","GFE_ID","Year","MAX_ESTIMATE")])
          break
        }
      }
      
    }else{ # if there are multiple IndexIds
      
      # return the series 
      series_l <- lapply(X = IndexIds, FUN = function(iid){
        cond_here <- nuseds_final$IndexId[cond] == iid
        return(nuseds_final[cond,][cond_here,c("Year","MAX_ESTIMATE")])
          })
      
      # check if any series in series_l has only NAs
      NA_any <- lapply(series_l,function(s){all(is.na(s[,"MAX_ESTIMATE"]))}) %>%
        unlist() %>%
        any()
      
      if(NA_any){ # remove them --> does not occure I did not code for it
        
        print("Duplicated years occur for a same CU with only NAs in certain series")
        
        plot_IndexId_GFE_ID_fun(IndexIds = IndexIds,
                                GFE_IDs = rep(unique(nuseds_final$GFE_ID[cond]),length(IndexIds)),
                                all_areas_nuseds = nuseds_final)
        
        break
      }
    }
  }
  
  count_percent <- round(r/nrow(CU_GFE_ID)*100,1)
  if(count_show <= count_percent){
    print(paste("Progress:",count_percent,"%"))
    count_show <- count_show + 1
  }
}
print(paste("Progress:",count_percent,"%"))
nrow(nuseds_final) # 306823

#
# Additional fix: locations with same coordinates (dealt earlier but just in case) -----
#
# nuseds_final <- import_mostRecent_file_fun(wd = wd_output,
#                                            pattern = "NuSEDS_escapement_data_collated")

nuseds_final$X_LONGT <- round(nuseds_final$X_LONGT,6)
nuseds_final$Y_LAT <- round(nuseds_final$Y_LAT,6)

locations <- unique(nuseds_final[,c("GFE_ID","X_LONGT","Y_LAT")])
nrow(locations) # 2312
locations$GFE_ID[duplicated(locations$GFE_ID)] # 0

coord_duplicated <- locations[,c("X_LONGT","Y_LAT")][duplicated(locations[,c("X_LONGT","Y_LAT")]),]
nrow(coord_duplicated) # 0

# check
sum(is.na(nuseds_final$coordinates_changed))

#
# Export CSV files: ------

# Export nusweds_final
date <- "20240307"
date <- "20240328"
date <- as.character(Sys.time())
date <- strsplit(x = date, split = " ")[[1]][1]
date <- gsub("-","",date)

write.csv(nuseds_final,paste0(wd_output,"/NuSEDS_escapement_data_collated_",date,".csv"),
          row.names = F)

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

write.csv(trackRecord_toExport,paste0(wd_output,"/series_inNUSEDS_noInCUSS_",date,".csv"),
          row.names = F)


#' Export the CUs removed and those added:
write.csv(removed_all,paste0(wd_output,"/series_removed_",date,".csv"),row.names = F)

write.csv(added_all,paste0(wd_output,"/series_added_",date,".csv"),row.names = F)

# END
