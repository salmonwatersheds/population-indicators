
#'******************************************************************************
#' The goal of the script is to conduct differentc checks
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
source("code/functions_set_wd.R")
source("code/functions_general.R")

# return the name of the directories for the different projects:
subDir_projects <- subDir_projects_fun()

wds_l <- set_working_directories_fun(subDir = subDir_projects$biological_status,
                                     Export_locally = F)
wd_head <- wds_l$wd_head
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS
wd_project_dropbox <- wds_l$wd_project_dropbox
wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# The datasets to input were outputted by other scripts 
wd_data_input <- wd_output

# Import functions for this specific project
source("Code/functions.R")

# Load packages

# Paths to the repositories containing the run reconstruction datasets for each 
# region.
wd_data_regions <- wd_data_regions_fun(wd_root = wd_X_Drive1_PROJECTS)

# Import species names and acronyms
species_acronym_df <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#------------------------------------------------------------------------------#
# Selection of region(s) and species and benchmark %
#------------------------------------------------------------------------------#

# Choosing the region
# BSC: This will have to eventually be automatized and eventually allows for 
# multiple regions to be passed on.
region <- regions_df$Fraser

# multiple regions:
region <- c(
  regions_df$Haida_Gwaii,
  regions_df$Central_coast)

# all the regions
region <- as.character(regions_df[1,])

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "CK"],    
             species_acronym_df$species_name[species_acronym_df$species_acro == "SX"])

# If we do not specify the species: all the species that have a _SRdata files are 
# returned: 
# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- T

#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

#' Import the cuspawnerabundance.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' To calculating current spawner abundance for biostatus assessment
fromDatabase <- F
update_file_csv <- F

cuspawnerabundance <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[2],
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)

#' Import the recruitsperspawner.csv from population-indicators/data_input or 
#' download it from the PSF database
recruitsperspawner <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[3],
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)

#
# Check that the data spawner abundance data matches between cuspawnerabundance and recruitsperspawner -------

#'* Are they CUs only present in one of the two datasets? *

colnamesCu <- c("region","species_name","cuid","cu_name_pse")

SpawnAbund <- unique(cuspawnerabundance[,colnamesCu])
nrow(SpawnAbund) # 464

SpawnRecru <- unique(recruitsperspawner[,colnamesCu])
nrow(SpawnRecru) # 184

#'* Are they CUs in recruitsperspawner not in cuspawnerabundance *

SpawnAbund$cuspawnerabundance <- T
SpawnRecru$recruitsperspawner <- T

mergedDF <- merge(x = SpawnRecru,y = SpawnAbund, by = colnamesCu, all.x = T)
nrow(mergedDF) # 184

mergedDF[is.na(mergedDF$recruitsperspawner)] # empty

#' CONCLUSION: all the CUs in recruitsperspawner are in cuspawnerabundance.

#'* Do the spawner abundances match? *

head(mergedDF)
mismatch_l <- list()
count <- 1
identical_l <- list()
count_identical <- 1
for(r in 1:nrow(mergedDF)){
  # r <- 1
  region <- mergedDF$region[r]
  species <- mergedDF$species[r]
  cu_name_pse <- mergedDF$cu_name_pse[r]
  cuspawnerabundanceHere <- cuspawnerabundance[cuspawnerabundance$region == region &
                                                 cuspawnerabundance$species_name == species &
                                                 cuspawnerabundance$cu_name_pse == cu_name_pse,]
  recruitsperspawnerHere <- recruitsperspawner[recruitsperspawner$region == region &
                                                 recruitsperspawner$species_name == species &
                                                 recruitsperspawner$cu_name_pse == cu_name_pse,]
  
  cuspawnerabundanceHere[,c(colnamesCu,"year","estimated_count","observed_count")]
  recruitsperspawnerHere[,c(colnamesCu,"year","spawners")]
  mergeDFHere <- merge(x = recruitsperspawnerHere[,c(colnamesCu,"year","spawners")], 
                       y = cuspawnerabundanceHere[,c(colnamesCu,"year","estimated_count","observed_count")],
                       by = c(colnamesCu,"year"), all = T)
  namesHere <- paste(region,species,cu_name_pse,sep=" - ")
  if(!identical(mergeDFHere$spawners,mergeDFHere$estimated_count)){
    mismatch_l[[count]] <- mergeDFHere
    names(mismatch_l)[count] <- namesHere
    count <- count + 1
    print(paste0("Mismatch with: ",namesHere))
  }else{
    identical_l[[count_identical]] <- mergeDFHere
    names(identical_l)[count_identical] <- namesHere
    count_identical <- count_identical + 1
  }
}
length(mismatch_l) # 143
mismatch_l[[5]]
length(identical_l) # 41
identical_l[[1]]

mismatch_remain_l <- mismatch_l

#' list of CUS with only NAs in recruitsperspawner$spawners and list of 
#' CUs with only NAs in cuspawnerabundance$estimated_count
mismatch_allNAs_recruitsperspawner_l <- list()
count_RS <- 1
mismatch_allNAs_cuspawnerabundance_l <- list()
count_cuS <- 1
for(i in 1:length(mismatch_remain_l)){
  # i <- 1
  mergeDFHere <- mismatch_remain_l[[i]]
  if(sum(!is.na(mergeDFHere$spawners)) == 0){
    mismatch_allNAs_recruitsperspawner_l[[count_RS]] <- mergeDFHere
    names(mismatch_allNAs_recruitsperspawner_l)[count_RS] <- names(mismatch_l)[i]
    count_RS <- count_RS + 1
  }else if(sum(!is.na(mergeDFHere$estimated_count)) == 0){
    mismatch_allNAs_cuspawnerabundance_l[[count_cuS]] <- mergeDFHere
    names(mismatch_allNAs_cuspawnerabundance_l)[count_cuS] <- names(mismatch_l)[i]
    count_cuS <- count_cuS + 1
  }
}
length(mismatch_allNAs_recruitsperspawner_l) # 8
length(mismatch_allNAs_cuspawnerabundance_l) # 1

mismatch_remain_l <- mismatch_remain_l[!names(mismatch_remain_l) %in% names(mismatch_allNAs_recruitsperspawner_l)]
mismatch_remain_l <- mismatch_remain_l[!names(mismatch_remain_l) %in% names(mismatch_allNAs_cuspawnerabundance_l)]
length(mismatch_remain_l) # 134

#' CUs with missing values in recruitsperspawner$spawners only in the recent years,
#' otherwise all rest of the data is identical
mismatch_NAsRecentYr_l <- list()
count <- 1
for(i in 1:length(mismatch_remain_l)){
  # i <- 1
  mergeDFHere <- mismatch_remain_l[[i]]
  lastYear <- max(mergeDFHere$year[!is.na(mergeDFHere$spawners)])
  mergeDFHereCut <- mergeDFHere[mergeDFHere$year <= lastYear,]
  if(identical(mergeDFHereCut$spawners,mergeDFHereCut$estimated_count)){
    mismatch_NAsRecentYr_l[[count]] <- mergeDFHere
    names(mismatch_NAsRecentYr_l)[count] <- names(mismatch_remain_l)[i]
    count <- count + 1
  }
}
length(mismatch_NAsRecentYr_l) # 93
mismatch_remain_l <- mismatch_remain_l[!names(mismatch_remain_l) %in% names(mismatch_NAsRecentYr_l)]
length(mismatch_remain_l) # 41

#' CUs with missing values in past years but values present are identical and 
#' the ones with contrasting values:
mismatch_NAsPastYr_l <- list()
count <- 1
mismatch_diffYr_l <- list()
count_diff <- 1
for(i in 1:length(mismatch_remain_l)){
  # i <- 1
  mergeDFHere <- mismatch_remain_l[[i]]
  lastYear <- max(mergeDFHere$year[!is.na(mergeDFHere$spawners)])
  mergeDFHereCut <- mergeDFHere[mergeDFHere$year <= lastYear,]
  mergeDFHereCut <- mergeDFHereCut[!is.na(mergeDFHereCut$spawners),]
  if(identical(mergeDFHereCut$spawners,mergeDFHereCut$estimated_count)){
    mismatch_NAsPastYr_l[[count]] <- mergeDFHere
    names(mismatch_NAsPastYr_l)[count] <- names(mismatch_remain_l)[i]
    count <- count + 1
  }else{
    mismatch_diffYr_l[[count_diff]] <- mergeDFHereCut[mergeDFHereCut$spawners != mergeDFHereCut$estimated_count,]
    names(mismatch_diffYr_l)[count_diff] <- names(mismatch_remain_l)[i]
    count_diff <- count_diff + 1
  }
}
length(mismatch_NAsPastYr_l) # 1
mismatch_NAsPastYr_l
length(mismatch_diffYr_l)    # 40
mismatch_diffYr_l[[1]]
mismatch_diffYr_l[[2]]
mismatch_diffYr_l[[3]]

mismatch_remain_l <- mismatch_remain_l[!names(mismatch_remain_l) %in% names(mismatch_NAsPastYr_l)]
mismatch_remain_l <- mismatch_remain_l[!names(mismatch_remain_l) %in% names(mismatch_diffYr_l)]
length(mismatch_remain_l) # 0

#' Final lists:
identical_l       # the list of CUs that with identical spawner abundances (41)
mismatch_l        # the list of CUs with mistmatches (143) 
mismatch_allNAs_recruitsperspawner_l # only NAs in recruitsperspawner$spawners (8)
mismatch_allNAs_cuspawnerabundance_l # only NAs in cuspawnerabundance$estimated_count (1)
mismatch_NAsRecentYr_l #  CUs with missing values in recruitsperspawner$spawners only in the recent years, rest is identical (93)
mismatch_NAsPastYr_l   # CUs with missing values in past years but values present are identical (1)
mismatch_NAsPastYr_l   # CU with missing data in the past for recruitsperspawner$spawners (1)
mismatch_diffYr_l      # CUs with differing counts (40)



#
# --------







