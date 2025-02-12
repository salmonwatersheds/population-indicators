
#'******************************************************************************
#' The goal of the script is to update the hatchery release data for all region 
#' and species except Transboundary (TBR) and steelhead (SH). The script consists
#' in formatting the dataset sent by DFO (PSF_modified_SEP_releases.xlsx file) for
#' the PSE.
#' 
#' The data for TBR and SH is created elsewhere and uploaded to the database 
#' separately. However, it is imported here to edit the field "location_name_pse", 
#' and exported again. This convoluted workflow will eventually be removed and 
#' the hatchery data will be exported in one unique (PSF_modified_SEP_releases.xlsx)
#' file.
#' 
#' Files imported:
#' - conservationunits_decoder.csv               # List of CUs in the PSE
#' - PSF_modified_SEP_releases_2023.xlsx         # Source file sent by DFO to Eric
#' - SWP_hatchery_data_template.xlsx             # use for formatting the output dataset
#' - dataset384_hatchery_releases_YYYY-MM-DD.csv # The hatchery release dataset downloaded from the database (previously dataset384_output.csv)
#' 
#' Files exported:
#' - SWP_hatchery_data_DATE.xlsx  # The formatted and edited hatchery release data
#' - cuid_broodstock_multi.csv    # to check cases where there are multiple cuid_broodstock for a same release_site_name-release_stage-release_site_CUID-release_date combination.
#' - dataset384_hatchery_releases_SH_YYYY-MM-DD.csv   # the edited dataset384_hatchery_releases_YYYY-MM-DD.csv for SH
#' - dataset384_hatchery_releases_TBR_YYYY-MM-DD.csv  #  the edited dataset384_hatchery_releases_YYYY-MM-DD.csv for TBR
#' 
#'******************************************************************************

# 
rm(list = ls())
graphics.off()

options(java.parameters = "- Xmx1024m") # to be able to export a large excel file
#options(java.parameters = "- Xmx2G") # to be able to export a large excel file

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

subDir_projects <- subDir_projects_fun()

wds_l <- set_working_directories_fun(subDir = subDir_projects$hatchery_releases,
                                     Export_locally = F)
wd_head <- wds_l$wd_head
wd_project <- wds_l$wd_project
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS

wd_data_dropbox <- paste(wd_X_Drive1_PROJECTS,
                         wds_l$wd_project_dropbox,
                         "data",sep="/")

wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

wd_pop_indic_data_gis_dropbox <- gsub("input","gis",wd_pop_indic_data_input_dropbox)

# Define wd to access population-indicators/spawner-surveys/data/conservation-units.csv

wd_spawner_surveys_data <- paste(wd_X_Drive1_PROJECTS,
                                 "1_Active/Population Methods and Analysis/population-indicators/spawner-surveys",
                                 "data",sep="/")

wd_spawner_surveys_output <- paste(wd_X_Drive1_PROJECTS,
                                 "1_Active/Population Methods and Analysis/population-indicators/spawner-surveys",
                                 "output",sep="/")
library(xlsx)
library(readxl)
library(tidyverse)
library(stringr)
library(scales)  # for alpha
library(sf)
library(sp)     # for spDists() TERRA is the replacement

source(paste(wd_code,"functions.R",sep = "/"))

#
# Import datasets --------

#' * Import the hatchery template from wd_data as a list *
filePSF_l <- hatchery_template_fun(wd_data = wd_data,
                                   filePSFname = "SWP_hatchery_data_template.xlsx",
                                   asDataFrame = T)

#'* Import conservation-units.csv from wd_spawner_surveys_data *
#' This file comes from the PSF database all allows to match the DFO STOCK_CU_INDEX
#' with the PSF 'cuid' (or 'CUID') with the field 'cu_index' (= STOCK_CU_INDEX)
#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = F,
                                                   update_file_csv = F,
                                                   wd = wd_pop_indic_data_input_dropbox)

#'* Import the most recent version of PSF_modified_SEP_releases_DATE.xlsx in wd_data *
#' The is the file from DFO
DFO_df_all <- import_mostRecent_file_fun(wd = wd_data,
                                         pattern = "PSF_modified_SEP_releases")
head(DFO_df_all)
DFO_df_all <- as.data.frame(DFO_df_all)
nrow(DFO_df_all) # 35623

# Meta data:
# https://waves-vagues.dfo-mpo.gc.ca/Library/144345.pdf
# - STOCK_TYPE_CODE: H (hatchery) W (wild) M (mixed) and U (Unkonwn) C ?!
# - REARING_TYPE_CODE: H (hatchery, seapen, lakepen, rearing channel), W (wild unfed) F (wild fed) U (unkown)

DFO_df_all$STOCK_TYPE_CODE |> unique()


#'* Import the stream - GFE_ID data file from DFO *
#' (emailed from Wu Zhipeng, DFO, 09/04/2024)
#' There are different locations in the 2nd first sheets to we combine them into a unique dataframe
DFO_All_Streams_Segments <- read_xlsx(paste0(wd_spawner_surveys_data,"/DFO_All_Streams_Segments_20240408.xlsx"),
                                      sheet = "All Streams")

DFO_All_Streams_Segments2 <- read_xlsx(paste0(wd_spawner_surveys_data,"/DFO_All_Streams_Segments_20240408.xlsx"),
                                       sheet = "Stream Segments")

columns <- c("NME","ID","X_LONGT","Y_LAT")
DFO_All_Streams_Segments <- rbind(DFO_All_Streams_Segments[,columns],
                                  DFO_All_Streams_Segments2[,columns])
DFO_All_Streams_Segments <- unique(DFO_All_Streams_Segments)

rm(DFO_All_Streams_Segments2)

#'* Check if geo coordinates in DFO hatchery file match ones in NUSEDS$GFE_ID  *
d <- unique(DFO_df_all[,c("REL_GFE_ID","REL_LATITUDE","REL_LONGITUDE","STOCK_GFE_ID","STOCK_LATITUDE","STOCK_LONGITUDE")])
d

DFO_df_all$STOCK_LONGITUDE

# Import the NuSEDS data with PSF's cuid
nuseds <- import_mostRecent_file_fun(wd = paste0(wd_spawner_surveys_output,"/archive"),
                                     pattern = "nuseds_cuid_streamid_2024-04-19")
dim(nuseds) # 306823     45

#'* Import the shape files for the Region boundaries  *
# wd_maps_rg <- gsub("1_PROJECTS","5_DATA",wd_X_Drive1_PROJECTS) # files not up to date
# wd_maps_rg <- gsub("1_PROJECTS","5_DATA",wd_X_Drive1_PROJECTS)
# wd_maps_rg <- paste0(wd_pop_indic_data_gis_dropbox,"/se_boundary_regions")
regions_shp <- st_read(paste0(wd_pop_indic_data_gis_dropbox,"/se_boundary_regions/se_boundary_regions.shp")) %>%
  st_transform(crs = 4269)
unique(regions_shp$regionname)
sf_use_s2(FALSE) # so that st_intersects() and st_simplify() can be used
regions_shp_full <- regions_shp
regions_shp <- st_simplify(x = regions_shp, dTolerance = .002) # .001

plot(st_geometry(regions_shp_full[1,]))
plot(st_geometry(regions_shp[1,]))

#'* Import the geodatabase for the CU boundaries  *
# wd_maps_cu <- gsub("1_PROJECTS","5_DATA",wd_X_Drive1_PROJECTS) # files not up to date
# wd_maps_cu <- paste0(wd_maps_cu,"/CUs_Master/GDB")
CUs_gdb <- st_read(paste0(wd_pop_indic_data_gis_dropbox,"/pse_conservation_units/pse_conservation_units.gdb")) %>%
  st_transform(crs = 4269)
head(CUs_gdb)
unique(CUs_gdb$species)
unique(CUs_gdb$region)

CUs_gdb_full <- CUs_gdb      
CUs_gdb <- st_simplify(x = CUs_gdb_full, dTolerance = .002)

plot(st_geometry(CUs_gdb_full[1,]))
plot(st_geometry(CUs_gdb[1,]), add = F)

# FIX: there are values that are " " instead of being NA in CUs_gdb:
cond <- CUs_gdb$FULL_CU_IN == " " & !is.na(CUs_gdb$FULL_CU_IN)
sum(cond) # 89
d <- CUs_gdb[cond,c("region","species","cu_name_pse","CU_NAME","CUID","FULL_CU_IN")] |> as.data.frame()
d <- d[,colnames(d) != "Shape"]

cuids <- CUs_gdb$CUID[cond]
length(unique(cuids)) # 89
sum(is.na(cuids)) # 0

d$FULL_CU_IN_decoder <- sapply(cuids,function(cu){
  cond <- conservationunits_decoder$cuid == cu
  return(conservationunits_decoder$cu_index[cond])
})

d

for(r in 1:nrow(CUs_gdb[cond,])){
  cuid <- CUs_gdb$CUID[cond][r]
  FULL_CU_IN <- sapply(cuid,function(cu){
    cond <- conservationunits_decoder$cuid == cu
    return(conservationunits_decoder$cu_index[cond])
  })
  CUs_gdb$FULL_CU_IN[cond][r] <- FULL_CU_IN
}

CUs_gdb[cond,c("region","species","cu_name_pse","CU_NAME","CUID","FULL_CU_IN")]

#
# Fixes on coordinates and remove Cutthroat, Kokanee and rainbow trout -----
#

unique(DFO_df_all$SPECIES_NAME)

#'* remove Cutthroat *
cond_ct <- DFO_df_all$SPECIES_NAME == "Cutthroat"
unique(DFO_df_all$STOCK_CU_INDEX[cond_ct])#  NA

# remove Cutthroat
DFO_df_all <- DFO_df_all[!cond_ct,]
nrow(DFO_df_all) # 35016

#'* Remove Kokanee and rainbow trouts *
cond <- DFO_df_all$SPECIES_NAME == "Sockeye" & DFO_df_all$RUN_NAME == "Landlocked" &
  !is.na(DFO_df_all$RUN_NAME) # there are no NAs but just in case
sum(cond) # 9
DFO_df_all <- DFO_df_all[!cond,]

cond <- DFO_df_all$SPECIES_NAME == "Steelhead" & DFO_df_all$RUN_NAME == "Landlocked" &
  !is.na(DFO_df_all$RUN_NAME)
sum(cond) # 23
DFO_df_all <- DFO_df_all[!cond,]

#'* fix longitude > 0 *
#' certain X_LONGT are > 0 --> should be < 0
cond <- DFO_df_all$STOCK_LONGITUDE > 0 & !is.na(DFO_df_all$STOCK_LONGITUDE)
DFO_df_all$STOCK_LONGITUDE[cond] # none
DFO_df_all$STOCK_LONGITUDE[cond] <- DFO_df_all$STOCK_LONGITUDE[cond] * -1 # just in case
cond <- DFO_df_all$REL_LONGITUDE > 0 & !is.na(DFO_df_all$REL_LONGITUDE)
DFO_df_all$REL_LONGITUDE[cond] <- DFO_df_all$REL_LONGITUDE[cond] * -1
cond <- DFO_df_all$STOCK_LATITUDE < 0 & !is.na(DFO_df_all$STOCK_LATITUDE)
DFO_df_all$STOCK_LATITUDE[cond]
cond <- DFO_df_all$REL_LATITUDE < 0 & !is.na(DFO_df_all$REL_LATITUDE)
DFO_df_all$REL_LATITUDE[cond]

#'* Manual fix for Bentinck Arm North *
# details in PSE Data Check-In Meeting Notes ; 2024-09-05
# - the change was made by locating "Bentinck Arm North" in Google Map and 
# minimizing the number of digit to modify: adding +2 to the longitude provided
# a location vert close to the gogle maps's pin.
cond <- DFO_df_all$RELEASE_SITE_NAME == "Bentinck Arm N"
long_new <- DFO_df_all$REL_LONGITUDE[cond] + 2 |> unique()
long_new
# -126.97
DFO_df_all$REL_LONGITUDE[cond] <- long_new

#'* Manual fix for Bedwell Bay ?*
# These coordinates are wrong, they are way too far West (West VIMI in the ocean)
# while the Bedweel Bay is inland (Indian Arm)
# correct REL_LONGITUDE
cond <- DFO_df_all$REL_GFE_ID == 31496 & !is.na(DFO_df_all$REL_GFE_ID)
DFO_df_all$RELEASE_SITE_NAME[cond] |> unique()  # "Bedwell Bay"
DFO_df_all$REL_LATITUDE[cond] |> unique()       # 49.32
DFO_df_all$REL_LONGITUDE[cond] |> unique()      # -126.97
DFO_df_all$FACILITY_LATITUDE[cond] |> unique()  #  49.31983
DFO_df_all$FACILITY_LONGITUDE[cond] |> unique() # -122.9113
DFO_df_all$SPECIES_NAME[cond] |> unique()       # "Chinook" "Coho"
DFO_df_all[cond,]
DFO_df_all$REL_LONGITUDE[cond] <- DFO_df_all$FACILITY_LONGITUDE[cond]

#
# Find missing coordinates for STOCK and RELEASE SITE ------
#
#'* Deal with NAs in STOCK_CU_INDEX (= FULL_CU_IN in NuSEDS) *

# Number of STOCK_GFE_ID without coordinates
sum(is.na(DFO_df_all$STOCK_LONGITUDE )) # 453
STOCK_GFE_IDs <- unique(DFO_df_all[,c("STOCK_GFE_ID","STOCK_LONGITUDE","STOCK_LATITUDE")])
cond_STOCK_GFE_ID <- !is.na(STOCK_GFE_IDs$STOCK_GFE_ID)
cond_STOCK_coord <- !is.na(STOCK_GFE_IDs$STOCK_LATITUDE) & !is.na(STOCK_GFE_IDs$STOCK_LONGITUDE)
sum(cond_STOCK_GFE_ID & !cond_STOCK_coord) # 61
sum(cond_STOCK_GFE_ID & !cond_STOCK_coord)/sum(cond_STOCK_GFE_ID) # 0.11
STOCK_GFE_IDs[cond_STOCK_GFE_ID & !cond_STOCK_coord,]
for(gfe_id in STOCK_GFE_IDs$STOCK_GFE_ID[cond_STOCK_GFE_ID & !cond_STOCK_coord]){
  
  X_LONGT <- Y_LAT <- NA
  
  cond <- DFO_All_Streams_Segments$ID == gfe_id & !is.na(DFO_All_Streams_Segments$ID)
  if(any(cond)){
    X_LONGT <- DFO_All_Streams_Segments$X_LONGT[cond] |> as.numeric()
    Y_LAT <- DFO_All_Streams_Segments$Y_LAT[cond] |> as.numeric()
    GFE_NAME <- DFO_All_Streams_Segments$NME[cond]

  }else{
    cond <- nuseds$GFE_ID == gfe_id & !is.na(nuseds$GFE_ID)
    if(any(cond)){
      X_LONGT <- nuseds$X_LONGT[cond] |> unique()
      Y_LAT <- nuseds$Y_LAT[cond] |> unique()
      GFE_NAME <- nuseds$SYSTEM_SITE[cond] |> unique()
      
    }else{
      print("STOCK_GFE_ID not in DFO_All_Streams_Segments nor in NuSEDS")
    }
  }
  
  if(!is.na(X_LONGT) & !is.na(Y_LAT)){
    cond <- DFO_df_all$STOCK_GFE_ID == gfe_id & !is.na(DFO_df_all$STOCK_GFE_ID)
    DFO_df_all$STOCK_LATITUDE[cond] <- Y_LAT
    DFO_df_all$STOCK_LONGITUDE[cond] <- X_LONGT
    DFO_df_all$STOCK_GFE_NAME[cond] <- GFE_NAME
  }
}
sum(is.na(DFO_df_all$STOCK_LONGITUDE )) # 220

# same for REL_GFE_ID
sum(is.na(DFO_df_all$REL_LONGITUDE )) # 4449
REL_GFE_IDs <- unique(DFO_df_all[,c("REL_GFE_ID","REL_LONGITUDE","REL_LATITUDE")])
cond_REL_GFE_ID <- !is.na(REL_GFE_IDs$REL_GFE_ID)
cond_REL_coord <- !is.na(REL_GFE_IDs$REL_LATITUDE) & !is.na(REL_GFE_IDs$REL_LONGITUDE)
sum(cond_REL_GFE_ID & !cond_REL_coord) # 481
sum(cond_REL_GFE_ID & !cond_REL_coord)/sum(cond_REL_GFE_ID) # 0.35
REL_GFE_IDs[cond_REL_GFE_ID & !cond_REL_coord,]
for(gfe_id in REL_GFE_IDs$REL_GFE_ID[cond_REL_GFE_ID & !cond_REL_coord]){
  
  X_LONGT <- Y_LAT <- NA
  
  cond <- DFO_All_Streams_Segments$ID == gfe_id & !is.na(DFO_All_Streams_Segments$ID)
  if(any(cond)){
    X_LONGT <- DFO_All_Streams_Segments$X_LONGT[cond] |> as.numeric()
    Y_LAT <- DFO_All_Streams_Segments$Y_LAT[cond] |> as.numeric()
    GFE_NAME <- DFO_All_Streams_Segments$NME[cond]
    
  }else{
    cond <- nuseds$GFE_ID == gfe_id & !is.na(nuseds$GFE_ID)
    if(any(cond)){
      X_LONGT <- nuseds$X_LONGT[cond] |> unique()
      Y_LAT <- nuseds$Y_LAT[cond] |> unique()
      GFE_NAME <- nuseds$SYSTEM_SITE[cond] |> unique()
      
    }else{
      print("REL_GFE_ID not in DFO_All_Streams_Segments nor in NuSEDS")
    }
  }
  
  if(!is.na(X_LONGT) & !is.na(Y_LAT)){
    cond <- DFO_df_all$REL_GFE_ID == gfe_id & !is.na(DFO_df_all$REL_GFE_ID)
    DFO_df_all$REL_LATITUDE[cond] <- Y_LAT
    DFO_df_all$REL_LONGITUDE[cond] <- X_LONGT
    # DFO_df_all$REL_GFE_NAME[cond] <- GFE_NAME   # the field does not exist
  }
}
sum(is.na(DFO_df_all$REL_LONGITUDE )) # 2825

#
#
# Find the missing STOCK_CU_INDEX and REL_CU_INDEX (== FULL_CU_IN) -------
#

# find the missing STOCK_CU_INDEX
STOCK_CU_INDEX_NA <- find_CU_hatchery_fun(hatchery_DFO = DFO_df_all, 
                                          nuseds_cleaned = nuseds,
                                          cu_index_type = "STOCK_CU_INDEX", 
                                          DFO_All_Streams_Segments = DFO_All_Streams_Segments, 
                                          CUs_gdb = CUs_gdb, regions_shp = regions_shp)

head(STOCK_CU_INDEX_NA)
table(STOCK_CU_INDEX_NA$comment_broodstock)

# % found: 90.2%
100 - sum(grepl("FAILURE",STOCK_CU_INDEX_NA$comment_broodstock))/nrow(STOCK_CU_INDEX_NA) * 100 # 90.21739
sum(!is.na(STOCK_CU_INDEX_NA$STOCK_CU_INDEX))/nrow(STOCK_CU_INDEX_NA) * 100   # 46.7
sum(!is.na(STOCK_CU_INDEX_NA$cuid_broodstock))/nrow(STOCK_CU_INDEX_NA) * 100  # 89.7
sum(!is.na(STOCK_CU_INDEX_NA$cuid_broodstock) | !is.na(STOCK_CU_INDEX_NA$STOCK_CU_INDEX))/nrow(STOCK_CU_INDEX_NA) * 100  # 90.21739



# find the missing STOCK_CU_INDEX (takes a few minutes)
REL_CU_INDEX_NA <- find_CU_hatchery_fun(hatchery_DFO = DFO_df_all, 
                                          nuseds_cleaned = nuseds,
                                          cu_index_type = "REL_CU_INDEX", 
                                          DFO_All_Streams_Segments = DFO_All_Streams_Segments, 
                                          CUs_gdb = CUs_gdb, regions_shp = regions_shp)

head(REL_CU_INDEX_NA)
table(REL_CU_INDEX_NA$comment_release_site)

# % not found: 23.9%
100 - sum(grepl("FAILURE",REL_CU_INDEX_NA$comment_release_site))/nrow(REL_CU_INDEX_NA) * 100 #  76.07223
sum(!is.na(REL_CU_INDEX_NA$REL_CU_INDEX))/nrow(REL_CU_INDEX_NA) * 100       # 65.538
sum(!is.na(REL_CU_INDEX_NA$cuid_release_site))/nrow(REL_CU_INDEX_NA) * 100  # 75.77126
sum(!is.na(REL_CU_INDEX_NA$cuid_release_site) | !is.na(REL_CU_INDEX_NA$REL_CU_INDEX))/nrow(REL_CU_INDEX_NA) * 100  #  76.07223

cond <- is.na(REL_CU_INDEX_NA$cuid_release_site) & is.na(REL_CU_INDEX_NA$REL_CU_INDEX) & !grepl("FAILURE",REL_CU_INDEX_NA$comment_release_site)
REL_CU_INDEX_NA[cond,]


# CHECK: there should not be NA comments: --> OK
sum(is.na(STOCK_CU_INDEX_NA$comment_broodstock))
sum(is.na(REL_CU_INDEX_NA$comment_release_site))

# CHECK: the cases below should not have STOCK coordinates --> OK
cond <- STOCK_CU_INDEX_NA$comment_broodstock == "GFE_ID not in NuSEDS; GFE_ID not in DFO streams file - FAILURE" 
STOCK_CU_INDEX_NA[cond,]

# CHECK:
#' The two CUs to fix in priority:
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1732751072916579?thread_ts=1729891432.329409&cid=CJ5RVHVCG
#' - Columbia SEL 1300  Osoyoos SEL-01-01 --> STOCK_NAME = Okanagan R, STOCK_POP_ID = NA, STOCK_CU_INDEX = NA
#' - Columbia  CK  301 Okanagan      CK-1 --> STOCK_NAME = Okanagan R, STOCK_POP_ID = 48435, STOCK_CU_INDEX = NA
cond <- STOCK_CU_INDEX_NA$cuid == 1300 & !is.na(STOCK_CU_INDEX_NA$cuid)
STOCK_CU_INDEX_NA[cond,] # OK
cond <- STOCK_CU_INDEX_NA$cuid == 301 & !is.na(STOCK_CU_INDEX_NA$cuid)
STOCK_CU_INDEX_NA[cond,] # OK

#
# Update the hatchery DFO file accordingly -----
#

# replace the values in DFO_df
DFO_df <- DFO_df_all

DFO_df$cuid_broodstock <- DFO_df$comment_broodstock <- NA
for(r in 1:nrow(STOCK_CU_INDEX_NA)){
  # r <- 1
  print(r)
  
  SPECIES_NAME <- STOCK_CU_INDEX_NA$SPECIES_NAME[r]
  STOCK_NAME <- STOCK_CU_INDEX_NA$STOCK_NAME[r]
  RUN_NAME <- STOCK_CU_INDEX_NA$RUN_NAME[r]
  STOCK_GFE_ID <- STOCK_CU_INDEX_NA$STOCK_GFE_ID[r]
  STOCK_LONGITUDE <- STOCK_CU_INDEX_NA$STOCK_LONGITUDE[r]
  
  cond1 <- DFO_df$SPECIES_NAME == SPECIES_NAME &
    DFO_df$STOCK_NAME == STOCK_NAME &
    DFO_df$RUN_NAME == RUN_NAME
    
  if(is.na(STOCK_GFE_ID)){
    cond2 <- is.na(DFO_df$STOCK_GFE_ID)
  }else{
    cond2 <- !is.na(DFO_df$STOCK_GFE_ID) & DFO_df$STOCK_GFE_ID == STOCK_GFE_ID
  }
  
  cond <- cond1 & cond2
  
  if(!any(cond)){
    print("Match not found; break")
    break
    
  }else if(length(unique(DFO_df$STOCK_LONGITUDE[cond])) > 1){
    print("More than one location; break")
    break
    
  }else{
    
    DFO_df$STOCK_CU_INDEX[cond] <- STOCK_CU_INDEX_NA$STOCK_CU_INDEX[r]
    DFO_df$STOCK_GFE_ID[cond] <- STOCK_CU_INDEX_NA$STOCK_GFE_ID[r]
    DFO_df$STOCK_LATITUDE[cond] <- STOCK_CU_INDEX_NA$STOCK_LATITUDE[r]
    DFO_df$STOCK_LONGITUDE[cond] <- STOCK_CU_INDEX_NA$STOCK_LONGITUDE[r]
    DFO_df$comment_broodstock[cond] <- STOCK_CU_INDEX_NA$comment_broodstock[r]
    DFO_df$cuid_broodstock[cond] <- STOCK_CU_INDEX_NA$cuid_broodstock[r]
  }
}

# same for REL_CU_INDEX_NA
DFO_df$cuid_release_site <- DFO_df$comment_release_site <- NA
for(r in 1:nrow(REL_CU_INDEX_NA)){
  # r <- 1
  print(r)
  
  SPECIES_NAME <- REL_CU_INDEX_NA$SPECIES_NAME[r]
  REL_CU_NAME <- REL_CU_INDEX_NA$REL_CU_NAME[r]
  RUN_NAME <- REL_CU_INDEX_NA$RUN_NAME[r]
  REL_GFE_ID <- REL_CU_INDEX_NA$REL_GFE_ID[r]
  REL_LONGITUDE <- REL_CU_INDEX_NA$REL_LONGITUDE[r]
  REL_LATITUDE <- REL_CU_INDEX_NA$REL_LATITUDE[r]

  cond1 <- DFO_df$SPECIES_NAME == SPECIES_NAME &
    DFO_df$RUN_NAME == RUN_NAME
  
  if(is.na(REL_CU_NAME)){
    cond2 <- is.na(DFO_df$REL_CU_NAME)
  }else{
    cond2 <- !is.na(DFO_df$REL_CU_NAME) & DFO_df$REL_CU_NAME == REL_CU_NAME
  }
  
  if(is.na(REL_GFE_ID)){
    cond3 <- is.na(DFO_df$REL_GFE_ID)
  }else{
    cond3 <- !is.na(DFO_df$REL_GFE_ID) & DFO_df$REL_GFE_ID == REL_GFE_ID
  }
  
  if(is.na(REL_LONGITUDE)){
    cond4 <- is.na(DFO_df$REL_LONGITUDE)
  }else{
    cond4 <- !is.na(DFO_df$REL_LONGITUDE) & round(DFO_df$REL_LONGITUDE,4) == round(REL_LONGITUDE,4)
  }
  
  if(is.na(REL_LATITUDE)){
    cond5 <- is.na(DFO_df$REL_LATITUDE)
  }else{
    cond5 <- !is.na(DFO_df$REL_LATITUDE) & round(DFO_df$REL_LATITUDE,4) == round(REL_LATITUDE,4)
  }
  
  cond <- cond1 & cond2 & cond3 & cond4 & cond5
  
  if(!any(cond)){
    print("Match not found; break")
    break
    
  }else if(length(unique(DFO_df$REL_LONGITUDE[cond])) > 1){
    print("More than one location; break")
    break
    
  }else{
    
    DFO_df$REL_CU_INDEX[cond] <- REL_CU_INDEX_NA$REL_CU_INDEX[r]
    DFO_df$REL_GFE_ID[cond] <- REL_CU_INDEX_NA$REL_GFE_ID[r]
    DFO_df$REL_LATITUDE[cond] <- REL_CU_INDEX_NA$REL_LATITUDE[r]
    DFO_df$REL_LONGITUDE[cond] <- REL_CU_INDEX_NA$REL_LONGITUDE[r]
    DFO_df$comment_release_site[cond] <- REL_CU_INDEX_NA$comment_release_site[r]
    DFO_df$cuid_release_site[cond] <- REL_CU_INDEX_NA$cuid_release_site[r]
  }
}

#
# Additional fixes ------
#

#'* 1) Attribute SEL-21-02 to the correct CU *  
#' TODO: ask how to proceed at Pop meeting 03/02/2025
#' The CU was split in to three CUs by PSF: 
cond <- grepl("SEL-21-02",conservationunits_decoder$cu_index)
conservationunits_decoder[cond,]

cond <- DFO_df$STOCK_CU_INDEX == "SEL-21-02" & !is.na(DFO_df$STOCK_CU_INDEX)
DFO_df[cond,c("SPECIES_NAME","RUN_NAME","STOCK_NAME","STOCK_POP_NAME","STOCK_GFE_NAME","STOCK_POP_ID","STOCK_CU_ID","STOCK_CU_INDEX","STOCK_GFE_ID")] |> unique()

# as in https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41102356.pdf
# Babine/Onerka               Babine  SEL-21-02-EW   Early Wild
# Nilkitkwa                Nilkitkwa  SEL-21-02-LW   Late Wild
# Babine (enhanced) Babine (enhanced) SEL-21-02-F    Fulton
#                                     SEL-21-02-P    Pinkut --> not in the decoder
cond <- DFO_df$SPECIES_NAME == "Sockeye" & grepl("Fulton",DFO_df$STOCK_NAME)
DFO_df[cond,]$STOCK_CU_INDEX <- "SEL-21-02-F"
DFO_df[cond,]$STOCK_GFE_ID |> unique()
DFO_df[cond,]$REL_GFE_ID |> unique()

#' set REL_CU_INDEX to STOCK_CU_INDEX when the REL_GFE_ID == a STOCK_GFE_ID associated
#' with STOCK_CU_INDEX == "SEL-21-02-F" --> does not change anything because the 
#' REL_GFE_ID for SEL-21-02-F are in in STOCK_GFE_ID
cond2 <- DFO_df$SPECIES_NAME == "Sockeye" &
  DFO_df$STOCK_CU_INDEX == "SEL-21-02-F" & !is.na(DFO_df$STOCK_CU_INDEX) &
  DFO_df$REL_GFE_ID %in% unique(DFO_df[cond,]$STOCK_GFE_ID) & !is.na(DFO_df$REL_GFE_ID)
unique(DFO_df$REL_GFE_ID[cond2]) # 501254   2111   2110
unique(DFO_df$REL_CU_INDEX[cond2]) # 
DFO_df$REL_CU_INDEX[cond2] <- "SEL-21-02-F"


#'* 2) attribute the cuid to STOCK_CU_IN and REL_CU_IN and vice versa *
#' 2.a STOCK_CU_INDEX --> cuid_broodstock
cols_here <- c("cuid_broodstock","STOCK_CU_INDEX")
cond <- is.na(DFO_df[,cols_here[1]]) & !is.na(DFO_df[,cols_here[2]])
data_temp <- unique(DFO_df[cond,cols_here])
nrow(data_temp) # 166
for(scui in unique(data_temp[,cols_here[2]])){
  # scui <- unique(data_temp[,cols_here[2]])[1]
  cond_scui <- conservationunits_decoder$cu_index == scui & !is.na( conservationunits_decoder$cu_index)
  if(any(cond_scui)){
    cuid <- conservationunits_decoder$cuid[cond_scui]
    cond <- DFO_df[,cols_here[2]] == scui & !is.na(DFO_df[,cols_here[2]])
    DFO_df[cond,cols_here[1]] <- cuid
    print(paste(scui,"-->",cuid))
  }else{
    print(paste(scui,"--> NA"))
  }
}

#' 2.b cuid_broodstock --> STOCK_CU_INDEX
cond <- !is.na(DFO_df[,cols_here[1]]) & is.na(DFO_df[,cols_here[2]])
data_temp <- unique(DFO_df[cond,cols_here])
nrow(data_temp) # 21
for(cuid in unique(data_temp[,cols_here[1]])){
  # cuid <- unique(data_temp[,cols_here[1]])[1]
  cond_cuid <- conservationunits_decoder$cuid == cuid & !is.na(conservationunits_decoder$cuid)
  if(any(cond_cuid)){
    cu_index <- conservationunits_decoder$cu_index[cond_cuid]
    if(is.na(cu_index)){
      print(paste(cuid,"--> NA in decoder"))
    }else{
      cond <- DFO_df[,cols_here[1]] == cu_index & !is.na(DFO_df[,cols_here[1]])
      DFO_df[cond,cols_here[2]] <- cu_index
      print(paste(cuid,"-->",cu_index))
    }
  }else{
    print(paste(cuid,"--> NA"))
  }
}

#' 2.c REL_CU_INDEX --> cuid_release_site
cols_here <- c("cuid_release_site","REL_CU_INDEX")
cond <- is.na(DFO_df[,cols_here[1]]) & !is.na(DFO_df[,cols_here[2]])
data_temp <- unique(DFO_df[cond,cols_here])
nrow(data_temp) # 163
for(scui in unique(data_temp[,cols_here[2]])){
  # scui <- unique(data_temp[,cols_here[2]])[1]
  cond_scui <- conservationunits_decoder$cu_index == scui & !is.na( conservationunits_decoder$cu_index)
  if(any(cond_scui)){
    cuid <- conservationunits_decoder$cuid[cond_scui]
    cond <- DFO_df[,cols_here[2]] == scui & !is.na(DFO_df[,cols_here[2]])
    DFO_df[cond,cols_here[1]] <- cuid
    print(paste(scui,"-->",cuid))
  }else{
    print(paste(scui,"--> NA"))
  }
}

#' 2.d cuid_broodstock --> STOCK_CU_INDEX
cond <- !is.na(DFO_df[,cols_here[1]]) & is.na(DFO_df[,cols_here[2]])
data_temp <- unique(DFO_df[cond,cols_here])
nrow(data_temp) # 19
for(cuid in unique(data_temp[,cols_here[1]])){
  # cuid <- unique(data_temp[,cols_here[1]])[1]
  cond_cuid <- conservationunits_decoder$cuid == cuid & !is.na(conservationunits_decoder$cuid)
  if(any(cond_cuid)){
    cu_index <- conservationunits_decoder$cu_index[cond_cuid]
    if(is.na(cu_index)){
      print(paste(cuid,"--> NA in decoder"))
    }else{
      cond <- DFO_df[,cols_here[1]] == cu_index & !is.na(DFO_df[,cols_here[1]])
      DFO_df[cond,cols_here[2]] <- cu_index
      print(paste(cuid,"-->",cu_index))
    }
  }else{
    print(paste(cuid,"--> NA"))
  }
}

sum(is.na(DFO_df$cuid_broodstock))   # 987

sum(is.na(DFO_df$STOCK_CU_INDEX))     # 1870
sum(is.na(DFO_df_all$STOCK_CU_INDEX)) # 2342

sum(is.na(DFO_df$REL_CU_INDEX))      # 3576
sum(is.na(DFO_df_all$REL_CU_INDEX))  # 8548


cond <- is.na(DFO_df$cuid_release_site) & !is.na(DFO_df$REL_CU_INDEX)
unique(DFO_df[,c("cuid_broodstock","STOCK_CU_INDEX","cuid_release_site","REL_CU_INDEX")])



#'* 3) remaining REL_CU_INDEX and cuid_release_site with NA <-- STOCK_CU_INDEX & cuid_broodstock *
#' Steph and Eric: "we assume that REL_CU_INDEX = STOCK_CU_INDEX when NAs are present 
#' in REL_CU_INDEX".
nrow(DFO_df[is.na(DFO_df$REL_CU_INDEX),]) # 3576
cond <- is.na(DFO_df$REL_CU_INDEX) & is.na(DFO_df$cuid_release_site)
sum(cond) # 1947
DFO_df$REL_CU_INDEX[cond] |> unique(cond)
DFO_df$STOCK_CU_INDEX[cond] |> unique(cond)
DFO_df$cuid_broodstock[cond] |> unique(cond)

DFO_df$REL_CU_INDEX[cond] <- DFO_df$STOCK_CU_INDEX[cond]
DFO_df$cuid_release_site[cond] <- DFO_df$cuid_broodstock[cond]


#'* 4) "Seapen" released (RELEASE_STAGE_NAME == "Seapen") *
#' Steph and Eric: "some cases REL_CU_INDEX = blank is for seapen released, since
#' maybe in those cases the release can’t be assigned to a CU? But in those cases
#' do we assume the fish will return to the broodstock CU? Eric: Yes."
#' TODO: deal with it later.
# View(DFO_df_all[grepl("Seapen",DFO_df_all$RELEASE_STAGE_NAME),])


#'* 5) Remove the CUs not present in PSE database *
#' in STOCK_CU_INDEX and REL_CU_INDEX that are not present in 
#' the PSF database 
#' - CK-9002, CK-9005, CK-9006, CK-9007, CK-9008 # Eric: These aren't real CUs. They are hatchery-only population so we don't show them on the PSE
#' - SEL-15-03                                   # Eric: This a CU that the central coast Nations said is not a CU. So we don't show on the PSE
#' - CM-9004                                     # Eric: a CU that no longer exists
CUToRemove <- c("CK-9002","CK-9005","CK-9006","CK-9007","CK-9008","SEL-15-03","CM-9004")
cond1 <- DFO_df$STOCK_CU_INDEX %in% CUToRemove
DFO_df[cond1,]$STOCK_CU_INDEX |> unique()
sum(cond1) # 747

cond2 <- DFO_df$REL_CU_INDEX %in% CUToRemove
DFO_df[cond2,]$REL_CU_INDEX |> unique()
sum(cond2) # 603

sum(cond1 & cond2) # 529
sum(cond1 | cond2) # 821

DFO_df <- DFO_df[!(cond1 | cond2),]
nrow(DFO_df) # 34163 34061 34071 31871 31830


sum(is.na(DFO_df$cuid_broodstock))   # 240

sum(is.na(DFO_df$STOCK_CU_INDEX))     # 1870
sum(is.na(DFO_df_all$STOCK_CU_INDEX)) # 2342

sum(is.na(DFO_df$cuid_release_site)) # 223
sum(is.na(DFO_df$REL_CU_INDEX))      # 1854
sum(is.na(DFO_df_all$REL_CU_INDEX))  # 8548


cond <- is.na(DFO_df$cuid_release_site) & !is.na(DFO_df$REL_CU_INDEX)
unique(DFO_df[cond,c("cuid_broodstock","STOCK_CU_INDEX","cuid_release_site","REL_CU_INDEX")])

# CHECK:
cond <- is.na(DFO_df$cuid_broodstock) & !is.na(DFO_df$cuid_release_site)
d <- unique(DFO_df[cond,c("SPECIES_NAME","RUN_NAME",
                          "cuid_broodstock","STOCK_CU_INDEX",
                          "cuid_release_site","REL_CU_INDEX",
                          "STOCK_GFE_ID","REL_GFE_ID")])
d$REL_cu_name_pse <- sapply(d$cuid_release_site,function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(unique(conservationunits_decoder$cu_name_pse[cond]))
})

d

#'* 6) Remove observation without cuid_broodstock *
sum(is.na(DFO_df$cuid_broodstock)) # 240

# check those with: (there should not be any BUT there is the unsolved case with SEL-21-02-P)
cond <- !is.na(DFO_df$STOCK_CU_INDEX) & is.na(DFO_df$cuid_broodstock)
sum(cond) # 138
unique(DFO_df[cond,c("RUN_NAME","STOCK_GFE_NAME","cuid_broodstock","STOCK_CU_INDEX","cuid_release_site","REL_CU_INDEX")])
# cuid_broodstock STOCK_CU_INDEX cuid_release_site REL_CU_INDEX
#              NA      SEL-21-02                NA    SEL-21-02

# check: there should not be any case like: OK
cond <- !is.na(DFO_df$cuid_broodstock) & is.na(DFO_df$cuid_release_site)
sum(cond) # 0 

# 
cond <- is.na(DFO_df$cuid_broodstock)
sum(cond) # 240
unique(DFO_df[cond,c("RUN_NAME","STOCK_GFE_NAME","cuid_broodstock","STOCK_CU_INDEX","cuid_release_site","REL_CU_INDEX")])
DFO_df <- DFO_df[!cond,]


# Check:
sum(is.na(DFO_df$cuid_broodstock))   # 0
sum(is.na(DFO_df$cuid_release_site)) # 0
nrow(DFO_df)     # 33923
nrow(DFO_df_all) # 34984


#'* 7) Find coordinates of certain release site manually ? NOT DONE YET *

cond <- is.na(DFO_df$REL_LATITUDE) & !is.na(DFO_df$RELEASE_SITE_NAME)
cols <- c("cuid_release_site","RELEASE_SITE_NAME","REL_LATITUDE","REL_LONGITUDE")
d <- unique(DFO_df[cond,cols])
d$region <- sapply(d$cuid_release_site,function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(unique(conservationunits_decoder$region[cond]))
})
d <- d[,c("region","RELEASE_SITE_NAME","REL_LATITUDE","REL_LONGITUDE")] |> unique()
nrow(d) # 281
head(d, 20)


#
# Create SWP_hatchery_data_DATE.xlsx  ----------
#
#' Create a dataframe with the name of the columns in PSF_modified_SEP_releases_DATE.xlsx
#' and corresponding column names and sheets in the survey file SWP_hatchery_data_...xlsx
# matchCol_df <- matching_columns_fun(wd_data = wd_data,
#                                     wd_spawner_surveys_data = wd_spawner_surveys_data,
#                                     DFO_df = DFO_df)

# make a copy of filePSF_l that is going to be filled
filePSFnew_l <- filePSF_l

#' Make a dataframe to report the cases where there are multiple cuid_broodstock 
#' for a same release_site_name - release_stage - release_site_CUID - release_date
#' combination --> to send to Katy,
cuid_broodstock_multi <- NULL

# Fill filePSF_l with new data (takes a couple minutes)
for(sheet_i in 2:length(names(filePSF_l))){   # Skip the 1st sheet
  
  # sheet_i <- 3
  sheetName <- names(filePSF_l)[sheet_i]
  sheetNew <- filePSF_l[[sheet_i]]
  
  #
  if(sheetName == "DataEntry_facilities"){ # sheet 2
    
    print("*** DataEntry_facilities ***")
    
    #
    field_DFO <- c("PROGRAM_CODE","PROJ_NAME","FACILITY_NAME","FACILITY_LATITUDE","FACILITY_LONGITUDE","START_DATE","END_DATE")
    field_PSF <- c("program","project","facilityname","facility_latitude","facility_longitude","startyear","endyear" )
    
    # sheetNew <- DFO_df[,field_DFO] |> unique()
    sheetNew <- DFO_df[,field_DFO] |> unique()

    colnames(sheetNew) <- field_PSF
    
    # only keep the year, e.g., '19920814' --> '1992'
    for(ycol in c("startyear","endyear")){
      # ycol <- c("startyear","endyear")[1]
      date <- sapply(X = sheetNew[,ycol], 
                     FUN = function(d){substr(x = d,start = 1, stop = 4)})
      date <- as.numeric(date)
      allDates <- date %in% 1900:2100
      if(!all(allDates)){
        print(paste0("The following dates in '",sheetName,"/",ycol,"' have to be checked:"))
        print(date[!allDates])
      }
      sheetNew[,ycol] <- date
    }
    
    #' Define the starting and ending years for each program-project-facilityname 
    #' combination
    sheetNew_l <- list()
    count <- 1
    for(fn in unique(sheetNew$facilityname)){
      # fn <- unique(sheetNew$facilityname)[1]
      sheetNew_fn <- sheetNew[sheetNew$facilityname == fn,]
      for(pg in unique(sheetNew_fn$program)){
        # pg <- unique(sheetNew_fn$program)[1]
        sheetNew_fn_pr <- sheetNew_fn[sheetNew_fn$program == pg,]
        for(pj in unique(sheetNew_fn_pr$project)){
          # pj <- unique(sheetNew_fn_pr$project)[1]
          sheetNew_fn_pr_pj <- sheetNew_fn_pr[sheetNew_fn_pr$project == pj,]
          startyear_min <- min(sheetNew_fn_pr_pj$startyear)
          endyear_max <- max(sheetNew_fn_pr_pj$endyear)
          slice <- sheetNew_fn_pr_pj[1,]
          slice$startyear <- startyear_min
          slice$endyear <- endyear_max
          sheetNew_l[[count]] <- slice
          count <- count + 1
        }
      }
    }
    sheetNew <- do.call(rbind,sheetNew_l)
    
    #' In "program" replace the acronyms by the full names
    sheetNew$program <- program_acronym_fun(prog_acro =  sheetNew$program)
    # sheetNew[sheetNew$program == "NA",]  # QUESTION: is that normal?
    # sheetNew[sheetNew$facilityname == "Omega Pacific H",]
    
    #' Define facilityid/facilityID:
    #' Katy: facilityid uniquely identifies “program-project-facilityname-facility_latitude-facility_longitude-startyear-endyear”
    #' Katy: facilityid only lives in this file so it can be defined here.
    sheetNew$facilityid <- 1:nrow(sheetNew)

    # Reorder columns
    sheetNew <- sheetNew[,c("facilityid",field_PSF)]
    
    # check if multiple coordinate values are attributed to a unique facilityname
    sheetNew_cut <- sheetNew[,c("facilityname","facility_latitude","facility_longitude")] |> unique()
    
   if(any(duplicated(sheetNew_cut$facilityname))){
     print("WARNING: The following facility has multiple coordinate values (and should not):")
     cond <- duplicated(sheetNew_cut$facilityname)
     d <- sheetNew_cut[cond,]
     d  <- d[order(d$facilityname),]
     print(d)
   }
    
  }else if(sheetName == "DataEntry_facilitiescuids"){
    
    print("*** DataEntry_facilitiescuids ***")
    
    #' find the facilityid/ID corresponding to each unique combination of 
    #' program-project-facilityname:
    field_DFO <- c("PROGRAM_CODE","PROJ_NAME","FACILITY_NAME","REL_CU_INDEX","cuid_release_site")
    sheetNew <- DFO_df[,field_DFO]
    colnames(sheetNew) <- c("program","project","facilityname","cu_index_release_site","cuid_release_site") # "cu_index" in conservationunits_decoder = the CU_INDEX = FULL_CU_IN in nuseds 
    sheetNew$program <- program_acronym_fun(prog_acro =  sheetNew$program)
    sheetNew <- merge(x = sheetNew, 
                      y = filePSFnew_l$DataEntry_facilities[,c("facilityid","program","project","facilityname")],
                      by = c("program","project","facilityname"), 
                      all = T)
    
    sheetNew$cu_name_pse <- sapply(sheetNew$cuid_release_site, FUN = function(cuid){
      cond <- conservationunits_decoder$cuid == cuid & !is.na(conservationunits_decoder$cuid)
      if(any(cond)){
        out <- conservationunits_decoder$cu_name_pse[cond]
      }else{
        out <- NA
      }
      return(out)
    })
    
    # remove unnecessary columns and update their names
    sheetNew <- sheetNew[,c("facilityid","cuid_release_site","cu_index_release_site","cu_name_pse")]

    # order rows
    sheetNew <- sheetNew[order(sheetNew$facilityid),]
    
    # remove duplicated rows
    sheetNew <- unique(sheetNew)

    # retain the desired columns
    # sheetNew <- sheetNew[,c("facilityID","CUID")]
    
  }else if(sheetName == "DataEntry_releases"){
    
    print("*** DataEntry_releases ***")

    col_DFO <- c("SPECIES_NAME",
                 "cuid_broodstock","STOCK_CU_INDEX",
                 "cuid_release_site","REL_CU_INDEX",
                 "REL_GFE_ID","REL_LATITUDE","REL_LONGITUDE","RELEASE_SITE_NAME","RELEASE_STAGE_NAME",
                 "FACILITY_NAME","PROGRAM_CODE","PROJ_NAME",
                "RELEASE_YEAR","TotalRelease")
    col_PSF <- c("species_name",
                 "cuid_broodstock","cu_index_broodstock",
                 "cuid_release_site","cu_index_release_site",
                 "release_site_GFE_ID","release_site_latitude","release_site_longitude","release_site_name","release_stage",
                 "facilityname","program","project",
                 "release_date","total_release")
    
    sheetNew <- DFO_df[,col_DFO]
    colnames(sheetNew) <- col_PSF
    
    #' there are duplicated rows:
    #' it is due to the non-included fields like "MRP_TAGCODE","RELEASE_COMMENT"
    #' hence the code below to troubleshoot
    nrow(sheetNew) # 33923
    nrow(unique(sheetNew)) # 33788
    nrow(sheetNew) - nrow(unique(sheetNew)) # 135
    sheetNew <- unique(sheetNew)
    
    #' there are duplicated rows:
    #' TODO: remove them from now but deal with it with Katy and co.
    # nrow(sheetNew) # 31830
    # nrow(DFO_df)   # 31830
    # sum(duplicated(sheetNew)) # 134
    # sum(duplicated(DFO_df))   # 0
    # 
    # rowsDuplicating <- which(duplicated(sheetNew) | duplicated(sheetNew, fromLast = TRUE))
    # View(sheetNew[rowsDuplicating,])
    # View(DFO_df[rowsDuplicating,field_DFO])
    # View(DFO_df[rowsDuplicating,])
    # 
    # sum(duplicated(DFO_df)) # 0
    # sum(duplicated(DFO_df[,colnames(DFO_df) != "MRP_TAGCODE"]))     # 35
    # sum(duplicated(DFO_df[,colnames(DFO_df) != "RELEASE_COMMENT"])) # 0   ?!
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","RELEASE_COMMENT")]))  # 48
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","BROOD_YEAR")]))       # 41
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","AVE_WEIGHT")]))       # 40
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","NoTagClip")]))        # 39
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","RELEASE_COMMENT","BROOD_YEAR","AVE_WEIGHT","NoTagClip")]))  # 69
    # 
    # remainingCol <- colnames(DFO_df)[! colnames(DFO_df) %in% c(field_DFO,field_DFO_forFacilityID)]
    # for(c in remainingCol){
    #   DFO_df_cut <- DFO_df[,colnames(DFO_df) != c]
    #   if(sum(duplicated(DFO_df_cut)) > 0){
    #     print(c)
    #   }
    # }
    # colSelected <- c("MRP_TAGCODE","RELEASE_COMMENT","BROOD_YEAR","AVE_WEIGHT","NoTagClip","START_DATE","END_DATE","PURPOSE_CODE","AVE_LENGTH")
    # remainingCol <- colnames(DFO_df)[! colnames(DFO_df) %in% c(field_DFO,field_DFO_forFacilityID,colSelected)]
    # threshold <- sum(duplicated(DFO_df[,!colnames(DFO_df) %in% colSelected])) # 100
    # for(c in remainingCol){
    #   # c <- "RELEASE_COMMENT"
    #   c_here <- c(c,colSelected)
    #   DFO_df_cut <- DFO_df[,!colnames(DFO_df) %in% c_here]
    #   nb_duplicates <- sum(duplicated(DFO_df_cut))
    #   if(nb_duplicates > threshold){
    #     print(paste0(nb_duplicates," duplicated; columns: "))
    #     print(c_here)
    #     print("")
    #   }
    # }
    # more columns must be removed to reach the 134 number of duplicated rows...
    
    # replace the program acronyms by their names
    sheetNew$program <- program_acronym_fun(prog_acro =  sheetNew$program)
    
    # add facilityid by merging with sheet DataEntry_facilities
    sheetNew <- merge(x = sheetNew, 
                      y = filePSFnew_l$DataEntry_facilities[,c("facilityid","program","project","facilityname")],
                      by = c("program","project","facilityname"), 
                      all = T)

    # drop columns 'program', 'project' and 'facilityname'
    sheetNew <- sheetNew[,!colnames(sheetNew) %in% c('program','project','facilityname')]
    
    # reorder rows
    sheetNew <- sheetNew[order(sheetNew$facilityid),]
    
    # check if multiple coordinate values are attributed to a unique release_site_name
    sheetNew_cut <- sheetNew[,c("release_site_name","release_site_latitude","release_site_longitude")] |> unique()
    cond <- duplicated(sheetNew_cut$release_site_name)
    if(any(cond)){
      print("WARNING: in DataEntry_releases: certain release_site_name and multiple coordinates")
    }
    
    #' sum total_release for a same combination of (i) release_site_name, 
    #' (ii) release_stage, (iii) release_site_cuid and (iv) release_date:
    #' UPDATE: sum up only if all the fields except "total_release" are the same
    nrow(sheetNew) # 33788 33935
    nrow(unique(sheetNew))
    nrow(unique(sheetNew[,c("release_site_name","release_stage","cuid_release_site","release_date")])) # 24128
    nrow(unique(sheetNew[,c("release_site_name","release_stage","cuid_release_site","cuid_broodstock","release_date")])) # 24239
    nrow(unique(sheetNew[,colnames(sheetNew) != "total_release"])) # 25750
    cols <- c("species_name","cuid_broodstock","cuid_release_site","release_stage",
              "release_site_name","release_site_GFE_ID","release_site_latitude","release_site_longitude",
              "release_date")
    nrow(unique(sheetNew[,cols])) # 24239

    sheetNew_sum <- unique(sheetNew[,cols])
    sheetNew_sum$total_release <- NA
    nrow(sheetNew_sum) # 24239
    sheetNew_sum$facilityid <- NA
    
    count_percent_threshold <- 0
    for(r in 1:nrow(sheetNew_sum)){
      # r <- 1
      sheetNew_sum[r,]
      cond <- sheetNew$species_name == sheetNew_sum$species_name[r]
      for(c in cols[2:length(cols)]){
        val <- sheetNew_sum[r,c]
        if(is.na(val)){
          cond <- cond & is.na(sheetNew[,c])
        }else{
          cond <- cond & sheetNew[,c] == val & !is.na(sheetNew[,c])
        }
      }
      
      if(!any(cond)){
        print("Issue, no condition here - BREAK")
      }
      
     if(sum(cond) == 1){
        sheetNew_sum$total_release[r] <- sheetNew$total_release[cond]
        sheetNew_sum$facilityid[r] <- sheetNew$facilityid[cond]
     }else{
       # sheetNew[cond,]
       sheetNew_sum$total_release[r] <- sum(sheetNew$total_release[cond])
       sheetNew_sum$facilityid[r] <- paste(unique(sheetNew$facilityid[cond]),collapse = ", ")
      }
      
      if(count_percent_threshold < (r/nrow(sheetNew_sum) * 100)){
        count_percent_threshold <- count_percent_threshold + 5
        print(paste0("Progress: ",count_percent_threshold,"%"))
      }
    }
    
    # unique(sheetNew_sum$facilityid)
    # head(sheetNew_sum)
    
    nrow(sheetNew_sum) # 24239 22441
    sheetNew <- sheetNew_sum
  }
  filePSFnew_l[[sheet_i]] <- as.data.frame(sheetNew)
}

head(filePSFnew_l$DataEntry_facilities)
head(filePSFnew_l$DataEntry_facilitiescuids)
filePSFnew_l$DataEntry_facilitiescuids$region <- sapply(filePSFnew_l$DataEntry_facilitiescuids$cuid_release_site,
                                                        FUN = function(cuid){
                                                          cond <- conservationunits_decoder$cuid == cuid
                                                          return(unique(conservationunits_decoder$region[cond]))
                                                        })
head(filePSFnew_l$DataEntry_releases)

#
# Correct name locations -------
#

filePSFnew_l_copy <- filePSFnew_l

fields_toCorrect <- data.frame(sheet = c(rep("DataEntry_facilities",2),"DataEntry_releases"),
                               field = c('project',"facilityname","release_site_name"))

for(s in unique(fields_toCorrect$sheet)){
  # s <-  unique(fields_toCorrect$sheet)[2]
  cond <- fields_toCorrect$sheet == s
  for(f in fields_toCorrect$field[cond]){
    # f <- fields_toCorrect$field[cond][1]
    # filePSFnew_l[[s]][,f]
    
    # cond <- grepl("/",filePSFnew_l[[s]][,f])
    # filePSFnew_l[[s]][,f][cond] <- gsub("/",", ",filePSFnew_l[[s]][,f][cond])
    
    # cond <- grepl("Woss Comm",filePSFnew_l[[s]][,f])
    # filePSFnew_l[[s]][,f][cond]
    
    cond <- grepl("Comm H",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Comm H","Community Hall",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("\\+",filePSFnew_l[[s]][,f]) & !grepl(" \\+ ",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("\\+"," \\+ ",filePSFnew_l[[s]][,f][cond])
    
    for(r in 1:nrow(abbreviations_df)){
      # r <- 1
      
      # abbreviations_df and character_replace_fun() are in functions_general.R
      full_name <- character_replace_fun(charToChange = abbreviations_df$abbrevation[r],
                                         charNew = abbreviations_df$word_full[r], 
                                         name_vector =  filePSFnew_l[[s]][,f],
                                         char_sep = c(" ","/"),
                                         print = F)
      
      filePSFnew_l[[s]][,f] <- full_name$name_vector_new
    }
    
    # Same but for acronymes
    for(r in 1:nrow(acronyms_df)){
      # r <- 10
      
      # Abbreviations_df and character_replace_fun() are in functions_general.R
      full_name <- character_replace_fun(charToChange = acronyms_df$acronym[r],
                                         charNew = acronyms_df$word_full[r], 
                                         name_vector =  filePSFnew_l[[s]][,f],
                                         char_sep = c(" ","/"),
                                         print = F)
      
      filePSFnew_l[[s]][,f] <- full_name$name_vector_new
    }
    
    # Extra corrections
    cond <- grepl("Burns L",filePSFnew_l[[s]][,f]) & ! grepl("Burns Lake",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Burns L","Burns Lake",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("Cr,",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Cr,","Creek,",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("R,",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("R,","River,",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("P Hardy",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("P Hardy","Port Hardy",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl(" R)",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub(" R)"," River),",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("Vanderh",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Vanderh","Vanderhoof",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("Salm Rest So",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Salm Rest So","Salmon Restortation Society",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("Spruce C",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Spruce C","Spruce City",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("Chamber of Commer",filePSFnew_l[[s]][,f]) & ! grepl("Chamber of Commerce",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Chamber of Commer","Chamber of Commerce",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("  ",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("  "," ",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("R@",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("R@","River, ",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("Lost Little",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Lost Little","Lost Lake",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("-use6501",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("-use6501","",filePSFnew_l[[s]][,f][cond])

    cond <- grepl("SES",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("SES","Salmonid Enhancement Society",filePSFnew_l[[s]][,f][cond])
    
    # "Stave Valley Salmonio Enhancement Society --> it says "salmonio" on the internet but probably a typo?
    
    cond <- grepl("ES",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("ES","Enhancement Society",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("Barkley Sound Rndtbl S1 Sbct",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Barkley Sound Rndtbl S1 Sbct","Barkley Sound Roundtable Smolt1+ Subcontractor",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("Houston Channel of Comm",filePSFnew_l[[s]][,f])  # correction for the correction "Ch" --> "channel"
    filePSFnew_l[[s]][,f][cond] <- gsub("Channel of Comm","Chamber of Commerce",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("Vol",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Vol","Volunteer",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("Br 100 Swamp",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Br","Bridge",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("Edith Lake C",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Lake C","Lake Channel",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("\\(Up\\)",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Up","Upper",filePSFnew_l[[s]][,f][cond])
    
    cond <- grepl("\\(Low\\)",filePSFnew_l[[s]][,f])
    filePSFnew_l[[s]][,f][cond] <- gsub("Low","Lower",filePSFnew_l[[s]][,f][cond])
    
    # filePSFnew_l[[s]][,f] |> unique()
  }
}

# Check
r <- 1
unique(filePSFnew_l[[fields_toCorrect$sheet[r]]][,fields_toCorrect$field[r]])
r <- 2
unique(filePSFnew_l[[fields_toCorrect$sheet[r]]][,fields_toCorrect$field[r]])
r <- 3
unique(filePSFnew_l[[fields_toCorrect$sheet[r]]][,fields_toCorrect$field[r]])
# 

# Remaining potential acronym/abbreviations to change:

# Check slack thread:
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1723737506824409?thread_ts=1723131586.527919&cid=CJ5RVHVCG

# ANSWERS from email from Brock Ramshaw (DFO) from 15/08/2024 (all the changes to do below have been done)
# For PROJ_NAME
# - Sproat Lake ES  - enhancement society
# - Barkley Sound Rndtbl S1 Sbct – the Barkley Sound Roundtable Smolt1+ Subcontractor which was the Omega Pacific Hatchery
# - Stave Valley SES – salmon enhancement society
# - Houston Channel of Comm – The Houston Chamber of Commerce
# - Thornton Creek Vols – volunteers (not entirely confident in this. It was end dated in 2002 so there’s nobody around to confirm)

# For FACILITY_NAME
# - "Inc" (e.g. Cougar Canyon Creek Inc) – Inc refers to instream incubation --> BSC: only "Incumbation" for us
# - "H" (e.g. Ayum Creek H) – H is for hatchery

# For RELEASE_SITE_NAME"
# - Br 100 Swamp – Shows up in the Salmon River Estuary (-50.378061, -125.944523) in JSt. The lat/longs are not always exact locations where the fish are released. It looks like it was only used for 1999 Salmon R coho. It could be a bridge?
# - Little Edith Lake C – no lat/long exists in EPAD for this. Looks like it was only used for 1996 Quatse R coho. I want to say it’s a Channel off of the lake? It is connected to the Little Edith Lake geofeature_ID.

#
# Place release sites and facilities without coordinates in new sheets  ------
#

#' In sheet DataEntry_releases, remove the row with NA values for release_site_latitude
#' and release_site_longitude and places these in a new additional sheet
toKeep <- !is.na(filePSFnew_l$DataEntry_releases$release_site_latitude) & 
  !is.na(filePSFnew_l$DataEntry_releases$release_site_latitude)
toKeepNot <- !toKeep
DataEntry_releases_noNA <- filePSFnew_l$DataEntry_releases[toKeep,]
DataEntry_releases_NA <- filePSFnew_l$DataEntry_releases[toKeepNot,]
filePSFnew_l$DataEntry_releases <- DataEntry_releases_noNA
filePSFnew_l$DataEntry_releases_NAcoord <- DataEntry_releases_NA

#' In sheet DataEntry_facilitiescuids, remove the facilites (i.e., facilityID) 
#' that do not have coordinate in sheet DataEntry_facilities and places these in 
#' a new additional sheet called "DataEntry_facilitiescuids_NAcoord"
cond <- !is.na(filePSFnew_l$DataEntry_facilities$facility_latitude) &
  !is.na(filePSFnew_l$DataEntry_facilities$facility_longitude)
facilityIDtoKeep <- filePSFnew_l$DataEntry_facilities$facilityid[cond]

cond <- filePSFnew_l$DataEntry_facilitiescuids$facilityid %in% facilityIDtoKeep
DataEntry_facilitiescuids_noNA <- filePSFnew_l$DataEntry_facilitiescuids[cond,]
DataEntry_facilitiescuids_NA <- filePSFnew_l$DataEntry_facilitiescuids[!cond,]

filePSFnew_l$DataEntry_facilitiescuids <- DataEntry_facilitiescuids_noNA
filePSFnew_l$DataEntry_facilitiescuids_NAcoord <- DataEntry_facilitiescuids_NA

#
# Ad column release_type_pse in DataEntry_releases ------ 
#' Cf. Pop meeting April 3rd 2024
#' https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing

release_type_df <- release_type_pse_fun()

filePSFnew_l$DataEntry_releases$release_type_pse <- NA

for(i in 1:nrow(release_type_df)){
  rs <- release_type_df$release_stage[i]
  rss <- release_type_df$release_type_pse[i]
  cond <- filePSFnew_l$DataEntry_releases$release_stage == rs
  filePSFnew_l$DataEntry_releases$release_type_pse[cond] <- rss
}

#
# Add location_name_pse to sheet DataEntry_releases in SWP_hatchery_data_20240404.xlsx TO REMOVE ??? DOES NOTHING -----
#' 

# DataEntry_release <- read.xlsx(file = paste0(wd_output,"/SWP_hatchery_data_20240404.xlsx"),
#                               sheetName = "DataEntry_releases")

DataEntry_release <- filePSFnew_l$DataEntry_releases

DataEntry_release$location_name_pse <- DataEntry_release$release_site_name

# # Replace "/" by " " (it is important to do it 1st)  TODO? I asked Katy here: https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1723747384743129?thread_ts=1723131586.527919&cid=CJ5RVHVCG
# cond <- grepl("/",DataEntry_release$location_name_pse)
# DataEntry_release$location_name_pse[cond] <- gsub("/"," ",DataEntry_release$location_name_pse[cond])
# 
# # Replace "+" by " + " (it is important to do it 1st)
# cond <- grepl("\\+",DataEntry_release$location_name_pse) & !grepl(" \\+ ",DataEntry_release$location_name_pse)
# DataEntry_release$location_name_pse[cond] <- gsub("+"," + ",DataEntry_release$location_name_pse, 
#                                            fixed = T)

# Remove the double spaces
# cond <- grepl("  ",DataEntry_release$location_name_pse)
# DataEntry_release$location_name_pse[cond]
# DataEntry_release$location_name_pse <- gsub("  "," ",DataEntry_release$location_name_pse)

# replace the abbreviations by their full name
# abbreviations_df # in functions_general.R
# 
# for(r in 1:nrow(abbreviations_df)){
#   # r <- 1
#   full_name <- character_replace_fun(charToChange = abbreviations_df$abbrevation[r],
#                                      charNew = abbreviations_df$word_full[r], 
#                                      name_vector = DataEntry_release$location_name_pse,
#                                      print = F)
#   
#   DataEntry_release$location_name_pse <- full_name$name_vector_new
# }

unique(DataEntry_release$location_name_pse)

# # Change the Acronyms:
# # https://www.marinescience.psf.ca/wp-content/uploads/2023/05/LFR_ReleaseStrategyEvaluationBC_16July2021-Cover-Screen.pdf
# # https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40594361.pdf
# acronyms_df # in functions_general.R
# 
# for(r in 1:nrow(acronyms_df)){
#   # r <- 1
#   char <- acronyms_df$acronym[r]
#   cond <- grepl(char,DataEntry_release$location_name_pse)
#   unique(DataEntry_release$location_name_pse[cond])
#   char_new <- paste0("(",acronyms_df$word_full[r],")")
#   DataEntry_release$location_name_pse[cond] <- gsub(char,char_new,DataEntry_release$location_name_pse[cond])
# }

# Extra corrections:
# char <- "@Duncan"  # 
# cond <- grepl(char,DataEntry_release$location_name_pse)
# unique(DataEntry_release$location_name_pse[cond])
# DataEntry_release$location_name_pse[cond] <- gsub(char," (Duncan)",DataEntry_release$location_name_pse[cond])
# 
# char <- "-use6501"  #
# cond <- grepl(char,DataEntry_release$location_name_pse)
# unique(DataEntry_release$location_name_pse[cond])
# DataEntry_release$location_name_pse[cond] <- gsub(char,"",DataEntry_release$location_name_pse[cond])
# 
# char <- "Culvert 150 Creek"
# cond <- grepl(char,DataEntry_release$location_name_pse)
# unique(DataEntry_release$location_name_pse[cond])
# DataEntry_release$location_name_pse[cond] <- gsub(char,"Culvert Creek",DataEntry_release$location_name_pse[cond])


# Check 
View(unique(data.frame(DataEntry_release$location_name_pse)))

# QUESTION: what to do with these ones?
# https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1714522371999499?thread_ts=1712267027.385489&cid=C03LB7KM6JK
#' - Three B Channel --> ???
#' - Br 100 Swamp  --> Bridge 100 Swamp???
#' - 28 Mile Creek --> CORRECT
#' - Branch 10 Creek --> CORRECT
#' - Ed Leon Slough --> CORRECT
#' - Ink Lake --> CORRECT

# pattern <- "Cowichan R (Duncan)"
# cond <- grepl(pattern,DataEntry_release$location_name_pse)
# unique(DataEntry_release$location_name_pse[cond])
# unique(DataEntry_release[cond,c("location_name_pse","release_site_latitude","release_site_longitude")])

#
# Export remove DataEntry_release_noTBR_DATE.csv NOT NEEDED (cf. Data PSE Check in August 22 2024) -------- 
#
cuid_toRemove <- conservationunits_decoder$cuid[conservationunits_decoder$region == "Transboundary"]
DataEntry_release <- DataEntry_release[! DataEntry_release$cuid_broodstock %in% cuid_toRemove,]

# date <- Sys.Date()
# date <- gsub(pattern = "-",replacement = "",x = date)
# write.csv(DataEntry_release,paste0(wd_output,"/DataEntry_release_noTBR_",date,".csv"),
#           row.names = F)

#
# Correct coordinates for two locations (Katy) -----
# details in PSE Data Check-In Meeting Notes ; 2024-09-05
#

filePSFnew_l <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"),
                                           pattern = "SWP_hatchery_data")

# release_site_name  release_site_latitude	release_site_longitude
# Bentinck Arm North                  52.3                 -128.97
 
# project	                   facilityname	facility_latitude	facility_longitude
# Snootli Creek	Bentinck Arm North Seapen	             #N/A             	#N/A

# release_site_name  release_site_latitude	release_site_longitude
#       Bedwell Bay                  49.32	               -126.97

# project	facilityname	facility_latitude	facility_longitude
# Bedwell Bay	Bedwell Bay Seapen	49.31983	-122.91132

#'* Manual fix for Bentinck Arm North *
# - the change was made by locating "Bentinck Arm North" in Google Map and 
# minimizing the number of digit to modify: adding +2 to the longitude provided
# a location vert close to the gogle maps's pin.
cond <- filePSFnew_l$DataEntry_releases$release_site_name == "Bentinck Arm North"
long_new <- filePSFnew_l$DataEntry_releases$release_site_longitude[cond] + 2
long_new
# -126.97
filePSFnew_l$DataEntry_releases$release_site_longitude[cond] <- -126.97


#'* Manual fix for Bedwell Bay *
# - the change was made by locating "BBedwell Bay" in Google Map and 
# minimising the number of digit to modify. Here it seems that the 
# filePSFnew_l$DataEntry_facilities$facility_longitude value match the location 
# very well.
cond <- grepl("Bedwell Bay",filePSFnew_l$DataEntry_facilities$facilityname)
long_new <- filePSFnew_l$DataEntry_facilities$facility_longitude[cond]
long_new
# -122.9113
cond <- filePSFnew_l$DataEntry_releases$release_site_name == "Bedwell Bay"
filePSFnew_l$DataEntry_releases$release_site_longitude[cond] <- -122.9113


#'* Add region to DataEntry_releases *

region <- sapply(filePSFnew_l$DataEntry_releases$cuid_broodstock, 
                 FUN = function(cuid){
                   cond <- conservationunits_decoder$cuid == cuid
                   return(conservationunits_decoder$region[cond])
                 })

DataEntry_releases <- cbind(data.frame(region = region),
                            filePSFnew_l$DataEntry_releases)

filePSFnew_l$DataEntry_releases <- DataEntry_releases
colnames(filePSFnew_l$DataEntry_releases)

#
# Export the file ------
#

date <- Sys.Date()

# Export to the archive dropbox subfolder
for(sh_i in 1:length(names(filePSFnew_l))){
  # sh_i <- 1
  if(sh_i == 1){
    append <- F
  }else{
    append <- T
  }
  sheetName <- names(filePSFnew_l)[sh_i]
  sheet <- as.data.frame(filePSFnew_l[[sheetName]])
  
  #
  file <- paste0(wd_output,"/archive/SWP_hatchery_data_",date,".xlsx")
  
  write.xlsx(sheet, 
             file = file,
             sheetName = sheetName, 
             row.names = FALSE,
             append = append,
             showNA = T)
  
  print(sh_i)
}


# Export locally to push to github (if not too large)
for(sh_i in 1:length(names(filePSFnew_l))){
  # sh_i <- 1
  if(sh_i == 1){
    append <- F
  }else{
    append <- T
  }
  sheetName <- names(filePSFnew_l)[sh_i]
  sheet <- as.data.frame(filePSFnew_l[[sheetName]])
  
  #
  file <- paste0(paste0(getwd(),"/output"),"/SWP_hatchery_data.xlsx")
  
  write.xlsx(sheet, 
             file = file,
             sheetName = sheetName, 
             row.names = FALSE,
             append = append,
             showNA = T)
  
  print(sh_i)
}



# Notes for Katy
# - 1) I implemented a CHECK to check that facilityname have a unique combination of facility_latitude and facility_longitude --> they do
# - 2) I implemented a CHECK to check that release_site_name have a unique combination of release_site_latitude and release_site_longitude --> they do
# - 3) I sum total_release for a same combination of (i) release_site_name, (ii) release_stage, (iii) release_site_CUID and (iv) release_date
# - 4) I implemented a CHECK in 3) above to very if multiple cuid_broodstock are present in a single combinations --> THERE ARE (wait to hear from Katy)
# - 5) In sheet DataEntry_releases, I removed the row with NA values for release_site_latitude and release_site_longitude and places these in a new additional sheet called DataEntry_releases_NAcoord
# - 6) In sheet DataEntry_facilitiescuids, I removed the facilites (i.e., facilityID) that do not have coordinate in sheet DataEntry_facilities and placed these in a new additional sheet called DataEntry_facilitiescuids_NAcoord
# - 7) I cannot do anything about the 1st sheet ??? progratically, it has to be copy pasted by hand from the template and then filled by hand

#
# Edit dataset dataset384_output release_type_pse for Transboundary and steelhead (ONE TIME FIX) -----------
#
#' The hatchery data for SH and TBR do not come from this DFO file but from other
#' sources (ask Eric more about it). This data is processed in the following 
#' respective folder:
#' - Hatchery data from TBR:
#'    - ...X Drive\1_PROJECTS\1_Active\Transboundary\Data & Assessments\transboundary-data 
#'    - Steph's message about it: https://salmonwatersheds.slack.com/archives/C0196AAR3UZ/p1723737033009499
#' Hatchery data from SH:
#'    - ...\X Drive\1_PROJECTS\1_Active\Steelhead\3_Data_Analysis   ??? I am not sure

#' The goal will eventually be to combine the DFO data and these other sources 
#' into one datasets that will be sent to Katy.


#' #'Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- F

#' Import dataset384_output
dataset384_output <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[12],
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)

nrow(dataset384_output) # 20347, 1480
head(dataset384_output)
unique(dataset384_output$species_name)
unique(dataset384_output$region)
unique(dataset384_output$location_name_pse)
unique(dataset384_output$release_type_pse)


#'* edit location_name_pse *

for(f in c("location_name_pse")){
  # f <- "location_name_pse"

  cond <- grepl("\\+",dataset384_output[,f]) & !grepl(" \\+ ",dataset384_output[,f])
  dataset384_output[,f][cond] <- gsub("\\+"," \\+ ",dataset384_output[,f][cond])
  
  cond <- grepl("/ ",dataset384_output[,f])
  dataset384_output[,f][cond]
  
  for(r in 1:nrow(abbreviations_df)){
    # r <- 1
    
    # abbreviations_df and character_replace_fun() are in functions_general.R
    full_name <- character_replace_fun(charToChange = abbreviations_df$abbrevation[r],
                                       charNew = abbreviations_df$word_full[r], 
                                       name_vector =  dataset384_output[,f],
                                       char_sep = c(" ","/"),
                                       print = F)
    
    dataset384_output[,f] <- full_name$name_vector_new
  }
  
  # Same but for acronymes
  for(r in 1:nrow(acronyms_df)){
    # r <- 10
    
    # Abbreviations_df and character_replace_fun() are in functions_general.R
    full_name <- character_replace_fun(charToChange = acronyms_df$acronym[r],
                                       charNew = acronyms_df$word_full[r], 
                                       name_vector =  dataset384_output[,f],
                                       char_sep = c(" ","/"),
                                       print = F)
    
    dataset384_output[,f] <- full_name$name_vector_new
  }
}

cond <- grepl("\\(Up\\)",dataset384_output[,f])
dataset384_output[,f][cond] <- gsub("Up","Upper",dataset384_output[,f][cond])

cond <- grepl("\\(Low\\)",dataset384_output[,f])
dataset384_output[,f][cond] <- gsub("Low","Lower",dataset384_output[,f][cond])

cond <- grepl("\\(Up\\)",dataset384_output[,f])
dataset384_output[,f][cond] |> unique() # <- gsub("\\+"," \\+ ",dataset384_output[,f][cond])

unique(dataset384_output$location_name_pse) |> as.data.frame() |> View()


# OLDER CODE - keep for now just in case
#' dataset384_output$location_name_pse <- dataset384_output$locationnam
#' 
#' cond <- grepl(" [R|r]",dataset384_output$location_name_pse) & !grepl(" RIVER",dataset384_output$location_name_pse)
#' dataset384_output$location_name_pse[cond]
#' dataset384_output$location_name_pse[cond] <- gsub(" R"," River",dataset384_output$location_name_pse[cond])
#' 
#' cond <- grepl(" Lk",dataset384_output$location_name_pse)
#' dataset384_output$location_name_pse[cond]
#' dataset384_output$location_name_pse[cond] <- gsub(" Lk"," Lake",dataset384_output$location_name_pse[cond])
#' 
#' cond <- grepl(" Cr",dataset384_output$location_name_pse)
#' dataset384_output$location_name_pse[cond]
#' dataset384_output$location_name_pse[cond] <- gsub(" Cr"," Creek",dataset384_output$location_name_pse[cond])
#' 
#' cond <- grepl(" Pd",dataset384_output$location_name_pse)
#' dataset384_output$location_name_pse[cond]
#' 
#' cond <- grepl(" In",dataset384_output$location_name_pse)
#' dataset384_output$location_name_pse[cond]
#' 
#' cond <- grepl(" Ch",dataset384_output$location_name_pse)
#' dataset384_output$location_name_pse[cond]
#' 
#' 
#' #'* Add release_type_pse *
#' release_type_df <- release_type_pse_fun()
#' 
#' dataset384_output$release_type_pse <- NA
#' 
#' for(i in 1:nrow(release_type_df)){
#'   rs <- release_type_df$release_stage[i]
#'   rss <- release_type_df$release_type_pse[i]
#'   cond <- dataset384_output$release_stage == rs
#'   dataset384_output$release_type_pse[cond] <- rss
#' }


#'* create the different datasets *
dataset384_output_SH <- dataset384_output[dataset384_output$species_name == "Steelhead",]
nrow(dataset384_output_SH) # 1367 1369

dataset384_output_TB <- dataset384_output[dataset384_output$region == "Transboundary",]
nrow(dataset384_output_TB) # 111

dataset384_output_rest <- dataset384_output[dataset384_output$region != "Transboundary",]
dataset384_output_rest <- dataset384_output_rest[dataset384_output_rest$species_name  != "Steelhead",]

unique(dataset384_output_SH$release_stage)
unique(dataset384_output_TB$release_stage)
unique(dataset384_output_rest$release_stage)

unique(dataset384_output_SH$release_type_pse)
unique(dataset384_output_TB$release_type_pse)

#' * Corrections *
#' Remove the following two data points from dataset384_output_SH
cond <- dataset384_output_SH$release_stage %in% c("Catchable","2 years") # get ride of them PLUS they don't match the value in the PSE
dataset384_output_SH[cond,]
dataset384_output_SH <- dataset384_output_SH[!cond,]

# Export the file to the /archive in dropbox
date <- Sys.Date()
date <- gsub(pattern = "-",replacement = "",x = date)
#date <- "20240404"
write.csv(dataset384_output_SH,paste0(wd_output,"/dataset384_output_SH_",date,".csv"),
          row.names = F)
write.csv(dataset384_output_TB,paste0(wd_output,"/dataset384_output_TBR_",date,".csv"),
          row.names = F)

# Export locally to push to github 
write.csv(dataset384_output_SH,paste0(paste0(getwd(),"/output"),"/dataset384_output_SH.csv"),
          row.names = F)
write.csv(dataset384_output_TB,paste0(paste0(getwd(),"/output"),"/dataset384_output_TBR_csv"),
          row.names = F)

#
# CHECK: Compare dataset384_output_TB to the TB in SWP_hatchery_data_20240404.xlsx -----
#' 
#' Related slack thread: TODO: still need to be addressed.
#' https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1713377494509899

DataEntry_release <- read.xlsx(file = paste0(wd_output,"/SWP_hatchery_data_20240404.xlsx"),
                              sheetName = "DataEntry_releases")
head(DataEntry_release)

#' Check if there are cuid in dataset384_output that are not in 
#' SWP_hatchery_data_20240404.xlsx
unique(dataset384_output_TB$cuid)[!unique(dataset384_output_TB$cuid) %in% unique(DataEntry_release$cuid_broodstock)] 
#' There are two: 1028 1033
#' This is not unexpected because these come from another source than the DFO one.

#' Check if there are facilityid in SWP_hatchery_data_20240404.xlsx not in 
#' dataset384_output
#' --> need to create the field region using conservationunits_decoder
cuid_TB <- unique(conservationunits_decoder$cuid[conservationunits_decoder$region == "Transboundary"])
DataEntry_release_TB <- DataEntry_release[DataEntry_release$cuid_broodstock %in% cuid_TB,]
unique(DataEntry_release_TB$cuid_broodstock)[!unique(DataEntry_release_TB$cuid_broodstock) %in% unique(dataset384_output_TB$cuid)] 
#' There is one: 1017, which is not normal.
DataEntry_release_TB[DataEntry_release_TB$cuid_broodstock == 1017,]
# TO REMOVE there is an issue with the facilityID (cf. Population meeting 16/04/2024)
# https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing
# I told Katy to do so there:
# 

# Make a simpler column for species name in dataset384_output
dataset384_output_TB$species_name_simple <- dataset384_output_TB$species_name
dataset384_output_TB$species_name_simple <- gsub("Lake s","S",dataset384_output_TB$species_name_simple)
dataset384_output_TB$species_name_simple <- gsub("River s","S",dataset384_output_TB$species_name_simple)

colnames(dataset384_output_TB)
colnames(DataEntry_release_TB)

unique(dataset384_output_TB$locationname)
unique(DataEntry_release_TB$release_site_name)

#' * Fix Tatsaminie vs. Tatsaminie LK *
#' Coordinate differ a bit
#' For Tatsaminie issue between VESTA and Katy --> there were multiple locations 
#' combined into one.
#' https://www.dropbox.com/s/mllqe0jfk5el1yn/Transboundary_Hatchery%20Data%20Sources%20%26%20Processing.docx?dl=0
unique(dataset384_output_TB[,c("latitude","longitude")][dataset384_output_TB$locationname == "Tatsamenie",])
unique(DataEntry_release_TB[,c("release_site_latitude","release_site_longitude")][DataEntry_release_TB$release_site_name == "Tatsamenie Lk",])
DataEntry_release_TB$release_site_name[DataEntry_release_TB$release_site_name == "Tatsamenie Lk"] <- "Tatsamenie"

#' * Compare the datasets *
col_384 <-       c("species_name_simple","cuid","locationname","release_stage")  # facilityid
col_DataEntry <- c("species","cuid_broodstock","release_site_name","release_stage") # facilityID

cuid_facility_yr_384 <- unique(dataset384_output_TB[,col_384])
cuid_facility_yr_DataEntry <- unique(DataEntry_release_TB[,col_DataEntry])

# Data in common:
data_merged_all <- merge(x = dataset384_output_TB[,c(col_384,"year","total_release")], 
                        y = DataEntry_release_TB[,c(col_DataEntry,"release_date","total_release")],
                        by.x = c(col_384,"year"), by.y = c(col_DataEntry,"release_date"), all = T)
data_merged_all

# Data in dataset384_output_TB not in DataEntry_release_TB
cond_384_DERno <- !is.na(data_merged_all$total_release.x) & is.na(data_merged_all$total_release.y)
data_merged_all[cond_384_DERno,c("species_name_simple","cuid","locationname","release_stage","year",
                                         "total_release.x","total_release.y")]
sum(cond_384_DERno)/nrow(data_merged_all) * 100 # 77.19

# Data in DataEntry_release_TB not in dataset384_output_TB 
cond_384no_DER <- is.na(data_merged_all$total_release.x) & !is.na(data_merged_all$total_release.y)
data_merged_all[cond_384no_DER,c("species_name_simple","cuid","locationname","release_stage","year",
                                         "total_release.x","total_release.y")]
#' As mentioned above this data point can be ignore because there is an issue with
#' the facilityID
sum(cond_384no_DER)/nrow(data_merged_all) * 100 # 0.9

# Data in both
cond_384_DER <- !is.na(data_merged_all$total_release.x) & !is.na(data_merged_all$total_release.y)
data_common <- data_merged_all[cond_384_DER,]
data_common
sum(cond_384_DER)/nrow(data_merged_all) * 100 # 20,2

# Data in both that does not match
cond_diff <- data_common$total_release.x != data_common$total_release.y
data_common[cond_diff,]
sum(cond_diff)/nrow(data_merged_all) * 100 # 8.0

cond <- dataset384_output_TB$year == 2015 &
  dataset384_output_TB$locationname == "Tatsamenie" 
dataset384_output_TB[cond,]

cond <- DataEntry_release_TB$release_date == 2015 &
  DataEntry_release_TB$release_site_name == "Tatsamenie" 
DataEntry_release_TB[cond,]

# 
#
# CHECK: Pink odd vs. even year release issue -------
# https://salmonwatersheds.slack.com/archives/C01D2S4PRC2/p1723670303736409

conservationunits_decoder

hatchery_data <- import_mostRecent_file_fun(wd = wd_output, pattern = "SWP_hatchery_data_")

hatchery_data$DataEntry_releases$cu_name_pse <- sapply(X = hatchery_data$DataEntry_releases$cuid_broodstock, 
                                                       FUN = function(cuid){
                                                         cond <- conservationunits_decoder$cuid == cuid
                                                         return(conservationunits_decoder$cu_name_pse[cond])
                                                       })
cond <- hatchery_data$DataEntry_releases$species == "Pink"
Data_releases_pink <- hatchery_data$DataEntry_releases[cond,]

cond_odd <- grepl("odd",Data_releases_pink$cu_name_pse)
Data_releases_pink[cond_odd,c("cu_name_pse","release_date")] |> unique() |> View()

cond_even <- grepl("even",Data_releases_pink$cu_name_pse)
Data_releases_pink[cond_even,c("cu_name_pse","release_date")] |> unique() |> View()


DFO_df_all <- return_file_lastVersion_fun(wd_data = wd_data,
                                          pattern = "PSF_modified_SEP_releases")

cond_odd <- grepl("PKO",DFO_df_all$STOCK_CU_INDEX)
DFO_df_all[cond_odd,c("STOCK_CU_INDEX","BROOD_YEAR","RELEASE_YEAR")] |> unique()

cond_even <- grepl("PKE",DFO_df_all$STOCK_CU_INDEX)
DFO_df_all[cond_even,c("STOCK_CU_INDEX","BROOD_YEAR","RELEASE_YEAR")] |> unique()


DFO_df_all$BROOD_YEAR


#
hatchery_data$DataEntry_releases$region <- sapply(X = hatchery_data$DataEntry_releases$cuid_broodstock, 
                                                       FUN = function(cuid){
                                                         cond <- conservationunits_decoder$cuid == cuid
                                                         return(conservationunits_decoder$region[cond])
                                                       })
nrow(hatchery_data$DataEntry_releases) # 18895
cond_TBR <- hatchery_data$DataEntry_releases$region == "Transboundary"
sum(cond_TBR) # 26
nrow(hatchery_data$DataEntry_releases) - sum(cond_TBR) # 18869

DataEntry_release_noTB <- read.csv(paste0(wd_output,"/DataEntry_release_noTB_20240404.csv"),header = T)
nrow(DataEntry_release_noTB) # 18869



#
# OLD NOTES: -------
# - Eric: the only trick will be translating the "STOCK_CU_INDEX" field into 
# "cuid_broodstock" AND CUID of the release site. Will have to use one of the tables in the decoder repo. 
# Let me know if you have any other questions.
# - decoder tables:
# C:\Users\bcarturan\Salmon Watersheds Dropbox\Bruno Carturan\X Drive\1_PROJECTS\1_Active\Population Methods and Analysis\decoders\tables

# decoder repo:
# C:\Users\bcarturan\Salmon Watersheds Dropbox\Bruno Carturan\X Drive\1_PROJECTS\1_Active\Population Methods and Analysis\decoders
    

# QA/QC database relationship related (OLD) ------
printDF <- T
relationships_twoCol_df_fn(df = DFO_df,col1 = "FACILITY_NAME",col2 = "PROGRAM_CODE", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "FACILITY_NAME",col2 = "PROJ_NAME", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "PROGRAM_CODE",col2 = "PROJ_NAME", printDF = printDF) # one to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "FACILITY_NAME",col2 = "PROJ_NAME", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col2 = "FACILITY_NAME",col1 = "PROJ_NAME", printDF = printDF)

relationships_twoCol_df_fn(df = DFO_df,col1 = "FACILITY_NAME",col2 = "START_DATE", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "PROGRAM_CODE",col2 = "START_DATE", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "PROJ_NAME",col2 = "START_DATE", printDF = printDF) # many to many

DFO_df$START_END_DATE <- paste(DFO_df$START_DATE,DFO_df$END_DATE,sep = "_")
relationships_twoCol_df_fn(df = DFO_df,col1 = "PROGRAM_CODE",col2 = "START_END_DATE", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "PROJ_NAME",col2 = "START_END_DATE", printDF = printDF) # many to many

DFO_df$PROGRAM_CODE_PROJ_NAME <- paste(DFO_df$PROGRAM_CODE,DFO_df$PROJ_NAME,sep = " _ ")
relationships_twoCol_df_fn(df = DFO_df,
                           col1 = "PROGRAM_CODE_PROJ_NAME",
                           col2 = "START_END_DATE", 
                           printDF = printDF) # many to many

DFO_df$PROGRAM_CODE_PROJ_NAME_FACILITY_NAME <- paste(DFO_df$PROGRAM_CODE_PROJ_NAME,DFO_df$FACILITY_NAME,sep = " _ ")
relationships_twoCol_df_fn(df = DFO_df,
                           col1 = "PROGRAM_CODE_PROJ_NAME_FACILITY_NAME",
                           col2 = "START_END_DATE", 
                           printDF = printDF) # many to many

sum(is.na(DFO_df$END_DATE))   # 0
sum(is.na(DFO_df$START_DATE)) # 0

sum(is.na(DFO_df$FACILITY_LATITUDE)) #  2043
sum(is.na(DFO_df$FACILITY_LONGITUDE)) #  2043
unique(DFO_df$FACILITY_NAME[is.na(DFO_df$FACILITY_LATITUDE)]) # facilities withuot GIS coordinates

# OLDER CODE -----

# cond <- grepl(" [R|r]",DataEntry_release$location_name_pse) & !grepl(" RIVER",DataEntry_release$location_name_pse)
cond <- grepl(" [R|r]",DataEntry_release$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" [R|r]",l),DataEntry_release$location_name_pse)
}
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" R"," River",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Lk",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Lk"," Lake",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Cr",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Cr"," Creek",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Cv",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Cv"," Cove",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Pd",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Pd"," Pond",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Ch",DataEntry_release$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" Ch",l),DataEntry_release$location_name_pse)
}
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Ch"," Channel",DataEntry_release$location_name_pse[cond])

cond <- grepl(" In",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" In"," Inlet",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Sl",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Sl"," Slough",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Is",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Is"," Island",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Strm",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Strm"," Stream",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Pk",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Pk"," Peak",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Sp",DataEntry_release$location_name_pse) & !grepl(" Spit",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Sp"," Spawning",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Cst",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Cst"," Coast",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Val",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Val"," Valley",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Sd",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Sd"," Sound",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Est",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Est"," Estuary",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Wtshd",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Wtshd"," Watershed",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Tribs",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Tribs"," Tributaries",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Trib",DataEntry_release$location_name_pse) & !grepl(" Tributaries",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Trib"," Tributary",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Div",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Div"," Division",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Hb",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Hb"," Harbour",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Fwy",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Fwy"," Freeway",DataEntry_release$location_name_pse[cond])

cond <- grepl(" Msh",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" Msh"," Marsh",DataEntry_release$location_name_pse[cond])

cond <- grepl(" N",DataEntry_release$location_name_pse) & !grepl(" No",DataEntry_release$location_name_pse) & !grepl(" Na",DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" N"," North",DataEntry_release$location_name_pse[cond])

cond <- grepl(" S",DataEntry_release$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" S",l),DataEntry_release$location_name_pse)
}
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" S"," South",DataEntry_release$location_name_pse[cond])

cond <- grepl(" E",DataEntry_release$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" E",l),DataEntry_release$location_name_pse)
}
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(" E"," East",DataEntry_release$location_name_pse[cond])


char <- "UPFR"
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Upper Fraser)",DataEntry_release$location_name_pse[cond])

char <- "LWFR"
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Lower Fraser)",DataEntry_release$location_name_pse[cond])

char <- "JNST"
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Johnstone Strait)",DataEntry_release$location_name_pse[cond])

char <- "TOMF"  # Upper Thompson ?!
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Upper Thompson)",DataEntry_release$location_name_pse[cond])

char <- "TOMM"  #= Lower Thompson ?!
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Lower Thompson)",DataEntry_release$location_name_pse[cond])

char <- "CCST"  # 
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Central Coast)",DataEntry_release$location_name_pse[cond])

char <- "SKNA"  # 
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Skeena River)",DataEntry_release$location_name_pse[cond])

char <- "SWVI"  # 
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Southwest Vancouver Island)",DataEntry_release$location_name_pse[cond])

char <- "NWVI"  #
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Northwest Vancouver Island)",DataEntry_release$location_name_pse[cond])

char <- "GSMN"  # 
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Strait of Georgia Mainland)",DataEntry_release$location_name_pse[cond])

char <- "GSVI"  # 
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Strait of Georgia Vancouver Island)",DataEntry_release$location_name_pse[cond])

char <- "QCI"  # "Queen Charlotte Islands", which is now Haida Gwaii
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (Haida Gwaii)",DataEntry_release$location_name_pse[cond])

char <- "NCST"  #
cond <- grepl(char,DataEntry_release$location_name_pse)
unique(DataEntry_release$location_name_pse[cond])
DataEntry_release$location_name_pse[cond] <- gsub(char," (North Coast)",DataEntry_release$location_name_pse[cond])

