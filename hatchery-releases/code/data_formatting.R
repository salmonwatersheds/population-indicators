
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

source(paste(wd_code,"functions.R",sep = "/"))

#
# Import datasets --------

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

unique(DFO_df_all$SPECIES_NAME)

cond_ct <- DFO_df_all$SPECIES_NAME == "Cutthroat"
unique(DFO_df_all$STOCK_CU_INDEX[cond_ct])#  NA

# remove Cutthroat
DFO_df_all <- DFO_df_all[!cond_ct,]
nrow(DFO_df_all) # 35016

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

#
# ATTEMPT TO COMPARE CU-RELATED FIELDS WITH NUSEDS - TO REMOVE EVENTUALLY ------
#

# recreate the original CU_INDEX from FULL_CU_IN by removing the the acronym for 
# the species
# goal: see if we can use CU_INDEX to compare to STOCK_CU_ID --> IT DOES NOT BECAUSE THE LATTER DOES NOT HAVE LETTERS
for(sp in unique(DFO_df_all$SPECIES_NAME)){
  print(sp)
  cond <- DFO_df_all$SPECIES_NAME == sp
  # print(unique(DFO_df_all[cond,c("STOCK_CU_INDEX")]))
  print(unique(DFO_df_all[cond,c("STOCK_CU_ID")]))
  print("")
}

nuseds$CU_INDEX <- NA # NOTE: THIS IS NOT THE SAME AS STOCK_CU_ID: the latter does not have letters as shown above
for(fcui in unique(nuseds$FULL_CU_IN)){
  # fcui <- unique(nuseds$FULL_CU_IN)[1]
  if(grepl("SE",fcui)){
    cui <- gsub("SE","",fcui)
  }else{
    sp <- strsplit(fcui,"-")[[1]][1]
    cui <- gsub(paste0(sp,"-"),"",fcui)
  }
  cond <- nuseds$FULL_CU_IN == fcui
  nuseds$CU_INDEX[cond] <- cui
}

unique(nuseds[,c("CU_INDEX","FULL_CU_IN")]) # |> head()

nuseds_pop <- unique(nuseds[,c("SPECIES","cuid","cu_name_dfo","cu_name_pse","CU_NAME","FULL_CU_IN","CU_INDEX","POP_ID",
                               "GFE_ID","SYSTEM_SITE","sys_nm_final","X_LONGT","Y_LAT")])

# some exploration:
round(sum(!is.na(DFO_df_all_pop_NA$STOCK_NAME))/nrow(DFO_df_all_pop_NA) * 100,1)
round(sum(!is.na(DFO_df_all_pop_NA$STOCK_POP_NAME))/nrow(DFO_df_all_pop_NA) * 100,1) # 12%
round(sum(!is.na(DFO_df_all_pop_NA$STOCK_GFE_NAME))/nrow(DFO_df_all_pop_NA) * 100,1) # 98.7
round(sum(!is.na(DFO_df_all_pop_NA$STOCK_CU_ID))/nrow(DFO_df_all_pop_NA) * 100,1)    # 0
round(sum(!is.na(DFO_df_all_pop_NA$STOCK_CU_INDEX))/nrow(DFO_df_all_pop_NA) * 100,1)    # 0
round(sum(!is.na(DFO_df_all_pop_NA$STOCK_POP_ID))/nrow(DFO_df_all_pop_NA) * 100,1)   # 12.0
round(sum(!is.na(DFO_df_all_pop_NA$STOCK_GFE_ID))/nrow(DFO_df_all_pop_NA) * 100,1)   # 98.7

# STOCK_NAME
cond <- !is.na(DFO_df_all_pop_NA$STOCK_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_NAME) %in% simplify_string_fun(nuseds_pop$CU_NAME)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_NAME) %in% simplify_string_fun(nuseds$SYSTEM_SITE)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_NAME) %in% simplify_string_fun(nuseds$WATERBODY)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_NAME) %in% simplify_string_fun(nuseds$GAZETTED_NAME)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_NAME) %in% simplify_string_fun(nuseds$LOCAL_NAME_2)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)

# STOCK_POP_NAME
cond <- !is.na(DFO_df_all_pop_NA$STOCK_POP_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_POP_NAME) %in% simplify_string_fun(nuseds_pop$CU_NAME)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_POP_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_POP_NAME) %in% simplify_string_fun(nuseds$SYSTEM_SITE)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_POP_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_POP_NAME) %in% simplify_string_fun(nuseds$WATERBODY)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_POP_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_POP_NAME) %in% simplify_string_fun(nuseds$GAZETTED_NAME)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_POP_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_POP_NAME) %in% simplify_string_fun(nuseds$LOCAL_NAME_2)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)

# STOCK_GFE_NAME
# - SYSTEM_SITE 66.7% match
# - WATERBODY 63.6% match
cond <- !is.na(DFO_df_all_pop_NA$STOCK_GFE_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_GFE_NAME) %in% simplify_string_fun(nuseds_pop$CU_NAME)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_GFE_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_GFE_NAME) %in% simplify_string_fun(nuseds$SYSTEM_SITE)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_GFE_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_GFE_NAME) %in% simplify_string_fun(nuseds$WATERBODY)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_GFE_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_GFE_NAME) %in% simplify_string_fun(nuseds$GAZETTED_NAME)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)
cond <- !is.na(DFO_df_all_pop_NA$STOCK_GFE_NAME) & simplify_string_fun(DFO_df_all_pop_NA$STOCK_GFE_NAME) %in% simplify_string_fun(nuseds$LOCAL_NAME_2)
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)

# STOCK_POP_ID vs. POP_ID
cond <- !is.na(DFO_df_all_pop_NA$STOCK_POP_ID) & DFO_df_all_pop_NA$STOCK_POP_ID %in% nuseds_pop$POP_ID
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)

# STOCK_CU_ID (ALL NAs) vs. CU_INDEX
cond <- !is.na(DFO_df_all_pop_NA$STOCK_CU_ID) & DFO_df_all_pop_NA$STOCK_CU_ID %in% nuseds_pop$CU_INDEX
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1) # 0

# STOCK_GFE_ID vs. GFE_ID
cond <- !is.na(DFO_df_all_pop_NA$STOCK_GFE_ID) & DFO_df_all_pop_NA$STOCK_GFE_ID %in% nuseds_pop$GFE_ID
round(sum(cond)/nrow(DFO_df_all_pop_NA) * 100,1)

#
#
# Find missing coordinates for STOCK and RELISE SITE ------
#
#'* 1) Deal with NAs in STOCK_CU_INDEX (= FULL_CU_IN in NuSEDS) *
#' FOR NOW: --> remove the rows without values for STOCK_CU_INDEX 
#' TODO: wait to hear from them and adjust accordingly
#' Eric: "It looks like the last time the data was wrangled we just didn't include
#' these ones. However, if you're feeling ambitious you can use the STOCK_GFE_ID
#' to match the GFE_ID in the NuSEDS data.
#' (you'll have to translate the SPECIES_QUALIFIED in the opendata file to 
#' SPECIES_NAME in the PSF_modified_SEP_releases_2023 file e.g. CK-> Chinook)".
#' 
#' cf. Pop Team Meeting from December 11, 2024 for discussion on this
#' 
#' TODO: TO FINISH

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

  }else{
    cond <- nuseds$GFE_ID == gfe_id & !is.na(nuseds$GFE_ID)
    if(any(cond)){
      X_LONGT <- nuseds$X_LONGT[cond] |> unique()
      Y_LAT <- nuseds$Y_LAT[cond] |> unique()
    }else{
      print("STOCK_GFE_ID not in DFO_All_Streams_Segments nor in NuSEDS")
    }
  }
  
  if(!is.na(X_LONGT) & !is.na(Y_LAT)){
    cond <- DFO_df_all$STOCK_GFE_ID == gfe_id & !is.na(DFO_df_all$STOCK_GFE_ID)
    DFO_df_all$STOCK_LATITUDE[cond] <- Y_LAT
    DFO_df_all$STOCK_LONGITUDE[cond] <- X_LONGT
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
    
  }else{
    cond <- nuseds$GFE_ID == gfe_id & !is.na(nuseds$GFE_ID)
    if(any(cond)){
      X_LONGT <- nuseds$X_LONGT[cond] |> unique()
      Y_LAT <- nuseds$Y_LAT[cond] |> unique()
    }else{
      print("REL_GFE_ID not in DFO_All_Streams_Segments nor in NuSEDS")
    }
  }
  
  if(!is.na(X_LONGT) & !is.na(Y_LAT)){
    cond <- DFO_df_all$REL_GFE_ID == gfe_id & !is.na(DFO_df_all$REL_GFE_ID)
    DFO_df_all$REL_LATITUDE[cond] <- Y_LAT
    DFO_df_all$REL_LONGITUDE[cond] <- X_LONGT
  }
}
sum(is.na(DFO_df_all$REL_LONGITUDE )) # 2825

#
# Find the missing STOCK_CU_INDEX (== FULL_CU_IN) -------
#

cond_na <- is.na(DFO_df_all$STOCK_CU_INDEX)
sum(cond_na) # 2374 2981
sum(cond_na)/nrow(DFO_df_all) * 100 # 6.8%
# col_cu <- c("PROGRAM_CODE","PROJ_NAME","SPECIES_NAME","RUN_NAME",
#             "STOCK_NAME","STOCK_POP_NAME","STOCK_GFE_NAME",
#             "STOCK_POP_ID","STOCK_CU_ID","STOCK_CU_INDEX",
#             "STOCK_GFE_ID") 
col_cu <- c("SPECIES_NAME","STOCK_NAME","STOCK_CU_INDEX","RUN_NAME",
            "STOCK_GFE_NAME","STOCK_GFE_ID","STOCK_LATITUDE","STOCK_LONGITUDE",
            "REL_GFE_ID","REL_LATITUDE","REL_LONGITUDE") # DFO_df_all$REL_LATITUDE STOCK_LATITUDE

# Create a dataframe with (1) STOCK_CU_INDEX is NA and (2) unique species, stock/CU,
# and GFE_ID (= location)
DFO_df_all_pop_NA <- unique(DFO_df_all[cond_na,col_cu])
unique(DFO_df_all[!cond_na,col_cu])
nrow(DFO_df_all_pop_NA) # 382 368 225 268
nrow(unique(DFO_df_all[,col_cu])) # 3444 3427
nrow(DFO_df_all_pop_NA)/nrow(unique(DFO_df_all[,col_cu])) * 100 # 11.09 16.6%

head(DFO_df_all_pop_NA)
sum(is.na(DFO_df_all$STOCK_NAME))   # 0 <- field to use over STOCK_POP_NAME (lots of NAs) and STOCK_GFE_NAME (a few NAs)
sum(is.na(DFO_df_all$STOCK_GFE_ID)) # 3

library(sf)
library(sp)     # for spDists() TERRA is the replacement

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

# CUs_gdb <- st_buffer(x = CUs_gdb_full, dist = .1)   # takes too much time       

# plot(CUs_gdb_rg_sp[1], col = alpha("green",.2))
# plot(st_buffer(x = CUs_gdb_rg_sp,dist = .1)[1])
# plot(st_simplify(x = CUs_gdb[cond_sp & cond_rg,][1,][1],dTolerance = .001),
#      col = alpha("red",.2))

# only retain the fields that are useful
nuseds_pop <- unique(nuseds[,c("SPECIES","cuid","cu_name_dfo","cu_name_pse","CU_NAME","FULL_CU_IN",
                               "GFE_ID","SYSTEM_SITE","WATERBODY","GAZETTED_NAME","sys_nm_final",
                               "X_LONGT","Y_LAT")])

nrow(nuseds_pop) # 6848

# proportion STOCK_GFE_ID & STOCK_GFE_NAME is.na
d <- unique(DFO_df_all_pop_NA[,c("SPECIES_NAME","STOCK_GFE_ID","STOCK_GFE_NAME")])
round(sum(apply(X = d[,c("STOCK_GFE_ID","STOCK_GFE_NAME")],MARGIN = 1,FUN = function(r){all(!is.na(r))})) / nrow(DFO_df_all_pop_NA) * 100,1) # 46.2 93.9

#' try to find STOCK_CU_INDEX (= FULL_CU_IN in NuSEDS) using:
#' - STOCK_GFE_ID -->  GFE_ID in NuSEDS
#' - STOCK_GFE_NAME --> SYSTEM_SITE, WATERBODY, GAZETTED_NAME
nrow(DFO_df_all_pop_NA) # 368

# check those for which we cannot do anything
cond <- is.na(DFO_df_all_pop_NA$STOCK_GFE_ID) & 
  is.na(DFO_df_all_pop_NA$STOCK_LATITUDE) &
  is.na(DFO_df_all_pop_NA$STOCK_LONGITUDE)
DFO_df_all_pop_NA[cond,]

# *** GFE_IDs ***
# how many STOCK_GFE_ID are in NuSEDS
STOCK_GFE_IDs <- unique(DFO_df_all_pop_NA$STOCK_GFE_ID)
length(STOCK_GFE_IDs) # 149
length(STOCK_GFE_IDs[STOCK_GFE_IDs %in% unique(nuseds_pop$GFE_ID)]) # 95
# are there any NAs? --> use STOCK_NAME fpr them
cond_NA <- is.na(DFO_df_all_pop_NA$STOCK_GFE_ID)
DFO_df_all_pop_NA[cond_NA,]

# how many STOCK_GFE_ID are in NuSEDS
REL_GFE_IDs <- unique(DFO_df_all_pop_NA$REL_GFE_ID)
length(REL_GFE_IDs) # 255
length(REL_GFE_IDs[REL_GFE_IDs %in% unique(nuseds_pop$GFE_ID)]) # 132
# how many STOCK_NAME are in NuSEDS --> none of them do so the field is useless
STOCK_NAMEs <- simplify_string_fun(unique(DFO_df_all_pop_NA$STOCK_NAME))
length(REL_GFE_IDs) # 255
sum(STOCK_NAMEs %in% unique(simplify_string_fun(nuseds_pop$CU_NAME))) # 0
sum(sapply(STOCK_NAMEs, function(n){grepl(STOCK_NAMEs,unique(simplify_string_fun(nuseds_pop$CU_NAME)))})) # 0

#' Procedure:
#' 1) use STOCK_GFE_ID and SPECIES_NAME to identify which CUs match in NuSEDS
#' 2) if 



#' 1) use STOCK_GFE_ID
#'  if NA, look if you can find the GFE_ID using STOCK_LATITUDE and STOCK_LONGITUDE
#' 2) if not try STOCK_NAME
#' 3) 

# convert 

p_all <- pn_all <- NULL
DFO_df_all_pop_NA$cuid <- NA
DFO_df_all_pop_NA$comment <- NA
for(r in 1:nrow(DFO_df_all_pop_NA)){
  # r <- 132
  # r <- which(DFO_df_all_pop_NA$SPECIES_NAME == "Sockeye" & DFO_df_all_pop_NA$STOCK_GFE_ID == 23742)
  
  print(r)
  
  SPECIES_NAME <- DFO_df_all_pop_NA$SPECIES_NAME[r]
  STOCK_NAME <- DFO_df_all_pop_NA$STOCK_NAME[r] # field is useless because there is no match at all with NuSEDS
  STOCK_GFE_ID <- DFO_df_all_pop_NA$STOCK_GFE_ID[r]
  RUN_NAME <- DFO_df_all_pop_NA$RUN_NAME[r]
  STOCK_GFE_NAME <- DFO_df_all_pop_NA$STOCK_GFE_NAME[r]
  STOCK_LONGITUDE <- DFO_df_all_pop_NA$STOCK_LONGITUDE[r]
  STOCK_LATITUDE <- DFO_df_all_pop_NA$STOCK_LATITUDE[r]
  # REL_GFE_ID <- DFO_df_all_pop_NA$REL_GFE_ID[r]
  REL_LONGITUDE <- DFO_df_all_pop_NA$REL_LONGITUDE[r]
  REL_LATITUDE <- DFO_df_all_pop_NA$REL_LATITUDE[r]
  
  # Case that is no solvable
  keepGoing <- T
  comment <- NA
  if(is.na(STOCK_GFE_ID) & is.na(STOCK_LONGITUDE) & is.na(STOCK_LATITUDE)){
    keepGoing <- F # FAILURE 
    DFO_df_all_pop_NA$comment[r] <- "no STOCK GFE_ID and coordinates - FAILURE"
  }
  
  #' 1) use STOCK_GFE_ID

  #' 1)1. if there is no GFE_ID but there are coordinates --> try to find the GFE_ID
  if(is.na(STOCK_GFE_ID) & !is.na(STOCK_LONGITUDE) & !is.na(STOCK_LATITUDE)){
    
    # try to find the GFE_ID in NuSEDS
    cond_coord <- round(as.numeric(nuseds$X_LONGT),4) == round(STOCK_LONGITUDE,4) &
      !is.na(nuseds$X_LONGT) & 
      round(as.numeric(nuseds$Y_LAT),4) == round(STOCK_LATITUDE,4) & 
      !is.na(nuseds$Y_LAT)
    
    if(any(cond_coord)){
      STOCK_GFE_ID <- unique(nuseds$GFE_ID[cond_coord])
      if(length(STOCK_GFE_ID) > 1){
        print("More than one GFE_ID in NuSEDS")
        break
      }
      
    }else{  # try with the GFE_ID stream file provided by DFO (emailed from Wu Zhipeng, DFO, 09/04/2024)
      
      cond_coord <- round(as.numeric(DFO_All_Streams_Segments$X_LONGT),4) == round(STOCK_LONGITUDE,4) &
        !is.na(DFO_All_Streams_Segments$X_LONGT) & 
        round(as.numeric(DFO_All_Streams_Segments$Y_LAT),4) == round(STOCK_LATITUDE,4) & 
        !is.na(DFO_All_Streams_Segments$Y_LAT)
      
      if(any(cond_coord)){
        STOCK_GFE_ID <- unique(DFO_All_Streams_Segments$ID[cond_coord])
        if(length(STOCK_GFE_ID) > 1){
          print("More than one GFE_ID in DFO_All_Streams_Segments")
          break
        }else{
          comment <- paste(comment,"GFE_ID not found in NuSEDS and DFO streams file", sep = "; ")
        }
      }
    }
  }
  
  #' 1)2. use GFE_ID and species to find the corresponding CU(s)
  if(!is.na(STOCK_GFE_ID)){ # can be redundant with previous step for cases where GFE_ID had to be found with STOCK coordinates
    
    # check if GFE_ID is in NuSEDS
    cond_gfe_id <- nuseds_pop$GFE_ID == STOCK_GFE_ID
    
    if(any(cond_gfe_id)){ # else (i.e. "GFE_ID not in NuSEDS"): need to go to step 2)
      
      cond_sp <- nuseds_pop$SPECIES == SPECIES_NAME
      
      if(!any(cond_sp & cond_gfe_id)){ # must go to 2)
        
        comment <- paste(comment,"no SPECIES & GFE_ID combo in NuSEDS", sep = "; ")
        
      }else{
        
        FULL_CU_IN <- nuseds_pop$FULL_CU_IN[cond_sp & cond_gfe_id]
        cuid <- nuseds_pop$cuid[cond_sp & cond_gfe_id]
        
        if(length(FULL_CU_IN) > 1 | length(cuid) > 1){
          
          if(SPECIES_NAME == "Pink"){ # check if odd or even population
            
            cond_pk <- DFO_df_all$SPECIES_NAME == SPECIES_NAME & 
              DFO_df_all$STOCK_GFE_ID == STOCK_GFE_ID &
              DFO_df_all$STOCK_NAME == DFO_df_all_pop_NA$STOCK_NAME[r]
            
            even <- sum(DFO_df_all$RELEASE_YEAR[cond_pk] %% 2 == 0) > 0
            
            if(even){
              cond <- grepl("PKE",FULL_CU_IN)
            }else{
              cond <- grepl("PKO",FULL_CU_IN)
            }
            FULL_CU_IN <- FULL_CU_IN[cond]
            cuid <- cuid[cond]
          }
        }
        
        if(length(FULL_CU_IN) > 1 | length(cuid) > 1){ # if still > 1
          
          cond_1 <- SPECIES_NAME == "Sockeye" & STOCK_GFE_ID == 223 & RUN_NAME == "Summer"
          #' there are three potential PSE CUs matching this one: 
          #' - CU 760 (Adams-Early Summer) (previously CU 751 which got split into 760 and 761)
          #' - CU 739 (Shuswap-Late (cyclic)
          #' - CU 738 (Momich-Early Summer) --> if using CU geo database
          #' but actually, STOCK_NAME = "Momich+Cayenne", which corresponds to CU 761: cu_name_pse = Momich-Early Summer
          if(cond_1){  # r = 132
            
            cuid <- 761
            FULL_CU_IN <- CUs_gdb$FULL_CU_IN[CUs_gdb$CUID == cuid]
            comment <- paste(comment,"manual attribution to CU 761 Momich-Early Summer",sep = "; ")
            
          }else{
            print("more than one FULL_CU_IN for SPECIES & GFE_ID combo in NuSEDS")
            break
            
            DFO_df_all_pop_NA[r,]
            cond <- DFO_df_all$SPECIES_NAME == SPECIES_NAME & DFO_df_all$STOCK_GFE_ID == STOCK_GFE_ID & DFO_df_all$RUN_NAME == RUN_NAME
            DFO_df_all[cond,]
            DFO_df_all[cond,]$RELEASE_YEAR
            nuseds_pop[cond_sp & cond_gfe_id,]
          }

        }
        
        if(length(FULL_CU_IN) == 1 | length(cuid) == 1){
          DFO_df_all_pop_NA$STOCK_CU_INDEX[r] <- FULL_CU_IN
          DFO_df_all_pop_NA$cuid[r] <- cuid
          DFO_df_all_pop_NA$comment[r] <- paste(comment,"Matched with GFE_ID and species", sep = "; ")
          keepGoing <- F # SUCCESS!
        }
      }
    }
  } # end of trying to find CU with GFE_ID
  
  #' 2)1. if step above did not work, use coordinates and shape files
  if(keepGoing){
    
    # if coordinates stock are not available: try to find them in NuSEDS
    if(!is.na(STOCK_GFE_ID) & (is.na(STOCK_LONGITUDE) | is.na(STOCK_LATITUDE))){
      
      # try to find the coordinates in Nuseds
      cond_gfe_id <- nuseds_pop$GFE_ID == STOCK_GFE_ID
      
      if(any(cond_gfe_id)){
        STOCK_LATITUDE <- nuseds_pop$Y_LAT[cond_gfe_id]
        STOCK_LONGITUDE <- nuseds_pop$X_LONGT[cond_gfe_id]
        
        if(is.na(STOCK_LATITUDE) | is.na(STOCK_LONGITUDE)){
          comment <- paste(comment,"GFE_ID in NuSEDS but coordinates are NAs", sep = "; ")
        }else{
          comment <- paste(comment,"GFE_ID in NuSEDS and coordinates available", sep = "; ")
        }
        
      }else{
        comment <- paste(comment,"GFE_ID not in NuSEDS", sep = "; ")
      }
    }
    
    # if coordinates stock are not available: try to find them in DFO streams file
    if(!is.na(STOCK_GFE_ID) & (is.na(STOCK_LONGITUDE) | is.na(STOCK_LATITUDE))){
      # 
      cond_gfe_id <- DFO_All_Streams_Segments$ID == STOCK_GFE_ID
      
      if(any(cond_gfe_id)){
        STOCK_LATITUDE <- as.numeric(DFO_All_Streams_Segments$Y_LAT[cond_gfe_id])
        STOCK_LONGITUDE <- as.numeric(DFO_All_Streams_Segments$X_LONGT[cond_gfe_id])
        
        if(is.na(STOCK_LATITUDE) | is.na(STOCK_LONGITUDE)){
          comment <- paste(comment,"GFE_ID in DFO streams file but coordinates are NAs - FAILURE", sep = "; ") # # FAILURE
          DFO_df_all_pop_NA$comment[r] <- comment
        }else{
          comment <- paste(comment,"GFE_ID in DFO streams file and coordinates available", sep = "; ")
        }
        
      }else{
        comment <- paste(comment,"GFE_ID not in DFO streams file - FAILURE", sep = "; ") # FAILURE
        DFO_df_all_pop_NA$comment[r] <- comment
      }
    }
    
    if(!is.na(STOCK_LONGITUDE) & !is.na(STOCK_LATITUDE)){
      
      DFO_df_all_pop_NA$STOCK_LATITUDE[r] <- STOCK_LATITUDE
      DFO_df_all_pop_NA$STOCK_LONGITUDE[r] <- STOCK_LONGITUDE
      
      # Find the region
      # https://stackoverflow.com/questions/75669453/r-check-to-see-if-coordinates-fall-inside-outside-of-a-shapefile-polygon
      point <- st_as_sf(DFO_df_all_pop_NA[r,], 
                        coords = c("STOCK_LONGITUDE","STOCK_LATITUDE"), crs = 4269)
      
      layer_rg <- st_intersects(point, regions_shp)
      if(length(layer_rg[[1]]) == 0){ # no match, try to buffer
        layer_rg <- st_intersects(point, st_buffer(x = regions_shp, dist = .1))
      }
      if(length(layer_rg[[1]]) == 0){ # still no match, 
        print("Issue with finding region even after buffering")
        break
        
      }else{
        layer_rg <- layer_rg[[1]]
      }
      rg <- regions_shp$regionname[layer_rg]
      
      # check
      # plot(st_geometry(regions_shp))
      # plot(st_geometry(regions_shp[layer_rg,]))
      # plot(st_geometry(point), add = T, col = "red", pch = 16, cex = 3)
      
      # filter region and species
      sp <- SPECIES_NAME
      if(SPECIES_NAME == "Sockeye"){
        sp <- c("River sockeye","Lake sockeye")
      }else if(SPECIES_NAME == "Pink"){
        sp <- c("Pink odd","Pink even")
      }
      
      cond_rg_sp <- CUs_gdb$region == rg & CUs_gdb$species %in% sp
      
      if(!any(cond_rg_sp)){
        
        if(SPECIES_NAME == "Steelhead"){
          comment <- paste(comment,"No region - species combo in CUs_gdb with Steelhead - FAILURE", sep = "; ")
          DFO_df_all_pop_NA$comment[r] <- comment
          
        }else{
          print("Issue with region - species combo in CUs_gdb")
          break
        }
        
      }else{
        
        CUs_gdb_rg_sp <- CUs_gdb[cond_rg_sp,]
        
        layer_CU <- st_intersects(point, CUs_gdb_rg_sp)
        if(length(layer_CU[[1]]) == 0){ # no match, try to buffer
          layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = .05))
        }
        if(length(layer_CU[[1]]) == 0){ # no match, try to buffer
          layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = .1))
        }
        if(length(layer_CU[[1]]) == 0){ # no match, try to buffer
          layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = .2))
        }
        if(length(layer_CU[[1]]) == 0){ # no match, try to buffer
          layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = .3)) # r = 163
        }
        # if(length(layer_CU[[1]]) == 0){ # no match, try to buffer
        #   layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = .95)) # r = 195
        #   it was decided to not do anything for this SH CU: see cond_1 just below
        # }
        
        if(length(layer_CU[[1]]) == 0){ # still no match, 
          
          # in the case below (r = 195), dist must = .95 to intersect. It is probably a rainbow trout, so to exclude
          cond_1 <- SPECIES_NAME == "Steelhead" & STOCK_NAME == "Eagle R"
          
          # in the case below (r = 284), dist must = .6 to intersect. It is probably a kokanee, so to exclude
          cond_2 <- SPECIES_NAME == "Sockeye" & STOCK_NAME == "Mission Cr/OKAN"
          
          # in the case below (r = 336), dist must = 1.2 to intersect. It is probably a rainbow trout, so to exclude
          cond_3 <- SPECIES_NAME == "Steelhead" & STOCK_NAME == "Shuswap R Middle"
          
          # in the case below (r = 366), dist must = 1.5 to intersect. It is not a kokanee because RUN_NAME = "fall" but location too far from the Slamon River near Shuswap Lake
          cond_4 <- SPECIES_NAME == "Sockeye" & STOCK_NAME == "Salmon R/UPFR"
          
          if(cond_1 | cond_3){
            comment <- paste(comment,"STOCK coordinates too far from CU area; probably rainbow trout - FAILURE",sep = "; ")
            DFO_df_all_pop_NA$comment[r] <- comment
            
          }else if(cond_2){
            comment <- paste(comment,"STOCK coordinates too far from CU area; probably Kokanee - FAILURE",sep = "; ")
            DFO_df_all_pop_NA$comment[r] <- comment
            
          }else if(cond_4){
            comment <- paste(comment,"STOCK coordinates too far from CU area; not a Kokanee but STOCK coordinates too far from potential CU - FAILURE",sep = "; ")
            DFO_df_all_pop_NA$comment[r] <- comment
            
          }else{
            
            print("Issue finding CU(s) in CUs_gdb even after buffering")
            break
            
            dist <- 1.5
            layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = dist)) # 
            layer_CU
            layer_CU <- layer_CU[[1]]
            
            plot(st_geometry(regions_shp[layer_rg,]))
            plot(st_geometry(CUs_gdb_rg_sp[layer_CU[1],]), add = T, col = alpha("red",.3))
            plot(st_geometry(point), add = T, col = "red", pch = 16, cex = 2)
            plot(st_geometry(st_buffer(CUs_gdb_rg_sp[layer_CU[1],],dist = dist)), 
                 add = T, col = alpha("green",.3))
            plot(st_geometry(st_as_sf(DFO_df_all_pop_NA[r,], 
                                      coords = c("REL_LONGITUDE","REL_LATITUDE"), crs = 4269)),
                 add = T, col = "black", pch = 17, cex = 2)
            
            plot(st_geometry(CUs_gdb_rg_sp[CUs_gdb_rg_sp$CU_NAME == "Shuswap Complex-Late Timing",]), add = T, col = alpha("red",.3))
            
            for(i in 1:nrow(CUs_gdb_rg_sp)){
              plot(st_geometry(CUs_gdb_rg_sp[i,]), add = T, col = alpha("red",.3))
            }

            DFO_df_all_pop_NA[r,]
            
            CUs_gdb_rg_sp[layer_CU,]
            
            cond <- DFO_df_all$SPECIES_NAME == SPECIES_NAME & 
              DFO_df_all$STOCK_GFE_ID == STOCK_GFE_ID &
              DFO_df_all$STOCK_NAME == STOCK_NAME
            DFO_df_all[cond,]
            
          }
        }
        
        if(length(layer_CU[[1]]) > 0){
          print(layer_CU)
          
          layer_CU <- layer_CU[[1]]
          # if there is more than one CUs
          if(length(layer_CU) > 1){
            
            # 
            CU_NAMEs <- sapply(layer_CU,function(l){
              return(CUs_gdb_rg_sp$CU_NAME[l])
            })
            
            FULL_CU_INs <- sapply(layer_CU,function(l){
              return(CUs_gdb_rg_sp$FULL_CU_IN[l])
            })
            
            CUIDs <- sapply(layer_CU,function(l){
              return(CUs_gdb_rg_sp$CUID[l])
            })
            
            cu_name_pses <- sapply(layer_CU,function(l){
              return(CUs_gdb_rg_sp$cu_name_pse[l])
            })
            
            # about the assumptions:
            # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1738112709281789?thread_ts=1734047406.340689&cid=CJ5RVHVCG
            
            # Assumption 1 (r = 23): Vernon SEL
            # STOCK_POP_NAME = "SEBALHALL CREEK SOCKEYE"
            cond_1 <- SPECIES_NAME == "Sockeye" & 
              CU_NAMEs[1] == "East Vancouver Island & Georgia Strait" & # SER
              CU_NAMEs[2] == "Vernon"                                   # SEL
            
            # Assumption 2 (r = 36):
            # STOCK_POP_NAME = " MARIE LAKE SOCKEYE"
            cond_2 <- SPECIES_NAME == "Sockeye" & 
              CU_NAMEs[1] == "North Haida Gwaii" &  # SER
              CU_NAMEs[2] == "Marie"                # SEL
            
            #' in case the answer is in the RUN_NAME, e.g., 
            #' CU_NAMEs = "East Vancouver Island Winter" "East Vancouver Island Summer" & RUN_NAME = "Summer"
            cond_3 <- grepl(simplify_string_fun(DFO_df_all_pop_NA$RUN_NAME[r]),
                            simplify_string_fun(CU_NAMEs))
            
            #' Case with SH Kispiox R, with potential choices:
            #' - CU_NAME = Kispiox vs. Middle Skeena --> the former
            cond_4 <- SPECIES_NAME == "Steelhead" & STOCK_NAME == "Kispiox R"
            
            # if(all(is.na(FULL_CU_INs))){  # because there are cuid, which ultimately what we want
            #   layer_CU <- layer_CU[1]
            #   comment <- paste0(comment,"more than one CUs layer but FULL_CU_INs is NA in both case")
            # }
            
            cond_5 <- SPECIES_NAME == "Sockeye" & STOCK_NAME == "Coquitlam R"
            # three option:  Widgeon (river-type), Pitt-Early Summer, Coquitlam-Early Summer --> the latter

            if(cond_1 | cond_2){
              layer_CU <- layer_CU[2]
              
            }else if(sum(cond_3) == 1){ # there should one and only one option
              layer_CU <- layer_CU[cond_3]
              
            }else if(cond_4){
              layer_CU <- layer_CU[grepl("Kispiox",CUs_gdb_rg_sp$CU_NAME[layer_CU])]
              
            }else if(cond_5){
              cond <- grepl("Coquitlam",cu_name_pses)
              layer_CU <- layer_CU[cond]
              
            }else{
              
              print("more than one CUs")
              break
              
              plot(st_geometry(regions_shp[layer_rg,]))
              plot(st_geometry(CUs_gdb_rg_sp[layer_CU[1],]), add = T, col = alpha("red",.3))
              plot(st_geometry(CUs_gdb_rg_sp[layer_CU[2],]), add = T, col = alpha("blue",.3))
              plot(st_geometry(CUs_gdb_rg_sp[layer_CU[3],]), add = T, col = alpha("green",.3))
              plot(st_geometry(point), add = T, col = "red", pch = 16, cex = 1)
              plot(st_geometry(st_as_sf(DFO_df_all_pop_NA[r,], 
                                        coords = c("REL_LONGITUDE","REL_LATITUDE"), crs = 4269)),
                   add = T, col = "black", pch = 17, cex = 2)
              
              DFO_df_all_pop_NA[r,]
              
              CUs_gdb_rg_sp[layer_CU,]
              
              cond <- DFO_df_all$SPECIES_NAME == SPECIES_NAME & 
                DFO_df_all$STOCK_GFE_ID == STOCK_GFE_ID &
                DFO_df_all$STOCK_NAME == STOCK_NAME
              DFO_df_all[cond,]
              DFO_df_all[cond,]$RELEASE_YEAR
              
              cond <- nuseds$SPECIES == SPECIES_NAME & nuseds$CU_NAME %in% CU_NAMEs
              nuseds[cond,]
              
            }
          }

          CU_NAME <- CUs_gdb_rg_sp$CU_NAME[layer_CU]
          FULL_CU_IN <- CUs_gdb_rg_sp$FULL_CU_IN[layer_CU]
          cuid <- CUs_gdb_rg_sp$CUID[layer_CU]
          
          if(is.na(cuid)){ # try the decoder
            # comment <- paste(comment,"Matched with geocoordinates and species but FULL_CU_IN is NA - FAILURE", sep = "; ")
            print("cuid is NAs from gdb file")
            break
          }

          comment <- paste(comment,"Matched with geocoordinates and species", sep = "; ")
          
          DFO_df_all_pop_NA$STOCK_CU_INDEX[r] <- FULL_CU_IN
          DFO_df_all_pop_NA$cuid[r] <- cuid
          DFO_df_all_pop_NA$comment[r] <- comment
          keepGoing <- F # SUCCESS!
          
          # check
          # plot(st_geometry(CUs_gdb_rg_sp[layer_CU,]))
          # plot(st_geometry(point), add = T, col = "red", pch = 16, cex = 3)
        } # if CU(s) found
      } # if region - species combo in CUs_gdb
    } # if coordinates were found
  }  # end of 2)
}

DFO_df_all_pop_NA$comment <- gsub("NA; ","",DFO_df_all_pop_NA$comment)

sum(!is.na(DFO_df_all_pop_NA$cuid))/nrow(DFO_df_all_pop_NA) # 0.91
sum(!is.na(DFO_df_all_pop_NA$STOCK_CU_INDEX))/nrow(DFO_df_all_pop_NA) # 0.92
unique(DFO_df_all_pop_NA$comment)
table(DFO_df_all_pop_NA$comment)

# CHECK: the cases below should not have STOCK coordinates --> OK
cond <- DFO_df_all_pop_NA$comment == "NA; GFE_ID not in NuSEDS; GFE_ID not in DFO streams file - FAILURE" & !is.na(DFO_df_all_pop_NA$comment)
DFO_df_all_pop_NA[cond,]

# CHECK: there should not be any NA comment --> OK
cond <- is.na(DFO_df_all_pop_NA$comment)
DFO_df_all_pop_NA[cond,]

# CHECK:
#' The two CUs to fix in priority:
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1732751072916579?thread_ts=1729891432.329409&cid=CJ5RVHVCG
#' - Columbia SEL 1300  Osoyoos SEL-01-01 --> STOCK_NAME = Okanagan R, STOCK_POP_ID = NA, STOCK_CU_INDEX = NA
#' - Columbia  CK  301 Okanagan      CK-1 --> STOCK_NAME = Okanagan R, STOCK_POP_ID = 48435, STOCK_CU_INDEX = NA
cond <- DFO_df_all_pop_NA$cuid == 1300 & !is.na(DFO_df_all_pop_NA$cuid)
DFO_df_all_pop_NA[cond,] # OK
cond <- DFO_df_all_pop_NA$cuid == 301 & !is.na(DFO_df_all_pop_NA$cuid)
DFO_df_all_pop_NA[cond,] # OK








#'* Attribute SEL-21-02 to the correct CU *
#' The CU was split in to three: 
cond <- grepl("SEL-21-02",conservationunits_decoder$cu_index)
conservationunits_decoder[cond,]

cond <- DFO_df_all$STOCK_CU_INDEX == "SEL-21-02" & !is.na(DFO_df_all$STOCK_CU_INDEX)
DFO_df_all[cond,c("SPECIES_NAME","RUN_NAME","STOCK_NAME","STOCK_POP_NAME","STOCK_GFE_NAME","STOCK_POP_ID","STOCK_CU_ID","STOCK_CU_INDEX","STOCK_GFE_ID")] |> unique()




#' 2) STOCK_CU_INDEX (CU of the population of origin) and REL_CU_INDEX (CU of 
#' the releasing site)
#' Steph and Eric: "we assume that REL_CU_INDEX = STOCK_CU_INDEX when NAs are present 
#' in REL_CU_INDEX".
nrow(DFO_df[is.na(DFO_df$REL_CU_INDEX),]) # 6314
DFO_df$REL_CU_INDEX[is.na(DFO_df$REL_CU_INDEX)] <- DFO_df$STOCK_CU_INDEX[is.na(DFO_df$REL_CU_INDEX)]

#' 3) "Seapen" released (RELEASE_STAGE_NAME == "Seapen")
#' Steph and Eric: "some cases REL_CU_INDEX = blank is for seapen released, since
#' maybe in those cases the release can’t be assigned to a CU? But in those cases
#' do we assume the fish will return to the broodstock CU? Eric: Yes."
#' TODO: deal with it later.
# View(DFO_df_all[grepl("Seapen",DFO_df_all$RELEASE_STAGE_NAME),])

#' 4) Remove the CUs in STOCK_CU_INDEX and REL_CU_INDEX that are not present in 
#' the PSF database 
#' (i.e., conservation-units.csv):
#' - CK-9002, CK-9005, CK-9006, CK-9007, CK-9008 # Eric: These aren't real CUs. They are hatchery-only population so we don't show them on the PSE
#' - SEL-15-03                                   # Eric: This a CU that the central coast Nations said is not a CU. So we don't show on the PSE
#' - CM-9004                                     # Eric: a CU that no longer exists
CUToRemove <- c("CK-9002","CK-9005","CK-9006","CK-9007","CK-9008","SEL-15-03","CM-9004")
DFO_df <- DFO_df[! DFO_df$STOCK_CU_INDEX %in% CUToRemove,]
DFO_df <- DFO_df[! DFO_df$REL_CU_INDEX %in% CUToRemove,]
nrow(DFO_df) # 31871 31830


#' * Import the hatchery template from wd_data as a list *
filePSF_l <- hatchery_template_fun(wd_data = wd_data,
                                   filePSFname = "SWP_hatchery_data_template.xlsx",
                                   asDataFrame = T)

#
#
# Create SWP_hatchery_data_DATE.xlsx  ----------
#
#' Create a dataframe with the name of the columns in PSF_modified_SEP_releases_DATE.xlsx
#' and corresponding column names and sheets in the survey file SWP_hatchery_data_...xlsx
matchCol_df <- matching_columns_fun(wd_data = wd_data,
                                    wd_spawner_surveys_data = wd_spawner_surveys_data,
                                    DFO_df = DFO_df)

# make a copy of filePSF_l that is going to be filled
filePSFnew_l <- filePSF_l

#' Make a dataframe to report the cases where there are multiple cuid_broodstock 
#' for a same release_site_name - release_stage - release_site_CUID - release_date
#' combination --> to send to Katy,
cuid_broodstock_multi <- NULL

# Fill filePSF_l with new data (takes a couple minutes)
for(sheet_i in 2:length(names(filePSF_l))){   # Skip the 1st sheet
  
  # sheet_i <- 4
  sheetName <- names(filePSF_l)[sheet_i]
  sheetNew <- filePSF_l[[sheet_i]]
  
  # if(sheet_i == 4){
  #   break
  # }
  
  # subset matchCol_df for the current sheet
  # matchCol_df_cut <- matchCol_df[matchCol_df$PSF_sheet == sheetName,]
  
  # 
  field_PSF <- matchCol_df$PSF_colnames[matchCol_df$PSF_sheet == sheetName]
  field_DFO <- matchCol_df$DFO_colnames[matchCol_df$PSF_sheet == sheetName]
  field_DFO <- field_DFO[!is.na(field_DFO)]
  #
  if(sheetName == "DataEntry_facilities"){ # sheet 2
    
    #
    sheetNew <- DFO_df[,field_DFO]
    
    colnames(sheetNew) <- field_PSF[field_PSF != "facilityid"] # "program" "project" "facilityname" "facility_latitude" "facility_longitude" "startyear"  "endyear"  
    
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
    sheetNew <- sheetNew[,field_PSF]
    
    # check if multiple coordinate values are attributed to a unique facilityname
    sheetNew_cut <- sheetNew[,c("facilityname","facility_latitude","facility_longitude")]
    for(fn_i in 1:length(unique(sheetNew_cut$facilityname))){
      # fn_i <- 1
      fn_here <- unique(sheetNew_cut$facilityname)[fn_i]
      sheetNew_cut2 <- sheetNew_cut[sheetNew_cut$facilityname == fn_here,]
      sheetNew_cut2 <- unique(sheetNew_cut2)
      if(nrow(sheetNew_cut2) > 1){
        print("WARNING: The following facility has multiple coordinate values (and should not):")
        print(sheetNew_cut2)
      }
    }
    
    
  }else if(sheetName == "DataEntry_facilitiescuids"){
    
    #' find the facilityid/ID corresponding to each unique combination of 
    #' program-project-facilityname:
    field_DFO <- c("PROGRAM_CODE","PROJ_NAME","FACILITY_NAME","REL_CU_INDEX")
    sheetNew <- DFO_df[,field_DFO]
    colnames(sheetNew) <- c("program","project","facilityname","cu_index") # "cu_index" in conservationunits_decoder = the CU_INDEX
    sheetNew$program <- program_acronym_fun(prog_acro =  sheetNew$program)
    sheetNew <- merge(x = sheetNew, 
                      y = filePSFnew_l$DataEntry_facilities[,c("facilityid","program","project","facilityname")],
                      by = c("program","project","facilityname"), 
                      all = T)
    
    # remove unnecessary columns and update their names
    sheetNew <- sheetNew[,c("facilityid","cu_index")]  
    colnames(sheetNew)[colnames(sheetNew) == "facilityid"] <- "facilityID"
    
    # order rows
    sheetNew <- sheetNew[order(sheetNew$facilityID),]
    
    # remove duplicated rows
    sheetNew <- unique(sheetNew)
    
    # convert CUID (i.e. REL_CU_INDEX) to the PSE cuid
    sheetNew$CUID <- cuid_cu_index_conservation_units_fun(cu_index = sheetNew$cu_index,
                                                         conservation_units = conservationunits_decoder)
    
    # retain the desired columns
    sheetNew <- sheetNew[,c("facilityID","CUID")]
    
  }else if(sheetName == "DataEntry_releases"){
    
    #
    sheetNew <- DFO_df[,field_DFO]
    colnames(sheetNew) <- field_PSF
    
    #' find the facilityid/ID corresponding to each unique combination of 
    #' program-project-facilityname:
    field_DFO_forFacilityID <- c("PROGRAM_CODE","PROJ_NAME","FACILITY_NAME")

    sheetNew <- cbind(sheetNew, DFO_df[,field_DFO_forFacilityID])
    
    colnames(sheetNew)[colnames(sheetNew) == "PROGRAM_CODE"] <- "program"
    colnames(sheetNew)[colnames(sheetNew) == "PROJ_NAME"] <- "project"
    colnames(sheetNew)[colnames(sheetNew) == "facilityID"] <- "facilityname"
    
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
    
    # There are two locations with longitude > 0 which is a mistake in DFO_df
    # https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1723519875081859?thread_ts=1723514711.630399&cid=C03LB7KM6JK
    #' TODO: check where the error is coming from above (the source file seems to be correct)
    cond <- sheetNew$release_site_longitude > 0 & !is.na(sheetNew$release_site_longitude)
    sheetNew[cond,]$release_site_longitude <- -1 * sheetNew[cond,]$release_site_longitude
    
    # replace the program acronyms by their names
    sheetNew$program <- program_acronym_fun(prog_acro =  sheetNew$program)
    
    # add facilityid by merging with sheet DataEntry_facilities
    sheetNew <- merge(x = sheetNew, 
                      y = filePSFnew_l$DataEntry_facilities[,c("facilityid","program","project","facilityname")],
                      by = c("program","project","facilityname"), 
                      all = T)
    colnames(sheetNew)[colnames(sheetNew) == "facilityid"] <- "facilityID"
    
    # drop columns 'program', 'project' and 'facilityname'
    sheetNew <- sheetNew[,!colnames(sheetNew) %in% c('program','project','facilityname')]
    
    # replace STOCK_CU_INDEX and REL_CU_INDEX values by the PSF CUID
    sheetNew$release_site_CUID <- cuid_cu_index_conservation_units_fun(cu_index = sheetNew$release_site_CUID, 
                                                                      conservation_units = conservationunits_decoder)
    sheetNew$cuid_broodstock <- cuid_cu_index_conservation_units_fun(cu_index = sheetNew$cuid_broodstock, 
                                                                    conservation_units = conservationunits_decoder)
    
    # reorder columns and rows
    sheetNew <- sheetNew[field_PSF]
    sheetNew <- sheetNew[order(sheetNew$facilityID),]
    
    # check if multiple coordinate values are attributed to a unique release_site_name
    sheetNew_cut <- sheetNew[,c("release_site_name","release_site_latitude","release_site_longitude")]
    for(rs_i in 1:length(unique(sheetNew_cut$release_site_name))){
      # rs_i <- 1
      rs_here <- unique(sheetNew_cut$release_site_name)[rs_i]
      sheetNew_cut2 <- sheetNew_cut[sheetNew_cut$release_site_name == rs_here,]
      sheetNew_cut2 <- unique(sheetNew_cut2)
      if(nrow(sheetNew_cut2) > 1){
        print("WARNING: in DataEntry_releases")
        print("The following release site has multiple coordinate values (and should not):")
        print(sheetNew_cut2)
      }
    }
    
    #' sum total_release for a same combination of (i) release_site_name, 
    #' (ii) release_stage, (iii) release_site_CUID and (iv) release_date:
    nrow(sheetNew) # 31830
    sheetNew_l <- list()
    count <- 1
    for(rsn in unique(sheetNew$release_site_name)){
      # rsn <- unique(sheetNew$release_site_name)[1]
      sheetNew_rsn <- sheetNew[sheetNew$release_site_name == rsn,]
      for(rs in unique(sheetNew_rsn$release_stage)){
        # rs <- unique(sheetNew_rsn$release_stage)[1]
        sheetNew_rsn_rs <- sheetNew_rsn[sheetNew_rsn$release_stage == rs,]
        for(cuid in unique(sheetNew_rsn_rs$release_site_CUID)){
          # cuid <- unique(sheetNew_rsn_rs$release_site_CUID)[1]
          sheetNew_rsn_rs_cuid <- sheetNew_rsn_rs[sheetNew_rsn_rs$release_site_CUID == cuid,]
          for(rd in unique(sheetNew_rsn_rs_cuid$release_date)){
            # rd <- unique(sheetNew_rsn_rs_cuid$release_date)[1]
            sheetNew_rsn_rs_cuid_rd <- sheetNew_rsn_rs_cuid[sheetNew_rsn_rs_cuid$release_date == rd,]
            slice <- sheetNew_rsn_rs_cuid_rd[1,,drop = F]
            slice$total_release <- sum(sheetNew_rsn_rs_cuid_rd$total_release)
            sheetNew_l[[count]] <- slice
            count <- count + 1
            # CHECK UP
            if(length(unique(sheetNew_rsn_rs_cuid_rd$cuid_broodstock)) > 1){
              print("WARNING: in DataEntry_releases")
              print("The following release_site_name-release_stage-release_site_CUID-release_date combination has multiple cuid_broodstock:")
              print("Ask Katy to know what to do")
              print(sheetNew_rsn_rs_cuid_rd)
              
              if(is.null(cuid_broodstock_multi)){
                cuid_broodstock_multi <- sheetNew_rsn_rs_cuid_rd
              }else{
                cuid_broodstock_multi <- rbind(cuid_broodstock_multi,sheetNew_rsn_rs_cuid_rd)
              }
            }
          }
        }
      }
    }
    sheetNew <- do.call(rbind,sheetNew_l)
    nrow(sheetNew) # 22441
    
    # remove duplicted rows (SEE TODO ABOVE ABOUT THAT)
    # nrow(DFO_df)
    # nrow(sheetNew) - nrow(unique(sheetNew))
    sheetNew <- unique(sheetNew)
    # sheetNew[sheetNew$cuid_broodstock != sheetNew$release_site_CUID,]
    
    # "release_site_name" ("RELEASE_SITE_NAME")
    # QUESTION: unabbreviate names, e.g., 'Adams R Up' --> 'Adams River Upper' --> wait to hear from Katy
    #' TODO: QUESTION: Should I do it? is that correct? Am I missing something?
    # release_site_abbrev <-        c("Cr","R","Up","Low","Sl","N","S","E","W","LK")
    # names(release_site_abbrev) <- c("Creek","River","Upper","Lower","Slough","North","South","East","West","Lake")
    
  }
  filePSFnew_l[[sheet_i]] <- as.data.frame(sheetNew)
}

filePSFnew_l$DataEntry_facilities

#' 
colOrder <- c("species","release_site_latitude","release_site_longitude","facilityID",
              "release_site_name","release_stage","release_site_CUID","release_date",
              "cuid_broodstock","total_release")

# write.csv(cuid_broodstock_multi[,colOrder],paste0(wd_output,"/cuid_broodstock_multi.csv"),
#           row.names = F)

#
# Correct name locations -------
#

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

cond <- filePSFnew_l$DataEntry_facilitiescuids$facilityID %in% facilityIDtoKeep
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
# Add location_name_pse to sheet DataEntry_releases in SWP_hatchery_data_20240404.xlsx -----
#' 

# DataEntry_relase <- read.xlsx(file = paste0(wd_output,"/SWP_hatchery_data_20240404.xlsx"),
#                               sheetName = "DataEntry_releases")

DataEntry_relase <- filePSFnew_l$DataEntry_releases

DataEntry_relase$location_name_pse <- DataEntry_relase$release_site_name

# # Replace "/" by " " (it is important to do it 1st)  TODO? I asked Katy here: https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1723747384743129?thread_ts=1723131586.527919&cid=CJ5RVHVCG
# cond <- grepl("/",DataEntry_relase$location_name_pse)
# DataEntry_relase$location_name_pse[cond] <- gsub("/"," ",DataEntry_relase$location_name_pse[cond])
# 
# # Replace "+" by " + " (it is important to do it 1st)
# cond <- grepl("\\+",DataEntry_relase$location_name_pse) & !grepl(" \\+ ",DataEntry_relase$location_name_pse)
# DataEntry_relase$location_name_pse[cond] <- gsub("+"," + ",DataEntry_relase$location_name_pse, 
#                                            fixed = T)

# Remove the double spaces
# cond <- grepl("  ",DataEntry_relase$location_name_pse)
# DataEntry_relase$location_name_pse[cond]
# DataEntry_relase$location_name_pse <- gsub("  "," ",DataEntry_relase$location_name_pse)

# replace the abbreviations by their full name
# abbreviations_df # in functions_general.R
# 
# for(r in 1:nrow(abbreviations_df)){
#   # r <- 1
#   full_name <- character_replace_fun(charToChange = abbreviations_df$abbrevation[r],
#                                      charNew = abbreviations_df$word_full[r], 
#                                      name_vector = DataEntry_relase$location_name_pse,
#                                      print = F)
#   
#   DataEntry_relase$location_name_pse <- full_name$name_vector_new
# }

unique(DataEntry_relase$location_name_pse)

# # Change the Acronyms:
# # https://www.marinescience.psf.ca/wp-content/uploads/2023/05/LFR_ReleaseStrategyEvaluationBC_16July2021-Cover-Screen.pdf
# # https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40594361.pdf
# acronyms_df # in functions_general.R
# 
# for(r in 1:nrow(acronyms_df)){
#   # r <- 1
#   char <- acronyms_df$acronym[r]
#   cond <- grepl(char,DataEntry_relase$location_name_pse)
#   unique(DataEntry_relase$location_name_pse[cond])
#   char_new <- paste0("(",acronyms_df$word_full[r],")")
#   DataEntry_relase$location_name_pse[cond] <- gsub(char,char_new,DataEntry_relase$location_name_pse[cond])
# }

# Extra corrections:
# char <- "@Duncan"  # 
# cond <- grepl(char,DataEntry_relase$location_name_pse)
# unique(DataEntry_relase$location_name_pse[cond])
# DataEntry_relase$location_name_pse[cond] <- gsub(char," (Duncan)",DataEntry_relase$location_name_pse[cond])
# 
# char <- "-use6501"  #
# cond <- grepl(char,DataEntry_relase$location_name_pse)
# unique(DataEntry_relase$location_name_pse[cond])
# DataEntry_relase$location_name_pse[cond] <- gsub(char,"",DataEntry_relase$location_name_pse[cond])
# 
# char <- "Culvert 150 Creek"
# cond <- grepl(char,DataEntry_relase$location_name_pse)
# unique(DataEntry_relase$location_name_pse[cond])
# DataEntry_relase$location_name_pse[cond] <- gsub(char,"Culvert Creek",DataEntry_relase$location_name_pse[cond])


# Check 
View(unique(data.frame(DataEntry_relase$location_name_pse)))

# QUESTION: what to do with these ones?
# https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1714522371999499?thread_ts=1712267027.385489&cid=C03LB7KM6JK
#' - Three B Channel --> ???
#' - Br 100 Swamp  --> Bridge 100 Swamp???
#' - 28 Mile Creek --> CORRECT
#' - Branch 10 Creek --> CORRECT
#' - Ed Leon Slough --> CORRECT
#' - Ink Lake --> CORRECT

# pattern <- "Cowichan R (Duncan)"
# cond <- grepl(pattern,DataEntry_relase$location_name_pse)
# unique(DataEntry_relase$location_name_pse[cond])
# unique(DataEntry_relase[cond,c("location_name_pse","release_site_latitude","release_site_longitude")])

#
# Export remove DataEntry_relase_noTBR_DATE.csv NOT NEEDED (cf. Data PSE Check in August 22 2024) -------- 
#
cuid_toRemove <- conservationunits_decoder$cuid[conservationunits_decoder$region == "Transboundary"]
DataEntry_relase <- DataEntry_relase[! DataEntry_relase$cuid_broodstock %in% cuid_toRemove,]

# date <- Sys.Date()
# date <- gsub(pattern = "-",replacement = "",x = date)
# write.csv(DataEntry_relase,paste0(wd_output,"/DataEntry_relase_noTBR_",date,".csv"),
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

DataEntry_relase <- read.xlsx(file = paste0(wd_output,"/SWP_hatchery_data_20240404.xlsx"),
                              sheetName = "DataEntry_releases")
head(DataEntry_relase)

#' Check if there are cuid in dataset384_output that are not in 
#' SWP_hatchery_data_20240404.xlsx
unique(dataset384_output_TB$cuid)[!unique(dataset384_output_TB$cuid) %in% unique(DataEntry_relase$cuid_broodstock)] 
#' There are two: 1028 1033
#' This is not unexpected because these come from another source than the DFO one.

#' Check if there are facilityid in SWP_hatchery_data_20240404.xlsx not in 
#' dataset384_output
#' --> need to create the field region using conservationunits_decoder
cuid_TB <- unique(conservationunits_decoder$cuid[conservationunits_decoder$region == "Transboundary"])
DataEntry_relase_TB <- DataEntry_relase[DataEntry_relase$cuid_broodstock %in% cuid_TB,]
unique(DataEntry_relase_TB$cuid_broodstock)[!unique(DataEntry_relase_TB$cuid_broodstock) %in% unique(dataset384_output_TB$cuid)] 
#' There is one: 1017, which is not normal.
DataEntry_relase_TB[DataEntry_relase_TB$cuid_broodstock == 1017,]
# TO REMOVE there is an issue with the facilityID (cf. Population meeting 16/04/2024)
# https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing
# I told Katy to do so there:
# 

# Make a simpler column for species name in dataset384_output
dataset384_output_TB$species_name_simple <- dataset384_output_TB$species_name
dataset384_output_TB$species_name_simple <- gsub("Lake s","S",dataset384_output_TB$species_name_simple)
dataset384_output_TB$species_name_simple <- gsub("River s","S",dataset384_output_TB$species_name_simple)

colnames(dataset384_output_TB)
colnames(DataEntry_relase_TB)

unique(dataset384_output_TB$locationname)
unique(DataEntry_relase_TB$release_site_name)

#' * Fix Tatsaminie vs. Tatsaminie LK *
#' Coordinate differ a bit
#' For Tatsaminie issue between VESTA and Katy --> there were multiple locations 
#' combined into one.
#' https://www.dropbox.com/s/mllqe0jfk5el1yn/Transboundary_Hatchery%20Data%20Sources%20%26%20Processing.docx?dl=0
unique(dataset384_output_TB[,c("latitude","longitude")][dataset384_output_TB$locationname == "Tatsamenie",])
unique(DataEntry_relase_TB[,c("release_site_latitude","release_site_longitude")][DataEntry_relase_TB$release_site_name == "Tatsamenie Lk",])
DataEntry_relase_TB$release_site_name[DataEntry_relase_TB$release_site_name == "Tatsamenie Lk"] <- "Tatsamenie"

#' * Compare the datasets *
col_384 <-       c("species_name_simple","cuid","locationname","release_stage")  # facilityid
col_DataEntry <- c("species","cuid_broodstock","release_site_name","release_stage") # facilityID

cuid_facility_yr_384 <- unique(dataset384_output_TB[,col_384])
cuid_facility_yr_DataEntry <- unique(DataEntry_relase_TB[,col_DataEntry])

# Data in common:
data_merged_all <- merge(x = dataset384_output_TB[,c(col_384,"year","total_release")], 
                        y = DataEntry_relase_TB[,c(col_DataEntry,"release_date","total_release")],
                        by.x = c(col_384,"year"), by.y = c(col_DataEntry,"release_date"), all = T)
data_merged_all

# Data in dataset384_output_TB not in DataEntry_relase_TB
cond_384_DERno <- !is.na(data_merged_all$total_release.x) & is.na(data_merged_all$total_release.y)
data_merged_all[cond_384_DERno,c("species_name_simple","cuid","locationname","release_stage","year",
                                         "total_release.x","total_release.y")]
sum(cond_384_DERno)/nrow(data_merged_all) * 100 # 77.19

# Data in DataEntry_relase_TB not in dataset384_output_TB 
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

cond <- DataEntry_relase_TB$release_date == 2015 &
  DataEntry_relase_TB$release_site_name == "Tatsamenie" 
DataEntry_relase_TB[cond,]

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

DataEntry_relase_noTB <- read.csv(paste0(wd_output,"/DataEntry_relase_noTB_20240404.csv"),header = T)
nrow(DataEntry_relase_noTB) # 18869



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

# cond <- grepl(" [R|r]",DataEntry_relase$location_name_pse) & !grepl(" RIVER",DataEntry_relase$location_name_pse)
cond <- grepl(" [R|r]",DataEntry_relase$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" [R|r]",l),DataEntry_relase$location_name_pse)
}
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" R"," River",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Lk",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Lk"," Lake",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Cr",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Cr"," Creek",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Cv",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Cv"," Cove",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Pd",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Pd"," Pond",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Ch",DataEntry_relase$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" Ch",l),DataEntry_relase$location_name_pse)
}
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Ch"," Channel",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" In",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" In"," Inlet",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Sl",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Sl"," Slough",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Is",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Is"," Island",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Strm",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Strm"," Stream",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Pk",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Pk"," Peak",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Sp",DataEntry_relase$location_name_pse) & !grepl(" Spit",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Sp"," Spawning",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Cst",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Cst"," Coast",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Val",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Val"," Valley",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Sd",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Sd"," Sound",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Est",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Est"," Estuary",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Wtshd",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Wtshd"," Watershed",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Tribs",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Tribs"," Tributaries",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Trib",DataEntry_relase$location_name_pse) & !grepl(" Tributaries",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Trib"," Tributary",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Div",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Div"," Division",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Hb",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Hb"," Harbour",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Fwy",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Fwy"," Freeway",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Msh",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Msh"," Marsh",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" N",DataEntry_relase$location_name_pse) & !grepl(" No",DataEntry_relase$location_name_pse) & !grepl(" Na",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" N"," North",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" S",DataEntry_relase$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" S",l),DataEntry_relase$location_name_pse)
}
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" S"," South",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" E",DataEntry_relase$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" E",l),DataEntry_relase$location_name_pse)
}
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" E"," East",DataEntry_relase$location_name_pse[cond])


char <- "UPFR"
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Upper Fraser)",DataEntry_relase$location_name_pse[cond])

char <- "LWFR"
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Lower Fraser)",DataEntry_relase$location_name_pse[cond])

char <- "JNST"
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Johnstone Strait)",DataEntry_relase$location_name_pse[cond])

char <- "TOMF"  # Upper Thompson ?!
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Upper Thompson)",DataEntry_relase$location_name_pse[cond])

char <- "TOMM"  #= Lower Thompson ?!
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Lower Thompson)",DataEntry_relase$location_name_pse[cond])

char <- "CCST"  # 
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Central Coast)",DataEntry_relase$location_name_pse[cond])

char <- "SKNA"  # 
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Skeena River)",DataEntry_relase$location_name_pse[cond])

char <- "SWVI"  # 
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Southwest Vancouver Island)",DataEntry_relase$location_name_pse[cond])

char <- "NWVI"  #
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Northwest Vancouver Island)",DataEntry_relase$location_name_pse[cond])

char <- "GSMN"  # 
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Strait of Georgia Mainland)",DataEntry_relase$location_name_pse[cond])

char <- "GSVI"  # 
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Strait of Georgia Vancouver Island)",DataEntry_relase$location_name_pse[cond])

char <- "QCI"  # "Queen Charlotte Islands", which is now Haida Gwaii
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Haida Gwaii)",DataEntry_relase$location_name_pse[cond])

char <- "NCST"  #
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (North Coast)",DataEntry_relase$location_name_pse[cond])

