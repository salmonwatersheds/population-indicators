
#'******************************************************************************
#' The goal of the script is to combine the cleaned NuSEDS data with the cleaned
#' data sent by other labs.
#'  
#' sent by Arianne Nickels from the Reynolds' Lab (SFU).
#' 
#' Files imported:
#' - nuseds_cuid_streamid_nuseds_20240419.csv : the cleaned NuSEDS data with PSE cuid and streamid (and with data not in PSE)
#' - data_extra_Reynolds_lab_DATE.csv : the cleaned  Reynolds' Lab (SFU) data (sent by  Arianne Nickels)
#' - SFU_Escapement_issues.csv        : 
#' - .../steelhead_dataset2.DATE.csv                             : dataset generated in its own repository
#' - data-input/columbia_dataset2.DATE.csv                       : dataset generated in its own repository
#' - data-input/yukon_dataset2.DATEYukon.csv                     : dataset generated in its own repository
#' - data-input/transboundary-data/output/dataset2_spawner_surveys.csv : dataset generated in its own repository
#' 
#' Files produced: 
#' - dataset2_spawner_surveys_DATE.csv  # previously dataset2_DATE.csv
#' 
#' 
#'******************************************************************************

# 
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
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS
wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

wd_data_dropbox <- paste(wd_X_Drive1_PROJECTS,
                         wds_l$wd_project_dropbox,
                         "data",sep="/")

wd_references_dropbox <- paste(wd_X_Drive1_PROJECTS,
                               wds_l$wd_project_dropbox,
                               "references",sep="/")

# The datasets to input were outputted by other scripts 
wd_data_input <- wd_output

# The wd in the population-data files
wd_population_data <- gsub("population-indicators/data-input","population-data",wd_pop_indic_data_input_dropbox)


# Import functions for this specific project
source("Code/functions.R")

library(xlsx)
library(readxl)
library(dplyr)

options(warn = 0)
options(digits = 9) ## 7

# Import files for MAIN NUSEDS UPDATE -------
#

#'* Import the cleaned NuSEDS data that includes the Reynolds data *
nuseds_cuid_streamid <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                                   pattern = "3_nuseds_cuid_streamid_Reynolds")

unique(nuseds_cuid_streamid$source_id)
# "NuSEDS_20251103"   "Reynolds_20260209"

nuseds_cuid_streamid$sys_nm <- nuseds_cuid_streamid$sys_nm |> toupper()


#'* Import the conservationunits_decoder.csv *
datasetsNames_database <- datasetsNames_database_fun()

#' Import the cuspawnerabundance.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' To calculating current spawner abundance for biostatus assessment
fromDatabase <- update_file_csv <- F


#'* Import the conservationunits_decoder.csv *
#' from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- datasets_database_fun(nameDataSet = "conservationunits_decoder.csv",
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

#'Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- F


#'* Import data from TBR *
#' Documentation about the corrections made mannually:
#' https://www.dropbox.com/scl/fi/5m93l1whnuqtcvi3h9rh0/NuSEDS_modifications_hardcode.docx?rlkey=c1fykonhk1cdvdg0i8aizsgkc&e=1&dl=0
region <- "transboundary-data"
data_TBR <- import_mostRecent_file_fun(wd = paste0(wd_population_data,"/",region,"/output/archive"), 
                                                   pattern = "dataset2_spawner-surveys")

# some corrections in the format
colnames(data_TBR)[colnames(data_TBR) == "survey_method"] <- "stream_survey_method"
colnames(data_TBR)[colnames(data_TBR) == "survey_qual"] <- "stream_survey_quality"

data_TBR$species_name[data_TBR$species_name %in% c("Pink (even)","Pink (odd)")] <- "Pink"
data_TBR$species_name[data_TBR$species_name %in% c("Lake sockeye","River sockeye")] <- "Sockeye"

unique(data_TBR$region)
data_TBR$region <- "Northern Transboundary"


# correction for cuid 1016 (SEL) --> should be resolved now
# https://pacificsalmonfdn.slack.com/archives/CKNVB4MCG/p1771895587752779
cond <- data_TBR$cuid == 1026
data_TBR[cond,c("region","cuid","cu_name_pse","stream_name_pse","longitude","latitude","streamid")] |> unique() # "Tahltan River" --> should be lake

cond_nuseds <- !is.na(nuseds_cuid_streamid$cuid) & nuseds_cuid_streamid$cuid == 1026
unique(nuseds_cuid_streamid[cond_nuseds,c("cuid","sys_nm","X_LONGT","Y_LAT")]) |> unique()

# data_TBR$stream_name_pse[cond] <- "Tahltan Lake"
# data_TBR$latitude[cond] <- nuseds_cuid_streamid$Y_LAT[cond_nuseds] |> unique()
# data_TBR$longitude[cond] <- nuseds_cuid_streamid$X_LONGT[cond_nuseds] |> unique()

data_TBR$stream_name_pse <- data_TBR$stream_name_pse |> toupper()


#'* Import data from Yukon *
region <- "yukon-data"
data_yukon <- import_mostRecent_file_fun(wd = paste0(wd_population_data,"/",region,"/output/archive"), 
                                       pattern = "dataset2_spawner-surveys")


data_yukon$stream_name_pse <- data_yukon$stream_name_pse |> toupper()


#'* Import data from Central Coast *
region <- "central-coast-data"
data_cc <- import_mostRecent_file_fun(wd = paste0(wd_population_data,"/",region,"/output"), 
                                         pattern = "dataset2_spawner-surveys")
# "File imported: dataset2_spawner-surveys_CentralCoast_2025-04-03.csv ; Date modified: 2025-04-03 09:11:32"

data_cc$stream_name_pse <- data_cc$stream_name_pse |> toupper()


#'* Import data from Steelhead *
region <- "steelhead-data"
data_SH <- import_mostRecent_file_fun(wd = paste0(wd_population_data,"/",region,"/output"), 
                                         pattern = "dataset2_spawner-surveys")
# 2025-03-26.csv ; Date modified: 2025-04-15

data_SH$stream_name_pse <- data_SH$stream_name_pse |> toupper()


#'* Import data from Columbia ??? *
region <- "columbia-data"
# data_columbia <- import_mostRecent_file_fun(wd = paste0(wd_population_data,"/",region,"/output/archive"), 
#                                          pattern = "dataset2_spawner-surveys")

# data_columbia <- import_mostRecent_file_fun(wd = wd_pop_indic_data_input_dropbox,
#                                             pattern = "columbia_dataset2") # TEMPORARY ???

# There is this dataset:
data_Columbia <- import_mostRecent_file_fun(wd = paste0(wd_population_data,"/",region,"/data"), 
                                            pattern = "dataset2_spawner-surveys")
# "File imported: dataset2_spawner-surveys_ColumbiaSteelhead_2025-03-25.csv ; Date modified: 2025-03-26 15:21:10"

# Is this SH data already in data_SH?
unique(data_Columbia$region)
unique(data_Columbia$species_name)

colnames(data_Columbia)
all(data_Columbia$streamid %in% data_SH$streamid) # TRUE

for(sid in unique(data_Columbia$streamid)){
  # sid <- unique(data_Columbia$streamid)[1]
  
  cond_columbia <- data_Columbia$streamid == sid
  data_Columbia_here <- unique(data_Columbia[cond_columbia,c("cuid","stream_name_pse","streamid","year","stream_observed_count")])
  data_Columbia_here <- data_Columbia_here[order(data_Columbia_here$year),]
  rownames(data_Columbia_here) <- NULL
  
  cond_SH <- data_SH$streamid == sid
  data_SH_here <- unique(data_SH[cond_SH,c("cuid","stream_name_pse","streamid","year","stream_observed_count")])
  data_SH_here <- data_SH_here[order(data_SH_here$year),]
  rownames(data_SH_here) <- NULL
  
  if(!identical(data_Columbia_here,data_SH_here)){
    print("Not identical - BREAK")
    break
  }
}

#'* Import streamlocationids to obtain the streamid *
streamlocationids <- datasets_database_fun(nameDataSet = "streamlocationids.csv", # datasetsNames_database$name_CSV[9],
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)

# correction
# https://pacificsalmonfdn.slack.com/archives/CKNVB4MCG/p1771895587752779

streamlocationids$correction <- NA
streamlocationids$data_present <- NA

cond <- streamlocationids$cuid == 1026 & streamlocationids$sys_nm == "Tahltan Lake"
streamlocationids[cond,c("sys_nm","GFE_ID","longitude","latitude","cu_name_pse","cuid","streamid")]

cond_nuseds <- !is.na(nuseds_cuid_streamid$cuid) & nuseds_cuid_streamid$cuid == 1026
unique(nuseds_cuid_streamid[cond_nuseds,c("cuid","GFE_ID","sys_nm","X_LONGT","Y_LAT")]) |> unique()

streamlocationids$latitude[cond] <- nuseds_cuid_streamid$Y_LAT[cond_nuseds] |> unique()
streamlocationids$longitude[cond] <- nuseds_cuid_streamid$X_LONGT[cond_nuseds] |> unique()
streamlocationids$GFE_ID[cond] <- nuseds_cuid_streamid$GFE_ID[cond_nuseds] |> unique()
streamlocationids$correction[cond] <- "Coordinates & GFE_ID was NA"
streamlocationids$data_present[cond] <- TRUE

# correct the streamid in nuseds:
nuseds_cuid_streamid$streamid[cond_nuseds] <- streamlocationids$streamid[cond] 

streamlocationids$sys_nm <- streamlocationids$sys_nm |> toupper()

# add the field region from the decoder
streamlocationids$region <- NA
for(rg_i in unique(streamlocationids$regionid)){
  cond <- conservationunits_decoder$drv_regionid == rg_i
  region <- unique(conservationunits_decoder$region[cond])
  
  cond <- streamlocationids$regionid == rg_i
  streamlocationids$region[cond] <- region
}

# add the field species_qualified from the decoder
streamlocationids$species_qualified <- NA
for(cuid in unique(streamlocationids$cuid)){
  cond <- conservationunits_decoder$cuid == cuid
  species_qualified <- unique(conservationunits_decoder$species_qualified[cond])
  
  cond <- streamlocationids$cuid == cuid
  streamlocationids$species_qualified[cond] <- species_qualified
}

head(streamlocationids)

# check for duplicates entries and remove them
sum(duplicated(streamlocationids$streamid)) # 4
cond <- duplicated(streamlocationids$streamid)
streamid_duplicated <- streamlocationids$streamid[cond]
cond <- streamlocationids$streamid %in% streamid_duplicated
streamlocationids[cond,]

# remove those
cond_remove <- cond & (streamlocationids$locationid == 28136 | is.na(streamlocationids$GFE_ID))
streamlocationids[cond_remove,]
streamlocationids <- streamlocationids[!cond_remove,]

# remove unnecessary and confusing fields
# streamlocationids <- streamlocationids[,c("region","sys_nm","GFE_ID","latitude","longitude","cu_name_pse","cuid","streamid")]
rownames(streamlocationids) <- NULL

# CORRRECTION in streamlocationids, the coordinates for PORCUPINE RIVER in the Yukon
# there is a PORCUPINE RIVER in the NTB TOO !
cond_pse <- streamlocationids$sys_nm == "PORCUPINE RIVER"
streamlocationids[cond_pse,]

cond_pse <- streamlocationids$sys_nm == "PORCUPINE RIVER" &
  streamlocationids$region == "Yukon"
streamlocationids[cond_pse,]

cond_rg <- data_yukon$stream_name_pse == "PORCUPINE RIVER"
data_yukon[cond_rg,][1,] # the correct coordinates

streamlocationids$latitude[cond_pse] <- unique(data_yukon$latitude[cond_rg])
streamlocationids$longitude[cond_pse] <- unique(data_yukon$longitude[cond_rg])
streamlocationids$GFE_ID[cond_pse] <- unique(data_yukon$GFE_ID[cond_rg])
streamlocationids$correction[cond_pse] <- "Coordinates & GFE_ID"

# all good so data_Columbia is not needed.

#'* Import the stream - GFE_ID data file from DFO *
#' (emailed from Wu Zhipeng, DFO, 09/04/2024)
#' There are different locations in the 2nd first sheets to we combine them into a unique dataframe
DFO_All_Streams_Segments <- read_xlsx(paste0(wd_data_dropbox,"/DFO_All_Streams_Segments_20240408.xlsx"),
                                      sheet = "All Streams")

DFO_All_Streams_Segments2 <- read_xlsx(paste0(wd_data_dropbox,"/DFO_All_Streams_Segments_20240408.xlsx"),
                                       sheet = "Stream Segments")

# Emailed as "GFE_IDs.xlsx"
# DFO_All_Streams_Segments3 <- read_xlsx(paste0(wd_data_dropbox,"/DFO_GFE_IDs_list_1.xlsx"),
#                                       sheet = "Export Worksheet")

# Emailed as "GFE_IDs_20240305.xlsx"
# DFO_All_Streams_Segments4 <- read_xlsx(paste0(wd_data_dropbox,"/DFO_GFE_IDs_list_2.xlsx"),
#                                        sheet = "Export Worksheet")

columns <- c("NME","ID","X_LONGT","Y_LAT")
DFO_All_Streams_Segments <- rbind(DFO_All_Streams_Segments[,columns],
                                  DFO_All_Streams_Segments2[,columns]
                                  # DFO_All_Streams_Segments3[,columns],
                                  # DFO_All_Streams_Segments4[,columns]
)
DFO_All_Streams_Segments <- unique(DFO_All_Streams_Segments)

rm(DFO_All_Streams_Segments2)
head(DFO_All_Streams_Segments)

#
#
# Fixes things with Central Coast ------
#
#'* Central Coast *

#' data_cc is just for SEL South Atnarko Lake (528)
#' - and the following POP_ID should be removed if they are not already:
#     - Atnarko Lakes Sockeye (POP_ID 7818) - just one datapoint
#     - Atnarko River (POP_ID 51780)
#' https://pacificsalmonfdn.slack.com/archives/CJ5RVHVCG/p1743696717590829

head(data_cc)

data_cc$GFE_ID <- NA

cond <- !is.na(nuseds_cuid_streamid$POP_ID) & nuseds_cuid_streamid$POP_ID %in% c(7818,51780)
nuseds_cuid_streamid[cond,c("region","SPECIES_QUALIFIED","cuid","POP_ID",
                            "cu_name_pse","streamid","sys_nm","GFE_ID",
                            "Y_LAT","X_LONGT")] |> unique()

# Plot the data
yr_range_nuseds <- range(nuseds_cuid_streamid$Year[cond])
count_range_nuseds <- range(nuseds_cuid_streamid$MAX_ESTIMATE[cond], na.rm = T)

yr_range <- range(data_cc$year,yr_range_nuseds)
count_max <- max(count_range_nuseds,data_cc$stream_observed_count, na.rm = T)

plot(x = data_cc$year, y = data_cc$stream_observed_count, lwd = 2, type = "l", 
     xlim = yr_range, ylim = c(0,count_max))
points(x = data_cc$year, y = data_cc$stream_observed_count, lwd = 2, pch = 16)
leg1 <- paste(data_cc$species_qualified,data_cc$cuid,data_cc$cu_name_pse,data_cc$stream_name_pse, sep = " - ") |> unique()

cond <- !is.na(nuseds_cuid_streamid$POP_ID) & nuseds_cuid_streamid$POP_ID %in% c(7818)
data <- nuseds_cuid_streamid[cond,]
data <- data[order(data$Year),]
lines(x = data$Year, y = data$MAX_ESTIMATE, lwd = 2, col = "blue")
points(x = data$Year, y = data$MAX_ESTIMATE, lwd = 2, col = "blue", pch = 1, cex = 2)
leg2 <- paste(data$SPECIES_QUALIFIED,data$cuid,data$cu_name_pse,data$sys_nm, sep = " - ") |> unique()

cond <- !is.na(nuseds_cuid_streamid$POP_ID) & nuseds_cuid_streamid$POP_ID %in% c(51780)
data <- nuseds_cuid_streamid[cond,]
data <- data[order(data$Year),]
lines(x = data$Year, y = data$MAX_ESTIMATE, lwd = 2, col = "red")
points(x = data$Year, y = data$MAX_ESTIMATE, lwd = 2, col = "red", pch = 1, cex = 2)
leg3 <- paste(data$SPECIES_QUALIFIED,data$cuid,data$cu_name_pse,data$sys_nm, sep = " - ") |> unique()

legend("topright",c(leg1,leg2,leg3), lwd = 2, col = c("black","blue","red"), pch = c(16,1,1), bty = "n")

# ISSUE: The coordinates for "Atnarko River" were defined manually, as shown by the comment
# I pasted here (from the code in population-data/central-coast-data/code/atnarko-sockeye-comparison.R):
# "
# latitude = rep(52.341852, n_obs) # Put just below Stillwater, but includes counts above and below in some years
# longitude = rep(-125.771562, n_obs)
# "

# BUT in NUSEDS, "Atnarko River" and "Atnarko Lake" have GFE_IDs and coordinates
# PLUS: SEL should be associated to "Atnarko Lake", not "Atnarko River"
cond <- grepl("ATNARKO",nuseds_cuid_streamid$sys_nm) & nuseds_cuid_streamid$SPECIES_QUALIFIED %in% c("SEL","SER")
nuseds_cuid_streamid[cond,c("sys_nm","GFE_ID","Y_LAT","X_LONGT","SPECIES_QUALIFIED","cuid","streamid")] |> unique()

#' SO two things to do:
#' - change "river" to "lake"
#' - edit the coordinates and the GFE_ID accordingly
cond_nuseds <- nuseds_cuid_streamid$sys_nm == "ATNARKO LAKES"
data_cc$stream_name_pse <- nuseds_cuid_streamid$sys_nm[cond_nuseds] |> unique()
data_cc$latitude <- nuseds_cuid_streamid$Y_LAT[cond_nuseds] |> unique()
data_cc$longitude <- nuseds_cuid_streamid$X_LONGT[cond_nuseds] |> unique()
data_cc$GFE_ID <- nuseds_cuid_streamid$GFE_ID[cond_nuseds] |> unique()


# Check that things are correct in streamlocationids --> it is not
cond_pse <- streamlocationids$cuid == 528
streamlocationids[cond_pse,]

streamlocationids$sys_nm[cond_pse] <- nuseds_cuid_streamid$sys_nm[cond_nuseds] |> unique()
streamlocationids$latitude[cond_pse] <- nuseds_cuid_streamid$Y_LAT[cond_nuseds] |> unique()
streamlocationids$longitude[cond_pse] <- nuseds_cuid_streamid$X_LONGT[cond_nuseds] |> unique()
streamlocationids$GFE_ID[cond_pse] <- nuseds_cuid_streamid$GFE_ID[cond_nuseds] |> unique()
streamlocationids$correction[cond_pse] <- "coordinates, sys_nm and GFE_ID was NA"
streamlocationids$data_present[cond_pse] <- T

# edit streamid in data_cc (which is NA)
data_cc$streamid <- streamlocationids$streamid[cond_pse]


# remove the data for the two POP_ID in nuseds_cuid_streamid
cond <- !is.na(nuseds_cuid_streamid$POP_ID) & nuseds_cuid_streamid$POP_ID %in% c(7818,51780)
nuseds_cuid_streamid <- nuseds_cuid_streamid[!cond,]


#
# Corrections of time series in TBR NOW DONE IN 1_nuseds_collation.rmd -------

# nuseds_cuid_streamid_copy <- nuseds_cuid_streamid
# nuseds_cuid_streamid <- nuseds_cuid_streamid_copy

#' There are changes to do to the data; these changes should ideally be done 
#' in 1_nuseds_collation.rmd, but we do then 
# file:///C:/Users/bcarturan/Salmon%20Watersheds%20Dropbox/Bruno%20Carturan/X%20Drive/1_PROJECTS/1_Active/Population%20Methods%20and%20Analysis/population-data/transboundary-data/README.html
# https://www.dropbox.com/scl/fi/5m93l1whnuqtcvi3h9rh0/NuSEDS_modifications_hardcode.docx?rlkey=c1fykonhk1cdvdg0i8aizsgkc&e=1&dl=0
# We first check that the changes in the document above are done, if not do them

# Import the last original conservation_unit_census_sites dataset:
# conservation_unit_census_sites <- import_mostRecent_file_fun(wd = wd_data_dropbox, 
#                                                              pattern = "Conservation Unit Census Sites") # previously: "conservation_unit_census_sites"

# Import the last original all_areas_nuseds dataset:
all_areas_nuseds <- import_mostRecent_file_fun(wd = wd_data_dropbox, 
                                               pattern = "All Areas NuSEDS") # previously: "all_areas_nuseds"

series_removed <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                             pattern = "series_removed")

#'* CHECK *
#' POP_ID = 45144: Revise name of sockeye survey from Tahltan River to Tahltan Lake (Jason Calvert, pers. comm. January 9, 2025)
cond <- !is.na(nuseds_cuid_streamid$POP_ID) & nuseds_cuid_streamid$POP_ID == 45144
nuseds_cuid_streamid[cond,c("region","SPECIES_QUALIFIED","cuid",'POPULATION',"sys_nm","WATERBODY")] |> unique()
#' --> ALL GOOD NOTHING TO DO

#'* CHECK *
#' TATSAMENIE RIVER coho (POP_ID = 45152) for 1994 and earlier changed to 
#' TATSATUA RIVER (45154) and remaining records 1995+ are removed.

cond <- !is.na(nuseds_cuid_streamid$POP_ID) & nuseds_cuid_streamid$POP_ID %in% c(45152,45154)
data <- nuseds_cuid_streamid[cond,c("region","SPECIES_QUALIFIED","cu_name_pse","IndexId","cuid",'POPULATION',"sys_nm",
                                    "CENSUS_SITE","GFE_ID","X_LONGT","Y_LAT")] |> unique()
data

plot_IndexId_GFE_ID_fun(IndexIds = data$IndexId, 
                        GFE_IDs = data$GFE_ID, 
                        all_areas_nuseds = nuseds_cuid_streamid)
legend("top",data$POPULATION,bty = "n")
legend("topright",data$CENSUS_SITE,bty = "n")
mtext("POPULATION",side = 3,line = 1, adj = .5)
mtext("CENSUS_SITE",side = 3,line = 1, adj = 1)
mtext(data$cu_name_pse[2],side = 3,line = 1, adj = 0)
mtext(data$cu_name_pse[1],side = 3,line = 2, adj = 0)

#' DECISION MADE:
#' - for year <= 1994: replace POP_ID 45152 --> 45154 BUT keep GFE_ID = 2211
#' - for year > 1994: delete 

#' Note that the GFE_ID was not changed because POP_ID = 45154 is associated to 
#' another time series; so now the same POP_ID is associated to two GFE_IDs,
#' which is not ideal but not too bad either.

# Check that the change was recorded:
cond <- series_removed$IndexId == "CO_45152"
series_removed[cond,]

#' 45152 does not exist anymore --> ALL GOOD NOTHING TO DO


#'* CHECK *
#' Any records of 45151 (SEL) for 1994 or earlier get changed to 45153 (TATSATUA RIVER river-type sockeye)
cond <- !is.na(nuseds_cuid_streamid$POP_ID) & nuseds_cuid_streamid$POP_ID %in% c(45151,45153)
data <- nuseds_cuid_streamid[cond,c("region","SPECIES_QUALIFIED","cu_name_pse",
                                    "IndexId","cuid",'POPULATION',"sys_nm","CENSUS_SITE","GFE_ID","X_LONGT","Y_LAT")] |> unique()
data

plot_IndexId_GFE_ID_fun(IndexIds = data$IndexId, 
                        GFE_IDs = data$GFE_ID, 
                        all_areas_nuseds = nuseds_cuid_streamid)
legend("top",data$POPULATION,bty = "n")
legend("topright",data$CENSUS_SITE,bty = "n")
mtext("POPULATION",side = 3,line = 1, adj = .5)
mtext("CENSUS_SITE",side = 3,line = 1, adj = 1)
mtext(data$SPECIES_QUALIFIED[2],side = 3,line = 1, adj = 0)
mtext(data$SPECIES_QUALIFIED[1],side = 3,line = 2, adj = 0)

#' DECISION:
#' - delete series POP_ID = SX_45153 and GFE_ID = 2212
#' - for years <= 1994: change IndexId from 45151 to 45153 and GFE_ID from 2211 to 2212

#' ALL GOOD NOTHING TO DO


#'* CHECK * 
#' one record with 45165 change to 45164 (NAHLIN Chinook)
cond <- !is.na(nuseds_cuid_streamid$POP_ID) & nuseds_cuid_streamid$POP_ID %in% c(45165,45164)
nuseds_cuid_streamid[cond,c("region","IndexId","cuid",'POPULATION',"sys_nm","GFE_ID")] |> unique()

plot_IndexId_GFE_ID_fun(IndexIds = c("CN_45164","CN_45165"), 
                        GFE_IDs = c(2224,2224), 
                        all_areas_nuseds = nuseds_cuid_streamid)

cond <- series_removed$IndexId == "CN_45165"
series_removed[cond,]

# --> ALL GOOD, already done in 1_nuseds_collation.rmd

#
# Uniformise the locations & streamid between NuSEDS and streamlocationids -----
#

nuseds_cuid_streamid_copy <- nuseds_cuid_streamid
# nuseds_cuid_streamid <- nuseds_cuid_streamid_copy

# Remove in nuseds_cuid_streamid the data without a cuid
cond <- is.na(nuseds_cuid_streamid$cuid)
nuseds_cuid_streamid <- nuseds_cuid_streamid[!cond,]
nrow(nuseds_cuid_streamid) # 313697


# Check if there are GFE_ID attributed to different locations
nuseds_locations <- unique(nuseds_cuid_streamid[,c("region","CENSUS_SITE","sys_nm","Y_LAT","X_LONGT","GFE_ID")])
nrow(nuseds_locations) # 2536 (2547 --> with cuid = NA)

cond_GFE_ID_nuseds <- !is.na(nuseds_locations$GFE_ID)
sum(!cond_GFE_ID_nuseds) # 8
sum(cond_GFE_ID_nuseds) # 2528
length(unique(nuseds_locations$GFE_ID[cond_GFE_ID_nuseds])) # 2366

GFE_ID_dupli <- nuseds_locations$GFE_ID[cond_GFE_ID_nuseds][duplicated(nuseds_locations$GFE_ID[cond_GFE_ID_nuseds])]
cond <- nuseds_locations$GFE_ID[cond_GFE_ID_nuseds] %in% GFE_ID_dupli
show <- nuseds_locations[cond_GFE_ID_nuseds,][cond,]
show <- show[order(show$GFE_ID),]
show

#' Explanation: there are duplicated instances because the region is related to the CU,
#' not the stream location so a same GFE_ID can belong to two regions if it is 
#' associated with CUs belonging to those regions.

nuseds_locations <- unique(nuseds_cuid_streamid[,c("CENSUS_SITE","sys_nm","Y_LAT","X_LONGT","GFE_ID")])
nrow(nuseds_locations) # 2374

cond_GFE_ID_nuseds <- !is.na(nuseds_locations$GFE_ID)
sum(!cond_GFE_ID_nuseds) # 8
sum(cond_GFE_ID_nuseds) # 2366
length(unique(nuseds_locations$GFE_ID[cond_GFE_ID_nuseds])) # 2366
# --> number of GFE_IDs = number of rows in  nuseds_locations[cond_GFE_ID_nuseds,]
# --> all good


#'* Make sure the locations in streamlocationids matches with NuSEDS *

# Make sure the locations with a GFE_ID match, if not, correct them

cond_GFE_ID_pse <- !is.na(streamlocationids$GFE_ID)
sum(!cond_GFE_ID_pse) # 91 93
sum(cond_GFE_ID_pse)  # 6826 6824

streamlocationids_copy <- streamlocationids
# streamlocationids <- streamlocationids_copy

nuseds_locations$check <- NA
stop <- F
threshold <- 5
for(r in 1:nrow(nuseds_locations[cond_GFE_ID_nuseds,])){ # no GFE_ID with NA
  # r <- 1
  # r <- which(nuseds_locations$GFE_ID == 2138) # PORCUPINE RIVER in the TBR region
  # r <- which(is.na(nuseds_locations$GFE_ID) & nuseds_locations$sys_nm == "PORCUPINE RIVER") # PORCUPINE RIVER in the Yukon region
  GFE_ID <- nuseds_locations$GFE_ID[cond_GFE_ID_nuseds][r]
  sys_nm <- nuseds_locations$sys_nm[cond_GFE_ID_nuseds][r]
  X_LONGT <- nuseds_locations$X_LONGT[cond_GFE_ID_nuseds][r]
  Y_LAT <- nuseds_locations$Y_LAT[cond_GFE_ID_nuseds][r]
  
  cond_GFE_ID <- !is.na(streamlocationids$GFE_ID) & streamlocationids$GFE_ID == GFE_ID
  
  cond_sys_nm <- simplify_string_fun(streamlocationids$sys_nm) == simplify_string_fun(sys_nm)
  
  cond_coordinates <- round(streamlocationids$latitude,6) == round(Y_LAT,6) & 
    round(streamlocationids$longitude,6) == round(X_LONGT,6)
  
  if(any(cond_GFE_ID & cond_sys_nm & cond_coordinates)){
    
    streamlocationids$correction[cond_GFE_ID & cond_sys_nm] <- "None"
    streamlocationids$data_present[cond_GFE_ID & cond_sys_nm] <- TRUE
    nuseds_locations$check[cond_GFE_ID_nuseds][r] <- FALSE
    
  }else if(any(cond_GFE_ID & cond_sys_nm & !cond_coordinates)){ # coordinates are wrong ; silenced because that happens a lot
    # print("Change coordinates from:")
    # print(unique(streamlocationids[cond_GFE_ID & cond_sys_nm ,c("sys_nm","latitude","longitude","GFE_ID")]))
    
    streamlocationids$latitude[cond_GFE_ID & cond_sys_nm] <- Y_LAT
    streamlocationids$longitude[cond_GFE_ID & cond_sys_nm] <- X_LONGT
    
    streamlocationids$correction[cond_GFE_ID & cond_sys_nm] <- "Coordinates"
    streamlocationids$data_present[cond_GFE_ID & cond_sys_nm] <- TRUE
    nuseds_locations$check[cond_GFE_ID_nuseds][r] <- FALSE
    
    # print("to:")
    # print(unique(streamlocationids[cond_GFE_ID & cond_sys_nm ,c("sys_nm","latitude","longitude","GFE_ID")]))
    # print("***")
    
  }else if(any(cond_GFE_ID & !cond_sys_nm & cond_coordinates)){
    print("Change sys_nm from:")
    print(unique(streamlocationids[cond_GFE_ID & cond_sys_nm ,c("sys_nm","latitude","longitude","GFE_ID")]))
    
    streamlocationids$sys_nm[cond_GFE_ID & cond_sys_nm] <- sys_nm
    
    streamlocationids$correction[cond_GFE_ID & cond_sys_nm] <- "sys_nm"
    streamlocationids$data_present[cond_GFE_ID & cond_sys_nm] <- TRUE
    nuseds_locations$check[cond_GFE_ID_nuseds][r] <- FALSE
    
    print("to:")
    print(unique(streamlocationids[cond_GFE_ID & cond_sys_nm ,c("sys_nm","latitude","longitude","GFE_ID")]))
    print("***")
    print("***")
    
  }else if(any(!cond_GFE_ID & cond_sys_nm & cond_coordinates)){
    
    GFE_ID_here <- unique(streamlocationids$GFE_ID[cond_sys_nm & cond_coordinates])
    
    if(!is.na(GFE_ID_here)){ # if there is a GFE_ID: then show message
      print("Change GFE_ID from:")
      print(unique(streamlocationids[cond_sys_nm & cond_coordinates,c("sys_nm","latitude","longitude","GFE_ID")]))
      
      streamlocationids$GFE_ID[cond_sys_nm & cond_coordinates] <- GFE_ID
      streamlocationids$correction[cond_sys_nm & cond_coordinates] <- "GFE_ID"
      
      print("to:")
      print(unique(streamlocationids[cond_sys_nm & cond_coordinates ,c("sys_nm","latitude","longitude","GFE_ID")]))
      print("***")
      print("***")
      
    }else{ # otherwise do not: there are many instances where the GFE_ID was NA
      streamlocationids$GFE_ID[cond_sys_nm & cond_coordinates] <- GFE_ID
      streamlocationids$correction[cond_sys_nm & cond_coordinates] <- "GFE_ID was NA"
    }
    
    streamlocationids$data_present[cond_GFE_ID & cond_sys_nm] <- TRUE
    nuseds_locations$check[cond_GFE_ID_nuseds][r] <- FALSE
    
  }else if(any(cond_GFE_ID & !cond_sys_nm & !cond_coordinates)){ 
    print("Same GFE_ID but need to change both sys_nm and coordinates from:")
    print(unique(streamlocationids[cond_GFE_ID & cond_sys_nm ,c("sys_nm","latitude","longitude","GFE_ID")]))
    
    streamlocationids$latitude[cond_GFE_ID & cond_sys_nm] <- Y_LAT
    streamlocationids$longitude[cond_GFE_ID & cond_sys_nm] <- X_LONGT
    streamlocationids$sys_nm[cond_GFE_ID & cond_sys_nm] <- sys_nm
    
    streamlocationids$correction[cond_GFE_ID & cond_sys_nm] <- "Coordinates & sys_nm"
    streamlocationids$data_present[cond_GFE_ID & cond_sys_nm] <- TRUE
    nuseds_locations$check[cond_GFE_ID_nuseds][r] <- FALSE
    
    print("to:")
    print(unique(streamlocationids[cond_GFE_ID & cond_sys_nm ,c("sys_nm","latitude","longitude","GFE_ID")]))
    print("***")
    print("***")
    
  }else if(any(!cond_GFE_ID & cond_sys_nm & !cond_coordinates)){
    
    print("CHECK same sys_nm but rest is different... check location after")
    
    nuseds_locations$check[cond_GFE_ID_nuseds][r] <- TRUE
    
    # check if there is a location with the same coordinates in nuseds
    # data_pse <- unique(streamlocationids[cond_sys_nm ,c("sys_nm","latitude","longitude","GFE_ID")])
    # 
    # for(rr in 1:nrow(data_pse)){
    #   
    #   if(!is.na(data_pse$GFE_ID[rr])){
    #     cond_here <- !is.na(nuseds_locations$GFE_ID) & nuseds_locations$GFE_ID == GFE_ID
    #   }else{
    #     cond_here <- round(nuseds_locations$Y_LAT,6) == data_pse$latitude[rr] & 
    #       round(nuseds_locations$X_LONGT,6) == data_pse$longitude[rr]
    #   }
    #   
    #   if(any(cond_here)){ # if there is another location with the different sys_nm and same GFE_ID or coordinates in NuSEDS --> so leave it
    #     
    #     # print("... the coordinates are associated to another location in NuSEDS - BREAK")
    #     # print(data_pse[rr,])
    #     # print("NuSEDS:")
    #     # print(nuseds_locations[cond_here,])
    #     # stop <- T
    #     
    #   }else{ # 
    #     
    #     print("... coordinates and GFE_ID are changed from:")
    #     print(unique(streamlocationids[cond_sys_nm,c("sys_nm","latitude","longitude","GFE_ID")]))
    #     
    #     i <- which(streamlocationids$latitude[cond_sys_nm] == data_pse$latitude[rr] & 
    #                  streamlocationids$longitude[cond_sys_nm] == data_pse$longitude[rr])
    #     
    #     streamlocationids$latitude[cond_sys_nm][i] <- Y_LAT
    #     streamlocationids$longitude[cond_sys_nm][i] <- X_LONGT
    #     streamlocationids$GFE_ID[cond_sys_nm][i] <- GFE_ID
    #     
    #     print("to:")
    #     print(unique(streamlocationids[cond_sys_nm,c("sys_nm","latitude","longitude","GFE_ID")]))
    #     print("***")
    #     print("***")
    #   }
    # }
    
  }else if(any(!cond_GFE_ID & !cond_sys_nm & cond_coordinates)){
    
    print("CHECK same coordinates but rest is different... check location after")
    
    nuseds_locations$check[cond_GFE_ID_nuseds][r] <- TRUE
    
    # check if streamlocationids$sys_nm[cond_coordinates] is in nuseds with others coordinates
    # cond_here <- simplify_string_fun(nuseds_locations$sys_nm) == simplify_string_fun(streamlocationids$sys_nm[cond_coordinates])
    # if(any(cond_here)){
    #   print("... the focal sys_nm is in nuseds under a differnt GFE_ID and coordinates - BREAK")
    #   print(unique(streamlocationids[cond_coordinates,c("sys_nm","latitude","longitude","GFE_ID")]))
    #   print("NuSEDS:")
    #   print(nuseds_locations[r,])
    #   stop <- T
    #   
    # }else{
    #   print("... sys_nm and GFE_ID are changed from:")
    #   print(unique(streamlocationids[cond_coordinates,c("sys_nm","latitude","longitude","GFE_ID")]))
    #   
    #   streamlocationids$sys_nm[cond_coordinates] <- sys_nm
    #   streamlocationids$GFE_ID[cond_coordinates] <- GFE_ID
    #   
    #   print("to:")
    #   print(unique(streamlocationids[cond_coordinates,c("sys_nm","latitude","longitude","GFE_ID")]))
    #   print("***")
    #   print("***")
    # }
  }
  if(stop){
    break
  }
  ratio <- r /sum(cond_GFE_ID_nuseds) * 100
  if(threshold < ratio){
    print(paste0(threshold,"%"))
    threshold <- threshold + 5
  }
}

nuseds_cuid_streamid_copy1 <- nuseds_cuid_streamid

# nuseds_cuid_streamid <- nuseds_cuid_streamid_copy1
# streamlocationids <- streamlocationids_copy

# Check the ones to check:
table(nuseds_locations$check, useNA = "always")
#  FALSE  TRUE  <NA> 
#   2324     6    44 

cond_check <- !is.na(nuseds_locations$check) & nuseds_locations$check == T
nuseds_locations[cond_check,]

r <- 1 # KOWATUA CREEK 58.7716407 -132.265713   2221
nuseds_locations[cond_check,][r,]
GFE_ID <- nuseds_locations$GFE_ID[cond_check][r]
sys_nm <- nuseds_locations$sys_nm[cond_check][r]
X_LONGT <- nuseds_locations$X_LONGT[cond_check][r]
Y_LAT <- nuseds_locations$Y_LAT[cond_check][r]

cond_GFE_ID <- !is.na(streamlocationids$GFE_ID) & streamlocationids$GFE_ID == GFE_ID
cond_sys_nm <- simplify_string_fun(streamlocationids$sys_nm) == simplify_string_fun(sys_nm)
cond_coordinates <- round(streamlocationids$latitude,6) == round(Y_LAT,6) & 
  round(streamlocationids$longitude,6) == round(X_LONGT,6)

any(cond_GFE_ID)
any(cond_sys_nm)
any(cond_coordinates)

streamlocationids[cond_coordinates,] # Kowatua River --> is there also a Kowatua River in nuseds ? --> no
cond <- simplify_string_fun(nuseds_cuid_streamid$sys_nm) == simplify_string_fun("Kowatua River")
unique(nuseds_cuid_streamid[cond,c("region","cu_name_pse","cuid","GFE_ID")])
cond <- grepl("kowatua",simplify_string_fun(nuseds_cuid_streamid$sys_nm))
unique(nuseds_cuid_streamid[cond,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","streamid")]) # --> the one
#                 region      cu_name_pse cuid GFE_ID streamid
# Northern Transboundary Taku-Late Timing 1008   2221    10552

streamlocationids[cond_coordinates,]$GFE_ID <- nuseds_locations[cond_check,][r,]$GFE_ID
streamlocationids[cond_coordinates,]$sys_nm <- "Kowatua Creek" |> toupper()
streamlocationids[cond_coordinates,]$correction <- "sys_nm & GFE_ID was NA"
streamlocationids[cond_coordinates,]$data_present <- TRUE

# edit streamid in NuSEDS
nuseds_cuid_streamid$streamid[cond] <- streamlocationids[cond_coordinates,]$streamid 


r <- 2 # NAKINA RIVER 58.9042877 -133.145891   2216
nuseds_locations[cond_check,][r,]
GFE_ID <- nuseds_locations$GFE_ID[cond_check][r]
sys_nm <- nuseds_locations$sys_nm[cond_check][r]
X_LONGT <- nuseds_locations$X_LONGT[cond_check][r]
Y_LAT <- nuseds_locations$Y_LAT[cond_check][r]

cond_GFE_ID <- !is.na(streamlocationids$GFE_ID) & streamlocationids$GFE_ID == GFE_ID
cond_sys_nm <- simplify_string_fun(streamlocationids$sys_nm) == simplify_string_fun(sys_nm)
cond_coordinates <- round(streamlocationids$latitude,6) == round(Y_LAT,6) & 
  round(streamlocationids$longitude,6) == round(X_LONGT,6)

any(cond_GFE_ID)
any(cond_sys_nm)
any(cond_coordinates)

streamlocationids[cond_sys_nm,] # 
cond <- simplify_string_fun(nuseds_cuid_streamid$sys_nm) == simplify_string_fun("Nakina River")
unique(nuseds_cuid_streamid[cond,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","Y_LAT","X_LONGT","streamid")])
# same populations --> correct streamlocationids

streamlocationids[cond_sys_nm,]$GFE_ID <- GFE_ID
streamlocationids[cond_sys_nm,]$latitude <- Y_LAT
streamlocationids[cond_sys_nm,]$longitude <- X_LONGT
streamlocationids[cond_sys_nm,]$correction <- "Coordinates & GFE_ID was NA"
streamlocationids[cond_sys_nm,]$data_present <- TRUE

# edit streamid in NuSEDS
for(row in 1:nrow(streamlocationids[cond_sys_nm,])){
  # row <- 1
  cuid <- streamlocationids[cond_sys_nm,]$cuid[row]
  streamid <- streamlocationids[cond_sys_nm,]$streamid[row]
  cond <- !is.na(nuseds_cuid_streamid$cuid) & nuseds_cuid_streamid$cuid == cuid &
    !is.na(nuseds_cuid_streamid$GFE_ID) & nuseds_cuid_streamid$GFE_ID == GFE_ID
  
  nuseds_cuid_streamid$streamid[cond] <- streamid
}


r <- 3 # EAGLE CREEK EAGLE CREEK 49.5049227 -122.014251  52747
nuseds_locations[cond_check,][r,]
GFE_ID <- nuseds_locations$GFE_ID[cond_check][r]
sys_nm <- nuseds_locations$sys_nm[cond_check][r]
X_LONGT <- nuseds_locations$X_LONGT[cond_check][r]
Y_LAT <- nuseds_locations$Y_LAT[cond_check][r]

cond_GFE_ID <- !is.na(streamlocationids$GFE_ID) & streamlocationids$GFE_ID == GFE_ID
cond_sys_nm <- simplify_string_fun(streamlocationids$sys_nm) == simplify_string_fun(sys_nm)
cond_coordinates <- round(streamlocationids$latitude,6) == round(Y_LAT,6) & 
  round(streamlocationids$longitude,6) == round(X_LONGT,6)

any(cond_GFE_ID)
any(cond_sys_nm)
any(cond_coordinates)

nuseds_locations[cond_check,][r,]
streamlocationids[cond_sys_nm,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","latitude","longitude","streamid")] # 
streamlocationids[cond_sys_nm,c("region","GFE_ID","sys_nm","latitude","longitude")] |> unique() 

# Are those GFE_ID in nuseds too? --> yes all of them and the streamid match
cond <- nuseds_cuid_streamid$GFE_ID %in% c(1887,701,441)
unique(nuseds_cuid_streamid[cond,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","Y_LAT","X_LONGT","streamid")])

# conclusion: this is a new location with the same name --> add it to streamlocationids
cond <- !is.na(nuseds_cuid_streamid$GFE_ID) & nuseds_cuid_streamid$GFE_ID == GFE_ID
unique(nuseds_cuid_streamid[cond,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","Y_LAT","X_LONGT","streamid")])
unique(nuseds_cuid_streamid$cuid[cond])

new <- streamlocationids[1,]
new$region <- unique(nuseds_cuid_streamid$region[cond])
new$regionid <- streamlocationids$regionid[streamlocationids$region == new$region] |> unique()

# check if streamid is already in 
any(streamlocationids$streamid == unique(nuseds_cuid_streamid$streamid[cond])) # FALSE
new$streamid <- unique(nuseds_cuid_streamid$streamid[cond])

new$species_qualified <- unique(nuseds_cuid_streamid$SPECIES_QUALIFIED[cond])
new$locationid <- NA
new$pointid <- NA
new$sys_nm <- "Eagle Creek" |> toupper()
new$cuid <- unique(nuseds_cuid_streamid$cuid[cond])
new$latitude <- unique(nuseds_cuid_streamid$Y_LAT[cond])
new$longitude <- unique(nuseds_cuid_streamid$X_LONGT[cond])
new$cu_name_pse <- unique(nuseds_cuid_streamid$cu_name_pse[cond])
new$GFE_ID <- unique(nuseds_cuid_streamid$GFE_ID[cond])
new$correction <- "new entry"
new$data_present <- TRUE
streamlocationids <- rbind(streamlocationids,new)


r <- 4 # KATETE RIVER 56.63627 -131.843732   2571
nuseds_locations[cond_check,][r,]
GFE_ID <- nuseds_locations$GFE_ID[cond_check][r]
sys_nm <- nuseds_locations$sys_nm[cond_check][r]
X_LONGT <- nuseds_locations$X_LONGT[cond_check][r]
Y_LAT <- nuseds_locations$Y_LAT[cond_check][r]

cond_GFE_ID <- !is.na(streamlocationids$GFE_ID) & streamlocationids$GFE_ID == GFE_ID
cond_sys_nm <- simplify_string_fun(streamlocationids$sys_nm) == simplify_string_fun(sys_nm)
cond_coordinates <- round(streamlocationids$latitude,6) == round(Y_LAT,6) & 
  round(streamlocationids$longitude,6) == round(X_LONGT,6)

any(cond_GFE_ID)
any(cond_sys_nm)
any(cond_coordinates)

nuseds_locations[cond_check,][r,]

# CHECK 1
streamlocationids[cond_sys_nm & cond_coordinates,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","latitude","longitude","streamid")] # none
streamlocationids[cond_coordinates,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","latitude","longitude","streamid")] 
#                 region   cu_name_pse cuid GFE_ID            sys_nm latitude   longitude streamid
# Northern Transboundary Lower Stikine 1017     NA West Katete River 56.63627 -131.843732    10157

cond <- grepl("katete",simplify_string_fun(nuseds_cuid_streamid$sys_nm)) 
unique(nuseds_cuid_streamid[cond,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","Y_LAT","X_LONGT","streamid")])
#                 region                                cu_name_pse cuid GFE_ID       sys_nm      Y_LAT     X_LONGT streamid
# Northern Transboundary                        Stikine-Late Timing 1011   2119 KATETE RIVER 56.6660832 -131.815945    10171
# Northern Transboundary                              Lower Stikine 1017   2571 KATETE RIVER 56.6362700 -131.843732    10555 --> to correct one --> edit streamlocationids + correct streamid in NuSEDS
# Northern Transboundary                              Lower Stikine 1017   2119 KATETE RIVER 56.6660832 -131.815945    10161
# Northern Transboundary Northern Transboundary Fjords (river-type) 1023   2119 KATETE RIVER 56.6660832 -131.815945    10566

streamlocationids[cond_coordinates,]$sys_nm <- "Katete River" |> toupper()
streamlocationids[cond_coordinates,]$GFE_ID <- 2571
streamlocationids[cond_coordinates,]$correction <- "sys_nm & GFE_ID was NA"
streamlocationids[cond_coordinates,]$data_present <- TRUE

#
cond <- !is.na(nuseds_cuid_streamid$cuid) & nuseds_cuid_streamid$cuid == 1017 & 
  !is.na(nuseds_cuid_streamid$GFE_ID) & nuseds_cuid_streamid$GFE_ID == 2571 &
  nuseds_cuid_streamid$sys_nm == "KATETE RIVER"
unique(nuseds_cuid_streamid[cond,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","Y_LAT","X_LONGT","streamid")])
nuseds_cuid_streamid$streamid[cond] <- 10157


# CHECK 2
streamlocationids[cond_sys_nm,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","latitude","longitude","streamid")] 
#                 region         cu_name_pse cuid GFE_ID       sys_nm   latitude   longitude streamid
# Northern Transboundary       Lower Stikine 1017   2119 Katete River 56.6660832 -131.815945    10161
# Northern Transboundary Stikine-Late Timing 1011   2119 Katete River 56.6660832 -131.815945    10171

# check what is GFE_ID = 2119 in nuseds
cond <- !is.na(nuseds_cuid_streamid$GFE_ID) & nuseds_cuid_streamid$GFE_ID == 2119 
unique(nuseds_cuid_streamid[cond,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","Y_LAT","X_LONGT","streamid")])
#' --> streamid is missing
#'                 region                                cu_name_pse cuid GFE_ID       sys_nm      Y_LAT     X_LONGT streamid 
#' Northern Transboundary                        Stikine-Late Timing 1011   2119 KATETE RIVER 56.6660832 -131.815945    10171
#' Northern Transboundary                              Lower Stikine 1017   2119 KATETE RIVER 56.6660832 -131.815945    10161
#' Northern Transboundary Northern Transboundary Fjords (river-type) 1023   2119 KATETE RIVER 56.6660832 -131.815945    10566  --> TODO: add to streamlocationids

cond <- !is.na(nuseds_cuid_streamid$GFE_ID) & nuseds_cuid_streamid$GFE_ID == 2119 & 
  !is.na(nuseds_cuid_streamid$cuid) & nuseds_cuid_streamid$cuid == 1023

new <- streamlocationids[1,]

new$region <- unique(nuseds_cuid_streamid$region[cond])
new$regionid <- streamlocationids$regionid[streamlocationids$region == new$region] |> unique()

# check if streamid is already in 
any(streamlocationids$streamid == unique(nuseds_cuid_streamid$streamid[cond])) # FALSE
new$streamid <- unique(nuseds_cuid_streamid$streamid[cond])

new$species_qualified <- unique(nuseds_cuid_streamid$SPECIES_QUALIFIED[cond])
new$locationid <- NA
new$pointid <- NA
new$sys_nm <- "Katete River" |> toupper()
new$cuid <- unique(nuseds_cuid_streamid$cuid[cond])
new$latitude <- unique(nuseds_cuid_streamid$Y_LAT[cond])
new$longitude <- unique(nuseds_cuid_streamid$X_LONGT[cond])
new$cu_name_pse <- unique(nuseds_cuid_streamid$cu_name_pse[cond])
new$GFE_ID <- unique(nuseds_cuid_streamid$GFE_ID[cond])
new$correction <- "new entry"
new$data_present <- TRUE
streamlocationids <- rbind(streamlocationids,new)


r <- 5 # STEPHENS CREEK 55.7596369 -128.536063   3559  TRUE
nuseds_locations[cond_check,][r,]
GFE_ID <- nuseds_locations$GFE_ID[cond_check][r]
sys_nm <- nuseds_locations$sys_nm[cond_check][r]
X_LONGT <- nuseds_locations$X_LONGT[cond_check][r]
Y_LAT <- nuseds_locations$Y_LAT[cond_check][r]

cond_GFE_ID <- !is.na(streamlocationids$GFE_ID) & streamlocationids$GFE_ID == GFE_ID
cond_sys_nm <- simplify_string_fun(streamlocationids$sys_nm) == simplify_string_fun(sys_nm)
cond_coordinates <- round(streamlocationids$latitude,6) == round(Y_LAT,6) & 
  round(streamlocationids$longitude,6) == round(X_LONGT,6)

any(cond_GFE_ID)
any(cond_sys_nm)
any(cond_coordinates)


streamlocationids[cond_sys_nm,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","latitude","longitude","streamid")]

# check if those are in NuSEDS --> yes they are
cond <- !is.na(nuseds_cuid_streamid$GFE_ID) & nuseds_cuid_streamid$GFE_ID %in% c(585,1460)
unique(nuseds_cuid_streamid[cond,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","Y_LAT","X_LONGT","streamid")])

# is it a new entry then ? Yes if cuid is available --> add new entry
cond <- !is.na(nuseds_cuid_streamid$GFE_ID) & nuseds_cuid_streamid$GFE_ID %in% c(GFE_ID)
unique(nuseds_cuid_streamid[cond,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","Y_LAT","X_LONGT","streamid")])

new <- streamlocationids[1,]

new$region <- unique(nuseds_cuid_streamid$region[cond])
new$regionid <- streamlocationids$regionid[streamlocationids$region == new$region] |> unique()

# check if streamid is already in 
any(streamlocationids$streamid == unique(nuseds_cuid_streamid$streamid[cond])) # FALSE
new$streamid <- unique(nuseds_cuid_streamid$streamid[cond])

new$species_qualified <- unique(nuseds_cuid_streamid$SPECIES_QUALIFIED[cond])
new$locationid <- NA
new$pointid <- NA
new$sys_nm <- "Stephens Creek" |> toupper()
new$cuid <- unique(nuseds_cuid_streamid$cuid[cond])
new$latitude <- unique(nuseds_cuid_streamid$Y_LAT[cond])
new$longitude <- unique(nuseds_cuid_streamid$X_LONGT[cond])
new$cu_name_pse <- unique(nuseds_cuid_streamid$cu_name_pse[cond])
new$GFE_ID <- unique(nuseds_cuid_streamid$GFE_ID[cond])
new$correction <- "new entry"
new$data_present <- TRUE
streamlocationids <- rbind(streamlocationids,new)


r <- 6 # VERRETT SLOUGHS 56.6948632 -130.980493   2570
nuseds_locations[cond_check,][r,]
GFE_ID <- nuseds_locations$GFE_ID[cond_check][r]
sys_nm <- nuseds_locations$sys_nm[cond_check][r]
X_LONGT <- nuseds_locations$X_LONGT[cond_check][r]
Y_LAT <- nuseds_locations$Y_LAT[cond_check][r]

cond_GFE_ID <- !is.na(streamlocationids$GFE_ID) & streamlocationids$GFE_ID == GFE_ID
cond_sys_nm <- simplify_string_fun(streamlocationids$sys_nm) == simplify_string_fun(sys_nm)
cond_coordinates <- round(streamlocationids$latitude,6) == round(Y_LAT,6) & 
  round(streamlocationids$longitude,6) == round(X_LONGT,6)

any(cond_GFE_ID)
any(cond_sys_nm)
any(cond_coordinates)

streamlocationids[cond_coordinates,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","latitude","longitude","streamid")]

cond <- !is.na(nuseds_cuid_streamid$GFE_ID) & nuseds_cuid_streamid$GFE_ID %in% c(GFE_ID)
unique(nuseds_cuid_streamid[cond,c("region","cu_name_pse","cuid","GFE_ID","sys_nm","Y_LAT","X_LONGT","streamid")])

# correction
streamlocationids[cond_coordinates,]$sys_nm <- "Verrett Sloughs" |> toupper() # "s" added
streamlocationids[cond_coordinates,]$GFE_ID <- GFE_ID
streamlocationids[cond_coordinates,]$correction <- "sys_nm & GFE_ID was NA"
streamlocationids[cond_coordinates,]$data_present <- TRUE

# correct streamid in NuSEDS
nuseds_cuid_streamid$streamid[cond] <- streamlocationids[cond_coordinates,]$streamid


#'* check that all cuid in nuseds are in streamlocationid ? *

table(streamlocationids$data_present,useNA = "always")
#   TRUE <NA> 
#   6834   86

# check again that there is no duplicated streamid in streamlocationids
sum(duplicated(streamlocationids$streamid)) # 0

# check that all cuid are present in streamlocationids --> two are not
sum(!unique(nuseds_cuid_streamid$cuid) %in% unique(streamlocationids$cuid)) # 2
cond <- !unique(nuseds_cuid_streamid$cuid) %in% unique(streamlocationids$cuid)
cuid <- unique(nuseds_cuid_streamid$cuid)[cond]
cond <- nuseds_cuid_streamid$cuid %in% cuid
unique(nuseds_cuid_streamid[cond,c("region","SPECIES_QUALIFIED","cu_name_pse","cuid","sys_nm","GFE_ID","streamid")])

# looking at the PSE there is indeed no data for those two CUs

# TODO: add them to streamlocationids
data <- unique(nuseds_cuid_streamid[cond,c("region","SPECIES_QUALIFIED","cu_name_pse",
                                                "cuid","sys_nm","GFE_ID","Y_LAT","X_LONGT",
                                                "streamid")])

for(r in 1:nrow(data)){
  new <- streamlocationids[1,]
  
  new$region <- unique(data$region[r])
  new$regionid <- streamlocationids$regionid[streamlocationids$region == new$region] |> unique()
  
  # check if streamid is already in 
  if(any(streamlocationids$streamid == unique(data$streamid[r]))){
    print("streamid already present in streamlocationids - BREAK")
    break
  }else{
    new$streamid <- unique(data$streamid[r])
    new$species_qualified <- unique(data$SPECIES_QUALIFIED[r])
    new$locationid <- NA
    new$pointid <- NA
    new$sys_nm <- unique(data$sys_nm[r])
    new$cuid <- unique(data$cuid[r])
    new$latitude <- unique(data$Y_LAT[r])
    new$longitude <- unique(data$X_LONGT[r])
    new$cu_name_pse <- unique(data$cu_name_pse[r])
    new$GFE_ID <- unique(data$GFE_ID[r])
    new$correction <- "new entry"
    new$data_present <- TRUE
    streamlocationids <- rbind(streamlocationids,new)
  }
}

sum(!unique(nuseds_cuid_streamid$cuid) %in% unique(streamlocationids$cuid))


#'* Check that all the GFE_ID in nuseds are in streamlocationids *

streamlocationids_copy1 <- streamlocationids
# streamlocationids <- streamlocationids_copy1

GFE_ID <- unique(nuseds_cuid_streamid$GFE_ID)
GFE_ID <- GFE_ID[!is.na(GFE_ID)]
all(GFE_ID %in% streamlocationids$GFE_ID)
cond <- !GFE_ID %in% streamlocationids$GFE_ID
sum(cond) # 36
GFE_ID <- GFE_ID[cond]
length(GFE_ID) # 36
# There are 36 GFE_IDs not in streamlocationids

cond <- !is.na(nuseds_cuid_streamid$GFE_ID) & 
  nuseds_cuid_streamid$GFE_ID %in% GFE_ID

unique(nuseds_cuid_streamid[cond,c("region","SPECIES_QUALIFIED","cu_name_pse","cuid","sys_nm","GFE_ID","streamid")])

data <- unique(nuseds_cuid_streamid[cond,c("region","SPECIES_QUALIFIED","cu_name_pse",
                                           "cuid","sys_nm","GFE_ID","Y_LAT","X_LONGT",
                                           "streamid")])

# check that all the streamid are not in streamlocationids --> no, good
any(data$streamid %in% streamlocationids$streamid) # FALSE

# Add the references to streamlocationids
for(gfeid in GFE_ID){
  # gfeid <- GFE_ID[1]
  
  cond <- !is.na(nuseds_cuid_streamid$GFE_ID) & 
    nuseds_cuid_streamid$GFE_ID == gfeid
  
  data <- unique(nuseds_cuid_streamid[cond,c("region","SPECIES_QUALIFIED","cu_name_pse",
                                                  "cuid","sys_nm","GFE_ID","Y_LAT","X_LONGT",
                                                  "streamid")])
  for(r in 1:nrow(data)){
    
    new <- streamlocationids[1,]
    
    new$region <- unique(data$region[r])
    new$regionid <- streamlocationids$regionid[streamlocationids$region == new$region] |> unique()
    
    new$streamid <- unique(data$streamid[r])
    new$species_qualified <- unique(data$SPECIES_QUALIFIED[r])
    new$locationid <- NA
    new$pointid <- NA
    new$sys_nm <- unique(data$sys_nm[r])
    new$cuid <- unique(data$cuid[r])
    new$latitude <- unique(data$Y_LAT[r])
    new$longitude <- unique(data$X_LONGT[r])
    new$cu_name_pse <- unique(data$cu_name_pse[r])
    new$GFE_ID <- unique(data$GFE_ID[r])
    new$correction <- "new entry"
    new$data_present <- TRUE
    streamlocationids <- rbind(streamlocationids,new)
  }
}

all(GFE_ID %in% streamlocationids$GFE_ID) # TRUE


#'* check that the locations without a GFE_ID are in streamlocationids *
#'

streamlocationids_copy2 <- streamlocationids
nuseds_cuid_streamid_copy2 <- nuseds_cuid_streamid
# streamlocationids <- streamlocationids_copy2
# nuseds_cuid_streamid_noNA <- nuseds_cuid_streamid_noNA_copy

cond <- is.na(nuseds_cuid_streamid$GFE_ID)
nuseds_location_NA <- unique(nuseds_cuid_streamid[cond,c("region","sys_nm","GFE_ID","Y_LAT","X_LONGT","cu_name_pse","cuid","streamid")])
nuseds_location_NA <- unique(nuseds_cuid_streamid[cond,c("region","sys_nm","GFE_ID","Y_LAT","X_LONGT")])
nrow(nuseds_location_NA) # 8

# are any sys_nm in streamlocationids --> NO
any(simplify_string_fun(nuseds_location_NA$sys_nm) %in% simplify_string_fun(streamlocationids$sys_nm))

# are any of these coordinates in streamlocationids --> no
dec <- 6
apply(nuseds_location_NA,1,function(r){
  Y_LAT <- r["Y_LAT"] |> as.numeric()
  X_LONGT <- r["X_LONGT"] |> as.numeric()
  cond_coordinates <- round(streamlocationids$latitude,dec) == round(Y_LAT,dec) &
    round(streamlocationids$longitude,dec) == round(X_LONGT,dec)
  return(any(cond_coordinates))
})

dec <- 4
apply(nuseds_location_NA,1,function(r){
  Y_LAT <- r["Y_LAT"] |> as.numeric()
  X_LONGT <- r["X_LONGT"] |> as.numeric()
  cond_coordinates <- round(streamlocationids$latitude,dec) == round(Y_LAT,dec) &
    round(streamlocationids$longitude,dec) == round(X_LONGT,dec)
  return(any(cond_coordinates))
})


# Add the references to streamlocationids
for(r in 1:nrow(nuseds_location_NA)){
  # r <- 1
  region <- nuseds_location_NA$region[r]
  sys_nm <- nuseds_location_NA$sys_nm[r]
  Y_LAT <- nuseds_location_NA$Y_LAT[r]
  X_LONGT <- nuseds_location_NA$X_LONGT[r]
  
  cond_nuseds <- nuseds_cuid_streamid$sys_nm == sys_nm & 
    nuseds_cuid_streamid$Y_LAT == Y_LAT & 
    nuseds_cuid_streamid$X_LONGT == X_LONGT

  data <- unique(nuseds_cuid_streamid[cond_nuseds,c("region","sys_nm","GFE_ID",
                                                  "Y_LAT","X_LONGT","cu_name_pse",
                                                  "cuid","streamid","SPECIES_QUALIFIED")])
  # There are potentially multiple CUs associated to this locations
  for(rr in 1:nrow(data)){
    new <- streamlocationids[1,]
    
    new$region <- unique(data$region[rr])
    new$regionid <- streamlocationids$regionid[streamlocationids$region == new$region] |> unique()
    
    # check if streamid is already in 
    if(!is.na(data$streamid[rr])){
      print("streamid is not NA which require more work but should not happen - BREAK")
      break
    }else{
      
      streamid <- max(max(streamlocationids$streamid),
                      max(nuseds_cuid_streamid$streamid, na.rm = T)) + 1
      
      new$streamid <- streamid
      
      new$species_qualified <- unique(data$SPECIES_QUALIFIED[rr])
      new$locationid <- NA
      new$pointid <- NA
      new$sys_nm <- unique(data$sys_nm[rr])
      new$cuid <- unique(data$cuid[rr])
      new$latitude <- unique(data$Y_LAT[rr])
      new$longitude <- unique(data$X_LONGT[rr])
      new$cu_name_pse <- unique(data$cu_name_pse[rr])
      new$GFE_ID <- unique(data$GFE_ID[rr])
      new$correction <- "new entry"
      new$data_present <- TRUE
      streamlocationids <- rbind(streamlocationids,new)
      
      # edit streamid in NuSEDS
      cond <- cond_nuseds & nuseds_cuid_streamid$cuid == data$cuid[rr]
      nuseds_cuid_streamid$streamid[cond] <- streamid
    }
  }
}



#'* check that the streamid in nuseds are correct *
#' all cuid in Nuseds are in streamlocationids
#' all GFE_ID in nusedS in streamlocationids
#' all locations in nuseds are in streamlocationids
#' it is possible that certain cuid - GFE_ID combo are not --> add them to streamlocationids
#' it is possible that a streamid in nuseds is wrong --> correct nuseds_cuid_streamid

streamlocationids_copy3 <- streamlocationids
nuseds_cuid_streamid_copy3 <- nuseds_cuid_streamid
# streamlocationids <- streamlocationids_copy3
# nuseds_cuid_streamid_noNA <- nuseds_cuid_streamid_noNA_copy1

nuseds_streamid <- unique(nuseds_cuid_streamid[,c("region","sys_nm","GFE_ID","Y_LAT","X_LONGT","cu_name_pse","cuid","streamid")])
nrow(nuseds_streamid) # 7023

threshold <- 5
for(r in 1:nrow(nuseds_streamid)){
  # r <- 1
  cuid <- nuseds_streamid$cuid[r]
  GFE_ID <- nuseds_streamid$GFE_ID[r]
  sys_nm <- nuseds_streamid$sys_nm[r]
  Y_LAT <- nuseds_streamid$Y_LAT[r]
  X_LONGT <- nuseds_streamid$X_LONGT[r]
  region <- nuseds_streamid$region[r]
  streamid <- nuseds_streamid$streamid[r]
  
  cond_cuid <- streamlocationids$cuid == cuid
  
  if(is.na(GFE_ID)){
    cond_location <- simplify_string_fun(streamlocationids$sys_nm) == simplify_string_fun(sys_nm) &
      round(streamlocationids$latitude,6) == round(Y_LAT,6) &
      round(streamlocationids$longitude,6) == round(X_LONGT,6)
      
  }else{
    cond_location <- !is.na(streamlocationids$GFE_ID) & streamlocationids$GFE_ID == GFE_ID
  }
  
  if(!any(cond_location)){
    print("No location found - BREAK")
    break
  }
  
  if(!any(cond_cuid)){
    print("No cuid found - BREAK")
    break
  }
  
  if(is.na(GFE_ID)){
    cond_nuseds <- nuseds_cuid_streamid$cuid == cuid & 
      nuseds_cuid_streamid$sys_nm == sys_nm &
      nuseds_cuid_streamid$Y_LAT == Y_LAT &
      nuseds_cuid_streamid$X_LONGT == X_LONGT
    
  }else{
    cond_nuseds <- nuseds_cuid_streamid$cuid == cuid & 
      !is.na(nuseds_cuid_streamid$GFE_ID) &
      nuseds_cuid_streamid$GFE_ID == GFE_ID
  }
  
  if(any(cond_cuid & cond_location)){ # check the the streamid is the same
    
    streamlocationids$data_present[cond_cuid & cond_location] <- TRUE
    
    streamid_pse <- streamlocationids$streamid[cond_cuid & cond_location]
    if(is.na(streamid)){
      nuseds_cuid_streamid_noNA$streamid[cond_nuseds] <- streamid_pse
      
    }else{
      
      if(unique(nuseds_cuid_streamid$streamid[cond_nuseds]) != streamid_pse){
        print("sreamid differ between NuSEDS and PSE ; NuSEDS is edited")
        print(nuseds_cuid_streamid[cond_nuseds,c("sys_nm","GFE_ID","Y_LAT","X_LONGT","cuid","streamid","cu_name_pse")] |> unique())
        print(streamlocationids[cond_cuid & cond_location,c("sys_nm","GFE_ID","latitude","longitude","cuid","streamid","cu_name_pse")])
        
        nuseds_cuid_streamid$streamid[cond_nuseds] <- streamid_pse
      }
    }
  }else{ # create a new entry
    
    if(is.na(streamid)){
      streamid <- max(max(streamlocationids$streamid),
                      max(nuseds_cuid_streamid$streamid, na.rm = T)) + 1
      
      nuseds_cuid_streamid$streamid[cond_nuseds] <- streamid
    }
    
    new <- streamlocationids[1,]
    new$region <- unique(nuseds_cuid_streamid$region[cond_nuseds])
    new$regionid <- streamlocationids$regionid[streamlocationids$region == new$region] |> unique()
    
    new$streamid <- streamid
    new$species_qualified <- unique(nuseds_cuid_streamid$SPECIES_QUALIFIED[cond_nuseds])
    new$locationid <- NA
    new$pointid <- NA
    new$sys_nm <- unique(nuseds_cuid_streamid$sys_nm[cond_nuseds])
    new$cuid <- unique(nuseds_cuid_streamid$cuid[cond_nuseds])
    new$latitude <- unique(nuseds_cuid_streamid$Y_LAT[cond_nuseds])
    new$longitude <- unique(nuseds_cuid_streamid$X_LONGT[cond_nuseds])
    new$cu_name_pse <- unique(nuseds_cuid_streamid$cu_name_pse[cond_nuseds])
    new$GFE_ID <- unique(nuseds_cuid_streamid$GFE_ID[cond_nuseds])
    new$correction <- "new entry"
    new$data_present <- TRUE
    streamlocationids <- rbind(streamlocationids,new)
    
    # print("New entry:")
    # print(new)
  }
  ratio <- r /nrow(nuseds_streamid) * 100
  if(threshold < ratio){
    print(paste0(threshold,"%"))
    threshold <- threshold + 5
  }
}

# CHECKS 
sum(is.na(nuseds_cuid_streamid$streamid)) # 0
all(nuseds_cuid_streamid$streamid |> unique() %in% streamlocationids$streamid) # TRUE


#'* check that the GFE_ID in streamlocationids have unique sys_nm and coordinates *
#'

# 1) Check locations with a GFE_ID
check <- unique(streamlocationids[,c("GFE_ID","sys_nm","latitude","longitude")])
check <- check[!is.na(check$GFE_ID),]
nrow(check) # 2430

GFE_ID <- check$GFE_ID[duplicated(check$GFE_ID)]
length(GFE_ID) # 61
cond <- check$GFE_ID %in% GFE_ID
check <- check[cond,]
check <- check[order(check$GFE_ID),]
check

#' Correct those coordinates using nuseds as a reference.
for(r in 1:nrow(check)){
  
  cond_pse <- !is.na(streamlocationids$GFE_ID) & streamlocationids$GFE_ID ==  check$GFE_ID[r]
  latitude <- streamlocationids$latitude[cond_pse]
  longitude <- streamlocationids$longitude[cond_pse]
  sys_nm <- streamlocationids$sys_nm[cond_pse] |> unique()
  
  cond_nuseds <- !is.na(nuseds_cuid_streamid$GFE_ID) & nuseds_cuid_streamid$GFE_ID ==  check$GFE_ID[r]
  Y_LAT <- unique(nuseds_cuid_streamid$Y_LAT[cond_nuseds])
  X_LONGT <- unique(nuseds_cuid_streamid$X_LONGT[cond_nuseds])
  sys_nm_nuseds <- unique(nuseds_cuid_streamid$sys_nm[cond_nuseds])
  
  if(length(Y_LAT) > 1 | length(X_LONGT) > 1){
    print("More than one coordinate value for this GFE_ID in NuSEDS - BREAK")
    break
    
  }else{
    
    if(!any(latitude == Y_LAT) | !any(longitude == X_LONGT)){
      print("None of the coordinates in PSE match the ones with NuSEDS")
    }
    
    streamlocationids$latitude[cond_pse] <- Y_LAT
    streamlocationids$longitude[cond_pse] <- X_LONGT
    
  }
  
  if(length(sys_nm) > 1){
    print("More than one sys_nm for the GFE_ID in PSE - BREAK")
    break
    
  }else if(sys_nm != sys_nm_nuseds){
    print("sys_nm differ between PSE and NuSEDS- BREAK")
    break
  }
  
  if(length(sys_nm) > 1){
    print("More than one sys_nm for the GFE_ID in PSE - BREAK")
    break
    
  }
}


check <- unique(streamlocationids[,c("GFE_ID","sys_nm","latitude","longitude")])
check <- check[!is.na(check$GFE_ID),]
nrow(check) # 2369

GFE_ID <- check$GFE_ID[duplicated(check$GFE_ID)]
length(GFE_ID) # 0 GOOD


# 2) Check locations without a GFE_ID
check <- unique(streamlocationids[,c("sys_nm","latitude","longitude","GFE_ID")])
check <- check[is.na(check$GFE_ID),]
nrow(check) # 42

sys_nm <- check$sys_nm[duplicated(check$sys_nm)]
length(sys_nm) # 1
sys_nm
# "CANYON ISLAND"


cond <- streamlocationids$sys_nm %in% sys_nm
check <- unique(streamlocationids[cond,c("region","streamid","sys_nm","cuid","latitude","longitude","GFE_ID","data_present")])
check <- check[order(check$sys_nm),]
check$species_name <- sapply(check$cuid,function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(unique(conservationunits_decoder$species_name[cond]))
})
check

#                  region streamid        sys_nm cuid  latitude   longitude GFE_ID data_present species_name
#  Northern Transboundary    10305 CANYON ISLAND 1001 58.552425 -133.680472     NA           NA         Chum
#  Northern Transboundary    10306 CANYON ISLAND 1019 58.552425 -133.680472     NA           NA         Pink
#  Northern Transboundary    10307 CANYON ISLAND 1020 58.552425 -133.680472     NA           NA         Pink
#  Northern Transboundary    10309 CANYON ISLAND 1081 58.510451 -133.806583     NA           NA    Steelhead --> different source

# The location is not in nuseds_cuid_streamid:
cond <- nuseds_cuid_streamid$sys_nm == "CANYON ISLAND"
cond <- nuseds_cuid_streamid$CENSUS_SITE == "CANYON ISLAND"
cond <- grepl("CANYON",nuseds_cuid_streamid$sys_nm)
unique(nuseds_cuid_streamid[cond,c("region","sys_nm","GFE_ID","Y_LAT","X_LONGT")])

# the location is not in DFO_All_Streams_Segments
cond <- DFO_All_Streams_Segments$NME == "CANYON ISLAND"
unique(DFO_All_Streams_Segments[cond,])


cond <- data_TBR$stream_name_pse == "CANYON ISLAND"
unique(data_TBR[cond,c("region","species_name","stream_name_pse","latitude","longitude")])
#                 region species_name stream_name_pse  latitude   longitude
# Northern Transboundary         Chum   CANYON ISLAND 58.552425 -133.680472
# Northern Transboundary         Pink   CANYON ISLAND 58.552425 -133.680472
# Northern Transboundary    Steelhead   CANYON ISLAND 58.510451 -133.806583

#' SELECTION OF COORDINATES:
#' The Fish wheel at Canyon Island is just behind the US boarder, according to 
#' Figure 1 in report "N19-I30A Taku River Sockeye Stock Assessment and Escapement
#'Goal Review Part II - 2019 Report".
#' --> The coordinates for Chum, Pink seems right.
#' --> Correct for Steelthead

cond_SH <- streamlocationids$species_qualified == "SH"
cond_ci <- streamlocationids$sys_nm == "CANYON ISLAND"
streamlocationids$latitude[cond_ci & cond_SH] <- streamlocationids$latitude[cond_ci & !cond_SH] |> unique()
streamlocationids$longitude[cond_ci & cond_SH] <- streamlocationids$longitude[cond_ci & !cond_SH] |> unique()

cond_SH <- data_TBR$species_name == "Steelhead"
cond_ci <- data_TBR$stream_name_pse == "CANYON ISLAND"
data_TBR$latitude[cond_ci & cond_SH] <- data_TBR$latitude[cond_ci & !cond_SH] |> unique()
data_TBR$longitude[cond_ci & cond_SH] <- data_TBR$longitude[cond_ci & !cond_SH] |> unique()

cond_ci_SH <- data_SH$stream_name_pse == "CANYON ISLAND"
unique(data_SH[cond_ci_SH,c("region","species_name","stream_name_pse","latitude","longitude")])
data_TBR$latitude[cond_ci_SH] <- data_TBR$latitude[cond_ci & !cond_SH] |> unique()
data_TBR$longitude[cond_ci_SH] <- data_TBR$longitude[cond_ci & !cond_SH] |> unique()


# Check
check <- unique(streamlocationids[,c("sys_nm","latitude","longitude","GFE_ID")])
check <- check[is.na(check$GFE_ID),]
nrow(check) # 41

sys_nm <- check$sys_nm[duplicated(check$sys_nm)]
length(sys_nm) # 0
sys_nm

#
# Uniformise locations and streamid with other datasets ------
#

rm(streamlocationids_copy)
rm(streamlocationids_copy1)
rm(streamlocationids_copy2)
rm(streamlocationids_copy3)

rm(nuseds_cuid_streamid_copy)
rm(nuseds_cuid_streamid_copy1)
rm(nuseds_cuid_streamid_copy2)
rm(nuseds_cuid_streamid_copy3)

rm(all_areas_nuseds)
# rm(conservation_unit_census_sites)
rm(series_removed)

streamlocationids_copy <- streamlocationids
# streamlocationids <- streamlocationids_copy


#'* TBR *

# Are all the streamid in streamlocationids?
data_TBR$streamid |> unique()
data_TBR$stream_name_pse |> unique()
all(unique(data_TBR$streamid) %in% streamlocationids$streamid) # TRUE !!!

# check that locations, coordinates and sys_nam and cuid match

data_TBR$GFE_ID <- NA
data_here <- unique(data_TBR[,c("cuid","streamid","stream_name_pse","latitude","longitude")])
for(r in 1:nrow(data_here)){
  
  cond_pse <- streamlocationids$streamid == data_here$streamid[r]
  
  cond_rg <- data_TBR$streamid == data_here$streamid[r]
  
  if(streamlocationids$cuid[cond_pse] != data_here$cuid[r]){
    print("Wrong cuid - BREAK")
    break
  }else{
    streamlocationids$data_present[cond_pse] <- T
  }
  
  cond <- simplify_string_fun(streamlocationids$sys_nm[cond_pse]) != simplify_string_fun(data_here$stream_name_pse[r])
  if(cond){
    print("Wrong sys_nm corrected from:")
    print(unique(data_TBR$stream_name_pse[cond_rg]))
    
    data_TBR$stream_name_pse[cond_rg] <- streamlocationids$sys_nm[cond_pse]
    
    print("To:")
    print(unique(data_TBR$stream_name_pse[cond_rg]))
    print("***")
  }
  
  cond <- round(streamlocationids$latitude[cond_pse],6) != round(data_here$latitude[r],6) |
    round(streamlocationids$longitude[cond_pse],6) != round(data_here$longitude[r],6)
  if(cond){
    print(paste("Wrong coordinates for",data_here$stream_name_pse[r], "corrected from:"))
    print(paste(unique(data_TBR$latitude[cond_rg]),unique(data_TBR$longitude[cond_rg])))
    
    data_TBR$latitude[cond_rg] <- streamlocationids$latitude[cond_pse]
    data_TBR$longitude[cond_rg] <- streamlocationids$longitude[cond_pse]
    
    print("To:")
    print(paste(unique(data_TBR$latitude[cond_rg]),unique(data_TBR$longitude[cond_rg])))
    print("***")
  }
  
  data_TBR$GFE_ID[cond_rg] <- unique(streamlocationids$GFE_ID[cond_pse])

}

colnames(data_TBR)


# provide POP_ID if available
data_TBR$POP_ID <- NA
for(sid in unique(data_TBR$streamid)){
  # sid <- unique(data_TBR$streamid)[1]
  cond_rg <- data_TBR$streamid == sid
  cond_pse <- nuseds_cuid_streamid$streamid == sid
  if(any(cond_pse)){
    data_TBR$POP_ID[cond_rg] <- unique(nuseds_cuid_streamid$POP_ID[cond_pse])
  }
}

unique(data_TBR$POP_ID)

#'* Yukon *

# Are all the streamid in streamlocationids?
data_yukon$streamid |> unique()
data_yukon$GFE_ID |> unique()
data_yukon$stream_name_pse |> unique()
all(unique(data_yukon$streamid) %in% streamlocationids$streamid) # TRUE !!!
all(unique(data_yukon$GFE_ID) %in% streamlocationids$GFE_ID) # TRUE !!!

# check that locations, coordinates and sys_nam match and cuid
data_here <- unique(data_yukon[,c("cuid","streamid","stream_name_pse","latitude","longitude","GFE_ID")])
for(r in 1:nrow(data_here)){
  
  cond_pse <- streamlocationids$streamid == data_here$streamid[r]
  
  cond_rg <- data_yukon$streamid == data_here$streamid[r]
  
  if(streamlocationids$cuid[cond_pse] != data_here$cuid[r]){
    print("Wrong cuid - BREAK")
    break
  }else{
    streamlocationids$data_present[cond_pse] <- T
  }
  
  cond <- simplify_string_fun(streamlocationids$sys_nm[cond_pse]) != simplify_string_fun(data_here$stream_name_pse[r])
  if(cond){
    print("Wrong sys_nm corrected from:")
    print(unique(data_yukon$stream_name_pse[cond_rg]))
    
    data_yukon$stream_name_pse[cond_rg] <- streamlocationids$sys_nm[cond_pse]
    
    print("To:")
    print(unique(data_yukon$stream_name_pse[cond_rg]))
    print("***")
  }
  
  cond <- round(streamlocationids$latitude[cond_pse],6) != round(data_here$latitude[r],6) |
    round(streamlocationids$longitude[cond_pse],6) != round(data_here$longitude[r],6)
  if(cond){
    print(paste("Wrong coordinates for",data_here$stream_name_pse[r], "corrected from:"))
    print(paste(unique(data_yukon$latitude[cond_rg]),unique(data_yukon$longitude[cond_rg])))
    
    data_yukon$latitude[cond_rg] <- streamlocationids$latitude[cond_pse]
    data_yukon$longitude[cond_rg] <- streamlocationids$longitude[cond_pse]
    
    print("To:")
    print(paste(unique(data_yukon$latitude[cond_rg]),unique(data_yukon$longitude[cond_rg])))
    print("***")
  }
  
  # check that GFE_ID match
  cond <- (is.na(unique(data_yukon$GFE_ID[cond_rg])) & is.na(streamlocationids$GFE_ID[cond_pse])) |
    (unique(data_yukon$GFE_ID[cond_rg]) == streamlocationids$GFE_ID[cond_pse])
  if(!cond){
    print("Wrong GFE_ID - BREAK")
    break
  }
}

# provide POP_ID if available
unique(data_yukon$streamid) %in% unique(nuseds_cuid_streamid$streamid)
data_yukon$POP_ID <- NA
for(sid in unique(data_yukon$streamid)){
  # sid <- unique(data_yukon$streamid)[1]
  cond_rg <- data_yukon$streamid == sid
  cond_pse <- nuseds_cuid_streamid$streamid == sid
  if(any(cond_pse)){
    data_yukon$POP_ID[cond_rg] <- unique(nuseds_cuid_streamid$POP_ID[cond_pse])
  }
}

cond <- is.na(data_yukon$POP_ID) & data_yukon$source_id == "NuSEDS_20251103"
unique(data_yukon[cond,c("stream_name_pse","GFE_ID","species_qualified","cu_name_pse","POP_ID","source_id")])
#     stream_name_pse GFE_ID species_qualified cu_name_pse POP_ID       source_id
#    BIG SALMON RIVER   2516                CK  Big Salmon     NA NuSEDS_20251103
#        TESLIN RIVER   2499                CK      Teslin     NA NuSEDS_20251103

cond <- nuseds_cuid_streamid$sys_nm %in% c("BIG SALMON RIVER","TESLIN RIVER") & 
  nuseds_cuid_streamid$SPECIES == "Chinook"
nuseds_cuid_streamid[cond,c("region","SPECIES","cu_name_pse","POP_ID","sys_nm")] |> unique()
# region SPECIES  cu_name_pse POP_ID           sys_nm
#  Yukon Chinook Middle Yukon  44597 BIG SALMON RIVER
#  Yukon Chinook  Upper Yukon  44585     TESLIN RIVER
# 

#' NOTE: these were corrected by changing cu_name_pse to "Big Salmon" and "Teslin", 
#' respectively in yukon-data/code/spawner-survey.R. So here we preserve the POP_ID.

cond_yk <- data_yukon$source_id == "NuSEDS_20251103" & 
  data_yukon$stream_name_pse == "BIG SALMON RIVER" & 
  data_yukon$cu_name_pse == "Big Salmon" &
  data_yukon$species_qualified == "CK"

cond_nuseds <- nuseds_cuid_streamid$sys_nm %in% c("BIG SALMON RIVER") & 
  nuseds_cuid_streamid$SPECIES == "Chinook"

data_yukon[cond_yk,]$POP_ID <- unique(nuseds_cuid_streamid$POP_ID[cond_nuseds])


cond_yk <- data_yukon$source_id == "NuSEDS_20251103" & 
  data_yukon$stream_name_pse == "TESLIN RIVER" & 
  data_yukon$cu_name_pse == "Teslin" & 
  data_yukon$species_qualified == "CK"

cond_nuseds <- nuseds_cuid_streamid$sys_nm %in% c("TESLIN RIVER") & 
  nuseds_cuid_streamid$SPECIES == "Chinook"

data_yukon[cond_yk,]$POP_ID <- unique(nuseds_cuid_streamid$POP_ID[cond_nuseds])


#'* SH *

# Are all the streamid in streamlocationids?
data_SH$streamid |> unique()
data_SH$GFE_ID |> unique()
data_SH$stream_name_pse |> unique()
all(unique(data_SH$streamid) %in% streamlocationids$streamid) # TRUE !!!
all(unique(data_SH$GFE_I) %in% unique(streamlocationids$GFE_ID)) # TRUE !!!

data_SH$GFE_ID <- NA
data_here <- unique(data_SH[,c("cuid","streamid","stream_name_pse","latitude","longitude")])
for(r in 1:nrow(data_here)){
  # r <- which(data_here$stream_name_pse == "KSI GINGOLX")

  cond_pse <- streamlocationids$streamid == data_here$streamid[r]
  
  cond_rg <- data_SH$streamid == data_here$streamid[r]
  
  if(streamlocationids$cuid[cond_pse] != data_here$cuid[r]){
    print("Wrong cuid - BREAK")
    break
  }else{
    streamlocationids$data_present[cond_pse] <- T
  }
  
  cond <- simplify_string_fun(streamlocationids$sys_nm[cond_pse]) != simplify_string_fun(data_here$stream_name_pse[r])
  if(cond){
    print("Wrong sys_nm corrected from:")
    print(unique(data_SH$stream_name_pse[cond_rg]))
    
    data_SH$stream_name_pse[cond_rg] <- streamlocationids$sys_nm[cond_pse]
    
    print("To:")
    print(unique(data_SH$stream_name_pse[cond_rg]))
    print("***")
  }
  
  cond <- round(streamlocationids$latitude[cond_pse],6) != round(data_here$latitude[r],6) |
    round(streamlocationids$longitude[cond_pse],6) != round(data_here$longitude[r],6)
  if(cond){
    print(paste("Wrong coordinates for",data_here$stream_name_pse[r], "corrected from:"))
    print(paste(unique(data_SH$latitude[cond_rg]),unique(data_SH$longitude[cond_rg])))
    
    data_SH$latitude[cond_rg] <- streamlocationids$latitude[cond_pse]
    data_SH$longitude[cond_rg] <- streamlocationids$longitude[cond_pse]
    
    print("To:")
    print(paste(unique(data_SH$latitude[cond_rg]),unique(data_SH$longitude[cond_rg])))
    print("***")
  }
  
  data_SH$GFE_ID[cond_rg] <- unique(streamlocationids$GFE_ID[cond_pse])
}

# provide POP_ID if available --> there is no SH in nuseds_cuid_streamid
data_SH$POP_ID <- NA
# for(sid in unique(data_SH$streamid)){
#   # sid <- unique(data_SH$streamid)[1]
#   cond_rg <- data_SH$streamid == sid
#   cond_pse <- nuseds_cuid_streamid$streamid == sid
#   if(any(cond_pse)){
#     data_SH$POP_ID[cond_rg] <- unique(nuseds_cuid_streamid$POP_ID[cond_pse])
#   }
# }

unique(data_SH$POP_ID)


#'* Central- Coast *

# Are all the streamid in streamlocationids?
data_cc$streamid |> unique()
data_cc$GFE_ID |> unique()
data_cc$stream_name_pse |> unique()
all(unique(data_cc$streamid) %in% streamlocationids$streamid) # TRUE !!!
all(unique(data_cc$GFE_I) %in% unique(streamlocationids$GFE_ID)) # TRUE !!!

cond <- streamlocationids$streamid == unique(data_cc$streamid)

unique(data_cc$latitude) == streamlocationids$latitude[cond]   # TRUE 
unique(data_cc$longitude) == streamlocationids$longitude[cond] # TRUE 
unique(data_cc$GFE_ID) == streamlocationids$GFE_ID[cond]       # TRUE 


#
# check streamlocationids --------
#

# 
table(streamlocationids$data_present,exclude = "always")
#   TRUE <NA> 
#   7077    8 

cond_NA <- is.na(streamlocationids$data_present)
streamlocationids[cond_NA,]

r <- 1 # FENNELL CREEK AND SASKUM CREEK  709 
streamlocationids[cond_NA,][r,]
cuid <- streamlocationids[cond_NA,]$cuid[r]
GFE_ID <- streamlocationids[cond_NA,]$GFE_ID[r]
species_qualified <- streamlocationids[cond_NA,]$species_qualified[r]
cond <- nuseds_cuid_streamid$SPECIES_QUALIFIED == species_qualified & 
  !is.na(nuseds_cuid_streamid$GFE_ID) &
  nuseds_cuid_streamid$GFE_ID == GFE_ID
nuseds_cuid_streamid[cond,c("region","cuid","GFE_ID")] |> unique() # none
cond <- nuseds_cuid_streamid$cuid == cuid & 
  !is.na(nuseds_cuid_streamid$GFE_ID) &
  nuseds_cuid_streamid$GFE_ID == GFE_ID
nuseds_cuid_streamid[cond,c("region","cuid","GFE_ID")] |> unique() # none
cond <- !is.na(nuseds_cuid_streamid$GFE_ID) &
  nuseds_cuid_streamid$GFE_ID == GFE_ID
nuseds_cuid_streamid[cond,c("region","cuid","GFE_ID")] |> unique() # none
streamlocationids[cond_NA,][r,]$data_present <- F

r <- 2 # TATSATUA RIVER 1029
streamlocationids[cond_NA,][r,]
cuid <- streamlocationids[cond_NA,]$cuid[r]
cond <- nuseds_cuid_streamid$cuid == cuid
nuseds_cuid_streamid[cond,c("region","cuid","GFE_ID","sys_nm","streamid","cu_name_pse","SPECIES_QUALIFIED")] |> unique() # none
#' --> TATSATUA RIVER --> TATSATUA CREEK
cond <- streamlocationids$streamid %in% c(10146,10570)
streamlocationids[cond,]

# Correct streamid in NuSEDS
cond <- nuseds_cuid_streamid$streamid == 10570
nuseds_cuid_streamid$streamid[cond] <- 10146

# Correct streamlocationids
cond_10146 <- streamlocationids$streamid == 10146
cond_10570 <- streamlocationids$streamid == 10570
streamlocationids$sys_nm[cond_10146] <- streamlocationids$sys_nm[cond_10570]
streamlocationids$latitude[cond_10146] <- streamlocationids$latitude[cond_10570]
streamlocationids$longitude[cond_10146] <- streamlocationids$longitude[cond_10570]
streamlocationids$GFE_ID[cond_10146] <- streamlocationids$GFE_ID[cond_10570]
streamlocationids$correction[cond_10146] <- "Coordinates & sys_nm & & GFE_ID was NA"
streamlocationids$data_present[cond_10146] <- T
streamlocationids <- streamlocationids[!cond_10570,]


r <- 3 # TATSATUA RIVER 1035
streamlocationids[cond_NA,][r,]
cuid <- streamlocationids[cond_NA,]$cuid[r]
cond <- nuseds_cuid_streamid$cuid == cuid
nuseds_cuid_streamid[cond,c("region","cuid","GFE_ID","sys_nm","streamid","cu_name_pse","SPECIES_QUALIFIED")] |> unique() # none
cond <- data_TBR$cuid == cuid
data_TBR[cond,] |> unique()

cond_CO <- data_TBR$species_name == "Coho"
unique(data_TBR$cu_name_pse[cond_CO])

cond_CO <- nuseds_cuid_streamid$SPECIES == "Coho" & nuseds_cuid_streamid$region == "Northern Transboundary"
unique(nuseds_cuid_streamid$cu_name_pse[cond_CO])

cond <- conservationunits_decoder$cuid == 1035
conservationunits_decoder[cond,]
#  CO-42 --> not in the last raw nuseds NuSEDS

streamlocationids[cond_NA,][r,]$data_present <- F


r <- 4 # same cuid = 1035
streamlocationids[cond_NA,][r,]
cuid <- streamlocationids[cond_NA,]$cuid[r]
cond <- nuseds_cuid_streamid$cuid == cuid
nuseds_cuid_streamid[cond,c("region","cuid","GFE_ID","sys_nm","streamid","cu_name_pse","SPECIES_QUALIFIED")] |> unique() # none
cond <- data_TBR$cuid == cuid
data_TBR[cond,] |> unique()
cond <- conservationunits_decoder$cuid == cuid
conservationunits_decoder[cond,]
#  CO-42 --> not in the last raw nuseds 

streamlocationids[cond_NA,][r,]$data_present <- F

r <- 5 # cuid 1034
streamlocationids[cond_NA,][r,]
cuid <- streamlocationids[cond_NA,]$cuid[r]
cond <- nuseds_cuid_streamid$cuid == cuid
nuseds_cuid_streamid[cond,c("region","cuid","GFE_ID","sys_nm","streamid","cu_name_pse","SPECIES_QUALIFIED")] |> unique() # none
cond <- data_TBR$cuid == cuid
data_TBR[cond,] |> unique()
cond <- conservationunits_decoder$cuid == cuid
conservationunits_decoder[cond,]
#  CO-43 --> not in the last raw nuseds

streamlocationids[cond_NA,][r,]$data_present <- F


r <- 6 # cuid 1034
streamlocationids[cond_NA,][r,]
cuid <- streamlocationids[cond_NA,]$cuid[r]
cond <- nuseds_cuid_streamid$cuid == cuid
nuseds_cuid_streamid[cond,c("region","cuid","GFE_ID","sys_nm","streamid","cu_name_pse","SPECIES_QUALIFIED")] |> unique() # none
cond <- conservationunits_decoder$cuid == cuid
conservationunits_decoder[cond,]
#  CO-43 --> not in the last raw nuseds

streamlocationids[cond_NA,][r,]$data_present <- F

r <- 7 # cuid 1035
streamlocationids[cond_NA,][r,]
cuid <- streamlocationids[cond_NA,]$cuid[r]
cond <- nuseds_cuid_streamid$cuid == cuid
nuseds_cuid_streamid[cond,c("region","cuid","GFE_ID","sys_nm","streamid","cu_name_pse","SPECIES_QUALIFIED")] |> unique() # none
cond <- conservationunits_decoder$cuid == cuid
conservationunits_decoder[cond,]
#  CO-42 --> not in the last raw nuseds 

streamlocationids[cond_NA,][r,]$data_present <- F

r <- 8 # cuid 302
streamlocationids[cond_NA,][r,]
cuid <- streamlocationids[cond_NA,]$cuid[r]
cond <- nuseds_cuid_streamid$cuid == cuid & 
  nuseds_cuid_streamid$sys_nm == "CAMPBELL RIVER"
nuseds_cuid_streamid[cond,c("region","cuid","GFE_ID","sys_nm","streamid","cu_name_pse","SPECIES_QUALIFIED")] |> unique() # none

cond <- nuseds_cuid_streamid$GFE_ID %in% c(64739,677)
unique(unique(nuseds_cuid_streamid[cond,c("region","sys_nm","GFE_ID","Y_LAT","X_LONGT","SPECIES_QUALIFIED","cu_name_pse","streamid")]))
# not sure what 64739 is 

any(nuseds_cuid_streamid$GFE_ID %in% c(64739)) # FLASE
any(DFO_All_Streams_Segments$ID %in% c(64739)) # 
cond <- DFO_All_Streams_Segments$ID %in% c(64739,677)
DFO_All_Streams_Segments[cond,]

# GFE_ID 64739 is not in the updated NuSEDS
streamlocationids[cond_NA,][r,]$data_present <- F

table(streamlocationids$data_present,exclude = "always")
# FALSE  TRUE 
#     7  7077

#
# Edit NuSEDS data colnames --> dataset2 ------
#

dataset2 <- nuseds_cuid_streamid

# add pointid and leave as NA
# https://pacificsalmonfdn.slack.com/archives/CJ5RVHVCG/p1724089498307439

dataset2$pointid <- NA

# edit column names
field_toChange <- c("SPECIES","SPECIES_QUALIFIED","IS_INDICATOR","Year",
                    "MAX_ESTIMATE","ESTIMATE_METHOD",
                    "sys_nm","X_LONGT","Y_LAT")

fields_new <- c("species_name","species_qualified","indicator","year",
                "stream_observed_count","stream_survey_method",
                "stream_name_pse","longitude","latitude")

for(i in 1:length(field_toChange)){
  colnames(dataset2)[colnames(dataset2) == field_toChange[i]] <- fields_new[i]
}

# remove NAs in stream_observed_count
# dataset2 <- dataset2[!is.na(dataset2$stream_observed_count),]


# select columns
colToKeep <- c("region","species_name","species_qualified","cuid","cu_name_pse",
               "pointid","streamid","POP_ID","GFE_ID",
               "stream_name_pse","indicator","latitude","longitude","year",
               "stream_observed_count","stream_survey_method","stream_survey_quality",
               "source_id")

# colToKeep[! colToKeep %in% colnames(dataset2)]

dataset2 <- dataset2[,colToKeep]


nrow(dataset2)
# 313697 313763

#
# Add the other datasets ------
#

#'* Central- Coast *

data_cc$pointid <- NA

data_cc$POP_ID <- NA

# combine the datasets
dataset2 <- rbind(dataset2,data_cc[,colnames(dataset2)])


#'* Transboundary *

data_TBR$species_qualified <- sapply(data_TBR$cuid,function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(unique(conservationunits_decoder$species_qualified[cond]))
})


# Remove steelhead data because the latter is in data_SH
unique(data_TBR$species_name)

cond_SH <- data_TBR$species_name == "Steelhead"
sum(cond_SH) # 29
unique(data_TBR[cond_SH,c("cuid","stream_name_pse")])

cond_rg <- data_SH$region == "Northern Transboundary"
sum(cond_rg) # 29
unique(data_SH[cond_rg,c("cuid","stream_name_pse")])

data_TBR <- data_TBR[!cond_SH,]

# All the source_id should not be NuSEDS
data_TBR$source_id |> unique()

# check if some cuid-stream_name_pse are in common
# The NuSEDS time series (in blue in the figures) should be deleted
any(unique(data_TBR$streamid) %in% unique(nuseds_cuid_streamid$streamid))
cond <- unique(data_TBR$streamid) %in% unique(nuseds_cuid_streamid$streamid)
streamid <- unique(data_TBR$streamid)[cond]
for(sid in streamid){
  cond_rg <- data_TBR$streamid == sid
  cond_nuseds <- nuseds_cuid_streamid$streamid == sid
  
  yr_min <- min(min(data_TBR$year[cond_rg]),min(nuseds_cuid_streamid$Year[cond_nuseds]))
  yr_max <- max(max(data_TBR$year[cond_rg]),max(nuseds_cuid_streamid$Year[cond_nuseds]))
  
  count_max <- max(max(data_TBR$stream_observed_count[cond_rg], na.rm = T),
                   max(nuseds_cuid_streamid$MAX_ESTIMATE[cond_nuseds], na.rm = T))
  
  d_rg <- data_TBR[cond_rg,]
  d_rg <- d_rg[order(d_rg$year),]
  
  d_nuseds <- nuseds_cuid_streamid[cond_nuseds,]
  d_nuseds <- d_nuseds[order(d_nuseds$Year),]
  
  main <- paste(unique(d_rg$species_qualified),unique(d_rg$cu_name_pse),unique(d_rg$cuid),unique(d_rg$stream_name_pse), sep = " - ")
  
  plot(x = d_rg$year, y = d_rg$stream_observed_count, type = "l", ylim = c(0,count_max), 
       lwd = 2, xlim = c(yr_min,yr_max), main = main)
  points(x = d_rg$year, y = d_rg$stream_observed_count, cex = 1.5, pch = 16)
  
  lines(x = d_nuseds$Year, y = d_nuseds$MAX_ESTIMATE, lwd = 2, col = "blue")
  points(x = d_nuseds$Year, y = d_nuseds$MAX_ESTIMATE, cex = 2, pch = 1, lwd = 2, col = "blue")
  
  legend("topright",c("TBR data","NuSEDS"), col = c("black","blue"), lwd = 2, pch = c(16,1), bty = "n")
  
  # delete the data in dataset2
  cond <- dataset2$streamid == sid
  dataset2 <- dataset2[!cond,]
}


data_TBR$pointid <- NA

# combine the datasets
dataset2 <- rbind(dataset2,data_TBR[,colnames(dataset2)])

nrow(dataset2)
# 314122 314113

colnames(dataset2)[!colnames(dataset2) %in% colnames(data_TBR)]
                          
#'* Yukon *

unique(data_yukon$species_name)

cond <- dataset2$region == "Yukon"
max(dataset2$year[cond]) # 2008
sum(cond) # 600

max(data_yukon$year) # 2025
nrow(data_yukon) # 784

unique(data_yukon$source_id)

# remove the NuSEDS data from dataset2
dataset2 <- dataset2[dataset2$region != "Yukon",]

data_yukon$pointid <- NA

dataset2 <- rbind(dataset2,data_yukon[,colnames(dataset2)])

nrow(dataset2)
# 314306

#
#'* Columbia data ??? *
#

cond <- nuseds_cuid_streamid$region == "Columbia"
unique(nuseds_cuid_streamid[cond,c("cu_name_pse","CENSUS_SITE","GFE_ID","IndexId","Year")])
max(nuseds_cuid_streamid$Year[cond])


#
#'* Steelhead *
#


# need to update VIMI
cuid <- unique(data_SH$cuid)
for(c in cuid){
  cond <- conservationunits_decoder$cuid == c
  region <- unique(conservationunits_decoder$region[cond])
  
  cond <- data_SH$cuid == c
  data_SH$region[cond] <- region
}

unique(data_SH$region)

# 
unique(data_SH$species_name)
unique(dataset2$species_name)

data_SH$species_qualified <- sapply(data_SH$cuid,function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(unique(conservationunits_decoder$species_qualified[cond]))
})

data_SH$pointid <- NA

colnames(dataset2)[!colnames(dataset2) %in% colnames(data_SH)]

dataset2 <- rbind(dataset2,data_SH[,colnames(dataset2)])

nrow(dataset2)
# 314928


#
# checks on dataset2 ----
# 

#'* Check if a same location has multiple coordinates *

# Check for those with GFE_ID 
check_locations <- dataset2[,c("stream_name_pse","latitude","longitude","GFE_ID")] |> unique()
nrow(check_locations) # 2406 2407

cond_NA <- is.na(check_locations$GFE_ID)
any(duplicated(check_locations$GFE_ID[!cond_NA]))
cond <- duplicated(check_locations$GFE_ID[!cond_NA])
any(cond)
# FALSE

# Check for those without GFE_ID 
cond_NA <- is.na(dataset2$GFE_ID)
check_locations <- dataset2[cond_NA,c("region","stream_name_pse","latitude","longitude")] |> unique()
nrow(check_locations) # 41

cond <- duplicated(check_locations$stream_name_pse)
unique(check_locations[cond,])

cond <- dataset2$stream_name_pse == "PORCUPINE RIVER"
dataset2[cond,c("region","stream_name_pse","GFE_ID","latitude","longitude","streamid")] |> unique()

# all good.


#'* Are each streamid unique *
length(unique(dataset2$streamid)) - nrow(unique(dataset2[,c("cuid","stream_name_pse","latitude","longitude")])) # 0


sum(is.na(dataset2$cuid))
sum(is.na(dataset2$stream_name_pse))
sum(is.na(dataset2$streamid))

#
# Define point_id WHICH IS NOW A PERMANENT FIELD -----
#

# point_id is the ID of the location, like DFO's GFE_ID except the latter is not 
# available for each location.

cols <- c("stream_name_pse","latitude","longitude")
nrow(unique(dataset2[,cols]))             # 2406
nrow(unique(dataset2[,c(cols,"GFE_ID")])) # 2406

locations <- unique(dataset2[,cols])
nrow(locations) # 2406

count <- 1
for(r in 1:nrow(locations)){
  cond <- dataset2$stream_name_pse == locations$stream_name_pse[r] &
    dataset2$latitude == locations$latitude[r] &
    dataset2$longitude == locations$longitude[r]
  dataset2$pointid[cond] <- count
  count <- count + 1
}

nrow(unique(dataset2[,c(cols,"GFE_ID","pointid")])) # 2406
length(unique(dataset2$pointid)) # 2406

#
# Export dataset2_spawner_surveys_DATE.csv -----
#

unique(dataset2$region)

# order fields:
dataset2 <- dataset2 %>% 
  arrange(factor(region, levels = c("Yukon",
                                    "Northern Transboundary",
                                    "Haida Gwaii",
                                    "Nass",
                                    "Skeena",
                                    "Central Coast",
                                    "East Vancouver Island & Mainland Inlets",
                                    "West Vancouver Island",
                                    "Fraser",
                                    "Columbia")),
          species_name,
          cu_name_pse,
          factor(indicator, levels = c("Y","N","",NA)),
          stream_name_pse,
          year)


# Produce a dummy datasets in the loca; /ouput repo to push to github
write.csv(dataset2[1:2,],paste0(getwd(),"/output/dataset2_spawner-surveys_dummy.csv"))

date <- Sys.Date()
write.csv(dataset2,paste0(wd_output,"/archive/dataset2_spawner-surveys_",date,".csv"),
          row.names = F)


# dataset2 <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive/"),
#                                              pattern = "dataset2") # dataset2_spawner_surveys
# cond <- dataset2$species_name == "Coho"
# sites_Coho <- unique(dataset2$GFE_ID[cond])
# length(sites_Coho) # 1705

#
# Check if multiple series appear for a same cuid - streamid combination OLD -----------
#' There WERE several instances were there were multiple locations associated to
#' a same sys_nm for a same cuid
#' where generate multiple instances where several time series have the same streamid and cuid and point id.

nrow(dataset2) # 149828

d1 <- dataset2 %>%
  # group_by(region,species_name,species_abbr,cuid,cu_name_pse,year) %>%
  group_by(region,species_name,species_qualified,cuid,cu_name_pse,pointid,streamid,
           stream_name_pse,year,survey_method,survey_quality) %>%
  summarise(stream_observed_count_2 = sum(stream_observed_count))

nrow(d1) # 149828

nrow(dataset2) - nrow(d1) # 0 --> all good

series <- unique(dataset2[,c("region","cuid","pointid","streamid")])
for(r in 1:nrow(series)){
  # r <- 1
  region <- series$region[r]
  cuid <- series$cuid[r]
  pointid <- series$pointid[r]
  streamid <- series$streamid[r]
  
  cond <- dataset2$region == region &
    dataset2$cuid == cuid &
    dataset2$streamid == streamid
  
  yrs <- dataset2[cond,]$year
  
  if(any(duplicated(yrs))){
    break
    dataset2_cut <- dataset2[cond,]
    
    # years with conflicting survey_method and survey_quality
    yrs_dupli <- yrs[duplicated(yrs)]
    
    dataset2_cut[dataset2_cut$year %in% yrs_dupli,]
    
    cond_method_diff <- cond & dataset2$year %in% yrs_dupli 
    
    # check if the survey_method are the same for each duplicated year:
    survey_method_yr_diff <- sapply(X = yrs_dupli, FUN = function(yr){
      cond_yr <- cond & dataset2$year == yr
      survey_method <- unique(dataset2$survey_method[cond_yr])
      out <- length(survey_method) > 1
      return(out)
    })
    
    # check if the survey_quality are the same for each duplicated year:
    survey_quality_yr_diff <- sapply(X = yrs_dupli, FUN = function(yr){
      cond_yr <- cond & dataset2$year == yr
      survey_quality <- unique(dataset2$survey_quality[cond_yr])
      out <- length(survey_quality) > 1
      return(out)
    })
    
    # no different in the survey methods and quality in duplicated years
    if(!any(survey_method_yr_diff) & !any(survey_quality_yr_diff)){
      
      d_new <- dataset2_cut %>%
        # group_by(region,species_name,species_abbr,cuid,cu_name_pse,year) %>%
        group_by(region,species_name,species_abbr,cuid,cu_name_pse,pointid,streamid,
                 stream_name_pse,indicator,longitude,latitude,year,
                 survey_method,survey_quality) %>%
        summarise(stream_observed_count = sum(stream_observed_count))
      
      
    }else{
      break
      
      
    }
    # dataset2 <- dataset2[!cond,]
    # dataset2 <- rbind(dataset2,d_new)
  }
}


#
# UPDATE YUKON ONLY Start here OLD ------
#

#'* conservationunits_decoder * 
conservationunits_decoder <- read.csv(paste0(wd_pop_indic_data_input_dropbox,
                                             "/conservationunits_decoder.csv"),header = T)
unique(conservationunits_decoder$region)

#' * Import the full dataset2_spawner-surveys_DATE.csv  *
#' 
spawnerSurveryFull <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"),
                                                 pattern = "dataset2_spawner-surveys") #

head(spawnerSurveryFull)

# remove the Yukon data from spawnerSurveryFull
cond_Yukon <- spawnerSurveryFull$region == "Yukon"
sum(cond_Yukon) # 657

spawnerSurveryFull <- spawnerSurveryFull[!cond_Yukon,]
unique(spawnerSurveryFull$region)

# check source_id
unique(spawnerSurveryFull$source_id)
cond <- spawnerSurveryFull$source_id == ""
unique(spawnerSurveryFull$region[cond])
unique(spawnerSurveryFull$species_name[cond]) # --> no source for steelhead yet
spawnerSurveryFull[cond,]

#' 
#' * Import the Yukon data *
wd_Yukon_data <- paste0(wd_X_Drive1_PROJECTS,
                        "/1_Active/Population Methods and Analysis/population-data/yukon-data/output/archive")

Yukon_data <- import_mostRecent_file_fun(wd = wd_Yukon_data,
                                         pattern = "dataset2_spawner-surveys_Yukon") # TEMPORARY
head(Yukon_data)
nrow(Yukon_data) # 772

# merge the two
spawnerSurveryFull <- rbind(spawnerSurveryFull,Yukon_data)

# Update VIMI:
cond_VIMI <- spawnerSurveryFull$region ==  "Vancouver Island & Mainland Inlets"
for(cu in unique(spawnerSurveryFull$cuid[cond_VIMI])){
  cond <- conservationunits_decoder$cuid == cu
  rg <- unique(conservationunits_decoder$region[cond])
  
  cond <- spawnerSurveryFull$cuid == cu
  spawnerSurveryFull$region[cond] <- rg
}
unique(spawnerSurveryFull$region)


# order fields:
spawnerSurveryFull <- spawnerSurveryFull %>% 
  arrange(factor(region, levels = c("Yukon","Transboundary","Haida Gwaii","Nass",
                                    "Skeena","Central Coast",
                                    "East Vancouver Island & Mainland Inlets",
                                    "West Vancouver Island",
                                    "Fraser","Columbia")),
          species_name,
          cu_name_pse,
          factor(indicator, levels = c("Y","N","",NA)),
          stream_name_pse,
          year)

#
date <- Sys.Date()
date <- "2025-09-15"
write.csv(spawnerSurveryFull,paste0(wd_output,"/archive/dataset2_spawner-surveys_",date,".csv"),
          row.names = F)

# Produce a dummy datasets in the loca; /ouput repo to push to github
write.csv(spawnerSurveryFull[1:2,],paste0(getwd(),"/output/dataset2_spawner-surveys_dummy.csv"))


