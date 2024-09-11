

#'******************************************************************************
#' The goal of the script is to combine the cleaned NuSEDS data with the data 
#' sent by Arianne Nickels from the Reynolds' Lab (SFU).
#' 
#' Files imported:
#' - SFU_Escapement_PSF.xlsx       : the Reynolds's Lab main excel file 
#' - SFU_stream_Coordinates.xlsx   : the Reynolds's Lab stream coordinates used here
#' - 2_nuseds_cuid_streamid_nuseds_20240419.csv : the cleaned NuSEDS data with PSE cuid and streamid (and with data no in PSE)
#' - DFO_All_Streams_Segments_20240408.xlsx : the file sent by Wu Zhipeng (DFO) with stream-related information
#' 
#' Files produced: 
#' - 3_data_extra_Reynolds_lab_DATE.csv    
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

# Import functions for this specific project
source("Code/functions.R")

library(xlsx)
library(readxl)
library(dplyr)

options(warn = 0)
options(digits = 9) ## 7

# Import files and add relevant fields ------

#'* Import the SFU data: stream info *
SFU_stream <- read_excel(paste(wd_data_dropbox,"SFU_Escapement_PSF.xlsx",sep="/"), 
                        sheet = 2) |>
  as.data.frame()

head(SFU_stream)


#'* Import the SFU data: escapement *
SFU_escap <- read_excel(paste(wd_data_dropbox,"SFU_Escapement_PSF.xlsx",sep="/"), 
                       sheet = 4) |>
  as.data.frame()

head(SFU_escap)
nrow(SFU_escap) # 748

# Remove rows with NAs
cond <- is.na(SFU_escap$`Reynold Lab Final Number`) | SFU_escap$`Reynold Lab Final Number` == "NA"
sum(cond) # 83
SFU_escap <- SFU_escap[!cond,]

SFU_escap_colnames <- colnames(SFU_escap)

#'* Import the SFU stream coordinates * 
#' It is unclear how the coordinates were collected in SFU_stream. Arianne 
#' Nickels sent us another set of coordinates (cf. email from May 13 2024):
#' "The coordinates I sent you were provided to help locate the streams we monitor.
#' Unfortunately, they predate both of us, so we aren't entirely sure how they 
#' were defined.
#' However, I found two more useful sets of coordinates. Attached is a sheet with
#' UTMs for all of our streams, determined based on the watershed code from the 
#' BC Gov Watershed Dictionary Query. It also includes coordinates taken at the 
#' start of the estuary of each river that we monitor, approximately where we 
#' begin our counts." 
SFU_stream_2 <-  read_excel(paste(wd_data_dropbox,"SFU_stream_Coordinates.xlsx",
                                  sep="/")) |>
  as.data.frame()

head(SFU_stream_2)


#'* Import the cleaned NuSEDS data matched with the cuid and streamid of the PSE *
nuseds_cuid_streamid <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                                   pattern = "nuseds_cuid_streamid")

head(nuseds_cuid_streamid)

#'* Import the NuSEDS field definitions *
nuseds_fields_definitions <- nuseds_fields_definitions_fun(wd_references = wd_references_dropbox)

#'* Import list of all the stream provided by Wu by email Zhipeng (DFO) *
all_streams_DFO <- read_excel(paste0(wd_data_dropbox,"/DFO_All_Streams_Segments_20240408.xlsx"), 
                              sheet = "All Streams")


#'* Import the conservationunits_decoder.csv *
datasetsNames_database <- datasetsNames_database_fun()
fromDatabase <- update_file_csv <- F
#' # To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

#'* Import the original conservation_unit_system_sites: *
NuSEDS_datasets_names <- NuSEDS_datasets_names_fun()
conservation_unit_system_sites <- datasets_NuSEDS_fun(name_dataSet = NuSEDS_datasets_names$conservation_unit_system_sites, 
                                                      from_NuSEDS_website = F, 
                                                      wd = wd_data_dropbox)
unique(conservation_unit_system_sites$SPECIES_QUALIFIED)

# Add SPECIES to conservation_unit_system_sites
conservation_unit_system_sites$SPECIES <- NA
SPECIES_QUALIFIED <- c("CK","CO","PKE","PKO","CM","SEL","SER")
SPECIES <- c("Chinook","Coho","Pink","Pink","Chum","Sockeye","Sockeye")
for(i in 1:length(SPECIES_QUALIFIED)){
  cond <- conservation_unit_system_sites$SPECIES_QUALIFIED == SPECIES_QUALIFIED[i]
  conservation_unit_system_sites$SPECIES[cond] <- SPECIES[i]
}

#'* Import the original all_areas_nuseds data: *
all_areas_nuseds <- datasets_NuSEDS_fun(name_dataSet = NuSEDS_datasets_names$all_areas_nuseds, 
                                        from_NuSEDS_website = F, 
                                        wd = wd_data_dropbox)

#' remove "Steelhead" and "Atlantic"
all_areas_nuseds <- all_areas_nuseds[!all_areas_nuseds$SPECIES %in% c("Steelhead","Atlantic"),]

#' add the field var_in_MAX_ESTIMATE to all_areas_nuseds
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

#' add SPECIES_QUALIFIED to all_areas_nuseds
all_areas_nuseds$SPECIES_QUALIFIED <- NA
species <- c("Coho","Chinook","Chum","Kokanee") # do pink and sockeye after
SPECIES_QUALIFIED <- c("CO","CK","CM","SEL")
for(i in 1:length(species)){
  cond <- all_areas_nuseds$SPECIES == species[i]
  all_areas_nuseds$SPECIES_QUALIFIED[cond] <- SPECIES_QUALIFIED[i]
}

# for pink:
cond <- all_areas_nuseds$SPECIES == "Pink"
cond_even <- cond & all_areas_nuseds$ANALYSIS_YR %% 2 == 0
cond_odd <- cond & all_areas_nuseds$ANALYSIS_YR %% 2 == 1
all_areas_nuseds$SPECIES_QUALIFIED[cond_even] <- "PKE"
all_areas_nuseds$SPECIES_QUALIFIED[cond_odd] <- "PKO"

#' for Sockeye: SEL vs SER
#' 1st check in conservation_system_site
cond_sockeye <- all_areas_nuseds$SPECIES == "Sockeye"
sockeye_POP_ID <- unique(all_areas_nuseds[cond_sockeye,c("SPECIES","POP_ID")])
for(i in 1:nrow(sockeye_POP_ID)){
  cond_cuss <- conservation_unit_system_sites$SPECIES == sockeye_POP_ID$SPECIES[i] & 
    conservation_unit_system_sites$POP_ID == sockeye_POP_ID$POP_ID[i]
  
  SPECIES_QUALIFIED <- unique(conservation_unit_system_sites$SPECIES_QUALIFIED[cond_cuss])
  
  if(length(SPECIES_QUALIFIED) == 1){
    cond_nuseds <- all_areas_nuseds$SPECIES == sockeye_POP_ID$SPECIES[i] &
      all_areas_nuseds$POP_ID == sockeye_POP_ID$POP_ID[i]
    
    all_areas_nuseds$SPECIES_QUALIFIED[cond_nuseds] <- SPECIES_QUALIFIED
  }
}

# There are still NAs...
unique(all_areas_nuseds$SPECIES_QUALIFIED)

#' ... because there are 971 Sockeye populations that are not in 
#' conservation_unit_system_sites:
cond_NA <- is.na(all_areas_nuseds$SPECIES_QUALIFIED)
sum(cond_NA)
unique(all_areas_nuseds[cond_NA,c("SPECIES")])
unique(all_areas_nuseds[cond_NA,c("SPECIES","POP_ID","POPULATION")])
unique(all_areas_nuseds[cond_NA,c("SPECIES","POP_ID","POPULATION")]) |>
  nrow()
sum(conservation_unit_system_sites$POP_ID %in% unique(all_areas_nuseds[cond_NA,c("POP_ID")]))

all_areas_nuseds$SPECIES_QUALIFIED[cond_NA] <- "SE"

unique(all_areas_nuseds$SPECIES_QUALIFIED)

#
# Match stream names -------------
#
nrow(SFU_stream) # 22

# Convert coordinates from degree to decimal
SFU_stream$Latitude <- gsub("\"","",SFU_stream$Latitude)
SFU_stream$Longitude <- gsub("\"","",SFU_stream$Longitude)

# some typos to fix 1st
char_toFix <- c("\\.","'","’","”")
for(c in char_toFix){
  SFU_stream$Latitude <- gsub(c," ",SFU_stream$Latitude)
  SFU_stream$Longitude <- gsub(c," ",SFU_stream$Longitude)
}

SFU_stream$Latitude <- angle_to_dec_fun(SFU_stream$Latitude)
SFU_stream$Longitude <- angle_to_dec_fun(SFU_stream$Longitude)

# similar for SFU_stream_2:
lat_long_2 <- strsplit(SFU_stream_2$`Latitude_Longitude (River Mouth)`,
                        split = " ")

char_toFix <- c("°","'","’","”")
lat_long_2 <- lapply(lat_long_2,function(ll){
  out <- ll
  for(c in char_toFix){
    out <- gsub(c," ",out)
  }
  out <- gsub("\"W","",out)
  out <- gsub("\"N","",out)
  out <- angle_to_dec_fun(out)
  return(out)
}) 

SFU_stream_2$Latitude_2 <- sapply(lat_long_2,function(ll){ll[1]})
SFU_stream_2$Longitude_2 <- sapply(lat_long_2,function(ll){ll[2]})

# add the negative to Longitude so we don't end up in Russia
SFU_stream$Longitude <- - SFU_stream$Longitude
SFU_stream_2$Longitude_2 <- - SFU_stream_2$Longitude_2

# add the new coordinates to SFU_stream
SFU_stream$`SFU Stream Name`[!SFU_stream$`SFU Stream Name` %in% SFU_stream_2$Stream]
SFU_stream_2$Stream[! SFU_stream_2$Stream %in% SFU_stream$`SFU Stream Name`]
SFU_stream$Longitude_2 <- SFU_stream$Latitude_2 <- NA
for(r in 1:nrow(SFU_stream)){
  # r <- 1
  name <- SFU_stream$`SFU Stream Name`[r]
  if(name == "Beales (left and right)"){ 
    name <- "Beales Left" # same coordinates for left and right so just pick one
  }else if(name == "Goat Bushu Creek"){
    name <- "Goat Bushu"
  }else if(name == "Kunsoot Main"){
    name <- "Kunsoot"
  }else if(name == "Mosquito (left and Right)"){
    name <- "Mosquito Bay Left" # same coordinates for left and right so just pick one
  }
  
  cond <- SFU_stream_2$Stream == name
  SFU_stream$Longitude_2[r] <- SFU_stream_2$Longitude_2[cond]
  SFU_stream$Latitude_2[r] <- SFU_stream_2$Latitude_2[cond]
}

SFU_stream[,c("Latitude","Latitude_2","Longitude","Longitude_2")]

# remove space in column names in SFU_stream:
colnames(SFU_stream) <- gsub(" ","_",colnames(SFU_stream))

# return the unique locations from nuseds_cuid_streamid 
stream_name_PSE_col <- c("WATERBODY","SYSTEM_SITE","sys_nm_final","GFE_ID","AREA")
stream_name_PSE <- unique(nuseds_cuid_streamid[,stream_name_PSE_col])

# few corrections of typos in SFU_stream$Stream_Name_in_NuSEDS:
unique(SFU_stream$Stream_Name_in_Salmon_Explorer)
SFU_stream$Stream_Name_in_Salmon_Explorer <- gsub("\\?","",SFU_stream$Stream_Name_in_Salmon_Explorer)
cond <- SFU_stream$Stream_Name_in_Salmon_Explorer == "NA"
SFU_stream$Stream_Name_in_Salmon_Explorer[cond] <- NA
cond <- SFU_stream$Stream_Name_in_NuSEDS == "NA"
SFU_stream$Stream_Name_in_NuSEDS[cond] <- NA

# options(warn=1)  # print warnings as they occur
options(warn = 2)  # treat warnings as errors

#' Try to match the stream names in SFU_stream with those in the PSE 
#' (stream_name_PSE). Provide the associated GFE_ID in that case.
SFU_stream$GFE_ID <- NA
SFU_stream$in_PSE <- NA
for(r in 1:nrow(SFU_stream)){
  # r <- 2
  SFU_Stream_Name <- simplify_string_fun(SFU_stream$SFU_Stream_Name[r])
  Stream_Name_in_NuSEDS <- simplify_string_fun(SFU_stream$Stream_Name_in_NuSEDS[r])
  Stream_Name_in_PSE <- simplify_string_fun( SFU_stream$Stream_Name_in_Salmon_Explorer[r])
  AREA <- as.character(SFU_stream$Area[r])
  
  cond_NUSEDS <- Stream_Name_in_NuSEDS == simplify_string_fun(stream_name_PSE$WATERBODY) |
    Stream_Name_in_NuSEDS == simplify_string_fun(stream_name_PSE$SYSTEM_SITE)
  
  cond_PSE <- Stream_Name_in_PSE == simplify_string_fun(stream_name_PSE$sys_nm_final)
  
  if(!any(is.na(cond_NUSEDS)) & !any(is.na(cond_PSE))){
    cond_Stream_Name <- cond_NUSEDS | cond_PSE
    
  }else if(!any(is.na(cond_NUSEDS)) & any(is.na(cond_PSE))){
    cond_Stream_Name <- cond_NUSEDS
    
  }else if(any(is.na(cond_NUSEDS)) & !any(is.na(cond_PSE))){
    cond_Stream_Name <- cond_PSE
    
  }else{
    cond_Stream_Name <- NA
  }
  
  # filter with AREA:
  cond_AREA <- AREA == stream_name_PSE$AREA
  
  if(!is.na(cond_Stream_Name)[1]){
    cond <- cond_Stream_Name & cond_AREA
    
  }else{
    cond <- NA
    
  }
  
  if(!is.na(cond)[1] & sum(cond) > 0){
    SFU_stream$GFE_ID[r] <- stream_name_PSE$GFE_ID[cond]
    SFU_stream$in_PSE[r] <- T
    
  }else{
    SFU_stream$in_PSE[r] <- F
  }
}

options(warn = 0) 

# streams that matched:
cond <- !is.na(SFU_stream$GFE_ID)
SFU_stream[cond,colnames(SFU_stream) != "Notes"]

# Compare the coordinates with the ones in NuSEDS
SFU_stream$Y_LAT <- SFU_stream$X_LONGT <- SFU_stream$dist <- NA
SFU_stream$Y_LAT[cond] <- sapply(X = SFU_stream$GFE_ID[cond],
                                 FUN = function(gfeid){
                                   cond_here <- nuseds_cuid_streamid$GFE_ID == gfeid
                                   return(unique(nuseds_cuid_streamid$Y_LAT[cond_here]))
                                 }) %>% unlist()

SFU_stream$X_LONGT[cond] <- sapply(X = SFU_stream$GFE_ID[cond],
                                   FUN = function(gfeid){
                                     cond_here <- nuseds_cuid_streamid$GFE_ID == gfeid
                                     return(unique(nuseds_cuid_streamid$X_LONGT[cond_here]))
                                  }) %>% unlist()

SFU_stream$dist[cond] <- apply(X = SFU_stream[cond,],
                                  MARGIN = 1,
                                  FUN = function(r){
                                    dist <- distance_Euclidean_fun(x_ref = r["Longitude_2"],
                                                                   y_ref = r["Latitude_2"], 
                                                                   x = r["X_LONGT"],
                                                                   y = r["Y_LAT"])
                                     return(round(dist,4))
                                  })

# For dist, 0.1 ~ 10km
SFU_stream[cond,c("SFU_Stream_Name","Stream_Name_in_NuSEDS","Latitude_2","Longitude_2","GFE_ID","Y_LAT","X_LONGT","dist")]

nuseds_fields_definitions_fun(wd_references = wd_references_dropbox)

# streams that did not matched:
cond <- is.na(SFU_stream$GFE_ID)
SFU_stream[cond,colnames(SFU_stream) != "Notes"]


#' Investigate "FANCY COVE HEAD CREEK" --> "FANCY COVE HEAD CREEK"
#' It is in the original all_area_nuseds but not in conservation_unit_system_sites 
#' (CUSS):
#' - both the GFE_ID (2685) and POP_IDs (7796, 7797) are not in CUSS.
#' But it is in the in the list of streams sent by DFO (all_streams_DFO) --> 
#' find the GFE_ID.
cond_nuseds <- all_areas_nuseds$WATERBODY == "FANCY COVE HEAD CREEK"
all_areas_nuseds[cond_nuseds,c("WATERBODY","SPECIES","POP_ID","ANALYSIS_YR","GFE_ID")]

cond_cuss <- conservation_unit_system_sites$GFE_ID == unique(all_areas_nuseds$GFE_ID[cond_nuseds])
conservation_unit_system_sites[cond_cuss,]

cond_cuss <- conservation_unit_system_sites$POP_ID %in% unique(all_areas_nuseds$POP_ID[cond_nuseds])
conservation_unit_system_sites[cond_cuss,]

cond_DFO <- all_streams_DFO$NME == "FANCY COVE HEAD CREEK"
cond_SFU <- SFU_stream$Stream_Name_in_NuSEDS == "FANCY COVE HEAD CREEK" & 
  !is.na(SFU_stream$Stream_Name_in_NuSEDS)

SFU_stream[cond_SFU,c('SFU_Stream_Name','Latitude','Longitude')]
SFU_stream$GFE_ID[cond_SFU] <- unique(all_streams_DFO$ID[cond_DFO])
SFU_stream$X_LONGT[cond_SFU] <- unique(all_streams_DFO$X_LONGT[cond_DFO]) %>%
  as.numeric(.) %>%
  round(.,6)
SFU_stream$Y_LAT[cond_SFU] <- unique(all_streams_DFO$Y_LAT[cond_DFO]) %>%
  as.numeric(.) %>%
  round(.,6) 

SFU_stream[cond_SFU,]

#' Investigate "Fancy Right" --> "FANCY COVE RIGHT HAND CREEK"?
cond_SFU <- SFU_stream$SFU_Stream_Name == "Fancy Right"
SFU_stream[cond_SFU,]

char <- simplify_string_fun("Fancy")
cond_DFO <- grepl(char,simplify_string_fun(all_streams_DFO$NME)) &
  !is.na(all_streams_DFO$NME) &
  all_streams_DFO$AREA == SFU_stream$Area[cond_SFU]
all_streams_DFO[cond_DFO,c("NME","Y_LAT","X_LONGT")]

distance_Euclidean_fun(x_ref = SFU_stream$Longitude[cond_SFU],
                       y_ref = SFU_stream$Latitude[cond_SFU], 
                       x = all_streams_DFO$X_LONGT[cond_DFO],
                       y = all_streams_DFO$Y_LAT[cond_DFO])

cond_DFO <- all_streams_DFO$NME == "FANCY COVE RIGHT HAND CREEK"

SFU_stream[cond_SFU,c('SFU_Stream_Name','Stream_Name_in_NuSEDS','Latitude','Longitude')]
SFU_stream$Stream_Name_in_NuSEDS[cond_SFU] <- unique(all_streams_DFO$NME[cond_DFO])
SFU_stream$GFE_ID[cond_SFU] <- unique(all_streams_DFO$ID[cond_DFO])
SFU_stream$X_LONGT[cond_SFU] <- unique(all_streams_DFO$X_LONGT[cond_DFO]) %>%
  as.numeric(.) %>%
  round(.,6)
SFU_stream$Y_LAT[cond_SFU] <- unique(all_streams_DFO$Y_LAT[cond_DFO]) %>%
  as.numeric(.) %>%
  round(.,6)

SFU_stream[cond_SFU,]

#' Investigate "Bullock Square" --> New location
cond_SFU <- SFU_stream$SFU_Stream_Name == "Bullock Square"
char <- simplify_string_fun("Bullock")
cond <- grepl(char,simplify_string_fun(all_streams_DFO$NME)) &
  !is.na(all_streams_DFO$NME) &
  all_streams_DFO$AREA == SFU_stream$Area[cond_SFU]
all_streams_DFO$NME[cond]

distance_Euclidean_fun(x_ref = SFU_stream$Longitude[cond_SFU],
                       y_ref = SFU_stream$Latitude[cond_SFU], 
                       x = all_streams_DFO$X_LONGT[cond],
                       y = all_streams_DFO$Y_LAT[cond])

char <- simplify_string_fun("Bullock Square")
cond_nuseds <- grepl(char,simplify_string_fun(all_areas_nuseds$WATERBODY)) |
  grepl(char,simplify_string_fun(all_areas_nuseds$GAZETTED_NAME)) |
  #grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_1)) |
  grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_2))
all_areas_nuseds[cond_nuseds,]

cond_nuseds <- grepl(char,simplify_string_fun(conservation_unit_system_sites$SYSTEM_SITE))
conservation_unit_system_sites[cond_nuseds,]


#' Investigate "Codville" --> New location
cond_SFU <- SFU_stream$SFU_Stream_Name == "Codville"
char <- simplify_string_fun("Codville")
cond <- grepl(char,simplify_string_fun(all_streams_DFO$NME)) &
  !is.na(all_streams_DFO$NME) &
  all_streams_DFO$AREA == SFU_stream$Area[cond_SFU]
all_streams_DFO$NME[cond]

cond_nuseds <- grepl(char,simplify_string_fun(all_areas_nuseds$WATERBODY)) |
  grepl(char,simplify_string_fun(all_areas_nuseds$GAZETTED_NAME)) |
  #grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_1)) |
  grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_2))
all_areas_nuseds[cond_nuseds,]

cond_nuseds <- grepl(char,simplify_string_fun(conservation_unit_system_sites$SYSTEM_SITE))
conservation_unit_system_sites[cond_nuseds,]

#' Investigate "Fell" --> FELL CREEK
cond_SFU <- SFU_stream$SFU_Stream_Name == "Fell"
char <- simplify_string_fun("Fell")
SFU_stream[cond_SFU,]
cond_DFO <- grepl(char,simplify_string_fun(all_streams_DFO$NME)) &
  !is.na(all_streams_DFO$NME) &
  all_streams_DFO$AREA == SFU_stream$Area[cond_SFU]
all_streams_DFO[cond_DFO,c("NME","Y_LAT","X_LONGT")]

distance_Euclidean_fun(x_ref = SFU_stream$Longitude[cond_SFU],
                       y_ref = SFU_stream$Latitude[cond_SFU], 
                       x = all_streams_DFO$X_LONGT[cond_DFO],
                       y = all_streams_DFO$Y_LAT[cond_DFO])

cond_DFO <- all_streams_DFO$NME == "FELL CREEK"

SFU_stream[cond_SFU,c('SFU_Stream_Name','Stream_Name_in_NuSEDS','Latitude','Longitude')]
SFU_stream$Stream_Name_in_NuSEDS[cond_SFU] <- unique(all_streams_DFO$NME[cond_DFO])
SFU_stream$GFE_ID[cond_SFU] <- unique(all_streams_DFO$ID[cond_DFO])
SFU_stream$X_LONGT[cond_SFU] <- unique(all_streams_DFO$X_LONGT[cond_DFO]) %>%
  as.numeric(.) %>%
  round(.,6)
SFU_stream$Y_LAT[cond_SFU] <- unique(all_streams_DFO$Y_LAT[cond_DFO]) %>%
  as.numeric(.) %>%
  round(.,6)

#' Investigate "Kill" --> new location
cond_SFU <- SFU_stream$SFU_Stream_Name == "Kill"
char <- simplify_string_fun("Kill")
SFU_stream[cond_SFU,]
cond <- grepl(char,simplify_string_fun(all_streams_DFO$NME)) &
  !is.na(all_streams_DFO$NME) &
  all_streams_DFO$AREA == SFU_stream$Area[cond_SFU]
all_streams_DFO[cond,c("NME","Y_LAT","X_LONGT")]

cond_nuseds <- (grepl(char,simplify_string_fun(all_areas_nuseds$WATERBODY)) |
  grepl(char,simplify_string_fun(all_areas_nuseds$GAZETTED_NAME)) |
  #grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_1)) |
  grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_2))) &
  all_areas_nuseds$AREA == SFU_stream$Area[cond_SFU]
  
all_areas_nuseds[cond_nuseds,c("AREA","WATERBODY","GAZETTED_NAME","LOCAL_NAME_1","LOCAL_NAME_2","ANALYSIS_YR")] |>
  unique()

cond_nuseds <- grepl(char,simplify_string_fun(conservation_unit_system_sites$SYSTEM_SITE)) &
  conservation_unit_system_sites$AREA == SFU_stream$Area[cond_SFU]
conservation_unit_system_sites[cond_nuseds,]


#' Investigate "Mosquito (left and Right)" --> new location
cond_SFU <- SFU_stream$SFU_Stream_Name == "Mosquito (left and Right)"
SFU_stream[cond_SFU,]
char <- simplify_string_fun("Mosquito")
cond <- grepl(char,simplify_string_fun(all_streams_DFO$NME)) &
  !is.na(all_streams_DFO$NME) &
  all_streams_DFO$AREA == SFU_stream$Area[cond_SFU]
all_streams_DFO[cond,c("NME","Y_LAT","X_LONGT")]

cond_nuseds <- (grepl(char,simplify_string_fun(all_areas_nuseds$WATERBODY)) |
                  grepl(char,simplify_string_fun(all_areas_nuseds$GAZETTED_NAME)) |
                  #grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_1)) |
                  grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_2))) &
  all_areas_nuseds$AREA == SFU_stream$Area[cond_SFU]

all_areas_nuseds[cond_nuseds,c("AREA","WATERBODY","GAZETTED_NAME","LOCAL_NAME_1","LOCAL_NAME_2","ANALYSIS_YR")] |>
  unique()

cond_nuseds <- grepl(char,simplify_string_fun(conservation_unit_system_sites$SYSTEM_SITE)) &
  conservation_unit_system_sites$AREA == SFU_stream$Area[cond_SFU]
conservation_unit_system_sites[cond_nuseds,]


#' Investigate "Troupe North" --> New location
cond_SFU <- SFU_stream$SFU_Stream_Name == "Troupe North"
SFU_stream[cond_SFU,]
char <- simplify_string_fun("Troupe")
cond <- grepl(char,simplify_string_fun(all_streams_DFO$NME)) &
  !is.na(all_streams_DFO$NME) &
  all_streams_DFO$AREA == SFU_stream$Area[cond_SFU]
all_streams_DFO[cond,c("NME","Y_LAT","X_LONGT")]

cond_nuseds <- (grepl(char,simplify_string_fun(all_areas_nuseds$WATERBODY)) |
                  grepl(char,simplify_string_fun(all_areas_nuseds$GAZETTED_NAME)) |
                  #grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_1)) |
                  grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_2))) &
  all_areas_nuseds$AREA == SFU_stream$Area[cond_SFU]

all_areas_nuseds[cond_nuseds,c("AREA","WATERBODY","GAZETTED_NAME","LOCAL_NAME_1","LOCAL_NAME_2","ANALYSIS_YR")] |>
  unique()

cond_nuseds <- grepl(char,simplify_string_fun(conservation_unit_system_sites$SYSTEM_SITE)) &
  conservation_unit_system_sites$AREA == SFU_stream$Area[cond_SFU]
conservation_unit_system_sites[cond_nuseds,]


#' Investigate "Troupe South" --> New location
cond_SFU <- SFU_stream$SFU_Stream_Name == "Troupe South"
SFU_stream[cond_SFU,]
char <- simplify_string_fun("Troupe")
cond <- grepl(char,simplify_string_fun(all_streams_DFO$NME)) &
  !is.na(all_streams_DFO$NME) &
  all_streams_DFO$AREA == SFU_stream$Area[cond_SFU]
all_streams_DFO[cond,c("NME","Y_LAT","X_LONGT")]

cond_nuseds <- (grepl(char,simplify_string_fun(all_areas_nuseds$WATERBODY)) |
                  grepl(char,simplify_string_fun(all_areas_nuseds$GAZETTED_NAME)) |
                  #grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_1)) |
                  grepl(char,simplify_string_fun(all_areas_nuseds$LOCAL_NAME_2))) &
  all_areas_nuseds$AREA == SFU_stream$Area[cond_SFU]

all_areas_nuseds[cond_nuseds,c("AREA","WATERBODY","GAZETTED_NAME","LOCAL_NAME_1","LOCAL_NAME_2","ANALYSIS_YR")] |>
  unique()

cond_nuseds <- grepl(char,simplify_string_fun(conservation_unit_system_sites$SYSTEM_SITE)) &
  conservation_unit_system_sites$AREA == SFU_stream$Area[cond_SFU]
conservation_unit_system_sites[cond_nuseds,]


# Refresh the distance:
SFU_stream$dist <- apply(X = SFU_stream,
                         MARGIN = 1,
                         FUN = function(r){
                                 dist <- distance_Euclidean_fun(x_ref = r["Longitude_2"],
                                                                y_ref = r["Latitude_2"], 
                                                                x = r["X_LONGT"],
                                                                y = r["Y_LAT"])
                                 return(round(dist,4))
                               })
SFU_stream[,colnames(SFU_stream) != "Notes"]
SFU_stream[SFU_stream$dist > 0.3 & !is.na(SFU_stream$dist),]

#
# Match populations -------
#' - if GFE_ID is PSE, look for CUs associated in PSE with same species.
#'    - if none --> trouble shoot
#'    - if > 2 --> trouble shoot
#' - if GFE_ID not present in PSE
#'    - 
#' Check that for a given location and species how many different CU are present.

# Add relevant fields:
SFU_escap$cuid <- SFU_escap$cu_name_pse <- SFU_escap$GFE_ID <- NA

#' Create the species_abbr
SFU_escap$species_abbr <- NA
SFU_escap$species_abbr[SFU_escap$Species == "Chum"] <- "CM"

# define PKE and PKO
cond_pink <- SFU_escap$Species == "Pink"
locations_pink <- unique(SFU_escap$`Stream Name`[cond_pink])
for(l in locations_pink){
  # l <- locations_pink[1]
  cond_pink_l <- SFU_escap$`Stream Name` == l & 
    SFU_escap$Species == "Pink"
  
  if(any(duplicated(SFU_escap[cond_pink_l,]$Year))){
    print("Duplicated years to trouble shoot.")
    break
  }
  
  yr_odd <- SFU_escap$Year[cond_pink_l][SFU_escap$Year[cond_pink_l] %% 2 == 1]
  yr_even <- SFU_escap$Year[cond_pink_l][SFU_escap$Year[cond_pink_l] %% 2 == 0]
  
  if(length(yr_odd) > 0){
    cond_pink_l_odd <- cond_pink_l & SFU_escap$Year %in% yr_odd
    SFU_escap$species_abbr[cond_pink_l_odd] <- "PKO"
  }
  
  if(length(yr_even) > 0){
    cond_pink_l_even <- cond_pink_l & SFU_escap$Year %in% yr_even
    SFU_escap$species_abbr[cond_pink_l_even] <- "PKE"
  }
}
SFU_escap[cond_pink,]

# replace " " by "_" in colnames(SFU_escap)
colnames(SFU_escap) <- gsub(" ","_",colnames(SFU_escap))

# Remove "?"
SFU_escap$Salmon_Explorer_Name <- gsub("\\?","",SFU_escap$Salmon_Explorer_Name)
SFU_escap$Salmon_Explorer_Name[SFU_escap$Salmon_Explorer_Name == "N/A"] <- NA

# unique stream-species associations
SFU_pop <- unique(SFU_escap[,c("Stream_Name","Species","species_abbr")])
SFU_pop$cuid <- SFU_pop$cu_name_pse <- SFU_pop$GFE_ID <- NA

#' The loop highlights cases where there are multiple potential CU_NAMEs for a given
#' location by printing ....
#' 
for(r in 1:nrow(SFU_pop)){
  # r <- 49
  Stream_Name <- simplify_string_fun(SFU_pop$Stream_Name[r])
  Species <- SFU_pop$Species[r]
  species_abbr <- SFU_pop$species_abbr[r]
  
  cond_SFU_escap <- SFU_escap$Stream_Name == SFU_pop$Stream_Name[r] &
    SFU_escap$Species == SFU_pop$Species[r]
  
  # some corrections to do in Stream_Name
  if(Stream_Name %in% c("fellcreek","leecreek")){ # remove "creek"
    Stream_Name <- gsub("creek","",Stream_Name)
    
  }else if(Stream_Name %in% c("goatbushu")){ # add "creek"
    Stream_Name <-paste0(Stream_Name,"creek")
    
  }else if(Stream_Name %in% c("kunsoot")){ # add "main"
    Stream_Name <-paste0(Stream_Name,"main")
  }
  
  cond_SFU_stream <- simplify_string_fun(SFU_stream$SFU_Stream_Name) == Stream_Name
  
  Done <- F
  
  #' if the location is in PSE: look for cuid(s) associated with the GFE_ID and
  #' species. 
  #' - Flag if there is more than one suggestions.
  #' - If there is no suggestions: CU_NAME is set to NA. The code after will look
  #' for the CU_NAME of the same species that is the closest to this GFE_ID.
  if(SFU_stream$in_PSE[cond_SFU_stream]){
    
    GFE_ID <- SFU_stream$GFE_ID[cond_SFU_stream]
    
    cond_PSE <- nuseds_cuid_streamid$GFE_ID == GFE_ID &
      nuseds_cuid_streamid$SPECIES_QUALIFIED == species_abbr
    
    if(sum(cond_PSE) == 0){
      # print("GFE_ID is in PSE but not CU found with this species. To trouble shoot.")
      CU_NAME <- NA
      # break
      
    }else{
      
      cuid <- unique(nuseds_cuid_streamid$cuid[cond_PSE])
      
      if(length(cuid) > 1){
        print("GFE_ID is in PSE but multiple cuid are found with this species. To trouble shoot.")
        break
        
      }else{
        SFU_pop$cuid[r] <- cuid
        SFU_pop$cu_name_pse[r] <- unique(nuseds_cuid_streamid$cu_name_pse[cond_PSE])
        SFU_pop$GFE_ID[r] <- SFU_stream$GFE_ID[cond_SFU_stream]
        Done <- T
      }
    }
  }
  
  #' If there is a GFE_ID but it is not in SFU_stream, look in both the original 
  #' all_areas_nuseds and conservation_system_sites to find the CU(s) associated
  #' with the GFE_ID and the species.
  #' - 1st look in conservation_system_sites (CUSS)
  #' - 2nd, if not in CUSS, look in all_areas_nuseds, find POP_ID, then look for 
  #'   POP_ID in cuss
  if(!Done & !is.na(SFU_stream$GFE_ID[cond_SFU_stream])){
    
    GFE_ID <- SFU_stream$GFE_ID[cond_SFU_stream]
    
    # try to find the CU associated to the GFE_ID and species
    cond_cuss <- conservation_unit_system_sites$GFE_ID == GFE_ID & 
      conservation_unit_system_sites$SPECIES_QUALIFIED == species_abbr
    
    # if the CUs are found in CUSS
    if(any(cond_cuss)){ 
      
      CU_NAME <- conservation_unit_system_sites$CU_NAME[cond_cuss]
      
      if(length(CU_NAME) > 1){
        print("Multiple CU_NAME for the same species and GFE_ID. To troubble shoot.")
        break
      }
      
    }else{    
      # if the CUs are not found in CUSS --> look in all_areas_nuseds, find the 
      # POP_ID, then check if POP_ID is in conservation_unit_system_sites
      
      cond_nuseds <- all_areas_nuseds$GFE_ID == GFE_ID & 
        all_areas_nuseds$SPECIES_QUALIFIED == species_abbr
      
      POP_ID <- unique(all_areas_nuseds$POP_ID[cond_nuseds])
      
      cond_cuss <- conservation_unit_system_sites$POP_ID == POP_ID
      
      if(any(cond_cuss)){ # if the CUs are found in CUSS
        CU_NAME <- conservation_unit_system_sites$CU_NAME[cond_cuss]
        
        if(length(CU_NAME) > 1){
          print("Multiple CU_NAME for the same species and POP_ID. To troubble shoot.")
          break
          
        }
      }else{  # if POP_ID in not in CUSS
        CU_NAME <- NA
      }
    } 
    
    #' If CU_NAME was found, find the corresponding cuid in nuseds_cuid_streamid
    if(!is.na(CU_NAME)){
      
      cond_PSE <- grepl(simplify_string_fun(CU_NAME),simplify_string_fun(nuseds_cuid_streamid$cu_name_dfo)) &
        !is.na( nuseds_cuid_streamid$cu_name_dfo) &
        nuseds_cuid_streamid$SPECIES_QUALIFIED == species_abbr
      
      if(any(cond_PSE)){
        
        cuid <- unique(nuseds_cuid_streamid$cuid[cond_PSE])
        
        if(length(cuid) > 1){
          print("GFE_ID is in PSE but multiple cuid are found with this species. To trouble shoot.")
          break
          
        }else{
          SFU_pop$cuid[r] <- cuid
          SFU_pop$cu_name_pse[r] <- unique(nuseds_cuid_streamid$cu_name_pse[cond_PSE])
          SFU_pop$GFE_ID[r] <- SFU_stream$GFE_ID[cond_SFU_stream]
          Done <- T
        }
        
      }else{
        print("CU_NAME found but it is not in nuseds_cuid_streamid$cu_name_dfo. (1)")
        break
        
      }
    }
  }
  
  #' If there is no GFE_ID (i.e. it is a new location) or if the CU could not be 
  #' found in any datasets:
  #' --> find in conservation_system_sites (CUSS) the closest locations to this 
  if(!Done){
    
    Area <- SFU_stream$Area[cond_SFU_stream]           # there is no Area in conservation_system_sites
    Longitude <- SFU_stream$Longitude_2[cond_SFU_stream]
    Latitude <- SFU_stream$Latitude_2[cond_SFU_stream]
    
    cond_CUSS <- conservation_unit_system_sites$SPECIES_QUALIFIED == species_abbr
    CUSS_cut <- conservation_unit_system_sites[cond_CUSS,]
    
    CUSS_cut$dist <- distance_Euclidean_fun(x_ref = Longitude,
                                            y_ref = Latitude, 
                                            x = CUSS_cut$X_LONGT, 
                                            y = CUSS_cut$Y_LAT)
    
    CUSS_cut <- CUSS_cut[CUSS_cut$dist < 0.5,]
    
    CUs_dist <- sapply(X =  unique(CUSS_cut[,"CU_NAME"]), 
                       FUN = function(cun){
                         cond <- CUSS_cut$CU_NAME == cun
                         out <- min(CUSS_cut$dist[cond])
                       })
    
    CUs_GFE_ID <- sapply(X =  unique(CUSS_cut[,"CU_NAME"]), 
                       FUN = function(cun){
                         cond <- CUSS_cut$CU_NAME == cun
                         out <- min(CUSS_cut$GFE_ID[cond])
                       })
    
    print("Smallest distance per potential CU; the one with the smallest dist is selected:")
    print(paste("r:",r))
    toPrint <- c(AREA,species_abbr)
    names(toPrint) <- c("AREA","SPECIES_QUALIFY")
    print(toPrint)
    print("Distances:")
    print(round(CUs_dist,3))
    print("GFE_ID:")
    print(CUs_GFE_ID)
    print("***")
    
    #' Exception: there are two very close distances, and the smallest one is 
    #' actually the wrong CU (according to PSE) --> remove the distance of the
    #' other wrong CU
    # SPILLER-FITZ HUGH-BURKE         HECATE LOWLANDS           MUSSEL-KYNOCH 
    #            0.0765339825            0.0614786157            0.3262141662 
    if("SPILLER-FITZ HUGH-BURKE" %in% names(CUs_dist)){
      CUs_dist <- CUs_dist[names(CUs_dist) != "HECATE LOWLANDS"] 
    }
    
    CU_NAME <- names(CUs_dist)[CUs_dist == min(CUs_dist)]
    
    # find the corresponding cuid
    if(!is.na(CU_NAME)){
      
      cond_PSE <- grepl(simplify_string_fun(CU_NAME),simplify_string_fun(nuseds_cuid_streamid$cu_name_dfo)) &
        !is.na( nuseds_cuid_streamid$cu_name_dfo) &
        nuseds_cuid_streamid$SPECIES_QUALIFIED == species_abbr
      
      # any(cond_PSE)
      # nuseds_cuid_streamid[cond_PSE,]
      
      if(any(cond_PSE)){
        
        cuid <- unique(nuseds_cuid_streamid$cuid[cond_PSE])
        
        if(length(cuid) > 1){
          print("GFE_ID is in PSE but multiple cuid are found with this species. To trouble shoot.")
          break
          
        }else{
          SFU_pop$cuid[r] <- cuid
          SFU_pop$cu_name_pse[r] <- unique(nuseds_cuid_streamid$cu_name_pse[cond_PSE])
          SFU_pop$GFE_ID[r] <- SFU_stream$GFE_ID[cond_SFU_stream]
          Done <- T
        }
        
      }else{
        print("CU_NAME found but it is not in nuseds_cuid_streamid$cu_name_dfo. (1)")
        break
        
      }
    }
  }
  
  if(!Done){
    print("Still not done")
    break
  }
}

# View(SFU_pop)

# check if all populations now have a cuid:
sum(is.na(SFU_pop$cuid))

# Fill SFU_escap

for(r in 1:nrow(SFU_pop)){
  # r <- 11
  cond <- SFU_escap$Stream_Name == SFU_pop$Stream_Name[r] &
    SFU_escap$species_abbr == SFU_pop$species_abbr[r]
  SFU_escap$cuid[cond] <- SFU_pop$cuid[r]
  SFU_escap$GFE_ID[cond] <- SFU_pop$GFE_ID[r]
  SFU_escap$cu_name_pse[cond] <- SFU_pop$cu_name_pse[r]
}

# View(SFU_escap)
unique(SFU_escap$GFE_ID)

#
#
# Add methods and survey quality fields to SFU_escap ----
# cf. email from Kate ONeill from June 10 2024.

SFU_escap$Reynold_Lab_Final_Number <- as.numeric(SFU_escap$Reynold_Lab_Final_Number)

# replace NA by "NA" in Data_Source_of_Final_Number
unique(SFU_escap$Data_Source_of_Final_Number)
cond <- is.na(SFU_escap$Data_Source_of_Final_Number)
SFU_escap$Data_Source_of_Final_Number[cond] <- "NA"

SFU_escap$ESTIMATE_METHOD <- NA
SFU_escap$ESTIMATE_CLASSIFICATION <- NA
SFU_escap$stream_survey_quality <- NA   # PSF field 

#' Procedure:
#' - look for each row if the data points is from DFO
#'    - if yes: check that the values match NuSEDS
#'        - if not: report it.
#'        - if yes, edit ESTIMATE_METHOD and ESTIMATE_CLASSIFICATION
#'    - if not DFO source, edit ESTIMATE_METHOD and ESTIMATE_CLASSIFICATION using 
#'      Data_Source_of_Final_Number

col <- c("ESTIMATE_METHOD","ESTIMATE_CLASSIFICATION","stream_survey_quality")
cond <- !is.na(nuseds_cuid_streamid$MAX_ESTIMATE)
methods_classification_nuseds <- unique(nuseds_cuid_streamid[cond,col])
methods_classification_nuseds

issues_df <- data.frame()
for(i in 1:nrow(SFU_escap)){
  # i <- 1
  # SFU_escap[i,]
  cuid <- SFU_escap$cuid[i]
  GFE_ID <- SFU_escap$GFE_ID[i]
  Species <- SFU_escap$Species[i]
  Year <- SFU_escap$Year[i]
  Reynold_Lab_Final_Number <- SFU_escap$Reynold_Lab_Final_Number[i]
  Data_Source_of_Final_Number <- SFU_escap$Data_Source_of_Final_Number[i]
  
  
  if(grepl("DFO",Data_Source_of_Final_Number) & is.na(GFE_ID)){
    
    #' This should not happen because all locations in NuSEDS have a GFE_ID.
    #' There are two cases with Codville Lagoon not in NuSEDS. This is probably
    #' a typo in Data_Source_of_Final_Number for 2011 because it is "SFU" for all
    #' other years.
    
    issue_new <- SFU_escap[i,gsub(" ","_",SFU_escap_colnames)]
    issue_new$GFE_ID <- GFE_ID
    issue_new$issue <- "Locations not present in NuSEDS"
    issues_df <- rbind(issues_df,
                       issue_new)
    
    ESTIMATE_CLASSIFICATION <- "issue"
    ESTIMATE_METHOD <- "issue"
    stream_survey_quality <- "issue"
    
  }else if(grepl("DFO",Data_Source_of_Final_Number)){
    
    cond_nuseds <- nuseds_cuid_streamid$cuid == cuid & !is.na(nuseds_cuid_streamid$cuid) &
      nuseds_cuid_streamid$GFE_ID == GFE_ID &
      nuseds_cuid_streamid$Year == Year
    
    if(any(cond_nuseds)){
      MAX_ESTIMATE <- nuseds_cuid_streamid$MAX_ESTIMATE[cond_nuseds]
      
      if(is.na(MAX_ESTIMATE)){ # then there is an issue
        
        issue_new <- SFU_escap[i,gsub(" ","_",SFU_escap_colnames)]
        issue_new$GFE_ID <- GFE_ID
        issue_new$issue <- "Count is NA in orginal NuSEDS data"
        issues_df <- rbind(issues_df,
                           issue_new)
        
        ESTIMATE_CLASSIFICATION <- "issue"
        ESTIMATE_METHOD <- "issue"
        stream_survey_quality <- "issue"
        
      }else{ # if MAX_ESTIMATE is not NA
        
        if(Reynold_Lab_Final_Number == MAX_ESTIMATE){ # all good
          
          ESTIMATE_METHOD <- nuseds_cuid_streamid$ESTIMATE_METHOD[cond_nuseds]
          ESTIMATE_CLASSIFICATION <- nuseds_cuid_streamid$ESTIMATE_CLASSIFICATION[cond_nuseds]
          
          cond <- methods_classification_nuseds$ESTIMATE_CLASSIFICATION == ESTIMATE_CLASSIFICATION
          stream_survey_quality <- methods_classification_nuseds$stream_survey_quality[cond] |> unique()
          
        }else{
          
          issue_new <- SFU_escap[i,gsub(" ","_",SFU_escap_colnames)]
          issue_new$GFE_ID <- GFE_ID
          issue_new$issue <- paste0("Count is ",MAX_ESTIMATE," in orginal NuSEDS data")
          issues_df <- rbind(issues_df,
                             issue_new)
          
          ESTIMATE_CLASSIFICATION <- "issue"
          ESTIMATE_METHOD <- "issue"
          stream_survey_quality <- "issue"
        }
      }
    }
  }else if(Data_Source_of_Final_Number == "NA"){ # look if the number match DFO's 
    # if yes --> get the method and quality info, if not do nothing.
    
    cond_nuseds <- nuseds_cuid_streamid$cuid == cuid & !is.na(nuseds_cuid_streamid$cuid) &
      nuseds_cuid_streamid$GFE_ID == GFE_ID &
      nuseds_cuid_streamid$Year == Year
    
    MAX_ESTIMATE <- nuseds_cuid_streamid$MAX_ESTIMATE[cond_nuseds]
    
    if(!is.na(MAX_ESTIMATE)[1] & MAX_ESTIMATE[1] == Reynold_Lab_Final_Number){
      
      SFU_escap$Data_Source_of_Final_Number[i] <- "DFO (correction from PSF)"
      ESTIMATE_METHOD <- nuseds_cuid_streamid$ESTIMATE_METHOD[cond_nuseds]
      ESTIMATE_CLASSIFICATION <- nuseds_cuid_streamid$ESTIMATE_CLASSIFICATION[cond_nuseds]
      
      cond <- methods_classification_nuseds$ESTIMATE_CLASSIFICATION == ESTIMATE_CLASSIFICATION
      stream_survey_quality <- methods_classification_nuseds$stream_survey_quality[cond] |> unique()
      
    }else{
      
      issue_new <- SFU_escap[i,gsub(" ","_",SFU_escap_colnames)]
      issue_new$GFE_ID <- GFE_ID
      issue_new$issue <- paste0("Data_Source_of_Final_Number is NA and Reynold_Lab_Final_Number does not match NuSEDS")
      issues_df <- rbind(issues_df,
                         issue_new)
      
      ESTIMATE_CLASSIFICATION <- "issue"
      ESTIMATE_METHOD <- "issue"
      stream_survey_quality <- "issue"
    }
    
  }else{ # source is not DFO and not NA --> edit the dataset...
    
    if(grepl("Peak",Data_Source_of_Final_Number)){
      ESTIMATE_METHOD <- "Peak Live + Dead"
    }else if(grepl("AUC",Data_Source_of_Final_Number)){
      ESTIMATE_METHOD <- "Area Under the Curve"
    }else if(Data_Source_of_Final_Number == "NA"){
      ESTIMATE_METHOD <- NA
    }else{
      print("Other value to add - break")
      break
    }
    
    # cond <- methods_classification_nuseds$ESTIMATE_METHOD == ESTIMATE_METHOD
    # ESTIMATE_CLASSIFICATION <- methods_classification_nuseds$ESTIMATE_CLASSIFICATION[cond] |> unique()
    # nuseds_fields_definitions$all_areas_nuseds$ESTIMATE_CLASSIFICATION
    
    # cond <- methods_classification_nuseds$ESTIMATE_METHOD == "Peak Live + Dead"
    # cond <- methods_classification_nuseds$ESTIMATE_METHOD == "Area Under the Curve"
    # methods_classification_nuseds$ESTIMATE_CLASSIFICATION[cond] |> unique()
    
    ESTIMATE_CLASSIFICATION <-  "RELATIVE ABUNDANCE (TYPE-4)" # to be conservative
    
    cond <- methods_classification_nuseds$ESTIMATE_CLASSIFICATION == ESTIMATE_CLASSIFICATION
    stream_survey_quality <- methods_classification_nuseds$stream_survey_quality[cond] |> unique()
    
  }
  
  SFU_escap$ESTIMATE_METHOD[i] <- ESTIMATE_METHOD
  SFU_escap$ESTIMATE_CLASSIFICATION[i] <- ESTIMATE_CLASSIFICATION
  SFU_escap$stream_survey_quality[i] <- stream_survey_quality
}

SFU_escap[i,]

issues_df
nrow(issues_df) # 29
unique(SFU_escap$Data_Source_of_Final_Number)

# check that there is no NA in ESTIMATE_METHOD:
SFU_escap$ESTIMATE_METHOD |> unique()

unique(SFU_escap$GFE_ID)

# export these issues and notify Arianne
write.csv(issues_df,paste0(wd_output,"/archive/SFU_Escapement_issues.csv"), row.names = F)


# Checks on the issues:
# Issue i = ??? : I do not know where the 200 comes from
cond <- (grepl("ADA ",all_areas_nuseds$WATERBODY) | grepl("COOPER ",all_areas_nuseds$WATERBODY)) & 
  all_areas_nuseds$ANALYSIS_YR == 2008 & 
  all_areas_nuseds$SPECIES == "Chum"
all_areas_nuseds[cond,c("WATERBODY",var_in_MAX_ESTIMATE,"MAX_ESTIMATE")]
all_areas_nuseds[cond,]

cond <- (grepl("ADA ",all_areas_nuseds$WATERBODY) | grepl("COOPER ",all_areas_nuseds$WATERBODY)) & 
  all_areas_nuseds$ANALYSIS_YR == 2008 & 
  all_areas_nuseds$SPECIES == "Pink"
all_areas_nuseds[cond,c("WATERBODY",var_in_MAX_ESTIMATE,"MAX_ESTIMATE")]

cond <- grepl("scribner",simplify_string_fun(conservationunits_decoder$cu_name_pse)) # ?!
conservationunits_decoder$cu_name_dfo[cond]
cond <- grepl("beales",simplify_string_fun(all_areas_nuseds$WATERBODY)) & 
  all_areas_nuseds$ANALYSIS_YR %in% c(2007,2008,2020) & 
  all_areas_nuseds$SPECIES == "Chum"
all_areas_nuseds[cond,c("WATERBODY",var_in_MAX_ESTIMATE,"MAX_ESTIMATE")]

cond <- grepl("beales",simplify_string_fun(all_areas_nuseds$WATERBODY)) & 
  all_areas_nuseds$ANALYSIS_YR %in% c(2007,2008) & 
  all_areas_nuseds$SPECIES == "Pink"
all_areas_nuseds[cond,c("WATERBODY",var_in_MAX_ESTIMATE,"MAX_ESTIMATE")]

cond <- grepl("bullockchannel#2",simplify_string_fun(all_areas_nuseds$WATERBODY)) & 
  all_areas_nuseds$ANALYSIS_YR %in% c(2007,2008) & 
  all_areas_nuseds$SPECIES == "Chum"
all_areas_nuseds[cond,c("WATERBODY","POP_ID",var_in_MAX_ESTIMATE,"MAX_ESTIMATE")]

cond <- grepl("bullockchannel#2",simplify_string_fun(all_areas_nuseds$WATERBODY)) & 
  all_areas_nuseds$ANALYSIS_YR %in% c(2007,2008) & 
  all_areas_nuseds$SPECIES == "Pink"
all_areas_nuseds[cond,c("WATERBODY",var_in_MAX_ESTIMATE,"MAX_ESTIMATE")]

cond <- grepl("rainbow",simplify_string_fun(all_areas_nuseds$WATERBODY)) & 
  all_areas_nuseds$ANALYSIS_YR %in% c(2009) & 
  all_areas_nuseds$SPECIES == "Chum"
all_areas_nuseds[cond,c("WATERBODY",var_in_MAX_ESTIMATE,"MAX_ESTIMATE")]
all_areas_nuseds[cond,]

cond <- grepl("COOPER INLET",all_areas_nuseds$WATERBODY) & 
  all_areas_nuseds$ANALYSIS_YR == 2008 & 
  all_areas_nuseds$SPECIES == "Chum"

#
# Create nuseds_cuid_streamid_Reynolds_lab_DATE.csv -----
#' The goal is to create a dataframe with the same structure as 
#' nuseds_cuid_streamid but with the Reynolds' lab data.
#' The Reynolds' Lab data will be combined in 4_datasets_for_PSE.R. Here we only 
#' add the data to nuseds_cuid_streamid_add and display the different with the
#' NuSEDS data.

# return the NuSEDS field names
nuseds_fields <- fields_IndexId_GFE_ID_fun(conservation_unit_system_sites = nuseds_cuid_streamid)

# Fields related to the populations:
fields_pop <- c(nuseds_fields$NUSEDS$IndexId,nuseds_fields$CUSS$IndexId) |> unique()
fields_pop <- fields_pop[fields_pop %in% colnames(nuseds_cuid_streamid)]

# Fields related to the location:
fields_gfeid <- c(nuseds_fields$NUSEDS$GFE_ID,nuseds_fields$CUSS$GFE_ID) |> unique()
fields_gfeid <- fields_gfeid[fields_gfeid %in% colnames(nuseds_cuid_streamid)]


#'* 1) Time series with the new locations *
nuseds_cuid_streamid_add <- data.frame()

cond <- is.na(SFU_pop$GFE_ID)
SFU_pop_na <- SFU_pop[cond,]
Stream_Names <- unique(SFU_pop_na$Stream_Name)
for(i in 1:length(Stream_Names)){
  # i <- 1
  Stream_Name <- Stream_Names[i]
  
  # get the cuid for this location
  cond <- SFU_pop_na$Stream_Name == Stream_Name
  cuids <- unique(SFU_pop_na$cuid[cond])
  
  layout(matrix(1:length(cuids), nrow = 1, byrow = F))
  for(j in 1:length(cuids)){
    # j <- 1
    cuid <- cuids[j]
    cond_sfu <-  SFU_escap$Stream_Name == Stream_Name &
      SFU_escap$cuid == cuid
    
    yr_sfu <- range(SFU_escap$Year[cond_sfu])
    y_sfu <- range(SFU_escap$Reynold_Lab_Final_Number[cond_sfu], na.rm = T)

    data_sfu <- SFU_escap[cond_sfu,]
    data_sfu <- data_sfu[order(data_sfu$Year),]
    
    par(mar = c(4.5,4.5,.5,.5))
    plot(NA, xlab = "Year", ylab = "Spawner abundance",
         xlim = c(min(yr_sfu, na.rm = T),max(yr_sfu, na.rm = T)),
         ylim = c(0,max(y_sfu, na.rm = T)))
    points(x = data_sfu$Year, y = data_sfu$Reynold_Lab_Final_Number, 
           type = "o", lwd = 2, col = "firebrick3")
    legend("topright",c("SFU"), col = c("firebrick3"), lwd = 2, pch = 1, bty = 'n')
    legend("top",c(unique(data_sfu$Stream_Name),unique(data_sfu$species_abbr)),bty = 'n')
    
    # create an empty nuseds_cuid_streamid_add
    nuseds_cuid_streamid_new <- nuseds_cuid_streamid[1:nrow(data_sfu),]
    for(c in colnames(nuseds_cuid_streamid_new)){
      nuseds_cuid_streamid_new[,c] <- NA
    }
    
    #* Fill CU-related info from nuseds_cuid_streamid *
    cond_cuid <- nuseds_cuid_streamid$cuid == cuid & !is.na(nuseds_cuid_streamid$cuid)
    col <- c("region","regionid",fields_pop,"cuid","cu_name_pse","cu_name_dfo")
    # remove the fields associated to populations
    col <- col[! col %in% c("POP_ID","IndexId","MAZ_ACRO","FAZ_ACRO","JAZ_ACRO",
                            "IS_INDICATOR")]
    for(c in col){
      nuseds_cuid_streamid_new[,c] <- unique(nuseds_cuid_streamid[cond_cuid,c])
    }
    
    #* Fill location-related info from SFU_stream *
    nuseds_cuid_streamid_new$GFE_ID <- NA
    nuseds_cuid_streamid_new$sys_nm <- Stream_Name
    nuseds_cuid_streamid_new$sys_nm_final <- Stream_Name
    nuseds_cuid_streamid_new$location_new <- T

    Stream_Name_2 <- Stream_Name
    # if(Stream_Name == "Goat Bushu Creek"){
    #   Stream_Name_2 <- "Goat Bushu"
    # }else if(Stream_Name == "Kunsoot Main"){
    #   Stream_Name_2 <- "Kunsoot"
    # }
    if(Stream_Name == "Mosquito (Left and Right)"){ # note it is "Left" and not "left"
      Stream_Name_2 <- "Mosquito (left and Right)"  # same coordinates for left and right so just pick one
    }
    cond_sfu_stream <- SFU_stream$SFU_Stream_Name == Stream_Name_2
    if(!any(cond_sfu_stream)){
      print("Stream_Name_2 not matching SFU_stream$SFU_Stream_Name. To check for typos")
      break
    }
    nuseds_cuid_streamid_new$latitude <- SFU_stream$Latitude_2[cond_sfu_stream]
    nuseds_cuid_streamid_new$longitude <- SFU_stream$Longitude_2[cond_sfu_stream]
    nuseds_cuid_streamid_new$latitude_final <- SFU_stream$Latitude_2[cond_sfu_stream]
    nuseds_cuid_streamid_new$longitude_final <- SFU_stream$Longitude_2[cond_sfu_stream]
    
    #* Fill the abundance-related data from SFU_escap (data_sfu) *
    nuseds_cuid_streamid_new$Year <- data_sfu$Year
    nuseds_cuid_streamid_new$MAX_ESTIMATE <- data_sfu$Reynold_Lab_Final_Number
    
    #* Fill the methods manually *
    nuseds_cuid_streamid_new$ESTIMATE_CLASSIFICATION <- data_sfu$ESTIMATE_CLASSIFICATION
    nuseds_cuid_streamid_new$ESTIMATE_METHOD <- data_sfu$ESTIMATE_METHOD
    nuseds_cuid_streamid_new$stream_survey_quality <- data_sfu$stream_survey_quality
    
    if(nrow(nuseds_cuid_streamid_add) == 0){
      nuseds_cuid_streamid_add <- nuseds_cuid_streamid_new
    }else{
      nuseds_cuid_streamid_add <- rbind(nuseds_cuid_streamid_add,
                                        nuseds_cuid_streamid_new)
    }
  }
}

# View(nuseds_cuid_streamid_add)

nuseds_cuid_streamid_add$comment <- "New location (hence the absence of GFE_ID)"

#'* 2) Add the time series with existing locations *

nuseds_cuid_streamid_add2 <- data.frame()

GFE_IDs <- unique(SFU_pop$GFE_ID)
GFE_IDs <- GFE_IDs[!is.na(GFE_IDs)]

for(i in 1:length(GFE_IDs)){
  # i <- 6
  GFE_ID <- GFE_IDs[i]
  
  # get the cuid of the CUs present in this GFE_ID
  cond_GFE_ID <- SFU_escap$GFE_ID == GFE_ID & !is.na(SFU_escap$GFE_ID)
  cuids <- unique(SFU_escap$cuid[cond_GFE_ID])
  
  layout(matrix(1:length(cuids), nrow = 1, byrow = F))
  for(j in 1:length(cuids)){
    # j <- 1
    cuid <- cuids[j]
    cond_sfu <-  cond_GFE_ID & SFU_escap$cuid == cuid
    cond_pse <-  nuseds_cuid_streamid$GFE_ID == GFE_ID & 
      nuseds_cuid_streamid$cuid == cuid & !is.na( nuseds_cuid_streamid$cuid)
    
    yr_sfu <- range(SFU_escap$Year[cond_sfu])
    yr_pse <- range(nuseds_cuid_streamid$Year[cond_pse])
    yr_pse[is.infinite(yr_pse)] <- NA
    
    y_sfu <- range(SFU_escap$Reynold_Lab_Final_Number[cond_sfu], na.rm = T)
    y_pse <- range(nuseds_cuid_streamid$MAX_ESTIMATE[cond_pse], na.rm = T)
    y_pse[is.infinite(y_pse)] <- NA
    
    data_sfu <- SFU_escap[cond_sfu,]
    data_sfu <- data_sfu[order(data_sfu$Year),]
    
    data_pse <- nuseds_cuid_streamid[cond_pse,]
    data_pse <- data_pse[order(data_pse$Year),]
    
    par(mar = c(.5,4.5,.5,.5))
    plot(NA, xlab = "", ylab = "Spawner abundance",
         xlim = c(min(yr_sfu,yr_pse, na.rm = T),max(yr_sfu,yr_pse, na.rm = T)),
         ylim = c(0,max(y_sfu,y_pse, na.rm = T)), xaxt = 'n')
    points(x = data_sfu$Year, y = data_sfu$Reynold_Lab_Final_Number, 
           type = "o", lwd = 2, col = "firebrick3")
    points(x = data_pse$Year, y = data_pse$MAX_ESTIMATE, 
           type = "o", lwd = 2, col = "dodgerblue3")
    legend("topright",c("SFU","NuSEDS"), col = c("firebrick3","dodgerblue3"), 
           lwd = 2, pch = 1, bty = 'n')
    legend("top",c(unique(data_sfu$Stream_Name),unique(data_sfu$Species)),bty = 'n')
    
    #
    # create an empty nuseds_cuid_streamid_add2
    nuseds_cuid_streamid_new <- nuseds_cuid_streamid[1:nrow(data_sfu),]
    for(c in colnames(nuseds_cuid_streamid_new)){
      nuseds_cuid_streamid_new[,c] <- NA
    }
    
    #* Fill CU-related info from nuseds_cuid_streamid *
    cond_cuid <- nuseds_cuid_streamid$cuid == cuid & !is.na(nuseds_cuid_streamid$cuid)
    col <- c("region","regionid",fields_pop,"cuid","cu_name_pse","cu_name_dfo")
    col <- col[! col %in% c("POP_ID","IndexId","MAZ_ACRO","FAZ_ACRO","JAZ_ACRO",
                            "IS_INDICATOR")]
    # remove the fields associated to populations
    col <- col[! col %in% c("POP_ID","IndexId","MAZ_ACRO","FAZ_ACRO","JAZ_ACRO",
                            "IS_INDICATOR")]
    for(c in col){
      nuseds_cuid_streamid_new[,c] <- unique(nuseds_cuid_streamid[cond_cuid,c])
    }
    
    #* Fill location-related info from SFU_stream *
    cond_gfeid <- nuseds_cuid_streamid$GFE_ID == GFE_ID
    if(any(cond_gfeid)){ # if GFE_ID is in PSE
      nuseds_cuid_streamid_new$location_new <- F
      col <- c(fields_gfeid,"sys_nm","sys_nm_final")
      # col <- col[!col %in% c("AREA")]
      for(c in col){
        nuseds_cuid_streamid_new[,c] <- unique(nuseds_cuid_streamid[cond_gfeid,c])
      }
    }else{ # if GFE_ID is not in PSE (i.e. new location but present in all_streams_DFO)
      cond_gfeid <- all_streams_DFO$ID == GFE_ID
      nuseds_cuid_streamid_new$location_new <- T

      nuseds_cuid_streamid_new$AREA <- all_streams_DFO$AREA[cond_gfeid]
      nuseds_cuid_streamid_new$GFE_ID <- all_streams_DFO$ID[cond_gfeid]
      nuseds_cuid_streamid_new$sys_nm_final <- all_streams_DFO$NME[cond_gfeid]
      nuseds_cuid_streamid_new$X_LONGT <- all_streams_DFO$X_LONGT[cond_gfeid]
      nuseds_cuid_streamid_new$Y_LAT <- all_streams_DFO$Y_LAT[cond_gfeid]
    }
    # use their coordinates
    Stream_Name_2 <- Stream_Name
    # if(Stream_Name == "Goat Bushu Creek"){
    #   Stream_Name_2 <- "Goat Bushu"
    # }else if(Stream_Name == "Kunsoot Main"){
    #   Stream_Name_2 <- "Kunsoot"
    # }
    if(Stream_Name == "Mosquito (Left and Right)"){ # note it is "Left" and not "left"
      Stream_Name_2 <- "Mosquito (left and Right)"  # same coordinates for left and right so just pick one
    }
    cond_sfu_stream <- SFU_stream$SFU_Stream_Name == Stream_Name_2
    if(!any(cond_sfu_stream)){
      print("Stream_Name_2 not matching SFU_stream$SFU_Stream_Name. To check for typos")
      break
    }
    nuseds_cuid_streamid_new$latitude <- SFU_stream$Latitude_2[cond_sfu_stream]
    nuseds_cuid_streamid_new$longitude <- SFU_stream$Longitude_2[cond_sfu_stream]
    nuseds_cuid_streamid_new$latitude_final <- SFU_stream$Latitude_2[cond_sfu_stream]
    nuseds_cuid_streamid_new$longitude_final <- SFU_stream$Longitude_2[cond_sfu_stream]

    #* Fill the abundance-related data from SFU_escap (data_sfu) *
    nuseds_cuid_streamid_new$Year <- data_sfu$Year
    nuseds_cuid_streamid_new$MAX_ESTIMATE <- data_sfu$Reynold_Lab_Final_Number
    
    #* Fill the methods *
    nuseds_cuid_streamid_new$ESTIMATE_CLASSIFICATION <- data_sfu$ESTIMATE_CLASSIFICATION
    nuseds_cuid_streamid_new$ESTIMATE_METHOD <- data_sfu$ESTIMATE_METHOD
    nuseds_cuid_streamid_new$stream_survey_quality <- data_sfu$stream_survey_quality
    
    if(nrow(nuseds_cuid_streamid_add2) == 0){
      nuseds_cuid_streamid_add2 <- nuseds_cuid_streamid_new
    }else{
      nuseds_cuid_streamid_add2 <- rbind(nuseds_cuid_streamid_add2,
                                        nuseds_cuid_streamid_new)
    }
  }
}

nuseds_cuid_streamid_add2$comment <- NA

# View(nuseds_cuid_streamid_add2)

sum(is.na(nuseds_cuid_streamid_add$sys_nm_final))

#
# Write CSF file data_extra_Reynolds_lab_DATE.csv -------
#

nuseds_cuid_streamid_add <- rbind(nuseds_cuid_streamid_add,
                                  nuseds_cuid_streamid_add2)

# View(nuseds_cuid_streamid_add)

date <- Sys.Date()
write.csv(nuseds_cuid_streamid_add,
          paste0(wd_output,"/archive/3_data_extra_Reynolds_lab_",date,".csv"),
          row.names = F)


