
#'******************************************************************************
#' The goal of the script is to combine the cleaned NuSEDS data with the cleaned
#' data sent by other labs.
#'  
#' sent by Arianne Nickels from the Reynolds' Lab (SFU).
#' 
#' Files imported:
#' - nuseds_cuid_streamid_nuseds_20240419.csv : the cleaned NuSEDS data with PSE cuid and streamid (and with data no in PSE)
#' - data_extra_Reynolds_lab_DATE.csv : the cleaned  Reynolds' Lab (SFU) data (sent by  Arianne Nickels)
#' - SFU_Escapement_issues.csv        : 
#' 
#' Files produced: 
#' - dataset_1part2_DATE.csv
#' 
#'******************************************************************************

NOTE: THE REYNOLD LAB DATA HAS NOT BEEN ADDED YET AS WE WAIT FOR AFTER THE PSE 2.0 LAUNCH 1ST

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

# Import files -------

#'* Import the cleaned NuSEDS data matched with the cuid and streamid of the PSE *
nuseds_cuid_streamid <- import_mostRecent_file_fun(wd = wd_output, 
                                                   pattern = "nuseds_cuid_streamid")

#'* Import the cleaned Reynolds Lab cleaned data *
Reynolds_data <- import_mostRecent_file_fun(wd = wd_output, 
                                                   pattern = "data_extra_Reynolds_lab")


# check columns names:
colnames(nuseds_cuid_streamid)[!colnames(nuseds_cuid_streamid) %in% colnames(Reynolds_data)]
colnames(Reynolds_data)[!colnames(Reynolds_data) %in% colnames(nuseds_cuid_streamid)]

# add the source_data field:
nuseds_cuid_streamid$source <- "NuSEDS"
Reynolds_data$source <- "Reynolds lab"

#'* Import the reference to data points with issues in the Reynolds Lab dataset *
Reynolds_issues <- import_mostRecent_file_fun(wd = wd_output, 
                                            pattern = "SFU_Escapement_issues")


#'* Import the conservationunits_decoder.csv *
datasetsNames_database <- datasetsNames_database_fun()

#' Import the cuspawnerabundance.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' To calculating current spawner abundance for biostatus assessment
fromDatabase <- update_file_csv <- F

#' Import the conservationunits_decoder.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)


#
# Combine nuseds_cuid_streamid with data_extra_Reynolds_lab (WAIT UNTIL AFTER PSE 2.0 RELEASE) -----
#' - All populations were attributed a cuid (in data_extra_Reynolds_lab.R), 
#' - several locations were new and do not have a GFE_ID.

# save the original nuseds_cuid_streamid and edit the other one
nuseds_cuid_streamid_original <- nuseds_cuid_streamid
nrow(nuseds_cuid_streamid) # 306823

# nuseds_cuid_streamid <- nuseds_cuid_streamid_original

#'* Deal with data points with issues *
#' These issue consern data points with "DFO" in the Data_Source_of_Final_Number
#' in the orginal excel file: the number either do not match our numbers or the 
#' data source is NAs or there is no data in NuSEDS. 
#' ACTION: rescue the two data points for which is was clearly a mistake to put
#' "DFO" instead of "SFU"; discard the rest.
cond <- Reynolds_issues$issue == "Locations not present in NuSEDS"
pointsToRescue <- Reynolds_issues[cond,c("Stream_Name","Year","Species")]
for(r in 1:nrow(pointsToRescue)){
  # r <- 2
  cond <- Reynolds_data$sys_nm_final == pointsToRescue$Stream_Name[r] &
    Reynolds_data$SPECIES == pointsToRescue$Species[r] &
    Reynolds_data$Year == pointsToRescue$Year[r]
  
  # the values for the fields to correct in the other years:
  cond2 <- Reynolds_data$sys_nm_final == pointsToRescue$Stream_Name[r] &
    Reynolds_data$SPECIES == pointsToRescue$Species[r] &
    Reynolds_data$ESTIMATE_METHOD != "issue"
  
  Reynolds_data$ESTIMATE_METHOD[cond] <- Reynolds_data$ESTIMATE_METHOD[cond2] |> unique()
  Reynolds_data$ESTIMATE_CLASSIFICATION[cond] <- Reynolds_data$ESTIMATE_CLASSIFICATION[cond2] |> unique()
  Reynolds_data$stream_survey_quality[cond] <- Reynolds_data$stream_survey_quality[cond2] |> unique()
}

# SFU_escap <- read_excel(paste(wd_data_dropbox,"SFU_Escapement_PSF.xlsx",sep="/"), 
#                         sheet = 4) |>
#   as.data.frame()
# cond <- SFU_escap$`Stream Name` == "Codville"
# data <- SFU_escap[cond,]
# data <- data |> arrange(Species,Year)

# Remove the other data points
cond <- Reynolds_data$ESTIMATE_METHOD == "issue"
Reynolds_data[cond,]
Reynolds_data <- Reynolds_data[!cond,]

nrow(Reynolds_data) # 638


#'* Time series in new locations *
#' Checks:
cond_gfeid_na <- is.na(Reynolds_data$GFE_ID)
sum(cond_gfeid_na) # 170
cond_location_new <- Reynolds_data$location_new
sum(cond_location_new) # 258
sum(cond_gfeid_na & cond_location_new) # 170 as it should be
sum(cond_gfeid_na & !cond_location_new) # 0 as it should be
sum(!cond_gfeid_na & cond_location_new) # 88 --> new time series of a cuid in a existing location

# Retain the time series:
Reynolds_data_location_new <- Reynolds_data[cond_location_new,]
Reynolds_data_location_new <- Reynolds_data_location_new[,colnames(Reynolds_data_location_new) != "comment"]
nrow(Reynolds_data_location_new) # 258
unique(Reynolds_data_location_new$ESTIMATE_METHOD)

# add Reynolds_data_location_new to nuseds_cuid_streamid
nuseds_cuid_streamid <- rbind(nuseds_cuid_streamid,
                              Reynolds_data_location_new)

nrow(nuseds_cuid_streamid) # 307081

#'* Time series in existing locations *
#' In case of data points conflicts between the NuSEDS vs. Reynolds Lab data, keep
#' the latter because it is probably more accurate.
Reynolds_data_location_exist <- Reynolds_data[!cond_location_new,]
Reynolds_data_location_exist <- Reynolds_data_location_exist[,colnames(Reynolds_data_location_exist) != "comment"]
nrow(Reynolds_data_location_exist) # 380

#
GFE_IDs <- unique(Reynolds_data_location_exist$GFE_ID)
for(i in 1:length(GFE_IDs)){
  # i <- 12
  GFE_ID <- GFE_IDs[i]
  
  # get the cuid of the CUs present in this GFE_ID
  cond_Reynolds_gfeid <- Reynolds_data_location_exist$GFE_ID == GFE_ID
  cuids <- unique(Reynolds_data_location_exist$cuid[cond_Reynolds_gfeid])
  
  layout(matrix(1:(length(cuids)*2), nrow = length(cuids), byrow = T))
  for(j in 1:length(cuids)){
    # j <- 2
    cuid <- cuids[j]
    
    cond_Reynolds_gfeid_cuid <-  cond_Reynolds_gfeid & 
      Reynolds_data_location_exist$cuid == cuid
    
    cond_pse_gfeid_cuid <-  nuseds_cuid_streamid$GFE_ID == GFE_ID & 
      !is.na(nuseds_cuid_streamid$GFE_ID) &
      nuseds_cuid_streamid$cuid == cuid & 
      !is.na(nuseds_cuid_streamid$cuid) &
      !is.na( nuseds_cuid_streamid$cuid)
    
    yr_Reynolds <- range(Reynolds_data_location_exist$Year[cond_Reynolds_gfeid_cuid])
    yr_pse <- range(nuseds_cuid_streamid$Year[cond_pse_gfeid_cuid])
    yr_pse[is.infinite(yr_pse)] <- NA
    
    y_Reynolds <- range(Reynolds_data_location_exist$MAX_ESTIMATE[cond_Reynolds_gfeid_cuid], na.rm = T)
    y_pse <- range(nuseds_cuid_streamid$MAX_ESTIMATE[cond_pse_gfeid_cuid], na.rm = T)
    y_pse[is.infinite(y_pse)] <- NA
    
    data_Reynolds <- Reynolds_data_location_exist[cond_Reynolds_gfeid_cuid,]
    data_Reynolds <- data_Reynolds[order(data_Reynolds$Year),]
    
    data_pse <- nuseds_cuid_streamid[cond_pse_gfeid_cuid,]
    data_pse <- data_pse[order(data_pse$Year),]
    
    par(mar = c(4.5,4.5,.5,.5))
    plot(NA, xlab = "", ylab = "Spawner abundance",
         xlim = c(min(yr_Reynolds,yr_pse, na.rm = T),max(yr_Reynolds,yr_pse, na.rm = T)),
         ylim = c(0,max(y_Reynolds,y_pse, na.rm = T)), xaxt = 's')
    points(x = data_Reynolds$Year, y = data_Reynolds$MAX_ESTIMATE, 
           type = "o", lwd = 2, col = "firebrick3", pch = 2)
    points(x = data_pse$Year, y = data_pse$MAX_ESTIMATE, 
           type = "o", lwd = 2, col = "dodgerblue3")
    legend("topright",c("SFU","NuSEDS"), col = c("firebrick3","dodgerblue3"), 
           lwd = 2, pch = c(2,1), bty = 'n')
    legend("top",c(paste0(unique(data_Reynolds$sys_nm)," - ",GFE_ID),
                   paste0(unique(data_Reynolds$SPECIES_QUALIFIED)," - ",cuid)),bty = 'n')
    
    # If entire new time series 
    if(!any(cond_pse_gfeid_cuid)){
      cond <- !is.na(data_Reynolds$MAX_ESTIMATE)
      rows_new <- data_Reynolds[cond,]
      
    }else{ # if series exists in PSE
      
      data_merge <- merge(x = data_Reynolds[,c("Year","MAX_ESTIMATE")],
                          y = data_pse[,c("Year","MAX_ESTIMATE")],
                          by = "Year", all = T)
      
      # Points with NAs in Reynolds --> to remove
      years_toRemove_Reynolds <- data_merge$Year[is.na(data_merge$MAX_ESTIMATE.x)]
      
      # duplicate data points in data_Reynolds --> to remove
      cond <- data_merge$MAX_ESTIMATE.x == data_merge$MAX_ESTIMATE.y &
        !is.na(data_merge$MAX_ESTIMATE.y) &
        !is.na(data_merge$MAX_ESTIMATE.x)
      years_toRemove_Reynolds <- c(years_toRemove_Reynolds,
                                   data_merge$Year[cond])

      # remove the rows in data_Reynolds
      cond <- ! data_Reynolds$Year %in% years_toRemove_Reynolds
      data_Reynolds_cut <- data_Reynolds[cond,]
      if(nrow(data_Reynolds_cut) > 0){
        cond <- ! data_merge$Year %in% years_toRemove_Reynolds
        years_toReplace_pse <- data_merge$Year[cond]
        
        # data_merge[data_merge$Year %in% years_toReplace_pse,]
        
        # remove the rows in nuseds_cuid_streamid
        cond <- cond_pse_gfeid_cuid & nuseds_cuid_streamid$Year %in% years_toReplace_pse
        nuseds_cuid_streamid <- nuseds_cuid_streamid[!cond,]
        
        # add the new rows to nuseds_cuid_streamid
        # edit certain fields for consistency with existing data
        cond <- data_Reynolds$Year %in% years_toReplace_pse
        rows_new <- data_Reynolds[cond,]
        rows_new$longitude_final <- unique(data_pse$longitude_final)
        rows_new$latitude_final <- unique(data_pse$latitude_final)
        if(length(unique(data_pse$longitude_final)) > 1){
          print("More than one coordinate for this location in data_pse so STOPPED :-/")
          break
        }
      }
    }
    # add rows_new to nuseds_cuid_streamid
    nuseds_cuid_streamid <- rbind(nuseds_cuid_streamid,rows_new)
    
    # plot to check
    cond_pse_gfeid_cuid <-  nuseds_cuid_streamid$GFE_ID == GFE_ID & 
      !is.na(nuseds_cuid_streamid$GFE_ID) &
      nuseds_cuid_streamid$cuid == cuid & 
      !is.na(nuseds_cuid_streamid$cuid) &
      !is.na(nuseds_cuid_streamid$cuid)
    
    data_pse <- nuseds_cuid_streamid[cond_pse_gfeid_cuid,]
    data_pse <- data_pse[order(data_pse$Year),]
    
    plot(NA, xlab = "", ylab = "Spawner abundance",
         xlim = c(min(yr_Reynolds,yr_pse, na.rm = T),max(yr_Reynolds,yr_pse, na.rm = T)),
         ylim = c(0,max(y_Reynolds,y_pse, na.rm = T)), xaxt = 's')
    points(x = data_pse$Year, y = data_pse$MAX_ESTIMATE, 
           type = "o", lwd = 2, col = "chartreuse4")
    legend("topright",c("PSE"), col = c("chartreuse4"), 
           lwd = 2, pch = c(1), bty = 'n')
  }
}


WAIT UNTIL AFTER PSE 2.0 RELEASE



#
# Generate dataset_1part2 --------
# example dataset: spawner_surveys_dataset_1part2_2024-03-27.csv
# https://www.dropbox.com/scl/fi/qi5f132o5qc6fzd1hkhhz/spawner_surveys_dataset_1part2_2024-03-27.csv?rlkey=9iymit683c97qew7xo0t59hg9&dl=0

# previous slack thread:
# https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1712944527551599?thread_ts=1712252256.802999&cid=C03LB7KM6JK

# Google doc meeting Katy, Steph and Bruno
# https://docs.google.com/document/d/1Fmsjtb_f9yOaoQ3b1sZUXxCumdO70YuUnMdXjoFY8Xo/edit?usp=sharing

nuseds_final <- import_mostRecent_file_fun(wd = wd_output,
                                           pattern = "nuseds_cuid_streamid")

cond_cuid_na <- is.na(nuseds_final$cuid)
sum(cond_cuid_na) # 1998
cond_pointid_na <- is.na(nuseds_final$pointid)
sum(cond_pointid_na) # 9388 2726
cond_gfe_id_na <- is.na(nuseds_final$GFE_ID)    # 0
sum(cond_gfe_id_na) # 0
cond_streamid_na <- is.na(nuseds_final$streamid)
sum(cond_streamid_na) # 1998
# cond_noNa <- !cond_cuid_na & !cond_pointid_na & !cond_streamid_na
cond_noNa <- cond_cuid_na
dataset_1part2 <- nuseds_final[!cond_noNa,]

# rename sys_nm to stream_name_pse
colnames(dataset_1part2)[colnames(dataset_1part2) == "sys_nm_final"] <- "stream_name_pse"

# edit column names
field_toChange <- c("SPECIES","SPECIES_QUALIFIED","IS_INDICATOR","Year",
                    "MAX_ESTIMATE","ESTIMATE_METHOD","stream_survey_quality")

fields_new <- c("species_name","species_abbr","indicator","year",
                "stream_observed_count","survey_method","survey_quality")

for(i in 1:length(field_toChange)){
  colnames(dataset_1part2)[colnames(dataset_1part2) == field_toChange[i]] <- fields_new[i]
}

# remove NAs in stream_observed_count
dataset_1part2 <- dataset_1part2[!is.na(dataset_1part2$stream_observed_count),]


# select columns

colToKeep <- c("region","species_name","species_abbr","cuid","cu_name_pse",
               "pointid","GFE_ID","streamid",
               "stream_name_pse","indicator","latitude_final","longitude_final","year",
               "stream_observed_count","survey_method","survey_quality","survey_score")

dataset_1part2 <- dataset_1part2[,colToKeep]

colnames(dataset_1part2)[colnames(dataset_1part2) == "latitude_final"] <- "latitude"
colnames(dataset_1part2)[colnames(dataset_1part2) == "longitude_final"] <- "longitude"

# order fields:
dataset_1part2 <- dataset_1part2 %>% 
  arrange(factor(region, levels = c("Yukon","Transboundary","Haida Gwaii","Nass",
                                    "Skeena","Central Coast",
                                    "Vancouver Island & Mainland Inlets",
                                    "Fraser","Columbia")),
          species_name,
          cu_name_pse,
          factor(indicator, levels = c("Y","N","",NA)),
          stream_name_pse,
          year)


dataset_1part2_total <- dataset_1part2

#'* Remove Steelhead * (already removed)
#'# --> already removed in nuseds_data.collation.R
cond <- dataset_1part2$species_name != "Steelhead"
dataset_1part2_SH <-  dataset_1part2[!cond,]
dataset_1part2 <- dataset_1part2[cond,]

#'* Remove TBR, Columbia and Yukon *
cond <- !dataset_1part2$region %in% c("Yukon","Columbia","Transboundary")
dataset_1part2_TBR <-  dataset_1part2[dataset_1part2$region == c("Transboundary"),]
dataset_1part2_YK <-  dataset_1part2[dataset_1part2$region == c("Yukon"),]
dataset_1part2_CB <-  dataset_1part2[dataset_1part2$region == c("Columbia"),]
dataset_1part2 <- dataset_1part2[cond,]

head(dataset_1part2)
sum(is.na(dataset_1part2$streamid)) # 0
sum(is.na(dataset_1part2$cuid)) # 0
sum(is.na(dataset_1part2$pointid)) # 5001  5117

dim(dataset_1part2) 
# 149827 149828     17

head(dataset_1part2)

#
# Add datasets for SH, Columbia, TBR and Yukon -------
#
# https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1723659667266449

wd_TBR <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Transboundary/Data & Assessments/transboundary-data/output") # not -status
TBR_data <- import_mostRecent_file_fun(wd = wd_TBR, pattern = "dataset_1part2")
colnames(TBR_data)[colnames(TBR_data) == "survey_qual"] <- "survey_quality"
#  2024-04-19
head(TBR_data)

wd_SH <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Steelhead/3_Data_Analysis/steelhead-status/output/pse-data")
SH_data <- import_mostRecent_file_fun(wd = wd_SH, pattern = "dataset_1part2")
#  2024-05-02
SH_data <- import_mostRecent_file_fun(wd = wd_pop_indic_data_input_dropbox, 
                                      pattern = "steelhead_dataset_1part2") # TEMPORARY
head(SH_data)
SH_data$region <- sapply(SH_data$CUID,FUN = function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(conservationunits_decoder$region[cond])
})

wd_columbia <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Columbia/data & analysis/analysis/columbia-status/Output")
Columbia_data <- import_mostRecent_file_fun(wd = wd_columbia, pattern = "dataset_1part2")
# 2024-02-12
Columbia_data <- import_mostRecent_file_fun(wd = wd_pop_indic_data_input_dropbox, 
                                            pattern = "columbia_dataset_1part2") # TEMPORARY
head(Columbia_data)
Columbia_data$region <- "Columbia"

wd_yukon <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Yukon/Data & Assessments/yukon-status/output")
Yukon_data <- import_mostRecent_file_fun(wd = wd_yukon, pattern = "dataset_1part2")
# 2022-12-19 
Yukon_data <- import_mostRecent_file_fun(wd = wd_pop_indic_data_input_dropbox, 
                                         pattern = "yukon_dataset_1part2") # TEMPORARY
head(Yukon_data)
Yukon_data$region <- "Yukon"

#' * Import stream level data quality (dataset386_output) *
# datasetsNames_database <- datasetsNames_database_fun()
# 
# fromDatabase <- update_file_csv <- F
# 
# dataset386_output <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[22],
#                                            fromDatabase = fromDatabase,
#                                            update_file_csv = update_file_csv,
#                                            wd = wd_pop_indic_data_input_dropbox)

# TEMPORARY SOLUTION cause the code above does not work
dataset386_output <- import_mostRecent_file_fun(wd = wd_pop_indic_data_input_dropbox,
                                                pattern = "dataset386")

head(dataset386_output)

# Check that streamid is unique for a cuid & stream location
# 1) no streamid can be associated to multiple cuid:
data_check <- dataset386_output[,c("cuid","OLD_streamid")] |> unique()
sum(duplicated(data_check$OLD_streamid)) # 0 OK
# 2) no streamid can be associated to multiple SYSTEM_SITE
data_check <- dataset386_output[!is.na(dataset386_output$SYSTEM_SITE),c("OLD_streamid","SYSTEM_SITE")] |> unique()
sum(duplicated(data_check$OLD_streamid)) # 0 OK


#' * Merge Yukon_data, SH_data and Columbia_data & clean them *
data_merged <- rbind(Yukon_data,SH_data,Columbia_data)
data_merged <- data_merged[,-1]
colnames(data_merged)[colnames(data_merged) == "NuSEDS.counts.by.stream"] <- "stream_observed_count"
colnames(data_merged)[colnames(data_merged) == "CUID"] <- "cuid"
colnames(data_merged)[colnames(data_merged) == "Species"] <- "species_name"
colnames(data_merged)[colnames(data_merged) == "streamname"] <- "stream_name_pse"
data_merged$species_abbr <- sapply(data_merged$cuid, FUN = function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(conservationunits_decoder$species_abbr[cond])
})
data_merged$cu_name_pse <- sapply(data_merged$cuid, FUN = function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(conservationunits_decoder$cu_name_pse[cond])
})


#'* survey_method" & "survey_quality & survey_score *
# Ask Eric
data_merged$survey_method <- NA
data_merged$survey_quality <- NA
data_merged$survey_score <- NA
data_merged$comment <- ""
for(r in 1:nrow(unique(data_merged[,c("OLD_streamid","cuid","year")]))){
  # r <- 1
  # if(any(unique(data_merged[,c("OLD_streamid","cuid","year")])$OLD_streamid[r] == 9755 &
  #        #data_merged$year[r] == 2009 &
  #        unique(data_merged[,c("OLD_streamid","cuid","year")])$cuid[r] == 481)){
  #   break
  # }
  cond_streamid <- dataset386_output$OLD_streamid == unique(data_merged[,c("OLD_streamid","cuid","year")])$OLD_streamid[r]
  cond_cuid <- dataset386_output$cuid == unique(data_merged[,c("OLD_streamid","cuid","year")])$cuid[r]
  cond_year <- dataset386_output$year == unique(data_merged[,c("OLD_streamid","cuid","year")])$year[r]
  
  if(unique(data_merged[,c("OLD_streamid","cuid","year")])$OLD_streamid[r] == 9755){
    cond_cuid <- dataset386_output$cuid == 480 # old cuid for Nass SH Ksi Gingolx (https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1724198353599609?thread_ts=1724089498.307439&cid=CJ5RVHVCG)
  }
  
  cond_total <- cond_streamid & cond_cuid & cond_year
  
  if(any(cond_total)){
    cond <- data_merged$OLD_streamid == unique(data_merged[,c("OLD_streamid","cuid","year")])$OLD_streamid[r] &
      data_merged$cuid == unique(data_merged[,c("OLD_streamid","cuid","year")])$cuid[r] &
      data_merged$year == unique(data_merged[,c("OLD_streamid","cuid","year")])$year[r]
    
    data_merged$survey_method[cond] <- dataset386_output$survey_method[cond_streamid & cond_cuid & cond_year]
    data_merged$survey_quality[cond] <- dataset386_output$survey_qual[cond_streamid & cond_cuid & cond_year]
    data_merged$survey_score[cond] <- dataset386_output$survey_score[cond_streamid & cond_cuid & cond_year]
    
    if(is.na(dataset386_output$survey_method[cond_streamid & cond_cuid & cond_year])){
      data_merged$comment[cond] <- "NA value"
    }
    
  }else{
    comment <- ""
    if(!any(cond_streamid)){
      cond <- data_merged$OLD_streamid == unique(data_merged[,c("OLD_streamid","cuid","year")])$OLD_streamid[r]
      comment <- "streamid missing"
 
    }else if(!any(cond_streamid & cond_year)){
      cond <- data_merged$OLD_streamid == unique(data_merged[,c("OLD_streamid","cuid","year")])$OLD_streamid[r] & 
        data_merged$year == unique(data_merged[,c("OLD_streamid","cuid","year")])$year[r]
      comment <- "year missing, streamid present"
      
    }else if(!any(cond_cuid)){
      cond <- data_merged$OLD_streamid == unique(data_merged[,c("OLD_streamid","cuid","year")])$OLD_streamid[r] & 
        data_merged$year == unique(data_merged[,c("OLD_streamid","cuid","year")])$year[r]
      comment <- "cuid missing, year present, streamid present"
      
    }
    
    data_merged$comment[cond] <- comment
  }
}

# View(data_merged)
sum(is.na(data_merged$survey_score))  # 930
sum(!is.na(data_merged$survey_score)) # 410

# Check if there is any "" comment without values
cond <- data_merged$comment == "" & is.na(data_merged$survey_score)
data_merged[cond,] # no --> all good

# return the region, species, cuid and years 
comments <- data_merged$comment |> unique()
comments <- comments[comments != ""]
for(c in comments){
  cond_c <- data_merged$comment == c
  colHere <- c("region","species_name","cu_name_pse","cuid","OLD_streamid","stream_name_pse")
  if(c %in% c("year missing, streamid present","cuid missing, year present, streamid present")){ # "NA value"
    colHere <- c(colHere,"year")
  }
  out <- data_merged[cond_c,colHere] |> unique()
  print(c)
  print(out)
  print("")
}

# TODO: we just leave NAs for now until Eric eventually get back to me
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1724198353599609?thread_ts=1724089498.307439&cid=CJ5RVHVCG


#'* GFE_ID (The PSE does not care about GFE_ID so far) *
#'# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1724108249417019
# Try to find GFE_ID 
data_merged$GFE_ID <- NA
for(r in 1:nrow(unique(data_merged[,c("region","stream_name_pse")]))){
  region <- unique(data_merged[,c("region","stream_name_pse")])$region[r]
  stream_name_pse <- unique(data_merged[,c("region","stream_name_pse")])$stream_name_pse[r]
  cond_rg <- dataset_1part2_total$region == region
  cond_str <- dataset_1part2_total$stream_name_pse == stream_name_pse
  
  if(!any(cond_rg & cond_str)){
    cond_str <- simplify_string_fun(dataset_1part2_total$stream_name_pse) == simplify_string_fun(stream_name_pse)
  }
  
  if(!any(cond_rg & cond_str)){
    gfeid <- NA
  }else{
    gfeid <- dataset_1part2_total$GFE_ID[cond_rg & cond_str] |> unique()
    if(length(gfeid) > 1){
      print("STOP: More than one GFE_ID")
      break
    }
    data_merged$GFE_ID[r] <- gfeid
  }
}

sum(is.na(data_merged$GFE_ID)) # 1267
sum(!is.na(data_merged$GFE_ID)) # 73


#'* pointid & streamid *
data_merged$pointid <- NA
data_merged$streamid <- data_merged$OLD_streamid


#'* Merge data_merged with TBR_data *
colnames(TBR_data)[!colnames(TBR_data) %in% colnames(data_merged)]
colnames(data_merged)[!colnames(data_merged) %in% colnames(TBR_data)]

# Add species_abbr
TBR_data$species_abbr <- sapply(X = TBR_data$cuid, function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(conservationunits_decoder$species_abbr[cond])
})

# Add GFE_ID, 
TBR_data$GFE_ID <- NA
for(r in 1:nrow(unique(TBR_data[,c("region","stream_name_pse")]))){
  region <- unique(TBR_data[,c("region","stream_name_pse")])$region[r]
  stream_name_pse <- unique(TBR_data[,c("region","stream_name_pse")])$stream_name_pse[r]
  cond_rg <- dataset_1part2_total$region == region
  cond_str <- dataset_1part2_total$stream_name_pse == stream_name_pse
  
  if(!any(cond_rg & cond_str)){
    cond_str <- simplify_string_fun(dataset_1part2_total$stream_name_pse) == simplify_string_fun(stream_name_pse)
  }
  
  if(!any(cond_rg & cond_str)){
    gfeid <- NA
  }else{
    gfeid <- dataset_1part2_total$GFE_ID[cond_rg & cond_str] |> unique()
    if(length(gfeid) > 1){
      print("STOP: More than one GFE_ID")
      break
    }
    TBR_data$GFE_ID[r] <- gfeid
  }
}
sum(is.na(TBR_data$GFE_ID)) # 1522
sum(!is.na(TBR_data$GFE_ID)) # 36
  
# Add pointid
TBR_data$pointid <- NA
  
# Add survey score
data_merged[,c("survey_method","survey_quality","survey_score")] |> unique()
TBR_data$survey_quality |> unique()

TBR_data$survey_score <- sapply(TBR_data$survey_quality, function(sq){
  if(sq == "Low"){
    out <- 5
  }else if(sq == "Medium-Low"){
    out <- 4
  }else if(sq == "Medium"){
    out <- 3
  }else if(sq == "Medium-High"){
    out <- 2
  }else if(sq == "High"){
    out <- 1
  }else if(sq == "Unknown"){
    out <- "Unknown"
  }
  return(out)
})

# Merge data_merged and TBR_data
data_merged <- data_merged[,colnames(TBR_data)]
data_merged <- rbind(data_merged,TBR_data)
nrow(data_merged) # 2898

# check if there are streamid in common between data_merged and dataset_1part2
# --> no , all good then
cond <- data_merged$streamid %in% dataset_1part2$streamid
data_merged$streamid[cond]


# merge  data_merged and dataset_1part2
colnames(dataset_1part2)[!colnames(dataset_1part2) %in% colnames(data_merged)]
colnames(data_merged)[!colnames(data_merged) %in% colnames(dataset_1part2)]

data_merged <- data_merged[,colnames(dataset_1part2)]

dataset_1part2_final <- rbind(dataset_1part2,data_merged)

# order fields:
dataset_1part2_final <- dataset_1part2_final %>% 
  arrange(factor(region, levels = c("Yukon","Transboundary","Haida Gwaii","Nass",
                                    "Skeena","Central Coast",
                                    "Vancouver Island & Mainland Inlets",
                                    "Fraser","Columbia")),
          species_name,
          cu_name_pse,
          factor(indicator, levels = c("Y","N","",NA)),
          stream_name_pse,
          year)


#
# Save dataset_1part2_DATE.csv -----

date <- as.character(Sys.time())
date <- strsplit(x = date, split = " ")[[1]][1]
# date <- gsub("-","",date)
write.csv(dataset_1part2_final,paste0(wd_output,"/dataset_1part2_",date,".csv"),
          row.names = F)

dataset_1part2 <- import_mostRecent_file_fun(wd = wd_output,
                                             pattern = "dataset_1part2")
cond <- dataset_1part2$species_name == "Coho"
sites_Coho <- unique(dataset_1part2$GFE_ID[cond])
length(sites_Coho) # 1705

#
# Check if multiple series appear for a same cuid - streamid combination -----------
#' There WERE several instances were there were multiple locations associated to
#' a same sys_nm for a same cuid
#' where generate multiple instances where several time series have the same streamid and cuid and point id.

nrow(dataset_1part2) # 149828

d1 <- dataset_1part2 %>%
  # group_by(region,species_name,species_abbr,cuid,cu_name_pse,year) %>%
  group_by(region,species_name,species_abbr,cuid,cu_name_pse,pointid,streamid,
           stream_name_pse,year,survey_method,survey_quality) %>%
  summarise(stream_observed_count_2 = sum(stream_observed_count))

nrow(d1) # 149828

nrow(dataset_1part2) - nrow(d1) # 0 --> all good

series <- unique(dataset_1part2[,c("region","cuid","pointid","streamid")])
for(r in 1:nrow(series)){
  # r <- 1
  region <- series$region[r]
  cuid <- series$cuid[r]
  pointid <- series$pointid[r]
  streamid <- series$streamid[r]
  
  cond <- dataset_1part2$region == region &
    dataset_1part2$cuid == cuid &
    dataset_1part2$streamid == streamid
  
  yrs <- dataset_1part2[cond,]$year
  
  if(any(duplicated(yrs))){
    break
    dataset_1part2_cut <- dataset_1part2[cond,]
    
    # years with conflicting survey_method and survey_quality
    yrs_dupli <- yrs[duplicated(yrs)]
    
    dataset_1part2_cut[dataset_1part2_cut$year %in% yrs_dupli,]
    
    cond_method_diff <- cond & dataset_1part2$year %in% yrs_dupli 
    
    # check if the survey_method are the same for each duplicated year:
    survey_method_yr_diff <- sapply(X = yrs_dupli, FUN = function(yr){
      cond_yr <- cond & dataset_1part2$year == yr
      survey_method <- unique(dataset_1part2$survey_method[cond_yr])
      out <- length(survey_method) > 1
      return(out)
    })
    
    # check if the survey_quality are the same for each duplicated year:
    survey_quality_yr_diff <- sapply(X = yrs_dupli, FUN = function(yr){
      cond_yr <- cond & dataset_1part2$year == yr
      survey_quality <- unique(dataset_1part2$survey_quality[cond_yr])
      out <- length(survey_quality) > 1
      return(out)
    })
    
    # no different in the survey methods and quality in duplicated years
    if(!any(survey_method_yr_diff) & !any(survey_quality_yr_diff)){
      
      d_new <- dataset_1part2_cut %>%
        # group_by(region,species_name,species_abbr,cuid,cu_name_pse,year) %>%
        group_by(region,species_name,species_abbr,cuid,cu_name_pse,pointid,streamid,
                 stream_name_pse,indicator,longitude,latitude,year,
                 survey_method,survey_quality) %>%
        summarise(stream_observed_count = sum(stream_observed_count))
      
      
    }else{
      break
      
      
    }
    # dataset_1part2 <- dataset_1part2[!cond,]
    # dataset_1part2 <- rbind(dataset_1part2,d_new)
  }
}



dataset_1part2$streami