
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


#
# Combine nuseds_cuid_streamid with data_extra_Reynolds_lab -----
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
sum(cond_pointid_na) # 2726
cond_gfe_id_na <- is.na(nuseds_final$GFE_ID)    # 0
sum(cond_gfe_id_na) # 0
cond_streamid_na <- is.na(nuseds_final$streamid)
sum(cond_streamid_na) # 1998
# cond_noNa <- !cond_cuid_na & !cond_pointid_na & !cond_streamid_na
cond_noNa <- cond_cuid_na
dataset_1part2 <- nuseds_final[!cond_noNa,]

#' Remove Steelhead --> already removed in nuseds_data.collation.R

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

# remove NAs
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

head(dataset_1part2)
sum(is.na(dataset_1part2$streamid)) # 0
sum(is.na(dataset_1part2$cuid)) # 0
sum(is.na(dataset_1part2$pointid)) # 5117


dim(dataset_1part2) 
# 149828     17

#
# Save dataset_1part2_DATE.csv -----

date <- as.character(Sys.time())
date <- strsplit(x = date, split = " ")[[1]][1]
date <- gsub("-","",date)
write.csv(dataset_1part2,paste0(wd_output,"/dataset_1part2_",date,".csv"),
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