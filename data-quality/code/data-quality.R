###############################################################################
# Code to compile data quality output for the different indicators and 
# output CU-level data quality dataset (dataset 390)
###############################################################################

# Parameters to be included in dataset 390:
# (https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1712104520826029?thread_ts=1712097226.312849&cid=CJG0SHWCW)
#
# regionname: one of Yukon, Transboundary, Haida Gwaii, Nass, Skeena, 
#             Central Coast, Vancouver Island & Mainland Inlets, Fraser, 
#             Columbia
#
# species: one of Chinook, Chum, Coho, Pink (even), Pink (odd), Lake sockeye, 
#          River sockeye, Steelhead
#
# cuid: unique PSF-assigned cu identifier
#
# cu_name_pse: CU name as displayed in the PSE
#
# parameter: one of 
# ** component criteria scores **
#    survey_quality
#    survey_coverage
#    survey_execution
#    catch_quality
#    stockid_quality
#    juvenile_quality
#    runtiming_quality
# ** indicator scores summarized across component criteria **
#    spawner_surveys
#    juvenile_surveys
#    spawner_abundance
#    run_timing
#    catch_run_size
#    recruits_per_spawner
#    trends_spawner_abund
#    biological_status 
#    dq_score*
#
# datavalue: value of data quality criteria
#
# *Note: maintain dq_score as the sum of survey_quality, survey_coverage, 
# survey_execution and catch_quality for application in the legacy site.
# In PSE 2.0, CU-level DQ scores will be the average of relevant criteria.

#' Files imported:
#' - dataset390_DATE.csv                     # the file to update
#' - juvenilesurveys.csv
#' - streamspawnersurveys_output.csv
#' - conservationunits_decoder.csv
#' - Fraser Catch and StockID quality.xlsx
#' - run-timing-data-quality_2024-03-08.csv
#' 
#' Files exported:
#' - dataset390_DATE.csv


library(tidyverse)
library(dplyr)

source("code/functions_general.R")


#------------------------------------------------------------------------------
# Set Dropbox directory depending on user
#------------------------------------------------------------------------------

# return the name of the directories for the different projects:
Dropbox_root <- read.delim("wd_X_Drive1_PROJECTS.txt", header = FALSE)[1,1]
Dropbox_directory <- paste0(Dropbox_root, "/1_Active/Population Methods and Analysis/population-indicators/")

################################################################################
# Set up dataframe
###############################################################################

#------------------------------------------------------------------------------
# Load database data **Do this at the top so the rest of the script can be easily run
#------------------------------------------------------------------------------

# Juvenile survey data
js <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset88_output") # Read direct from database if needs updating

# # write.csv(js, file= paste0(Dropbox_directory, "data-input/juvenilesurveys.csv")) # Update in Dropbox
# js <- read.csv(paste0(Dropbox_directory, "data-input/juvenilesurveys.csv")) # Read from Dropbox so script can be sourced

# # Spawner survey data
spawner_surveys0 <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_streamspawnersurveys_output") %>% 
  filter(stream_observed_count != -989898)

# # Read from Dropbox so script can be sourced
# spawner_surveys <- read.csv(paste0(Dropbox_directory, "data-input/streamspawnersurveys_output.csv")) %>%
#   filter(stream_survey_quality %in% c("Unknown", "-989898") == FALSE) # Remove survey years when spawner survey methods were Unknown

# Most recent year of data (for calculating average abundance over the most recent generation) depends on region
# 2021, 2022, or 2023
spawner_surveys <- spawner_surveys0 %>%
  group_by(region) %>%
  mutate(most_recent_year = max(year))

sort(unique(spawner_surveys$most_recent_year))

# Catch data
catch <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset3_output")

#------------------------------------------------------------------------------
# Create empty dataframe:
#------------------------------------------------------------------------------

# Load cu list
cu_list <- read.csv(paste0(Dropbox_directory, "data-input/conservationunits_decoder.csv")) %>%
  distinct(pooledcuid, .keep_all = TRUE) # there are duplicates for pooledcuid

unique(tapply(cu_list$cuid, cu_list$cuid, length))

dataset390 <- cu_list %>% 
  select(region, species_name,cu_name_pse, pooledcuid) %>%
  rename(species = "species_name", regionname = "region", cuid = "pooledcuid") %>%
  relocate(regionname, species, cuid, cu_name_pse)
  
head(dataset390)

###############################################################################
# Add in data-quality criteria 
###############################################################################

#------------------------------------------------------------------------------
# survey_quality 
#------------------------------------------------------------------------------

# To determine CU-level Spawner Survey Method data quality scores, we calculate 
# a weighted average of the stream-level survey method data quality scores 
# across all indicator streams over the most recent generation within the CU

# add gen_length for calculating most recent generation
spawner_surveys <- spawner_surveys %>% 
  left_join(cu_list %>%
              select(gen_length, cuid),
            by = "cuid")
  
head(spawner_surveys)

# create numeric data quality score from stream_survey_quality
unique(spawner_surveys$stream_survey_quality)

spawner_surveys$quality_num <- case_when(
  spawner_surveys$stream_survey_quality == "Low" ~ 1,
  spawner_surveys$stream_survey_quality == "Medium-Low" ~ 2,
  spawner_surveys$stream_survey_quality == "Medium" ~ 3,
  spawner_surveys$stream_survey_quality == "Medium-High" ~ 4,
  spawner_surveys$stream_survey_quality == "High" ~ 5,
  spawner_surveys$stream_survey_quality == "Unknown" ~ NA
)

# Calculate mean Q by streamid
# Question: Are these scores weighted by current spawner abundance?? -> Yes
stream_summary <- spawner_surveys %>%
  filter(year > most_recent_year - gen_length + 1) %>% # Look over the most recent generation
  group_by(streamid) %>%
  summarise(dq = mean(quality_num, na.rm = TRUE), # Mean data quality score for the stream over the most recent gen
            current_spawners = exp(mean(log(stream_observed_count + 0.01), na.rm = TRUE)) # geometric mean spawner abundance over most recent gen
  ) %>%
  left_join(spawner_surveys %>% # Add in cuid
              distinct(streamid, .keep_all = TRUE) %>%
              select(streamid, cuid, indicator)
  ) 

# add in summed CU spawners
stream_summary <- stream_summary %>% 
  left_join(stream_summary %>% # Calculate observed spawners in most recent gen
              group_by(cuid) %>%
              summarise(cu_summed_spawners = sum(current_spawners, na.rm = TRUE))
  ) %>%
  mutate(prop_spawners = current_spawners/cu_summed_spawners) # Calculate the proportion of observed spawners for each streamid

# Sum stream quality across indicator streams, weighted by proportion of observed spawners in that stream 
dataset390 <- dataset390 %>% left_join(stream_summary %>%
                           group_by(cuid) %>%
                           filter(indicator == "Y")%>% # Use only indicator streams
                           summarise(survey_quality = round(sum(dq*prop_spawners)))
)

head(dataset390)

#------------------------------------------------------------------------------
# survey_coverage 
#------------------------------------------------------------------------------

# https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#spawner-survey-coverage

dataset390 <- dataset390 %>% 
  left_join(stream_summary %>%
              group_by(cuid) %>%
              summarise(survey_coverage = (sum(prop_spawners[indicator=='Y']))))

dataset390 <- dataset390 %>%
  mutate(survey_coverage = case_when(survey_coverage >= 0.9 ~ 5,
                              survey_coverage < 0.9 & survey_coverage >= 0.7 ~ 4,
                              survey_coverage < 0.7 & survey_coverage >= 0.5 ~ 3,
                              survey_coverage < 0.5 & survey_coverage >= 0.3 ~ 2,
                              survey_coverage < 0.3 & survey_coverage >= 0 ~ 1,
                              ))

head(dataset390)

#------------------------------------------------------------------------------
# survey_execution 
#------------------------------------------------------------------------------

# https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#spawner-survey-excecution

spawner_surveys_ex <- spawner_surveys %>%
  filter(indicator == "Y") %>% # Use only indicator streams
  filter(year > most_recent_year - gen_length + 1)%>%  # Look over the most recent generation
  group_by(cuid) %>%
  summarise(n=n(),
    streams=n_distinct(streamid))

spawner_surveys_ex <- spawner_surveys_ex %>% 
  left_join(cu_list %>%
              select(gen_length, cuid),
            by = "cuid")          


spawner_surveys_ex$survey_execution <- with(spawner_surveys_ex, n/(streams*gen_length))

spawner_surveys_ex <- spawner_surveys_ex %>%
  select(cuid, survey_execution)

dataset390 <- dataset390 %>% left_join(spawner_surveys_ex, by='cuid')

dataset390 <- dataset390 %>%
  mutate(survey_execution = case_when(survey_execution >= 0.8 ~ 5,
                                      survey_execution < 0.8 & survey_execution >= 0.6 ~ 4,
                                      survey_execution < 0.6 & survey_execution >= 0.4 ~ 3,
                                      survey_execution < 0.4 & survey_execution >= 0.2 ~ 2,
                                      survey_execution < 0.2 & survey_execution >= 0 ~ 1,
  ))

head(dataset390)

#------------------------------------------------------------------------------
# catch_quality
#------------------------------------------------------------------------------

# https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#catch-estimates

# Read in catch_quality revisions from Legacy site
catch_quality_dq <- readxl::read_xlsx("data-quality/data/catch-quality_2024-09-04.xlsx")

# Change quality score of zero to NA for average calculations at indicator score step
catch_quality_dq$catch_quality_revised[catch_quality_dq$catch_quality_revised == 0] <- NA

# Check that no CUs with data are being assigned NA
if(sum(unique(catch$cuid[which(catch$cdn_catch != -989898 | catch$combined_catch != -989898)]) %in% catch_quality_dq$cuid[is.na(catch_quality_dq$catch_quality_revised)]) > 0) {
  cuid.x <- unique(catch$cuid[which(catch$cdn_catch != -989898 | catch$combined_catch != -989898)])
  print(paste("cuid:", cuid.x[which(cuid.x %in% catch_quality_dq$cuid[is.na(catch_quality_dq$catch_quality_revised)] == TRUE)], collapse = ", "))
  stop("CU with catch data has a catch_quality of zero. Need to assign non-zero catch quality.")
}

# Join to dataset390
dataset390 <- dataset390 %>% 
  left_join(catch_quality_dq %>% 
              select(cuid, catch_quality_revised)) %>%
  rename(catch_quality = catch_quality_revised)

head(dataset390)

#------------------------------------------------------------------------------
# stockid_quality 
#------------------------------------------------------------------------------

# New indicator from PSC that **only applies to Fraser sockeye**
psc_stockid <- readxl::read_xlsx(paste0(Dropbox_directory, "data-quality/data/Fraser Catch and StockID quality.xlsx")) %>%
  select(location, species, 'Stock ID quality')

# Can we match on cu name?
psc_stockid$location %in% cu_list$cu_name_pse # Yes

# index of stockid_quality parameter
dataset390 <- dataset390 %>% left_join(psc_stockid %>%
                                         select(location, 'Stock ID quality') %>%
                                         rename(cu_name_pse = "location", stockid_quality = 'Stock ID quality'))

head(dataset390)

#------------------------------------------------------------------------------
# juvenile_quality
#------------------------------------------------------------------------------

# Based on juvenile survey method from dataset88_juvenile_surveys
# Dataset sourced in header code from database

# add gen_length for calculating most recent generation
js <- js %>% 
  left_join(cu_list %>%
              select(gen_length, cuid),
            by = "cuid")

head(js)

# Assign score for each record
unique(js$enumeration_method)

js$Q <- case_when(
  js$enumeration_method == "Fyke/Inclined Plane Trap" ~ 3,
  js$enumeration_method == "Fyke Trap" ~ 3,
  js$enumeration_method == "Mark-recapture" ~ 4,
  js$enumeration_method == "Fence" ~ 5,
  js$enumeration_method == "Electrofishing" ~ 2,
  js$enumeration_method == "Weir sampling" ~ 5,
  js$enumeration_method == "Weir Sampling" ~ 5,
  js$enumeration_method == "Snorkel" ~ 2,
  js$enumeration_method == "Rotary Screw Trap" ~ 3,
  js$enumeration_method == "Not specified" ~ 1,
  js$enumeration_method == "Full Spanning Fence" ~ 5,
  js$enumeration_method == "Fence, Mark-Recapture" ~ 4,
  js$enumeration_method == "Fence?" ~ 5,
  js$enumeration_method == "Mark recapture" ~ 4,
  js$enumeration_method == "Smolt Weir" ~ 5,
  js$enumeration_method == "Mark Recapture" ~ 4
)

unique(js$Q)


# Calculate mean Q by cuid and join
dataset390 <- dataset390 %>% left_join(js %>%
  group_by(cuid) %>%
  summarise(juvenile_quality = round(mean(Q, na.rm = TRUE)))
)

head(dataset390)

#------------------------------------------------------------------------------
# runtiming_quality
#------------------------------------------------------------------------------

# Based on run timing data quality scores compile in run-timing folder
rt_dq <- read.csv(paste0(Dropbox_directory, "timing/output/run-timing-data-quality.csv"))

# Original run timing score is on a scale of 1 = good to 6 = poor
# Re-scale
sort(unique(rt_dq$rt_dat_qual))
rt_dq$runtiming_quality <- case_when(
  rt_dq$rt_dat_qual == 1 ~ 5,
  rt_dq$rt_dat_qual == 2 ~ 4,
  rt_dq$rt_dat_qual %in% c(3, 3.5) ~ 3,
  rt_dq$rt_dat_qual %in% c(4, 4.5) ~ 2,
  rt_dq$rt_dat_qual >= 5 ~ 1
)

# Join to dataset
dataset390 <- dataset390 %>% left_join(rt_dq %>%
                                         select(cuid, runtiming_quality))

head(dataset390)


###############################################################################
# Compute indicator level scores
###############################################################################

# Note: use na.rm = TRUE to remove a criteria if it does not exist. For 
# example, if catch_quality = NA, then biological status is presumably based
# on percentiles and the criteria associated with spawner abundance should
# apply. StockID is NA for all CUs except Fraser sockeye, so should be ignored
# if NA.

# To ensure this works, we need to set data quality scores that were zero in the 
# old data to NA
dataset390[which(dataset390 == 0, arr.ind = TRUE)] <- NA

#------------------------------------------------------------------------------
# dq_score ** Included for legacy site
#------------------------------------------------------------------------------

# Sum of survey_quality, survey_coverage, survey_execution, catch_quality 
# NOTE: This will throw an error that "survey_coverage does not exist" until we get that finished

dataset390 <- dataset390 %>% 
  left_join(dataset390 %>% 
              group_by(cuid) %>%
              summarise(dq_score = round(sum(survey_quality, survey_coverage, survey_execution, catch_quality, na.rm = TRUE)))
  )
            
#------------------------------------------------------------------------------
# spawner_surveys
#------------------------------------------------------------------------------

dataset390 <- dataset390 %>% 
  left_join(dataset390 %>% 
              group_by(cuid) %>%
              summarise(spawner_surveys = round(mean(c(survey_quality, survey_coverage, survey_execution), na.rm = TRUE)))
  )

#------------------------------------------------------------------------------
# juvenile_surveys
#------------------------------------------------------------------------------
dataset390 <- dataset390 %>% 
  mutate(juvenile_surveys = juvenile_quality)

#------------------------------------------------------------------------------
# spawner_abundance
#------------------------------------------------------------------------------

dataset390 <- dataset390 %>% 
  left_join(dataset390 %>% 
              group_by(cuid) %>%
              summarise(spawner_abundance = round(mean(c(survey_quality, survey_coverage, survey_execution), na.rm = TRUE)))
  )

#------------------------------------------------------------------------------
# run_timing
#------------------------------------------------------------------------------

dataset390 <- dataset390 %>% 
  mutate(run_timing = runtiming_quality)

#------------------------------------------------------------------------------
# catch_run_size
#------------------------------------------------------------------------------

# Import catch data 
dataset390 <- dataset390 %>% 
  left_join(dataset390 %>% 
              group_by(cuid) %>%
              summarise(catch_run_size = round(mean(c(catch_quality, stockid_quality), na.rm = TRUE)))
  )

#------------------------------------------------------------------------------
# recruits_per_spawner
#------------------------------------------------------------------------------

dataset390 <- dataset390 %>% 
  left_join(dataset390 %>% 
              group_by(cuid) %>%
              summarise(recruits_per_spawner = round(mean(c(survey_quality, survey_coverage, survey_execution, catch_quality, stockid_quality), na.rm = TRUE)))
  )

#------------------------------------------------------------------------------
# trends_spawner_abund
#------------------------------------------------------------------------------

dataset390 <- dataset390 %>% 
  left_join(dataset390 %>% 
              group_by(cuid) %>%
              summarise(trends_spawner_abund = round(mean(c(survey_quality, survey_coverage, survey_execution), na.rm = TRUE)))
  )
#------------------------------------------------------------------------------
# biological_status 
#------------------------------------------------------------------------------

dataset390 <- dataset390 %>% 
  left_join(dataset390 %>% 
              group_by(cuid) %>%
              summarise(biological_status = round(mean(c(survey_quality, survey_coverage, survey_execution, catch_quality, stockid_quality), na.rm = TRUE)))
  )

###############################################################################
# Write output dataset
###############################################################################

# Replace NAs with DQ score of zero
dataset390[which(is.na(dataset390), arr.ind = TRUE)] <- 0

# Write tracked copy
write.csv(dataset390, file = "data-quality/output/dataset390_data_quality.csv", row.names = FALSE)

# Write archive cope
write.csv(dataset390, file = paste0(Dropbox_directory, "data-quality/output/archive/dataset390_data_quality_", Sys.Date(), ".csv"), row.names = FALSE)

###############################################################################
# Compare to old dataset390
###############################################################################

# # Current live DQ data in Legacy site
# dataset390_filename <- list.files(path = paste0(Dropbox_directory, "data-quality/output/archive"), 
#                                   pattern = "dataset390") %>%
#   sort() %>%
#   tail(1)
# 
# dataset390_old <- read.csv(paste0(Dropbox_directory, "data-quality/output/archive/", dataset390_filename))
# 
# 
# cbind(names(dataset390), names(dataset390_old))
# dim(dataset390)
# dim(dataset390_old)
# 
# # For each parameter, list which rows changed
# change_rows <- list()
# J <- 0
# for(j in 1:length(names(dataset390))){
#   n_changes <- 466 - sum(dataset390[,j] == dataset390_old[,j])
#   print(paste0("Parameter ", names(dataset390)[j]," : ", n_changes, " changes"))
#   if(n_changes > 0){
#     J <- J + 1
#     change_rows[[J]] <- which(dataset390[,j] != dataset390_old[,j])
#     names(change_rows)[J] <- names(dataset390)[j]
#   }
# }
# 
# # Which catch & run size changed?
# dataset390[change_rows$catch_run_size, c(1:4, match(c("catch_quality", "stockid_quality", "catch_run_size"), names(dataset390)))]
# 
# dataset390_old[change_rows$catch_run_size, c(1:3, match(c("catch_quality", "stockid_quality", "catch_run_size"), names(dataset390)))]
# # catch_run_size - previously the mean seemed to use catch_quality == 0 when it should have been NA?