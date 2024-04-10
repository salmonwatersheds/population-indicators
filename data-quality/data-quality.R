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

library(tidyverse)
source("code/functions_general.R")

# recent_year <- 2022 # What is the most recent year of spawner abundance data??

#############################################################################################
# Set up dataframe
###############################################################################

# Set Dropbox directory depending on user
Dropbox_directory <- "/Users/erichertz/Salmon Watersheds Dropbox/Eric Hertz/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/"

Dropbox_directory <- "/Users/stephaniepeacock/Salmon Watersheds Dropbox/Stephanie Peacock/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/"

#------------------------------------------------------------------------------
# Load database data **Do this at the top so the rest of the script can be easily run
#------------------------------------------------------------------------------

# Current DQ data
dataset390_old <- read.csv(paste0(Dropbox_directory, "data-quality/output/dataset390_2023-05-20.csv"))

# Juvenile survey data
# js <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset88_output") # Read direct from database if needs updating
# write.csv(js, file= paste0(Dropbox_directory, "data-input/juvenilesurveys.csv")) # Update in Dropbox
js <- read.csv(paste0(Dropbox_directory, "data-input/juvenilesurveys.csv")) # Read from Dropbox so script can be sourced

# Spawner survey data
# spawner_surveys <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_streamspawnersurveys_output") %>%
#   filter(stream_survey_quality %in% c("Unknown", "-989898") == FALSE) %>% # Remove survey years when spawner survey methods were Unknown
#   filter(indicator == "Y") # Use only indicator streams

# Read from Dropbox so script can be sourced
spawner_surveys <- read.csv(paste0(Dropbox_directory, "data-input/streamspawnersurveys_output.csv")) %>%
  filter(stream_survey_quality %in% c("Unknown", "-989898") == FALSE) # Remove survey years when spawner survey methods were Unknown


#------------------------------------------------------------------------------
# Create empty dataframe:
#------------------------------------------------------------------------------

# Load cu list
cu_list <- read.csv(paste0(Dropbox_directory, "data-input/conservationunits_decoder.csv")) %>%
  distinct(pooledcuid, .keep_all = TRUE) %>% # there are duplicates for pooledcuid
  filter(cu_type != "Bin")
  
unique(tapply(cu_list$cuid, cu_list$cuid, length))
# cu_list <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_conservationunits_decoder") 
# write.csv(cu_list, file= paste0(Dropbox_directory, "data-input/conservationunits_decoder.csv")) # Update in Dropbox

dataset390 <- cu_list %>% select(region, species_name,cu_name_pse, pooledcuid) %>%
  rename(species = "species_name", regionname = "region", cuid = "pooledcuid") %>%
  relocate(regionname, species, cuid, cu_name_pse)
  
head(dataset390)

###############################################################################
# Add in data-quality criteria 
###############################################################################

#------------------------------------------------------------------------------
# survey_quality - Eric to review and ensure the approach is correct
#------------------------------------------------------------------------------

# To determine CU-level Spawner Survey Method data quality scores, we calculate 
# a weighted average of the stream-level survey method data quality scores 
# across all indicator streams over the most recent generation within the CU


# # read in spawner survey data
# spawner_surveys <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_streamspawnersurveys_output") %>%
#   filter(stream_survey_quality %in% c("Unknown", "-989898") == FALSE) %>% # Remove survey years when spawner survey methods were Unknown
#   filter(indicator == "Y") # Use only indicator streams
# 
# head(spawner_surveys)

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
)

# Calculate mean Q by streamid
# Question: Are these scores weighted by current spawner abundance?? -> Yes
stream_summary <- spawner_surveys %>%
  filter(year > 2023 - gen_length + 1) %>% # Look over the most recent generation
  group_by(streamid) %>%
  summarise(dq = mean(quality_num, na.rm = TRUE), # Mean data quality score for the stream over the most recent gen
            current_spawners = exp(mean(log(stream_observed_count + 0.01), na.rm = TRUE)), # geometric mean spawner abundance over most recent gen
  ) %>%
  left_join(spawner_surveys %>% # Add in cuid
              distinct(streamid, .keep_all = TRUE) %>%
              select(streamid, cuid,indicator)
  ) 

# add in summed CU spawners
stream_summary <- stream_summary %>% 
  left_join(stream_summary %>% # Calculate observed spawners in most recent gen
              group_by(cuid) %>%
              summarise(cu_summed_spawners = sum(current_spawners))
  ) %>%
  mutate(prop_spawners = current_spawners/cu_summed_spawners) # Claculate the proportion of observed spawners for each streamid

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

dataset390 <- dataset390 %>% left_join(stream_summary %>%
                                        group_by(cuid) %>%
                                        summarise(survey_coverage = (sum(prop_spawners[indicator=='Y']))))

dataset390 <- dataset390 %>%
  mutate(survey_coverage = case_when(survey_coverage >= 0.9 ~ 5,
                              survey_coverage < 0.9 & survey_coverage >= 0.7 ~ 4,
                              survey_coverage < 0.7 & survey_coverage >= 0.5 ~ 3,
                              survey_coverage < 0.5 & survey_coverage >= 0.3 ~ 2,
                              survey_coverage < 0.3 & survey_coverage >= 0 ~ 1,
                              ))
  
#------------------------------------------------------------------------------
# survey_execution 
#------------------------------------------------------------------------------

# https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#spawner-survey-excecution

# Use old data for now
spawner_surveys_ex <- spawner_surveys %>%
  filter(indicator == "Y") %>% # Use only indicator streams
  filter(year > 2023 - gen_length + 1) %>% # Look over the most recent generation
  group_by(cuid) %>%
  summarise(survey_execution = ))) ## didn't quite figure this out yet


df <- subset(nuseds.counts, CU_findex=="SER-22" )%>%
  subset(.,IsIndicator=="Y")%>%
  select(.,X2015,X2016,X2017,X2018,X2019)

((length(df)*nrow(df))-sum(is.na(df)))/(length(df)*nrow(df))

#------------------------------------------------------------------------------
# catch_quality
#------------------------------------------------------------------------------

# https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#catch-estimates

# No change from existing?
dataset390 <- dataset390 %>% left_join(dataset390_old %>% 
                           filter(parameter == "catch_quality") %>%
                           select(cuid, datavalue) %>% 
                           rename(catch_quality = "datavalue")
)

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

# Based on juveniele survey method
# Read in dataset88
# read in juvenile survey (js) data (moved to top)
# js <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset88_output") 
# head(js)

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
  filter(year > 2022 - gen_length + 1) %>% # Look over the most recent generation
  group_by(cuid) %>%
  summarise(juvenile_quality = round(mean(Q, na.rm = TRUE)))
)

head(dataset390)

#------------------------------------------------------------------------------
# runtiming_quality
#------------------------------------------------------------------------------

# Based on run timing data quality scores compile in run-timing folder
rt_dq <- read.csv(paste0(Dropbox_directory, "timing/output/run-timing-data-quality_2024-03-08.csv"))

# Orignial run timing score is on a scale of 1 = good to 6 = poor
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

write.csv(dataset390, file = paste0(Dropbox_directory, "data-quality/output/dataset390_", Sys.Date(), ".csv"), row.names = FALSE)

