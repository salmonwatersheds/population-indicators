###############################################################################
# Code to compile data quality output for the different indicators and 
# output CU-level data quality dataset (dataset 390)
###############################################################################

# Parameters to be included in dataset 390:
# regionname: one of Yukon, Transboundary, Haida Gwaii, Nass, Skeena, 
#             Central Coast, Vancouver Island & Mainland Inlets, Fraser, 
#             Columbia
# species: one of Chinook, Chum, Coho, Pink (even), Pink (odd), Lake sockeye, 
#          River sockeye, Steelhead
# cuid: unique PSF-assigned cu identifier
# cu_name_pse: CU name as displayed in the PSE
# parameter: one of survey_quality, survey_coverage, survey_execution, 
#            catch_quality, stockid_quality, juvenile_quality, 
#            rt_quality, dq_score*
# datavalue: value of data quality criteria
#
# *Note: maintain dq_score as the sum of survey_quality, survey_coverage, 
# survey_execution and catch_quality for application in the legacy site.
# In PSE 2.0, CU-level DQ scores will be the average of relevant criteria.

library(dplyr)
source("code/functions_general.R")

recent_year <- 2022 # What is the most recent year of spawner abundance data??

#############################################################################################
# Set up empty dataframe
###############################################################################

Dropbox_directory <- "/Users/stephaniepeacock/Salmon\ Watersheds\ Dropbox/Stephanie\ Peacock/X\ Drive/1_PROJECTS/1_Active/Population\ Methods\ and\ Analysis/population-indicators/"

cu_list <- read.csv(paste0(Dropbox_directory, "data-input/conservationunits_decoder.csv"))

cuid <- unique(cu_list$pooledcuid)
parameters <- c("survey_quality", "survey_coverage", "survey_execution", "catch_quality", "stockid_quality", "juvenile_quality", "rt_quality", "dq_score")

dataset390 <- expand.grid(cuid, parameters) %>%
  rename(cuid = "Var1", parameter = "Var2") %>%
  arrange(cuid)

head(dataset390)

# Add in CU info
names(cu_list)

dataset390 <- dataset390 %>% left_join(cu_list %>%
                                         select(region,species_name,cu_name_pse, cuid),
                                       by = "cuid") %>%
  rename(species = "species_name", regionname = "region") %>%
  relocate(regionname, species, cuid, cu_name_pse, parameter) %>%
  mutate(datavalue = NA)
  
head(dataset390)

###############################################################################
# Add in data-quality criteria 
###############################################################################

#------------------------------------------------------------------------------
# survey_quality
#------------------------------------------------------------------------------

# To determine CU-level Spawner Survey Method data quality scores, we calculate a weighted average of the stream-level survey method data quality scores across all indicator streams over the most recent generation within the CU
# 

# read in spawner survey data
spawner_surveys <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_streamspawnersurveys_output") %>%
  filter(stream_survey_method != "Unknown Estimate Method") %>% # Remove survey years when spawner survey methods were Unknown
  filter(indicator == "Y") # Use only indicator streams

head(spawner_surveys)

# add gen_length for calculating most recent generation
spawner_surveys <- spawner_surveys %>% 
  left_join(cu_list %>%
              select(gen_length, cuid),
            by = "cuid")
  
head(spawner_surveys)

# create numeric data quality score from stream_survey_quality
unique(spawner_surveys$stream_survey_quality)

spawner_surveys$Q <- case_when(
  spawner_surveys$stream_survey_quality == "Low" ~ 1,
  spawner_surveys$stream_survey_quality == "Medium-Low" ~ 2,
  spawner_surveys$stream_survey_quality == "Medium" ~ 3,
  spawner_surveys$stream_survey_quality == "Medium-High" ~ 4,
  spawner_surveys$stream_survey_quality == "High" ~ 5,
  spawner_surveys$stream_survey_quality == -989898 ~ NA, # Should this be zero?
  spawner_surveys$stream_survey_quality == "Unknown" ~ NA
)

# Calculate mean Q by cuid
# Question: Are these scores weighted by current spawner abundance??
datavalue1 <- spawner_surveys %>%
  filter(year > 2022 - gen_length + 1) %>% # Look over the most recent generation
  group_by(cuid) %>%
  summarise(survey_quality = mean(Q, na.rm = TRUE))
datavalue1 <- datavalue1[!is.na(datavalue1$survey_quality),]

# SP: Got stuck with dplyr...suggestions on this sectino? Something to do with mutate?
par.ind <- which(dataset390$parameter == "survey_quality")

dataset390$datavalue[par.ind[match(datavalue1$cuid, dataset390$cuid[par.ind])]] <- datavalue1$survey_quality
# Quite different from existing DQ scores for survey_quality...

#------------------------------------------------------------------------------
# survey_coverage
#------------------------------------------------------------------------------

# https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#spawner-survey-coverage

#------------------------------------------------------------------------------
# survey_execution
#------------------------------------------------------------------------------

# https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#spawner-survey-excecution

#------------------------------------------------------------------------------
# catch_quality
#------------------------------------------------------------------------------

# https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#catch-estimates

#------------------------------------------------------------------------------
# stockid_quality
#------------------------------------------------------------------------------

# New indicator from PSC that only applies to Fraser sockeye


#------------------------------------------------------------------------------
# juvenile_quality
#------------------------------------------------------------------------------

# Based on juveniele survey method
# Read in dataset88
# read in juvenile survey (js) data
js <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset88_output") 

head(js)

# Assign score for each record


#------------------------------------------------------------------------------
# rt_quality
#------------------------------------------------------------------------------

# Based on run timing data quality scores compile in run-timing folder
rt_dq <- read.csv(paste0(Dropbox_directory, "run-timing/output/"))

#------------------------------------------------------------------------------
# dq_score
#------------------------------------------------------------------------------

###############################################################################
# Write output dataset
###############################################################################

write.csv(dataset390, file = paste0(Dropbox_directory, "data-quality/output/dataset390_", Sys.Date(), ".csv"), row.names = FALSE)

