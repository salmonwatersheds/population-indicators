
#'******************************************************************************
#' The goal of the script is to sum the stream-level observed spawner counts 
#' (**dataset2_spawner-surveys_YYYY-MM-DD.csv** produced in
#' **spawner-surveys/code/4_datasets_for_PSE.R**) for each CU and to export the 
#' results as the field `observed_count` in 
#' **dataset1_spawner-abundance_YYYY-MM-DD.csv**.
#' 
#' UPDATE: this script WAS in spawner-abundance/code and is now here!!!
#' 
#' Files imported (from dropbox):
#' - streamspawnersurveys_output.csv  # from the database; = dataset2_spawner_surveys_YYYY-MM-DD.csv from spawner-surveys/code/4_datasets_for_PSE.R
#' - dataset1cu_output.csv            # from the database; = dataset1_spawner_abundance_YYYY-MM-DD.csv from elsewhere
#' 
#' Files produced: 
#' - dataset1_spawner_abundance_YYYY-MM-DD.csv # previously - dataset1cu_output.csv; = dataset1cu_output.csv in DB but without columns 'estimated_count' and 'total_run'
#'  
#' Note: code taken from 
#' Transboundary/Data & Assessments/transboundary-data/code/4_pse-spawner-abundance.R
#' https://www.dropbox.com/scl/fi/pt80lerubav9r83uh4vtr/4_pse-spawner-abundance.R?rlkey=e723ohin2r1k5vpr0in80lvxy&dl=0
#' 
#' Example of outputed dataset:
#' https://www.dropbox.com/scl/fi/b9jkohs2wixv48ua78r1w/spawner_abundance_dataset_1part1_2024-03-20.csv?rlkey=dpz1vykjx8c2wezidhl5tkhto&dl=0
#' 
#' Code adapted from Previous script: Fraser_salmon_CU_updates.Rmd
#' 
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


wd_X_Drive1_PROJECTS <- paste0(get_XDrive(),"1_PROJECTS")

wd_output <- paste(wd_X_Drive1_PROJECTS,
                   "1_Active/Population Methods and Analysis/population-indicators/spawner-abundance/output",
                   sep = "/")

wd_input <- paste(wd_X_Drive1_PROJECTS,
                   "1_Active/Population Methods and Analysis/population-indicators/spawner-surveys/output",
                   sep = "/")

# Loading packages & functions
library(tidyverse)
library(tidyr)


#'* Import streamspawnersurveys_output from the database *

spawnersurveys <- import_mostRecent_file_fun(wd = paste0(wd_input,"/archive"), # wd_output_sp_surveys
                                             pattern = "dataset2_spawner-surveys")  # TODO: replace eventually by dataset2_spawner_surveys
head(spawnersurveys)
cond <- spawnersurveys$region == ""
spawnersurveys <- spawnersurveys[!cond,]

spawnersurveys$source_id |> unique()

unique(spawnersurveys$region)


#'* Sum spawnersurveys per cuid and year *

dataset1_observed <- spawnersurveys %>%
  group_by(region, species_name, species_qualified, cuid, cu_name_pse, year) %>%
  summarise(observed_spawners = sum(stream_observed_count, na.rm = T))

dataset1_observed <- dataset1_observed  %>% 
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
          year)

head(dataset1_observed)

# Rename observed_spawners to observed_count (to be changed when dataset paramters are finalized)
colnames(dataset1_observed)[colnames(dataset1_observed) == "observed_spawners"] <- "observed_count"

#' DO NOT ADD source_id
#' cf. PSE data check- in meeting 2026-01-29

# dataset1_observed$source_id <- NA
# for(r in 1:nrow(dataset1_observed)){
#   # r <- 1
#   cond <- spawnersurveys$cuid == dataset1_observed$cuid[r] & 
#     spawnersurveys$year == dataset1_observed$year[r]
#   
#   source_id <- spawnersurveys$source_id[cond] |> unique()
#   
#   if("" %in% source_id & length(source_id) > 1){
#     source_id <- source_id[source_id != ""]
#   }
#   
#   source_id <- paste(source_id,collapse = " ")            # case there is a different source in different streams
#   
#   dataset1_observed$source_id[r] <- source_id
# }
# 
# unique(dataset1_observed$source_id)
# 
# head(dataset1_observed)

min(dataset1_observed$observed_count, na.rm = T)

dataset1_observed$observed_count <- round(dataset1_observed$observed_count)

# Export to /archive folder on dropbox:
date <- as.character(Sys.Date())
write.csv(dataset1_observed,
          paste0(wd_output,"/archive/dataset1_observed-spawners_",date,".csv"), # dataset_1part1_ previously
          row.names = FALSE)

# Export to /output locally to push to github
write.csv(dataset1_observed,
          paste0(getwd(),"/spawner-abundance/output/dataset1_observed-spawners.csv"), # dataset_1part1_ previously
          row.names = FALSE)


