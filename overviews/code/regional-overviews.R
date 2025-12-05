###############################################################################
#
# Dataset 558 "Overviews Conservation Unit Abundance Anomalies"
#
# Code to read-in and transform CU-level spawner abundances to 
# per cent anomalies for Regional Overviews.
#
###############################################################################


library(dplyr)

#------------------------------------------------------------------------------
# Create Dropbox paths
#------------------------------------------------------------------------------
# Directory where the local github repo population-indicators is located
base_dir <- getwd()

# Move up in the file structure until you find the parent directory that contains Dropbox 
# **Does this work on a PC/Linux OS?**
Dropbox_dir <- base_dir
while(length(grep("Salmon Watersheds Dropbox", list.files(path = Dropbox_dir))) == 0){
  setwd("..")
  Dropbox_dir <- getwd()
}

# Now set that working directory
Dropbox_dir <- paste(getwd(), "Salmon Watersheds Dropbox", sep = "/")

# WIthin Dropbox, the X Drive is in a named folder. But this can be found through exclusion of other common folders
name_folder <- list.files(path = Dropbox_dir)[which((list.files(path = Dropbox_dir) %in% c("Icon\r", "Team Folder", "Team Paper Docs")) == FALSE)]

if(length(name_folder) == 1){ 
  Dropbox_dir <- paste(Dropbox_dir, name_folder, sep = "/")
} else{
  stop("More than one possible Dropbox folder")
}

pop_dir <- paste(Dropbox_dir, "/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators", sep = "/")


setwd(base_dir)


#------------------------------------------------------------------------------
# Function to import data from database
#------------------------------------------------------------------------------
source("code/functions_general.R")
source("code/colours.R")

###############################################################################
# Read in data
###############################################################################

# If most up-to-date data are in Database use:
cu_abund <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset1cu_output")
cu_smoothed <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset103_output")
cu_smoothed$year <- as.integer(cu_smoothed$year)

# # If most up-to-date data are in Dropbox (not yet pushed to database) use:
# # !! Bruno: I don't think the datasets below are kept up-to-date. 
# # Pls check and update based on where you're storing the latest update!!
# cu_abund <- read.csv(paste0(pop_dir, "/spawner-abundance/output/dataset1_spawner_abundance.csv"))
# cu_smoothed <- read.csv(paste0(pop_dir, "/trends/output/dataset103_log_smoothed_spawners.csv"))

# Ignore code to integrate run size into dataset for now.
# The regional abundance is taken from Dataset 551 "Regional Salmon Abundance"
# Slack: https://pacificsalmonfdn.slack.com/archives/CKNVB4MCG/p1763416770067599
#
# cu_catch <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_catchrunsize_output")
# cu_catch[which(cu_catch == -989898, arr.ind = TRUE)] <- NA


###############################################################################
# Transform cu_abund
###############################################################################

# Calculate long-term average spawner abundance for each CU
cu_avg <- cu_abund %>%
  group_by(cuid) %>%
  summarise(
    cuid = unique(cuid),
    avg = exp(mean(log(estimated_count + 1), na.rm = TRUE)))

# Create smoothed_abund and perc_anom datasets
cu_spawner_combined <- cu_smoothed %>%
  mutate(smoothed_abund = exp(avg_escape_log)) %>%
  left_join(cu_avg) %>%
  mutate(perc_anom = round((smoothed_abund - avg)/avg*100, 1)) %>%
  rename("species" = species_name)

# ###############################################################################
# # Transform cu_catch
# # Note; commented out for now as we have deferred adding run size to this 
# # dataset. If we add in, will have to integrate smoothing, too. (Dec 5, 2025)
# ###############################################################################
# 
# #------------------------------------------------------------------------------
# # Calculate total run size
# #------------------------------------------------------------------------------
# 
# # Check that combined is only when us and cdn blank
# which(!is.na(cu_catch$combined_catch) & (!is.na(cu_catch$cdn_catch)|!is.na(cu_catch$us_catch)))
# 
# # Check that estimated count matches cu_abund
# cu_abund %>% 
#   select(cuid, year, estimated_count) %>% 
#   left_join(cu_catch %>% select(cuid, year, estimated_counts)) %>%
#   mutate(count_diff = estimated_count - estimated_counts) %>%
#   filter(count_diff != 0)
# 
# # If inseason, is everything else blank?
# cu_catch %>% filter(!is.na(in_season_run_size)) # Yes, except observed counts
# 
# # Calculate total run size
# cu_catch$total_run_calculated <- apply(cu_catch[, c("estimated_counts", 'us_catch', "cdn_catch", "combined_catch", "run_size_adjustment", "in_season_run_size")], 1, sum, na.rm = TRUE)
# 
# cond.na <- which(apply(cu_catch[, c('us_catch', "cdn_catch", "combined_catch", "run_size_adjustment", "in_season_run_size")], 1, function(x){sum(!is.na(x)) == 0}))
# cu_catch$total_run_calculated[cond.na] <- NA
# 
# #Check how different from run_size in dataset
# runsize_diff <- cu_catch %>% filter((total_run - total_run_calculated) != 0) %>%
#   left_join(cu_abund %>% select(region, species_name, cuid, cu_name_pse) %>% distinct(cuid, .keep_all = TRUE)) %>%
#   select(region, species_name, cu_name_pse, cuid, year, total_run, total_run_calculated) %>%
#   mutate(diff_run = total_run - total_run_calculated) %>%
#   mutate(perc_diff = round(diff_run/total_run_calculated*100))
# 
# runsize_diff %>% filter(perc_diff > 5) # Mostly Osoyoos sockeye; will leave for now but need to correct.
# 
# 
# # Calculate long-term average catch
# cu_catch_avg <- cu_catch %>%
#   group_by(cuid) %>%
#   summarise(
#     cuid = unique(cuid),
#     avg = exp(mean(log(total_run_calculated + 1), na.rm = TRUE)))
# 
# # Combine catch datasets
# # *****NOTE: NEED TO DO THE SMOOTHING AT SOME POINT!! *********************
# cu_catch_combined <- cu_catch %>%
#   left_join(cu_catch_avg) %>%
#   mutate(perc_anom = round((total_run_calculated - avg)/avg*100, 1)) %>%
#   left_join(cu_abund %>% select(region, species_name, cuid, cu_name_pse) %>% distinct(cuid, .keep_all = TRUE)) %>%
#   mutate(scale = "ConservationUnit", abundance_type = "total") %>%
#   rename(species = "species_name") %>%
#   select(scale, abundance_type, region, species, cuid, cu_name_pse, year, perc_anom)
#   
# 

###############################################################################
# Create Dataset 558 "Overviews Conservation Unit Trends in Abundance"
###############################################################################

# Note: Commented out code that adds in regional scale and total abundance (run size)
ro_dat <- cu_spawner_combined %>%
  mutate(abundance_type = "spawner") %>%
  select(region, species, cuid, cu_name_pse, abundance_type, year, perc_anom) #%>%
  # bind_rows(cu_catch_combined %>% 
# select(region, species, cuid, cu_name_pse, abundance_type, year, perc_anom)) %>%
  # arrange(match(region, regions), species, cuid, abundance_type, year)

write.csv(ro_dat, "overviews/output/dataset558_cu-anomaly.csv", row.names = FALSE)
write.csv(ro_dat, paste0(pop_dir, "/overviews/output/dataset558_cu-anomaly.csv"), row.names = FALSE)            
write.csv(ro_dat, paste0(pop_dir, "/overviews/output/archive/dataset558_cu-anomaly_", Sys.Date(), ".csv"), row.names = FALSE)            

