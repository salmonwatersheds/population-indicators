

#'******************************************************************************
#' The goal of the script is to compare the new *versus* previous biological status.
#' 
#' Files imported:
#' - conservationunits_decoder.csv                     (from database)
#' - dataset390_output.csv                             (from database) CU-level data quality
#' - cuspawnerabundance.csv                            (from database) observed spawner abundance
#' - biostatus_legacy_appendix4_2024-09-05.csv         The previous biological status values, benchmarks are other relevant information
#' - output/archive/dataset101_biological_status_YYYY-MM-DD.csv # final biostatus values ; produced in 3_biological_status.R
#' - output/archive/dataset102_benchmarks_YYYY-MM-DD.csv        # benchmark values  and other relevant information; produced in 3_biological_status.R
#' 
#' Files produced: 
#' - output/archive/biostatus_new_old_differences_YYYY-MM-DD.csv  # 
#' 
#' 
#' Resources:
#' - Steph diagram for decision rules
# https://www.dropbox.com/s/p0gf5aswd2kbd2p/Status_assessment_flowchart_SP.pptx?dl=0
# https://www.dropbox.com/s/x5xoy9qr5mrc32c/Status_assessment_flowchart_v19_percentile_edit.pptx?dl=0
#
# - Clare's slack thread about the 8th decision rule:
# https://salmonwatersheds.slack.com/archives/CPD76USTU/p1700508091399359
#
# - List CUs with high exploitation/low productivity & cyclic dominance
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1700673066049189?thread_ts=1700604709.505309&cid=CJ5RVHVCG
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
source("code/colours.R")

# return the name of the directories for the different projects:
subDir_projects <- subDir_projects_fun()

wds_l <- set_working_directories_fun(subDir = subDir_projects$biological_status,
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

wd_pop_data_quality_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                     "1_Active/Population Methods and Analysis/population-indicators/data-quality",
                                     sep = "/")

# The datasets to input were outputted by other scripts 
wd_data_input <- wd_output

# Import functions for this specific project
source("Code/functions.R")

library(tidyr)
library(dplyr)
library(xlsx)


# Import species names and acronyms
species_acronym_df <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

# select all the regions
region <- as.character(regions_df[1,])

# select certain species
species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "CK"],    
             species_acronym_df$species_name[species_acronym_df$species_acro == "SX"])

# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- TRUE

printFig <- F

options(warn = 0)

#
# Import datasets ------
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1717565600400059?thread_ts=1717434872.482819&cid=CJ5RVHVCG
# 

#'* Import the old biostatus & benchmarks from legacy site (appendix 4) *
# Query the data from the the database at appdata.vwdl_setr_appendix4
# biostatus_old <- retrieve_data_from_PSF_databse_fun(dsn_database = "salmondb_legacy",
#                                                     name_dataset = "appdata.vwdl_setr_appendix4")

# write.csv(biostatus_old,
#           paste0(wd_pop_indic_data_input_dropbox,"/biostatus_legacy_appendix4_2024-09-05.csv"),
#           row.names = F)

# biostatus_old <- read.csv(paste0(wd_pop_indic_data_input_dropbox,
#                                  "/biostatus_legacy_appendix4_2024-09-05.csv"),
#                           header = T)
# add psf_status_type to biostatus_old
# biostatus_old$psf_status_type <- NA
# cond <- !is.na(biostatus_old$sr_status)     # I assumed that is not NA then it is "sr"
# biostatus_old$psf_status_type[cond] <- "sr"
# cond <- is.na(biostatus_old$sr_status) & !is.na(biostatus_old$percentile_status)
# biostatus_old$psf_status_type[cond] <- "percentile"

biostatus_old <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                            pattern = "dataset101_biological_status",
                                            second_last = T)

head(biostatus_old)
nrow(biostatus_old) # 463

colnames(biostatus_old)

benchmarks_old <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                             pattern = "dataset102_benchmarks",
                                             second_last = T)

# TOREMOVE --> just this one time: last pdates pushed to the PSE:
biostatus_old <- read.csv(paste0(wd_output,"/archive/",
                                 "dataset101_biological_status_2024-09-13.csv"),
                          header = T)

benchmarks_old <- read.csv(paste0(wd_output,"/archive/",
                                 "dataset102_benchmarks_2024-09-13.csv"),
                          header = T)

colnames(biostatus_old)[colnames(biostatus_old) == "species_abbr"] <- "species_qualified"
colnames(benchmarks_old)[colnames(benchmarks_old) == "species_abbr"] <- "species_qualified"

biostatus_old$species_name[grepl("[S|s]ockeye",biostatus_old$species_name)] <- "Sockeye"
biostatus_old$species_name[grepl("Pink",biostatus_old$species_name)] <- "Pink"
benchmarks_old$species_name[grepl("[S|s]ockeye",benchmarks_old$species_name)] <- "Sockeye"
benchmarks_old$species_name[grepl("Pink",benchmarks_old$species_name)] <- "Pink"


#'* Import the new biostatus *
biostatus_new <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                            pattern = "dataset101_biological_status")

head(biostatus_new)
nrow(biostatus_new)  # 463 465 466

#'* Import the new benchmarks *
benchmarks_new <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                             pattern = "dataset102_benchmarks")
head(benchmarks_new)


#'* add curr_spw_end_year and curr_spw_start_year to biostatus *

# add curr_spw_end_year to biostatus_new
biostatus_new$curr_spw_end_year <- sapply(biostatus_new$cuid,function(cuid){
  cond <- benchmarks_new$cuid == cuid
  return(benchmarks_new$curr_spw_end_year[cond])
})

# add curr_spw_start_year to biostatus_new
biostatus_new$curr_spw_start_year <- sapply(biostatus_new$cuid,function(cuid){
  cond <- benchmarks_new$cuid == cuid
  return(benchmarks_new$curr_spw_start_year[cond])
})

# add curr_spw_end_year to biostatus_old
biostatus_old$curr_spw_end_year <- sapply(biostatus_old$cuid,function(cuid){
  cond <- benchmarks_old$cuid == cuid
  return(benchmarks_old$curr_spw_end_year[cond])
})

# add curr_spw_start_year to biostatus_old
biostatus_old$curr_spw_start_year <- sapply(biostatus_old$cuid,function(cuid){
  cond <- benchmarks_old$cuid == cuid
  return(benchmarks_old$curr_spw_start_year[cond])
})

#' #'* Import the current spawner abundance (for generation length) *
datasetsNames_database <- datasetsNames_database_fun()
fromDatabase <- update_file_csv <- F
cuspawnerabundance <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[2],
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)

#'* Import dataset390_output for survey_quality (for catch_method quality and decision tree) *
dataset390_output <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[18],
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)
# TEMPORARY (11/09/2024)
# dataset390_output <- import_mostRecent_file_fun(wd = wd_pop_indic_data_input_dropbox,
#                                                 pattern = "dataset390_data_quality")
# dataset390_output <- dataset390_output[,c("region","species_name","cuid","cu_name_pse","catch_quality")]
# colnames(dataset390_output)[colnames(dataset390_output) == "catch_quality"] <- "catch_method"
dataset390_output <- dataset390_output[,c("region","species_name","cuid","cu_name_pse","catch_method")]
head(dataset390_output)


nrow(dataset390_output) # 463 465 466

#'* Import conservationunits_decoder for generation length *
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

length(unique(conservationunits_decoder$pooledcuid)) # 463

#
# Compare old vs. new biostatus -------
#

#'* Merge biostatus *
columns <- c("region","species_name","species_qualified","cuid",
             "cu_name_pse","psf_status","psf_status_type",
             "curr_spw_start_year","curr_spw_end_year",
             "psf_status_code","psf_status_code_all")
biostatus_merge <- merge(x = biostatus_old[,columns],
                         y = biostatus_new[,columns],
                         by = c("region","species_name","species_qualified","cuid","cu_name_pse"))

head(biostatus_merge)

cond_x <- grepl("\\.x",colnames(biostatus_merge))
colnames(biostatus_merge)[cond_x] <- gsub("\\.x","_old",colnames(biostatus_merge)[cond_x])

cond_y <- grepl("\\.y",colnames(biostatus_merge))
colnames(biostatus_merge)[cond_y] <- gsub("\\.y","_new",colnames(biostatus_merge)[cond_y])

# order columns
biostatus_merge <- biostatus_merge[,c("region","species_name","species_qualified","cuid","cu_name_pse",
                                      "psf_status_old","psf_status_new",
                                      "psf_status_type_old","psf_status_type_new",
                                      "curr_spw_start_year_old","curr_spw_start_year_new",
                                      "curr_spw_end_year_old","curr_spw_end_year_new",
                                      "psf_status_code_old","psf_status_code_new",
                                      "psf_status_code_all_old","psf_status_code_all_new")]

# add the new psf_status_code_all
biostatus_merge$psf_status_code_all_new <- sapply(biostatus_merge$cuid,function(cuid){
  cond <- biostatus_new$cuid == cuid
  return(biostatus_new$psf_status_code_all[cond])
})


#'* CUs in old but not in new dataset and vice versa *
cuid_old <- biostatus_old$cuid
cuid_new <- biostatus_new$cuid
cuid_old[!cuid_old %in% cuid_new] # none 241 188 521 751
cond <- biostatus_old$cuid %in% cuid_old[!cuid_old %in% cuid_new]
biostatus_old[cond,] 
# 188 Swan/Club: all good, was supposed to be removed
# https://salmonwatersheds.slack.com/archives/CKNVB4MCG/p1713985785863399?thread_ts=1709839326.139849&cid=CKNVB4MCG

# 521  Brim-Wahoo  all good, was supposed to be removed
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1725336203245259?thread_ts=1717434872.482819&cid=CJ5RVHVCG
cuid_new[!cuid_new %in% cuid_old] # none

# 241 & 751 All good
# https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1725896435990229?thread_ts=1725564850.867719&cid=C03LB7KM6JK


#'* cases where curr_spw_end_year_new < curr_spw_end_year_old *
cond <- biostatus_merge$curr_spw_end_year_new < biostatus_merge$curr_spw_end_year_old &
  !is.na(biostatus_merge$curr_spw_end_year_new) &
  !is.na(biostatus_merge$curr_spw_end_year_old)
sum(cond) # 0 34
biostatus_merge[cond,]

#' REASON:  OLD
#' Because the field curr_spw_end_year in cuspawnerabundance.csv used to be the
#' year of the update and not necessarily the last year with available data.
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1707360348221989?thread_ts=1707332952.867199&cid=CJ5RVHVCG

#'* Condition same status *
cond_same <- biostatus_merge$psf_status_old == biostatus_merge$psf_status_new
sum(cond_same) # 378 432 370 369 362 346
biostatus_merge[cond_same,] # |> View()

# with biostatus assessed:
cond_123 <- biostatus_merge$psf_status_old[cond_same] %in% c("poor","fair","goog")
sum(cond_123) # 76 87 48 44

# with same method used:
cond_method_same <- biostatus_merge[cond_same,][cond_123,]$psf_status_type_old ==  biostatus_merge[cond_same,][cond_123,]$psf_status_type_new
sum(cond_method_same) # 70 85 46 43

biostatus_merge[cond_same,][cond_123,][cond_method_same,]
biostatus_merge[cond_same,][cond_123,][!cond_method_same,]

#'* Different status *
cond_diff <- biostatus_merge$psf_status_old != biostatus_merge$psf_status_new
sum(cond_diff) # 41 31 93 103 120

biostatus_merge[cond_diff,c("region","species_qualified","cuid","cu_name_pse","psf_status_old","psf_status_new")]

cases <- unique(biostatus_merge[cond_diff,c("psf_status_old","psf_status_new")])
cases

count <- 0
count_max <- sum(cond_diff) 

# record the CUs that are different
biostatus_merge_diff <- biostatus_merge[cond_diff,]
biostatus_merge_diff$type_diff <- NA
biostatus_merge_diff$explanation <- "?"

cuid_diff <- biostatus_merge_diff$cuid

#'* Different methods (and diff status) and why (7 out of 103) *
cond_method_NA_new <- is.na(biostatus_merge$psf_status_type_new)
cond_method_NA_old <- is.na(biostatus_merge$psf_status_type_old)
cond_sr_new <- biostatus_merge$psf_status_type_new == "sr" & !cond_method_NA_new
cond_sr_old <- biostatus_merge$psf_status_type_old == "sr" & !cond_method_NA_old
cond_percent_new <- biostatus_merge$psf_status_type_new == "percentile" & !cond_method_NA_new
cond_percent_old <- biostatus_merge$psf_status_type_old == "percentile" & !cond_method_NA_old
cond_diff_method <- (cond_sr_new & cond_percent_old) | (cond_sr_old & cond_percent_new)

# nb of CUs with different methods overall:
sum(cond_diff_method) # 8 2 14 11 16
biostatus_merge[cond_diff_method,c("region","species_qualified","cuid","cu_name_pse",
                                   "psf_status_type_old","psf_status_type_new",
                                   "psf_status_old","psf_status_new","curr_spw_end_year_old","curr_spw_end_year_new")]

biostatus_merge$curr_spw_end_year_old

# nb of CUs with different method and output
sum(cond_diff_method & cond_diff) # 0 7 8
biostatus_merge[cond_diff_method & cond_diff,]

count <- count + sum(cond_diff_method & cond_diff)

# 1) sr to percentile (13 overall, 7 with different biostatus)
cond_sr_to_percent <- cond_sr_old & cond_percent_new
sum(cond_sr_to_percent) # 13 10 14
sum(cond_sr_to_percent & cond_diff) # 7
cuid <- biostatus_merge[cond_sr_to_percent & cond_diff,]$cuid
cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "sr to percentile"

# reasons 1: rule 2: catch estimate medium-low or higher : 6 CUs
cond_390_cuid <- dataset390_output$cuid %in% cuid
cond_cuid_catchLow <- dataset390_output$catch_method[cond_390_cuid] == 1
sum(cond_cuid_catchLow) # 3
cuid_catchLow <- dataset390_output[cond_390_cuid,]$cuid[cond_cuid_catchLow]

cond_cuid <- biostatus_merge_diff$cuid %in% cuid_catchLow
biostatus_merge_diff$explanation[cond_cuid] <- "rule 2: catch method quality = 1 ?"

# reason 2:  I do not see what other reason by just comparing to the old decision
# tree
# https://www.dropbox.com/scl/fi/slxgndryjp43z3lay2mfh/Status_assessment_flowchart_v15.pptx?rlkey=3qf7cpijleegrm9mpri5qfoxt&dl=0

# 2) percentile to sr (1 overall, 0 with different biostatus)
cond_percent_to_sr <- cond_sr_new & cond_percent_old
sum(cond_percent_to_sr) # 1 2
sum(cond_percent_to_sr & cond_diff) # 1
cuid <- biostatus_merge[cond_percent_to_sr & cond_diff,]$cuid
cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "percentile to sr"

# reasons 1: rule 2: catch estimate medium-low or higher : 
cond_390_cuid <- dataset390_output$cuid %in% cuid
cond_cuid_catchLow <- dataset390_output$catch_method[cond_390_cuid] != 1 & !is.na(dataset390_output$catch_method[cond_390_cuid])
sum(cond_cuid_catchLow) # 0 1
cuid_catchLow <- dataset390_output[cond_390_cuid,]$cuid[cond_cuid_catchLow]

cond_cuid <- biostatus_merge_diff$cuid %in% cuid_catchLow
biostatus_merge_diff$explanation[cond_cuid] <- "rule 2: catch method quality != 1 ?"

#'* Biostatus improved or worsened (same method) (23 + 11 out of 103) *

cond_same_method <- (cond_sr_old & cond_sr_new) | (cond_percent_old & cond_percent_new)

# Improved biostatus
cond_diff_status123_improve <- (biostatus_merge$psf_status_old == "poor" & 
                                  biostatus_merge$psf_status_new %in% c("fair","good")) |
  (biostatus_merge$psf_status_old == "fair" & 
     biostatus_merge$psf_status_new == "good")
sum(cond_same_method & cond_diff_status123_improve) # 2 23 21 29
biostatus_merge[cond_same_method & cond_diff_status123_improve,] # |> View()
cuid <- biostatus_merge[cond_same_method & cond_diff_status123_improve,]$cuid
cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "same method, biostatus improved"


# Worsened biostatus
cond_diff_status123_worsened <- (biostatus_merge$psf_status_new == "poor" & 
                                   biostatus_merge$psf_status_old %in% c("fair","good")) |
  (biostatus_merge$psf_status_new == "fair" & 
     biostatus_merge$psf_status_old == "good")
sum(cond_same_method & cond_diff_status123_worsened) # 7 11 12
biostatus_merge[cond_same_method & cond_diff_status123_worsened,] # |> View()
cuid <- biostatus_merge[cond_same_method & cond_diff_status123_worsened,]$cuid
cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "same method, biostatus worsened"

# Both
cond_diff_status123 <- cond_same_method & (cond_diff_status123_improve | cond_diff_status123_worsened)
sum(cond_diff_status123) # 9 34 32 41
biostatus_merge[cond_diff_status123,]
cuid <- biostatus_merge[cond_diff_status123,]$cuid
cond_cuid <- biostatus_merge_diff$cuid %in% cuid

count <- count + sum(cond_diff_status123)

# Reason 1: updated spawner data --> more recent (20)
cond_update <- biostatus_merge_diff$curr_spw_end_year_new[cond_cuid] > biostatus_merge_diff$curr_spw_end_year_old[cond_cuid]
sum(cond_update) # 20 19
biostatus_merge_diff[cond_cuid,]$explanation[cond_update] <- "updated estimated spawner abundance"


# Reason 2 (not mutually exclusive): wrong number of years considered in current spawner abundance (2)
sum(!cond_update) # 12 22
cuid <- biostatus_merge[cond_diff_status123,]$cuid
wrong_nb_yr <- sapply(cuid, function(c){
  # c <- cuid[1]
  cond <- conservationunits_decoder$cuid == c
  gen_length <- conservationunits_decoder$gen_length[cond]
  
  cond <- biostatus_merge_diff$cuid == c
  curr_spw_start_yr <- biostatus_merge_diff$curr_spw_start_year_old[cond]
  curr_spw_end_yr <- biostatus_merge_diff$curr_spw_end_year_old[cond]
  nb_yr <- curr_spw_end_yr - curr_spw_start_yr + 1
  
  if(nb_yr != gen_length){
    out <- T
  }else{
    out <- F
  }
  return(out)
})

cuid_wrong <- cuid[wrong_nb_yr]
cond <- biostatus_merge_diff$cuid %in% cuid_wrong
biostatus_merge_diff$type_diff[cond]
explanation <- biostatus_merge_diff$explanation[cond]
explanation <- paste(explanation,"wrong nb year for curr spw abun",sep = "; ")
explanation <- gsub("\\?; ","",explanation)

biostatus_merge_diff$explanation[cond] <- explanation

# Other potential reasons: ?

#'* data-deficient --> status (12) *
cond_dataDeff_biostatus <- biostatus_merge$psf_status_old == "data-deficient" & 
  biostatus_merge$psf_status_new %in% c("poor","fair","good")
sum(cond_dataDeff_biostatus) # 5 12 13 42
biostatus_merge[cond_dataDeff_biostatus,] # |> View()
cuid <- biostatus_merge$cuid[cond_dataDeff_biostatus]
cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "data-deficient to biostatus"

# cases with percentile and red status --> 
# biostatus_merge$psf_status_code_all_new == "3, 6"
# cond_3 <- grepl("3",biostatus_merge$psf_status_code_all_new)
# cond_6 <- grepl("6",biostatus_merge$psf_status_code_all_new)
# sum(cond_3 & cond_6) # 10

sum(biostatus_merge$psf_status_code_all_new == "3, 6") # 1

biostatus_merge_diff$explanation[cond_cuid]

count <- count + sum(cond_dataDeff_biostatus)

#'* data-deficient --> not-assessed (17) *
cond <- biostatus_merge$psf_status_old == "data-deficient" & 
  biostatus_merge$psf_status_new == "not-assessed"
sum(cond) # 17 27
biostatus_merge[cond,]
# the "not assessed" status is new and corresponds to the following cases:
# Not assessed if:
# - cyclic (SR) --> 5
# - low pro / high expl --> 6
biostatus_merge$psf_status_code_all_new[cond] |> unique() # make sense
cuid_dataDeff_notAssess <- biostatus_merge$cuid[cond]

cond_cuid <- biostatus_merge_diff$cuid %in% cuid_dataDeff_notAssess
biostatus_merge_diff$type_diff[cond_cuid] <- "data-deficient to not-assessed"
biostatus_merge_diff$explanation[cond_cuid] <- "new terminology for rule 5 and 6"

count <- count + sum(cond)

#'* extinct --> status (1)*
cond <- biostatus_merge$psf_status_old == "extinct" & 
  biostatus_merge$psf_status_new %in% c("poor","fair","good")
sum(cond) # 1
biostatus_merge[cond,]
cuid <- biostatus_merge$cuid[cond]

cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "extinct to biostatus"
biostatus_merge_diff$explanation[cond_cuid] <- "de novo"

count <- count + sum(cond)

#'* extinct --> data-deficient (2)*
cond <- biostatus_merge$psf_status_old == "extinct" & 
  biostatus_merge$psf_status_new == "data-deficient"
sum(cond) # 2 1
biostatus_merge[cond,]
#' --> cyclic or high exploit/low prod

cuid <- biostatus_merge$cuid[cond]

cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "extinct to data-deficient"
biostatus_merge_diff$explanation[cond_cuid] <- "no longer extinct but not enough data"

count <- count + sum(cond)


#'* status --> data-deficient (21)*
cond <- biostatus_merge$psf_status_old %in% c("poor","fair","good") & 
  biostatus_merge$psf_status_new == "data-deficient"
sum(cond) # 21
biostatus_merge[cond,]

cuid <- biostatus_merge$cuid[cond]

cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "status to data-deficient"
biostatus_merge_diff$explanation[cond_cuid] <- "related to new rule for status code 8"


count <- count + sum(cond)


count == count_max


#'  - 1 = good
#'  - 2 = fair
#'  - 3 = poor
#'  - 4 = extinct
#'  - 5 = not-assessed (cyclic dominance)
#'  - 6 = not-assessed (low productivity or high exploitation)
#'  - 7 = data-deficient (insufficient time series length)
#'  - 8 = data-deficient (no estimates of spawner abundance in the most recent generation)
#'  - 9 = data-deficient (no spawner estimates available)

# cuid <- 603
# plot_spawnerAbundance_benchmarks_fun(cuid = cuid,
#                                      cuspawnerabundance = cuspawnerabundance,
#                                      dataset101_output = biostatus_new,
#                                      dataset102_output = benchmarks_new,
#                                      #dataset103_output = cuspawnerabund_smooth,
#                                      conservationunits_decoder = conservationunits_decoder,
#                                      figure_print = F, # figure_print,
#                                      wd_figures = wd_figures)


data <- Sys.Date()
write.csv(biostatus_merge_diff,
          paste0(wd_output,"/archive/biostatus_new_old_differences_",data,".csv"),
          row.names = F)

