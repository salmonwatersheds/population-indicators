

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

#'* Import the old biostatus & benchmarks from legacy site *

biostatus_old <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                            pattern = "dataset101_biological_status",
                                            second_last = T)

head(biostatus_old)
nrow(biostatus_old) # 463

colnames(biostatus_old)

benchmarks_old <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                             pattern = "dataset102_benchmarks",
                                             second_last = T)

# Old format updates - TO keep in case there is a new to compare datasets before
# the 2024-12-18 version
# colnames(biostatus_old)[colnames(biostatus_old) == "species_abbr"] <- "species_qualified"
# colnames(benchmarks_old)[colnames(benchmarks_old) == "species_abbr"] <- "species_qualified"

# biostatus_old$species_name[grepl("[S|s]ockeye",biostatus_old$species_name)] <- "Sockeye"
# biostatus_old$species_name[grepl("Pink",biostatus_old$species_name)] <- "Pink"
# benchmarks_old$species_name[grepl("[S|s]ockeye",benchmarks_old$species_name)] <- "Sockeye"
# benchmarks_old$species_name[grepl("Pink",benchmarks_old$species_name)] <- "Pink"

# TEMPORARY CHANGE
# the Henderson lake sockeye CU in VIMI is now called Hucuktlis
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1747060690168909
cond <- biostatus_old$cuid == 953
biostatus_old[cond,]
biostatus_old[cond,]$cu_name_pse <- "Hucuktlis"

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


datasetsNames_database <- datasetsNames_database_fun()
fromDatabase <- update_file_csv <- F

#'* Import conservationunits_decoder for generation length *
conservationunits_decoder <- datasets_database_fun(nameDataSet = "conservationunits_decoder.csv",
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

# remove the SMU-related information because that causes issues lower (one CU belongs to 2 SMUs)
nrow(conservationunits_decoder) # 470
conservationunits_decoder <- conservationunits_decoder[,!grepl("smu",colnames(conservationunits_decoder))]
conservationunits_decoder <- unique(conservationunits_decoder)
nrow(conservationunits_decoder) # 469

length(unique(conservationunits_decoder$pooledcuid)) # 463


#'* Import the current spawner abundance (for generation length) *
# cuspawnerabundance <- datasets_database_fun(nameDataSet = "cuspawnerabundance.csv",
#                                             fromDatabase = fromDatabase,
#                                             update_file_csv = update_file_csv,
#                                             wd = wd_pop_indic_data_input_dropbox)

#'* Import dataset390_output for survey_quality (for catch_method quality and decision tree) *
dataset390_output <- datasets_database_fun(nameDataSet = "dataset390_data_quality.csv", # see datasetsNames_database$name_CSV for names
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)

dataset390_output <- dataset390_output[,c("region","species_name","cuid","cu_name_pse","catch_method")]
head(dataset390_output)

nrow(dataset390_output) # 463 465 466


#'* Import the list of priors for Smax (produced in 1a_HBSRM.R) *
priors_Smax <- read.csv(paste0(wd_data,"/priors_Smax.csv"),header = T)

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
                         by = c("region","species_name","species_qualified","cuid","cu_name_pse"), 
                         all = F)

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
unique(biostatus_merge$region)
nrow(biostatus_merge) # 463

#'* CUs in old but not in new dataset and vice versa *
cuid_old <- biostatus_old$cuid
cuid_new <- biostatus_new$cuid
cuid_old[!cuid_old %in% cuid_new] # none 241 188 521 751
cond <- biostatus_old$cuid %in% cuid_old[!cuid_old %in% cuid_new]
biostatus_old[cond,] 

cuid_new[!cuid_new %in% cuid_old] # none

# Old comments:
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
sum(cond_same) # 450 436 442 378 432 370 369 362 346
biostatus_merge[cond_same,] # |> View()

# with biostatus assessed:
cond_123 <- biostatus_merge$psf_status_old[cond_same] %in% c("poor","fair","goog")
sum(cond_123) # 129 106 88 76 87 48 44

# and with same method used:
cond_method_same <- biostatus_merge[cond_same,][cond_123,]$psf_status_type_old ==  biostatus_merge[cond_same,][cond_123,]$psf_status_type_new
sum(cond_method_same) # 128 106 88 70 85 46 43

biostatus_merge[cond_same,][cond_123,][cond_method_same,]
biostatus_merge[cond_same,][cond_123,][!cond_method_same,]

#'* Different status *
cond_diff <- biostatus_merge$psf_status_old != biostatus_merge$psf_status_new
sum(cond_diff) # 13 27 21 41 31 93 103 120

show <- biostatus_merge[cond_diff,c("region","species_qualified","cuid","cu_name_pse",
                            "psf_status_old","psf_status_new",
                            "psf_status_code_old","psf_status_code_new")]

show$curr_spw_end_year <- sapply(show$cuid,function(cuid){
  # cuid <- show$cuid[1]
  cond <- benchmarks_new$cuid == cuid
  curr_spw_end_year_new <- benchmarks_new$curr_spw_end_year[cond]
  cond <- benchmarks_old$cuid == cuid
  curr_spw_end_year_old <- benchmarks_old$curr_spw_end_year[cond]
  
  if(is.na(curr_spw_end_year_old) & !is.na(curr_spw_end_year_new)){
    out <- "curr_spw_end_year_old is NA"
  }else if(!is.na(curr_spw_end_year_old) & is.na(curr_spw_end_year_new)){
    out <- "curr_spw_end_year_new is NA"
  }else if(curr_spw_end_year_new != curr_spw_end_year_old){
    out <- paste0(curr_spw_end_year_old,"(old) < ",curr_spw_end_year_new," (new)")
  }else{
   out <- curr_spw_end_year_old
  }
  return(out)
})

show$gen_length <- sapply(show$cuid,function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  out <- conservationunits_decoder$gen_length[cond]
  return(out)
})

show
nrow(show) # 13 27

cases <- unique(biostatus_merge[cond_diff,c("psf_status_old","psf_status_new")])
cases$count <- apply(cases,1,function(r){
  cond <- biostatus_merge[cond_diff,]$psf_status_old == r["psf_status_old"] &
    biostatus_merge[cond_diff,]$psf_status_new == r["psf_status_new"]
  return(sum(cond))
})
cases
sum(cases$count) # 13 27

# record the CUs that are different
biostatus_merge_diff <- biostatus_merge[cond_diff,]
biostatus_merge_diff$type_diff <- NA
biostatus_merge_diff$explanation <- "?"

cuid_diff <- biostatus_merge_diff$cuid

#'* Different status: Different methods (psf_status_type) and why *
cond_method_NA_new <- is.na(biostatus_merge$psf_status_type_new)
cond_method_NA_old <- is.na(biostatus_merge$psf_status_type_old)
cond_sr_new <- biostatus_merge$psf_status_type_new == "sr" & !cond_method_NA_new
cond_sr_old <- biostatus_merge$psf_status_type_old == "sr" & !cond_method_NA_old
cond_percent_new <- biostatus_merge$psf_status_type_new == "percentile" & !cond_method_NA_new
cond_percent_old <- biostatus_merge$psf_status_type_old == "percentile" & !cond_method_NA_old
cond_absolute_new <- biostatus_merge$psf_status_type_new == "absolute" & !cond_method_NA_new
cond_absolute_old <- biostatus_merge$psf_status_type_old == "absolute" & !cond_method_NA_old
cond_diff_method <- (cond_sr_new & cond_percent_old) | 
  (cond_sr_old & cond_percent_new) | 
  (cond_absolute_new & !cond_absolute_old) | 
  (!cond_absolute_new & cond_absolute_old)

# nb of CUs with different methods overall:
sum(cond_diff_method) # 8 8 2 14 11 16
biostatus_merge[cond_diff_method,c("region","species_qualified","cuid","cu_name_pse",
                                   "psf_status_type_old","psf_status_type_new",
                                   "psf_status_old","psf_status_new","curr_spw_end_year_old","curr_spw_end_year_new")]

# make sense --> Teslin CK & CM we now use other data and percentile

cuid <- biostatus_merge$cuid[cond_diff_method]
cond <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff[cond,]

# nb of CUs with different method and output
sum(cond_diff_method) # 8 0 7 8
biostatus_merge[cond_diff_method,]

# 1) sr to percentile (13 overall, 7 with different biostatus)
cond_sr_to_percent <- cond_sr_old & cond_percent_new
sum(cond_sr_to_percent) # 1 13 10 14
sum(cond_sr_to_percent & cond_diff) # 1 7
cuid <- biostatus_merge[cond_sr_to_percent & cond_diff,]$cuid
cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "sr to percentile"
biostatus_merge_diff[cond_cuid,] 

# reasons 1: rule 2: catch estimate medium-low or higher : 6 CUs
cond_390_cuid <- dataset390_output$cuid %in% cuid
cond_cuid_catchLow <- dataset390_output$catch_method[cond_390_cuid] == 1
sum(cond_cuid_catchLow) # 0 3
cuid_catchLow <- dataset390_output[cond_390_cuid,]$cuid[cond_cuid_catchLow]

cond_cuid <- biostatus_merge_diff$cuid %in% cuid_catchLow
biostatus_merge_diff$explanation[cond_cuid] <- "rule 2: catch method quality = 1 ?"

# reason 2:  I do not see what other reason by just comparing to the old decision
# tree
# https://www.dropbox.com/scl/fi/slxgndryjp43z3lay2mfh/Status_assessment_flowchart_v15.pptx?rlkey=3qf7cpijleegrm9mpri5qfoxt&dl=0

# reason 3: for CK Teslin: we decided to use another source of data and the percentile metod
cond <- biostatus_merge_diff$cuid == 1212
biostatus_merge_diff[cond,]$explanation <- "used altenative spawner data"

# reason 4: more recent data and absolute benchmark
cond <- biostatus_merge$psf_status_type_new == "Absolute" & !is.na(biostatus_merge$psf_status_type_new) &
  biostatus_merge$psf_status_type_old != "Absolute" & !is.na(biostatus_merge$psf_status_type_old)
cuid <- biostatus_merge[cond & cond_diff,]$cuid
cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff[cond_cuid,]
biostatus_merge_diff$type_diff[cond_cuid] <- "relative to absolute"
biostatus_merge_diff$explanation[cond_cuid] <- "updated estimated spawner abundance"

# 2) percentile to sr
cond_percent_to_sr <- cond_sr_new & cond_percent_old
sum(cond_percent_to_sr) # 0 1 2
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

#'* Different status & Biostatus improved or worsened (same method) *

cond_same_method <- (cond_sr_old & cond_sr_new) | (cond_percent_old & cond_percent_new)

# Improved biostatus
cond_diff_status123_improve <- (biostatus_merge$psf_status_old == "poor" & 
                                  biostatus_merge$psf_status_new %in% c("fair","good")) |
  (biostatus_merge$psf_status_old == "fair" & 
     biostatus_merge$psf_status_new == "good")
sum(cond_same_method & cond_diff_status123_improve) # 5 2 23 21 29
biostatus_merge[cond_same_method & cond_diff_status123_improve,] # |> View()
cuid <- biostatus_merge[cond_same_method & cond_diff_status123_improve,]$cuid
cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "same method, biostatus improved"

biostatus_merge_diff[cond_cuid,]

# check current spawner abundance
cond_new <- benchmarks_new$cuid %in% cuid
cond_old <- benchmarks_old$cuid %in% cuid
merge(x = benchmarks_new[cond_new,c("region","species_qualified","cuid","cu_name_pse","curr_spw")], 
      y = benchmarks_old[cond_old,c("region","species_qualified","cuid","cu_name_pse","curr_spw")],
      by = c("region","species_qualified","cuid","cu_name_pse"))
# --> same... so why is status different?!

# check if it could be because of new priors --> YES
cuid[cuid %in% priors_Smax$cuid]
biostatus_merge_diff$explanation[cond_cuid] <- "new priors"

# Worsened biostatus
cond_diff_status123_worsened <- (biostatus_merge$psf_status_new == "poor" & 
                                   biostatus_merge$psf_status_old %in% c("fair","good")) |
  (biostatus_merge$psf_status_new == "fair" & 
     biostatus_merge$psf_status_old == "good")
sum(cond_same_method & cond_diff_status123_worsened) # 9 7 11 12
biostatus_merge[cond_same_method & cond_diff_status123_worsened,] # |> View()
cuid <- biostatus_merge[cond_same_method & cond_diff_status123_worsened,]$cuid
cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "same method, biostatus worsened"
biostatus_merge_diff[cond_cuid,]

# reason 1: new data
cond_data_more_recent <- biostatus_merge_diff$curr_spw_end_year_new > biostatus_merge_diff$curr_spw_end_year_old
biostatus_merge_diff[cond_cuid & cond_data_more_recent,]$explanation <- "updated estimated spawner abundance"

# reason 2: new priors?
cuid_remain <- cuid[!cuid %in% biostatus_merge_diff[cond_cuid & cond_data_more_recent,]$cuid]
cuid_remain
cuid_remain[cuid_remain %in% priors_Smax$cuid]
cuid_prior <- cuid_remain[cuid_remain %in% priors_Smax$cuid]
cond <- biostatus_merge_diff$cuid %in% cuid_prior
biostatus_merge_diff$explanation[cond] <- "new priors"

# reason 3 for Stephens: --> because we used a prior before, that was certainly only
# for Stephens and not Swan, whereas now we determine Smax based on time series of
# S, which is right because we grouped Stephens and Swan into Stephens
cond <- biostatus_merge_diff$cuid == 187
biostatus_merge_diff[cond,]$explanation <- "no more use of (wrong) prior"

# Both
cond_diff_status123 <- cond_same_method & (cond_diff_status123_improve | cond_diff_status123_worsened)
sum(cond_diff_status123) # 14 9 34 32 41
biostatus_merge[cond_diff_status123,]
cuid <- biostatus_merge[cond_diff_status123,]$cuid
cond_cuid <- biostatus_merge_diff$cuid %in% cuid

# Reason 1: updated spawner data --> more recent OLD
# cond_update <- biostatus_merge_diff$curr_spw_end_year_new[cond_cuid] > biostatus_merge_diff$curr_spw_end_year_old[cond_cuid]
# sum(cond_update) # 5 20 19
# biostatus_merge_diff[cond_cuid,]$explanation[cond_update] <- "updated estimated spawner abundance"

# Reason 2 (not mutually exclusive): wrong number of years considered in current spawner abundance (2) OLD
# AND I DO NOT UNDERSTAND THE REASONING 
# sum(!cond_update) # 9 12 22
# cuid <- biostatus_merge[cond_diff_status123,]$cuid
# wrong_nb_yr <- sapply(cuid, function(c){
#   # c <- 532
#   cond <- conservationunits_decoder$cuid == c
#   gen_length <- conservationunits_decoder$gen_length[cond]
#   
#   cond <- biostatus_merge_diff$cuid == c
#   curr_spw_start_yr <- biostatus_merge_diff$curr_spw_start_year_old[cond]
#   curr_spw_end_yr <- biostatus_merge_diff$curr_spw_end_year_old[cond]
#   nb_yr <- curr_spw_end_yr - curr_spw_start_yr + 1
#   
#   if(nb_yr != gen_length){
#     out <- T
#   }else{
#     out <- F
#   }
#   return(out)
# })
# 
# cuid_wrong <- cuid[wrong_nb_yr]
# cond <- biostatus_merge_diff$cuid %in% cuid_wrong
# biostatus_merge_diff[cond,]
# biostatus_merge_diff$type_diff[cond]
# explanation <- biostatus_merge_diff$explanation[cond]
# explanation <- paste(explanation,"wrong nb year for curr spw abun",sep = "; ")
# explanation <- gsub("\\?; ","",explanation)
# 
# biostatus_merge_diff$explanation[cond] <- explanation

# Other potential reasons: ?

#'*  Different status: data-deficient --> status *
cond_dataDeff_biostatus <- biostatus_merge$psf_status_old == "data-deficient" & 
  biostatus_merge$psf_status_new %in% c("poor","fair","good")
sum(cond_dataDeff_biostatus) # 8 5 12 13 42
biostatus_merge[cond_dataDeff_biostatus,] # |> View()
cuid <- biostatus_merge$cuid[cond_dataDeff_biostatus]
cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "data-deficient to biostatus"

# cases with percentile and red status --> 
# biostatus_merge$psf_status_code_all_new == "3, 6"
# cond_3 <- grepl("3",biostatus_merge$psf_status_code_all_new)
# cond_6 <- grepl("6",biostatus_merge$psf_status_code_all_new)
# sum(cond_3 & cond_6) # 10

# reason: data was not available (code = 9)
# That's all of them
cond <- biostatus_merge_diff$psf_status_code_old == 9 & biostatus_merge_diff$psf_status_code_new != 9
biostatus_merge_diff[cond_cuid & cond,]
biostatus_merge_diff[cond_cuid & cond,]$explanation <- "no data was available"

#'*  Different status: data-deficient --> not-assessed *
cond <- biostatus_merge$psf_status_old == "data-deficient" & 
  biostatus_merge$psf_status_new == "not-assessed"
sum(cond) # 0 17 27
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

#'*  Different status: not-assessed --> data-deficient *
cond <- biostatus_merge$psf_status_new == "data-deficient" & 
  biostatus_merge$psf_status_old == "not-assessed"
sum(cond) # 0 17 27
biostatus_merge[cond,]

biostatus_merge$psf_status_code_all_new[cond] |> unique() # make sense
cuid_dataDeff_notAssess <- biostatus_merge$cuid[cond]

cond_cuid <- biostatus_merge_diff$cuid %in% cuid_dataDeff_notAssess
biostatus_merge_diff$type_diff[cond_cuid] <- " not-assessed to data-deficient"
biostatus_merge_diff$explanation[cond_cuid] <- "rule 6 and rule 8 --> rule 8"

#'*  Different status: extinct --> status *
cond <- biostatus_merge$psf_status_old == "extinct" & 
  biostatus_merge$psf_status_new %in% c("poor","fair","good")
sum(cond) # 0 1
biostatus_merge[cond,]
cuid <- biostatus_merge$cuid[cond]

cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "extinct to biostatus"
biostatus_merge_diff$explanation[cond_cuid] <- "de novo"

#'* Different status: extinct --> data-deficient *
cond <- biostatus_merge$psf_status_old == "extinct" & 
  biostatus_merge$psf_status_new == "data-deficient"
sum(cond) # 0 2 1
biostatus_merge[cond,]
#' --> cyclic or high exploit/low prod

cuid <- biostatus_merge$cuid[cond]

cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "extinct to data-deficient"
biostatus_merge_diff$explanation[cond_cuid] <- "no longer extinct but not enough data"

#'* Different status: status --> data-deficient *
cond <- biostatus_merge$psf_status_old %in% c("poor","fair","good") & 
  biostatus_merge$psf_status_new == "data-deficient"
sum(cond) # 1 21
biostatus_merge[cond,]

cuid <- biostatus_merge$cuid[cond]

#' Reason: it was decided to exclude CU 216 Skeena River-High Interior (river-type)
#' from applying the absolute benchmark because of uncertainty in the data
#' cf. Pop meeting 02/06/2025 (June)
cond_cuid <- biostatus_merge_diff$cuid %in% cuid
biostatus_merge_diff$type_diff[cond_cuid] <- "status to data-deficient"
biostatus_merge_diff$explanation[cond_cuid] <- "exception made"

#'* Different status: not-assessed --> status *
cond <- biostatus_merge$psf_status_old == "not-assessed" & 
  biostatus_merge$psf_status_new %in% c("poor","fair","good")
sum(cond) # 1
biostatus_merge[cond,]

# reason: rule: show status if red with percentile with high exploitation / 
# low productivity
cond <- biostatus_merge_diff$psf_status_code_all_new == "3, 6" & biostatus_merge_diff$psf_status_code_all_old == "6"
biostatus_merge_diff[cond,]
biostatus_merge_diff[cond,]$type_diff <- "not-assessed to status"
biostatus_merge_diff[cond,]$explanation <- "high exploit / low prod & red percentile status"

# check if there is any left unexplained:
cond <- biostatus_merge_diff$explanation == "?"
biostatus_merge_diff[cond,]

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

