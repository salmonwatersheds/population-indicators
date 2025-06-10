
#'******************************************************************************
#' The goal of the script is to combine the HBSRM and percentiles biostatus and 
#' define the corresponding psf_status_code and psf_status fields.
#' 
#' Files imported:
#' - REGION_SPECIES_biological_status_HBSRM.csv        (created in 2a_benchmarks_HBSRM.R)
#' - REGION_SPECIES_benchmarks_summary_HBSRM.csv       (created in 2a_benchmarks_HBSRM.R)
#' - REGION_SPECIES_biological_status_percentiles.csv  (created in 1b_benchmarks_percentiles.R)
#' - REGION_SPECIES_benchmarks_summary_HBSRM.csv       (created in 1b_benchmarks_percentiles.R)
#' - conservationunits_decoder.csv                     (from database)
#' - dataset390_output.csv                             (from database) CU-level data quality
#' - cuspawnerabundance.csv                            (from database) observed spawner abundance
#' - code_PSF_Status.csv                               (from database) different biostatus values and associated code
#' - cu_highExploit_lowProd.csv                        The CUs with high exploitation or low productivity (the dataframe is generated in function.R for now but will eventually be imported)
#' 
#' Files produced: 
#' - output/archive/dataset101_biological_status_YYYY-MM-DD.csv #
#' - output/archive/dataset102_benchmarks_YYYY-MM-DD.csv        # 
#' - output/dataset101_biological_status.csv # same as above but exported locally to be pushed to github
#' - output/dataset102_benchmarks.csv        # # same as above but exported locally to be pushed to github
#' 
#' Resources/Notes:
#' - Steph's diagram for decision rules
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

#' Include SR benchmarks for cyclic CUs
#' - if true the HBSR Ricker model was applied to the cyclic CUs to determine 
#' their biostatus
#' - if false, these CUs have a psf_status_code 5 = not-assessed
cyclic_biostatus <- T

#
# Import Datasets -----

# Import species names and acronyms
species_acronym_df <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

# select all the regions
region <- as.character(regions_df[1,])

# select certain species
# species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "CK"],    
#              species_acronym_df$species_name[species_acronym_df$species_acro == "SX"])

# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- TRUE

#'* Import benchmark values for the HBSRM method *
pattern <- "benchmarks_summary_HBSRM"
benchmarks_HBSRM <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                  wd_output = paste0(wd_output,"/intermediate"),
                                                  region = region,
                                                  species_all = species_all,
                                                  term_exclude = "cyclic")

#' check that the cyclic CUs are not in benchmarks_HBSRM
cond <- grepl("cyclic",benchmarks_HBSRM$CU)
benchmarks_HBSRM[cond,]
nrow(benchmarks_HBSRM) # 612 616

if(cyclic_biostatus){
  # Import the Ricker benchmarks down with the cycle CUs
  benchmarks_HBSRM_c <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = paste0(wd_output,"/intermediate"),
                                                      region = region,
                                                      species_all = species_all,
                                                      term_include = "cyclic", 
                                                      term_exclude = "Larkin")
  
  # Merge the two
  benchmarks_HBSRM <- rbind(benchmarks_HBSRM,benchmarks_HBSRM_c[,colnames(benchmarks_HBSRM)])
}
nrow(benchmarks_HBSRM) # 636 640

#'* Import benchmark values for the percentile method *
pattern <- "benchmarks_summary_percentiles"
benchmarks_percentile <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                       wd_output = paste0(wd_output,"/intermediate"),
                                                       region = region,
                                                       species_all = species_all,
                                                       term_exclude = "cyclic")
nrow(benchmarks_percentile) # 1341 1347

#'* Import biostatus obtained with HBSR Sgen - Smsy: *
pattern <- "biological_status_HBSRM"
biological_status_HBSRM <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                         wd_output = paste0(wd_output,"/intermediate"),
                                                         region = region,
                                                         species_all = species_all,
                                                         term_exclude = "cyclic")
nrow(biological_status_HBSRM) # 153 154 145 138 144 143 ;  137
head(biological_status_HBSRM)
colnames(biological_status_HBSRM)

unique(biological_status_HBSRM$comment)

# check that there is no cyclic CUs in biological_status_HBSRM
cond <- grepl("cyclic",biological_status_HBSRM$cu_name_pse)
biological_status_HBSRM[cond,]

if(cyclic_biostatus){
  # Import the Ricker benchmark values for cyclic CUs
  biological_status_HBSRM_c <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                             wd_output = paste0(wd_output,"/intermediate"),
                                                             region = region,
                                                             species_all = species_all,
                                                             term_include = "cyclic",
                                                             term_exclude = "Larkin")

  biological_status_HBSRM <- rbind(biological_status_HBSRM,
                                   biological_status_HBSRM_c[,colnames(biological_status_HBSRM)])
  
}

nrow(biological_status_HBSRM) # 159 160

# add column biostatus for both thresholds (Smsy and 80% Smsy)
colProb <- colnames(biological_status_HBSRM)[grepl("Smsy_",colnames(biological_status_HBSRM))]
biological_status_HBSRM$status_Smsy <- sapply(X = 1:nrow(biological_status_HBSRM), 
                                             FUN = function(r){
                                               # r <- 1
                                               slice <- biological_status_HBSRM[r,colProb]
                                               # out <- c("red","amber","green")[slice == max(slice)][1] 
                                               out <- c("poor","fair","good")[slice == max(slice)][1] # Katy's request
                                               return(out)
                                             })

colProb <- colnames(biological_status_HBSRM)[grepl("Smsy80_",colnames(biological_status_HBSRM))]
biological_status_HBSRM$status_Smsy80 <- sapply(X = 1:nrow(biological_status_HBSRM), 
                                               FUN = function(r){
                                                 # r <- 1
                                                 slice <- biological_status_HBSRM[r,colProb]
                                                 # out <- c("red","amber","green")[slice == max(slice)][1]
                                                 out <- c("poor","fair","good")[slice == max(slice)][1] # Katy's request
                                                 return(out)
                                               })

#'* Import the biological status obtained with the percentiles method: *
pattern <- "biological_status_percentiles"
biological_status_percentile <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = paste0(wd_output,"/intermediate"),
                                                      region = region,
                                                      species_all = species_all,
                                                      term_exclude = "cyclic")

nrow(biological_status_percentile) # 447 449 451 452 448


#'* Import the conservationunits_decoder.csv *
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- F

conservationunits_decoder <- datasets_database_fun(nameDataSet = "conservationunits_decoder.csv", # datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

nrow(unique(conservationunits_decoder[,c("region","species_name","cu_name_pse")])) # 463 465 466
length(unique(conservationunits_decoder$pooledcuid)) # 463
length(unique(conservationunits_decoder$cuid)) # 469

# TEMPORAL FIX (until fixed in the database)
colnames(conservationunits_decoder)[colnames(conservationunits_decoder) == "species_abbr"] <- "species_qualified"
unique(conservationunits_decoder$species_qualified)


#'* Import dataset390_data_quality (dataset390_output) for survey_quality *
dataset390_output <- datasets_database_fun(nameDataSet = "dataset390_data_quality.csv", # datasetsNames_database$name_CSV[19],
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)
dataset390_output <- dataset390_output[,c("region","species_name","cuid","cu_name_pse","catch_method")]

# TEMPORARY (11/09/2024)
# dataset390_output <- import_mostRecent_file_fun(wd = wd_pop_indic_data_input_dropbox,
#                                                 pattern = "dataset390_data_quality")
# dataset390_output <- dataset390_output[,c("region","species_name","cuid","cu_name_pse","catch_quality")]
# colnames(dataset390_output)[colnames(dataset390_output) == "catch_quality"] <- "catch_method"
head(dataset390_output)
nrow(dataset390_output) # 463 465 466

# survey_quality as in data-quality.R: 
# "Low" ~ 1
# "Medium-Low" ~ 2
# "Medium" ~ 3
# "Medium-High" ~ 4
# "High" ~ 5

# QUESTION: there is something wrong 
# cond <- dataset390_output$cuid %in% c(729,752)
# dataset390_output[cond,]
# 
# pattern <- "dataset390_output"
# dataset390_output_old <- import_mostRecent_file_fun(wd = paste0(wd_pop_data_quality_dropbox,"/output"),
#                                          pattern = pattern)
# dataset390_output_old <- read.csv(paste0( paste0(wd_pop_data_quality_dropbox,"/output"),"/dataset390_output_2023-05-20.csv"))
# dataset390_output_old <- dataset390_output_old[,c("regionname","species","cuid","cu_name_pse","survey_quality")]
# dataset390_output_old$survey_quality
# 
# # QUESTION: there is something wrong 
# cond <- dataset390_output_old$cuid %in% c(729,752)
# dataset390_output_old[cond,]

#'* Import the cuspawnerabundance.csv *
#' To Obtain the last year of observed spawner data in a given region.
#' For Rule 8: is there at least one data point with the last year of the data
#' availability in a region and this yeqr - generation length + 1
#' RULE 8 was updated (see below)
#' Dataset only needed for some checks
cuspawnerabundance <- datasets_database_fun(nameDataSet = "cuspawnerabundance.csv", # datasetsNames_database$name_CSV[2],
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)

d <- data.frame(region = unique(cuspawnerabundance$region))

d$year_last <- sapply(X = d$region,function(rg){
  cond_rg <- cuspawnerabundance$region == rg
  cond_noNA <- !is.na(cuspawnerabundance$estimated_count)
  out <- max(cuspawnerabundance$year[cond_rg & cond_noNA])
  return(out)
})

d


#'* Import code_PSF_Status from database *
#'related slack threads:
#' https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1723143369148479
#' https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1715065514795349?thread_ts=1701199596.229739&cid=CJG0SHWCW

# code_PSF_Status <- data.frame(psf_status_code = 1:9,
#                               psf_status = c("good","fair","poor","extinct",
#                                              "not-assessed","not-assessed",
#                                              "data-deficient","data-deficient",
#                                              "data-deficient"),
#                               comment = c(rep("",4),
#                                           "cyclic dominance",
#                                           "low productivity or high exploitation",
#                                           "insufficient time series length",
#                                           "no estimates of spawner abundance in the most recent generation",
#                                           "no spawner estimates available"))

code_PSF_Status <- datasets_database_fun(nameDataSet = "ssp.biologicalstatuscodes.csv", # datasetsNames_database$name_CSV[21],
                                         fromDatabase = fromDatabase,
                                         update_file_csv = update_file_csv,
                                         wd = wd_pop_indic_data_input_dropbox)


#'* Import list of CUs with high exploitation and/or low productivity *
# Return list of CUs that have high exploitation rate or low production rates,
# as well as a final call on keeping or removing the CUs depending of their
# biostatus: the one with already a red/poor status are kept (i.e. Clare's 8th rule).
highExploit_lowProd <- cu_highExploit_lowProd_fun(biological_status_percentile = biological_status_percentile,
                                                  wd_pop_indic_data_input_dropbox = wd_pop_indic_data_input_dropbox, 
                                                  conservationunits_decoder = conservationunits_decoder, 
                                                  export_csv = T)

#
# Create complete dataset with biostatus and psf_staus_code -----
# https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1701464111241899?thread_ts=1701199596.229739&cid=CJG0SHWCW

#' Add the extra rule 2) in update chart: "is there a spawner-recruit relationship
#' available with the CU-level catch estimates medium-low quality or higher"
#' #' cf. Population "2024 Population Analysis running notes" at March 19 for 
#' corresponding documentation. 
#' https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing
#' The data quality estimates are in:
#' population-indicators/data-quality/output as dataset390_output-2023-05-20.csv

#' Field to include:
#'- sr_red_prob
#'- sr_yellow_prob
#'- sr_green_prob
#'- sr_status values: good, fair, poor, NA
#'- percentile_red_prob
#'- percentile_yellow_prob
#'- percentile_green_prob
#'- percentile_status values: good, fair, poor, NA
#'- psf_status values: good, fair, poor, extinct, data-deficient, not-assessed
#'- psf_status_code values: 1 to 9
#'  - 1 = good
#'  - 2 = fair
#'  - 3 = poor
#'  - 4 = extinct
#'  - 5 = not-assessed (cyclic dominance)
#'  - 6 = not-assessed (low productivity or high exploitation)
#'  - 7 = data-deficient (insufficient time series length)
#'  - 8 = data-deficient (no estimates of spawner abundance in the most recent generation)
#'  - 9 = data-deficient (no spawner estimates available)

# 
# write.csv(code_PSF_Status,paste0(wd_data_dropbox,"/code_PSF_Status.csv"),
#           row.names = F)

#'* Create column psf_status_code *
biological_status_HBSRM$psf_status_code <- NA
biological_status_percentile$psf_status_code <- NA

#'* 4 = extinct *
#' TODO: this list should be provided by pulling from another dataset (380?)
#' cf. PSE Data Update 2024-09-05 meeting
#' For now import it from the code and generate a dated csv file
cu_extinct <- cu_extinct_fun(write_file = T, 
                             wd = wd_pop_indic_data_input_dropbox, 
                             conservationunits_decoder = conservationunits_decoder)
# Remove the CUs that should be removed
cu_extinct <- cu_extinct[cu_extinct$keep,]

row_toUpdate <- biological_status_HBSRM$cuid %in% cu_extinct$cuid
val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,4, sep = ", ")
biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- biological_status_percentile$cuid %in% cu_extinct$cuid
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,4, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

#'* 5 = not-assessed (cyclic dominance) *
#' UPDATE: it was decided during Population meeting of October 16 2024 to provide 
#' the Ricker benchmarks to the (Fraser) cyclic CUs
#' https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing

if(!cyclic_biostatus){
  row_toUpdate <- grepl("(cyclic)",biological_status_HBSRM$cu_name_pse)
  val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
  val_new <- paste(val_toUpdate,5, sep = ", ")
  biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new
  
  row_toUpdate <- grepl("(cyclic)",biological_status_percentile$cu_name_pse)
  val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
  val_new <- paste(val_toUpdate,5, sep = ", ")
  biological_status_percentile$psf_status_code[row_toUpdate] <- val_new
}

#'* 6 = not-assessed (low productivity or high exploitation) --> Percentile only *
for(r in 1:nrow(highExploit_lowProd)){
  # r <- 1
  cuid <- highExploit_lowProd$cuid[r]
  
  cond <- biological_status_percentile$cuid == cuid
  
  val_toUpdate <- biological_status_percentile$psf_status_code[cond]
  val_new <- paste(val_toUpdate,6, sep = ", ")
  
  #' Rule: show the biostatus if it is 'red'
  if(!is.na(highExploit_lowProd$biostatus_percentile[r]) &
     highExploit_lowProd$biostatus_percentile[r] %in% c("red","poor")){
    
    val_new <- paste(val_new,3, sep = ", ")
    val_new  <- strsplit(split = ", ", x = val_new)[[1]]
    val_new <- val_new[val_new != "NA"]
    val_new <- as.numeric(val_new)
    val_new <- sort(val_new)
    val_new <- paste(val_new, collapse = ", ")
    
    print(highExploit_lowProd[r,])
  }
  # biological_status_percentile[cond,]
  biological_status_percentile$psf_status_code[cond] <- val_new
}

#'* 7 = data-deficient (insufficient time series length) --> Percentile only *
row_toUpdate <- biological_status_percentile$dataPointNb < 20 # & biological_status_percentile$dataPointNb > 0 # so that rule 9 is shown
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,7, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

#'* 8 = data-deficient (no estimates of spawner abundance in the most recent generation) *
#' OLDER RULE:
#' Rule 8: To be considered for biostatus assessment CUs must have at least one 
#' data point within the interval defined by the last year of available data for
#' a given region and this last year - generation length + 1 
#' Update (2024-09-13): for Pink, there must be at least one data point within
#' the last year of data for the region and the 3 previous years.
#' 
#' NEW RULE (2025-03-05)
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1741145624936989?thread_ts=1741121314.247459&cid=CJ5RVHVCG
#' The cut-off year is defined for all species except for Pink as:
#' - current year - 2 - generation length + 1
#' - for Pink CUs: go back an extra year for odd pink CUs if current year is odd
#'   and vice-versa

rule8 <- data.frame(cuid = unique(c(biological_status_HBSRM$cuid,biological_status_percentile$cuid)))

rule8$species_name <- sapply(rule8$cuid, function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(conservationunits_decoder$species_name[cond])
})

rule8$species_qualified <- sapply(rule8$cuid, function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(conservationunits_decoder$species_qualified[cond])
})

rule8$gen_length <- sapply(rule8$cuid, function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(conservationunits_decoder$gen_length[cond])
})

# The last year of estimated spawner abundance data:
rule8$year_data_last <- sapply(rule8$cuid, function(cuid){
  # cuid <- 291
  cond <- biological_status_percentile$cuid == cuid
  return(biological_status_percentile$yr_withData_end[cond])
})

# The cut off year is not done per region any more (NEW RULE)
# rule8$year_data_last_rg <- sapply(rule8$cuid, function(cuid){
#   cond <- conservationunits_decoder$cuid == cuid
#   rg <- conservationunits_decoder$region[cond]
# 
#   cond <- cuspawnerabundance$region == rg
#   last_year <- max(cuspawnerabundance$year[cond], na.rm = T)
#   return(last_year)
# })

# For non-pink salmon:
# cond_pink <- grepl("Pink",rule8$species_name)
# rule8$concerned[!cond_pink] <- (rule8$year_data_last_rg[!cond_pink] - rule8$year_data_last[!cond_pink] + 1) > rule8$gen_length[!cond_pink]
# 
# # For pink salmon:
# rule8$concerned[cond_pink] <- (rule8$year_data_last_rg[cond_pink] - rule8$year_data_last[cond_pink] + 1) > 4
# 
# sum(rule8$concerned) # 51 50 67 77

# Find current year
year_now <- as.numeric(strftime(Sys.time(), format = "%Y"))

# year cut-off
rule8$year_cutoff <- NA
for(r in 1:nrow(rule8)){
  # r <- 1
  yr_cut <- year_now - 2 - rule8$gen_length[r] + 1
  
  if(rule8$species_qualified[r] == "PKO" & year_now %% 2 == 1){
    yr_cut <- yr_cut - 1
  }else if(rule8$species_qualified[r] == "PKE" & year_now %% 2 == 0){
    yr_cut <- yr_cut - 1
  }
  
  rule8$year_cutoff[r] <- yr_cut
}

rule8$concerned <- rule8$year_cutoff > rule8$year_data_last

# to check
rule8$comment <- sapply(rule8$cuid, function(cuid){
  # cuid <- 291
  cond <- biological_status_percentile$cuid == cuid
  return(biological_status_percentile$comment[cond])
})

# Include concern the CUs with no spawner estimates available
cond <- is.na(rule8$year_data_last)
unique(rule8$comment[cond])   # should only be "No estimated_count data in cuspawnerabundance.csv"
rule8$concerned[cond] <- T

# row_toUpdate <- biological_status_HBSRM$genLength_dataPointNb == 0 # old and wrong
# sum(row_toUpdate) # 2
row_toUpdate <- biological_status_HBSRM$cuid %in% rule8$cuid[rule8$concerned]
sum(row_toUpdate) # 38 24 23 33
val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,8, sep = ", ")
biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new

# row_toUpdate <- biological_status_percentile$genLength_dataPointNb == 0 # old and wrong
# sum(row_toUpdate) # 217
row_toUpdate <- biological_status_percentile$cuid %in% rule8$cuid[rule8$concerned]
sum(row_toUpdate) # 76 51 50 67 77
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,8, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

#'* 9 = data-deficient (no spawner estimates available) *
row_toUpdate <- biological_status_HBSRM$comment == "No estimated_count data in cuspawnerabundance.csv"
sum(row_toUpdate) # 2
val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,9, sep = ", ")
biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- biological_status_percentile$comment  == "No estimated_count data in cuspawnerabundance.csv"
sum(row_toUpdate) # 210
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,9, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

#'* 11 = data-deficient (any other reason) *
#'
#' Case with 
#' Vancouver Island & Mainland Inlets	Steelhead	980	South Coast Winter:
#' Eric: " the steelhead technical working group said that the Chekamus is not 
#' representative of the dynamics of other systems in that CU, and thus should
#' not be used to calculate biological status"
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1726266767976959?thread_ts=1726158954.361189&cid=CJ5RVHVCG

row_toUpdate <- biological_status_HBSRM$cuid == "980"
val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,11, sep = ", ")
biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- biological_status_percentile$cuid == "980"
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,11, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

# ENd 

# Remove "NA, " in psf_status_code
biological_status_HBSRM$psf_status_code <- gsub("NA, ","",biological_status_HBSRM$psf_status_code)
biological_status_percentile$psf_status_code <- gsub("NA, ","",biological_status_percentile$psf_status_code)

unique(biological_status_HBSRM$psf_status_code)
unique(biological_status_percentile$psf_status_code)

# Combine the two datasets:
colCommon <- c("region","species_name","species_qualified","cuid","cu_name_pse",
               "current_spawner_abundance","psf_status_code")
              # "year_last","year_first","genLength")

#' NOTE:
#' According to the PSAC meeting of June 2024, we should use the 80% Smsy (vs 100% ) and 
#' 50% percentile (vs. 75%) for the upper benchmarks.

#' NOTE: from 21/05/2025
#' We now use: 
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1745963219139359?thread_ts=1745442260.883189&cid=CJ5RVHVCG
#' Changes made: 
#' (1) implement a lower absolute abundance benchmark of 1,500
#' (2) increase the upper benchmark from 80%Smsy/50th percentile to Smsy/75th percentile.


colHBSR <- c("status_Smsy_red","status_Smsy_amber","status_Smsy_green",
             "status_Smsy")
# colHBSR <- c("status_Smsy80_red","status_Smsy80_amber","status_Smsy80_green",
#              "status_Smsy80")

colPercent <- c("status_percent_075_red","status_percent_075_amber","status_percent_075_green",
                "status_percent075")
# colPercent <- c("status_percent_05_red","status_percent_05_amber","status_percent_05_green",
#                 "status_percent050")
# colPercent <- c("status_percent05")

biological_status_merged <- merge(x = biological_status_HBSRM[,c(colCommon,colHBSR)],
                                  y = biological_status_percentile[,c(colCommon,colPercent)],
                                  by =  c("region","species_name","species_qualified",
                                          "cuid","cu_name_pse","current_spawner_abundance"), 
                                  all = T)

cond <- grepl("\\.x",colnames(biological_status_merged))
colnames(biological_status_merged)[cond] <- gsub(".x","_sr",colnames(biological_status_merged)[cond])

cond <- grepl("\\.y",colnames(biological_status_merged))
colnames(biological_status_merged)[cond] <- gsub(".y","_percentile",colnames(biological_status_merged)[cond])

head(biological_status_merged)

# Check if number of CUs is correct:
CUs_comm <- biological_status_HBSRM$cuid[biological_status_HBSRM$cuid %in% 
                                          biological_status_percentile$cuid]
length(CUs_comm) # 160 151 144 (138 + 6 cyclic CUs) ; 138 137

CUs_HBSRM_only <- biological_status_HBSRM$cuid[!biological_status_HBSRM$cuid %in% 
                                               biological_status_percentile$cuid]
length(CUs_HBSRM_only) # 0 should be 0

CUs_Percent_only <- biological_status_percentile$cuid[!biological_status_percentile$cuid %in% 
                                                       biological_status_HBSRM$cuid]
length(CUs_Percent_only) # 287 289 298 305 (311 - 6 cyclic) ; 311 313 315 311

# Expected number of rows in biological_status_merged:
length(CUs_comm) + length(CUs_HBSRM_only) + length(CUs_Percent_only) # 447 449 451 452 448
nrow(biological_status_merged) # 447 455 449 451 452 --> ALL GOOD

# cond <- biological_status_merged$cuid == 185
# biological_status_merged[cond,]

# Renames columns
colnames(biological_status_merged) <- gsub("red","red_prob",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("amber","yellow_prob",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("green","green_prob",colnames(biological_status_merged))

colnames(biological_status_merged) <- gsub("status_Smsy_","sr_",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_Smsy80_","sr_",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_percent_075_","percentile_",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_percent_05_","percentile_",colnames(biological_status_merged))

colnames(biological_status_merged) <- gsub("status_Smsy","sr_status",colnames(biological_status_merged))
# colnames(biological_status_merged) <- gsub("status_Smsy80","sr_status",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_percent075","percentile_status",colnames(biological_status_merged))
#colnames(biological_status_merged) <- gsub("status_percent050","percentile_status",colnames(biological_status_merged))

#'* Create psf_status_code_all fields & psf_status_type *
#' psf_status_code_all: attribute 1 (good), 2 (fair) or 3 (poor) for now
#' psf_status_type:  Katy's request. Either "sr", "percentile" or NA.

biological_status_merged$psf_status_code_all <- NA # values: 1 to 9

biological_status_merged$psf_status_type <- NA

# col_prob <- colnames(biological_status_merged)[grepl("_prob",colnames(biological_status_merged))]
# col_sr_prob <- col_prob[grepl("sr_",col_prob)]
# col_percent_prob <- col_prob[grepl("percentile_",col_prob)]

# Check: Cases where Sgen > Smsy
cond_HPD <- benchmarks_HBSRM$method == "HPD"
cond_Sgen <- benchmarks_HBSRM$benchmark == "Sgen"
cond_Smsy <- benchmarks_HBSRM$benchmark == "Smsy"
check <- sapply(unique(benchmarks_HBSRM$cuid),function(cuid){
  cond_cuid <- benchmarks_HBSRM$cuid == cuid
  Sgen <- benchmarks_HBSRM$m[cond_cuid & cond_HPD & cond_Sgen]
  Smsy <- benchmarks_HBSRM$m[cond_cuid & cond_HPD & cond_Smsy]
  if(Sgen > Smsy){
    return( benchmarks_HBSRM[cond_cuid & cond_HPD & (cond_Sgen | cond_Smsy),])
  }
})
check <- do.call(rbind.data.frame, check)
check
length(unique(check$cuid)) # 4 5 4 5

# This is considered in the forloop below in Condition 3: Sgen < Smsy.
# --> use percentile benchmarks if available.

#
for(r in 1:nrow(biological_status_merged)){
  # r <- 1
  # r <- which(biological_status_merged$cuid == 810)
  bs_here <- biological_status_merged[r,]
  
  # *** HBSRM method ***
  
  #' Condition 1: Are the probabilities available and the status_code is NA 
  #' (i.e. not 4, ...9)
  cond_HBRSM <- !is.na(bs_here$sr_status) & is.na(bs_here$psf_status_code_sr) # is.na(bs_here$psf_status_code.x) might not be necessary but does not hurt
  
  #' Condition 2: Is there a SR relationship with CU-level catch estimates >= medium-low
  # survey_quality 
  # In data-quality.R: 
  # "Low" ~ 1
  # "Medium-Low" ~ 2
  # "Medium" ~ 3
  # "Medium-High" ~ 4
  # "High" ~ 5
  cond <- dataset390_output$cuid == bs_here$cuid
  #' Steph: "data_quality scores that are NA or zero should not prevent HBSR
  #' status form being shown. Only low (1) or medium-low (2) catch_quality should
  #' be part of the decision rule"
  #' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1713982063020029?thread_ts=1713912766.189429&cid=CJ5RVHVCG
  # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1713912766189429
  cond_HBRSM_2 <- is.na(dataset390_output$catch_method[cond]) | dataset390_output$catch_method[cond] != 1
  
  #' Condition 3: Sgen < Smsy
  cond_cuid <- benchmarks_HBSRM$cuid == bs_here$cuid
  if(any(cond_cuid)){
    Sgen <- benchmarks_HBSRM$m[cond_cuid & cond_HPD & cond_Sgen]
    Smsy <- benchmarks_HBSRM$m[cond_cuid & cond_HPD & cond_Smsy]
    if(Sgen < Smsy){
      cond_HBRSM_3 <- T
    }else{
      cond_HBRSM_3 <- F     # Sgen > Smsy
    }
  }else{
    cond_HBRSM_3 <- F
  }
  
  if(cond_HBRSM & cond_HBRSM_2 & cond_HBRSM_3){
    
    # psf_status_here <- c("poor","fair","good")[bs_here[,col_sr_prob] == max(bs_here[,col_sr_prob])]
    # psf_status_code_all_here <- c(3:1)[psf_status_here == c("poor","fair","good")]
    # psf_status_code_all_here <- c(3:1)[bs_here[,col_sr_prob] == max(bs_here[,col_sr_prob])]
    psf_status_code_all_here <- c(3:1)[bs_here$sr_status == c("poor","fair","good")]
    psf_status_type_here <- "sr"
    
  # *** Percentile method ***
    
  }else if(!is.na(bs_here$percentile_status) & 
           (is.na(bs_here$psf_status_code_percentile) | bs_here$psf_status_code_percentile == "3, 6")){ # grepl("3, 6",bs_here$psf_status_code_percentile)
    #' grepl("3, 6",bs_here$psf_status_code.y) is for the CUs with high exploitation 
    #' / low productivity with poor status.

    # NOT USED: using the probabilities to determine the status
    #psf_status_code_all_here <- c(3:1)[bs_here[,col_percent_prob] == max(bs_here[,col_percent_prob])]
    
    # Get the percentile benchmark values:
    cond_cuid <- benchmarks_percentile$cuid == bs_here$cuid
    cond <- benchmarks_percentile$benchmark == "benchmark_0.25"
    bench_low <- benchmarks_percentile[cond_cuid & cond,"m"]
    cond <- benchmarks_percentile$benchmark == "benchmark_0.5"
    bench_up <- benchmarks_percentile[cond_cuid & cond,"m"]

    # compare to current spawner abundance
    if(is.na(bs_here$psf_status_code_percentile)){
      psf_status_code_all_here <- c(3:1)[bs_here$percentile_status == c("poor","fair","good")]
    }else{
      psf_status_code_all_here <- bs_here$psf_status_code_percentile # the only case this happen is when grepl("3, 6",bs_here$psf_status_code_percentile)
    }
    
    #
    psf_status_type_here <- "percentile"
  
  }else{ # biostatus is not available
    
    code_HBSRM <- bs_here$psf_status_code_sr
    code_Percentile <- bs_here$psf_status_code_percentile
    code_both <- c(code_HBSRM, code_Percentile)
    code_both <- code_both[!is.na(code_both)]
    code_both <- sapply(X = code_both, FUN =  strsplit, split = ", ")
    code_both <- as.numeric(code_both[[1]])
    code_both <- sort(code_both)
    code_both <- unique(code_both)
    
    code_both <- paste(code_both,collapse = ", ")
    psf_status_code_all_here <- code_both
    psf_status_type_here <- NA
  }

  biological_status_merged$psf_status_code_all[r] <- psf_status_code_all_here
  biological_status_merged$psf_status_type[r] <- psf_status_type_here
}

# Checks that there is no missing psf_status_code (i.e. no NAs)
unique(biological_status_merged$psf_status_code_all)
table(biological_status_merged$psf_status_code_all)


#'* Create psf_status_code and psf_status *
#'  - 1 = good
#'  - 2 = fair
#'  - 3 = poor
#'  - 4 = extinct
#'  - 5 = not-assessed (cyclic dominance)
#'  - 6 = not-assessed (low productivity or high exploitation)
#'  - 7 = data-deficient (insufficient time series length)
#'  - 8 = data-deficient (no estimates of spawner abundance in the most recent generation)
#'  - 9 = data-deficient (no spawner estimates available)
#'  - 10 = not-assessed: this is the code to use if we need to specify a CU specific explanation for a "not assessed" status
#'  - 11 = data-deficient: this is the code to use if we need to specify a CU specific explanation for a "data deficient" status"
#' Decisions for psf_status_code when multiple numbers in psf_status_code_all:
#' - If 7, 8, 9 then display 8 (9 implies 7 & 8 and it is not in decision rules) --> UPDATE: --> 9 https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1741231655948029?thread_ts=1741121314.247459&cid=CJ5RVHVCG
#' - if 7,9 --> 9
#' - If 7, 8, then display 8
#' Reason: this is the first step in the decision tree. If there is no recent
#' spawner abundance then we cannot assess status regardless of benchmarks.
#' - Similarly if 6,8 then display 8
#' - If 4, 8 then display 4! (NEED TO MAKE SURE THE EXTINCT CATEGORY IS UPDATED)
#' - If 4, 9 then display 4! (NEED TO MAKE SURE THE EXTINCT CATEGORY IS UPDATED)
#' (cf. Population meeting April 16 2024)
#' https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing
#' - "3, 6, 7" --> 7
#' - "6, 7, 8" --> 8
#' - "6, 7"    --> 7
#' - "3, 6"    --> 3 (the exception)
#' - "6, 9"    --> 9

biological_status_merged$psf_status_code <- biological_status_merged$psf_status_code_all

cond_4 <- grepl("4,",biological_status_merged$psf_status_code_all)
biological_status_merged$psf_status_code_all[cond_4] |> unique()
biological_status_merged$psf_status_code[cond_4] <- 4

cond_9 <- grepl("9",biological_status_merged$psf_status_code_all) & !cond_4
unique(biological_status_merged$psf_status_code_all[cond_9]) |> unique()
biological_status_merged$psf_status_code[cond_9] <- 9

cond_8 <- grepl("8",biological_status_merged$psf_status_code_all) & !cond_4 & !cond_9
unique(biological_status_merged$psf_status_code_all[cond_8]) |> unique()
biological_status_merged$psf_status_code[cond_8] <- 8

cond_7 <- grepl("7",biological_status_merged$psf_status_code_all) & !cond_4 & !cond_8 & !cond_9
unique(biological_status_merged$psf_status_code_all[cond_7]) |> unique()
biological_status_merged$psf_status_code[cond_7] <- 7

cond_36 <- grepl("3, 6",biological_status_merged$psf_status_code_all) & !cond_7 & !cond_8
unique(biological_status_merged$psf_status_code_all[cond_36]) |> unique()
biological_status_merged$psf_status_code[cond_36] <- 3

# cond_69 <- grepl("6, 9",biological_status_merged$psf_status_code_all)
# unique(biological_status_merged$psf_status_code_all[cond_69]) |> unique()
# biological_status_merged$psf_status_code[cond_69] <- 9

unique(biological_status_merged$psf_status_code) # all good

#'* Create psf_status * 
biological_status_merged$psf_status <- apply(X = biological_status_merged, 
                                             MARGIN = 1, FUN = function(r){
                                               code <- r["psf_status_code"]
                                               cond <- code_PSF_Status$psf_status_code == code
                                               return(code_PSF_Status$psf_status[cond])
                                             })

unique(biological_status_merged$psf_status)


#'* Absolute lower benchmark of 1500 *
#' NOTE: from 21/05/2025
#' We now use: 
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1745963219139359?thread_ts=1745442260.883189&cid=CJ5RVHVCG
#' Changes made: 
#' (1) implement a lower absolute abundance benchmark of 1,500
#' (2) increase the upper benchmark from 80%Smsy/50th percentile to Smsy/75th percentile.

#' For the CUs with good or fair biostatus:
cond_12 <- biological_status_merged$psf_status_code %in% 1:2
sum(is.na(biological_status_merged$current_spawner_abundance))
cond_1500 <- !is.na(biological_status_merged$current_spawner_abundance) &
  biological_status_merged$current_spawner_abundance < 1500

biological_status_merged[cond_12 & cond_1500,]

# update the fields:
biological_status_merged$psf_status_code_all[cond_12 & cond_1500] <- paste0(biological_status_merged$psf_status_code_all[cond_12 & cond_1500],", 3")
biological_status_merged$psf_status_code[cond_12 & cond_1500] <- 3
biological_status_merged$psf_status[cond_12 & cond_1500] <- "poor"
biological_status_merged$psf_status_type[cond_12 & cond_1500] <- "Absolute"


#' For the CUs with status_code 7: insufficient time series length:
#' EXCEPTION for CUID = 216 (SER Skeena River-HIgh Interior):
#' there is only only spawner survey location + in Swang Lake (outside CU boundary) 
#' → leave as data deficient for now
#' From population meeting 02/06/2025
cond_7 <- biological_status_merged$psf_status_code %in% 7
cond_exception <- biological_status_merged$cuid == 216
biological_status_merged[cond_7 & cond_1500 & !cond_exception,]
biological_status_merged$psf_status_code_all[cond_7 & cond_1500 & !cond_exception] <- paste0("3, ",biological_status_merged$psf_status_code_all[cond_7 & cond_1500 & !cond_exception])
biological_status_merged$psf_status_code[cond_7 & cond_1500 & !cond_exception] <- 3
biological_status_merged$psf_status[cond_7 & cond_1500 & !cond_exception] <- "poor"
biological_status_merged$psf_status_type[cond_7 & cond_1500 & !cond_exception] <- "Absolute"

# Check
cond <- biological_status_merged$cuid == 216
biological_status_merged[cond,]

unique(biological_status_merged$psf_status_code)

table(biological_status_merged$psf_status)
# data-deficient        extinct           fair           good   not-assessed           poor 
#            223              5             51             78             27             67 
#            296              4             37             51             17             46 
#            284              4             38             57             17             49 
#            285              4             38             56             17             49 
#            285              4             40             56             11             53 
#            268              4             37             59             18             63 
#            268              4             37             58             18             64 2024-11-19
#            265              4             38             60             18             64 
#            287              4             36             51             12             59 2025-03-05 
#            284              4             37             32             12             80 2025-05-21: the data is the same but the new rule with 100% Smsy, 75% percentile and <1500 are applied 
#            275              4             43             24             11             90 2025-06-03

#'* Show psf_status_type for CUs with psf_status_code_all == 8 *
#' Update (2024-11-20 from PSE data meeting): we still show the method used in 
#' psf_status_type for the CU with only data-deficient (no estimates of spawner abundance in the most recent generation)
#' https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1732225660151029?thread_ts=1732133777.124549&cid=C03LB7KM6JK
cond_8 <- biological_status_merged$psf_status_code_all == "8"
cond_sr <- !is.na(biological_status_merged$sr_status)
cond_percentile <- !is.na(biological_status_merged$percentile_status)
# biological_status_merged[cond_8,]
# Check that all these CUs have a biostatus_type available
sum(cond_8) - sum(cond_sr & cond_8) - sum(cond_8 & !cond_sr & cond_percentile) # should be 0
biological_status_merged$psf_status_type[cond_8 & cond_sr] <- "sr"
biological_status_merged$psf_status_type[cond_8 & !cond_sr & cond_percentile] <- "percentile"

#'* Add field  hist_COLOUR *
#' Note: this will be be removed in future. And no need to do the same for 
#' sr_COLOUR because the process is different.
biological_status_merged$hist_red <- NA
biological_status_merged$hist_yellow <- NA
biological_status_merged$hist_green <- NA

for(r in 1:nrow(biological_status_merged)){
  # r <- 1
  col_here <- NA
  if(is.na(biological_status_merged$percentile_status[r])){
    # col_here <- "#FFFFFF"
    hist_red <- hist_yellow <- hist_green <- NA
  }else{
    cond <- biological_status_merged$percentile_status[r] == c("poor","fair","good")
    col_here <- c("#CC0000","#FFFF00","#009900")[cond]
    
    hist_red <- hist_yellow <- hist_green <- "#FFFFFF"
    if(col_here == "#CC0000"){
      hist_red <- col_here
    }else if(col_here == "#FFFF00"){
      hist_yellow <- col_here
    }else{
      hist_green <- col_here
    }
  }
  
  #
  biological_status_merged$hist_red[r] <- hist_red
  biological_status_merged$hist_yellow[r] <- hist_yellow
  biological_status_merged$hist_green[r] <- hist_green
}

#'* Drop unecessary columns *
colToDrop <- c("psf_status_code_sr","psf_status_code_percentile")
biological_status_merged <- biological_status_merged[,!colnames(biological_status_merged) %in% colToDrop]

# Check
cond <- is.na(biological_status_merged$current_spawner_abundance)
unique(biological_status_merged[,c("psf_status_code","psf_status_code_all")][cond,])

# Check that the CUs with status code 6 (high exploitation rate or low production rates)
# with red status are still available
cond <- grepl("6",biological_status_merged$psf_status_code_all) &
  biological_status_merged$percentile_status == 'poor' &
  !is.na(biological_status_merged$percentile_status)

biological_status_merged[cond,]
biological_status_merged$psf_status[cond] |> unique()

# Re-arrange columns and drop current_spawner_abundance
col_prob <- colnames(biological_status_merged)[grepl("_prob",colnames(biological_status_merged))]
col_status <- colnames(biological_status_merged)[grepl("_status",colnames(biological_status_merged))]

biological_status_merged <- biological_status_merged[,c("region","species_name","species_qualified",
                                                        "cuid","cu_name_pse",
                                                        col_prob,
                                                        c("hist_red","hist_yellow","hist_green"),
                                                        col_status)]
head(biological_status_merged)

# Check if the psf code of cyclic communities: should be 5 or not depending on biostatus_cyclic
#' if biostatus_cyclic == T" there should not be any CU with "5"
biological_status_merged[grepl("cyclic",biological_status_merged$cu_name_pse),]
biological_status_merged[grepl("5",biological_status_merged$psf_status_code_all),]

#
# Counts of the CUs with sr and percentile benchmarks, no status, etc. ---- 
#

# Number CUs total:
nrow(biological_status_merged) # 447 449 451 452 448

# Number CUs with biostatus assessed over both methods
condition <- biological_status_merged$psf_status_code %in% 1:3
sum(condition) # 157 146 162 159 149 (--> 6 cyclic CUs) 143 144 134 196 195

# CUs not assessed because cyclic dynamics, low productivity/high mortality or data deficient
condition_5 <- biological_status_merged$psf_status_code == "5"
condition_6 <- biological_status_merged$psf_status_code == "6"
condition_7 <- biological_status_merged$psf_status_code == "7"
condition_8 <- biological_status_merged$psf_status_code == "8"
condition_5_6_7 <- condition_5 | condition_6 | condition_7
biological_status_merged$psf_status_code_all[condition_5_6_7]
sum(condition_5_6_7) # 13 16 229 233 226 (less 6 cyclic CUs) 232 234 38

# Number CUs with biostatus assessed with HBSRM:
condition_1_2_3 <- biological_status_merged$psf_status_code %in% 1:3
condition_HBSRM <- !is.na(biological_status_merged$psf_status_type) & 
  biological_status_merged$psf_status_type == "sr"
sum(condition_HBSRM)                   # 137 140 152 117 110 104 97 124 125
sum(condition_1_2_3 & condition_HBSRM) # 99 102 114 125 117 110 (--> + 6 cyclic CUs) 104 97 124 125
sum(condition_8 & condition_HBSRM)     # 38 24 with not enough recent data but psf_status_type still shown

# Number CUs with biostatus assessed with percentile method: 
condition_Percent <- !is.na(biological_status_merged$psf_status_type) & 
  biological_status_merged$psf_status_type == "percentile"
sum(condition_Percent)                       # 60 48 57 42 39 40 37 72 71 69
sum(condition_1_2_3 & condition_Percent)     # 33 27 37 42 39 40 37 72 71 69

condition_absolute <- !is.na(biological_status_merged$psf_status_type) & 
  biological_status_merged$psf_status_type == "Absolute"

sum(condition_HBSRM) + 
  sum(condition_Percent) + 
  sum(condition_absolute) - 
  sum(condition_HBSRM & condition_8)- 
  sum(condition_Percent & condition_8) == sum(condition_1_2_3) # should be TRUE

#
# Create benchmarks_HBSR_Percentile_all.csv (part of dataset_102) --------

# Import Biological_status_HBSR_Percentile_all.csv create just before
# biological_status_merged <- import_mostRecent_file_fun(wd = wd_output,
#                                                        pattern = "Biological_status_HBSR_Percentile_all_")

benchmarks_merged <- biological_status_merged[,c("region","species_name","species_qualified",
                                                 "cuid","cu_name_pse")]

benchmarks_merged$curr_spw <- NA       # current spawner abundance
benchmarks_merged$sr_lower <- NA           # lower HBSRM benchmark
benchmarks_merged$sr_lower_025 <- NA     #
benchmarks_merged$sr_lower_975 <- NA     #
benchmarks_merged$sr_upper <- NA           # upper HBSRM benchmark
benchmarks_merged$sr_upper_025 <- NA     #
benchmarks_merged$sr_upper_975 <- NA     #
benchmarks_merged$percentile_lower <- NA      # lower percentile benchmark
benchmarks_merged$percentile_lower_025 <- NA
benchmarks_merged$percentile_lower_975 <- NA
benchmarks_merged$percentile_upper <- NA      # upper percentile benchmark
benchmarks_merged$percentile_upper_025 <- NA
benchmarks_merged$percentile_upper_975 <- NA
benchmarks_merged$curr_spw_start_year <- NA       # 
benchmarks_merged$curr_spw_end_year <- NA       # 
# benchmarks_merged$exp_start_year <- NA       #  ?
# benchmarks_merged$exp_end_year <- NA       # ?

for(r in 1:nrow(benchmarks_merged)){
  # r <- 2
  cuid <- biological_status_merged$cuid[r]
  
  # get the current spawner abundance 
  cond <- biological_status_percentile$cuid == cuid
  benchmarks_merged$curr_spw[r] <- biological_status_percentile$current_spawner_abundance[cond]
  benchmarks_merged$curr_spw_start_year[r] <- biological_status_percentile$yr_withData_start[cond]
  benchmarks_merged$curr_spw_end_year[r] <- biological_status_percentile$yr_withData_end[cond]
  
  # get the benchmarks for HBSRM:
  cond_cuid <- benchmarks_HBSRM$cuid == cuid
  if(any(cond_cuid)){ # if the HBSR benchmarks are available
    
    cond_method <- benchmarks_HBSRM$method == "HPD"    # "medQuan" # or "HPD"
    cond_sgen <- benchmarks_HBSRM$benchmark == "Sgen"
    cond_Smsy <- benchmarks_HBSRM$benchmark == "Smsy"
    
    benchmarks_merged$sr_lower[r] <- benchmarks_HBSRM$m[cond_cuid & cond_method & cond_sgen]
    benchmarks_merged$sr_lower_025[r] <- benchmarks_HBSRM$CI025[cond_cuid & cond_method & cond_sgen]
    benchmarks_merged$sr_lower_975[r] <- benchmarks_HBSRM$CI975[cond_cuid & cond_method & cond_sgen]

    # QUESTION: how to deal with the CI for 80% Smsy:
    # https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1714495136554939?thread_ts=1701199596.229739&cid=CJG0SHWCW
    
    benchmarks_merged$sr_upper[r] <- benchmarks_HBSRM$m[cond_cuid & cond_method & cond_Smsy] # * 0.8           # not 80% anymore as of 21/05/2025 with new rule implemented (see above)
    benchmarks_merged$sr_upper_025[r] <- benchmarks_HBSRM$CI025[cond_cuid & cond_method & cond_Smsy] # * 0.8
    benchmarks_merged$sr_upper_975[r] <- benchmarks_HBSRM$CI975[cond_cuid & cond_method & cond_Smsy] # * 0.8
  }
  
  # get the benchmarks for the percentile method:
  cond_cuid <- benchmarks_percentile$cuid == cuid
  cond_025 <- benchmarks_percentile$benchmark == "benchmark_0.25"
  cond_075 <- benchmarks_percentile$benchmark == "benchmark_0.75"
  #cond_05 <- benchmarks_percentile$benchmark == "benchmark_0.5"     # # not 50% anymore as of 21/05/2025 with new rule implemented (see above)

  benchmarks_merged$percentile_lower[r] <- benchmarks_percentile$m[cond_cuid & cond_025]
  benchmarks_merged$percentile_lower_025[r] <- benchmarks_percentile$CI025[cond_cuid & cond_025]
  benchmarks_merged$percentile_lower_975[r] <- benchmarks_percentile$CI975[cond_cuid & cond_025]
    
  benchmarks_merged$percentile_upper[r] <- benchmarks_percentile$m[cond_cuid & cond_075]
  benchmarks_merged$percentile_upper_025[r] <- benchmarks_percentile$CI025[cond_cuid & cond_075]
  benchmarks_merged$percentile_upper_975[r] <- benchmarks_percentile$CI975[cond_cuid & cond_075]
  
  # COMMENT:
  # This is ineeded the 50% percentile and not the 75, despite the name being "75%_spw"
  # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1707332952867199
  # !!!! TO CHANGE !!! --> DONE
  
  # benchmarks_merged$percentile_upper[r] <- benchmarks_percentile$m[cond_cuid & cond_05]
  # benchmarks_merged$percentile_upper_025[r] <- benchmarks_percentile$CI025[cond_cuid & cond_05]
  # benchmarks_merged$percentile_upper_975[r] <- benchmarks_percentile$CI975[cond_cuid & cond_05]
}

#
# Add the missing CUs to the output files ------
#' There are CUs excluded in the biostatus analysis because there is 
#' no spawner survey for them (not in dataset5_output (in the R code: recruitsperspawner)
#' nor in dataset1cu_output (in the R code: cuspawnerabundance)).
#' https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1714665184357159?thread_ts=1701199596.229739&cid=CJG0SHWCW

# biological_status_merged <- import_mostRecent_file_fun(wd = wd_output,
#                                                        pattern = "Biological_status_HBSR_Percentile_all_")
# 
# benchmarks_merged <- import_mostRecent_file_fun(wd_output,
#                                                 pattern = "Benchmarks_HBSR_Percentile_all_")

nrow(biological_status_merged)  # 447 449
nrow(conservationunits_decoder) # 469

# We use the "pooledcuid" field and not "cuid" because the latter contains cuid
# values that are not used anymore because the associated CUs have been pooled
# (i.e. associated to another cuid, whose value is in pooledcuid).
pooledcuid <- conservationunits_decoder$pooledcuid |> unique()
cuid <- conservationunits_decoder$cuid |> unique()

length(pooledcuid) # 463
length(cuid)       # 469

pooledcuid[! pooledcuid %in% cuid] # none
cuid[! cuid %in% pooledcuid]       # 172 173 243 184 179 204 --> those have been pooled with other CUs

cond <- conservationunits_decoder$cuid %in% cuid[! cuid %in% pooledcuid]
conservationunits_decoder[cond,c("region","species_qualified","pooledcuid","cuid","cu_name_pse","cu_name_dfo","cu_type")]

# find their cuid:
cond <- ! conservationunits_decoder$pooledcuid %in% biological_status_merged$cuid #
cuidMissing <- conservationunits_decoder$cuid[cond] |> unique()
cuidMissing
length(cuidMissing) # 16 14

#'* Add to biological_status_merged * 
head(biological_status_merged)

biological_status_add <- biological_status_merged[1:length(cuidMissing),]
biological_status_add$region <- conservationunits_decoder$region[cond]
species_name <- conservationunits_decoder$species_name[cond]
# TEMPORARY
# simplify species_name as in PSE data meeting December 11 2024
# To remove eventually when conservationunits_decoder$species_name 
# changed as well.
species_name <- sapply(species_name, FUN = function(sp){
  out <- sp
  if(grepl("[s|S]ockeye",sp)){
    out <- "Sockeye"
  }else if(grepl("Pink",sp)){
    out <- "Pink"
  }
  return(out)
})
biological_status_add$species_name <- species_name
biological_status_add$species_qualified <- conservationunits_decoder$species_qualified[cond]
biological_status_add$cuid <- conservationunits_decoder$cuid[cond]
biological_status_add$cu_name_pse <- conservationunits_decoder$cu_name_pse[cond]

cond_col <- ! colnames(biological_status_add) %in% c("region","species_name","species_qualified",
                                                     "cuid","cu_name_pse")
biological_status_add[,cond_col] <- NA

biological_status_add$psf_status <- "data-deficient"
biological_status_add$psf_status_code <- 9 # 8    # OR 9 
biological_status_add$psf_status_code_all <- "8, 9"  

biological_status_merged <- rbind(biological_status_merged,
                                  biological_status_add)


nrow(biological_status_merged) # 463
cond_yk <- grepl("Yukon",biological_status_merged$region)
sum(!cond_yk) # 443

table(biological_status_merged$psf_status[!cond_yk])


#'* Add to benchmarks_merged *
head(benchmarks_merged)

benchmarks_add <- benchmarks_merged[1:length(cuidMissing),]
benchmarks_add$region <- conservationunits_decoder$region[cond]
benchmarks_add$species_name <- species_name
benchmarks_add$species_qualified <- conservationunits_decoder$species_qualified[cond]
benchmarks_add$cuid <- conservationunits_decoder$cuid[cond]
benchmarks_add$cu_name_pse <- conservationunits_decoder$cu_name_pse[cond]

cond_col <- ! colnames(benchmarks_add) %in% c("region","species_name","species_qualified",
                                              "cuid","cu_name_pse")
benchmarks_add[,cond_col] <- NA

benchmarks_merged <- rbind(benchmarks_merged,
                           benchmarks_add)

nrow(benchmarks_merged) # 463

#
# round fish count to closest integer in benchmarks_merged ------
#
head(benchmarks_merged)
head(biological_status_merged)

fields <- c("curr_spw",
            "sr_lower","sr_lower_025","sr_lower_975",
            "sr_upper","sr_upper_025","sr_upper_975",
            "percentile_lower","percentile_lower_025","percentile_lower_975",
            "percentile_upper","percentile_upper_025","percentile_upper_975")

for(f in fields){
  benchmarks_merged[,f] <- round(benchmarks_merged[,f])
}

#
# Check if extinct status matches COSEWIC -----
#

dataset380 <- import_mostRecent_file_fun(wd = paste0(wd_X_Drive1_PROJECTS,
                                                     "/1_Active/Population Methods and Analysis/New features/Integrated status and COSEWIC"),
                                         pattern = "Dataset380")

cond <- !is.na(dataset380$Sheet1$COSEWIC_status) & dataset380$Sheet1$COSEWIC_status == "Extinct"
cuid_extinct_cosewic <- dataset380$Sheet1$cuid[cond]

#
cond <- biological_status_merged$psf_status == "extinct"
cuid_extinct_pse <- biological_status_merged$cuid[cond]

cuid_extinct_cosewic[! cuid_extinct_cosewic %in% cuid_extinct_pse] # NULL
cuid_extinct_pse[! cuid_extinct_pse %in% cuid_extinct_cosewic]     # 758
dataset380$Sheet1[dataset380$Sheet1$cuid %in% c(758),]

#
# Check: is there any CUs with biostatus with last year data point too old ------
#

yr_last_rg <- sapply(region,function(rg){
  cond_rg <- cuspawnerabundance$region == rg
  cond_NA <- is.na(cuspawnerabundance$estimated_count)
  return(max(cuspawnerabundance$year[cond_rg & !cond_NA]))
})

for(i in 1:nrow(biological_status_merged)){ # It should not print anything
  
  if(biological_status_merged$psf_status[i] %in% 1:3){
    cond <- conservationunits_decoder$cuid == biological_status_merged$cuid[i]
    gen_length <- conservationunits_decoder$gen_length[cond]
    region <- conservationunits_decoder$region[cond]
    
    cond <- names(yr_last_rg) == region
    yr_last_rg_here <- yr_last_rg[cond]
    
    cond <- cuspawnerabundance$cuid == biological_status_merged$cuid[i]
    cond_NA <- is.na(cuspawnerabundance$estimated_count)
    yr_last <- max(cuspawnerabundance$year[cond & !cond_NA])
    
    if(yr_last < (yr_last_rg_here - gen_length + 1)){
      print("Issue with:")
      print(conservationunits_decoder[i,])
    }
  }
}

#
# MANUAL CHANGES: for Fraser CO and SH -----
# From population meeting in 18/12/2024
# 

# Set to not assessed the 5 Fraser CO CUs for now until we figure out what to do 
# with them (the fit looks weird)
cuid_here <- c(705,707:709,749)
cond <- biological_status_merged$cuid %in% cuid_here
biological_status_merged[cond,]
biological_status_merged$sr_red_prob[cond] <- NA
biological_status_merged$sr_yellow_prob[cond] <- NA
biological_status_merged$sr_green_prob[cond] <- NA
biological_status_merged$sr_status[cond] <- NA
biological_status_merged$psf_status_code[cond] <- 6
biological_status_merged$psf_status_type[cond] <- NA
biological_status_merged$psf_status[cond] <- "not-assessed"

# use the percentile approach for the two SH Fraser CUs (THIS SHOULD BE PERMANENT)
cuid_here <- c(780,781)
cond <- biological_status_merged$cuid %in% cuid_here
biological_status_merged[cond,]
biological_status_merged$psf_status[cond] <- biological_status_merged$percentile_status[cond]
biological_status_merged$psf_status_code[cond] <- sapply(biological_status_merged$psf_status[cond],FUN = function(bs){
  return((3:1)[bs == c("poor","fair","good")])
})
biological_status_merged$psf_status_type[cond] <- "percentile"

#
# Export files /dataset101_biological_status and dataset102_benchmarks -------
# This is just to edit the file name
# https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1721074762139209?thread_ts=1701199596.229739&cid=CJG0SHWCW
# benchmarks_merged <- import_mostRecent_file_fun(wd = wd_output,
#                                                 pattern = "Benchmarks_HBSR_Percentile_all_")
# 
# biological_status_merged <- import_mostRecent_file_fun(wd = wd_output,
#                                                        pattern = "Biological_status_HBSR_Percentile_all_")

# write files in /output/archive
date <- as.character(Sys.Date())
date <- "2025-06-03"

# write in the /output/archive in dropbox
write.csv(biological_status_merged,
          paste0(wd_output,"/archive/dataset101_biological_status_",date,".csv"),
          row.names = F)

write.csv(benchmarks_merged,
          paste0(wd_output,"/archive/dataset102_benchmarks_",date,".csv"),
          row.names = F)

# write files in /output locally to push to github
write.csv(biological_status_merged,
          paste0(getwd(),"/output/dataset101_biological_status.csv"),
          row.names = F)

write.csv(benchmarks_merged,
          paste0(getwd(),"/output/dataset102_benchmarks.csv"),
          row.names = F)


# check the CUs with different biostatus between SR and percentile

cond <- biological_status_merged$sr_status != biological_status_merged$percentile_status &
  !is.na(biological_status_merged$sr_status) &
  !is.na( biological_status_merged$percentile_status)

biological_status_merged[cond,]
biological_status_merged[cond,] |> nrow()





