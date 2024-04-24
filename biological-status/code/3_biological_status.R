
#'******************************************************************************
#' The goal of the script is to analyse the biological status probabilities 
#' obtained from the HBSRM analysis.
#' 
#' Files imported (from ):
#' - region_species_biological_status.csv (created in benchmarks_HBSRM.R)
#' 
#' Files produced: 
#' - Biological_status_HBSR_Percentile_all.csv


#' - Biological_status_diff_SMsySmsy80_percent05075.csv (biological_status_merge_diff)
#' - comparison_bioStatusPercentiles_75_50_region.jpeg
#' - comparison_bioStatusPercentiles_75_50_species.jpeg
#' - comparison_bioStatus_Smsy_Smsy80_region.jpeg
#' - comparison_bioStatus_Smsy_Smsy80_species.jpeg
#' - comparison_bioStatus_HBSR_Percentiles.jpeg
#' - data/code_PSF_Status.csv
#' 
#' Notes:
#' - 
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

# return the name of the directories for the different projects:
subDir_projects <- subDir_projects_fun()

wds_l <- set_working_directories_fun(subDir = subDir_projects$biological_status,
                                     Export_locally = F)
wd_head <- wds_l$wd_head
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_data_dropbox_dropbox <- wds_l$wd_X_Drive1_PROJECTS
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

# Import species names and acronyms
species_acronym_df <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#------------------------------------------------------------------------------#
# Analyses
#------------------------------------------------------------------------------#

# select all the regions
region <- as.character(regions_df[1,])

# select certain species
species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "CK"],    
             species_acronym_df$species_name[species_acronym_df$species_acro == "SX"])

# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- TRUE

printFig <- F

#
# 1) Import Datasets -----

#'* Import biostatus obtained with HBSR Sgen - Smsy: *
pattern <- "biological_status_HBSRM"
biological_status_HBSRM <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)
head(biological_status_HBSRM)
colnames(biological_status_HBSRM)
nrow(biological_status_HBSRM) # 136
unique(biological_status_HBSRM$comment)

# add column biostatus for both thresholds (Smsy and 80% Smsy)
colProb <- colnames(biological_status_HBSRM)[grepl("Smsy_",colnames(biological_status_HBSRM))]
biological_status_HBSRM$status_Smsy <- sapply(X = 1:nrow(biological_status_HBSRM), 
                                             FUN = function(r){
                                               # r <- 1
                                               slice <- biological_status_HBSRM[r,colProb]
                                               out <- c("red","amber","green")[slice == max(slice)][1]
                                               return(out)
                                             })

colProb <- colnames(biological_status_HBSRM)[grepl("Smsy80_",colnames(biological_status_HBSRM))]
biological_status_HBSRM$status_Smsy80 <- sapply(X = 1:nrow(biological_status_HBSRM), 
                                               FUN = function(r){
                                                 # r <- 1
                                                 slice <- biological_status_HBSRM[r,colProb]
                                                 out <- c("red","amber","green")[slice == max(slice)][1]
                                                 return(out)
                                               })

#'* Import the benchmark values associated with the HBSRM *
pattern <- "benchmarks_summary_HBSRM"
benchmarks_summary_HBSRM <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                       wd_output = wd_output,
                                                       region = region,
                                                       species_all = F)

head(benchmarks_summary_HBSRM)
nrow(unique(benchmarks_summary_HBSRM[,c("region","species","CU")])) # 136

# merge biological_status_HBSRM with benchmarks_summary_HBSRM with the highest posterior density estimate
benchmarks_summary_HBSRM
benchmarks_summary_HBSRM_Smsy_HPD <- benchmarks_summary_HBSRM[benchmarks_summary_HBSRM$benchmark == "Smsy" &
                                                          benchmarks_summary_HBSRM$method == "HPD",]

final_HBSRM <- merge(x = biological_status_HBSRM, 
                     y = benchmarks_summary_HBSRM_Smsy_HPD[,c("region","species","CU","m")],
                     by = c("region","species","CU"),
                     all.x = T)

#'* Import the biological status obtained with HS percentile method: *
pattern <- "biological_status_percentiles"
biological_status_percentile <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)

nrow(biological_status_percentile) # 447

# add final biostatus for both thresholds (i.e., 0.75 and 0.5 upper threshold)
colProb <- colnames(biological_status_percentile)[grepl("_075_",colnames(biological_status_percentile))]
biological_status_percentile$status_percent075 <- sapply(X = 1:nrow(biological_status_percentile), 
                                           FUN = function(r){
                                             # r <- 1
                                             slice <- biological_status_percentile[r,colProb]
                                             out <- c("red","amber","green")[slice == max(slice)][1]
                                             return(out)
                                           })

colProb <- colnames(biological_status_percentile)[grepl("_05_",colnames(biological_status_percentile))]
biological_status_percentile$status_percent05 <- sapply(X = 1:nrow(biological_status_percentile), 
                                                     FUN = function(r){
                                                       # r <- 1
                                                       slice <- biological_status_percentile[r,colProb]
                                                       out <- c("red","amber","green")[slice == max(slice)][1]
                                                       return(out)
                                                     })

#'* Import the percentile benchmark values *
pattern <- "benchmarks_summary_percentiles"
benchmarks_summary_percentile <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                                 wd_output = wd_output,
                                                                 region = region,
                                                                 species_all = F)

head(benchmarks_summary_percentile)

#'* Import the conservationunits_decoder.csv *
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- F

conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

nrow(unique(conservationunits_decoder[,c("region","species_name","cu_name_pse")]))

#'* Import dataset390 for survey_quality *
dataset390 <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[18],
                                    fromDatabase = fromDatabase,
                                    update_file_csv = update_file_csv,
                                    wd = wd_pop_indic_data_input_dropbox)
head(dataset390)
dataset390 <- dataset390[,c("region","species_name","cuid","cu_name_pse","catch_method")]
# survey_quality as in data-quality.R: 
# "Low" ~ 1
# "Medium-Low" ~ 2
# "Medium" ~ 3
# "Medium-High" ~ 4
# "High" ~ 5

# QUESTION: there is something wrong 
# cond <- dataset390$cuid %in% c(729,752)
# dataset390[cond,]
# 
# pattern <- "dataset390"
# dataset390_old <- import_mostRecent_file_fun(wd = paste0(wd_pop_data_quality_dropbox,"/output"),
#                                          pattern = pattern)
# dataset390_old <- read.csv(paste0( paste0(wd_pop_data_quality_dropbox,"/output"),"/dataset390_2023-05-20.csv"))
# dataset390_old <- dataset390_old[,c("regionname","species","cuid","cu_name_pse","survey_quality")]
# dataset390_old$survey_quality
# 
# # QUESTION: there is something wrong 
# cond <- dataset390_old$cuid %in% c(729,752)
# dataset390_old[cond,]


#'* Import list of CUs with high exploitation and/or low productivity *
# Return list of CUs that have high exploitation rate or low production rates,
# as well as a final call on keeping or removing the CUs depending of their
# biostatus: the one with already a red/poor status are kept (i.e. Clare's 8th rule).
highExploit_lowProd <- cu_highExploit_lowProd_fun(biological_status_percentile,
                                                  wd_output = wd_output, 
                                                  conservationunits_decoder = conservationunits_decoder)

# TO REMOVE BELOW? not used and does not work
# cond <- grepl("_Smsy_",colnames(biological_status_HBSRM)) |
#   grepl("_Smsy80_",colnames(biological_status_HBSRM))
# colRemove_HBSRM <- c(colnames(biological_status_HBSRM)[cond],"comment")
# 
# cond <- grepl("_HSPercent_",colnames(biological_status_percentile))
# colRemove_Percent <- c(colnames(biological_status_percentile)[cond],
#                        "dataPointNb","comment")
# 
# colComm <- c("region","species","cuid","CU_pse","CU_dfo")
# colComm <- unique(c(colnames(biological_status_HBSRM),
#                     colnames(biological_status_percentile)))
# colComm <- colComm[!colComm %in% c(colRemove_HBSRM,colRemove_Percent,
#                                    "status_Smsy","status_Smsy80",
#                                    "status_percent075","status_percent05")]
# 
# biological_status_all <- merge(x = biological_status_HBSRM[,!colnames(biological_status_HBSRM) %in% colRemove_HBSRM], 
#                                y = biological_status_percentile[,!colnames(biological_status_percentile) %in% colRemove_Percent], 
#                                by = colComm, 
#                                all = T)

cond <- grepl("Swan/Club",biological_status_HBSRM$CU_pse)
biological_status_HBSRM[cond,]
cond <- grepl("Swan/Club",biological_status_percentile$CU_pse)
biological_status_percentile[cond,]


#
# 2) Create complete dataset with biostatus and psf_staus_code -----
# https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1701464111241899?thread_ts=1701199596.229739&cid=CJG0SHWCW

#' TODO: add the extra rule 2) in update chart: "is there a spawner-recruit relationship
#' available with the CU-level catch estimates medium-low quality or higher"
#' #' cf. Population "2024 Population Analysis running notes" at March 19 for 
#' corresponding documentation. 
#' https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing
#' The data quality estimates are in:
#' population-indicators/data-quality/output as dataset390-2023-05-20.csv

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

# Make this list into a dataframe that can be communiticated
code_PSF_Status <- data.frame(psf_status_code = 1:9,
                              psf_status = c("good","fair","poor","extinct",
                                             "not-assessed","not-assessed",
                                             "data-deficient","data-deficient",
                                             "data-deficient"),
                              comment = c(rep("",4),
                                          "cyclic dominance",
                                          "low productivity or high exploitation",
                                          "insufficient time series length",
                                          "no estimates of spawner abundance in the most recent generation",
                                          "no spawner estimates available"))
# 
# write.csv(code_PSF_Status,paste0(wd_data_dropbox,"/code_PSF_Status.csv"),
#           row.names = F)

# A. add column psf_status_code to each dataset:
biological_status_HBSRM$psf_status_code <- NA
biological_status_percentile$psf_status_code <- NA

#'* 4 = extinct *
cu_extinct <- cu_extinct_fun()

row_toUpdate <- biological_status_HBSRM$cuid %in% cu_extinct$cuid
val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,4, sep = ", ")
biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- biological_status_percentile$cuid %in% cu_extinct$cuid
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,4, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

#'* 5 = not-assessed (cyclic dominance) *
row_toUpdate <- grepl("(cyclic)",biological_status_HBSRM$CU_pse)
val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,5, sep = ", ")
biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- grepl("(cyclic)",biological_status_percentile$CU_pse)
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,5, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

#'* 6 = not-assessed (low productivity or high exploitation) --> Percentile only *
for(r in 1:nrow(highExploit_lowProd)){
  # r <- 1
  cuid <- highExploit_lowProd$cuid[r]
  
  cond <- biological_status_percentile$cuid == cuid
  
  val_toUpdate <- biological_status_percentile$psf_status_code[cond]
  val_new <- paste(val_toUpdate,6, sep = ", ")
  
  if(!is.na(highExploit_lowProd$biostatus_percentile[r]) &
     highExploit_lowProd$biostatus_percentile[r] == "red"){
    
    val_new <- paste(val_new,3, sep = ", ")
    val_new  <- strsplit(split = ", ", x = val_new)[[1]]
    val_new <- val_new[val_new != "NA"]
    val_new <- as.numeric(val_new)
    val_new <- sort(val_new)
    val_new <- paste(val_new, collapse = ", ")
  }
  #biological_status_percentile[cond,]
  biological_status_percentile$psf_status_code[cond] <- val_new
}

#'* 7 = data-deficient (insufficient time series length) --> Percentile only *
row_toUpdate <- biological_status_percentile$dataPointNb < 20
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,7, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

#'* 8 = data-deficient (no estimates of spawner abundance in the most recent generation) *
row_toUpdate <- biological_status_HBSRM$genLength_dataPointNb == 0
val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,8, sep = ", ")
biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- biological_status_percentile$genLength_dataPointNb == 0
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,8, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

#'* 9 = data-deficient (no spawner estimates available) *
row_toUpdate <- biological_status_HBSRM$comment == "Only NAs in cuspawnerabundance.csv for this CU" &
  !is.na(biological_status_HBSRM$comment)
val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,9, sep = ", ")
biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- biological_status_percentile$comment  == "Only NAs in cuspawnerabundance.csv for this CU" &
  !is.na(biological_status_percentile$comment)
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,9, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

# Remove "NA, " in psf_status_code
biological_status_HBSRM$psf_status_code <- gsub("NA, ","",biological_status_HBSRM$psf_status_code)
biological_status_percentile$psf_status_code <- gsub("NA, ","",biological_status_percentile$psf_status_code)

unique(biological_status_HBSRM$psf_status_code)
unique(biological_status_percentile$psf_status_code)

# B. Combine the two datasets:

colCommon <- c("region","species","cuid","CU_pse","current_spawner_abundance",
               "psf_status_code")
              # "year_last","year_first","genLength")
colHBSR <- c("status_Smsy_red","status_Smsy_amber","status_Smsy_green",
             "status_Smsy")
colPercent <- c("status_percent_075_red","status_percent_075_amber","status_percent_075_green",
                "status_percent075")

biological_status_merged <- merge(x = biological_status_HBSRM[,c(colCommon,colHBSR)],
                                  y = biological_status_percentile[,c(colCommon,colPercent)],
                                  by =  c("region","species","cuid","CU_pse","current_spawner_abundance"), 
                                  all = T)
head(biological_status_merged)

# Check if number of CUs is correct:
CUs_comm <- biological_status_HBSRM$cuid[biological_status_HBSRM$cuid %in% 
                                          biological_status_percentile$cuid]
length(CUs_comm) # 136
CUs_HBSRM_only <- biological_status_HBSRM$cuid[!biological_status_HBSRM$cuid %in% 
                                               biological_status_percentile$cuid]
length(CUs_HBSRM_only) # 0
CUs_Percent_only <- biological_status_percentile$cuid[!biological_status_percentile$cuid %in% 
                                                       biological_status_HBSRM$cuid]
length(CUs_Percent_only) # 311

# Expected number of rows in biological_status_merged:
length(CUs_comm) + length(CUs_HBSRM_only) + length(CUs_Percent_only) # 447
nrow(biological_status_merged) # 447 --> ALL GOOD

# Renames columns
colnames(biological_status_merged) <- gsub("red","red_prob",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("amber","yellow_prob",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("green","green_prob",colnames(biological_status_merged))

colnames(biological_status_merged) <- gsub("status_Smsy_","sr_",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_percent_075_","percentile_",colnames(biological_status_merged))

colnames(biological_status_merged) <- gsub("status_Smsy","sr_status",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_percent075","percentile_status",colnames(biological_status_merged))

#'* Create psf_status_code_all fields & attribute 1 (good), 2 (fair) or 3 (poor) * 
biological_status_merged$psf_status_code_all <- NA # values: 1 to 9

col_prob <- colnames(biological_status_merged)[grepl("_prob",colnames(biological_status_merged))]
col_sr_prob <- col_prob[grepl("sr_",col_prob)]
col_percent_prob <- col_prob[grepl("percentile_",col_prob)]

#
for(r in 1:nrow(biological_status_merged)){
  # r <- 170
  bs_here <- biological_status_merged[r,]
  
  # *** HBSRM method ***
  
  # Are the probabilities available and the status_code is NA (i.e. not 4, ...9)
  cond_HBRSM <- !is.na(bs_here$sr_red_prob) & is.na(bs_here$psf_status_code.x) # is.na(bs_here$psf_status_code.x) might not be necessary but does not hurt
  
  #' Rule 2: Is there a SR relationship with CU-level catch estimates >= medium-low
  # survey_quality 
  # In data-quality.R: 
  # "Low" ~ 1
  # "Medium-Low" ~ 2
  # "Medium" ~ 3
  # "Medium-High" ~ 4
  # "High" ~ 5
  cond <- dataset390$cuid == biological_status_merged$cuid[r]
  if(is.na(dataset390$catch_method[cond])){  # QUESTION: is that normal? Why is it NA?  https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1713912766189429
    cond_HBRSM_2 <- T # F
    # break
  }else{
    #' Steph: "data_quality scores that are NA or zero should not prevent HBSR
    #'status form being shown. Only low (1) or medium-low (2) catch_quality should
    #' be part of the decision rule"
    #' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1713982063020029?thread_ts=1713912766.189429&cid=CJ5RVHVCG
    cond_HBRSM_2 <- dataset390$catch_method[cond] != 1
  }
  if(cond_HBRSM & cond_HBRSM_2){
    
    # psf_status_here <- c("poor","fair","good")[bs_here[,col_sr_prob] == max(bs_here[,col_sr_prob])]
    # psf_status_code_all_here <- c(3:1)[psf_status_here == c("poor","fair","good")]
    psf_status_code_all_here <- c(3:1)[bs_here[,col_sr_prob] == max(bs_here[,col_sr_prob])]

    
  # *** Percentile method ***
    
  }else if(!is.na(bs_here$percentile_red_prob) & is.na(bs_here$psf_status_code.y)){
    #' Note that the CU with high exploitattion / low productivity are excluded
    #' here regardless if they have poor status or not but that's ok because
    #' their psf_status_code.y was already set to "3, 6" above. 
    
    # psf_status_here <- c("poor","fair","good")[bs_here[,col_percent_prob] == max(bs_here[,col_percent_prob])]
    # psf_status_code_all_here <- c(3:1)[psf_status_here == c("poor","fair","good")]
    psf_status_code_all_here <- c(3:1)[bs_here[,col_percent_prob] == max(bs_here[,col_percent_prob])]
  
  }else{ # biostatus is not available
    
    code_HBSRM <- bs_here$psf_status_code.x
    code_Percentile <- bs_here$psf_status_code.y
    code_both <- c(code_HBSRM, code_Percentile)
    code_both <- code_both[!is.na(code_both)]
    code_both <- sapply(X = code_both, FUN =  strsplit, split = ", ")
    code_both <- as.numeric(code_both[[1]])
    code_both <- sort(code_both)
    code_both <- unique(code_both)
    
    code_both <- paste(code_both,collapse = ", ")
    psf_status_code_all_here <- code_both
  }

  biological_status_merged$psf_status_code_all[r] <- psf_status_code_all_here
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
#' Decisions for psf_status_code when multiple numbers in psf_status_code_all:
#' - If 7, 8, 9 then display 8 (9 implies 7 & 8 and it is not in decision rules) 
#' - If 7, 8, then display 8
#' Reason: this is the first step in the decision tree. If there is no recent
#' spawner abundance then we cannot assess status regardless of benchmarks.
#' - Similarly if 6,8 then display 8
#' - If 4, 8 then display 4! (NEED TO MAKE SURE THE EXTINCT CATEGORY IS UPDATED)
#' (cf. Population meeting April 16 2024)
#' https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing
#' - "3, 6, 7" --> 7
#' - "6, 7, 8" --> 8
#' - "6, 7"    --> 7
#' - "3, 6"    --> 3 (the exception)
biological_status_merged$psf_status_code <- biological_status_merged$psf_status_code_all

cond_4 <- grepl("4,",biological_status_merged$psf_status_code_all) 
biological_status_merged$psf_status_code_all[cond_4]
biological_status_merged$psf_status_code[cond_4] <- 4

cond_8 <- grepl("8",biological_status_merged$psf_status_code_all) & !cond_4
unique(biological_status_merged$psf_status_code_all[cond_8])
biological_status_merged$psf_status_code[cond_8] <- 8

cond_7 <- grepl("7",biological_status_merged$psf_status_code_all) & !cond_4 & !cond_8
unique(biological_status_merged$psf_status_code_all[cond_7])
biological_status_merged$psf_status_code[cond_7] <- 7

cond_36 <- grepl("3, 6",biological_status_merged$psf_status_code_all) & !cond_7
unique(biological_status_merged$psf_status_code_all[cond_36])
biological_status_merged$psf_status_code[cond_36] <- 3

unique(biological_status_merged$psf_status_code) # all good

#'* Create psf_status * 
biological_status_merged$psf_status <- apply(X = biological_status_merged, 
                                             MARGIN = 1, FUN = function(r){
                                               code <- r["psf_status_code"]
                                               cond <- code_PSF_Status$psf_status_code == code
                                               return(code_PSF_Status$psf_status[cond])
                                             })

unique(biological_status_merged$psf_status)
table(biological_status_merged$psf_status)

# Drop necessary columns:
colToDrop <- c("psf_status_code.x","psf_status_code.y")
biological_status_merged <- biological_status_merged[,!colnames(biological_status_merged) %in% colToDrop]

# rename CU_pse
colnames(biological_status_merged)[colnames(biological_status_merged) == "CU_pse"] <- "cu_name_pse"

# add species_name and species_abbr and remove species
biological_status_merged$species_name <- sapply(X = biological_status_merged$cuid,
                                                FUN = function(cuid){
                                                  cond <- conservationunits_decoder$cuid == cuid
                                                  return(conservationunits_decoder$species_name[cond])
                                                })
biological_status_merged$species_abbr <- sapply(X = biological_status_merged$cuid,
                                                FUN = function(cuid){
                                                  cond <- conservationunits_decoder$cuid == cuid
                                                  return(conservationunits_decoder$species_abbr[cond])
                                                })
biological_status_merged <- biological_status_merged[,colnames(biological_status_merged) != "species"]

# Check that CUs without current spawner abundance info have the number 8
cond <- is.na(biological_status_merged$current_spawner_abundance)
unique(biological_status_merged[,c("psf_status_code","psf_status_code_all")][cond,])

# check that the CUs with status code 6 (high exploitation rate or production rates)
# with red status are still available:
cond <- grepl("6",biological_status_merged$psf_status_code_all)
biological_status_merged[cond,]

# re-arrange columns and drop current_spawner_abundance
col_prob <- colnames(biological_status_merged)[grepl("_prob",colnames(biological_status_merged))]
col_status <- colnames(biological_status_merged)[grepl("_status",colnames(biological_status_merged))]
biological_status_merged <- biological_status_merged[,c("region","cuid","species_abbr",
                                                        "species_name","cu_name_pse",
                                                        col_prob,col_status)]
head(biological_status_merged)

# check if the psf code of cyclic communities: should be 5
biological_status_merged[grepl("cyclic",biological_status_merged$cu_name_pse),]

# Number CUs total:
nrow(biological_status_merged) # 447

# number CUs with biostatus assessed over both methods
condition <- biological_status_merged$psf_status_code %in% 1:3
sum(condition) # 192

# CUs not assessed because cyclic dynamics, low productivity/high mortality or data deficient
condition_5 <- grepl(pattern = "5",biological_status_merged$psf_status_code_all)
condition_6 <- grepl(pattern = "6",biological_status_merged$psf_status_code_all)
condition_7 <- grepl(pattern = "7",biological_status_merged$psf_status_code_all)
condition_5_6_7 <- condition_5 | condition_6 | condition_7
biological_status_merged$psf_status_code_all[condition_5_6_7]
sum(condition_5_6_7) # 263

# number CUs with biostatus assessed with HBSRM:
condition_1_2_3 <- biological_status_merged$psf_status_code %in% 1:3
condition_HBSRM <- !is.na(biological_status_merged$sr_status) # 
nrow(biological_status_merged[condition_1_2_3 & condition_HBSRM,]) # 135

# number CUs with biostatus assessed with percentile method: 
condition_Percent <- !is.na(biological_status_merged$percentile_status)
nrow(biological_status_merged[condition_1_2_3 & condition_Percent,]) # 209
nrow(biological_status_merged[condition_1_2_3 & condition_Percent & !condition_HBSRM,]) # 74

# Write the file -----
#
write.csv(biological_status_merged,paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),
          row.names = F)

biological_status_merged <- read.csv(paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),
                                     header = T)


#
# 3) Figures biological status based on HBSRM comparison Smsy vs. 80% Smsy ------
#
condition_1_2_3 <- biological_status_merged$psf_status_code %in% 1:3
condition_HBSRM <- !is.na(biological_status_merged$sr_status) # 107
nrow(biological_status_merged[condition_1_2_3 & condition_HBSRM,]) # 107
nrow(biological_status_merged[condition_HBSRM,]) # 107

cuids_HBSRM <- biological_status_merged$cuid[condition_1_2_3 & condition_HBSRM]
condition <- biological_status_HBSRM$cuid %in% cuids_HBSRM
biological_status_HBSRM[condition,]

biological_status_compare_fun(biological_status_df = biological_status_HBSRM[condition,],
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "region")

biological_status_compare_fun(biological_status_df = biological_status_HBSRM[condition,],
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "species")

# number of CUs with different status between Smsy and Smsy80%:
biological_status_HBSRM_cut <- biological_status_HBSRM[condition,]
condition_diff <- biological_status_HBSRM_cut$status_Smsy != biological_status_HBSRM_cut$status_Smsy80
biological_status_HBSRM_cut[condition_diff,]
nrow(biological_status_HBSRM_cut[condition_diff,]) # 14

#
# 4) Figures comparison biological status HS abundance percentiles 0.75 vs. 0.50 -----
#
condition_1_2_3 <- biological_status_merged$psf_status_code %in% 1:3
condition_HBSRM <- !is.na(biological_status_merged$sr_status) 
sum(condition_HBSRM) # 107
condition_Percent <- !is.na(biological_status_merged$percentile_status) #
sum(condition_Percent) # 172
nrow(biological_status_merged[condition_1_2_3 & !condition_HBSRM & condition_Percent,]) # 44

cuids_Percent <- biological_status_merged$cuid[condition_1_2_3 & !condition_HBSRM & condition_Percent] 
condition <- biological_status_percentile$cuid %in% cuids_Percent

biological_status_compare_fun(biological_status_df = biological_status_percentile[condition,],
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "region")

biological_status_compare_fun(biological_status_df = biological_status_percentile[condition,],
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "species")

# number of CUs with different status between upper 0.5 and 0.75 :
biological_status_percentile_cut <- biological_status_percentile[condition,]
condition_diff <- biological_status_percentile_cut$status_percent075 != biological_status_percentile_cut$status_percent05
biological_status_percentile_cut[condition_diff,]
nrow(biological_status_percentile_cut[condition_diff,]) # 7

#
# 4) Figure that compares biological status between HBSR and HS percentiles approach -----
#
condition_1_2_3 <- biological_status_merged$psf_status_code %in% 1:3
condition_HBSRM <- !is.na(biological_status_merged$sr_status) # 107
condition_Percent <- !is.na(biological_status_merged$percentile_status) # 172

bioStatus_merged <- biological_status_merged[condition_1_2_3 & (condition_HBSRM | condition_Percent),]
nrow(bioStatus_merged) # 151

# count how many CUs have the same biostatus with both approaches
bioStatus_merged_noNA <- bioStatus_merged[!is.na(bioStatus_merged$sr_status) &
                                            !is.na(bioStatus_merged$percentile_status),]
nrow(bioStatus_merged_noNA) # 107

# Same status:
bioStatus_merged_same <- bioStatus_merged_noNA[bioStatus_merged_noNA$sr_status == bioStatus_merged_noNA$percentile_status,]
nrow(bioStatus_merged_same) # 53
countHere <- table(factor(bioStatus_merged_same$sr_status,levels = c("red","amber","green")))
table_n <- data.frame(bioStatus = c("red","amber","green"),
                      n_same = as.numeric(countHere))

# Status only values for HBSRM:
bioStatus_merged_HBSRM_only <- bioStatus_merged[!is.na(bioStatus_merged$sr_status) & 
                                                 is.na(bioStatus_merged$percentile_status),]
nrow(bioStatus_merged_HBSRM_only) # 0
countHere <- table(factor(bioStatus_merged_HBSRM_only$sr_status,levels = c("red","amber","green")))
table_n$HBSR_only <- as.numeric(countHere)

# Status only values for HS benchmarks:
bioStatus_merged_percentile_only <- bioStatus_merged[is.na(bioStatus_merged$sr_status) & 
                                                    !is.na(bioStatus_merged$percentile_status),]
nrow(bioStatus_merged_percentile_only) # 44
countHere <- table(factor(bioStatus_merged_percentile_only$percentile_status,levels = c("red","amber","green")))
table_n$HSBench_only <- as.numeric(countHere)

# Different status:
bioStatus_merged_diff <- bioStatus_merged_noNA[bioStatus_merged_noNA$sr_status != bioStatus_merged_noNA$percentile_status,]
nrow(bioStatus_merged_diff) # 54
countHere <- table(factor(bioStatus_merged_diff$sr_status,levels = c("red","amber","green")))
table_n$diff <- as.numeric(countHere)

table_m <- as.matrix(table_n[,c("n_same","HBSR_only","HSBench_only","diff")])
rownames(table_m) <- table_n$bioStatus

coloursStatus <- rev(c(g = "#8EB687", a = "#DFD98D", r = "#9A3F3F"))

if(printFig){
  jpeg(paste0(wd_figures,"/comparison_bioStatus_HBSR_Percentiles.jpg"), 
       width = 20,height = 15, units = "cm", res = 300)
}
barplot(height = table_m, col = coloursStatus, 
        ylim = c(0,max(apply(table_m,2,sum)) + 5), las = 1,
        ylab = "Number of CUs",xlab = "Biological status difference",
        main = "Bio-status differences with HBSR vs. percentiles methods",
        names.arg = c('Same','HBSR only','percentiles only','Different'))
#polygon(x = c(1,2,2,1),y = c(20,20,60,60))
offset <- .2
barLarger <- 1
x0 <- offset*4 + (ncol(table_m)-1)*barLarger + barLarger/2
x1 <- x0 + barLarger/2
# create a polygons for each possible combination of biostatus
height <- 0
for(bs in c("red","amber","green")){
  # bs <- c("red","amber","green")[2]
  bioStatus_merged_diff_cut <- bioStatus_merged_diff[bioStatus_merged_diff$sr_status == bs,]
  if(nrow(bioStatus_merged_diff) > 0){
    bioStatusHSPercentHere <- unique(bioStatus_merged_diff_cut$percentile_status)
    for(bshsp in bioStatusHSPercentHere){
      # bshsp <- bioStatusHSPercentHere[]
      colourHere <- coloursStatus[bshsp == c("red","amber","green")]
      bioStatus_merged_diff_cut2 <- bioStatus_merged_diff_cut[bioStatus_merged_diff_cut$percentile_status == bshsp,]
      heightUP <- nrow(bioStatus_merged_diff_cut2) + height
      polygon(x = c(x0,x1,x1,x0),y = c(height,height,heightUP,heightUP),col = colourHere)
      height <- heightUP
    }
  }
}
# add the correspondging method for the lab bar
text(labels = "HBSR",x = x0 - barLarger/4, y = height, pos = 3, cex = .8)
text(labels = "percentiles",x = x0 + barLarger/4, y = height, pos = 3, cex = .8)
#
if(printFig){
  dev.off()
}

# big contrasts:
bioStatus_merged_diff[bioStatus_merged_diff$sr_status == "red" & 
                        bioStatus_merged_diff$percentile_status == "green",]

#
# 5) change of status between the old vs. new upper threshold: -----
#
condition_1_2_3 <- biological_status_merged$psf_status_code %in% 1:3
condition_HBSRM <- !is.na(biological_status_merged$sr_status) # 107
condition_Percent <- !is.na(biological_status_merged$percentile_status) # 172

cuids_percent <- biological_status_merged[condition_1_2_3 & condition_Percent & !condition_HBSRM,]$cuid
biological_status_merged[biological_status_merged$cuid %in% cuids_percent,]
cuids_HBSRM <- biological_status_merged[condition_1_2_3 & condition_HBSRM,]$cuid
biological_status_merged[biological_status_merged$cuid %in% cuids_HBSRM,]

condition_HBSRM_diff <- biological_status_HBSRM$status_Smsy !=  biological_status_HBSRM$status_Smsy80
biological_status_HBSRM_diff <- biological_status_HBSRM[biological_status_HBSRM$cuid %in% cuids_HBSRM & 
                                                        condition_HBSRM_diff,]
biological_status_HBSRM_diff$biostatus_old <- biological_status_HBSRM_diff$status_Smsy80
biological_status_HBSRM_diff$biostatus_new <- biological_status_HBSRM_diff$status_Smsy
biological_status_HBSRM_diff$benchmark_type <- "HBSRM"

condition_percentile_diff <- biological_status_percentile$status_percent05 !=  biological_status_percentile$status_percent075
biological_status_percentile_diff <- biological_status_percentile[biological_status_percentile$cuid %in% cuids_percent & 
                                                              condition_percentile_diff,]
biological_status_percentile_diff$biostatus_old <- biological_status_percentile_diff$status_percent05
biological_status_percentile_diff$biostatus_new <- biological_status_percentile_diff$status_percent075
biological_status_percentile_diff$benchmark_type <- "percentile"

colInCommon <- c("region","species","cuid","CU_pse","biostatus_old","biostatus_new",
                 "benchmark_type")

biological_status_merge_diff <- rbind(biological_status_HBSRM_diff[,colInCommon],
                                      biological_status_percentile_diff[,colInCommon])

sum(biological_status_merge_diff$cuid %in% cuids_percent) # 7
sum(biological_status_merge_diff$cuid %in% cuids_HBSRM)   # 14

nrow(biological_status_merge_diff) # 21
biological_status_merge_diff[biological_status_merge_diff$cuid %in% cuids_percent,]
biological_status_merge_diff[biological_status_merge_diff$cuid %in% cuids_HBSRM,]

biological_status_merge_diff$cuid[biological_status_merge_diff$benchmark_type == "HBSRM"]
biological_status_merge_diff$cuid[biological_status_merge_diff$cuid %in% cuids_HBSRM]

biological_status_merge_diff[biological_status_merge_diff$benchmark_type == "HBSRM",]

# write.csv(biological_status_merge_diff,paste0(wd_output,"/Biological_status_diff_SMsySmsy80_percent05075.csv"),
#           row.names = F)

biological_status_merge_diff <- read.csv(paste0(wd_output,"/Biological_status_diff_SMsySmsy80_percent05075.csv"),
                                         header = T)

#
# Check the difference between normal percentile benchmarks and the simulated ones -----
nrow(benchmarks_summary_percentile)
benchmarks_summary_percentile_noNA <- benchmarks_summary_percentile[!is.na(benchmarks_summary_percentile$m),]
nrow(benchmarks_summary_percentile_noNA)
percent_diff <- (benchmarks_summary_percentile_noNA$m - benchmarks_summary_percentile_noNA$m_sim) / benchmarks_summary_percentile_noNA$m * 100
hist(percent_diff)

benchmarks_summary_percentile_noNA[percent_diff < -50,]
# do those have cyclic dynamics ?

# ) TODELETE Decision rules for HBSR and percentile benchmarks -------
#' The 5 rules to exclude data:
#' - cyclic dominance --> for both methods
#' - low productivity or high exploitation rate (EXCEPT IF ALREADY RED) --> for percentile
#' - data deficient  --> for both methods
#' - no current abundance --> for both methods
#' - insufficient time series length (< 20 data points) --> for percentile
#' New rule from Claire: low productivity or high exploitation rate (EXCEPT IF ALREADY RED)
#' 

# Steph diagram:
# https://www.dropbox.com/s/p0gf5aswd2kbd2p/Status_assessment_flowchart_SP.pptx?dl=0

# Clare's slack thread about the 8th decision rule:
# https://salmonwatersheds.slack.com/archives/CPD76USTU/p1700508091399359
# 

# List CUs with high exploitation/low productivity & cyclic dominance
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1700673066049189?thread_ts=1700604709.505309&cid=CJ5RVHVCG

# Return list of CUs that have high exploitation rate or low production rates,
# as well as a final call on keeping or removing the CUs depending of their
# biostatus: the one with already a red/poor status are kept (i.e. Clare's 8th rule).
highExploit_lowProd <- cu_highExploit_lowProd_fun(biological_status_percentile)

# Are all these CUs in --> yes
highExploit_lowProd$CU_name[! highExploit_lowProd$CU_name %in% biological_status_percentile$CU_pse]

nrow(biological_status_percentile) # 428

# Remove Cus with < 20 data points in biological_status_percentile:
biological_status_percent_cut <- biological_status_percentile[biological_status_percentile$dataPointNb >= 20,]
nrow(biological_status_percent_cut) # 206

# Remove the cyclic ones in both biological_status_percentile and biological_status_HBSRM
# the cyclic have it written in name (e.g. Chilliwack-Early Summer (cyclic))
biological_status_percent_cut <- biological_status_percent_cut[!grepl("(cyclic)",biological_status_percent_cut$CU_pse),]
nrow(biological_status_percent_cut) # 200
biological_status_HBSR_cut <- biological_status_HBSRM[!grepl("(cyclic)",biological_status_HBSRM$CU_pse),]
nrow(biological_status_HBSR_cut) # 136

# Remove the CUs with high exploitation/low productivity in biological_status_percent_cut:
# BUT only if status_percent075 != "red".
# If status_percent075 is NA then remove.
CUsToRemove <- highExploit_lowProd$CU_name[highExploit_lowProd$toRemove]
biological_status_percent_cut <- biological_status_percent_cut[!biological_status_percent_cut$CU_pse %in% CUsToRemove,]
nrow(biological_status_percent_cut) # 193

#
# ) TODELETE Figures biological status based on HBSRM comparison Smsy vs. 80% Smsy ------
#

biological_status_compare_fun(biological_status_df = biological_status_HBSR_cut,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "region")

biological_status_compare_fun(biological_status_df = biological_status_HBSRM,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "species")

#
# ) TODELETE Figures comparison biological status HS abundance percentiles 0.75 vs. 0.50 -----

biological_status_compare_fun(biological_status_df = biological_status_percent_cut,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "region")

biological_status_compare_fun(biological_status_df = biological_status_percent_cut,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "species")

#
# ) TODELETE Figure that compares biological status between HBSR and HS percentiles approach -----
#

bioStatus_HBSR <- biological_status_HBSR_cut
nrow(bioStatus_HBSR) # 136

bioStatus_percent <- biological_status_percent_cut
nrow(bioStatus_percent) # 193

# merge the two datasets:
colToKeep_percent <- c("region","species","CU_pse","status_percent075")
colToKeep_HBSR <- c("region","species","CU_pse","status_Smsy")

bioStatus_merged <- merge(x = bioStatus_HBSR[,colToKeep_HBSR],
                          y = bioStatus_percent[,colToKeep_percent], 
                          by = c("region","species","CU_pse"),
                          all = T)

nrow(bioStatus_merged) # 195
nrow(unique(bioStatus_merged[,c("region","species","CU_pse")]))

# count how many CUs have the same biostatus with both approaches
bioStatus_merged_noNA <- bioStatus_merged[!is.na(bioStatus_merged$status_Smsy) &
                                            !is.na(bioStatus_merged$status_percent075),]
nrow(bioStatus_merged_noNA) # 106

# Same status:
bioStatus_merged_same <- bioStatus_merged_noNA[bioStatus_merged_noNA$status_Smsy == bioStatus_merged_noNA$status_percent075,]
nrow(bioStatus_merged_same) # 53 ; was 49
countHere <- table(factor(bioStatus_merged_same$status_Smsy,levels = c("red","amber","green")))
table_n <- data.frame(bioStatus = c("red","amber","green"),
                      n_same = as.numeric(countHere))

# Status only values for HBSR:
bioStatus_merged_HBSRM_only <- bioStatus_merged[!is.na(bioStatus_merged$status_Smsy) & 
                                                 is.na(bioStatus_merged$status_percent075),]
nrow(bioStatus_merged_HBSRM_only) # 1
countHere <- table(factor(bioStatus_merged_HBSRM_only$status_Smsy,levels = c("red","amber","green")))
table_n$HBSR_only <- as.numeric(countHere)

# Status only values for HS benchmarks:
bioStatus_merged_percentile_only <- bioStatus_merged[is.na(bioStatus_merged$status_Smsy) & 
                                                    !is.na(bioStatus_merged$status_percent075),]
nrow(bioStatus_merged_percentile_only) # 44
countHere <- table(factor(bioStatus_merged_percentile_only$status_percent075,levels = c("red","amber","green")))
table_n$HSBench_only <- as.numeric(countHere)

# Different status:
bioStatus_merged_diff <- bioStatus_merged_noNA[bioStatus_merged_noNA$status_Smsy != bioStatus_merged_noNA$status_percent075,]
nrow(bioStatus_merged_diff) # 53 ; was 57
countHere <- table(factor(bioStatus_merged_diff$status_Smsy,levels = c("red","amber","green")))
table_n$diff <- as.numeric(countHere)

table_m <- as.matrix(table_n[,c("n_same","HBSR_only","HSBench_only","diff")])
rownames(table_m) <- table_n$bioStatus

coloursStatus <- rev(c(g = "#8EB687", a = "#DFD98D", r = "#9A3F3F"))

if(printFig){
  jpeg(paste0(wd_figures,"/comparison_bioStatus_HBSR_Percentiles.jpg"), 
       width = 20,height = 15, units = "cm", res = 300)
}
barplot(height = table_m, col = coloursStatus, 
        ylim = c(0,max(apply(table_m,2,sum)) + 5), las = 1,
        ylab = "Number of CUs",xlab = "Biological status difference",
        main = "Bio-status differences with HBSR vs. percentiles methods",
        names.arg = c('Same','HBSR only','percentiles only','Different'))
#polygon(x = c(1,2,2,1),y = c(20,20,60,60))
offset <- .2
barLarger <- 1
x0 <- offset*4 + (ncol(table_m)-1)*barLarger + barLarger/2
x1 <- x0 + barLarger/2
# create a polygons for each possible combination of biostatus
height <- 0
for(bs in c("red","amber","green")){
  # bs <- c("red","amber","green")[2]
  bioStatus_merged_diff_cut <- bioStatus_merged_diff[bioStatus_merged_diff$status_Smsy == bs,]
  if(nrow(bioStatus_merged_diff) > 0){
    bioStatusHSPercentHere <- unique(bioStatus_merged_diff_cut$status_percent075)
    for(bshsp in bioStatusHSPercentHere){
      # bshsp <- bioStatusHSPercentHere[]
      colourHere <- coloursStatus[bshsp == c("red","amber","green")]
      bioStatus_merged_diff_cut2 <- bioStatus_merged_diff_cut[bioStatus_merged_diff_cut$status_percent075 == bshsp,]
      heightUP <- nrow(bioStatus_merged_diff_cut2) + height
      polygon(x = c(x0,x1,x1,x0),y = c(height,height,heightUP,heightUP),col = colourHere)
      height <- heightUP
    }
  }
}
# add the correspondging method for the lab bar
text(labels = "HBSR",x = x0 - barLarger/4, y = height, pos = 3, cex = .8)
text(labels = "percentiles",x = x0 + barLarger/4, y = height, pos = 3, cex = .8)
#
if(printFig){
  dev.off()
}

# big contrasts:
bioStatus_merged_diff[bioStatus_merged_diff$status_Smsy == "red" & 
                        bioStatus_merged_diff$status_percent075 == "green",]
# Haida Gwaii      PK West Haida Gwaii (even)          green                 red
#      Skeena      SX                Stephens          green                 red

bioStatus_merged_diff[bioStatus_merged_diff$status_Smsy == "red" & bioStatus_merged_diff$status_percent075 == "green",]

bioStatus_merged_HBSRM_only

#

# ) TO DELETE Create summary dataset of CUs that had a change in status for each method ------
#' - with fields for cuid, CU name, species, region, and benchmark_type applied
#' - (SR, if no SR then percentile)

# Remove CUs with no benchmark estimations and retain only the ones with contrasting status
biological_status_HBSR_cut_noNA <- biological_status_HBSR_cut[!is.na(biological_status_HBSR_cut$status_Smsy),]
biological_status_percent_cut_noNA <- biological_status_percent_cut[!is.na(biological_status_percent_cut$status_percent075),]

biological_status_HBSR_diff <- biological_status_HBSR_cut_noNA[biological_status_HBSR_cut_noNA$status_Smsy != biological_status_HBSR_cut_noNA$status_Smsy80,]
nrow(biological_status_HBSR_diff) # 14
biological_status_percent_diff <- biological_status_percent_cut_noNA[biological_status_percent_cut_noNA$status_percent075 != biological_status_percent_cut_noNA$status_percent05,]
nrow(biological_status_percent_diff) # 29

#' Remove the CUs in biological_status_percent_diff that are in biological_status_HBSR_diff
#' because HBSRM biostatus has priority
toRemove <- biological_status_percent_diff$cuid %in% biological_status_HBSR_diff$cuid
biological_status_percent_diff$cuid[toRemove]

biological_status_percent_diff <- biological_status_percent_diff[!toRemove,]
nrow(biological_status_percent_diff) # 28

# Merge biological_status_HBSR_diff and biological_status_percent_diff
biological_status_HBSR_diff$benchmark_type <- "HBSR"
biological_status_percent_diff$benchmark_type <- "percentiles"
biological_status_HBSR_diff$biostatus_new <- biological_status_HBSR_diff$status_Smsy
biological_status_HBSR_diff$biostatus_old <- biological_status_HBSR_diff$status_Smsy80
biological_status_percent_diff$biostatus_new <- biological_status_percent_diff$status_percent075
biological_status_percent_diff$biostatus_old <- biological_status_percent_diff$status_percent05

colInCommon <- c("region","species","cuid","CU_pse","biostatus_new","biostatus_old","benchmark_type")

biological_status_merge_diff <- merge(x = biological_status_HBSR_diff[,colInCommon],
                                      y = biological_status_percent_diff[,colInCommon],
                                      by = c("region","species","cuid","CU_pse"),
                                      all = T)

nrow(biological_status_merge_diff) # 42

biological_status_merge_diff$biostatus_new <- sapply(X = 1:nrow(biological_status_merge_diff),
                                                     FUN = function(r){
                                                       # r <- 1
                                                       out <- biological_status_merge_diff[r,c("biostatus_new.x","biostatus_new.y")]
                                                       out <- out[!is.na(out)]
                                                       return(out)
                                                     })
biological_status_merge_diff$biostatus_old <- sapply(X = 1:nrow(biological_status_merge_diff),
                                                     FUN = function(r){
                                                       # r <- 1
                                                       out <- biological_status_merge_diff[r,c("biostatus_old.x","biostatus_old.y")]
                                                       out <- out[!is.na(out)]
                                                       return(out)
                                                     })

biological_status_merge_diff$benchmark_type <- sapply(X = 1:nrow(biological_status_merge_diff),
                                                      FUN = function(r){
                                                        # r <- 1
                                                        out <- biological_status_merge_diff[r,c("benchmark_type.x","benchmark_type.y")]
                                                        out <- out[!is.na(out)]
                                                        return(out)
                                                      })

colToRemove <- c("biostatus_new.x","biostatus_new.y",
                 "biostatus_old.x","biostatus_old.y",
                 "benchmark_type.x","benchmark_type.y")
colToKeep <- colnames(biological_status_merge_diff)[! colnames(biological_status_merge_diff) %in% colToRemove]
biological_status_merge_diff <- biological_status_merge_diff[,colToKeep]

# write CSV:
# write.csv(biological_status_merge_diff,
#           paste0(wd_output,"/Biological_status_diff_SMsySmsy80_percent05075.csv"),
#           row.names = F)


#


biological_status_HBSRM <- rbind_biologicalStatusCSV_fun(pattern = "biological_status_percentiles",
                                                         wd_output = wd_output,
                                                         region = "Transboundary",
                                                         species_all = species_all)

rbind_biologicalStatusCSV_fun(pattern = "biological_status_percentiles",
                              wd_output = wd_output,
                              region = "Transboundary",
                              species_all = species_all)

rbind_biologicalStatusCSV_fun(pattern = "benchmarks_summary_percentiles",
                              wd_output = wd_output,
                              region = "Transboundary",
                              species_all = species_all)
