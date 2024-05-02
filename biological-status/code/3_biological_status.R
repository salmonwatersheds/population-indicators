
#'******************************************************************************
#' The goal of the script is to combine the HBSRM and percentiles biostatus and 
#' define the corresponding psf_status_code and psf_status fields.
#' 
#' Files imported:
#' - REGION_SPECIES_biological_status_HBSRM.csv (created in 2a_benchmarks_HBSRM.R)
#' - REGION_SPECIES_biological_status_percentiles.csv (created in 1b_benchmarks_percentiles.R)
#' - conservationunits_decoder.csv (from database)
#' - dataset390_output.csv (from database)
#' 
#' Files produced: 
#' - output/Biological_status_HBSR_Percentile_all.csv    # should become dataset_101
#' - 
#' - data/code_PSF_Status.csv
#' - population-indicators/data-input/CUs_highExploitation_lowProductivity.csv
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

printFig <- F

#
# 1) Import Datasets -----

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

#'* Import biostatus obtained with HBSR Sgen - Smsy: *
pattern <- "biological_status_HBSRM"
biological_status_HBSRM <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)
head(biological_status_HBSRM)
colnames(biological_status_HBSRM)
nrow(biological_status_HBSRM) # 137
unique(biological_status_HBSRM$comment)

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
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)

nrow(biological_status_percentile) # 448

# add final biostatus for both thresholds (i.e., 0.75 and 0.5 upper threshold)
colProb <- colnames(biological_status_percentile)[grepl("_075_",colnames(biological_status_percentile))]
biological_status_percentile$status_percent075 <- sapply(X = 1:nrow(biological_status_percentile), 
                                           FUN = function(r){
                                             # r <- 1
                                             slice <- biological_status_percentile[r,colProb]
                                             # out <- c("red","amber","green")[slice == max(slice)][1]
                                             out <- c("poor","fair","good")[slice == max(slice)][1] # Katy's request
                                             return(out)
                                           })

colProb <- colnames(biological_status_percentile)[grepl("_05_",colnames(biological_status_percentile))]
biological_status_percentile$status_percent05 <- sapply(X = 1:nrow(biological_status_percentile), 
                                                     FUN = function(r){
                                                       # r <- 1
                                                       slice <- biological_status_percentile[r,colProb]
                                                       # out <- c("red","amber","green")[slice == max(slice)][1]
                                                       out <- c("poor","fair","good")[slice == max(slice)][1] # Katy's request
                                                       return(out)
                                                     })


#'* Import benchmark values for the HBSRM method *
pattern <- "benchmarks_summary_HBSRM"
benchmarks_HBSRM <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                  wd_output = wd_output,
                                                  region = region,
                                                  species_all = species_all)


#'* Import benchmark values for the percentile method *
pattern <- "benchmarks_summary_percentiles"
benchmarks_percentile <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                       wd_output = wd_output,
                                                       region = region,
                                                       species_all = species_all)


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

#'* Import dataset390_output for survey_quality *
dataset390_output <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[18],
                                    fromDatabase = fromDatabase,
                                    update_file_csv = update_file_csv,
                                    wd = wd_pop_indic_data_input_dropbox)
head(dataset390_output)
dataset390_output <- dataset390_output[,c("region","species_name","cuid","cu_name_pse","catch_method")]
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

#
# 2) Create complete dataset with biostatus and psf_staus_code -----
# https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1701464111241899?thread_ts=1701199596.229739&cid=CJG0SHWCW

#' TODO: add the extra rule 2) in update chart: "is there a spawner-recruit relationship
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

# According to the last PSAC meeting:
# colHBSR <- c("status_Smsy_red","status_Smsy_amber","status_Smsy_green",
#              "status_Smsy")
colHBSR <- c("status_Smsy80_red","status_Smsy80_amber","status_Smsy80_green",
             "status_Smsy80")

# colPercent <- c("status_percent_075_red","status_percent_075_amber","status_percent_075_green",
#                 "status_percent075")
colPercent <- c("status_percent_05_red","status_percent_05_amber","status_percent_05_green",
                "status_percent05")

biological_status_merged <- merge(x = biological_status_HBSRM[,c(colCommon,colHBSR)],
                                  y = biological_status_percentile[,c(colCommon,colPercent)],
                                  by =  c("region","species","cuid","CU_pse","current_spawner_abundance"), 
                                  all = T)
head(biological_status_merged)

# Check if number of CUs is correct:
CUs_comm <- biological_status_HBSRM$cuid[biological_status_HBSRM$cuid %in% 
                                          biological_status_percentile$cuid]
length(CUs_comm) # 137
CUs_HBSRM_only <- biological_status_HBSRM$cuid[!biological_status_HBSRM$cuid %in% 
                                               biological_status_percentile$cuid]
length(CUs_HBSRM_only) # 0
CUs_Percent_only <- biological_status_percentile$cuid[!biological_status_percentile$cuid %in% 
                                                       biological_status_HBSRM$cuid]
length(CUs_Percent_only) # 311

# Expected number of rows in biological_status_merged:
length(CUs_comm) + length(CUs_HBSRM_only) + length(CUs_Percent_only) # 448
nrow(biological_status_merged) # 448 --> ALL GOOD

# Renames columns
colnames(biological_status_merged) <- gsub("red","red_prob",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("amber","yellow_prob",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("green","green_prob",colnames(biological_status_merged))

# colnames(biological_status_merged) <- gsub("status_Smsy_","sr_",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_Smsy80_","sr_",colnames(biological_status_merged))
# colnames(biological_status_merged) <- gsub("status_percent_075_","percentile_",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_percent_05_","percentile_",colnames(biological_status_merged))

# colnames(biological_status_merged) <- gsub("status_Smsy","sr_status",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_Smsy80","sr_status",colnames(biological_status_merged))
# colnames(biological_status_merged) <- gsub("status_percent075","percentile_status",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_percent05","percentile_status",colnames(biological_status_merged))

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
  cond <- dataset390_output$cuid == biological_status_merged$cuid[r]
  if(is.na(dataset390_output$catch_method[cond])){  # QUESTION: is that normal? Why is it NA?  https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1713912766189429
    cond_HBRSM_2 <- T # F
    # break
  }else{
    #' Steph: "data_quality scores that are NA or zero should not prevent HBSR
    #'status form being shown. Only low (1) or medium-low (2) catch_quality should
    #' be part of the decision rule"
    #' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1713982063020029?thread_ts=1713912766.189429&cid=CJ5RVHVCG
    cond_HBRSM_2 <- dataset390_output$catch_method[cond] != 1
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


#' * Add field  hist_COLOUR * 
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


#'* Add field psf_status_type *
#' Katy's request. Either "sr", "percentile" or NA.
biological_status_merged$psf_status_type <- NA

biological_status_merged$psf_status_type <- apply(X = biological_status_merged, 
                                                  MARGIN = 1, 
                                                  FUN = function(r){
                                                    out <- NA
                                                    if(r["psf_status"] %in% c("poor","fair","good")){
                                                      if(is.na(r["sr_status"])){
                                                        out <- "percentile"
                                                      }else{
                                                        out <- "sr"
                                                      }
                                                    }
                                                    return(out)
                                                  })



#'* Drop necessary columns *
colToDrop <- c("psf_status_code.x","psf_status_code.y")
biological_status_merged <- biological_status_merged[,!colnames(biological_status_merged) %in% colToDrop]

#'* rename CU_pse *
colnames(biological_status_merged)[colnames(biological_status_merged) == "CU_pse"] <- "cu_name_pse"

#'* add species_name and species_abbr and remove species *
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
                                                        col_prob,
                                                        c("hist_red","hist_yellow","hist_green"),
                                                        col_status)]
head(biological_status_merged)

# check if the psf code of cyclic communities: should be 5
biological_status_merged[grepl("cyclic",biological_status_merged$cu_name_pse),]
biological_status_merged[grepl("5",biological_status_merged$psf_status_code_all),]

# Number CUs total:
nrow(biological_status_merged) # 448

# number CUs with biostatus assessed over both methods
condition <- biological_status_merged$psf_status_code %in% 1:3
sum(condition) # 184

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
nrow(biological_status_merged[condition_1_2_3 & condition_HBSRM,]) # 136

# number CUs with biostatus assessed with percentile method: 
condition_Percent <- !is.na(biological_status_merged$percentile_status)
nrow(biological_status_merged[condition_1_2_3 & condition_Percent,]) # 184
nrow(biological_status_merged[condition_1_2_3 & condition_Percent & !condition_HBSRM,]) # 48

# Write the file Biological_status_HBSR_Percentile_all.csv (future dataset_101)-----
#

write.csv(biological_status_merged,paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),
          row.names = F)

biological_status_merged <- read.csv(paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),
                                     header = T)

#
# Create benchmarks_HBSR_Percentile_all.csv (part of dataset_102) --------

# Import Biological_status_HBSR_Percentile_all.csv create just before
biological_status_merged <- read.csv(paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),
                                     header = T)

benchmarks_merged <- biological_status_merged[,c("region","cuid","species_abbr",
                                                 "species_name","cu_name_pse")]

benchmarks_merged$curr_spw <- NA       # current spawner abundance
benchmarks_merged$sgen <- NA           # lower HBSRM benchmark
benchmarks_merged$sgen_lower <- NA     #
benchmarks_merged$sgen_upper <- NA     #
benchmarks_merged$smsy <- NA           # upper HBSRM benchmark
benchmarks_merged$smsy_lower <- NA     #
benchmarks_merged$smsy_upper <- NA     #
benchmarks_merged$`25%_spw` <- NA      # lower percentile benchmark
benchmarks_merged$`25%_spw_lower` <- NA
benchmarks_merged$`25%_spw_upper` <- NA
benchmarks_merged$`75%_spw` <- NA      # upper percentile benchmark
benchmarks_merged$`75%_spw_lower` <- NA
benchmarks_merged$`75%_spw_upper` <- NA
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
    
    benchmarks_merged$sgen[r] <- benchmarks_HBSRM$m[cond_cuid & cond_method & cond_sgen]
    benchmarks_merged$sgen_lower[r] <- benchmarks_HBSRM$CI025[cond_cuid & cond_method & cond_sgen]
    benchmarks_merged$sgen_upper[r] <- benchmarks_HBSRM$CI975[cond_cuid & cond_method & cond_sgen]

    # QUESTION: how to deal with the CI for 80% Smsy:
    # https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1714495136554939?thread_ts=1701199596.229739&cid=CJG0SHWCW
    
    benchmarks_merged$smsy[r] <- benchmarks_HBSRM$m[cond_cuid & cond_method & cond_Smsy] * 0.8
    benchmarks_merged$smsy_lower[r] <- benchmarks_HBSRM$CI025[cond_cuid & cond_method & cond_Smsy] * 0.8
    benchmarks_merged$smsy_upper[r] <- benchmarks_HBSRM$CI975[cond_cuid & cond_method & cond_Smsy] * 0.8
  }
  
  # get the benchmarks for the percentile method:
  cond_cuid <- benchmarks_percentile$cuid == cuid
  cond_025 <- benchmarks_percentile$benchmark == "benchmark_0.25"
  # cond_075 <- benchmarks_percentile$benchmark == "benchmark_0.75"
  cond_05 <- benchmarks_percentile$benchmark == "benchmark_0.5"

  benchmarks_merged$`25%_spw`[r] <- benchmarks_percentile$m[cond_cuid & cond_025]
  benchmarks_merged$`25%_spw_lower`[r] <- benchmarks_percentile$CI025[cond_cuid & cond_025]
  benchmarks_merged$`25%_spw_upper`[r] <- benchmarks_percentile$CI975[cond_cuid & cond_025]
    
  # benchmarks_merged$`75%_spw`[r] <- benchmarks_percentile$m[cond_cuid & cond_075]
  # benchmarks_merged$`75%_spw_lower`[r] <- benchmarks_percentile$CI025[cond_cuid & cond_075]
  # benchmarks_merged$`75%_spw_upper`[r] <- benchmarks_percentile$CI975[cond_cuid & cond_075]
  
  benchmarks_merged$`75%_spw`[r] <- benchmarks_percentile$m[cond_cuid & cond_05]
  benchmarks_merged$`75%_spw_lower`[r] <- benchmarks_percentile$CI025[cond_cuid & cond_05]
  benchmarks_merged$`75%_spw_upper`[r] <- benchmarks_percentile$CI975[cond_cuid & cond_05]
}

#
# write Benchmarks_HBSR_Percentile_all.csv -----

write.csv(benchmarks_merged,paste0(wd_output,"/Benchmarks_HBSR_Percentile_all.csv"),
          row.names = F)

benchmarks_merged <- read.csv(paste0(wd_output,"/Benchmarks_HBSR_Percentile_all.csv"),
                                     header = T)

cond <- benchmarks_merged$cuid %in% c(753,756,757)
benchmarks_merged[cond,]












