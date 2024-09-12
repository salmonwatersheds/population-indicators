
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
#' - Biological_status_HBSR_Percentile_all.csv    # should become dataset_101_output I think
#' - Benchmarks_HBSR_Percentile_all.csv           # 
#' - data/code_PSF_Status.csv
#' - population-indicators/data-input/CUs_highExploitation_lowProductivity.csv
#' - output/dataset101_biological_status.csv #
#' - output/dataset102_benchmarks.csv        # 
#' - output/archive/dataset101_biological_status_YYYY-MM-DD.csv #
#' - output/archive/dataset102_benchmarks_YYYY-MM-DD.csv        # 
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

#
# Import Datasets -----

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

#'* Import benchmark values for the HBSRM method *
pattern <- "benchmarks_summary_HBSRM"
benchmarks_HBSRM <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                  wd_output = paste0(wd_output,"/intermediate"),
                                                  region = region,
                                                  species_all = species_all)
#' remove the cyclic  CUs for now because their HBSRM analysis has not been incorporated in the workflow yet
cond <- grepl("cyclic",benchmarks_HBSRM$CU)
benchmarks_HBSRM[cond,]
benchmarks_HBSRM <- benchmarks_HBSRM[!cond,]

#'* Import benchmark values for the percentile method *
pattern <- "benchmarks_summary_percentiles"
benchmarks_percentile <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                       wd_output = paste0(wd_output,"/intermediate"),
                                                       region = region,
                                                       species_all = species_all)

#'* Import biostatus obtained with HBSR Sgen - Smsy: *
pattern <- "biological_status_HBSRM"
biological_status_HBSRM <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                         wd_output = paste0(wd_output,"/intermediate"),
                                                         region = region,
                                                         species_all = species_all)
head(biological_status_HBSRM)
colnames(biological_status_HBSRM)
nrow(biological_status_HBSRM) # 144 143 ;  137

unique(biological_status_HBSRM$comment)


#' remove the cyclic  CUs for now because their HBSRM analysis has not been incomporated in the workflow yet
cond <- grepl("cyclic",biological_status_HBSRM$CU_pse)
biological_status_HBSRM[cond,]
biological_status_HBSRM <- biological_status_HBSRM[!cond,]


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
                                                      species_all = species_all)

nrow(biological_status_percentile) # 451 452 448

# Add final biostatus for both thresholds (i.e., 0.75 and 0.5 upper threshold)
#' NOTE: the percentile biostatus is obtain used the benchmarks are current spawner
#' abundance and not the probabilities like for HBSRM (hence the code commented out).
cond_025 <- benchmarks_percentile$benchmark == "benchmark_0.25"
cond_05 <- benchmarks_percentile$benchmark == "benchmark_0.5"
cond_075 <- benchmarks_percentile$benchmark == "benchmark_0.75"
colProb <- colnames(biological_status_percentile)[grepl("_075_",colnames(biological_status_percentile))]
biological_status_percentile$status_percent075 <- sapply(X = 1:nrow(biological_status_percentile), 
                                           FUN = function(r){
                                             # r <- 1
                                             # slice <- biological_status_percentile[r,colProb]
                                             # out <- c("poor","fair","good")[slice == max(slice)][1] # Katy's request
                                             cuid <- biological_status_percentile[r,"cuid"]
                                             csa <- biological_status_percentile[r,"current_spawner_abundance"]
                                             
                                             if(is.na(csa)){
                                               out <- NA
                                             }else{
                                               cond_cuid <- benchmarks_percentile$cuid == cuid
                                               bench_025 <- benchmarks_percentile$m[cond_cuid & cond_025]
                                               bench_075 <- benchmarks_percentile$m[cond_cuid & cond_075]
                                               if(csa <= bench_025){
                                                 out <- "poor"
                                               }else if(csa <= bench_075){
                                                 out <- "fair"
                                               }else{
                                                 out <- "good"
                                               }
                                             }
                                             return(out)
                                           })

colProb <- colnames(biological_status_percentile)[grepl("_05_",colnames(biological_status_percentile))]
biological_status_percentile$status_percent05 <- sapply(X = 1:nrow(biological_status_percentile), 
                                                     FUN = function(r){
                                                       # r <- 1
                                                       # slice <- biological_status_percentile[r,colProb]
                                                       # out <- c("poor","fair","good")[slice == max(slice)][1] # Katy's request
                                                       cuid <- biological_status_percentile[r,"cuid"]
                                                       csa <- biological_status_percentile[r,"current_spawner_abundance"]
                                                       
                                                       if(is.na(csa)){
                                                         out <- NA
                                                       }else{
                                                         cond_cuid <- benchmarks_percentile$cuid == cuid
                                                         bench_025 <- benchmarks_percentile$m[cond_cuid & cond_025]
                                                         bench_05 <- benchmarks_percentile$m[cond_cuid & cond_05]
                                                         if(csa <= bench_025){
                                                           out <- "poor"
                                                         }else if(csa <= bench_05){
                                                           out <- "fair"
                                                         }else{
                                                           out <- "good"
                                                         }
                                                       }
                                                       return(out)
                                                     })


#'* Import the conservationunits_decoder.csv *
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- F

conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

nrow(unique(conservationunits_decoder[,c("region","species_name","cu_name_pse")])) # 465 466

#'* Import dataset390_data_quality (dataset390_output) for survey_quality *
dataset390_output <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[18],
                                    fromDatabase = fromDatabase,
                                    update_file_csv = update_file_csv,
                                    wd = wd_pop_indic_data_input_dropbox)
dataset390_output <- dataset390_output[,c("region","species_name","cuid","cu_name_pse","catch_method")]

# TEMPORARY (11/09/2024)
dataset390_output <- import_mostRecent_file_fun(wd = wd_pop_indic_data_input_dropbox,
                                                pattern = "dataset390_data_quality")
dataset390_output <- dataset390_output[,c("region","species_name","cuid","cu_name_pse","catch_quality")]
colnames(dataset390_output)[colnames(dataset390_output) == "catch_quality"] <- "catch_method"
head(dataset390_output)
nrow(dataset390_output) # 465 466


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

code_PSF_Status <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[20],
                                         fromDatabase = fromDatabase,
                                         update_file_csv = update_file_csv,
                                         wd = wd_pop_indic_data_input_dropbox)


#'* Import list of CUs with high exploitation and/or low productivity *
# Return list of CUs that have high exploitation rate or low production rates,
# as well as a final call on keeping or removing the CUs depending of their
# biostatus: the one with already a red/poor status are kept (i.e. Clare's 8th rule).
highExploit_lowProd <- cu_highExploit_lowProd_fun(biological_status_percentile = biological_status_percentile,
                                                  wd_output = wd_output, 
                                                  conservationunits_decoder = conservationunits_decoder)

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
                             wd = wd_pop_indic_data_input_dropbox)

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

# Combine the two datasets:
colCommon <- c("region","species","cuid","CU_pse","current_spawner_abundance",
               "psf_status_code")
              # "year_last","year_first","genLength")

#' NOTE:
#' According to the last PSAC meeting:, we should use the 80% Smsy (vs 100% ) and 
#' 50% percentile (vs. 75%) for the upper benchmarks.

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
length(CUs_Percent_only) # 315 311

# Expected number of rows in biological_status_merged:
length(CUs_comm) + length(CUs_HBSRM_only) + length(CUs_Percent_only) # 452 448
nrow(biological_status_merged) # 452 --> ALL GOOD


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

#'* Create psf_status_code_all fields & psf_status_type *
#' psf_status_code_all: attribute 1 (good), 2 (fair) or 3 (poor) for now
#' psf_status_type:  Katy's request. Either "sr", "percentile" or NA.
biological_status_merged$psf_status_code_all <- NA # values: 1 to 9
biological_status_merged$psf_status_type <- NA

col_prob <- colnames(biological_status_merged)[grepl("_prob",colnames(biological_status_merged))]
col_sr_prob <- col_prob[grepl("sr_",col_prob)]
col_percent_prob <- col_prob[grepl("percentile_",col_prob)]

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
length(unique(check$cuid)) # 5

#
for(r in 1:nrow(biological_status_merged)){
  # r <- 170
  # r <- which(biological_status_merged$cuid == 516)
  bs_here <- biological_status_merged[r,]
  
  # *** HBSRM method ***
  
  #' Condition 1: Are the probabilities available and the status_code is NA 
  #' (i.e. not 4, ...9)
  cond_HBRSM <- !is.na(bs_here$sr_red_prob) & is.na(bs_here$psf_status_code.x) # is.na(bs_here$psf_status_code.x) might not be necessary but does not hurt
  
  #' Condition 2: Is there a SR relationship with CU-level catch estimates >= medium-low
  # survey_quality 
  # In data-quality.R: 
  # "Low" ~ 1
  # "Medium-Low" ~ 2
  # "Medium" ~ 3
  # "Medium-High" ~ 4
  # "High" ~ 5
  cond <- dataset390_output$cuid == biological_status_merged$cuid[r]
  #' Steph: "data_quality scores that are NA or zero should not prevent HBSR
  #' status form being shown. Only low (1) or medium-low (2) catch_quality should
  #' be part of the decision rule"
  #' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1713982063020029?thread_ts=1713912766.189429&cid=CJ5RVHVCG
  # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1713912766189429
  cond_HBRSM_2 <- is.na(dataset390_output$catch_method[cond]) | dataset390_output$catch_method[cond] != 1
  

  #' Condition 3: Sgen < Smsy
  cond_cuid <- benchmarks_HBSRM$cuid == biological_status_merged$cuid[r]
  if(any(cond_cuid)){
    Sgen <- benchmarks_HBSRM$m[cond_cuid & cond_HPD & cond_Sgen]
    Smsy <- benchmarks_HBSRM$m[cond_cuid & cond_HPD & cond_Smsy]
    if(Sgen < Smsy){
      cond_HBRSM_3 <- T
    }else{
      cond_HBRSM_3 <- F
    }
  }else{
    cond_HBRSM_3 <- F
  }
  
  if(cond_HBRSM & cond_HBRSM_2 & cond_HBRSM_3){
    
    # psf_status_here <- c("poor","fair","good")[bs_here[,col_sr_prob] == max(bs_here[,col_sr_prob])]
    # psf_status_code_all_here <- c(3:1)[psf_status_here == c("poor","fair","good")]
    psf_status_code_all_here <- c(3:1)[bs_here[,col_sr_prob] == max(bs_here[,col_sr_prob])]
    psf_status_type_here <- "sr"
    
    
  # *** Percentile method ***
    
  }else if(!is.na(bs_here$percentile_red_prob) & 
           (is.na(bs_here$psf_status_code.y) | grepl("3, 6",bs_here$psf_status_code.y))){
    #' grepl("3, 6",bs_here$psf_status_code.y) is for the CUs with high exploitation 
    #' / low productivity with poor status.

    # NOT USED: using the probabilities to determine the status
    #psf_status_code_all_here <- c(3:1)[bs_here[,col_percent_prob] == max(bs_here[,col_percent_prob])]
    
    # Get the percentile benchmark values:
    cond_cuid <- benchmarks_percentile$cuid == biological_status_merged$cuid[r]
    cond <- benchmarks_percentile$benchmark == "benchmark_0.25"
    bench_low <- benchmarks_percentile[cond_cuid & cond,"m"]
    cond <- benchmarks_percentile$benchmark == "benchmark_0.5"
    bench_up <- benchmarks_percentile[cond_cuid & cond,"m"]

    # compare to current spawner abundance
    csa <- biological_status_merged$current_spawner_abundance[r]
    if(csa <= bench_low){
      psf_status_code_all_here <- 3
    }else if(csa <= bench_up){
      psf_status_code_all_here <- 2
    }else{
      psf_status_code_all_here <- 1
    }
    
    #
    psf_status_type_here <- "percentile"
    
    # if(psf_status_code_all_here != psf_status_code_all_here_2){
    #   print(paste("Difference:",biological_status_merged$cuid[r]))
    # }
  
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
# data-deficient        extinct           fair           good   not-assessed           poor 
#            223              5             51             78             27             67 


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

# Check that the CUs with status code 6 (high exploitation rate or production rates)
# with red status are still available: --> is not any
cond <- grepl("6",biological_status_merged$psf_status_code_all) &
  biological_status_merged$percentile_status == 'poor' &
  !is.na(biological_status_merged$percentile_status)

biological_status_merged[cond,]
biological_status_merged$psf_status[cond] |> unique()

# Re-arrange columns and drop current_spawner_abundance
col_prob <- colnames(biological_status_merged)[grepl("_prob",colnames(biological_status_merged))]
col_status <- colnames(biological_status_merged)[grepl("_status",colnames(biological_status_merged))]

biological_status_merged <- biological_status_merged[,c("region","cuid","species_abbr",
                                                        "species_name","cu_name_pse",
                                                        col_prob,
                                                        c("hist_red","hist_yellow","hist_green"),
                                                        col_status)]
head(biological_status_merged)

# Check if the psf code of cyclic communities: should be 5
biological_status_merged[grepl("cyclic",biological_status_merged$cu_name_pse),]
biological_status_merged[grepl("5",biological_status_merged$psf_status_code_all),]

#
# Counts of the CUs with sr and percentile benchmarks, no status, etc. ---- 
#

# Number CUs total:
nrow(biological_status_merged) # 451 452 448

# Number CUs with biostatus assessed over both methods
condition <- biological_status_merged$psf_status_code %in% 1:3
sum(condition) # 196 195

# CUs not assessed because cyclic dynamics, low productivity/high mortality or data deficient
condition_5 <- grepl(pattern = "5",biological_status_merged$psf_status_code)
condition_6 <- grepl(pattern = "6",biological_status_merged$psf_status_code)
condition_7 <- grepl(pattern = "7",biological_status_merged$psf_status_code)
condition_5_6_7 <- condition_5 | condition_6 | condition_7
biological_status_merged$psf_status_code_all[condition_5_6_7]
sum(condition_5_6_7) # 38

# Number CUs with biostatus assessed with HBSRM:
condition_1_2_3 <- biological_status_merged$psf_status_code %in% 1:3
condition_HBSRM <- !is.na(biological_status_merged$psf_status_type) & 
  biological_status_merged$psf_status_type == "sr"
sum(condition_HBSRM)                   # 124 125
sum(condition_1_2_3 & condition_HBSRM) # 124 125

# Number CUs with biostatus assessed with percentile method: 
condition_Percent <- !is.na(biological_status_merged$psf_status_type) & 
  biological_status_merged$psf_status_type == "percentile"
sum(condition_Percent)                       # 72 71 69
sum(condition_1_2_3 & condition_Percent)     # 72 71 69
sum(condition_HBSRM) + sum(condition_Percent) == sum(condition) # should be TRUE

# Check: which CU went from not assessed to assessed with Percent:

# cuid_new <- biological_status_merged[condition_Percent,]$cuid
# 
# biological_status_old <- import_mostRecent_file_fun(wd = paste0(wd_output),
#                                                     pattern = "dataset101_biological_status")
# condition_Percent <- !is.na(biological_status_old$psf_status_type) & 
#   biological_status_old$psf_status_type == "percentile"
# cuid_old <- biological_status_old$cuid[condition_Percent]
# 
# cuid_new[! cuid_new %in% cuid_old] # 510 511
# cuid_old[! cuid_old %in% cuid_new] # 521
# 
# 
#  for(cuid in c(510,511,521)){
#    print(cuid)
#    print("New biostatus:")
#    cond <- biological_status_merged$cuid == cuid
#    print(biological_status_merged[cond,c("region","species_name","cu_name_pse","cuid",
#                                    "psf_status_type","psf_status_code","psf_status")])
#    print("Old biostatus:")
#    cond <- biological_status_old$cuid == cuid
#    print(biological_status_old[cond,c("region","species_name","cu_name_pse","cuid",
#                                          "psf_status_type","psf_status_code","psf_status")])
#    print("   ***   ")
#  }


#'* Check *
# 
# # find CUs that are new to the new biological_status_merged
# bio_status_previous <- read.csv(paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),header = T)
# cond <- !bio_status_previous$cuid %in% biological_status_merged$cuid
# bio_status_previous[cond,c("region","species_abbr","cu_name_pse","cuid","psf_status_code")]
# cuid_new <- bio_status_previous$cuid[cond]
# 
# # find CUs that were SR before and not any more
# cond_sr <- !is.na(bio_status_previous$psf_status_type) & 
#   bio_status_previous$psf_status_type == "sr" &
#   bio_status_previous$psf_status_code %in% 1:3 &
#   !bio_status_previous$cuid %in% cuid_new
# cuid_prev <- bio_status_previous$cuid[cond_sr]
# cuid_prev
# 
# cond <- !cuid_prev %in% biological_status_merged$cuid[condition_HBSRM]
# cuid_prev[cond]
# biological_status_merged[biological_status_merged$cuid %in% cuid_prev[cond],]
# 
# bio_status_previous[bio_status_previous$cuid %in% cuid_prev[cond],]
# 
# # Question to Eric and Katy: why are those two CUs data deficient now? --> normal
# # https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1723584891946199
# #
# 
# # find CUs that were percentile before but not amymore --> none
# cond_sr <- !is.na(bio_status_previous$psf_status_type) & 
#   bio_status_previous$psf_status_type == "percentile" &
#   bio_status_previous$psf_status_code %in% 1:3 &
#   !bio_status_previous$cuid %in% cuid_new
# cuid_prev <- bio_status_previous$cuid[cond_sr]
# cuid_prev
# 
# cond <- !cuid_prev %in% biological_status_merged$cuid[condition_Percent]
# cuid_prev[cond]
# biological_status_merged[biological_status_merged$cuid %in% cuid_prev[cond],]
# 
# bio_status_previous[bio_status_previous$cuid %in% cuid_prev[cond],]
# 
# # CUs that is no SR and was not before
# cond_sr <- !is.na(bio_status_previous$psf_status_type) & 
#   bio_status_previous$psf_status_type == "sr" &
#   bio_status_previous$psf_status_code %in% 1:3
# cuid_prev <- bio_status_previous$cuid[!cond_sr & !bio_status_previous$cuid %in% cuid_new]
# 
# cond <- cuid_prev %in% biological_status_merged$cuid[condition_HBSRM]
# cuid_prev[cond]
# biological_status_merged[biological_status_merged$cuid %in% cuid_prev[cond],]
# bio_status_previous[bio_status_previous$cuid %in% cuid_prev[cond],]


# Write the file Biological_status_HBSR_Percentile_all.csv (future dataset_101) REMOVE -----
#
# date <- as.character(Sys.time())
# date <- strsplit(x = date, split = " ")[[1]][1]
# date <- gsub("-","",date)
# 
# write.csv(biological_status_merged,paste0(wd_output,"/Biological_status_HBSR_Percentile_all_",date,".csv"),
#           row.names = F)
# 
# biological_status_merged <- import_mostRecent_file_fun(wd = wd_output,
#                                                        pattern = "Biological_status_HBSR_Percentile_all_")

#
# Create benchmarks_HBSR_Percentile_all.csv (part of dataset_102) --------

# Import Biological_status_HBSR_Percentile_all.csv create just before
# biological_status_merged <- import_mostRecent_file_fun(wd = wd_output,
#                                                        pattern = "Biological_status_HBSR_Percentile_all_")

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
  
  # COMMENT:
  # This is ineeded the 50% percentile and not the 75, despite the name being "75%_spw"
  # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1707332952867199
  
  benchmarks_merged$`75%_spw`[r] <- benchmarks_percentile$m[cond_cuid & cond_05]
  benchmarks_merged$`75%_spw_lower`[r] <- benchmarks_percentile$CI025[cond_cuid & cond_05]
  benchmarks_merged$`75%_spw_upper`[r] <- benchmarks_percentile$CI975[cond_cuid & cond_05]
}

#
# write Benchmarks_HBSR_Percentile_all.csv REMOVE -----

# date <- as.character(Sys.time())
# date <- strsplit(x = date, split = " ")[[1]][1]
# date <- gsub("-","",date)
# 
# write.csv(benchmarks_merged,paste0(wd_output,"/Benchmarks_HBSR_Percentile_all_",date,".csv"),
#           row.names = F)
# 
# benchmarks_merged <-import_mostRecent_file_fun(wd = wd_output,
#                                                pattern = "Benchmarks_HBSR_Percentile_all_")
# 
# cond <- benchmarks_merged$cuid %in% c(753,756,757)
# benchmarks_merged[cond,]

#
# Add the missing CUs to the output files (one time fix) ------
#' There are 18 CUs that were excluded in the biostatus analysis because there is 
#' no spawner survey for them (not in dataset5_output (in the R code: recruitsperspawner)
#' nor in dataset1cu_output (in the R code: cuspawnerabundance)).
#' https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1714665184357159?thread_ts=1701199596.229739&cid=CJG0SHWCW

# biological_status_merged <- import_mostRecent_file_fun(wd = wd_output,
#                                                        pattern = "Biological_status_HBSR_Percentile_all_")
# 
# benchmarks_merged <- import_mostRecent_file_fun(wd_output,
#                                                 pattern = "Benchmarks_HBSR_Percentile_all_")

# find their cuid:
cond <- ! conservationunits_decoder$pooledcuid %in% biological_status_merged$cuid
cuidMissing <- conservationunits_decoder$cuid[cond]

cuidMissing
length(cuidMissing) # 14

#'* Add to biological_status_merged * 
head(biological_status_merged)

biological_status_add <- biological_status_merged[1:length(cuidMissing),]
biological_status_add$region <- conservationunits_decoder$region[cond]
biological_status_add$cuid <- conservationunits_decoder$cuid[cond]
biological_status_add$species_abbr <- conservationunits_decoder$species_abbr[cond]
biological_status_add$species_name <- conservationunits_decoder$species_name[cond]
biological_status_add$cu_name_pse <- conservationunits_decoder$cu_name_pse[cond]

cond_col <- ! colnames(biological_status_add) %in% c("region","cuid","species_abbr",
                                                     "species_name","cu_name_pse")
biological_status_add[,cond_col] <- NA

biological_status_add$psf_status <- "data-deficient"
biological_status_add$psf_status_code <- 8
biological_status_add$psf_status_code_all <- "8, 9" 

biological_status_merged <- rbind(biological_status_merged,
                                  biological_status_add)

#'* Add to benchmarks_merged *
head(benchmarks_merged)

benchmarks_add <- benchmarks_merged[1:length(cuidMissing),]
benchmarks_add$region <- conservationunits_decoder$region[cond]
benchmarks_add$cuid <- conservationunits_decoder$cuid[cond]
benchmarks_add$species_abbr <- conservationunits_decoder$species_abbr[cond]
benchmarks_add$species_name <- conservationunits_decoder$species_name[cond]
benchmarks_add$cu_name_pse <- conservationunits_decoder$cu_name_pse[cond]

cond_col <- ! colnames(benchmarks_add) %in% c("region","cuid","species_abbr",
                                              "species_name","cu_name_pse")

benchmarks_add[,cond_col] <- NA

benchmarks_merged <- rbind(benchmarks_merged,
                           benchmarks_add)


# date <- as.character(Sys.time())
# date <- strsplit(x = date, split = " ")[[1]][1]
# date <- gsub("-","",date)
# 
# 
# write.csv(benchmarks_merged,paste0(wd_output,"/Benchmarks_HBSR_Percentile_all_",date,".csv"),
#           row.names = F)
# 
# write.csv(biological_status_merged,paste0(wd_output,"/Biological_status_HBSR_Percentile_all_",date,".csv"),
#           row.names = F)
# 
# # END
# 
# benchmarks_merged <- import_mostRecent_file_fun(wd = wd_output,
#                                                 pattern = "Benchmarks_HBSR_Percentile_all_")
# 
# biological_status_merged <- import_mostRecent_file_fun(wd = wd_output,
#                                                        pattern = "Biological_status_HBSR_Percentile_all_")

cond <- biological_status_merged$psf_status_type == "percentile" & !is.na(biological_status_merged$psf_status_type)
cond2 <- !is.na(biological_status_merged$sr_yellow_prob)
# View(biological_status_merged[cond & cond2,])

#
# round fish count to closest integer in benchmarks_merged ------
#
head(benchmarks_merged)
head(biological_status_merged)

fields <- c("curr_spw",
            "sgen","sgen_lower","sgen_upper",
            "smsy","smsy_lower","smsy_upper",
            "25%_spw","25%_spw_lower","25%_spw_upper",
            "75%_spw","75%_spw_lower","75%_spw_upper")

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
cuid_extinct_pse[! cuid_extinct_pse %in% cuid_extinct_cosewic]     # 758 761
dataset380$Sheet1[dataset380$Sheet1$cuid %in% c(758,761),]



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





