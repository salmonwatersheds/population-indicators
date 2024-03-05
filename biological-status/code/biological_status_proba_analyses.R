
#'******************************************************************************
#' The goal of the script is to analyse the biological status probabilities 
#' obtained from the HBSRM analysis.
#' 
#' Files imported (from ):
#' - region_species_biological_status.csv (created in benchmarks_HBSRM.R)
#' 
#' Files produced: 
#' - Biological_status_HBSR_Percentile_all.csv (biological_status_merged)
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


#' Import the conservationunits_decoder.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
datasetsNames_database <- datasetsNames_database_fun()
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = F,
                                                   update_file_csv = F,
                                                   wd = wd_pop_indic_data_input_dropbox)

nrow(unique(conservationunits_decoder[,c("region","species_name","cu_name_pse")]))

printFig <- F

#
# 1) Import benchmarks values and biological status files for both methods -----
# Import all the CSV files for each combination of region - species and rbind them

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

nrow(biological_status_percentile) # 428

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

cond <- grepl("_Smsy_",colnames(biological_status_HBSRM)) |
  grepl("_Smsy80_",colnames(biological_status_HBSRM))
colRemove_HBSRM <- c(colnames(biological_status_HBSRM)[cond],"comment")

cond <- grepl("_HSPercent_",colnames(biological_status_percentile))
colRemove_Percent <- c(colnames(biological_status_percentile)[cond],
                       "dataPointNb","comment")

colComm <- c("region","species","cuid","CU","CU_pse","CU_dfo")
colComm <- unique(c(colnames(biological_status_HBSRM),
                    colnames(biological_status_percentile)))
colComm <- colComm[!colComm %in% c(colRemove_HBSRM,colRemove_Percent,
                                   "status_Smsy","status_Smsy80",
                                   "status_percent075","status_percent05")]

biological_status_all <- merge(x = biological_status_HBSRM[,!colnames(biological_status_HBSRM) %in% colRemove_HBSRM], 
                               y = biological_status_percentile[,!colnames(biological_status_percentile) %in% colRemove_Percent], 
                               by = colComm, 
                               all = T)
#

# 2) Create complete dataset with all the CUs and their biostatus and psf_staus_code -----
# https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1701464111241899?thread_ts=1701199596.229739&cid=CJG0SHWCW

# Field to include:
# percentile_red_prob
# percentile_yellow_prob
# percentile_green_prob
# percentile_status values: good, fair, poor, NA
# sr_status values: good, fair, poor, NA
# psf_status values: good, fair, poor, extinct, data-deficient, not-assessed
# psf_status_code values: 1 to 9
# 1 = good
# 2 = fair
# 3 = poor
# 4 = extinct
# 5 = not-assessed (cyclic dominance)
# 6 = not-assessed (low productivity or high exploitation)
# 7 = data-deficient (insufficient time series length)
# 8 = data-deficient (no estimates of spawner abundance in the most recent generation)
# 9 = data-deficient (no spawner estimates available)

# Make this list into a dataframe that can be communiticated
code_PSF_Status <- data.frame(psf_status_code = 1:9,
                              psf_status = c("good","fair","poor","extinct",
                                             "not-assessed","not-assessed",
                                             "data-deficient"," data-deficient",
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

# Return list of CUs that have high exploitation rate or low production rates,
# as well as a final call on keeping or removing the CUs depending of their
# biostatus: the one with already a red/poor status are kept (i.e. Clare's 8th rule).
highExploit_lowProd <- cu_highExploit_lowProd_fun(biological_status_percentile)

# A. add column psf_status_code to each dataset:
biological_status_HBSRM$psf_status_code <- NA
biological_status_percentile$psf_status_code <- NA

# 4 = extinct
cu_extinct <- cu_extinct_fun()

row_toUpdate <- biological_status_HBSRM$cuid %in% cu_extinct$cuid
val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,4, sep = ", ")
biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- biological_status_percentile$cuid %in% cu_extinct$cuid
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,4, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

# 5 = not-assessed (cyclic dominance)
row_toUpdate <- grepl("(cyclic)",biological_status_HBSRM$CU_pse)
val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,5, sep = ", ")
biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- grepl("(cyclic)",biological_status_percentile$CU_pse)
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,5, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

# 6 = not-assessed (low productivity or high exploitation) --> Percentile only
highExploit_lowProd_toRemove <- highExploit_lowProd[highExploit_lowProd$toRemove,]
for(r in 1:nrow(highExploit_lowProd_toRemove)){
  # r <- 1
  rg <- highExploit_lowProd_toRemove$region[r]
  sp <- highExploit_lowProd_toRemove$species_abbr[r]
  cu <- highExploit_lowProd_toRemove$CU_name[r]
  
  row_toUpdate <- biological_status_percentile$region == rg &
    biological_status_percentile$species == sp &
    biological_status_percentile$CU_pse == cu
  
  val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
  val_new <- paste(val_toUpdate,6, sep = ", ")
  
  biological_status_percentile$psf_status_code[row_toUpdate] <- val_new
}

# 7 = data-deficient (insufficient time series length) --> Percentile only
row_toUpdate <- biological_status_percentile$dataPointNb < 20
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,7, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

# 8 = data-deficient (no estimates of spawner abundance in the most recent generation)
row_toUpdate <- biological_status_HBSRM$genLength_dataPointNb == 0
val_toUpdate <- biological_status_HBSRM$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,8, sep = ", ")
biological_status_HBSRM$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- biological_status_percentile$genLength_dataPointNb == 0
val_toUpdate <- biological_status_percentile$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,8, sep = ", ")
biological_status_percentile$psf_status_code[row_toUpdate] <- val_new

# 9 = data-deficient (no spawner estimates available)
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

# Remove "NA, "
biological_status_HBSRM$psf_status_code <- gsub("NA, ","",biological_status_HBSRM$psf_status_code)
biological_status_percentile$psf_status_code <- gsub("NA, ","",biological_status_percentile$psf_status_code)

unique(biological_status_HBSRM$psf_status_code)
unique(biological_status_percentile$psf_status_code)

# B. Combine the two datasets:

# rename certain columns in biological_status_percentile (might not be relevant anymore
# because the code in benchamrks_Percentiles.R has been updated).
colsToUpdate <- colnames(biological_status_percentile)[grepl("status_HSPercent",colnames(biological_status_percentile))]
colsUpdated <- gsub("HSPercent","percent",colsToUpdate)
colnames(biological_status_percentile)[colnames(biological_status_percentile) %in% colsToUpdate] <- colsUpdated

colCommon <- c("region","species","cuid","CU_pse","current_spawner_abundance","psf_status_code")
              # "year_last","year_first","genLength")
colHBSR <- c("status_Smsy_red","status_Smsy_amber","status_Smsy_green",
             "status_Smsy")
colPercent <- c("status_percent_075_red","status_percent_075_amber","status_percent_075_green",
                "status_percent075")

biological_status_merged <- merge(x = biological_status_HBSRM[,c(colCommon,colHBSR)],
                                  y = biological_status_percentile[,c(colCommon,colPercent)],
                                  by =  c("region","species","cuid","CU_pse","current_spawner_abundance"), 
                                  all = T)

# Check if number of CUs is correct:
CUs_comm <- biological_status_HBSRM$cuid[biological_status_HBSRM$cuid %in% 
                                          biological_status_percentile$cuid]
length(CUs_comm) # 136
CUs_HBSRM_only <- biological_status_HBSRM$cuid[!biological_status_HBSRM$cuid %in% 
                                               biological_status_percentile$cuid]
length(CUs_HBSRM_only) # 0
CUs_Percent_only <- biological_status_percentile$cuid[!biological_status_percentile$cuid %in% 
                                                       biological_status_HBSRM$cuid]
length(CUs_Percent_only) # 292

# Expected number of rows in biological_status_merged:
length(CUs_comm) + length(CUs_HBSRM_only) + length(CUs_Percent_only) # 428
nrow(biological_status_merged) # 428 --> ALL GOOD

# Renames columns
colnames(biological_status_merged) <- gsub("red","red_prob",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("amber","yellow_prob",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("green","green_prob",colnames(biological_status_merged))

colnames(biological_status_merged) <- gsub("status_Smsy_","sr_",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_percent_075_","percentile_",colnames(biological_status_merged))

colnames(biological_status_merged) <- gsub("status_Smsy","sr_status",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_percent075","percentile_status",colnames(biological_status_merged))

# Create psf_status field and Attribute 1 (good), 2 (fair) or 3 (poor) for 
biological_status_merged$psf_status <- NA #  values: good, fair, poor, extinct, data-deficient, not-assessed
biological_status_merged$psf_status_code <- NA # values: 1 to 9

col_prob <- colnames(biological_status_merged)[grepl("_prob",colnames(biological_status_merged))]
col_sr_prob <- col_prob[grepl("sr_",col_prob)]
col_percent_prob <- col_prob[grepl("percentile_",col_prob)]

for(r in 1:nrow(biological_status_merged)){
  # r <- 428
  bs_here <- biological_status_merged[r,]
  
  # HBSRM method:
  if(!is.na(bs_here$sr_red_prob) & is.na(bs_here$psf_status_code.x)){ # is.na(bs_here$psf_status_code.x) might not be necessary but does not hurt
    
    psf_status_here <- c("poor","fair","good")[bs_here[,col_sr_prob] == max(bs_here[,col_sr_prob])]
    psf_status_code_here <- c(3:1)[psf_status_here == c("poor","fair","good")]

  # Percentile method
  }else if(!is.na(bs_here$percentile_red_prob) & is.na(bs_here$psf_status_code.y)){
    
    psf_status_here <- c("poor","fair","good")[bs_here[,col_percent_prob] == max(bs_here[,col_percent_prob])]
    psf_status_code_here <- c(3:1)[psf_status_here == c("poor","fair","good")]
  
  # biostatus is not available 
  }else{
    
    code_HBSRM <- bs_here$psf_status_code.x
    code_Percentile <- bs_here$psf_status_code.y
    code_both <- c(code_HBSRM, code_Percentile)
    code_both <- code_both[!is.na(code_both)]
    code_both <- sapply(X = code_both, FUN =  strsplit, split = ", ")
    code_both <- as.numeric(code_both[[1]])
    code_both <- sort(code_both)
    code_both <- unique(code_both)
    
    psf_status_here <- NULL
    if(sum(code_both %in% 5:6) > 0){
      psf_status_here <- c(psf_status_here,"not-assessed")
    }
    if(sum(code_both %in% 7:9) > 0){
      psf_status_here <- c(psf_status_here,"data-deficient")
    }
    
    code_both <- paste(code_both,collapse = ", ")
    psf_status_code_here <- code_both
    
    psf_status_here <- paste(psf_status_here,collapse = ", ")
    
    # print(code_both)
  }
  
  biological_status_merged$psf_status[r] <- psf_status_here
  biological_status_merged$psf_status_code[r] <- psf_status_code_here
}

# Checks that there is no missing psf_status_code (i.e. no NAs)
unique(biological_status_merged$psf_status_code)
table(biological_status_merged$psf_status_code)
unique(biological_status_merged$psf_status)
table(biological_status_merged$psf_status)

# check that CUs without current spawner abundance info have the number 8
unique(biological_status_merged[,c("current_spawner_abundance","psf_status_code")][is.na(biological_status_merged$current_spawner_abundance),])

# Drop unecessary columns:
colToDrop <- c("psf_status_code.x","psf_status_code.y","current_spawner_abundance")
biological_status_merged <- biological_status_merged[,!colnames(biological_status_merged) %in% colToDrop]

# check if the psf code of cyclic communities: should be 5
biological_status_merged[grepl("cyclic",biological_status_merged$CU_pse),]

# Number CUs total:
nrow(biological_status_merged) # 428

# number CUs with biostatus assessed over both methods
condition <- biological_status_merged$psf_status_code %in% 1:3
sum(condition) # 151

# CUs not assessed because cyclic dynamics, low productivity/high mortality or data deficient
condition_5 <- grepl(pattern = "5",biological_status_merged$psf_status_code)
condition_6 <- grepl(pattern = "6",biological_status_merged$psf_status_code)
condition_7 <- grepl(pattern = "7",biological_status_merged$psf_status_code)
condition_5_6_7 <- condition_5 | condition_6 | condition_7
biological_status_merged$psf_status_code[condition_5_6_7]
sum(condition_5_6_7) # 233

# number CUs with biostatus assessed with HBSRM:
condition_HBSRM <- !is.na(biological_status_merged$sr_status) # 107
nrow(biological_status_merged[condition_1_2_3 & condition_HBSRM,]) # 107

# number CUs with biostatus assessed with percentile method: 
condition_Percent <- !is.na(biological_status_merged$percentile_status)
nrow(biological_status_merged[condition_1_2_3 & condition_Percent,]) # 151

nrow(biological_status_merged[condition_1_2_3 & condition_Percent & !condition_HBSRM,]) # 44

# write.csv(biological_status_merged,paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),
#           row.names = F)

biological_status_merged <- read.csv(paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),
                                     header = T)

# check that the CUs with status code 6 (high exploitation rate or production rates)
# with red status are still availalbe:
cond <- grepl("6",biological_status_merged$psf_status_code)
biological_status_merged[cond,]

cond <- biological_status_merged$percentile_status == 'red' & 
  !is.na(biological_status_merged$percentile_status)
unique(biological_status_merged$psf_status_code[cond])

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





