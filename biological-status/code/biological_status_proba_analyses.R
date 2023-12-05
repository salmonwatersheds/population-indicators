
#'******************************************************************************
#' The goal of the script is to analyse the biological status probabilities 
#' obtained from the HBSRM analysis.
#' 
#' Files imported (from ):
#' - region_species_biological_status.csv (created in benchmarks_HBSRM.R)
#' 
#' Files produced: 
#' - Biological_status_HBSRM_all.csv
#' 
#' Notes:
#' - 
#' 
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
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS
wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
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

# Import benchmarks values and biological status files for both methods -----
# Import all the CSV files for each combination of region - species and rbind them

#'* Import biostatus obtained with HBSR Sgen - Smsy: *
pattern <- "biological_status"
biological_status_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)

# write.csv(biological_status_df,paste0(wd_output,"/Biological_status_HBSRM_all.csv"),
#           row.names = F)
head(biological_status_df)
colnames(biological_status_df)
nrow(biological_status_df) # 142
unique(biological_status_df$comment)

#'* Import the benchmark values associated with the HBSRM *
pattern <- "benchmarks_summary"
benchmarks_summary_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                       wd_output = wd_output,
                                                       region = region,
                                                       species_all = F)

head(benchmarks_summary_df)
nrow(unique(benchmarks_summary_df[,c("region","species","CU")])) # 130

# merge biological_status_df with benchmarks_summary_df with the highest posterior density estimate
benchmarks_summary_df
benchmarks_summary_df_Smsy_HPD <- benchmarks_summary_df[benchmarks_summary_df$benchmark == "Smsy" & benchmarks_summary_df$method == "HPD",]

final_HBSRM <- merge(x = biological_status_df, 
                     y = benchmarks_summary_df_Smsy_HPD[,c("region","species","CU","m")],
                     by = c("region","species","CU"),
                     all.x = T)


#'* Import the biological status obtained with HS percentile method: *
pattern <- "biological_status_SH_percentiles"
biological_status_HSPercetn_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)

nrow(biological_status_HSPercetn_df) # 428

#'* Import the historical spawner abundance benchmark values *
pattern <- "HS_percentiles_summary"
benchmarks_summary_HSPercent_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                                 wd_output = wd_output,
                                                                 region = region,
                                                                 species_all = F)

head(benchmarks_summary_HSPercent_df)

#'* Import conservationunits_decoder of the PSE database *
datasetsNames_database <- datasetsNames_database_fun()
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   wd = wd_pop_indic_data_input_dropbox)

#
# Figures biological status based on HBSRM comparison Smsy vs. 80% Smsy ------
#
printFig <- F

biological_status_compare_fun(biological_status_df = biological_status_df,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "region")

biological_status_compare_fun(biological_status_df = biological_status_df,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "species")

#
# Figures comparison biological status HS abundance percentiles 0.75 vs. 0.50 -----

# remove CUs with less than 20 data points
biological_status_HSPercetn_20dpt <- biological_status_HSPercetn_df[biological_status_HSPercetn_df$dataPointNb >= 20,]
nrow(biological_status_HSPercetn_20dpt) # 206

printFig <- F

biological_status_compare_fun(biological_status_df = biological_status_HSPercetn_20dpt,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "region")

biological_status_compare_fun(biological_status_df = biological_status_HSPercetn_20dpt,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "species")

#
# Figure that compares biological status between HBSR and HS percentiles approach -----

# remove NAs
bioStatus_HSPercent <- biological_status_HSPercetn_df[!is.na(biological_status_HSPercetn_df$status_HSPercent_05_red),]

# remove Cus with less than 20 data points
bioStatus_HSPercent <- bioStatus_HSPercent[bioStatus_HSPercent$dataPointNb >= 20,]
nrow(bioStatus_HSPercent) # 158

# add column bioStatus for 0.25 - 0.75 benchmarks:
colProba <- colnames(bioStatus_HSPercent)[grepl("_075_",colnames(bioStatus_HSPercent))]
bioStatus_HSPercent$bioStatus_HSPercent <- sapply(X = 1:nrow(bioStatus_HSPercent), 
                                                  FUN = function(r){
                                                    # r <- 1
                                                    slice <- bioStatus_HSPercent[r,colProba]
                                                    out <- c("red","amber","green")[slice == max(slice)]
                                                    return(out)
                                                  })

colToKeep_HSPercent <- c("region","species","CU_pse","bioStatus_HSPercent")

# Biostatus obtained from HBSR:
# remove CUs with NAs
bioStatus_HBSR <- biological_status_df[!is.na(biological_status_df$status_Smsy_red),]

# add column bioStatus for Sgen - Smsy benchmarks:
colProba <- colnames(bioStatus_HBSR)[grepl("_Smsy_",colnames(bioStatus_HBSR))]
bioStatus_HBSR$bioStatus_HBSR <- sapply(X = 1:nrow(bioStatus_HBSR), 
                                                  FUN = function(r){
                                                    # r <- 1
                                                    slice <- bioStatus_HBSR[r,colProba]
                                                    out <- c("red","amber","green")[slice == max(slice)]
                                                    return(out)
                                                  })

colToKeep_HBSR <- c("region","species","CU_pse","bioStatus_HBSR")

# merge the two datasets:

bioStatus_merged <- merge(x = bioStatus_HBSR[,colToKeep_HBSR],
                          y = bioStatus_HSPercent[,colToKeep_HSPercent], 
                          by = c("region","species","CU_pse"),
                          all = T)

nrow(bioStatus_merged) # 221

# count how many CUs have the same biostatus with both approaches
bioStatus_merged_same <- bioStatus_merged[bioStatus_merged$bioStatus_HBSR == bioStatus_merged$bioStatus_HSPercent,]
bioStatus_merged_same <- bioStatus_merged_same[!is.na(bioStatus_merged_same$region),]

countHere <- table(bioStatus_merged_same$bioStatus_HBSR)[c("red","amber","green")]
sum(countHere) # 19

table_n <- data.frame(bioStatus = c("red","amber","green"),
                      n_same = as.numeric(countHere))

# counts for how many CUs have only values for HBSR
bioStatus_merged_HBSR_only <- bioStatus_merged[!is.na(bioStatus_merged$bioStatus_HBSR) & 
                                                 is.na(bioStatus_merged$bioStatus_HSPercent),]

countHere <- table(bioStatus_merged_HBSR_only$bioStatus_HBSR)[c("red","amber","green")]
sum(countHere) # 63

table_n$HBSR_only <- as.numeric(countHere)

# counts for how many CUs have only values for HS benchmarks
bioStatus_merged_HSBench_only <- bioStatus_merged[is.na(bioStatus_merged$bioStatus_HBSR) & 
                                                    !is.na(bioStatus_merged$bioStatus_HSPercent),]

countHere <- table(bioStatus_merged_HSBench_only$bioStatus_HSPercent)[c("red","amber","green")]
sum(countHere) # 108

table_n$HSBench_only <- as.numeric(countHere)

# counts for how many CUs have different biostatus
bioStatus_merged_diff <- bioStatus_merged[bioStatus_merged$bioStatus_HBSR != bioStatus_merged$bioStatus_HSPercent,]
bioStatus_merged_diff <- bioStatus_merged_diff[!is.na(bioStatus_merged_diff$region),]
nrow(bioStatus_merged_diff) # 31

# make all column for counts for HBSR 
countHere <- table(bioStatus_merged_diff$bioStatus_HBSR)[c("red","amber","green")]
sum(countHere) # 31

table_n$diff <- as.numeric(countHere)

table_m <- as.matrix(table_n[,c("n_same","HBSR_only","HSBench_only","diff")])
rownames(table_m) <- table_n$bioStatus

coloursStatus <- rev(c(g = "#8EB687", a = "#DFD98D", r = "#9A3F3F"))

if(printFig){
  jpeg(paste0(wd_figures,"/comparison_bioStatus_HBSR_HSPercent.jpg"), 
       width = 20,height = 15, units = "cm", res = 300)
}
barplot(height = table_m, col = coloursStatus, 
        ylab = "Number of CUs",xlab = "Biological status difference",
        main = "Bio-status differences with HBSR vs. HS percentiles approaches",
        names.arg = c('Same','HBSR only','HS percent only','Different'))
#polygon(x = c(1,2,2,1),y = c(20,20,60,60))
offset <- .2
barLarger <- 1
x0 <- offset*4 + (ncol(table_m)-1)*barLarger + barLarger/2
x1 <- x0 + barLarger/2
# create a polygons for each possible combination of biostatus
height <- 0
for(bs in c("red","amber","green")){
  # bs <- c("red","amber","green")[2]
  bioStatus_merged_diff_cut <- bioStatus_merged_diff[bioStatus_merged_diff$bioStatus_HBSR == bs,]
  if(nrow(bioStatus_merged_diff) > 0){
    bioStatusHSPercentHere <- unique(bioStatus_merged_diff_cut$bioStatus_HSPercent)
    for(bshsp in bioStatusHSPercentHere){
      # bshsp <- bioStatusHSPercentHere[]
      colourHere <- coloursStatus[bshsp == c("red","amber","green")]
      bioStatus_merged_diff_cut2 <- bioStatus_merged_diff_cut[bioStatus_merged_diff_cut$bioStatus_HSPercent == bshsp,]
      heightUP <- nrow(bioStatus_merged_diff_cut2) + height
      polygon(x = c(x0,x1,x1,x0),y = c(height,height,heightUP,heightUP),col = colourHere)
      height <- heightUP
    }
  }
}
# add the correspondging method for the lab bar
text(labels = "HBSR",x = x0 - barLarger/4, y = height, pos = 3, cex = .8)
text(labels = "HS percent",x = x0 + barLarger/4, y = height, pos = 3, cex = .8)
#
if(printFig){
  dev.off()
}

# big contrasts:
bioStatus_merged_diff[bioStatus_merged_diff$bioStatus_HBSR == "green" & bioStatus_merged_diff$bioStatus_HSPercent == "red",]
# Skeena      SX Stephens

bioStatus_merged_diff[bioStatus_merged_diff$bioStatus_HBSR == "red" & bioStatus_merged_diff$bioStatus_HSPercent == "green",]

#
# Decision rules for HS percentile benchmarks (TEMPORARY LOCATION) -------

# QUESTION: Are those for both methods or just the HS percentile? --> cf. figure

#' The 5 rules to exclude data:
#' - cyclic dominance
#' - low productivity or high exploitation rate 
#' - data deficient
#' - no current abundance
#' - insufficient time series length (< 20 data points)
#' New rule from Claire: 


# Katy's request about Columbia Lake SX CU 1300 dataset_101 and dataset_102 ------
# https://salmonwatersheds.slack.com/archives/C01D2S4PRC2/p1701474308941739
#' - update dataset 101 file for Columbia that includes the percentile confidence
#' intervals for cuid 1300 Osoyoos lake sockeye?
#' - fields 25%_spw_lower, 25%_spw_upper, 75%_spw_lower, 75%_spw_upper are empty in database
#' 

#'* Import the most recent dataset_101 *
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Columbia/data & analysis/analysis/columbia-status/Output")
pattern <- "dataset_101"
filesList <- list.files(path = path)
filesList <- filesList[grepl(pattern,filesList)]
mostRecentF <-filesList[file.info(paste(path,filesList,sep = "/"))$mtime == max(file.info(paste(path,filesList,sep = "/"))$mtime)]
# "dataset_101.May272022.csv"
dataset_101 <- read.csv(paste(path,mostRecentF,sep = "/"),header = T)
dataset_101 <- dataset_101[,-1]
head(dataset_101)
nrow(dataset_101)

# Import another dataset_101 file from Fraser_VIMI for comparison because the headers
# are bit different:
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Fraser_VIMI/analysis/Fraser status assessment/Output")
pattern <- "dataset_101"
filesList <- list.files(path = path)
filesList <- filesList[grepl(pattern,filesList)]
mostRecentF <- filesList[file.info(paste(path,filesList,sep = "/"))$mtime == max(file.info(paste(path,filesList,sep = "/"))$mtime)]
dataset_101_VIMI <- read.csv(paste(path,mostRecentF,sep = "/"),header = T)
dataset_101_VIMI <- dataset_101_VIMI[,-1]
head(dataset_101_VIMI)

# There is no data about Columbia in benchmarks_summary_df"
unique(benchmarks_summary_df$region)

# benchmarks_summary_HSPercent_df: 
benchmarks_summary_HSPercent__1300 <- benchmarks_summary_HSPercent_df[benchmarks_summary_HSPercent_df$region == "Columbia" &
                                                                      benchmarks_summary_HSPercent_df$cuid == 1300  &
                                                                      benchmarks_summary_HSPercent_df$dataPointNb >= 20,]

biological_status_HSPercetn_1300 <- biological_status_HSPercetn_df[biological_status_HSPercetn_df$region == "Columbia"  &
                                                                   biological_status_HSPercetn_df$cuid == 1300  &
                                                                   biological_status_HSPercetn_df$dataPointNb >= 20,]

# checking that the CUID is 1300
conservationunits_decoder$cuid[conservationunits_decoder$region == "Columbia"]
conservationunits_decoder[conservationunits_decoder$cuid == 1300,] # all good


# fill dataset_101

# CHANGE: rename "cu" by "cuid"
colnames(dataset_101)[colnames(dataset_101)== "cu"] <- "cuid"

# CHANGE: change "hist_" for "percentile_"
col_hist <- colnames(dataset_101)[grepl("hist_",colnames(dataset_101))]
colnames(dataset_101)[colnames(dataset_101) %in% col_hist] <- gsub("hist","percentile",col_hist)

# CHANGE: add percentile_red_prob for each of the three colours
dataset_101$percentile_prob_red <- biological_status_HSPercetn_1300$status_HSPercent_075_red
dataset_101$percentile_prob_yellow <- biological_status_HSPercetn_1300$status_HSPercent_075_amber
dataset_101$percentile_prob_green <- biological_status_HSPercetn_1300$status_HSPercent_075_green

# CHANGE: add percentile status
prob_status <- as.numeric(dataset_101[,c("percentile_prob_red","percentile_prob_yellow","percentile_prob_green")])
dataset_101$percentile_status <- c("poor","fair","good")[prob_status == max(prob_status)]

# CHANGE: fill the percentile_red/yellow/green columns
colours <- c("#CC0000","#FFFF00","#009900","#FFFFFF")
colHere <- paste0("percentile_",c("red","yellow","green"))
dataset_101[,colHere] <- "#FFFFFF"
dataset_101[,colHere][prob_status == max(prob_status)] <- c("#CC0000","#FFFF00","#009900")[prob_status == max(prob_status)]

# CHANGE: add extinct
dataset_101$extinct <- NA

# CHANGE: drop location
dataset_101 <- dataset_101[,colnames(dataset_101) != "location"]

# write CSV with date:
date <- Sys.Date()
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Columbia/data & analysis/analysis/columbia-status/Output")
write.csv(dataset_101,paste0(path,"/","dataset_101_",date,".csv"),row.names = F)


#'* Import the most recent dataset_102 *
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Columbia/data & analysis/analysis/columbia-status/Output")
pattern <- "dataset_102"
filesList <- list.files(path = path)
filesList <- filesList[grepl(pattern,filesList)]
mostRecentF <-filesList[file.info(paste(path,filesList,sep = "/"))$mtime == max(file.info(paste(path,filesList,sep = "/"))$mtime)]
# "dataset_101.May272022.csv"
dataset_102 <- read.csv(paste(path,mostRecentF,sep = "/"),header = T)
dataset_102 <- dataset_102[,-1]
head(dataset_102)
nrow(dataset_102)

# CHANGE: rename "cu" by "cuid"
colnames(dataset_102)[colnames(dataset_102) == "cu"] <- "cuid"

# CHANGE: fill the dataset
dataset_102$curr_spw[dataset_102$location == 1300] <- round(biological_status_HSPercetn_1300$current_spawner_abundance)
dataset_102$curr_spw_start_year[dataset_102$location == 1300] <- biological_status_HSPercetn_1300$year_first
dataset_102$curr_spw_end_year[dataset_102$location == 1300] <- biological_status_HSPercetn_1300$year_last
dataset_102$X25._spw[dataset_102$location == 1300] <- benchmarks_summary_HSPercent__1300$m[benchmarks_summary_HSPercent__1300$benchmark == "benchmark_0.25"]
dataset_102$X75._spw[dataset_102$location == 1300] <- benchmarks_summary_HSPercent__1300$m[benchmarks_summary_HSPercent__1300$benchmark == "benchmark_0.75"]

date <- Sys.Date()
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Columbia/data & analysis/analysis/columbia-status/Output")
write.csv(dataset_102,paste0(path,"/","dataset_102_",date,".csv"),row.names = F)

#
# Check the difference between normal percentile benchmarks and the simulated ones -----
nrow(benchmarks_summary_HSPercent_df)
benchmarks_summary_HSPercent_df_noNA <- benchmarks_summary_HSPercent_df[!is.na(benchmarks_summary_HSPercent_df$m),]
nrow(benchmarks_summary_HSPercent_df_noNA)
percent_diff <- (benchmarks_summary_HSPercent_df_noNA$m - benchmarks_summary_HSPercent_df_noNA$m_sim) / benchmarks_summary_HSPercent_df_noNA$m * 100
hist(percent_diff)

benchmarks_summary_HSPercent_df_noNA[percent_diff < -50,]
# do those have cyclic dynamics ?



