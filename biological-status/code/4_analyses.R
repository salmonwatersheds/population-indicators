
#' TODO FINISH CLEANING

#'******************************************************************************
#' The goal of the script is to analyse the biological status.
#' 
#' Files imported (from ):
#' - region_species_biological_status.csv (created in benchmarks_HBSRM.R)
#' - Biological_status_HBSR_Percentile_all.csv
#' 
#' Files produced: 
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
# Import datasets TO CLEAN ------

#'* Import the updated biostatus file for both HBSR and percentile *
pattern <- "Biological_status_HBSR_Percentile_all"
biological_status_merged <- import_mostRecent_file_fun(wd = wd_output, 
                                                       pattern = pattern)


#'* Import the updated benchmark file for both HBSR and percentile *
pattern <- "Benchmarks_HBSR_Percentile_all"
benchmarks_merged <- import_mostRecent_file_fun(wd = wd_output, 
                                                pattern = pattern)


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

#'* Import biostatus obtained with HBSR Sgen - Smsy: *
pattern <- "biological_status_HBSRM"
biological_status_HBSRM <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                         wd_output = wd_output,
                                                         region = region,
                                                         species_all = species_all)


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



#'* Import the current biostatus from the database (dataset101_output) *
fromDatabase <- update_file_csv <- F

# Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

biological_status_old <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[13],
                                               fromDatabase = fromDatabase,
                                               update_file_csv = update_file_csv,
                                               wd = wd_pop_indic_data_input_dropbox)

#'* Import the conservationunits_decoder.csv *
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

#
#
# Summary table of biostatus in BC (for Leah) ---------
#' @Bruno do you have an update on the total number of CUs in BC (including 
#' steelhead) that are red, amber, green, data deficient, and not assessed for 
#' biological status? The total count should be 446 CUs.
#' Can you also break this down by Region as the number of CUs (and %) in each 
#' category? As well as the number of CUs in each category by species?
#' https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1723581683183409?thread_ts=1723566844.000599&cid=CJG0SHWCW
biological_status_merged
benchmarks_merged

biological_status_merged$species_name |> unique()
biological_status_merged$psf_status |> unique()

unique(biological_status_merged[,c("psf_status","psf_status_code")])


# Find total number of CUs for each 

# Per region and species
data_summary_rg_sp <- biological_status_merged %>%
  filter(region != "Yukon") %>%
  group_by(region,species_name,psf_status) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  arrange(region,species_name,
          factor(psf_status, levels = c("good","fair","poor","extinct","not-assessed","data-deficient")))

# data_summary_rg_sp |> View()

# Per regions
data_summary_rg <- biological_status_merged %>%
  filter(region != "Yukon") %>%
  group_by(region,psf_status) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  arrange(region,
          factor(psf_status, levels = c("good","fair","poor","extinct","not-assessed","data-deficient")))

# data_summary_rg |> View()

# Per species
data_summary_sp <- biological_status_merged %>%
  filter(region != "Yukon") %>%
  group_by(species_name,psf_status) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  arrange(species_name,
          factor(psf_status, levels = c("good","fair","poor","extinct","not-assessed","data-deficient")))

# data_summary_sp |> View()

# For all BC:
data_summary_BC <- biological_status_merged %>%
  filter(region != "Yukon") %>%
  group_by(psf_status) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  arrange(factor(psf_status, levels = c("good","fair","poor","extinct","not-assessed","data-deficient")))

sum(data_summary_BC$count)

# make a list
list <- list(data_summary_BC,data_summary_rg,data_summary_sp,data_summary_rg_sp)

names(list) <- c("all BC","Per regions","Per species","Per regions and species")

# export an excel file
date <- Sys.Date()

for(sh_i in 1:length(names(list))){
  # sh_i <- 1
  if(sh_i == 1){
    append <- F
  }else{
    append <- T
  }
  sheetName <- names(list)[sh_i]
  sheet <- as.data.frame(list[[sheetName]])
  write.xlsx(sheet, 
             file = paste0(wd_output,"/Biostatus_BC_summary_",date,".xlsx"),
             sheetName = sheetName, 
             row.names = FALSE,
             append = append,
             showNA = T)
  print(sh_i)
}

#

#
# Figures biological status based on HBSRM comparison Smsy vs. 80% Smsy ------
#

condition_HBSRM <- !is.na(biological_status_merged$psf_status_type) & 
  biological_status_merged$psf_status_type == "sr"
sum(condition_HBSRM) # 125


# condition_1_2_3 <- biological_status_merged$psf_status_code %in% 1:3
# condition_HBSRM <- !is.na(biological_status_merged$sr_status) # 107
# nrow(biological_status_merged[condition_1_2_3 & condition_HBSRM,]) # 107
# nrow(biological_status_merged[condition_HBSRM,]) # 107
# 
cuids_HBSRM <- biological_status_merged$cuid[condition_HBSRM]
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
nrow(biological_status_HBSRM_cut[condition_diff,]) # 19

#
# Figures comparison biological status percentiles 0.75 vs. 0.50 -----
#
condition_Percent <- !is.na(biological_status_merged$psf_status_type) &
  biological_status_merged$psf_status_type == 'percentile'
sum(condition_Percent) # 69

nrow(biological_status_merged[condition_Percent,]) # 69

cuids_Percent <- biological_status_merged$cuid[condition_Percent] 
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
nrow(biological_status_percentile_cut[condition_diff,]) # 13



#
# Figure that compares biological status between HBSR and HS percentiles approach -----
#
condition_HBSRM <- !is.na(biological_status_merged$psf_status_type) & 
  biological_status_merged$psf_status_type == "sr"

condition_Percent <- !is.na(biological_status_merged$psf_status_type) &
  biological_status_merged$psf_status_type == 'percentile'

bioStatus_merged <- biological_status_merged[(condition_HBSRM | condition_Percent),]
nrow(bioStatus_merged) # 194

# Count how many CUs have the same biostatus with both approaches
bioStatus_merged_noNA <- bioStatus_merged[!is.na(bioStatus_merged$sr_status) &
                                            !is.na(bioStatus_merged$percentile_status),]
nrow(bioStatus_merged_noNA) # 136 --> number of CUs with biostatus with both methods

# Those with the same status:
cond <- bioStatus_merged_noNA$sr_status == bioStatus_merged_noNA$percentile_status
bioStatus_merged_same <- bioStatus_merged_noNA[cond,]
nrow(bioStatus_merged_same) # 82
# countHere <- table(factor(bioStatus_merged_same$sr_status,levels = c("red","amber","green")))
countHere <- table(factor(bioStatus_merged_same$sr_status,levels = c("poor","fair","good")))
table_n <- data.frame(bioStatus = c("poor","fair","good"),
                      n_same = as.numeric(countHere))

# Status only values for HBSRM:
bioStatus_merged_HBSRM_only <- bioStatus_merged[!is.na(bioStatus_merged$sr_status) & 
                                                  is.na(bioStatus_merged$percentile_status),]
nrow(bioStatus_merged_HBSRM_only) # 0
countHere <- table(factor(bioStatus_merged_HBSRM_only$sr_status,levels = c("poor","fair","good")))
table_n$sr_only <- as.numeric(countHere)

# Status only values for percentile benchmarks:
bioStatus_merged_percentile_only <- bioStatus_merged[is.na(bioStatus_merged$sr_status) & 
                                                       !is.na(bioStatus_merged$percentile_status),]
nrow(bioStatus_merged_percentile_only) # 58
countHere <- table(factor(bioStatus_merged_percentile_only$percentile_status,levels = c("poor","fair","good")))
table_n$percentile_only <- as.numeric(countHere)

# Different status:
cond <- bioStatus_merged_noNA$sr_status != bioStatus_merged_noNA$percentile_status
bioStatus_merged_diff <- bioStatus_merged_noNA[cond,]
nrow(bioStatus_merged_diff) # 54
# Here the count is based on sr_status
countHere <- table(factor(bioStatus_merged_diff$sr_status,levels = c("poor","fair","good")))
table_n$diff <- as.numeric(countHere)

table_m <- as.matrix(table_n[,c("n_same","sr_only","percentile_only","diff")])
rownames(table_m) <- table_n$bioStatus

# coloursStatus <- rev(c(g = "#8EB687", a = "#DFD98D", r = "#9A3F3F"))

if(printFig){
  jpeg(paste0(wd_figures,"/comparison_bioStatus_HBSR_Percentiles.jpg"), 
       width = 20,height = 15, units = "cm", res = 300)
}
barplot(height = table_m, 
        col = status_cols[c('red','amber','green')], 
        ylim = c(0,max(apply(table_m,2,sum)) + 5), las = 1,
        ylab = "Number of CUs",xlab = "Biological status difference",
        main = "Bio-status differences with HBSR vs. percentiles methods",
        names.arg = c('Same','SR only','percentile only','Different'))
#polygon(x = c(1,2,2,1),y = c(20,20,60,60))
offset <- .2
barLarger <- 1
x0 <- offset*4 + (ncol(table_m)-1)*barLarger + barLarger/2
x1 <- x0 + barLarger/2
# create a polygons for each possible combination of biostatus
height <- 0
for(bs in c("poor","fair","good")){
  # bs <- c("poor","fair","good")[1]
  bioStatus_merged_diff_cut <- bioStatus_merged_diff[bioStatus_merged_diff$sr_status == bs,]
  if(nrow(bioStatus_merged_diff) > 0){
    bioStatusHSPercentHere <- unique(bioStatus_merged_diff_cut$percentile_status)
    for(bshsp in bioStatusHSPercentHere){
      # bshsp <- bioStatusHSPercentHere[1]
      colourHere <- status_cols[c('red','amber','green')][bshsp == c("poor","fair","good")]
      cond <- bioStatus_merged_diff_cut$percentile_status == bshsp
      bioStatus_merged_diff_cut2 <- bioStatus_merged_diff_cut[cond,]
      heightUP <- nrow(bioStatus_merged_diff_cut2) + height
      polygon(x = c(x0,x1,x1,x0),y = c(height,height,heightUP,heightUP),col = colourHere)
      height <- heightUP
    }
  }
}
# add the correspondging method for the lab bar
text(labels = "SR",x = x0 - barLarger/4, y = height, pos = 3, cex = .8)
text(labels = "percentile",x = x0 + barLarger/4, y = height, pos = 3, cex = .8)
#
if(printFig){
  dev.off()
}

# big contrasts:
bioStatus_merged_diff[bioStatus_merged_diff$sr_status == "poor" & 
                        bioStatus_merged_diff$percentile_status == "good",]

bioStatus_merged_diff[bioStatus_merged_diff$sr_status == "good" & 
                        bioStatus_merged_diff$percentile_status == "poor",]

#
# Figures showing the spawner abundance time series and the benchmarks -------
#' - make new figures show the spawner abundance time series and the thresholds
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1717434872482819

datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- T

#' Import the recruitsperspawner.csv 
# recruitsperspawner <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[3],
#                                             fromDatabase = fromDatabase,
#                                             update_file_csv = update_file_csv,
#                                             wd = wd_pop_indic_data_input_dropbox)

#' Import the cuspawnerabundance.csv
spawnerabundance_cu <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[2],
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)

# Import the current biostatus from the database (dataset101_output) NOT NEEDED ?
biological_status_cu <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[13],
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)

# In case the file on the database is not updated:
# biological_status_cu <- read.csv(paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),
#                                      header = T)
# biological_status_cu <- import_mostRecent_file_fun(wd = wd_output,
#                                                    pattern = "dataset101_biological_status")

# biological_status <- read.csv(paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),
#                               header = T)

# Import the current biostatus from the database (dataset102_output)
benchmarks_cu <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[19],
                                    fromDatabase = fromDatabase,
                                    update_file_csv = update_file_csv,
                                    wd = wd_pop_indic_data_input_dropbox)

# In case the file on the database is not updated:
# benchmarks_cu <- read.csv(paste0(wd_output,"/Benchmarks_HBSR_Percentile_all.csv"),
#                                  header = T)
# benchmarks_cu <- import_mostRecent_file_fun(wd = wd_output,
#                                             pattern = "dataset102_benchmarks")

# Import the conservationunits_decoder.csv
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

# Import dataset103_output for the smooth spawner abundance:
# cuspawnerabund_smooth <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[16],
#                                                fromDatabase = fromDatabase,
#                                                update_file_csv = update_file_csv,
#                                                wd = wd_pop_indic_data_input_dropbox)


# find CUs with biostatus:
cond <- biological_status_cu$psf_status_code %in% 1:3
cuid_biostat <- biological_status_cu$cuid[cond]

figure_print <- T
percent <- 0
for(cuid in cuid_biostat){
  # cuid <- 408
  plot_spawnerAbundance_benchmarks_fun(cuid = cuid,
                                       cuspawnerabundance = spawnerabundance_cu, 
                                       dataset101_output = biological_status_cu, 
                                       dataset102_output = benchmarks_cu, 
                                       #dataset103_output = cuspawnerabund_smooth,
                                       conservationunits_decoder = conservationunits_decoder, 
                                       figure_print = figure_print, # figure_print, 
                                       wd_figures = wd_figures)
  
  progress <- which(cuid == cuid_biostat) / length(cuid_biostat) * 100
  if(progress > percent){
    percent <- percent + 10
    print(paste0("Progess: ",round(progress),"%"))
  }
}

#' Check the CUs with HBSR benchmarks and a different outcome when using the 
#' probabilities and the benchmarks:

cond <- biological_status_cu$psf_status_type == 'sr' & !is.na(biological_status_cu$psf_status_type)
cuid_sr <- biological_status_cu$cuid[cond]

bs_check <- biological_status_cu[cond,c("region","cuid","species_abbr","cu_name_pse","sr_status")]
bs_check$sr_status_2 <- NA

for(r in 1:nrow(bs_check)){
  # r <- 1
  cuid <- bs_check$cuid[r]
  cond <- benchmarks_cu$cuid %in% cuid
  csa <- benchmarks_cu$curr_spw[cond]
  Sgen <- benchmarks_cu$sgen[cond]
  Smsy <- benchmarks_cu$smsy[cond]
  if(csa <= Sgen){
    status <- "poor"
  }else if(csa <= Smsy){
    status <- "fair"
  }else{
    status <- "good"
  }
  bs_check$sr_status_2[r] <- status
}

cond <- bs_check$sr_status != bs_check$sr_status_2
bs_check[cond,]

#
# Same figure as above but for cyclic CUs Percentiles -----
# These CUs will have their figure produced in the above section in future so 
# no need to keep this section then.
#

datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- F

#' Import the cuspawnerabundance.csv
spawnerabundance <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[2],
                                             fromDatabase = fromDatabase,
                                             update_file_csv = update_file_csv,
                                             wd = wd_pop_indic_data_input_dropbox)

# Import the conservationunits_decoder.csv
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

# Import the cyclic_biological_status_percentiles files
biostatus_101 <- rbind_biologicalStatusCSV_fun(pattern = "cyclic_biological_status_percentiles",
                                           wd_output = paste0(wd_output,"/intermediate"), 
                                           region = unique(conservationunits_decoder$region))

head(biostatus_101)

# Import the cyclic_benchmarks_summary_percentiles files
benchmarks <- rbind_biologicalStatusCSV_fun(pattern = "cyclic_benchmarks_summary_percentiles",
                                           wd_output = paste0(wd_output,"/intermediate"), 
                                           region = unique(conservationunits_decoder$region))

head(benchmarks)

#'* fill biostatus_101 for these CUs because it was not done in 3_biological_status.R *
biostatus_101$percentile_red_prob <- biostatus_101$status_percent_05_red
biostatus_101$percentile_yellow_prob <- biostatus_101$status_percent_05_amber
biostatus_101$percentile_green_prob <- biostatus_101$status_percent_05_green

biostatus_101$percentile_status <- NA # apply did not work...?!
for(r in 1:nrow(biostatus_101)){
  # r <- which(biostatus_101$cuid == 731)
  csa <- biostatus_101$current_spawner_abundance[r]
  
  cond_cuid <- benchmarks$cuid == biostatus_101$cuid[r]
  cond_cl <- benchmarks$cycle_line_current
  cond_bench_low <- benchmarks$benchmark == "benchmark_0.25"
  cond_bench_up <- benchmarks$benchmark == "benchmark_0.5"
  if(csa <= benchmarks$m[cond_cuid & cond_cl & cond_bench_low]){
    percentile_status <- "poor"
  }else if(csa <= benchmarks$m[cond_cuid & cond_cl & cond_bench_up]){
    percentile_status <- "fair"
  }else{
    percentile_status <- "good"
  }
  
  biostatus_101$percentile_status[r]  <- percentile_status
}

biostatus_101$sr_red_prob <- NA
biostatus_101$sr_yellow_prob <- NA
biostatus_101$sr_green_prob <- NA
biostatus_101$sr_status <- NA

biostatus_101$psf_status_code <- NA
for(r in 1:nrow(biostatus_101)){
  out <- 1
  if(biostatus_101$percentile_status[r] == "fair"){
    out <- 2
  }else if(biostatus_101$percentile_status[r] == "poor"){
    out <- 3
  }
  biostatus_101$psf_status_code[r]  <- out
}
biostatus_101$psf_status_code_all <- biostatus_101$psf_status_code
biostatus_101$psf_status_type <- "percentile"


#'* fill biological_status_cu for these CUs because it was not done in 3_biological_status.R *

benchmarks_102_cyclic <- NULL
for(cuid in unique(benchmarks$cuid)){
  # cuid <- benchmarks$cuid[1]
  for(cl in 1:4){
    # cl <- 1
    cond_cuid <- conservationunits_decoder$cuid == cuid
    benchmarks_102_here <- data.frame(region = conservationunits_decoder$region[cond_cuid],
                                      cuid = cuid,
                                      region_abbr = conservationunits_decoder$region_abbr[cond_cuid],
                                      cu_name_pse = conservationunits_decoder$cu_name_pse[cond_cuid])
    
    cond_cuid <- biostatus_101$cuid == cuid
    benchmarks_102_here$curr_spw <- biostatus_101$current_spawner_abundance[cond_cuid]
    
    benchmarks_102_here$sgen <- NA           # lower HBSRM benchmark
    benchmarks_102_here$sgen_lower <- NA     #
    benchmarks_102_here$sgen_upper <- NA     #
    benchmarks_102_here$smsy <- NA           # upper HBSRM benchmark
    benchmarks_102_here$smsy_lower <- NA     #
    benchmarks_102_here$smsy_upper <- NA     #
    
    cond_cuid <- benchmarks$cuid == cuid
    cond_25 <- benchmarks$benchmark == "benchmark_0.25"
    cond_50 <- benchmarks$benchmark == "benchmark_0.5"
    cond_cl <- benchmarks$cycle_line == cl
    benchmarks_102_here$`25%_spw` <- benchmarks$m[cond_cuid & cond_cl & cond_25]
    benchmarks_102_here$`25%_spw_lower` <- benchmarks$CI025[cond_cuid & cond_cl & cond_25]
    benchmarks_102_here$`25%_spw_upper` <- benchmarks$CI975[cond_cuid & cond_cl & cond_25]
    benchmarks_102_here$`75%_spw` <- benchmarks$m[cond_cuid & cond_cl & cond_50]
    benchmarks_102_here$`75%_spw_lower` <- benchmarks$CI025[cond_cuid & cond_cl & cond_50]
    benchmarks_102_here$`75%_spw_upper` <- benchmarks$CI975[cond_cuid & cond_cl & cond_50]
    
    cond_cuid <- biostatus_101$cuid == cuid
    benchmarks_102_here$curr_spw_start_year <- biostatus_101$yr_withData_start[cond_cuid]
    benchmarks_102_here$curr_spw_end_year <- biostatus_101$yr_withData_end[cond_cuid]
    
    benchmarks_102_here$cycle_line <- cl
    
    # is this cycle-line the one including the last year of the data?
    cond_cuid <- benchmarks$cuid == cuid
    cond_cl <- benchmarks$cycle_line == cl
    cl_current <- benchmarks$cycle_line_current[cond_cuid & cond_cl] |> unique() # there are three rows, one for each benchmark value
    benchmarks_102_here$cycle_line_current <- cl_current
    
    if(is.null(benchmarks_102_cyclic)){
      benchmarks_102_cyclic <- benchmarks_102_here
    }else{
      benchmarks_102_cyclic <- rbind(benchmarks_102_cyclic,benchmarks_102_here)
    }
  }
}
head(benchmarks_102_cyclic)

log10_scale <- T
figure_print <- T
for(cuid in unique(benchmarks_102_cyclic$cuid)){
  # cuid <- unique(benchmarks_102_cyclic$cuid)[2]
  # cond_cl_current <- benchmarks_102_cyclic$cycle_line_current
  plot_spawnerAbundance_benchmarks_fun(cuid = cuid,
                                       cuspawnerabundance = spawnerabundance, 
                                       dataset101_biological_status = biostatus_101, # biological_status_cu, 
                                       dataset102_benchmarks = benchmarks_102_cyclic,# [cond_cl_current,], # benchmarks_cu, 
                                       #dataset103_output = cuspawnerabund_smooth,
                                       conservationunits_decoder = conservationunits_decoder, 
                                       log10_scale = log10_scale,
                                       figure_print = figure_print, 
                                       wd_figures = wd_figures, 
                                       file_name_nchar = 60)
}


#
# Change of status between the old vs. new upper threshold: OLD CODE -----
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
# Check the difference between normal percentile benchmarks and the simulated ones (OLD CODE) -----
nrow(benchmarks_summary_percentile)
benchmarks_summary_percentile_noNA <- benchmarks_summary_percentile[!is.na(benchmarks_summary_percentile$m),]
nrow(benchmarks_summary_percentile_noNA)
percent_diff <- (benchmarks_summary_percentile_noNA$m - benchmarks_summary_percentile_noNA$m_sim) / benchmarks_summary_percentile_noNA$m * 100
hist(percent_diff)

benchmarks_summary_percentile_noNA[percent_diff < -50,]
# do those have cyclic dynamics ?

# 
# Compare biostatus old vs. new IN PROGRESS ------
#' Goal:
#' - flag the CUs whose status changed
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1717434872482819

head(biological_status_old)
nrow(biological_status_old) # 448

head(biological_status_merged)
nrow(biological_status_merged) # 466

biological_status_oldNew <- merge(x = biological_status_old,
                                  y = biological_status_new, 
                                  by = c("region","species_name","cu_name_pse","cuid"), 
                                  all = T)
plot(x = biological_status_oldNew$percentile_green_prob.x, 
     y = biological_status_oldNew$percentile_green_prob.y)
abline(a = 0, b = 1)



# Only keep the CUs with biostatus in biological_status_merged
head(biological_status_merged)

biological_status_merged$sr_status
biological_status_merged$percentile_status
unique(biological_status_merged$psf_status)
unique(biological_status_merged$psf_status_code)
unique(biological_status_merged$psf_status_type)

cond <- biological_status_merged$psf_status_code %in% 1:3
biological_status_new <- biological_status_merged[cond,]
nrow(biological_status_new) # 184
 


#' Add the fields sr_status, percentile_status and psf_status to biological_status_old:
#' Use the colour code for that (!):
#' https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1714167872794159?thread_ts=1701199596.229739&cid=CJG0SHWCW
#' the hist_red, hist_yellow, and hist_green fields are currently used by the pse
#' data code to determine the percentile status. 
#' if hist_red = #CC0000 the status is poor elseif
#' hist_yellow = #FFFF00 the status is fair elseif 
#' hist_green = #009900 the status is good else
#' data-deficient. 
#' currently, in the hist_ hex fields (unlike in the sr_ hex fields) only the 
#' outcome status color is the colored (non-#FFFFFF) code. the code is set up to
#' report the worst status though across the three fields (so it will be fine if
#' non-#FFFFFF hex codes are output for any non-zero percentile benchmark status
#' outcome probabilities).
head(biological_status_old)
fields <- c("hist_red","hist_yellow","hist_green")
biological_status_old$hist_status <- NA
for(i in 1:nrow(biological_status_old)){
  r <- biological_status_old[i,fields]
  cond_NA <- is.na(r) 
  if(all(cond_NA)){
    status <- NA
  }else{
    cond <- r != "#FFFFFF"
    if(all(!cond)){
      status <- NA
    }else{
      col <- r[cond]
      cond <- col == c("#CC0000","#FFFF00","#009900")
      status <- c("poor","fair","good")[cond]
      if(length(status) == 0){
        print("Multiple colours so stopped")
        break
      }
    }
  }
  biological_status_old$hist_status[i] <- status
}

unique(biological_status_old$hist_status)

# Add sr_status
fields <- c("sr_red_prob","sr_yellow_prob","sr_green_prob")
biological_status_old$sr_status <- NA
for(i in 1:nrow(biological_status_old)){
  # i <- 1
  r <- biological_status_old[i,fields]
  cond_NA <- is.na(r) 
  if(all(cond_NA)){
    status <- NA
  }else{
    cond <- r == max(r)
    status <- c("poor","fair","good")[cond]
    if(length(status) == 0){
      print("Multiple max prob so stopped")
      break
    }
  }
  biological_status_old$sr_status[i] <- status
}

unique(biological_status_old$sr_status)

# Add psf_status and psf_status_type to biological_status_old
biological_status_old$psf_status <- apply(X = biological_status_old, 1, 
                                          FUN = function(r){
                                            # r <- biological_status_old[1,]
                                            if(!is.na(r[c("sr_status")])){
                                              out <- r[c("sr_status")]
                                            }else if(!is.na(r[c("hist_status")])){
                                              out <- r[c("hist_status")]
                                            }else{
                                              out <- NA
                                            }
                                            return(out)
                                          })

biological_status_old$psf_status_type <- apply(X = biological_status_old, 1, 
                                          FUN = function(r){
                                            # r <- biological_status_old[1,]
                                            if(!is.na(r[c("sr_status")])){
                                              out <- "sr"
                                            }else if(!is.na(r[c("hist_status")])){
                                              out <- "percentile"
                                            }else{
                                              out <- NA
                                            }
                                            return(out)
                                          })

# Are there any CUs with NAs for both hist_status and sr_status --> No
cond <- is.na(biological_status_old$sr_status) & is.na(biological_status_old$hist_status)
biological_status_old[cond,]

#'* merge the two dataframes
colnames(biological_status_old)
colnames(biological_status_new)

biological_status_oldNew <- merge(x = biological_status_old,
                                  y = biological_status_new, 
                                  by = c("region","species_name","cu_name_pse","cuid"), 
                                  all = T)

#'* CUs with a new biostatus but not an old one *
cond <- is.na(biological_status_oldNew$psf_status.x) & !is.na(biological_status_oldNew$psf_status.y) 
data <- biological_status_oldNew[cond,c("region","species_name","cu_name_pse","cuid",
                                "psf_status.x","psf_status.y","psf_status_code_all")]
nrow(data)
# 32 CUs






#'* CUs with a new biostatus but not an old one *
#'* CUs with a new biostatus but not an old one *
#'* CUs with a new biostatus but not an old one *



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
