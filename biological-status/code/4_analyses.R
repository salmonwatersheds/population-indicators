
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
# region <- as.character(regions_df[1,])

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
#
# Summary population indicators/biostatus (PSE 2.0 Launch Key message) -------
# https://salmonwatersheds.slack.com/archives/CLCTC622J/p1728083530788579

#'* Import the current biostatus from the database (dataset101_output) *
fromDatabase <- update_file_csv <- F

# Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

biological_status <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[13],
                                               fromDatabase = fromDatabase,
                                               update_file_csv = update_file_csv,
                                               wd = wd_pop_indic_data_input_dropbox)

# Import the latest data update with Ricker benchmark status obtained for cyclic CUs
biological_status <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"),
                                                pattern = "dataset101_biological_status")
 
nrow(biological_status) # 463

# remove the Yukon
cond_yk <- biological_status$region == "Yukon"
biological_status <- biological_status[!cond_yk,]

nrow(biological_status) # 443

# Northern Transboundary if needed
unique(biological_status$region)


#'* Import the conservationunits_decoder.csv *
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)


nrow(conservationunits_decoder) # 469


#'* biological status &  data gaps *

#' biostatus vs. data-deficient vs. not-assessed for 
#' - region
#' - species
#' - region & species ?


# biostatus_data <- biostatus_tot
plot_biostatus_summary_fun <- function(biostatus_data,n_width = 10, n_height = NA,
                                       main = "",col_border = "black",
                                       psf_status_col = NA, ylab = "", 
                                       cex = 1, font = 2, line = 1,bty = 'n'){
  
  if(is.na(n_height)){
    n_height <- ceiling(sum(biostatus_data$count) / n_width)
  }
  
  if(is.na(psf_status_col)[1]){
    psf_status <- c("good","fair","poor","extinct","not-assessed","data-deficient")
    psf_status_col <- c("#83B687","#DED38A","#C06363","#924848","black","#A7A9AC")
    names(psf_status_col) <- psf_status
  }else{
    psf_status <- names(psf_status_col)
  }
  
  plot(NA, xlim = c(0,n_width), ylim = c(0,n_height), xaxt = 'n', yaxt = 'n',
       xlab = '', ylab = '', bty = bty, main = "", yaxs = "i", xaxs = "i")
  
  if(ylab != ""){
    mtext(text = ylab, side = 2, line = line, cex = cex, font = font)
  }
  if(main != ""){
    mtext(text = main, side = 3, line = line, cex = cex, font = font)
  }
  
  y <- 1
  x <- 1
  count_tot <- 1
  for(s in psf_status){
    # s <- psf_status[2]
    col_here <- psf_status_col[s]
    cond_s <- biostatus_data$psf_status == s
    
    if(sum(cond_s)){
      for(c in 1:biostatus_data$count[cond_s]){
        # c <- 1
        if(length(1:count_tot) > (y * n_width)){
          y <- y + 1
        }
        polygon(x = c(x-1,x,x,x-1),y = c(y-1,y-1,y,y),border = col_border,col = col_here)
        count_tot <- count_tot + 1
        x <- x + 1
        if(x > n_width){
          x <- 1
        }
      }
    }
  }
}

psf_status <- c("good","fair","poor","extinct","not-assessed","data-deficient")
psf_status_col <- c("#83B687","#DED38A","#C06363","#924848","black","#A7A9AC")

names(psf_status_col) <- psf_status

#'* Total biostatus *

unique(biological_status$psf_status)

biostatus_tot <- biological_status %>%
  group_by(psf_status) %>%
  summarise(count = n()) %>%
  arrange(psf_status)

biostatus_tot$percent <- round(biostatus_tot$count / sum(biostatus_tot$count) * 100,2)

jpeg(filename = paste0(wd_figures,"/PSE_summary/Biological_status_PSE2.0.jpeg"),
     width = 15, height = 20, units = "cm", res = 300)

par(mar = c(4,.5,.5,.5))
plot_biostatus_summary_fun(biostatus_data = biostatus_tot, n_width = 20, 
                           col_border = "white",psf_status_col = psf_status_col)
legend("bottom",legend = psf_status[c(1,4,2,5,3,6)],
       fill = psf_status_col[c(1,4,2,5,3,6)], 
       bty = 'n', ncol = 3, inset = c(0, -.1), xpd = TRUE)

dev.off()


jpeg(filename = paste0(wd_figures,"/PSE_summary/Biological_status_PSE2.0_count.jpeg"),
     width = 15, height = 20, units = "cm", res = 300)

par(mar = c(4,.5,3,.5))
plot_biostatus_summary_fun(biostatus_data = biostatus_tot, n_width = 20, 
                           col_border = "white",psf_status_col = psf_status_col, 
                           main = paste(sum(biostatus_tot$count),"CUs"), cex = 2)
legend("bottom",legend = psf_status[c(1,4,2,5,3,6)],
       fill = psf_status_col[c(1,4,2,5,3,6)], 
       bty = 'n', ncol = 3, inset = c(0, -.1), xpd = TRUE)

dev.off()


#'* Total biostatus - option 2 *

n_width <- 10

count_max <- max(biostatus_tot$count)
y_max <- ceiling(count_max / n_width)
col_border <- "white"

jpeg(filename = paste0(wd_figures,"/PSE_summary/Biological_status_PSE2.0_wide.jpeg"),
     width = 20, height = 15, units = "cm", res = 300)

layout(matrix(1:length(psf_status), nrow = 1))
par(mar = c(4,.5,1,.5))
for(bs in psf_status){
  # bs <- psf_status[1]
  cond_bs <- biostatus_tot$psf_status == bs
  
  plot_biostatus_summary_fun(biostatus_data = biostatus_tot[cond_bs,], n_width = n_width, 
                             col_border = "black", psf_status_col = psf_status_col, 
                             n_height = y_max, cex = 1)
  mtext(text = bs, side = 1, line = 2)
}

dev.off()

jpeg(filename = paste0(wd_figures,"/PSE_summary/Biological_status_PSE2.0_wide_count.jpeg"),
     width = 20, height = 15, units = "cm", res = 300)

layout(matrix(1:length(psf_status), nrow = 1))
par(mar = c(4,.5,1,.5))
for(bs in psf_status){
  # bs <- psf_status[1]
  cond_bs <- biostatus_tot$psf_status == bs

  plot_biostatus_summary_fun(biostatus_data = biostatus_tot[cond_bs,], n_width = n_width, 
                             col_border = col_border, psf_status_col = psf_status_col, 
                             n_height = y_max + 2, cex = 1)
  mtext(text = bs, side = 1, line = 2)
  text(x = n_width/2, y = 1+ ceiling((biostatus_tot$count[cond_bs]/n_width)), 
       labels = sum(biostatus_tot$count[cond_bs]), cex = 2)
}

dev.off()


#'* Biostatus: regions *

unique(biological_status$region)

regions <- c("Northern Transboundary","Haida Gwaii","Nass","Skeena","Central Coast",
             "Vancouver Island & Mainland Inlets","Fraser","Columbia")

length(regions) # 9

# determine y_max as a function of n_width

n_width <- 10

biostatus_rg <- biological_status %>%
  group_by(region) %>%
  summarise(count = n()) %>%
  arrange(regions)

count_max <- max(biostatus_rg$count)
y_max <- ceiling(count_max / n_width)

jpeg(filename = paste0(wd_figures,"/PSE_summary/Biological_status_PSE2.0_rg.jpeg"),
     width = 15, height = 20, units = "cm", res = 300)

biostatus_rg_c <- NULL

layout(matrix(1:(length(regions)+1),byrow = T, nrow = 3))
for(rg in regions){
  # rg <- regions[2]
  
  cond_rg <- biological_status$region == rg
  
  biostatus_here <- biological_status[cond_rg,] %>%
    group_by(psf_status) %>%
    summarise(count = n()) %>%
    arrange(psf_status)
  
  biostatus_here$percent <- round(biostatus_here$count / sum(biostatus_here$count) * 100,2)
  biostatus_here$region <- rg
  biostatus_rg_c <- rbind(biostatus_rg_c,biostatus_here)
  
  par(mar = c(.5,.5,3,.5))
  
  rg_legend <- rg
  if(rg == "Vancouver Island & Mainland Inlets"){
    rg_legend <- "VIMI"
  }
  plot_biostatus_summary_fun(biostatus_data = biostatus_here, n_width = n_width, 
                             main = rg_legend, n_height = y_max, 
                             col_border = "white", psf_status_col = psf_status_col)
}
plot.new()
legend("center",legend = rev(psf_status), fill = rev(psf_status_col), bty = 'n')

dev.off()


jpeg(filename = paste0(wd_figures,"/PSE_summary/Biological_status_PSE2.0_rg_count.jpeg"),
     width = 15, height = 20, units = "cm", res = 300)

layout(matrix(1:(length(regions)+1),byrow = T, nrow = 3))
for(rg in regions){
  # rg <- regions[2]
  
  cond_rg <- biological_status$region == rg
  
  biostatus_here <- biological_status[cond_rg,] %>%
    group_by(psf_status) %>%
    summarise(count = n()) %>%
    arrange(psf_status)
  
  biostatus_here$percent <- round(biostatus_here$count / sum(biostatus_here$count) * 100,2)
  biostatus_here$region <- rg

  par(mar = c(3,.5,.5,.5))
  
  rg_legend <- rg
  if(rg == "Vancouver Island & Mainland Inlets"){
    rg_legend <- "VIMI"
  }
  plot_biostatus_summary_fun(biostatus_data = biostatus_here, n_width = n_width, 
                             main = "", n_height = y_max + 2, 
                             col_border = "white", psf_status_col = psf_status_col)
  mtext(text = rg_legend, side = 1, line = 1, font = 2)
  text(x = n_width/2, y = 1 + ceiling(sum(biostatus_here$count)/n_width), cex = 2, 
       labels = sum(biostatus_here$count))
}
plot.new()
legend("center",legend = rev(psf_status), fill = rev(psf_status_col), bty = 'n')

dev.off()




n_width <- 5

count_max <- max(biostatus_rg$count)
y_max <- ceiling(count_max / n_width)

# sort regions
regions_sorted <- biostatus_rg$region[order(biostatus_rg$count)]


jpeg(filename = paste0(wd_figures,"/PSE_summary/Biological_status_PSE2.0_rg_count_oneLine.jpeg"),
     width = 25, height = 14, units = "cm", res = 300)

layout(matrix(1:(length(regions)),byrow = T, nrow = 1))
for(rg in regions_sorted){
  # rg <- regions[2]
  
  cond_rg <- biological_status$region == rg
  
  biostatus_here <- biological_status[cond_rg,] %>%
    group_by(psf_status) %>%
    summarise(count = n()) %>%
    arrange(psf_status)
  
  biostatus_here$percent <- round(biostatus_here$count / sum(biostatus_here$count) * 100,2)
  biostatus_here$region <- rg
  
  par(mar = c(.5,3,.5,.5), xpd=TRUE)
  
  rg_legend <- rg
  # if(rg == "Vancouver Island & Mainland Inlets"){
  #   rg_legend <- "VIMI"
  # }
  plot_biostatus_summary_fun(biostatus_data = biostatus_here, n_width = n_width, 
                             main = "", n_height = y_max + 2, 
                             col_border = "white", psf_status_col = psf_status_col)
  mtext(text = rg_legend, side = 2, line = 1, font = 2, adj = 0, cex = 1)
  text(x = n_width/2, y = 1 + ceiling(sum(biostatus_here$count)/n_width), cex = 1.5, 
       labels = sum(biostatus_here$count))
  if(rg == regions_sorted[1]){
    legend("left",legend = rev(psf_status), fill = rev(psf_status_col), bty = 'n', 
           inset = c(-.2,0))
  }
}

dev.off()


biostatus_rg_c <- biostatus_rg_c[,c("region","psf_status","count","percent")]


#'* Biostatus: species *

unique(biological_status$species_name)
biological_status$species <- NA
for(spn in unique(biological_status$species_name)){
  
  species <- spn
  if(spn %in% c("Lake sockeye","River sockeye")){
    species <- "Sockeye"
  }else if(spn %in% c("Pink (odd)","Pink (even)")){
    species <- "Pink"
  }
  cond_spn <- biological_status$species_name == spn
  biological_status$species[cond_spn] <- species
}

species <- unique(biological_status$species)
species <- c("Sockeye","Coho","Chinook","Pink","Chum","Steelhead")

length(species) # 6

# determine y_max as a function of n_width

n_width <- 10

biostatus_sp <- biological_status %>%
  group_by(species) %>%
  summarise(count = n()) %>%
  arrange(species)

count_max <- max(biostatus_sp$count)
y_max <- ceiling(count_max / n_width)

jpeg(filename = paste0(wd_figures,"/PSE_summary/Biological_status_PSE2.0_sp.jpeg"),
     width = 15, height = 20*2/3, units = "cm", res = 300)

biostatus_sp_c <- NULL

layout(matrix(1:length(species),byrow = T, nrow = 2))
for(sp in species){

  cond_sp <- biological_status$species == sp
  
  biostatus_here <- biological_status[cond_sp,] %>%
    group_by(psf_status) %>%
    summarise(count = n()) %>%
    arrange(psf_status)
  
  biostatus_here$percent <- round(biostatus_here$count / sum(biostatus_here$count) * 100,2)
  biostatus_here$species <- sp
  biostatus_sp_c <- rbind(biostatus_sp_c,biostatus_here)
  
  par(mar = c(.5,.5,3,.5))
  plot_biostatus_summary_fun(biostatus_data = biostatus_here, n_width = n_width, 
                             main = sp, n_height = y_max)
}
legend("right",legend = rev(psf_status), fill = rev(psf_status_col), bty = 'n')

dev.off()


jpeg(filename = paste0(wd_figures,"/PSE_summary/Biological_status_PSE2.0_sp_count.jpeg"),
     width = 15, height = 20*2/3, units = "cm", res = 300)

biostatus_sp_c <- NULL

layout(matrix(1:length(species),byrow = T, nrow = 2))
for(sp in species){
  
  cond_sp <- biological_status$species == sp
  
  biostatus_here <- biological_status[cond_sp,] %>%
    group_by(psf_status) %>%
    summarise(count = n()) %>%
    arrange(psf_status)
  
  biostatus_here$percent <- round(biostatus_here$count / sum(biostatus_here$count) * 100,2)
  biostatus_here$species <- sp
  biostatus_sp_c <- rbind(biostatus_sp_c,biostatus_here)
  
  par(mar = c(3,.5,.5,.5))
  plot_biostatus_summary_fun(biostatus_data = biostatus_here, n_width = n_width, 
                             main = "", n_height = y_max + 2.5, col_border = "white")
  mtext(text = sp, side = 1, line = 1, font = 2)
  text(x = n_width/2, y = 1.5 + ceiling(sum(biostatus_here$count)/n_width), cex = 1.5, 
       labels = sum(biostatus_here$count))
}
legend("topright",legend = rev(psf_status), fill = rev(psf_status_col), bty = 'n')

dev.off()


biostatus_sp_c <- biostatus_sp_c[,c("species","psf_status","count","percent")]

#' Biostatus: region & species  


# determine y_max as a function of n_width

n_width <- 10

biostatus_rg_sp <- biological_status %>%
  group_by(region,species) %>%
  summarise(count = n()) %>%
  arrange(region,species)

count_max <- max(biostatus_rg_sp$count)
y_max <- ceiling(count_max / n_width)

side_min <- 0.1

bty <- 'o'

jpeg(filename = paste0(wd_figures,"/PSE_summary/Biological_status_PSE2.0_rg_sp.jpeg"),
     width = 20, height = 16*3/2, units = "cm", res = 300)

biostatus_rg_sp_c <- NULL

layout(matrix(1:(length(regions)*length(species)),byrow = T, nrow = length(regions)),
       widths = c(1.4,rep(1,length(species) - 1)), heights = c(1.4,rep(1,length(regions) - 1)))
for(rg in regions){
  # rg <- regions[1]
  for(sp in species){
    # sp <- species[1]
    cond_rg_sp <- biological_status$region == rg & biological_status$species == sp
    
    side2 <- side3 <- side_min
    main <- ylab <- ""
    if(rg == regions[1]){ # top row
      side3 <- 3
      main <- sp
    }
    if(sp == species[1]){ # left column
      side2 <- 3
      ylab <- rg
      if(rg == "Vancouver Island & Mainland Inlets"){
        ylab <- "VIMI"
      }else if(rg == "Central Coast"){
        ylab <- "CC"
      }else if(rg == "Haida Gwaii"){
        ylab <- "HG"
      }else if(rg == "Northern Transboundary"){
        ylab <- "NT"
      }
    }
    
    par(mar = c(side_min,side2,side3,side_min))
    
    if(!any(cond_rg_sp)){
      plot(1,1, xaxt = 'n', yaxt = 'n', xlab = NA, ylab = NA, bty = 'n', 
           col = "white", bty = bty)
      legend("center",legend = "NA",bty = 'n')
      mtext(text = ylab, side = 2, line = 1, font = 2, cex = 1)
      mtext(text = main, side = 3, line = 1, font = 2, cex = 1)
      
    }else{
      biostatus_here <- biological_status[cond_rg_sp,] %>%
        group_by(psf_status) %>%
        summarise(count = n()) %>%
        arrange(psf_status)
      
      biostatus_here$percent <- round(biostatus_here$count / sum(biostatus_here$count) * 100,2)
      biostatus_here$region <- rg
      biostatus_here$species <- sp
      biostatus_rg_sp_c <- rbind(biostatus_rg_sp_c,biostatus_here)
      
      plot_biostatus_summary_fun(biostatus_data = biostatus_here, n_width = n_width, 
                                 main = main, n_height = y_max, ylab = ylab, bty = bty)
    }
  }
}
legend("right",legend = rev(psf_status), fill = rev(psf_status_col), bty = 'n')

dev.off()

jpeg(filename = paste0(wd_figures,"/PSE_summary/Biological_status_PSE2.0_rg_sp_count.jpeg"),
     width = 20, height = 16*3/2, units = "cm", res = 300)

biostatus_rg_sp_c <- NULL

layout(matrix(1:(length(regions)*length(species)), byrow = T, nrow = length(regions)),
       widths = c(1.3,rep(1,length(species) - 1)), heights = c(1.4,rep(1,length(regions) - 1)))
for(rg in regions){
  # rg <- regions[1]
  for(sp in species){
    # sp <- species[1]
    cond_rg_sp <- biological_status$region == rg & biological_status$species == sp
    
    side2 <- side3 <- side_min
    main <- ylab <- ""
    if(rg == regions[1]){ # top row
      side3 <- 3
      main <- sp
    }
    if(sp == species[1]){ # left column
      side2 <- 3
      ylab <- rg
      if(rg == "Vancouver Island & Mainland Inlets"){
        ylab <- "VIMI"
      }else if(rg == "Central Coast"){
        ylab <- "CC"
      }else if(rg == "Haida Gwaii"){
        ylab <- "HG"
      }else if(rg == "Northern Transboundary"){
        ylab <- "NT"
      }
    }
    
    par(mar = c(side_min,side2,side3,side_min))
    
    if(!any(cond_rg_sp)){
      plot(x = NA, y = NA, ylim = c(0,y_max + 2), xlim = c(0,n_width), bty = bty,
           xaxt = 'n', yaxt = 'n')
      text(x = x <- n_width/2, y =  y_max / 2, cex = 1.3, labels = "0")
      mtext(text = ylab, side = 2, line = 1, font = 2, cex = 1)
      mtext(text = main, side = 3, line = 1, font = 2, cex = 1)
      
    }else{
      biostatus_here <- biological_status[cond_rg_sp,] %>%
        group_by(psf_status) %>%
        summarise(count = n()) %>%
        arrange(psf_status)
      
      biostatus_here$percent <- round(biostatus_here$count / sum(biostatus_here$count) * 100,2)
      biostatus_here$region <- rg
      biostatus_here$species <- sp
      biostatus_rg_sp_c <- rbind(biostatus_rg_sp_c,biostatus_here)
      
      plot_biostatus_summary_fun(biostatus_data = biostatus_here, n_width = n_width, 
                                 main = main, n_height = y_max + 2, ylab = ylab, 
                                 bty = 'n', col_border = "white")
      par(new = T)
      plot(x = NA, y = NA, ylim = c(0,y_max + 2), xlim = c(0,n_width), 
           xaxt = 'n', yaxt = 'n')
      
      if(ceiling(sum(biostatus_here$count)/n_width) < (y_max / 2)){
        y <- y_max / 2
      }else{
        y <- 1 + ceiling(sum(biostatus_here$count)/n_width)
      }
      if(rg == tail(regions,1) & sp == tail(species,1)){
        x <- 1
      }else{
        x <- n_width/2
      }
      text(x = x, y = y, cex = 1.3, labels = sum(biostatus_here$count))
    }
  }
}
legend("right",legend = rev(psf_status), fill = rev(psf_status_col), bty = 'n')

dev.off()

biostatus_rg_sp_c <- biostatus_rg_sp_c[,c("region","species","psf_status","count","percent")]


#' Alternative: 


#'* habitat threats *



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

fromDatabase <- update_file_csv <- F

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
# Same figure as above but for CYCLIC CUs PERCENTILES -----
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
biostatus_101$percentile_status <- biostatus_101$status_percent05

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
                                      cu_name_pse = conservationunits_decoder$cu_name_pse[cond_cuid],
                                      cycle_line = cl)
    
    cond_cuid <- biostatus_101$cuid == cuid
    cond_cl <- biostatus_101$cycle_line == cl
    benchmarks_102_here$cycle_line_dominant <- biostatus_101$cycle_line_dominant[cond_cuid & cond_cl]
    benchmarks_102_here$curr_spw <- biostatus_101$current_spawner_abundance[cond_cuid & cond_cl]

    benchmarks_102_here$sgen <- NA           # lower HBSRM benchmark
    benchmarks_102_here$sgen_lower <- NA     #
    benchmarks_102_here$sgen_upper <- NA     #
    benchmarks_102_here$smsy <- NA           # upper HBSRM benchmark
    benchmarks_102_here$smsy_lower <- NA     #
    benchmarks_102_here$smsy_upper <- NA     #
    
    cond_cuid <- benchmarks$cuid == cuid
    cond_cl <- benchmarks$cycle_line == cl
    cond_25 <- benchmarks$benchmark == "benchmark_0.25"
    cond_50 <- benchmarks$benchmark == "benchmark_0.5"
    benchmarks_102_here$`25%_spw` <- benchmarks$m[cond_cuid & cond_cl & cond_25]
    benchmarks_102_here$`25%_spw_lower` <- benchmarks$CI025[cond_cuid & cond_cl & cond_25]
    benchmarks_102_here$`25%_spw_upper` <- benchmarks$CI975[cond_cuid & cond_cl & cond_25]
    benchmarks_102_here$`75%_spw` <- benchmarks$m[cond_cuid & cond_cl & cond_50]
    benchmarks_102_here$`75%_spw_lower` <- benchmarks$CI025[cond_cuid & cond_cl & cond_50]
    benchmarks_102_here$`75%_spw_upper` <- benchmarks$CI975[cond_cuid & cond_cl & cond_50]
    
    cond_cuid <- biostatus_101$cuid == cuid
    cond_cl <- biostatus_101$cycle_line == cl
    benchmarks_102_here$curr_spw_start_year <- biostatus_101$yr_withData_start[cond_cuid & cond_cl]
    benchmarks_102_here$curr_spw_end_year <- biostatus_101$yr_withData_end[cond_cuid & cond_cl]
    
    
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
  plot_spawnerAbundance_benchmarks_cyclic_percentile_fun(cuid = cuid,
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
# Same figure as above but for CYCLIC CUs RICKER & LARKIN -----
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
biostatus_101 <- rbind_biologicalStatusCSV_fun(pattern = "cyclic_Larkin_biological_status_HBSRM",
                                               wd_output = paste0(wd_output,"/intermediate"), 
                                               region = unique(conservationunits_decoder$region))


biostatus_101_r <- rbind_biologicalStatusCSV_fun(pattern = "cyclic_Ricker_biological_status_HBSRM",
                                               wd_output = paste0(wd_output,"/intermediate"), 
                                               region = unique(conservationunits_decoder$region))

biostatus_101 <- rbind(biostatus_101,biostatus_101_r)

head(biostatus_101)

# Import the cyclic_benchmarks_summary_percentiles files
benchmarks <- rbind_biologicalStatusCSV_fun(pattern = "cyclic_Larkin_benchmarks_summary_HBSRM",
                                            wd_output = paste0(wd_output,"/intermediate"), 
                                            region = unique(conservationunits_decoder$region))

benchmarks_r <- rbind_biologicalStatusCSV_fun(pattern = "cyclic_Ricker_benchmarks_summary_HBSRM",
                                            wd_output = paste0(wd_output,"/intermediate"), 
                                            region = unique(conservationunits_decoder$region))

benchmarks <- rbind(benchmarks,benchmarks_r)

head(benchmarks)

#'* fill biostatus_101 for these CUs because it was not done in 3_biological_status.R *
# add column biostatus for both thresholds (Smsy and 80% Smsy)
colProb <- colnames(biostatus_101)[grepl("Smsy_",colnames(biostatus_101))]
biostatus_101$status_Smsy <- sapply(X = 1:nrow(biostatus_101), 
                                              FUN = function(r){
                                                # r <- 1
                                                slice <- biostatus_101[r,colProb]
                                                # out <- c("red","amber","green")[slice == max(slice)][1] 
                                                out <- c("poor","fair","good")[slice == max(slice)][1] # Katy's request
                                                return(out)
                                              })

colProb <- colnames(biostatus_101)[grepl("Smsy80_",colnames(biostatus_101))]
biostatus_101$status_Smsy80 <- sapply(X = 1:nrow(biostatus_101), 
                                                FUN = function(r){
                                                  # r <- 1
                                                  slice <- biostatus_101[r,colProb]
                                                  # out <- c("red","amber","green")[slice == max(slice)][1]
                                                  out <- c("poor","fair","good")[slice == max(slice)][1] # Katy's request
                                                  return(out)
                                                })

biostatus_101$sr_status <- biostatus_101$status_Smsy80

biostatus_101$percentile_red_prob <- NA
biostatus_101$percentile_yellow_prob <- NA
biostatus_101$percentile_green_prob <- NA
biostatus_101$percentile_status <- NA

biostatus_101$psf_status_code <- NA
for(r in 1:nrow(biostatus_101)){
  if(is.na(biostatus_101$sr_status[r])){
    out <- NA
  }else{
    out <- 1
    if(biostatus_101$sr_status[r] == "fair"){
      out <- 2
    }else if(biostatus_101$sr_status[r] == "poor"){
      out <- 3
    }
  }
  biostatus_101$psf_status_code[r]  <- out
}
biostatus_101$psf_status_code_all <- biostatus_101$psf_status_code
biostatus_101$psf_status_type <- "sr"


#'* fill biological_status_cu for these CUs because it was not done in 3_biological_status.R *

benchmarks_102_cyclic <- NULL
for(cuid in unique(benchmarks$cuid)){
  # cuid <- benchmarks$cuid[1]
  
  for(model in unique(benchmarks$model)){
    # model <- unique(benchmarks$model)[1]
    cond_model_bench <- benchmarks$model == model
    
    for(yr in unique(benchmarks$year[cond_model_bench])){
      # yr <- unique(benchmarks$year[cond_model_bench])[1]
      cond_cuid <- conservationunits_decoder$cuid == cuid
      benchmarks_102_here <- data.frame(region = conservationunits_decoder$region[cond_cuid],
                                        cuid = cuid,
                                        region_abbr = conservationunits_decoder$region_abbr[cond_cuid],
                                        cu_name_pse = conservationunits_decoder$cu_name_pse[cond_cuid],
                                        year = yr)
      
      cond_cuid <- biostatus_101$cuid == cuid
      cond_yr <- biostatus_101$year == yr
      cond_model <- biostatus_101$model == model
      benchmarks_102_here$curr_spw <- biostatus_101$current_spawner_abundance[cond_cuid & cond_yr & cond_model]

      cond_cuid <- benchmarks$cuid == cuid
      cond_yr <- benchmarks$year == yr
      cond_model <- benchmarks$model == model
      cond_Sgen <- benchmarks$benchmark == "Sgen"
      cond_Smsy <- benchmarks$benchmark == "Smsy"
      cond_HPD <- benchmarks$method == "HPD"
      benchmarks_102_here$sgen <- benchmarks$m[cond_cuid & cond_yr & cond_Sgen & cond_HPD & cond_model]        
      benchmarks_102_here$sgen_lower <- benchmarks$CI025[cond_cuid & cond_yr & cond_Sgen & cond_HPD & cond_model] 
      benchmarks_102_here$sgen_upper <- benchmarks$CI975[cond_cuid & cond_yr & cond_Sgen & cond_HPD & cond_model] 
      benchmarks_102_here$smsy <- benchmarks$m[cond_cuid & cond_yr & cond_Smsy & cond_HPD & cond_model] * 0.8
      benchmarks_102_here$smsy_lower <- benchmarks$CI025[cond_cuid & cond_yr & cond_Smsy & cond_HPD & cond_model] * 0.8
      benchmarks_102_here$smsy_upper <- benchmarks$CI975[cond_cuid & cond_yr & cond_Smsy & cond_HPD & cond_model] * 0.8
      
      benchmarks_102_here$`25%_spw` <- NA
      benchmarks_102_here$`25%_spw_lower` <- NA
      benchmarks_102_here$`25%_spw_upper` <- NA
      benchmarks_102_here$`75%_spw` <- NA
      benchmarks_102_here$`75%_spw_lower` <- NA
      benchmarks_102_here$`75%_spw_upper` <- NA
      
      cond_cuid <- biostatus_101$cuid == cuid
      cond_yr <- biostatus_101$year == yr
      cond_model <- biostatus_101$model == model
      benchmarks_102_here$curr_spw_start_year <- biostatus_101$yr_withData_start[cond_cuid & cond_yr & cond_model]
      benchmarks_102_here$curr_spw_end_year <- biostatus_101$yr_withData_end[cond_cuid & cond_yr & cond_model]
      
      benchmarks_102_here$model <- model
      
      if(is.null(benchmarks_102_cyclic)){
        benchmarks_102_cyclic <- benchmarks_102_here
      }else{
        benchmarks_102_cyclic <- rbind(benchmarks_102_cyclic,benchmarks_102_here)
      }
    }
  }
}
head(benchmarks_102_cyclic)

cond_cuid <- biostatus_101$cuid == 731
cond_yr <- biostatus_101$year == 2013
biostatus_101[cond_cuid & cond_yr,]

# Plot the Larkin model for the four last years:
cond_Ricker_bio <- biostatus_101$model == "Ricker"
cond_Ricker_bench <- benchmarks_102_cyclic$model == "Ricker"

log10_scale <- T
figure_print <- T
for(cuid in unique(benchmarks_102_cyclic$cuid)){
  # cuid <- unique(benchmarks_102_cyclic$cuid)[2]
  # cond_cl_current <- benchmarks_102_cyclic$cycle_line_current
  plot_spawnerAbundance_benchmarks_cyclic_Larkin_fun(cuid = cuid,
                                              cuspawnerabundance = spawnerabundance, 
                                              dataset101_biological_status = biostatus_101[!cond_Ricker_bio,], # biological_status_cu, 
                                              dataset102_benchmarks = benchmarks_102_cyclic[!cond_Ricker_bench,],# [cond_cl_current,], # benchmarks_cu, 
                                              #dataset103_output = cuspawnerabund_smooth,
                                              conservationunits_decoder = conservationunits_decoder, 
                                              log10_scale = log10_scale,
                                              figure_print = figure_print, 
                                              wd_figures = wd_figures, 
                                              file_name_nchar = 60)
}


# Plot the Ricker model for the last years:
for(cuid in unique(benchmarks_102_cyclic$cuid)){
  # cuid <- unique(benchmarks_102_cyclic$cuid)[2]
  # cond_cl_current <- benchmarks_102_cyclic$cycle_line_current
  plot_spawnerAbundance_benchmarks_fun(cuid = cuid,
                                                     cuspawnerabundance = spawnerabundance, 
                                                     dataset101_biological_status = biostatus_101[cond_Ricker_bio,], # biological_status_cu, 
                                                     dataset102_benchmarks = benchmarks_102_cyclic[cond_Ricker_bench,],# [cond_cl_current,], # benchmarks_cu, 
                                                     #dataset103_output = cuspawnerabund_smooth,
                                                     conservationunits_decoder = conservationunits_decoder, 
                                                     log10_scale = log10_scale,
                                                     figure_print = figure_print, 
                                                     wd_figures = wd_figures, 
                                                     file_name_nchar = 60)
}

cond_cuid <- biostatus_101$cuid == 738
biostatus_101[cond_cuid & cond_Ricker_bio,]


cond_cuid <- benchmarks_102_cyclic$cuid == 738
benchmarks_102_cyclic[cond_cuid & cond_Ricker_bench,]

#
#
# CYCLIC percentile vs. RICKER ---------
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
biostatus_101_percent <- rbind_biologicalStatusCSV_fun(pattern = "cyclic_biological_status_percentiles",
                                               wd_output = paste0(wd_output,"/intermediate"), 
                                               region = unique(conservationunits_decoder$region))

head(biostatus_101_percent)

# Import the cyclic_benchmarks_summary_percentiles files
benchmarks <- rbind_biologicalStatusCSV_fun(pattern = "cyclic_benchmarks_summary_percentiles",
                                            wd_output = paste0(wd_output,"/intermediate"), 
                                            region = unique(conservationunits_decoder$region))

head(benchmarks)

#'* fill biostatus_101_percent for these CUs because it was not done in 3_biological_status.R *
biostatus_101_percent$percentile_status <- biostatus_101_percent$status_percent05

biostatus_101_percent$sr_red_prob <- NA
biostatus_101_percent$sr_yellow_prob <- NA
biostatus_101_percent$sr_green_prob <- NA
biostatus_101_percent$sr_status <- NA

biostatus_101_percent$psf_status_code <- NA
for(r in 1:nrow(biostatus_101_percent)){
  out <- 1
  if(biostatus_101_percent$percentile_status[r] == "fair"){
    out <- 2
  }else if(biostatus_101_percent$percentile_status[r] == "poor"){
    out <- 3
  }
  biostatus_101_percent$psf_status_code[r]  <- out
}
biostatus_101_percent$psf_status_code_all <- biostatus_101_percent$psf_status_code
biostatus_101_percent$psf_status_type <- "percentile"


#'* fill biological_status_cu for these CUs because it was not done in 3_biological_status.R *

benchmarks_102_percent <- NULL
for(cuid in unique(benchmarks$cuid)){
  # cuid <- benchmarks$cuid[1]
  for(cl in 1:4){
    # cl <- 1
    cond_cuid <- conservationunits_decoder$cuid == cuid
    benchmarks_102_here <- data.frame(region = conservationunits_decoder$region[cond_cuid],
                                      cuid = cuid,
                                      region_abbr = conservationunits_decoder$region_abbr[cond_cuid],
                                      cu_name_pse = conservationunits_decoder$cu_name_pse[cond_cuid],
                                      cycle_line = cl)
    
    cond_cuid <- biostatus_101_percent$cuid == cuid
    cond_cl <- biostatus_101_percent$cycle_line == cl
    benchmarks_102_here$cycle_line_dominant <- biostatus_101_percent$cycle_line_dominant[cond_cuid & cond_cl]
    benchmarks_102_here$curr_spw <- biostatus_101_percent$current_spawner_abundance[cond_cuid & cond_cl]
    
    benchmarks_102_here$sgen <- NA           # lower HBSRM benchmark
    benchmarks_102_here$sgen_lower <- NA     #
    benchmarks_102_here$sgen_upper <- NA     #
    benchmarks_102_here$smsy <- NA           # upper HBSRM benchmark
    benchmarks_102_here$smsy_lower <- NA     #
    benchmarks_102_here$smsy_upper <- NA     #
    
    cond_cuid <- benchmarks$cuid == cuid
    cond_cl <- benchmarks$cycle_line == cl
    cond_25 <- benchmarks$benchmark == "benchmark_0.25"
    cond_50 <- benchmarks$benchmark == "benchmark_0.5"
    benchmarks_102_here$`25%_spw` <- benchmarks$m[cond_cuid & cond_cl & cond_25]
    benchmarks_102_here$`25%_spw_lower` <- benchmarks$CI025[cond_cuid & cond_cl & cond_25]
    benchmarks_102_here$`25%_spw_upper` <- benchmarks$CI975[cond_cuid & cond_cl & cond_25]
    benchmarks_102_here$`75%_spw` <- benchmarks$m[cond_cuid & cond_cl & cond_50]
    benchmarks_102_here$`75%_spw_lower` <- benchmarks$CI025[cond_cuid & cond_cl & cond_50]
    benchmarks_102_here$`75%_spw_upper` <- benchmarks$CI975[cond_cuid & cond_cl & cond_50]
    
    cond_cuid <- biostatus_101_percent$cuid == cuid
    cond_cl <- biostatus_101_percent$cycle_line == cl
    benchmarks_102_here$curr_spw_start_year <- biostatus_101_percent$yr_withData_start[cond_cuid & cond_cl]
    benchmarks_102_here$curr_spw_end_year <- biostatus_101_percent$yr_withData_end[cond_cuid & cond_cl]
    
    
    if(is.null(benchmarks_102_percent)){
      benchmarks_102_percent <- benchmarks_102_here
    }else{
      benchmarks_102_percent <- rbind(benchmarks_102_percent,benchmarks_102_here)
    }
  }
}



biostatus_101_Ricker <- rbind_biologicalStatusCSV_fun(pattern = "cyclic_Ricker_biological_status_HBSRM",
                                                 wd_output = paste0(wd_output,"/intermediate"), 
                                                 region = unique(conservationunits_decoder$region))

head(biostatus_101_Ricker)

# Import the cyclic_benchmarks_summary_percentiles files
benchmarks <- rbind_biologicalStatusCSV_fun(pattern = "cyclic_Ricker_benchmarks_summary_HBSRM",
                                              wd_output = paste0(wd_output,"/intermediate"), 
                                              region = unique(conservationunits_decoder$region))

head(benchmarks)

#'* fill biostatus_101_Ricker for these CUs because it was not done in 3_biological_status.R *
# add column biostatus for both thresholds (Smsy and 80% Smsy)
colProb <- colnames(biostatus_101_Ricker)[grepl("Smsy_",colnames(biostatus_101_Ricker))]
biostatus_101_Ricker$status_Smsy <- sapply(X = 1:nrow(biostatus_101_Ricker), 
                                    FUN = function(r){
                                      # r <- 1
                                      slice <- biostatus_101_Ricker[r,colProb]
                                      # out <- c("red","amber","green")[slice == max(slice)][1] 
                                      out <- c("poor","fair","good")[slice == max(slice)][1] # Katy's request
                                      return(out)
                                    })

colProb <- colnames(biostatus_101_Ricker)[grepl("Smsy80_",colnames(biostatus_101_Ricker))]
biostatus_101_Ricker$status_Smsy80 <- sapply(X = 1:nrow(biostatus_101_Ricker), 
                                      FUN = function(r){
                                        # r <- 1
                                        slice <- biostatus_101_Ricker[r,colProb]
                                        # out <- c("red","amber","green")[slice == max(slice)][1]
                                        out <- c("poor","fair","good")[slice == max(slice)][1] # Katy's request
                                        return(out)
                                      })

biostatus_101_Ricker$sr_status <- biostatus_101_Ricker$status_Smsy80

biostatus_101_Ricker$percentile_red_prob <- NA
biostatus_101_Ricker$percentile_yellow_prob <- NA
biostatus_101_Ricker$percentile_green_prob <- NA
biostatus_101_Ricker$percentile_status <- NA

biostatus_101_Ricker$psf_status_code <- NA
for(r in 1:nrow(biostatus_101_Ricker)){
  if(is.na(biostatus_101_Ricker$sr_status[r])){
    out <- NA
  }else{
    out <- 1
    if(biostatus_101_Ricker$sr_status[r] == "fair"){
      out <- 2
    }else if(biostatus_101_Ricker$sr_status[r] == "poor"){
      out <- 3
    }
  }
  biostatus_101_Ricker$psf_status_code[r]  <- out
}
biostatus_101_Ricker$psf_status_code_all <- biostatus_101_Ricker$psf_status_code
biostatus_101_Ricker$psf_status_type <- "sr"


#'* fill biological_status_cu for these CUs because it was not done in 3_biological_status.R *

benchmarks_102_Ricker <- NULL
for(cuid in unique(benchmarks$cuid)){
  # cuid <- benchmarks$cuid[1]
  
  for(model in unique(benchmarks$model)){
    # model <- unique(benchmarks$model)[1]
    cond_model_bench <- benchmarks$model == model
    
    for(yr in unique(benchmarks$year[cond_model_bench])){
      # yr <- unique(benchmarks$year[cond_model_bench])[1]
      cond_cuid <- conservationunits_decoder$cuid == cuid
      benchmarks_102_here <- data.frame(region = conservationunits_decoder$region[cond_cuid],
                                        cuid = cuid,
                                        region_abbr = conservationunits_decoder$region_abbr[cond_cuid],
                                        cu_name_pse = conservationunits_decoder$cu_name_pse[cond_cuid],
                                        year = yr)
      
      cond_cuid <- biostatus_101_Ricker$cuid == cuid
      cond_yr <- biostatus_101_Ricker$year == yr
      cond_model <- biostatus_101_Ricker$model == model
      benchmarks_102_here$curr_spw <- biostatus_101_Ricker$current_spawner_abundance[cond_cuid & cond_yr & cond_model]
      
      cond_cuid <- benchmarks$cuid == cuid
      cond_yr <- benchmarks$year == yr
      cond_model <- benchmarks$model == model
      cond_Sgen <- benchmarks$benchmark == "Sgen"
      cond_Smsy <- benchmarks$benchmark == "Smsy"
      cond_HPD <- benchmarks$method == "HPD"
      benchmarks_102_here$sgen <- benchmarks$m[cond_cuid & cond_yr & cond_Sgen & cond_HPD & cond_model]        
      benchmarks_102_here$sgen_lower <- benchmarks$CI025[cond_cuid & cond_yr & cond_Sgen & cond_HPD & cond_model] 
      benchmarks_102_here$sgen_upper <- benchmarks$CI975[cond_cuid & cond_yr & cond_Sgen & cond_HPD & cond_model] 
      benchmarks_102_here$smsy <- benchmarks$m[cond_cuid & cond_yr & cond_Smsy & cond_HPD & cond_model] * 0.8
      benchmarks_102_here$smsy_lower <- benchmarks$CI025[cond_cuid & cond_yr & cond_Smsy & cond_HPD & cond_model] * 0.8
      benchmarks_102_here$smsy_upper <- benchmarks$CI975[cond_cuid & cond_yr & cond_Smsy & cond_HPD & cond_model] * 0.8
      
      benchmarks_102_here$`25%_spw` <- NA
      benchmarks_102_here$`25%_spw_lower` <- NA
      benchmarks_102_here$`25%_spw_upper` <- NA
      benchmarks_102_here$`75%_spw` <- NA
      benchmarks_102_here$`75%_spw_lower` <- NA
      benchmarks_102_here$`75%_spw_upper` <- NA
      
      cond_cuid <- biostatus_101_Ricker$cuid == cuid
      cond_yr <- biostatus_101_Ricker$year == yr
      cond_model <- biostatus_101_Ricker$model == model
      benchmarks_102_here$curr_spw_start_year <- biostatus_101_Ricker$yr_withData_start[cond_cuid & cond_yr & cond_model]
      benchmarks_102_here$curr_spw_end_year <- biostatus_101_Ricker$yr_withData_end[cond_cuid & cond_yr & cond_model]
      
      benchmarks_102_here$model <- model
      
      if(is.null(benchmarks_102_Ricker)){
        benchmarks_102_Ricker <- benchmarks_102_here
      }else{
        benchmarks_102_Ricker <- rbind(benchmarks_102_Ricker,benchmarks_102_here)
      }
    }
  }
}


biostatus_101_percent$status_percent05
biostatus_101_Ricker$status_Smsy80

table <- data.frame(cu_name_pse = biostatus_101_Ricker$CU_pse)
table$Ricker <- biostatus_101_Ricker$status_Smsy80

table$percent_2022 <- sapply(table$cu_name_pse,function(cu){
  cond <- biostatus_101_percent$CU_pse == cu & biostatus_101_percent$yr_withData_end == 2022
  return(biostatus_101_percent$status_percent05[cond])
})






biostatus_101_percent$CU[cond_yr]












# 
# CYCLIC LARKIN ALPHA analysis -------

# Import the Larkin's alpha (or "a") and alpha' (or "a'") as in the model:
# log(Rt/St) = a - b*St - b1*St-1 - b2*St-2 - b3*St-3 
# log(Rt/St) = a' - b*St
alpha_Larkin <- import_mostRecent_file_fun(wd = paste0(wd_output,"/intermediate"), 
                                           pattern = "cyclic_Larkin_alphas_HBSRM")

# Import the corresponding benchmark values
benchmarks_Larkin <- import_mostRecent_file_fun(wd = paste0(wd_output,"/intermediate"), 
                                                pattern = "cyclic_Larkin_benchmarks_summary_HBSRM")

# Import the convergence diagnostic:
convDiagnostic_Larking <- import_mostRecent_file_fun(wd = paste0(wd_output,"/intermediate"), 
                                                     pattern = "cyclic_Larkin_DIC_HBSRM_convDiagnostic")


cuid <- unique(benchmarks_Larkin$cuid)

x_offset <- .1

m <- matrix(data = NA, nrow = length(cuid) * 2, ncol = 2)
r <- 1
count <- 1
for(c in cuid){
  m[r,] <- count
  m[r + 1,] <- c(count + 1, count + 2)
  r <- r + 2
  count <- count + 3
}

jpeg(paste0(wd_figures,"/Fraser_SE_cyclic_Larkin_alphas_comparisons.jpg"),
     width = 15, height = 30, units = "cm", res = 300)
layout(m, heights = c(rep(c(1,4),length(cuid) - 1),1,5))
for(c in cuid){
  # c <- cuid[1]
  
  # Benchmarks info
  cond_c <- benchmarks_Larkin$cuid == c
  
  region <- unique(benchmarks_Larkin$region[cond_c])
  species <- unique(benchmarks_Larkin$species[cond_c])
  cu_name_pse <- unique(benchmarks_Larkin$cu_name_pse[cond_c])
  model <- unique(benchmarks_Larkin$model[cond_c])
  
  cond_HPD <- benchmarks_Larkin$method == "HPD"
  cond_Sgen <- benchmarks_Larkin$benchmark == "Sgen"
  cond_Smsy <- benchmarks_Larkin$benchmark == "Smsy"
  
  yr <- benchmarks_Larkin$year[cond_c & cond_HPD & cond_Sgen]
  order_yr <- order(yr)
  Sgen <- benchmarks_Larkin$m[cond_c & cond_HPD & cond_Sgen]
  Smsy <- benchmarks_Larkin$m[cond_c & cond_HPD & cond_Smsy]
  Sgen_CIL <- benchmarks_Larkin$CI025[cond_c & cond_HPD & cond_Sgen]
  Sgen_CIU <- benchmarks_Larkin$CI975[cond_c & cond_HPD & cond_Sgen]
  Smsy_CIL <- benchmarks_Larkin$CI025[cond_c & cond_HPD & cond_Smsy]
  Smsy_CIU <- benchmarks_Larkin$CI975[cond_c & cond_HPD & cond_Smsy]
  # ymin <- min(c(Sgen_CIL,Smsy_CIL))
  ymax <- max(c(Sgen_CIU,Smsy_CIU))
  xlim <- c(min(yr) - .2, max(yr) + .2)
  
  # Convergence info
  cond_c <- convDiagnostic_Larking$cuid == c
  cond_model <- convDiagnostic_Larking$model == model
  convDiagnostic_Larking[cond_c & cond_model,]
  
  # Plot info CU
  par(mar = rep(0,4))
  plot.new()
  legend("center",legend = paste(region,species,cu_name_pse,c,model, sep = " - "), 
         bty = 'n')
  
  side1 <- 2
  if(which(c == cuid) == length(cuid)){
    side1 <- 4.5
  }
  
  par(mar = c(side1,4,.5,.5))
  
  plot(x = yr[order_yr] - x_offset, y = Sgen[order_yr], 
       ylim = c(0,ymax), xlim = xlim, ylab = "Spawner", xlab = "",
       pch = 16, col = status_cols["red"], xaxt = "n")
  axis(side = 1, at = yr[order_yr], labels = yr[order_yr])
  points(x = yr[order_yr] + x_offset, y = Smsy[order_yr], 
         pch = 16, col = status_cols["green"])
  segments(x0 = yr[order_yr] - x_offset, x1 = yr[order_yr] - x_offset, 
           y0 = Sgen_CIL[order_yr], y1 = Sgen_CIU[order_yr], 
           col =  status_cols["red"], lwd = 1.5)
  segments(x0 = yr[order_yr] + x_offset, x1 = yr[order_yr] + x_offset, 
           y0 = Smsy_CIL[order_yr], y1 = Smsy_CIU[order_yr], 
           col =  status_cols["green"], lwd = 1.5)
  
  if(which(c == cuid) == length(cuid)){
    mtext(text = "Year",side = 1, line = 2.5, cex = .8)
  }
  
  # Plot a and alpha
  cond_c <- alpha_Larkin$cuid == c
  yr <- alpha_Larkin$year[cond_c]
  order_yr <- order(yr)
  alpha <- alpha_Larkin$a_median[cond_c]
  alpha_prime <- alpha_Larkin$a_Larkin_median[cond_c]
  alpha_CIL <- alpha_Larkin$a_CI025[cond_c]
  alpha_CIU <- alpha_Larkin$a_CI975[cond_c]
  alpha_prime_CIL <- alpha_Larkin$a_Larkin_CI025[cond_c]
  alpha_prime_CIU <- alpha_Larkin$a_Larkin_CI975[cond_c]
  
  ymax <- max(c(alpha_CIU,alpha_prime_CIU))
  ymin <- min(c(0,alpha_CIL,alpha_prime_CIL))
  
  plot(x = yr[order_yr] - x_offset, y = alpha[order_yr], 
       ylim = c(ymin,ymax), xlim = xlim, pch = 16, xaxt = "n",
       ylab = "Spawner/year", xlab = "",)
  axis(side = 1, at = yr[order_yr], labels = yr[order_yr])
  points(x = yr[order_yr] + x_offset, y = alpha_prime[order_yr], 
         pch = 16, col = "blueviolet")
  segments(x0 = yr[order_yr] - x_offset, x1 = yr[order_yr] - x_offset, 
           y0 = alpha_CIL[order_yr], y1 = alpha_CIU[order_yr], lwd = 1.5)
  segments(x0 = yr[order_yr] + x_offset, x1 = yr[order_yr] + x_offset, 
           y0 = alpha_prime_CIL[order_yr], y1 = alpha_prime_CIU[order_yr], 
           col =  "blueviolet", lwd = 1.5)
  abline(h = 0, lty = 2)
  
  if(which(c == cuid) == length(cuid)){
    mtext(text = "Year",side = 1, line = 2.5, cex = .8)
  }
}
dev.off()

#
# Effect of changing rule 1 (at least one data point in most recent generation) on biostatus -----
#

dataset101_biological_status <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                                           pattern = "dataset101_biological_status")

dataset101_biological_status$psf_status_code_all |> unique()

# select CU with biostatus assessed
cond_123 <- dataset101_biological_status$psf_status_code %in% 1:3
cond_8 <- dataset101_biological_status$psf_status_code_all == "8"    # we want the CUs that were discarded because of rule 8 only

dataset101_biological_status <- dataset101_biological_status[cond_123 | cond_8,]
nrow(dataset101_biological_status) # 186


datasetsNames_database <- datasetsNames_database_fun()
fromDatabase <- update_file_csv <- F
spawnerabundance <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[2],
                                          fromDatabase = fromDatabase,
                                          update_file_csv = update_file_csv,
                                          wd = wd_pop_indic_data_input_dropbox)

# Number of CUs with biostatus using generation length:
counts <- c(sum(cond_123))
names(counts) <- "gen length *"

yr_cut <- 2:10
for(yr in yr_cut){
  # yr <- 4
  
  count_here <- 0
  for(r in 1:nrow(dataset101_biological_status)){
    # r <- 1
    cuid <- dataset101_biological_status$cuid[r]
    region <- dataset101_biological_status$region[r]
    
    # find the last year of data for the CU
    cond <- spawnerabundance$cuid == cuid
    cond_NAno <- !is.na(spawnerabundance$estimated_count)
    year_data_last <- max(spawnerabundance$year[cond & cond_NAno])
    
    # find the last year of data for the region
    cond <- spawnerabundance$region == region
    year_data_last_rg <- max(spawnerabundance$year[cond], na.rm = T)
    
    if((year_data_last_rg - year_data_last + 1) <= yr){
      count_here <- count_here + 1
    }
  }
  names(count_here) <- yr
  counts <- c(counts,count_here)
}

counts

wd_PSAC_figures <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/PSAC/Meetings/Meeting 14/figures")

jpeg(paste0(wd_PSAC_figures,"/nb_yr_cutoff_rule1_vs_biostatus.jpg"), 
     width = 20, height = 12, units = "cm", res = 300)

par(mar = c(5,5,1.5,.5))
barplot(height = counts,col = c("cadetblue4",rep("grey60",length(yr_cut))), las = 1,
        xlab = "Year", ylab = "Number of CUs with biological status",
        ylim = c(0,200))
abline(a = counts["gen length *"],b = 0, lwd = 1.5, lty = 2)

dev.off()


#
# Compare biostatus by changing year cut off current spawner abundance WRONG ----
#

dataset101_biological_status <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                                           pattern = "dataset101_biological_status")

dataset102_benchmarks <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"), 
                                                    pattern = "dataset102_benchmarks")


#' Import the cuspawnerabundance.csv
datasetsNames_database <- datasetsNames_database_fun()
fromDatabase <- update_file_csv <- F
spawnerabundance <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[2],
                                          fromDatabase = fromDatabase,
                                          update_file_csv = update_file_csv,
                                          wd = wd_pop_indic_data_input_dropbox)

conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                          fromDatabase = fromDatabase,
                                          update_file_csv = update_file_csv,
                                          wd = wd_pop_indic_data_input_dropbox)

# select CU with biostatus assessed
cond <- dataset101_biological_status$psf_status_code %in% 1:3
dataset101_biological_status <- dataset101_biological_status[cond,]
nrow(dataset101_biological_status) # 143

cond <- dataset102_benchmarks$cuid %in% dataset101_biological_status$cuid
dataset102_benchmarks <- dataset102_benchmarks[cond,]
nrow(dataset102_benchmarks) # 143

# add currwent spawner abundance to dataset101_biological_status
dataset101_biological_status$curr_spw <- sapply(dataset101_biological_status$cuid,
                                                function(cuid){
                                                  cond <- dataset102_benchmarks$cuid == cuid
                                                  return(dataset102_benchmarks$curr_spw[cond])
                                                })
yr_length <- 2:7

for(yr in yr_length){
  dataset101_biological_status$X <- NA
  cond <- colnames(dataset101_biological_status) == "X"
  colnames(dataset101_biological_status)[cond] <- paste0("psf_status_",yr,"yr")
}

# fill the dataset per region then species because the HBSRM benchmark posterior
# parameter distributions are grouped by region > species
for(region in unique(dataset101_biological_status$region)){
  # region <- unique(dataset101_biological_status$region)[2]
  regionName <- region
  if(region == "Vancouver Island & Mainland Inlets"){
    regionName <- "VIMI"
  }
  regionName <- gsub(" ","_",regionName)
  cond_rg <- dataset101_biological_status$region == region
  
  for(species_name in unique(dataset101_biological_status$species_name[cond_rg])){
    # species_name <- unique(dataset101_biological_status$species_name[cond_rg])[1]
    cond_rg_sp <- cond_rg & dataset101_biological_status$species_name == species_name
    species_abbr <- dataset101_biological_status$species_abbr[cond_rg_sp] |> unique()
    species_acro <- species_abbr
    if(species_abbr %in% c("SEL","SER")){
      species_acro <- "SX"
    }else if(species_abbr %in% c("PKE","PKO")){
      species_acro <- "PK"
    }
    
    # return the posterio distribution of the HBSRM parameters if available
    if(any(dataset101_biological_status$psf_status_type[cond_rg_sp] == "sr")){
      
      # Import the posterior distributions of the model parameters
      post <- readRDS(paste0(wd_output,"/intermediate/",regionName,"_",species_acro,
                             "_HBSRM_posteriors_priorShift.rds"))
      
      # Import the S and R matrices used for fitting the HBSR model (to get to order of the CUs in post)
      SRm <- readRDS(paste0(wd_output,"/intermediate/",regionName,"_",species_acro,
                            "_SR_matrices.rds"))
      
      CUs <- colnames(SRm$R)
      nCUs <-length(CUs)
      
      # nb of chains
      nchains <- length(post) # 6 chains
      
      # parameter names
      pnames <- colnames(post[[1]])
      
      # Unlist different chains of the posterior
      # 6 chains x nb iteration (?) x nb parameters
      post.arr <- array(
        data = NA, 
        dim = c(nchains, nrow(post[[1]]), ncol(post[[1]])), 
        dimnames = list(paste0("chain", 1:length(post)), NULL, pnames))
      
      for(i in 1:length(post)){  # for each chain
        post.arr[i, , ] <- post[[i]]
      }
      
      # Calculate benchmarks for all mcmc draws to account for correlation between a and b
      # nb CUs x nb different parameters (i.e., 5) x nb chains x nb iterations
      SR_bench <- array(
        data = NA,
        dim = c(nCUs, 5, length(post), nrow(post[[1]])),
        dimnames = list(CUs, 
                        c("a", "b", "sig", "Smsy", "Sgen"), 
                        paste0("chain", 1:length(post)), 
                        NULL))
      
      for(i in 1:nCUs){
        if(nCUs == 1){
          SR_bench[i, "a", , ] <- post.arr[, , which(pnames == "a")]      # matrix nb chains x nb mcmc draws --> all the values for that parameter
          SR_bench[i, "b", , ] <- post.arr[, , which(pnames == "b")]
          SR_bench[i, "sig", , ] <- post.arr[, , which(pnames == "sd")]   # sigma_bi
        }else{
          SR_bench[i, "a", , ] <- post.arr[, , which(pnames == paste0("a[", i, "]"))]
          SR_bench[i, "b", , ] <- post.arr[, , which(pnames == paste0("b[", i, "]"))]
          SR_bench[i, "sig", , ] <- post.arr[, , which(pnames == paste0("sd[", i, "]"))]
        }
      }
      
      # Calculate Smsy & Sgen (this takes a few mins...think of vectorizing/parallelizing)
      # Uses 1_functions.R which is different from previous versions by estimating Smsy
      # directly using the methods of Scheuerell (2016).
      for(i in 1:nCUs){
        # i <- 1
        # i <- 3   # issue with Sgen in Fraser CO CU nb 3
        for(j in 1:length(post)){ # for each chain
          # j <- 1
          # Smsy (function can handle vectors)
          SR_bench[i, "Smsy", j, ] <- calcSmsy(a = SR_bench[i, "a", j, ], 
                                               b = SR_bench[i, "b", j, ])
          
          # Sgen (function not currently set up to handle vectors..think of updating this)
          for(k in 1:nrow(post[[1]])){   # for each mcmc draw
            # k <- 1
            SR_bench[i, "Sgen", j, k] <- calcSgen(
              Sgen.hat = 0.5 * SR_bench[i, "Smsy", j, k], 
              theta = c(
                a = SR_bench[i, "a", j, k], 
                b = SR_bench[i, "b", j, k],
                sig = SR_bench[i, "sig", j, k]),
              Smsy = SR_bench[i, "Smsy", j, k])
          }
        }
      }
    }
    
    for(cuid in unique(dataset101_biological_status[cond_rg_sp,]$cuid)){
      # cuid <- unique(dataset101_biological_status$cuid[cond_rg_sp])[1]
      cond_rg_sp_cu <- cond_rg_sp & dataset101_biological_status$cuid == cuid
      cu_name_pse <- dataset101_biological_status$cu_name_pse[cond_rg_sp_cu]
      
      # calculate the current spawner abundanc for year length in yr_length then 
      # corresponding biostatus
      for(yr_l in yr_length){
        # yr_l <- yr_length[1]
        
        # calculate current spawner abundance 
        csa <- current_spawner_abundance_fun(cuids = cuid, 
                                             cuspawnerabundance = spawnerabundance, 
                                             yearCurrentAbundance = NA, 
                                             CU_genLength = yr_l)
        
        if(dataset101_biological_status$psf_status_type[cond_rg_sp_cu] == "percentile"){
          
          cond_cuid <- dataset102_benchmarks$cuid == dataset101_biological_status$cuid[cond_rg_sp_cu]
          
          bench_up <- dataset102_benchmarks$X75._spw[cond_cuid]  # this is 50% upper bench
          bench_low <- dataset102_benchmarks$X25._spw[cond_cuid]
          
          if(csa$curr_spw_abun <= bench_low){
            psf_status_here <- "poor"
          }else if(csa$curr_spw_abun <= bench_up){
            psf_status_here <- "fair"
          }else{
            psf_status_here <- "good"
          }
          
        }else if(dataset101_biological_status$psf_status_type[cond_rg_sp_cu] == "sr"){
          
          i <- which(cu_name_pse == colnames(SRm$R))
          
          status_Smsy <- status_Smsy80 <- c()
          for(j in 1:length(post)){ # for each chain
            # j <- 1
            for(k in 1:nrow(post[[1]])){   # for each mcmc draw
              # k <- 1
              LB_Sgen <- SR_bench[i, "Sgen", j, k]    # i corresponds to the CU
              UB_Smsy <- SR_bench[i, "Smsy", j, k]
              UB_Smsy80 <- UB_Smsy * .8
              
              if(!is.na(LB_Sgen) & !is.na(UB_Smsy)){
                if(csa$curr_spw_abun <= LB_Sgen){
                  #status_Smsy <- c(status_Smsy,'red')
                  status_Smsy80 <- c(status_Smsy80,"red")
                }else if(csa$curr_spw_abun <= UB_Smsy80){
                  #status_Smsy <- c(status_Smsy,'amber')
                  status_Smsy80 <- c(status_Smsy80,"amber")
                }else if(csa$curr_spw_abun <= UB_Smsy){
                  #status_Smsy <- c(status_Smsy,'amber')
                  status_Smsy80 <- c(status_Smsy80,"green")
                }else{
                  #status_Smsy <- c(status_Smsy,'green')
                  status_Smsy80 <- c(status_Smsy80,"green")
                }
              }else{
                #status_Smsy <- c(status_Smsy,NA)
                status_Smsy80 <- c(status_Smsy80,NA)
              }
            }
          }
          #status_Smsy <- status_Smsy[!is.na(status_Smsy)]
          status_Smsy80 <- status_Smsy80[!is.na(status_Smsy80)]
          
          #status_Smsy_prob <- round(table(factor(status_Smsy,levels = c("red","amber","green")))/length(status_Smsy)*100,4)
          status_Smsy80_prob <- round(table(factor(status_Smsy80,levels = c("red","amber","green")))/length(status_Smsy80)*100,4)
          
          psf_status_here <- c("poor","fair","good")[status_Smsy80_prob == max(status_Smsy80_prob)]
          
        }
        col_here <- paste0("psf_status_",yr_l,"yr")
        dataset101_biological_status[cond_rg_sp_cu,col_here] <- psf_status_here
      }
    }
  }
  print(paste("Region",region,"is done."))
}

View(dataset101_biological_status)

dataset101_biological_status

write.csv(dataset101_biological_status,
          paste0(wd_output,"/archive/dataset101_biological_status_compare_curr_spawn_lengths_",Sys.Date(),".csv"),
          row.names = F)

dataset101_biological_status <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"),
                                                           pattern = "dataset101_biological_status_compare_curr_spawn_lengths")

# Proportion of CUs with a different biostatus for each yr_length
# Remove the CUs when value corresponds to their generation length (make sure value)
# match

dataset101_biological_status$gen_length <- sapply(dataset101_biological_status$cuid, 
                                                  function(cuid){
                                                    cond <- conservationunits_decoder$cuid == cuid
                                                    genlength <- conservationunits_decoder$gen_length[cond]
                                                    return(genlength)
                                                  })

cu_biostatus_diff <- data.frame(yr_length = yr_length)
cu_biostatus_diff$nb_cus_tot <- NA
cu_biostatus_diff$nb_cus_diff <- NA
cu_biostatus_diff$nb_cus_improve <- NA
cu_biostatus_diff$nb_cus_worsen <- NA
for(r in 1:nrow(cu_biostatus_diff)){
  # r <- 1
  yr <- cu_biostatus_diff$yr_length[r]
  colhere <- paste0("psf_status_",yr,"yr")
  
  # CUs with corresponding gen length
  cond_genLength_diff <- dataset101_biological_status$gen_length != yr
  datahere <- dataset101_biological_status[cond_genLength_diff,]
  
  cu_biostatus_diff$nb_cus_tot[r] <- nrow(datahere)
  cu_biostatus_diff$nb_cus_diff[r] <- sum(datahere[,"psf_status"] != datahere[,colhere])
  
  cond_improve <- apply(datahere,1,function(row){
    out <- (row["psf_status"] %in% c("poor") & row[colhere] %in% c("fair","good")) |
      (row["psf_status"] %in% c("fair") & row[colhere] %in% c("good"))
    return(out)
  })
  
  cond_worsen <- apply(datahere,1,function(row){
    out <- (row["psf_status"] %in% c("good","fair") & row[colhere] %in% c("poor")) |
      (row["psf_status"] %in% c("good") & row[colhere] %in% c("fair"))
    return(out)
  })
  
  cu_biostatus_diff$nb_cus_improve[r] <- sum(cond_improve)
  cu_biostatus_diff$nb_cus_worsen[r] <- sum(cond_worsen)
  
  # check
  if(any(!cond_genLength_diff)){
    datacheck <- dataset101_biological_status[!cond_genLength_diff,]
    out <- sum(datacheck[,"psf_status"] != datacheck[,colhere]) / nrow(datacheck) * 100
    if(out > 0){
      print(paste0("There are differences here for genlength = ",yr))
      rows <- which(datacheck[,"psf_status"] != datacheck[,colhere])
      print(datacheck[rows,c("region","species_name","cu_name_pse","cuid","psf_status_type","psf_status",colhere)])
      print("***")
    }
  }
}

cu_biostatus_diff$nb_cus_diff_prop <- round(cu_biostatus_diff$nb_cus_diff / cu_biostatus_diff$nb_cus_tot,2)
cu_biostatus_diff$nb_cus_worsen_prop <- round(cu_biostatus_diff$nb_cus_worsen / cu_biostatus_diff$nb_cus_tot,2)
cu_biostatus_diff$nb_cus_improve_prop <- round(cu_biostatus_diff$nb_cus_improve / cu_biostatus_diff$nb_cus_tot,2)

ymax <- apply(cu_biostatus_diff[,c("nb_cus_diff_prop","nb_cus_worsen_prop","nb_cus_improve_prop")],2, function(c){max(c)})
ymax <- max(ymax)

plot(x = cu_biostatus_diff$yr_length, y = cu_biostatus_diff$nb_cus_diff_prop, 
     type = "l", xlab = "Year", ylab = "Proportion of CUs with different biostatus", 
     main = "Different year length --> current spawn abund --> biostatus", las = 1, 
     ylim = c(0,ymax), lwd = 2)
points(x = cu_biostatus_diff$yr_length, y = cu_biostatus_diff$nb_cus_diff_prop, pch = 16)

lines(x = cu_biostatus_diff$yr_length, y = cu_biostatus_diff$nb_cus_improve_prop, col = "blue", lwd = 2)
points(x = cu_biostatus_diff$yr_length, y = cu_biostatus_diff$nb_cus_improve_prop,  col = "blue", pch = 16)
lines(x = cu_biostatus_diff$yr_length, y = cu_biostatus_diff$nb_cus_worsen_prop, col = "red", lwd = 2)
points(x = cu_biostatus_diff$yr_length, y = cu_biostatus_diff$nb_cus_worsen_prop,  col = "red", pch = 16)
legend("topright",c("total","improved","worsened"), lwd = 2, col = c("black","blue","red"), bty = "n")

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
