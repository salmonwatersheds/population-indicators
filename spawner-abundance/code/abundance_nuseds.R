
#'******************************************************************************
#' The goal of the script is to 
#' 
#' Previous script: Fraser_salmon_CU_updates.Rmd
#' 
#' 
#' Files imported (from dropbox):
#' - dataset_1part2_DATE.csv      # = streamspawnersurveys_output.csv in DB
#' 
#' Files produced: 
#' - dataset_1part1_DATE.csv      # = dataset1cu_output.csv in DB ; columns 'estimated_count' and 'total_run' are missing 
#'  
#' Note: code taken from 
#' Transboundary/Data & Assessments/transboundary-data/code/4_pse-spawner-abundance.R
#' https://www.dropbox.com/scl/fi/pt80lerubav9r83uh4vtr/4_pse-spawner-abundance.R?rlkey=e723ohin2r1k5vpr0in80lvxy&dl=0
#' 
#' Example of outputed dataset:
#' https://www.dropbox.com/scl/fi/b9jkohs2wixv48ua78r1w/spawner_abundance_dataset_1part1_2024-03-20.csv?rlkey=dpz1vykjx8c2wezidhl5tkhto&dl=0
#'******************************************************************************

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

wds_l <- set_working_directories_fun(subDir = subDir_projects$spawner_abundance,
                                     Export_locally = F)
wd_head <- wds_l$wd_head
wd_project <- wds_l$wd_project
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS

wd_output_sp_surveys <- gsub("spawner-abundance","spawner-surveys",wd_output)

wd_data_dropbox <- paste(wd_X_Drive1_PROJECTS,
                         wds_l$wd_project_dropbox,
                         "data",sep="/")

wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# Loading packages & functions
library(tidyverse)
library(tidyr)

# source("code/functions.R")


# Observed spawner abundance (osa) 

# Import datasets --------

#'* Import dataset_1part2 from the database, which is streamspawnersurveys_output *
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- F
dataset_1part2 <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[4],
                                                     fromDatabase = fromDatabase,
                                                     update_file_csv = update_file_csv,
                                                     wd = wd_pop_indic_data_input_dropbox)
head(dataset_1part2)

# Arrange the rows for easier comparison and check up
dataset_1part2 <- dataset_1part2  %>% 
  arrange(factor(region, levels = c("Yukon","Transboundary","Haida Gwaii","Nass",
                                    "Skeena","Central Coast",
                                    "Vancouver Island & Mainland Inlets",
                                    "Fraser","Columbia")),
          species_name,
          cu_name_pse,
          year)

#' Alternatively, import the most recent dataset_1part2_DATE.csv dataset produced in 
#' /spawner-surveys
dataset_1part2 <- import_mostRecent_file_fun(wd = wd_output_sp_surveys,
                                             pattern = "dataset_1part2")
head(dataset_1part2)

#'* Import dataset_1part1 from the database, which is dataset1cu_output *
#' It is imported to see the format of the dataset that need to be exported.
dataset_1part1_old <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[15],
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)
head(dataset_1part1_old)

dataset_1part1_old <- dataset_1part1_old  %>% 
  arrange(factor(region, levels = c("Yukon","Transboundary","Haida Gwaii","Nass",
                                    "Skeena","Central Coast",
                                    "Vancouver Island & Mainland Inlets",
                                    "Fraser","Columbia")),
          species_name,
          cu_name_pse,
          year)

#
# Produce dataset_1part1_DATE.csv -----
#

#'* Sum dataset_1part2 per cuid and year *

# sum stream_observed_count per per year and cuid
dataset_1part1 <- dataset_1part2 %>%
  # group_by(region,species_name,species_abbr,cuid,cu_name_pse,year) %>%
  group_by(region,species_name,cuid,cu_name_pse,year) %>%
  summarise(observed_spawners = sum(stream_observed_count))

dataset_1part1 <- dataset_1part1  %>% 
  arrange(factor(region, levels = c("Yukon","Transboundary","Haida Gwaii","Nass",
                                    "Skeena","Central Coast",
                                    "Vancouver Island & Mainland Inlets",
                                    "Fraser","Columbia")),
          species_name,
          cu_name_pse,
          year)

head(dataset_1part1)

# Rename observed_spawners to observed_count
colnames(dataset_1part1)[colnames(dataset_1part1) == "observed_spawners"] <- "observed_count"

# Add columns estimated_count and total_run
dataset_1part1$estimated_count <- dataset_1part1$total_run <- NA

# Re-order columns
dataset_1part1 <- dataset_1part1[,colnames(dataset_1part1_old)]

head(dataset_1part1)

date <- ""
date <- as.character(Sys.time())
date <- strsplit(x = date, split = " ")[[1]][1]
date <- gsub("-","",date)
write.csv(dataset_1part1,paste0(wd_output,"/dataset_1part1_",date,".csv"),
          row.names = F)
#
# Compare dataset_1part2 from database vs. dropbox repo -----
#' The datasets should be the same except that the database version should have 
#' values for (1) Yukon, (2) Transboundary and (3) Columbia sockeye (cuid 1300 
#' streamid 9703) as those were removed from dataset_1part2 in 
#' spawner-surveys/2_nuseds_cuid_streamid.R.

# Import dataset_1part2 from database
datasetsNames_database <- datasetsNames_database_fun()
fromDatabase <- update_file_csv <- F
dataset_1part2 <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[4],
                                        fromDatabase = fromDatabase,
                                        update_file_csv = update_file_csv,
                                        wd = wd_pop_indic_data_input_dropbox)

# Import dataset_1part2 from dropbox
dataset_1part2_source <- import_mostRecent_file_fun(wd = wd_output_sp_surveys,
                                                    pattern = "dataset_1part2")

# remove Yukon, (2) Transboundary and (3) Columbia sockeye in dataset_1part2
dataset_1part2 <- dataset_1part2[! dataset_1part2$region %in% c("Transboundary","Yukon"),]

cond <- dataset_1part2$cuid == 1300 &
  !is.na(dataset_1part2$cuid) &
  dataset_1part2$streamid == 9703
dataset_1part2[cond,]
dataset_1part2 <- dataset_1part2[!cond,]

# Compare the two datasets:
nrow(dataset_1part2)        # 141471
nrow(dataset_1part2_source) # 140512
nrow(dataset_1part2)  - nrow(dataset_1part2_source) # 959

sum(is.na(dataset_1part2$year))        # 548
sum(is.na(dataset_1part2_source$year)) # 0

# check dataset_1part2 where year is NA
cond_yr_NA <- is.na(dataset_1part2$year)
dataset_1part2[cond_yr_NA,]




dataset_1part2 <- dataset_1part2[!is.na(dataset_1part2$year),]

rg_cuid_yr <- unique(dataset_1part2[,c("region","cuid","year")])
nrow(rg_cuid_yr) # 18486

rg_cuid_yr_source <- unique(dataset_1part2_source[,c("region","cuid","year")])
nrow(rg_cuid_yr_source) # 18074

rg_cuid <- unique(dataset_1part2[,c("region","cuid")])
nrow(rg_cuid) # 360

rg_cuid_source <- unique(dataset_1part2_source[,c("region","cuid")])
nrow(rg_cuid_source) # 345

rg_cuid_merged <- apply(X = rg_cuid, 1, function(r){paste(r,collapse = " - ")})
rg_cuid_merged_source <- apply(X = rg_cuid_source, 1, function(r){paste(r,collapse = " - ")})

rg_cuid_merged[! rg_cuid_merged %in% rg_cuid_merged_source]
rg_cuid_merged_source[! rg_cuid_merged_source %in% rg_cuid_merged] # 0

extras <- rg_cuid_merged[! rg_cuid_merged %in% rg_cuid_merged_source]
rg_cuid_extra <- data.frame(region = rep(NA,length(extras)),
                            cuid = rep(NA,length(extras)))

for(i in 1:nrow(rg_cuid_extra)){
  char <- strsplit(x = extras[i],split = " - ")[[1]]
  rg_cuid_extra$region[i] <- char[1]
  rg_cuid_extra$cuid[i] <- char[2]
}

rg_cuid_extra

# check if the time series match for the rest of the data
rg_cuid_stream <- unique(dataset_1part2[,c("region","cuid","streamid")])
nrow(rg_cuid_stream) # 6286

rg_cuid_stream_source <- unique(dataset_1part2_source[,c("region","cuid","streamid")])
nrow(rg_cuid_stream_source) # 6260

rg_cuid_strean_merged <- apply(X = rg_cuid_stream, 1, function(r){paste(r,collapse = " - ")})
rg_cuid_stream_merged_source <- apply(X = rg_cuid_stream_source, 1, function(r){paste(r,collapse = " - ")})

commons <- rg_cuid_strean_merged[ rg_cuid_strean_merged %in% rg_cuid_stream_merged_source]
count <- 1
count_thresh <- 0
toCkeck <- data.frame()
for(i in 1:length(commons)){
  char <- strsplit(x = commons[i],split = " - ")[[1]]
  region <- char[1]
  cuid <- as.numeric(char[2])
  streamid <- as.numeric(char[3])
  
  cond <- dataset_1part2$region == region & 
    dataset_1part2$cuid == cuid &
    dataset_1part2$streamid == streamid
  
  cond_source <- dataset_1part2_source$region == region &
    dataset_1part2_source$cuid == cuid &
    dataset_1part2_source$streamid == streamid
  
  series <- dataset_1part2[cond,c("stream_observed_count","year")]
  series_source <- dataset_1part2_source[cond_source,c("stream_observed_count","year")]
  rownames(series) <- rownames(series_source) <- NULL
  
  series$stream_observed_count <- round(series$stream_observed_count,1)
  series_source$stream_observed_count <- round(series_source$stream_observed_count,1)
  
  pointids_source <- unique(dataset_1part2_source$pointid[cond_source])
  stream_name_pse_here_source <- unique(dataset_1part2_source$stream_name_pse[cond_source])
  # dataset_1part2_source[cond_source,]
  
  if(!identical(series,series_source)){
    count <- count + 1
    xmin <- min(c(series$year,series_source$year))
    xmax <- max(c(series$year,series_source$year))
    ymin <- min(c(series$stream_observed_count,series_source$stream_observed_count))
    ymax <- max(c(series$stream_observed_count,series_source$stream_observed_count))
    plot(NULL,xlim = c(xmin,xmax),ylim = c(ymin,ymax))
    lines(x = series$year, y = series$stream_observed_count, lwd = 1.5, col = "red")
    points(x = series$year, y = series$stream_observed_count,  lwd = 1.5, col = "red", pch = 16)
    lines(x = series_source$year, y = series_source$stream_observed_count, lwd = 1.5, col = "blue", pch = 1)
    points(x = series_source$year, y = series_source$stream_observed_count,  lwd = 1.5, col = "blue")
    legend("topright",c("database","source"),fill = c("red","blue"),bty = "n")
    legend("top",legend = c(paste0("i = ",i),paste0("count = ",count)),bty = "n")
    legend("topleft",legend = c(paste0("region = ",region),
                                paste0("cuid = ",cuid),
                                paste0("streamid = ",streamid),
                                paste0("pointid = ",paste0(pointids_source,collapse = ", ")),
                                stream_name_pse_here_source),
           bty = "n")
    
    toCkeck_here <- data.frame(region = region, cuid = cuid, streamid = streamid, 
                               pointid = paste0(pointids_source,collapse = ", "),
                               stream_name_pse = stream_name_pse_here_source)
    if(nrow(toCkeck) == 0){
      toCkeck <- toCkeck_here
    }else{
      toCkeck <- rbind(toCkeck,toCkeck_here)
    }
    
  }
  progress <- round(i/length(commons)*100,1)
  if(progress > count_thresh){
    print(paste0(progress,"%"))
    count_thresh <- (round(ceiling(progress)/10) + 1) * 10
  }
}

toCkeck

