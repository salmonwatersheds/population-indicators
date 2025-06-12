

#'* NOT USED ANYMORE (as of Meeting with Steph 21/05/2025) *
#' the script is now spawner-surveys/code/5_observed_abundance_nuseds.R



#'******************************************************************************
#' The goal of the script is to sum the stream-level observed spawner counts 
#' (**dataset2_spawner-surveys_YYYY-MM-DD.csv** produced in
#' **spawner-surveys/code/4_datasets_for_PSE.R**) for each CU and to export the 
#' results as the field `observed_count` in 
#' **dataset1_spawner-abundance_YYYY-MM-DD.csv**.
#' 
#' 
#' Files imported (from dropbox):
#' - streamspawnersurveys_output.csv  # from the database; = dataset2_spawner_surveys_YYYY-MM-DD.csv from spawner-surveys/code/4_datasets_for_PSE.R
#' - dataset1cu_output.csv            # from the database; = dataset1_spawner_abundance_YYYY-MM-DD.csv from elsewhere
#' 
#' Files produced: 
#' - dataset1_spawner_abundance_YYYY-MM-DD.csv # previously - dataset1cu_output.csv; = dataset1cu_output.csv in DB but without columns 'estimated_count' and 'total_run'
#'  
#' Note: code taken from 
#' Transboundary/Data & Assessments/transboundary-data/code/4_pse-spawner-abundance.R
#' https://www.dropbox.com/scl/fi/pt80lerubav9r83uh4vtr/4_pse-spawner-abundance.R?rlkey=e723ohin2r1k5vpr0in80lvxy&dl=0
#' 
#' Example of outputed dataset:
#' https://www.dropbox.com/scl/fi/b9jkohs2wixv48ua78r1w/spawner_abundance_dataset_1part1_2024-03-20.csv?rlkey=dpz1vykjx8c2wezidhl5tkhto&dl=0
#' 
#' Code adapted from Previous script: Fraser_salmon_CU_updates.Rmd
#' 
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


# Import datasets --------
#

#'* Import streamspawnersurveys_output from the database *
#'  dataset2_spawner_surveys_YYYY-MM-DD.csv, previously dataset_1part2.csv
datasetsNames_database <- datasetsNames_database_fun()

# fromDatabase <- update_file_csv <- FALSE
# spawnersurveys <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[4],
#                                         fromDatabase = fromDatabase,
#                                         update_file_csv = update_file_csv,
#                                         wd = wd_pop_indic_data_input_dropbox)
# head(spawnersurveys)

#' Alternatively, import the most recent dataset produced in 
#' /spawner-surveys
spawnersurveys <- import_mostRecent_file_fun(wd = paste0(wd_output_sp_surveys,"/archive"),
                                             pattern = "dataset2_spawner-surveys")  # TODO: replace eventually by dataset2_spawner_surveys
head(spawnersurveys)


# Arrange the rows for easier comparison and check up
spawnersurveys <- spawnersurveys  %>% 
  arrange(factor(region, levels = c("Yukon","Transboundary","Haida Gwaii","Nass",
                                    "Skeena","Central Coast",
                                    "Vancouver Island & Mainland Inlets",
                                    "Fraser","Columbia")),
          species_name,
          cu_name_pse,
          year)


#' #'* Import dataset1cu_output.csv from the database *
#' 
#' #' = dataset1_spawner_abundance_YYYY-MM-DD.csv, previously dataset_1part1
#' #' It is imported to see the format of the dataset that need to be exported.
#' dataset_1part1_old <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[15],
#'                                             fromDatabase = fromDatabase,
#'                                             update_file_csv = update_file_csv,
#'                                             wd = wd_pop_indic_data_input_dropbox)
#' head(dataset_1part1_old)
#' 
#' dataset_1part1_old <- dataset_1part1_old  %>% 
#'   arrange(factor(region, levels = c("Yukon","Transboundary","Haida Gwaii","Nass",
#'                                     "Skeena","Central Coast",
#'                                     "Vancouver Island & Mainland Inlets",
#'                                     "Fraser","Columbia")),
#'           species_name,
#'           cu_name_pse,
#'           year)
#' 
#' 

# Produce dataset1_spawner_abundance_YYYY-MM-DD.csv (dataset_1part1_DATE.csv) -----
#

#'* Sum spawnersurveys per cuid and year *

#------------------------------------------------------------------------------
# Remove two spawner surveys that are duplicates (Seee 5.4.1 Observed Spawner Abundance)
#------------------------------------------------------------------------------

if(unique(spawnersurveys$stream_name_pse[which(spawnersurveys$streamid == 10138)]) != "Nesketahin Lake"){
  stop("Not Nesketahin Lake")
}

if(unique(spawnersurveys$stream_name_pse[which(spawnersurveys$streamid == 10271)]) != "INKANEEP CREEK"){
  stop("Not INKANEEP CREEK")
}

spawnersurveys <- spawnersurveys[- which(spawnersurveys$streamid %in% c(1038, 10271)), ]


#------------------------------------------------------------------------------
# sum stream_observed_count per per year and cuid
#------------------------------------------------------------------------------
dataset1_observed <- spawnersurveys %>%
  group_by(region, species_name, species_qualified, cuid, cu_name_pse, year) %>%
  summarise(observed_spawners = sum(stream_observed_count, na.rm = T))

dataset1_observed <- dataset1_observed  %>% 
  arrange(factor(region, levels = c("Yukon","Transboundary","Haida Gwaii","Nass",
                                    "Skeena","Central Coast",
                                    "Vancouver Island & Mainland Inlets",
                                    "Fraser","Columbia")),
          species_name,
          cu_name_pse,
          year)

head(dataset1_observed)

# Rename observed_spawners to observed_count (to be changed when dataset paramters are finalized)
colnames(dataset1_observed)[colnames(dataset1_observed) == "observed_spawners"] <- "observed_count"

head(dataset1_observed)

min(dataset1_observed$observed_count, na.rm = T)

# Export to /archive folder on dropbox:
date <- as.character(Sys.Date())
write.csv(dataset1_observed,
          paste0(wd_output,"/archive/dataset1_observed-spawners_",date,".csv"), # dataset_1part1_ previously
          row.names = FALSE)

# Export to /output locally to push to github
write.csv(dataset1_observed,
          paste0(paste0(getwd(),"/output"),"/dataset1_observed-spawners.csv"), # dataset_1part1_ previously
          row.names = FALSE)

#' #
#' # Compare spawnersurveys from database vs. dropbox repo -----
#' #' The datasets should be the same except that the database version should have 
#' #' values for (1) Yukon, (2) Transboundary and (3) Columbia sockeye (cuid 1300 
#' #' streamid 9703) as those were removed from spawnersurveys in 
#' #' spawner-surveys/2_nuseds_cuid_streamid.R.
#' 
#' # Import spawnersurveys from database
#' datasetsNames_database <- datasetsNames_database_fun()
#' fromDatabase <- update_file_csv <- F
#' spawnersurveys <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[4],
#'                                         fromDatabase = fromDatabase,
#'                                         update_file_csv = update_file_csv,
#'                                         wd = wd_pop_indic_data_input_dropbox)
#' 
#' # Import spawnersurveys from dropbox
#' spawnersurveys_source <- import_mostRecent_file_fun(wd = wd_output_sp_surveys,
#'                                                     pattern = "dataset2_spawner_surveys_") # dataset2_spawner_surveys_
#' 
#' # remove Yukon, (2) Transboundary and (3) Columbia sockeye in dataset_1part2
#' dataset_1part2 <- dataset_1part2[! dataset_1part2$region %in% c("Transboundary","Yukon"),]
#' 
#' cond <- dataset_1part2$cuid == 1300 &
#'   !is.na(dataset_1part2$cuid) &
#'   dataset_1part2$streamid == 9703
#' spawnersurveys[cond,]
#' spawnersurveys <- spawnersurveys[!cond,]
#' 
#' # Compare the two datasets:
#' nrow(spawnersurveys)        # 141471
#' nrow(spawnersurveys_source) # 140512
#' nrow(spawnersurveys)  - nrow(spawnersurveys_source) # 959
#' 
#' sum(is.na(spawnersurveys$year))        # 548
#' sum(is.na(spawnersurveys_source$year)) # 0
#' 
#' # check spawnersurveys where year is NA
#' cond_yr_NA <- is.na(spawnersurveys$year)
#' spawnersurveys[cond_yr_NA,]
#' 
#' 
#' 
#' 
#' spawnersurveys <- spawnersurveys[!is.na(spawnersurveys$year),]
#' 
#' rg_cuid_yr <- unique(spawnersurveys[,c("region","cuid","year")])
#' nrow(rg_cuid_yr) # 18486
#' 
#' rg_cuid_yr_source <- unique(spawnersurveys_source[,c("region","cuid","year")])
#' nrow(rg_cuid_yr_source) # 18074
#' 
#' rg_cuid <- unique(spawnersurveys[,c("region","cuid")])
#' nrow(rg_cuid) # 360
#' 
#' rg_cuid_source <- unique(spawnersurveys_source[,c("region","cuid")])
#' nrow(rg_cuid_source) # 345
#' 
#' rg_cuid_merged <- apply(X = rg_cuid, 1, function(r){paste(r,collapse = " - ")})
#' rg_cuid_merged_source <- apply(X = rg_cuid_source, 1, function(r){paste(r,collapse = " - ")})
#' 
#' rg_cuid_merged[! rg_cuid_merged %in% rg_cuid_merged_source]
#' rg_cuid_merged_source[! rg_cuid_merged_source %in% rg_cuid_merged] # 0
#' 
#' extras <- rg_cuid_merged[! rg_cuid_merged %in% rg_cuid_merged_source]
#' rg_cuid_extra <- data.frame(region = rep(NA,length(extras)),
#'                             cuid = rep(NA,length(extras)))
#' 
#' for(i in 1:nrow(rg_cuid_extra)){
#'   char <- strsplit(x = extras[i],split = " - ")[[1]]
#'   rg_cuid_extra$region[i] <- char[1]
#'   rg_cuid_extra$cuid[i] <- char[2]
#' }
#' 
#' rg_cuid_extra
#' 
#' # check if the time series match for the rest of the data
#' rg_cuid_stream <- unique(spawnersurveys[,c("region","cuid","streamid")])
#' nrow(rg_cuid_stream) # 6286
#' 
#' rg_cuid_stream_source <- unique(spawnersurveys_source[,c("region","cuid","streamid")])
#' nrow(rg_cuid_stream_source) # 6260
#' 
#' rg_cuid_strean_merged <- apply(X = rg_cuid_stream, 1, function(r){paste(r,collapse = " - ")})
#' rg_cuid_stream_merged_source <- apply(X = rg_cuid_stream_source, 1, function(r){paste(r,collapse = " - ")})
#' 
#' commons <- rg_cuid_strean_merged[ rg_cuid_strean_merged %in% rg_cuid_stream_merged_source]
#' count <- 1
#' count_thresh <- 0
#' toCkeck <- data.frame()
#' for(i in 1:length(commons)){
#'   char <- strsplit(x = commons[i],split = " - ")[[1]]
#'   region <- char[1]
#'   cuid <- as.numeric(char[2])
#'   streamid <- as.numeric(char[3])
#'   
#'   cond <- spawnersurveys$region == region & 
#'     spawnersurveys$cuid == cuid &
#'     spawnersurveys$streamid == streamid
#'   
#'   cond_source <- spawnersurveys_source$region == region &
#'     spawnersurveys_source$cuid == cuid &
#'     spawnersurveys_source$streamid == streamid
#'   
#'   series <- spawnersurveys[cond,c("stream_observed_count","year")]
#'   series_source <- spawnersurveys_source[cond_source,c("stream_observed_count","year")]
#'   rownames(series) <- rownames(series_source) <- NULL
#'   
#'   series$stream_observed_count <- round(series$stream_observed_count,1)
#'   series_source$stream_observed_count <- round(series_source$stream_observed_count,1)
#'   
#'   pointids_source <- unique(spawnersurveys_source$pointid[cond_source])
#'   stream_name_pse_here_source <- unique(spawnersurveys_source$stream_name_pse[cond_source])
#'   # spawnersurveys_source[cond_source,]
#'   
#'   if(!identical(series,series_source)){
#'     count <- count + 1
#'     xmin <- min(c(series$year,series_source$year))
#'     xmax <- max(c(series$year,series_source$year))
#'     ymin <- min(c(series$stream_observed_count,series_source$stream_observed_count))
#'     ymax <- max(c(series$stream_observed_count,series_source$stream_observed_count))
#'     plot(NULL,xlim = c(xmin,xmax),ylim = c(ymin,ymax))
#'     lines(x = series$year, y = series$stream_observed_count, lwd = 1.5, col = "red")
#'     points(x = series$year, y = series$stream_observed_count,  lwd = 1.5, col = "red", pch = 16)
#'     lines(x = series_source$year, y = series_source$stream_observed_count, lwd = 1.5, col = "blue", pch = 1)
#'     points(x = series_source$year, y = series_source$stream_observed_count,  lwd = 1.5, col = "blue")
#'     legend("topright",c("database","source"),fill = c("red","blue"),bty = "n")
#'     legend("top",legend = c(paste0("i = ",i),paste0("count = ",count)),bty = "n")
#'     legend("topleft",legend = c(paste0("region = ",region),
#'                                 paste0("cuid = ",cuid),
#'                                 paste0("streamid = ",streamid),
#'                                 paste0("pointid = ",paste0(pointids_source,collapse = ", ")),
#'                                 stream_name_pse_here_source),
#'            bty = "n")
#'     
#'     toCkeck_here <- data.frame(region = region, cuid = cuid, streamid = streamid, 
#'                                pointid = paste0(pointids_source,collapse = ", "),
#'                                stream_name_pse = stream_name_pse_here_source)
#'     if(nrow(toCkeck) == 0){
#'       toCkeck <- toCkeck_here
#'     }else{
#'       toCkeck <- rbind(toCkeck,toCkeck_here)
#'     }
#'     
#'   }
#'   progress <- round(i/length(commons)*100,1)
#'   if(progress > count_thresh){
#'     print(paste0(progress,"%"))
#'     count_thresh <- (round(ceiling(progress)/10) + 1) * 10
#'   }
#' }
#' 
#' toCkeck

