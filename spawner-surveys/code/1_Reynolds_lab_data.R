

#'******************************************************************************
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

wds_l <- set_working_directories_fun(subDir = subDir_projects$spawner_surveys,
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

library(xlsx)
library(readxl)


SFU_data4 <- read_excel(paste(wd_data_dropbox,"SFU_Escapement_PSF.xlsx",sep="/"), 
                       sheet = 4)
head(SFU_data4)
#' SFU Peak:	When there are only 1 or 2 counts, we used Peak Live + Dead.  If 
#' there counts are closer together than the residency time for the species, add
#' the live and dead from the highest count recorded. If there are 2 counts, and
#' they are further apart than the residency times for a species, add the 2 live 
#' counts together with the dead from the peak count.
 
#' SFU AUC:	Area Under the Curve, only used when there were 3 or more counts done.
#' 






