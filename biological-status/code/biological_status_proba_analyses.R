
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

# Load packages

# option to export the figures
print_fig <- F

# Import species names and acronyms
species_acronym <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#------------------------------------------------------------------------------#
# Analyses
#------------------------------------------------------------------------------#

#' Import the files for all regions, species and CUs and rbind them to a single 
#' dataframe.

region <- as.character(regions_df[1,])
region <- region[region != "Columbia"]

pattern <- "biological_status"
biological_status_df <- NULL
for(rg in region){
  # rg <- regions_df[1,1]
  
  # returns all the files with pattern rg and "biological_status"
  list_files <- list.files(path = paste0(wd_output))
  list_files <- list_files[grepl(rg,list_files) & grepl(pattern,list_files)]
  
  # import these files and rbind them
  for(i_f in 1:length(list_files)){
    # i_f <- 1
    fileHere <- read.csv(file = paste(wd_output,list_files[i_f],sep="/"),header = T)

    if(is.null(biological_status_df)){
      biological_status_df <- fileHere
    }else{
      biological_status_df <- rbind(biological_status_df,fileHere)
    }
  }
}


write.csv(biological_status_df,paste0(wd_output,"/Biological_status_HBSRM_all.csv"),
          row.names = F)





