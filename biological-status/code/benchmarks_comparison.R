
#'******************************************************************************
#' The goal of the script is to compare the benchmark central values and their
#' 95% CI among the different methods used (HBSRM vs. percentile), for all regions, 
#' species and CUs.
#' 
#' #' Files imported (from /output --> produced in this subdir):
#' - region_species_benchmarks_HS_percentiles_summary.csv
#' - region_species_benchmarks_summary.csv
#' 
#' Files produced: 
#' - figures/region_species_benchmarks_comparisons.jpeg
#'******************************************************************************

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

# The datasets to input were outputted by other scripts 
wd_data_input <- wd_output

# Import functions for this specific project
source("Code/functions.R")

# Load packages
library(tidyverse)

# Paths to the repositories containing the run reconstruction datasets for each 
# region.
wd_data_regions <- wd_data_regions_fun(wd_root = wd_X_Drive1_PROJECTS)

# Import species names and acronyms
species_acronym_df <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()


#------------------------------------------------------------------------------#
# Selection of region(s) and species and benchmark %
#------------------------------------------------------------------------------#

print_fig <- F  # to export the figures

# Choosing the region
# BSC: This will have to eventually be automatized and eventually allows for 
# multiple regions to be passed on.
region <- regions_df$Fraser
region <- regions_df$Yukon
region <- regions_df$Nass
region <- regions_df$Central_coast

# multiple regions:
region <- c(
  regions_df$Fraser,
  regions_df$Yukon,
  regions_df$Nass)

region <- c(
  regions_df$Central_coast,
  regions_df$Haida_Gwaii,
  regions_df$Skeena)

# all the regions
region <- as.character(regions_df[1,])

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "CK"],    
             species_acronym_df$species_name[species_acronym_df$species_acro == "SX"])

# If we do not specify the species: all the species that have a _SRdata files are 
# returned: 
# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- TRUE

# maximum number of plots that can be shown in one figure
nbplots_max <- 6^2 # could change to 5 or 4 if that's too many figures

for(i_rg in 1:length(region)){
  
  # i_rg <- 8
  
  region_i <- gsub("_"," ",region[i_rg])
  if(region_i == "Central coast"){
    region_i <- "Central Coast"
  }
  
  if(region[i_rg] == "Vancouver Island & Mainland Inlets"){
    regionName <- "VIMI"
  }else{
    regionName <- regionName <- gsub(" ","_",region[i_rg])
  }
  
  # select the benchmark files related to the region
  files_list <- list.files(wd_output)
  files_list <- files_list[grepl(pattern = "[B|b]enchmarks",x = files_list)]
  files_list <- files_list[grepl(pattern = regionName,x = files_list)]
  
  # returns the species available
  species_avail <- gsub(paste0(regionName,"_"),"",files_list)
  species_avail <- unique(substr(species_avail, start = 1, stop = 2))
  
  if(!species_all){
    
    species_availNot <- species[!species %in% species_avail]
    species <- species[species %in% species_avail]
    
    if(length(species_availNot) > 0){
      print(paste0("The following species are not present in ",region_i,": ",
                   paste(species_availNot, collapse = ", ")))
    }
    
  }else{
    species <- species_avail
  }
  
  if(length(species) > 0){    # in case none of the species selected are available for that region
    
    for(i_sp in 1:length(species)){
      
      # i_sp <- 4
      species_i <- species[i_sp]
      
      # import the files as dataframe and combine then by row
      files_list_sp <- files_list[grepl(species_i,files_list)]
      benchmarks_df <- lapply(X = files_list_sp, FUN = function(x){
        output <- read.csv(paste0(wd_output,"/",x),header = T)
        if(sum("benchmarks" %in% colnames(output)) == 0){
          output$benchmarks <- NA
        }
        colSelect <- c("region","species","cuid","CU","benchmark","method","m","CI025","CI975")
        return(output[,colSelect])
      })
      # colnames(benchmarks_df[[1]])[colnames(benchmarks_df[[1]]) %in% colnames(benchmarks_df[[2]])]
      benchmarks_df <- do.call(what = rbind,benchmarks_df)
      benchmarks_df$CU <- gsub("_"," ",benchmarks_df$CU)
      
      # find for each CUs the corresponding current spawner abundance
      files_bioStatus <- list.files(wd_output)
      files_bioStatus <- files_bioStatus[grepl(pattern = "biological_status",x = files_bioStatus)]
      files_bioStatus <- files_bioStatus[grepl(pattern = regionName,x = files_bioStatus)]
      files_bioStatus <- files_bioStatus[grepl(species_i,files_bioStatus)]
      current_S_cuid <- lapply(X = files_bioStatus, FUN = function(x){
        # x <- files_bioStatus[1]
        output <- read.csv(paste0(wd_output,"/",x),header = T)
        if(sum("benchmarks" %in% colnames(output)) == 0){
          output$benchmarks <- NA
        }
        colSelect <- c("cuid","current_spawner_abundance")
        return(output[,colSelect])
      })
      current_S_cuid <- do.call(rbind,current_S_cuid)
      current_S_cuid <- unique(current_S_cuid)
      benchmarks_df <- merge(x = benchmarks_df, y = current_S_cuid, by = 'cuid')

      # 
      CUs <- unique(benchmarks_df$CU)
      
      if(length(CUs) <= nbplots_max){
        
        # make the figure
        figure_compare_benchamrks_fun(BM_data = benchmarks_df, 
                                      nameRegion_show = T, 
                                      nameSpecies_show = T, 
                                      print_fig = print_fig,
                                      size_box_cm = 7,
                                      wd_figures = wd_figures,
                                      coeff_width_adj = .53)
        
      }else{ # need to create multiple files
        
        length(CUs) %% nbplots_max
        nb_loops <- length(CUs) %/% nbplots_max + 1
        
        for(i_loops in 1:nb_loops){
          start <- 1 + (i_loops - 1) * nbplots_max
          end <- i_loops * nbplots_max
          CUs_here <- CUs[start:end]
          CUs_here <- CUs_here[!is.na(CUs_here)]
          # i_loops <- 1
          benchmarks_df_loop <- benchmarks_df[benchmarks_df$CU %in% CUs_here,]
          
          figure_compare_benchamrks_fun(BM_data = benchmarks_df_loop, 
                                        nameRegion_show = T, 
                                        nameSpecies_show = T, 
                                        print_fig = print_fig,
                                        size_box_cm = 7,
                                        wd_figures = wd_figures, 
                                        addTonameFile = paste0("_",i_loops),
                                        coeff_width_adj = .53)
        }
      }
    }
  }
}







