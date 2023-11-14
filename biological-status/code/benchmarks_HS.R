
#'******************************************************************************
#' The goal of the script is to calculate the benchmarks from using the percentile
#' approach on the history spawner (HS) data.
#' 
#' #' Files imported (from dropbox):
#' - /data/spawner_abundance.csv
#' 
#' Files produced: 
#' - output/region_species__benchmarks_HS_percentiles_summary.csv
#'******************************************************************************

#' Background info: 
#' - https://www.dropbox.com/scl/fi/64dc2861izm12xzykf6sd/HS_ConfidenceIntervals_10June2020.docx?rlkey=9a8hh4zvgl6zc2s8bik9s612o&dl=0
#' - Steph's github repo:
#'    https://github.com/salmonwatersheds/percentile-benchmarks-CI
#' - Example application of the code to the Fraser in:
#'    C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS/Fraser_VIMI/analysis/fraser-status

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
source("code/functions_set_wd.R")

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
wd_project_dropbox <- wds_l$wd_project_dropbox

# The datasets to input were outputted by other scripts 
wd_data_input <- wd_output

# Import functions for this specific project
source("Code/functions.R")

# Load packages
library(tidyverse)

# option to export the figures
print_fig <- F

# Paths to the repositories containing the run reconstruction datasets for each 
# region.
wd_data_regions <- wd_data_regions_fun(wd_root = wd_X_Drive1_PROJECTS)

# Import species names and acronyms
species_acronym <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#------------------------------------------------------------------------------#
# Selection of region(s) and species and benchmark %
#------------------------------------------------------------------------------#

# Choosing the region
# BSC: This will have to eventually be automatized and eventually allows for 
# multiple regions to be passed on.
region <- regions_df$Fraser
region <- regions_df$Yukon
region <- regions_df$Nass

# multiple regions:
region <- c(
  regions_df$Fraser,
  regions_df$Yukon,
  regions_df$Nass)

region <- c(
  regions_df$Central_coast,
  regions_df$Haida_Gwaii,
  regions_df$Skeena)

region <- regions_df$Central_coast

# all the regions
region <- as.character(regions_df[1,])
region <- region[region != "Columbia"]  # no data in Columbia

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
species <- c(
  # species_acronym$Sockeye,    
  # species_acronym$Pink,
  # species_acronym$Coho
  species_acronym$Chinook,
  species_acronym$Chum
)

species <- species_acronym$Pink

# If we do not specify the species: all the species that have a _SRdata files are 
# returned: 
# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- T

# Import the spawner_abundance.csv, downloaded from SPS work.
spawner_abundance_path <- paste0(wd_X_Drive1_PROJECTS,"/",wd_project_dropbox,"/data")
spawner_abundance <- read.csv(paste0(spawner_abundance_path,"/spawner_abundance.csv"),header = T)
# head(spawner_abundance)
# unique(spawner_abundance$species_name)

# The benchmarks to use
benchmarks <- c(0.25, 0.75) # was c(0.25, 0.5)

for(i_rg in 1:length(region)){
  
  # i_rg <- 2
  
  region_i <- gsub("_"," ",region[i_rg])
  if(region_i == "Central coast"){
    region_i <- "Central Coast"
  }
  
  spawner_abundance_rg <- spawner_abundance[spawner_abundance$region == region_i,]
  
  if(species_all){
    species_full <- unique(spawner_abundance_rg$species_name)
  }else{
    # return the full name of the species
    species_full <- sapply(X = species, FUN = function(x){
      output <- names(species_acronym)[species_acronym == x]
      return(output)
    })
  }
  
  for(i_sp in 1:length(species_full)){
    # i_sp <- 1
    
    species_i <- species_full[i_sp]
    #
    species_i <- gsub(pattern = "Lake ",replacement = "",x = species_i)  # for sockeye
    species_i <- gsub(pattern = "River ",replacement = "",x = species_i) # for sockeye
    species_i <- gsub(pattern = "\\s*\\(odd\\)",replacement = "",x = species_i) # for Pink
    species_i <- gsub(pattern = "\\s*\\(even\\)",replacement = "",x = species_i)# for Pink

    # find the corresponding acronym
    species_i_luc <- character_lowerHigerCase_fun(species_i) # to not worry about upper and lower case species names
    species_acryn_i <- species_acronym[grepl(pattern = species_i_luc, x = names(species_acronym))]
    species_acryn_i <- as.character(species_acryn_i)
    
    # subset spawner_abundance_rg 
    # spawner_abundance_rg_sp <- spawner_abundance_rg[spawner_abundance_rg$species_name == species[i_sp],]
    spawner_abundance_rg_sp <- spawner_abundance_rg[grepl(species_i_luc,spawner_abundance_rg$species_name),]
    
    # in case the species was selected by the user it is not present in that region:
    if(nrow(spawner_abundance_rg_sp) > 0){
      
      
      # find the CUs present
      CUs <- unique(spawner_abundance_rg_sp$cu_name_pse)
      
      # create a dataframe to retain the benchmark information for all the CUs of
      # the species in the region
      benchSummary_region_species_df <- NULL
      
      for(i_cu in 1:length(CUs)){
        
        # i_cu <- 1
        
        # subset spawner_abundance_rg_sp
        spawner_abundance_rg_sp_cu <- spawner_abundance_rg_sp[spawner_abundance_rg_sp$cu_name_pse == CUs[i_cu],]
        
        # get the count
        series <- spawner_abundance_rg_sp_cu$estimated_count
        series[series <= 0] <- NA
        
        # This does not work so we remove the Nas instead of the odd or even years
        # if(species_acryn_i == "PK"){  # numLags has to be set to 2 year and not one because of the odd and even CUs
        #   numLags <- 2
        # }else{
        #   numLags <- 1
        # }
        numLags <- 1
        modelCI <- modelBoot(series = series, 
                             numLags = numLags, # numLags is the lag for the autocorrelation; default is just 1 year
                             nBoot = 10000, 
                             benchmarks = benchmarks)
        
        # place the information a dataframe
        benchSummary_df <- data.frame(region = rep(region[i_rg],2),
                                      species = rep(species_acryn_i,2),
                                      CU = rep(CUs[i_cu],2),
                                      benchmark = c('lower','upper'),
                                      method = rep('HS_percentiles',2))
        
        benchSummary_df$m <- modelCI$m
        benchSummary_df$CI025 <- modelCI$CI[,1]
        benchSummary_df$CI975 <- modelCI$CI[,2]
        benchSummary_df$benchmarks <- rep(paste(benchmarks,collapse = "-"),2)
        
        if(is.null(benchSummary_region_species_df)){
          benchSummary_region_species_df <- benchSummary_df
        }else{
          benchSummary_region_species_df <- rbind(benchSummary_region_species_df,
                                                  benchSummary_df)
        }
      } # end of loop for the CUs
      
      print(paste0("*** ",region[i_rg],"_",species_acryn_i," done ***"))
      write.csv(x = benchSummary_region_species_df, 
                file = paste0(wd_output,"/",region[i_rg],"_",species_acryn_i,"_benchmarks_HS_percentiles_summary.csv"),
                row.names = F) # keep region[i_rg] and not region_i because of "Central coast" is used to name the other files and not "Central Coast"
      
    }else{ # in case the species selected by the user is not this region
      
      print(paste0("The species ",species_i," is not present in ",region_i," in the dataset used."))
      
    }
  } # end of loop for the species
} # end of the loop for the regions





