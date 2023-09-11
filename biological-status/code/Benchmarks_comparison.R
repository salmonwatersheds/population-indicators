
# the goal of the script is to compare the benchmark central values and their
# 95% CI among the different methods used, for all regions, species and CUs.

# 
rm(list = ls())
graphics.off()

# Set directory to /biological-status
if(!grepl(pattern = "biological-status", x = getwd())){
  setwd(dir = paste0(getwd(),"/biological-status"))
}

# Import functions
source("Code/functions.R")

library(tidyverse)

# The pass ../Salmon Watersheds Dropbox/user_name/X Drive/1_PROJECTS.
# The pass is personal and must be copy past in wd_X_Drive1_PROJECTS.txt
# e.g.: "C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS"
wd_X_Drive1_PROJECTS <- readLines( "wd_X_Drive1_PROJECTS.txt")

# Define subdirectories:
wd_code <- paste0(getwd(),"/code")
wd_data <- paste0(getwd(),"/data")

# figures and datasets generated are 
Export_locally <- F
if(Export_locally){
  wd_figures <- paste0(wd_X_Drive1_PROJECTS,"/figures")
  wd_output <- paste0(getwd(),"/output")
}else{
  wd_biological_status <- "Population Methods and Analysis/population-indicators/biological-status"
  wd_figures <- paste0(wd_X_Drive1_PROJECTS,"/",wd_biological_status,"/figures")
  wd_output <- paste0(wd_X_Drive1_PROJECTS,"/",wd_biological_status,"/output")
}

# The datsets to input were outputted by other scripts 
wd_data_input <- wd_output

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

# all the regions
region <- as.character(regions_df[1,])
region <- region[region != "Columbia"]

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
species <- c(
  species_acronym$Sockeye,    
  species_acronym$Pink,
  species_acronym$Coho
  #species_acronym$Cutthroat,
  #species_acronym$Chum
)

# If we do not specify the species: all the species that have a _SRdata files are 
# returned: 
# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- TRUE

for(i_rg in 1:length(region)){
  
  # i_rg <- 1
  
  region_i <- region[i_rg]
  
  # select the benchmark files related to the region
  files_list <- list.files(wd_output)
  files_list <- files_list[grepl(pattern = "[B|b]enchmarks",x = files_list)]
  files_list <- files_list[grepl(pattern = region_i,x = files_list)]
  
  # returns the species available
  species_avail <- gsub(paste0(region_i,"_"),"",files_list)
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
      
      # i_sp <- 1
      species_i <- species[i_sp]
      
      # import the files as dataframe and combine then by row
      files_list_sp <- files_list[grepl(species_i,files_list)]
      benchmarks_df <- lapply(X = files_list_sp, FUN = function(x){
        output <- read.csv(paste0(wd_output,"/",x),header = T)
        if(sum("benchmarks" %in% colnames(output)) == 0){
          output$benchmarks <- NA
        }
        return(output)
      })
      
      benchmarks_df <- do.call(what = rbind,benchmarks_df)
      benchmarks_df$CU <- gsub("_"," ",benchmarks_df$CU)
      
      # 
      CUs <- unique(benchmarks_df$CU)
      
      nbplots_max <- 6^2 # could change to 5 or 4 if that's too many figures
      
      if(length(Cus) <= nbplots_max){
        
        # make the figure
        figure_compare_benchamrks_fun(BM_data = benchmarks_df, 
                                      nameRegion_show = T, 
                                      nameSpecies_show = T, 
                                      print_fig = T,
                                      size_box_cm = 7,
                                      wd_figures = wd_figures)
        
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
                                        print_fig = T,
                                        size_box_cm = 7,
                                        wd_figures = wd_figures, 
                                        addTonameFile = paste0("_",i_loops))
        }
      }
    }
  }
}







