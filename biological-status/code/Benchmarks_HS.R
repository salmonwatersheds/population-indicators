
# Script about the percentile benchmarks
# background info: 
# https://www.dropbox.com/scl/fi/64dc2861izm12xzykf6sd/HS_ConfidenceIntervals_10June2020.docx?rlkey=9a8hh4zvgl6zc2s8bik9s612o&dl=0
# 
# Steph's github repo:
# https://github.com/salmonwatersheds/percentile-benchmarks-CI

# Example application of the code to the Fraser in:
# C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS/Fraser_VIMI/analysis/fraser-status

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
Export_locally <- T
if(Export_locally){
  wd_figures <- paste0(wd_X_Drive1_PROJECTS,"/figures")
  wd_output <- paste0(getwd(),"/output")
}else{
  wd_biological_status <- "Population Methods and Analysis/population-indicators/biological-status"
  wd_figures <- paste0(wd_X_Drive1_PROJECTS,"/",wd_biological_status,"/figures")
  wd_output <- paste0(wd_X_Drive1_PROJECTS,"/",wd_biological_status,"/output")
}

wd_data_input <- wd_output




# Import species names and acronyms
species_acronym <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#------------------------------------------------------------------------------#
# Selection of region(s) and species
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

# set the path of the input data sets for that specific region
# wd_data_input <- paste0(wd_data_regions[,region])   # BSC: if we end up having the posterior_priorShift.rds file in dropbox
wd_data_input <- wd_output                          # if they are there

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

wd_biological_status <- "Population Methods and Analysis/population-indicators/biological-status"
spawner_abundance_path <- paste0(wd_X_Drive1_PROJECTS,"/",wd_biological_status,"/data")
spawner_abundance <- read.csv(paste0(spawner_abundance_path,"/spawner_abundance.csv"),header = T)
head(spawner_abundance)




for(i_rg in 1:length(region)){
  
  # i_rg <- 1
  
  region_i <- gsub("_"," ",region[i_rg])
  if(region_i == "Central coast"){
    region_i <- "Central Coast"
  }
  
  spawner_abundance_rg <- spawner_abundance[spawner_abundance$region == region_i,]
  
  if(species_all){
    
    species <- unique(spawner_abundance_rg$species_name)
    
  }

  
  for(i_sp in 1:length(species)){
    # i_sp <- 1
    spawner_abundance_rg_sp <- spawner_abundance_rg[spawner_abundance_rg$species_name == species[i_sp],]
    
    CUs <- unique(spawner_abundance_rg_sp$cu_name_pse)
    
    for(i_cu in 1:length(CUs)){
      
      # i_cu <- 1
      spawner_abundance_rg_sp_cu <- spawner_abundance_rg_sp[spawner_abundance_rg_sp$cu_name_pse == CUs[i_cu],]
      series <- spawner_abundance_rg_sp_cu$estimated_count
      series[series <= 0] <- NA
      
      modelCI <- modelBoot(series = dat$LGL.counts, numLags = 1, # numLags is the lag for the autocorrelation; default is just 1 year
                           nBoot = 10000, 
                           benchmarks = c(0.25, 0.5))
      
      modelCI$CI
   
      
    }

    
  }
}



#
filepath <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Transboundary/Data & Assessments/transboundary-status/Output")
abund_file <- read.csv(paste0(filepath,"/dataset_1part1.Jan202023.csv"), header = T)

abund_file <- abund_file %>% select(2,4,5)

head(abund_file)

dat <- subset(abund_file, CUID == '1039')
dat <- na.omit(dat)
dat <- dat %>% select (2,3)
dat <- dat %>% select (2)
dat <- as.numeric(dat)
dat[dat == 0] <- NA
dat

modelCI <- modelBoot(series = dat$LGL.counts, numLags = 1, # numLags is the lag for the autocorrelation; default is just 1 year
                     nBoot = 10000, 
                     benchmarks = c(0.25, 0.5))

modelCI$CI

wd_data_regions <- wd_data_regions_fun(wd_root = wd_X_Drive1_PROJECTS)










