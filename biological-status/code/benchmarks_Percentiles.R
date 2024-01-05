
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
wd_project_dropbox <- wds_l$wd_project_dropbox
wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

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
  regions_df$Haida_Gwaii,
  regions_df$Central_coast)

region <- regions_df$Columbia

# all the regions
region <- as.character(regions_df[1,])

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "CK"],    
             species_acronym_df$species_name[species_acronym_df$species_acro == "SX"])

species <- species_acronym_df$species_name[species_acronym_df$species_acro == "SX"]

# If we do not specify the species: all the species that have a _SRdata files are 
# returned: 
# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- T

#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

#' Import the cuspawnerabundance.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' To calculating current spawner abundance for biostatus assessment
fromDatabase <- F
update_file_csv <- F

cuspawnerabundance <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[2],
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)

#' Import the conservationunits_decoder.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

nrow(unique(conservationunits_decoder[,c("region","species_name","cu_name_pse")]))

# Import the spawner_abundance.csv, downloaded from SPS work. OLDER CODE
# spawner_abundance_path <- paste0(wd_X_Drive1_PROJECTS,"/",wd_project_dropbox,"/data")
# spawner_abundance <- read.csv(paste0(spawner_abundance_path,"/spawner_abundance.csv"),header = T)
# head(spawner_abundance)
# unique(spawner_abundance$species_name)

# The benchmarks to use
benchmarks <- c(0.25, 0.5, 0.75) # was c(0.25, 0.5)

# last year to calculate current spawner abundance
yearCurrentAbundance <- 2021

# 
nBoot <- 5000

#
for(i_rg in 1:length(region)){
  
  # i_rg <- 1
  
  region_i <- gsub("_"," ",region[i_rg])
  if(region_i == "Central coast"){
    region_i <- "Central Coast"
  }
  
  if(region[i_rg] == "Vancouver Island & Mainland Inlets"){
    regionName <- "VIMI"
  }else{
    regionName <- regionName <- gsub(" ","_",region[i_rg])
  }
  
  cuspawnerabundance_rg <- cuspawnerabundance[cuspawnerabundance$region == region_i,]
  
  if(species_all){
    species <- unique(cuspawnerabundance_rg$species_name)
  }else{
    # return the full name of the species
    # species <- sapply(X = species, FUN = function(x){
    #   output <- species_acronym_df$species_name[species_acronym_df$species_acro == x]
    #   return(output)
    # })
  }
  
  #' TODO: simplify this messy work around for species and species_acro
  # remove Steelhead
  species <- species[species != "Steelhead"]

  species_acro <- sapply(X = species,FUN = function(sp){
    species_acronym_df$species_acro[species_acronym_df$species_name == sp]
  })
  
  species_acro <- unique(species_acro)
  
  if(sum(!is.na(cuspawnerabundance_rg$estimated_count)) == 0){
    
    print(paste0("*** There is no data in for salmon in ",region[i_rg]," ***"))
    
  }else{
    
    for(i_sp in 1:length(unique(species_acro))){
      
      # i_sp <- 1
      
      speciesAcroHere <- unique(species_acro)[i_sp]
      species_i <- species_acronym_df$species_name[species_acronym_df$species_acro %in% speciesAcroHere]
      
      # species_i <- species[i_sp]
      #
      # species_i <- gsub(pattern = "Lake ",replacement = "",x = species_i)  # for sockeye
      # species_i <- gsub(pattern = "River ",replacement = "",x = species_i) # for sockeye
      # species_i <- gsub(pattern = "\\s*\\(odd\\)",replacement = "",x = species_i) # for Pink
      # species_i <- gsub(pattern = "\\s*\\(even\\)",replacement = "",x = species_i)# for Pink
      
      # subset cuspawnerabundance_rg 
      # cuspawnerabundance_rg_sp <- cuspawnerabundance_rg[cuspawnerabundance_rg$species_name == species[i_sp],]
      # cuspawnerabundance_rg_sp <- cuspawnerabundance_rg[species_i_luc,cuspawnerabundance_rg$species_name),]
      
      cuspawnerabundance_rg_sp <- cuspawnerabundance_rg[cuspawnerabundance_rg$species_name %in% species_i,]
      
      # in case the species was selected by the user it is not present in that region:
      if(nrow(cuspawnerabundance_rg_sp) == 0){
        
        print(paste0("The species ",species_i," is not present in ",region_i," in the dataset used."))
        
      }else{
        
        # find the CUs present
        CUs <- unique(cuspawnerabundance_rg_sp$cu_name_pse)
        
        # find the corresponding cuid 
        cuids <- sapply(X = CUs, function(cu){
          # cu <- CUs[6]
          conservationunits_decoder_cut <- conservationunits_decoder[conservationunits_decoder$region == region[i_rg] &
                                                                     conservationunits_decoder$species_name %in% species_i & 
                                                                     conservationunits_decoder$cu_name_pse == cu,]
          
          # we could probably simply use pooledcuid
          if(nrow(conservationunits_decoder_cut) == 1){
            out <- conservationunits_decoder_cut$cuid
          }else{
            out <- unique(conservationunits_decoder_cut$pooledcuid)
          }

          if(length(out) == 0){
            print(paste("There is no cuid for:",region[i_rg],"-",species_i,"-",cu))
          }
          return(out)
        })
        
        # create a dataframe to retain the benchmark information for all the CUs of
        # the species in the region
        benchSummary_region_species_df <- NULL
        
        biologicalStatus_region_species_df <- NULL
        
        for(i_cu in 1:length(CUs)){
          
          # i_cu <- 1
          
          # subset cuspawnerabundance_rg_sp
          cuspawnerabundance_rg_sp_cu <- cuspawnerabundance_rg_sp[cuspawnerabundance_rg_sp$cu_name_pse == CUs[i_cu],]
          
          # get the count
          spawnerAbundance <- cuspawnerabundance_rg_sp_cu$estimated_count
          spawnerAbundance[spawnerAbundance <= 0] <- NA
          names(spawnerAbundance) <- cuspawnerabundance_rg_sp_cu$year
          
          comment <- ""
          
          if(sum(!is.na(spawnerAbundance)) == 0){
            comment <- paste0("There is no estimated_count data for ",species_i[1]," - ",CUs[i_cu],", in ",regionName)
          }
          
          # This does not work so we remove the Nas instead of the odd or even years
          # if(speciesAcroHere == "PK"){  # numLags has to be set to 2 year and not one because of the odd and even CUs
          #   numLags <- 2
          # }else{
          #   numLags <- 1
          # }
          numLags <- 1
          modelCI <- modelBoot(series = spawnerAbundance, 
                               numLags = numLags, # numLags is the lag for the autocorrelation; default is just 1 year
                               nBoot = nBoot,
                               benchmarks = benchmarks)  # to be able to compare
          
          # place the information a dataframe
          
          benchSummary_df <- data.frame(region = rep(region[i_rg],length(benchmarks)),
                                        species = rep(speciesAcroHere,length(benchmarks)),
                                        cuid = rep(cuids[i_cu],length(benchmarks)),
                                        CU = rep(CUs[i_cu],length(benchmarks)),
                                        benchmark = paste0("benchmark_",benchmarks), # c('lower','upper'),
                                        method = rep('HS_percentiles',length(benchmarks)))
          
          benchSummary_df$m <- c(quantile(spawnerAbundance, 0.25, na.rm = T),
                                 quantile(spawnerAbundance, 0.5, na.rm = T),
                                 quantile(spawnerAbundance, 0.75, na.rm = T))
          benchSummary_df$m_sim <- modelCI$m
          benchSummary_df$CI025 <- modelCI$CI[1,]
          benchSummary_df$CI975 <- modelCI$CI[2,]
          benchSummary_df$benchmarks <- rep(paste(benchmarks,collapse = "-"),length(benchmarks))
          benchSummary_df$dataPointNb <- sum(!is.na(spawnerAbundance))
          benchSummary_df$comment <- comment
          
          if(is.null(benchSummary_region_species_df)){
            benchSummary_region_species_df <- benchSummary_df
          }else{
            benchSummary_region_species_df <- rbind(benchSummary_region_species_df,
                                                    benchSummary_df)
          }
          
          #------------------------------------------------------------------
          #' biological status probability with the average current spawner 
          #' abundance over the last generation
          #------------------------------------------------------------------
          
          conservationunits_decoder_rg_sp_cu <- conservationunits_decoder[conservationunits_decoder$region == region_i &
                                                                            conservationunits_decoder$species_name %in% species_i &
                                                                            conservationunits_decoder$cu_name_pse == CUs[i_cu],]
          
          if(nrow(conservationunits_decoder_rg_sp_cu) == 0){
            print("This CUS is not found in conservationunits_decoder:")
            print(paste(region[i_rg],species[i_sp],CUs[i_cu]))
            cat("\n")
          }else if(nrow(conservationunits_decoder_rg_sp_cu) > 1){
            if(length(unique(conservationunits_decoder_rg_sp_cu$pooledcuid)) > 1){ # if == 1 there are all the same CUs for PSF
              print("There are multiple CUs with that name who don't have the same pooledcuid, the 1st row is used")
              print(paste(region[i_rg],species[i_sp],CUs[i_cu]))
              print(conservationunits_decoder_rg_sp_cu)
              cat("\n")
            }
            conservationunits_decoder_rg_sp_cu <- conservationunits_decoder_rg_sp_cu[1,,drop = F]
          }
          
          # keep track of the different versions of the CU names
          CUname_pse <- conservationunits_decoder_rg_sp_cu$cu_name_pse
          CUname_dfo <- conservationunits_decoder_rg_sp_cu$cu_name_dfo
          
          CU_genLength <- conservationunits_decoder_rg_sp_cu$gen_length[1]
          CU_genLength_available <- TRUE
          
          if(is.na(CU_genLength)){
            # From Tech-Report: 
            #' "Where CU-specific data on age-at-return are unavailable, we assume 
            #' generation lengths of 
            #' - 5 years for Chinook CUs, 
            #' - 4 years for coho CUs, 
            #' - 4 years for chum CUs, 
            #' - 4 years for sockeye CUs"   --> BUT 5 years for the Cus in the northen region
            #' - Pink salmon have a consistent 2-year age-at-return and because 
            #' even- and odd-year lineages are considered separate CUs, the most 
            #' recent spawner abundance is simply the most recent year’s estimated 
            #' spawner abundance for this species
            CU_genLength <- generationLengthEstiamte_df$genLength[generationLengthEstiamte_df$species %in% speciesName]
            CU_genLength_available <- FALSE
            print(paste("No generation length for:",region[i_rg],species[i_sp],CUname))
          }
          
          if(sum(!is.na(spawnerAbundance)) == 0){
            # spawnerAbundance <- SRm$S[, i]
            # print("The following CU has only NAs for spawner abundance:")
            # print(paste(region[i_rg],species[i_sp],CUname))
            currentSpawnerData_available <- F
            yrInitial <- NA
            yrFinal <- NA
            #' Eric: if estimated is not available, we can't apply spawner-recruit 
            #' benchmarks.
          }else{
            currentSpawnerData_available <- T
            yrFinal <- tail(as.numeric(names(spawnerAbundance[!is.na(spawnerAbundance)])),1)
          }
          
          #' add number of years to spawnerAbundance until yearCurrentAbundance if needed
          yearsHere <- as.numeric(names(spawnerAbundance))
          yearLast <- max(yearsHere)
          while(yearLast < yearCurrentAbundance){
            yearLast <- yearLast + 1
            datapointHere <- NA
            names(datapointHere) <- yearLast
            spawnerAbundance <- c(spawnerAbundance,datapointHere)
          }
          
          # calculate current spawner abundance = the geometric mean over the last generation
          spawnerAbundance_lastGen <- tail(spawnerAbundance,CU_genLength)
          spawnerAbundance_lastGen_m <- mean_geom_fun(x = spawnerAbundance_lastGen)
          spawnerAbundance_lastGen_dataPointNb <- sum(!is.na(spawnerAbundance_lastGen))
          
          if(sum(!is.na(spawnerAbundance_lastGen)) == 0){
            currentSpawnerData_availableRecentEnough <- F
            # the 1st year with data for the calculation of the current spawner abundance
            yrInitial <- NA
          }else{
            currentSpawnerData_availableRecentEnough <- T
            # the 1st year with data for the calculation of the current spawner abundance
            yrInitial <- as.numeric(names(spawnerAbundance_lastGen[!is.na(spawnerAbundance_lastGen)]))[1]
          }
          
          # determine the number of time this CUs fall under the Red, Amber and Green 
          # status over all the simulations
          if(currentSpawnerData_available & currentSpawnerData_availableRecentEnough){
            
            status_percent_075 <- c()
            status_percent_05 <- c()
            
            for(j in 1:nrow(modelCI$benchmarkBoot)){
              # j <- 1
              LB <- modelCI$benchmarkBoot[j,"benchmark_0.25"]
              UB_075 <- modelCI$benchmarkBoot[j,"benchmark_0.75"]
              UB_05 <- modelCI$benchmarkBoot[j,"benchmark_0.5"]
              #
              if(!is.na(LB) & !is.na(UB_05) & !is.na(UB_075)){
                
                if(spawnerAbundance_lastGen_m <= LB){
                  status_percent_075 <- c(status_percent_075,'red')
                  status_percent_05 <- c(status_percent_05,'red')
                  
                }else if(spawnerAbundance_lastGen_m <= UB_05){
                  status_percent_075 <- c(status_percent_075,'amber')
                  status_percent_05 <- c(status_percent_05,'amber')
                  
                }else if(spawnerAbundance_lastGen_m <= UB_075){
                  status_percent_075 <- c(status_percent_075,'amber')
                  status_percent_05 <- c(status_percent_05,'green')
                  
                }else{
                  status_percent_075 <- c(status_percent_075,'green')
                  status_percent_05 <- c(status_percent_05,'green')
                }
              }else{
                status_percent_075 <- c(status_percent_075,NA)
                status_percent_05 <- c(status_percent_05,NA)
              }
            }
            
            status_percent_05 <- status_percent_05[!is.na(status_percent_05)]
            status_percent_075 <- status_percent_075[!is.na(status_percent_075)]
            
            status_percent_prob_05 <- round(table(factor(status_percent_05,
                                                           levels = c("red","amber","green")))/length(status_percent_05)*100,4)
            status_percent_prob_075 <- round(table(factor(status_percent_075,
                                                           levels = c("red","amber","green")))/length(status_percent_075)*100,4)
            
            comment <- ""
            
          }else{
            
            status_percent_prob_05 <- rep(NA,3)
            status_percent_prob_075 <- rep(NA,3)
            names(status_percent_prob_05) <- c("red","amber","green")
            names(status_percent_prob_075) <- c("red","amber","green")
            
            if(!currentSpawnerData_available){
              comment <- "Only NAs in cuspawnerabundance.csv for this CU"
            }else if(!currentSpawnerData_availableRecentEnough){
              comment <- paste0("Not recent enough data: last year with data is ",yrFinal," while generation length = ",CU_genLength," years and current year = ",yearCurrentAbundance)
            }
          }
          
          biologicalStatus_df <- data.frame(region = region[i_rg],
                                            species = speciesAcroHere,
                                            cuid = cuids[i_cu],
                                            CU = CUs[i_cu])
          biologicalStatus_df$CU_pse <- CUname_pse
          biologicalStatus_df$CU_dfo <- CUname_dfo
          biologicalStatus_df$current_spawner_abundance <- spawnerAbundance_lastGen_m
          biologicalStatus_df$year_last <- yrFinal
          biologicalStatus_df$year_first <- yrInitial
          biologicalStatus_df$genLength <- CU_genLength
          biologicalStatus_df$genLength_available <- CU_genLength_available
          biologicalStatus_df$dataPointNb <- sum(!is.na(spawnerAbundance))
          biologicalStatus_df$genLength_dataPointNb <- spawnerAbundance_lastGen_dataPointNb
          biologicalStatus_df$status_percent_05_red <- status_percent_prob_05["red"]
          biologicalStatus_df$status_percent_05_amber <- status_percent_prob_05["amber"]
          biologicalStatus_df$status_percent_05_green <- status_percent_prob_05["green"]
          biologicalStatus_df$status_percent_075_red <- status_percent_prob_075["red"]
          biologicalStatus_df$status_percent_075_amber <- status_percent_prob_075["amber"]
          biologicalStatus_df$status_percent_075_green <- status_percent_prob_075["green"]
          biologicalStatus_df$comment <- comment
          
          if(is.null(biologicalStatus_region_species_df)){
            biologicalStatus_region_species_df <- biologicalStatus_df
          }else{
            biologicalStatus_region_species_df <- rbind(biologicalStatus_region_species_df,
                                                        biologicalStatus_df)
          }

        } # end of loop for the CUs
        
        print(paste0("*** ",regionName,"_",speciesAcroHere," done ***"))
        write.csv(x = benchSummary_region_species_df, 
                  file = paste0(wd_output,"/",regionName,"_",speciesAcroHere,"_benchmarks_summary_percentiles.csv"),
                  row.names = F) # keep region[i_rg] and not region_i because of "Central coast" is used to name the other files and not "Central Coast"
        
        write.csv(x = biologicalStatus_region_species_df, 
                  file = paste0(wd_output,"/",regionName,"_",speciesAcroHere,"_biological_status_percentiles.csv"),
                  row.names = F)
        
      } # if there is data for this species
    } # end of loop for the species
  } # if there is enough data
} # end of the loop for the regions




