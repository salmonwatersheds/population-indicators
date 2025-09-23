
#'******************************************************************************
#' The goal of the script is to calculate the benchmarks from using the percentile
#' approach on the history spawner (HS) data.
#' 
#' #' Files imported (from dropbox):
#' - cuspawnerabundance (from database)
#' - conservationunits_decoder (from database)
#' 
#' Files produced: 
#' - output/REGION_SPECIES_benchmarks_summary_percentiles.csv
#' - output/REGION_SPECIES_biological_status_percentiles.csv
#' 
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
region <- regions_df$Yukon

# multiple regions:
region <- c(
  regions_df$WVI,
  regions_df$EVIMI)

# all the regions
region <- as.character(regions_df[1,])

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
cond_sp <- species_acronym_df$species_qualified_simple == "CK" | 
  species_acronym_df$species_qualified_simple == "SE"
cond_sp <- species_acronym_df$species_qualified_simple == "CK"
species <- "Chum"

# For all species
species <- species_acronym_df$species_name_simple |> unique()

#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()


fromDatabase <- update_file_csv <- F

#' Import the conservationunits_decoder.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- datasets_database_fun(nameDataSet = "conservationunits_decoder.csv",
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

# remove the SMU-related information because that causes issues lower (one CU belongs to 2 SMUs)
nrow(conservationunits_decoder) # 470
conservationunits_decoder <- conservationunits_decoder[,!grepl("smu",colnames(conservationunits_decoder))]
conservationunits_decoder <- unique(conservationunits_decoder)
nrow(conservationunits_decoder) # 469

#' Import the cuspawnerabundance.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' To calculating current spawner abundance for biostatus assessment
cuspawnerabundance <- datasets_database_fun(nameDataSet = "cuspawnerabundance.csv",
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)

#'* TEMOPORARY *
#' Replace the Yukon data with the one freshly produced in the Yukon-data folder
wd_yukon <- gsub("population-indicators/data-input",
                 "population-data/Yukon-data",
                 wd_pop_indic_data_input_dropbox)
spwAbd_yukon <- import_mostRecent_file_fun(wd = paste0(wd_yukon,"/output/archive/"),
                                           pattern = "dataset1_spawner-abundance")

spwAbd_yukon$observed_count <- NA
spwAbd_yukon$species_qualified <- NA
for(cuid in unique(spwAbd_yukon$cuid)){
  cond <- conservationunits_decoder$cuid == cuid
  sq <- conservationunits_decoder$species_qualified[cond] |> unique()
  
  cond <- spwAbd_yukon$cuid == cuid
  spwAbd_yukon$species_qualified[cond] <- sq
}

cond <- cuspawnerabundance$region == "Yukon"
cuspawnerabundance[cond,]
cuspawnerabundance <- cuspawnerabundance[!cond,]

cuspawnerabundance <- rbind(cuspawnerabundance,
                            spwAbd_yukon[,colnames(cuspawnerabundance)])


# The benchmarks to use
benchmarks <- c(0.25, 0.5, 0.75) # was c(0.25, 0.5)

#' Last year to calculate current spawner abundance.
#' If NA then the current spawner abundance is calculated considering the range 
#' between the last year with available data (e.g., yr_last) and
#' yr_last - generation length + 1.
#' If not NA (e.g., yr_last = 2021), current spawner abundance is calculated 
#' in the range yr_last : (yr_last - generation length + 1) even if there is no 
#' data available in more recent years.
yearCurrentAbundance <- NA # was 2021

# Number of iterations for the bootstrapping process to calculate thresholds
nBoot <- 5000

print_fig <- T

#
for(i_rg in 1:length(region)){
  
  # i_rg <- 7
  cond_csa_rg <- cuspawnerabundance$region == region[i_rg]
  
  if(region[i_rg] == "West Vancouver Island"){
    regionName <- "WVI"
  }else if(region[i_rg] == "East Vancouver Island & Mainland Inlets"){
    regionName <- "EVIMI"
  }else{
    regionName <- regionName <- gsub(" ","_",region[i_rg])
  }
  
  # if(region[i_rg] == "Vancouver Island & Mainland Inlets"){
  #   regionName <- "VIMI"
  # }

  species_acro <- sapply(X = species,FUN = function(sp){
    cond <- species_acronym_df$species_name == sp
    return(species_acronym_df$species_qualified_simple[cond])
  })
  
  cond <- all(is.na(cuspawnerabundance$estimated_count[cond_csa_rg]))
  if(cond){
    
    print(paste0("*** There is no data in for salmon in ",region[i_rg]," ***"))
    
  }else{
    
    for(i_sp in 1:length(species_acro)){
      
      # i_sp <- 4
      
      speciesAcroHere <- species_acro[i_sp]
      
      cond <- species_acronym_df$species_qualified_simple == speciesAcroHere
      speciesHere <- species_acronym_df$species_name[cond] |> unique()
      
      cond_csa_rg_sp <- cond_csa_rg & cuspawnerabundance$species_name %in% speciesHere

      # in case the species was selected by the user it is not present in that region:
      if(sum(cond_csa_rg_sp) == 0){
        
        print(paste0("The species ",speciesHere," is not present in ",region[i_rg] ," in the dataset used."))
        
      }else{
        
        # find the CUs present
        CUs <- unique(cuspawnerabundance$cu_name_pse[cond_csa_rg_sp])
        
        # find the corresponding cuid
        cuids <-  unique(cuspawnerabundance$cuid[cond_csa_rg_sp]) # 
        
        # create a data frame to retain the benchmark information for all the CUs of
        # the species in the region
        benchSummary_region_species_df <- NULL
        
        biologicalStatus_region_species_df <- NULL
        
        for(i_cu in 1:length(CUs)){
          
          # i_cu <- 1
          
          cond <- conservationunits_decoder$cuid == cuids[i_cu]
          species_qualifyHere <- conservationunits_decoder$species_qualified[cond]
          species_nameHere <- conservationunits_decoder$species_name[cond] |> unique()
          
          # TEMPORARY
          # simplify species_name as in PSE data meeting December 11 2024
          # To remove eventually when conservationunits_decoder$species_name 
          # changed as well.
          if(grepl("[s|S]ockeye",species_nameHere)){
            
            species_nameHere <- "Sockeye"
            
          }else if(grepl("Pink",species_nameHere)){
            
            species_nameHere <- "Pink"
            
          }
          
          # subset cuspawnerabundance_rg_sp
          cond_csa_rg_sp_cu <- cond_csa_rg_sp & cuspawnerabundance$cuid == cuids[i_cu]
          
          # get the count
          spawnerAbundance <- cuspawnerabundance$estimated_count[cond_csa_rg_sp_cu]
          spawnerAbundance[spawnerAbundance < 0] <- NA
          spawnerAbundance[spawnerAbundance == 0] <- 1
          names(spawnerAbundance) <- cuspawnerabundance$year[cond_csa_rg_sp_cu]
          
          # Simulate the time series to obtain the 95% CI for the percentile 
          # benchmarks
          numLags <- 1
          modelCI <- modelBoot(series = spawnerAbundance, 
                               numLags = numLags, # numLags is the lag for the autocorrelation; default is just 1 year
                               nBoot = nBoot,
                               benchmarks = benchmarks,   # to be able to compare
                               remove_NAs_tails = T)      # the NAs are 
          
          # place the information a dataframe
          benchSummary_df <- data.frame(region = rep(region[i_rg],length(benchmarks)),
                                        species_name = rep(species_nameHere,length(benchmarks)),
                                        species_qualified = rep(species_qualifyHere,length(benchmarks)),
                                        cuid = rep(cuids[i_cu],length(benchmarks)),
                                        cu_name_pse = rep(CUs[i_cu],length(benchmarks)),
                                        benchmark = paste0("benchmark_",benchmarks), # c('lower','upper'),
                                        method = rep('percentile',length(benchmarks))) # was HS_percentiles
          
          benchSummary_df$m <- c(quantile(spawnerAbundance, 0.25, na.rm = T),
                                 quantile(spawnerAbundance, 0.5, na.rm = T),
                                 quantile(spawnerAbundance, 0.75, na.rm = T))
          benchSummary_df$m_sim <- modelCI$m
          benchSummary_df$CI025 <- modelCI$CI[1,]
          benchSummary_df$CI975 <- modelCI$CI[2,]
          benchSummary_df$benchmarks <- rep(paste(benchmarks,collapse = "-"),length(benchmarks))
          benchSummary_df$dataPointNb <- sum(!is.na(spawnerAbundance))

          
          #------------------------------------------------------------------
          #' biological status probability with the average current spawner 
          #' abundance over the last generation
          #------------------------------------------------------------------
          cond_cud_cu <- conservationunits_decoder$cuid == cuids[i_cu]

          if(!any(cond_cud_cu)){
            print("This CUS is not found in conservationunits_decoder: BREAK")
            print(paste(region[i_rg],species[i_sp],CUs[i_cu],cuid[i_cu]))
            break
          }else if(sum(cond_cud_cu) > 1){
            print("There are multiple CUs with that CUID, TO CHECK BREAK")
            print(paste(region[i_rg],species[i_sp],CUs[i_cu]))
            print(conservationunits_decoder[cond_cud_cu,])
            break
          }
          
          # keep track of the different versions of the CU names
          cu_name_pse <- conservationunits_decoder$cu_name_pse[cond_cud_cu]
          cu_name_dfo <- conservationunits_decoder$cu_name_dfo[cond_cud_cu]
          
          CU_genLength <- conservationunits_decoder$gen_length[cond_cud_cu]
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
            # cond <- generationLengthEstiamte_df$species %in% speciesName # generationLengthEstiamte_df is created in functions_general.R
            # CU_genLength <- generationLengthEstiamte_df$genLength[cond] 
            # CU_genLength_available <- FALSE
            # print(paste("No generation length for:",region[i_rg],species[i_sp],CUname))
            print(paste("No generation length for:",region[i_rg],species[i_sp],CUname,"BREAK"))
            break
          }
          
          # Calculate current spawner abundance:
          # yearCurrentAbundance should be NA so it is calculated from the most 
          # recent year with data
          csa_df <- current_spawner_abundance_fun(cuids = cuids[i_cu], 
                                                  cuspawnerabundance = cuspawnerabundance, 
                                                  yearCurrentAbundance = yearCurrentAbundance, 
                                                  CU_genLength = CU_genLength)
          yrInitial <- csa_df$yr_withData_start   #TODO: rename/add "curr_spw"or something because this is confusing with the entire timeseries
          yrFinal <-  csa_df$yr_withData_end
          currentSpawnerData_available <- csa_df$curr_spw_available
          csa <- csa_df$curr_spw_abun
          spawnerAbundance_lastGen_dataPointNb <- csa_df$dataPointNb
          currentSpawnerData_availableRecentEnough <- csa_df$curr_spw_availableRecentEnough
        
          # determine the number of time this CUs fall under the Red, Amber and Green 
          # status over all the simulations
          if(currentSpawnerData_available & currentSpawnerData_availableRecentEnough){
            
            
            #' Determine the biostatus by comparing the percentile benchmarks 
            #' to current spawner abundance
            csa <- csa_df$curr_spw_abun
            cond_025 <- benchSummary_df$benchmark == "benchmark_0.25"
            cond_050 <- benchSummary_df$benchmark == "benchmark_0.5"
            cond_075 <- benchSummary_df$benchmark == "benchmark_0.75"
            bench_025 <- benchSummary_df$m[cond_025]
            bench_050 <- benchSummary_df$m[cond_050]
            bench_075 <- benchSummary_df$m[cond_075]
            status_percent075 <- status_percent050 <- NA
            if(csa <= bench_025){
              status_percent075 <- status_percent050 <- "poor"
            }else if(csa <= bench_050){
              status_percent075 <- status_percent050 <- "fair"
            }else if(csa > bench_050 & csa <= bench_075){
              status_percent075 <- "fair"
              status_percent050 <- "good"
            }else{
              status_percent075 <- status_percent050 <- "good"
            }
            
            comment <- "Biostatus calculated"
            
            #' This part of the code below determine the probability of the red, 
            #' yellow and green status using the modelCI$benchmarkBoot.
            #' THESE PROBABILITIES ARE NOT USED TO PROVIDE BIOSTATUS.
            #' We add those as columns in dataset101_biologocal_status.csv but
            #' just for information.

            status_percent_075 <- c()
            status_percent_05 <- c()
            
            for(j in 1:nrow(modelCI$benchmarkBoot)){
              # j <- 1
              LB <- modelCI$benchmarkBoot[j,"benchmark_0.25"]
              UB_075 <- modelCI$benchmarkBoot[j,"benchmark_0.75"]
              UB_05 <- modelCI$benchmarkBoot[j,"benchmark_0.5"]
              #
              if(!is.na(LB) & !is.na(UB_05) & !is.na(UB_075)){
                
                if(csa <= LB){
                  status_percent_075 <- c(status_percent_075,'red')
                  status_percent_05 <- c(status_percent_05,'red')
                  
                }else if(csa <= UB_05){
                  status_percent_075 <- c(status_percent_075,'amber')
                  status_percent_05 <- c(status_percent_05,'amber')
                  
                }else if(csa <= UB_075){
                  status_percent_075 <- c(status_percent_075,'amber')
                  status_percent_05 <- c(status_percent_05,'green')
                  
                }else{
                  status_percent_075 <- c(status_percent_075,'green')
                  status_percent_05 <- c(status_percent_05,'green')
                }
              }else{ # not needed I think
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
            
            # Figure
            if(print_fig){
              CUhere <- gsub(pattern = "/",'-',CUs[i_cu])  # in case "/" is in the CU's name
              CUhere <- gsub(pattern = "\\(","",CUhere)
              CUhere <- gsub(pattern = "\\)","",CUhere)
              
              if(nchar(CUhere) > 25){   # the figure can't print if the number of characters is too large
                CUhere <- substr(x = CUhere,start =  1,stop = 25) 
              }
              
              pathFile <- paste0(wd_figures,"/",regionName,"_",species_qualifyHere,"_",CUhere,
                                 "_benchmark_percentiles_hist.jpeg")
              
              jpeg(file = pathFile, width = 18, height = 14, units = "cm", res = 300)
            }
            maxS <- max(modelCI$benchmarkBoot[,"benchmark_0.5"], na.rm = T)
            
            hl <- hist(x = modelCI$benchmarkBoot[,"benchmark_0.25"], 
                       col = paste0(status_cols['red'], 50), border = NA,
                       breaks = seq(0, maxS, maxS/50), xlim = c(0, maxS), 
                       main = paste0(regionName," - ",species_qualifyHere," - ",CUs[i_cu]," - (",cuids[i_cu],")"), 
                       freq = FALSE, xlab = "Number of fish")
            
            hu_05 <- hist(x = modelCI$benchmarkBoot[,"benchmark_0.5"], 
                          col = paste0(status_cols['green'], 50), border = NA, 
                          breaks = seq(0, maxS, maxS/50), add = TRUE, 
                          freq = FALSE)
            
            segments(x0 = benchSummary_df$m[benchSummary_df$benchmark == 'benchmark_0.25'], 
                     x1 = benchSummary_df$m[benchSummary_df$benchmark == 'benchmark_0.25'], 
                     y0 = 0, y1 = 1^10, lwd = 2, col = status_cols['red'])
            segments(x0 = benchSummary_df$m[benchSummary_df$benchmark == 'benchmark_0.5'], 
                     x1 = benchSummary_df$m[benchSummary_df$benchmark == 'benchmark_0.5'], 
                     y0 = 0, y1 = 1^10, lwd = 2, col = status_cols['green'])
            segments(x0 = csa, x1 = csa, 
                     y0 = 0, y1 = 1^10, lwd = 2)
            
            if(print_fig){
              dev.off()
            }
            
          }else{
            
            status_percent075 <- status_percent050 <- NA
            status_percent_prob_05 <- status_percent_prob_075 <- rep(NA,3)
            names(status_percent_prob_05) <- names(status_percent_prob_05) <- c("red","amber","green")
            
            if(!currentSpawnerData_available){
              comment <- paste0("No estimated_count data in cuspawnerabundance.csv")
            }else if(!currentSpawnerData_availableRecentEnough){
              comment <- paste0("Not recent enough data: last year with data is ",yrFinal," while generation length = ",CU_genLength," years and current year = ",yearCurrentAbundance)
            }
          }
          
          biologicalStatus_df <- data.frame(region = region[i_rg],
                                            species_name = species_nameHere,
                                            species_qualified = species_qualifyHere,
                                            cuid = cuids[i_cu],
                                            cu_name_pse = cu_name_pse)
          
          biologicalStatus_df$current_spawner_abundance <- csa
          biologicalStatus_df$yr_withData_start <- yrInitial
          biologicalStatus_df$yr_withData_end <- yrFinal
          biologicalStatus_df$yr_end_imposed <- yearCurrentAbundance
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
          biologicalStatus_df$status_percent050 <- status_percent050 
          biologicalStatus_df$status_percent075 <- status_percent075
          biologicalStatus_df$comment <- comment
          
          if(is.null(biologicalStatus_region_species_df)){
            biologicalStatus_region_species_df <- biologicalStatus_df
          }else{
            biologicalStatus_region_species_df <- rbind(biologicalStatus_region_species_df,
                                                        biologicalStatus_df)
          }
          
          # add the comment to benchSummary_df
          benchSummary_df$comment <- comment
          
          if(is.null(benchSummary_region_species_df)){
            benchSummary_region_species_df <- benchSummary_df
          }else{
            benchSummary_region_species_df <- rbind(benchSummary_region_species_df,
                                                    benchSummary_df)
          }
        } # end of loop for the CUs
        
        print(paste0("*** ",regionName,"_",speciesAcroHere," done ***"))
        
        write.csv(x = benchSummary_region_species_df, 
                  file = paste0(wd_output,"/intermediate/",regionName,"_",speciesAcroHere,"_benchmarks_summary_percentiles.csv"),
                  row.names = F) # keep region[i_rg] and not region[i_rg]  because of "Central coast" is used to name the other files and not "Central Coast"
        
        write.csv(x = biologicalStatus_region_species_df, 
                  file = paste0(wd_output,"/intermediate/",regionName,"_",speciesAcroHere,"_biological_status_percentiles.csv"),
                  row.names = F)
        
      } # if there is data for this species
    } # end of loop for the species
  } # if there is enough data
} # end of the loop for the regions

