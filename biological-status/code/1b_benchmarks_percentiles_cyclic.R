
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
region <- regions_df$Fraser
region <- regions_df$Yukon
region <- regions_df$Transboundary

# multiple regions:
region <- c(
  regions_df$Fraser,
  regions_df$Yukon,
  regions_df$Nass)

region <- c(
  regions_df$Haida_Gwaii,
  regions_df$Central_coast)

region <- regions_df$Central_coast

# all the regions
region <- as.character(regions_df[1,])

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "CK"],    
             species_acronym_df$species_name[species_acronym_df$species_acro == "SX"])

species <- species_acronym_df$species_name[species_acronym_df$species_acro == "CO"]

# If we do not specify the species: all the species that have a _SRdata files are 
# returned: 
# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- F

#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

#' Import the cuspawnerabundance.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' To calculating current spawner abundance for biostatus assessment
fromDatabase <- update_file_csv <- F

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

nrow(unique(conservationunits_decoder[,c("region","species_name","cu_name_pse")])) # 466 467

#'* Select the cyclic CUs *
cond_cycl <- grepl("cyclic",conservationunits_decoder$cu_name_pse)
cuid_cycl <- conservationunits_decoder$cuid[cond_cycl]
conservationunits_decoder[cond_cycl,]

region <- unique(conservationunits_decoder$region[cond_cycl])
species <- unique(conservationunits_decoder$species_name[cond_cycl])

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
  }
  
  # remove Steelhead
  # species <- species[species != "Steelhead"]

  species_acro <- sapply(X = species,FUN = function(sp){
    species_acronym_df$species_acro[species_acronym_df$species_name == sp]
  }) |> unique()
  
  if(sum(!is.na(cuspawnerabundance_rg$estimated_count)) == 0){
    
    print(paste0("*** There is no data in for salmon in ",region[i_rg]," ***"))
    
  }else{
    
    for(i_sp in 1:length(unique(species_acro))){
      
      # i_sp <- 1
      
      speciesAcroHere <- unique(species_acro)[i_sp]
      cond <- species_acronym_df$species_acro %in% speciesAcroHere
      species_i <- species_acronym_df$species_name[cond]
      species_qualify_i <- species_acronym_df$species_acro2_details[cond] |> unique()
      
      cond <- cuspawnerabundance_rg$species_name %in% species_i &
        cuspawnerabundance_rg$cuid %in% cuid_cycl
      cuspawnerabundance_rg_sp <- cuspawnerabundance_rg[cond,]
      
      # in case the species was selected by the user it is not present in that region:
      if(nrow(cuspawnerabundance_rg_sp) == 0){
        print(paste0("The species ",species_i," is not present in ",region_i," in the dataset used."))
        
      }else{
        
        # find the CUs present
        CUs <- unique(cuspawnerabundance_rg_sp$cu_name_pse)
        
        # find the corresponding cuid
        cuids <-  unique(cuspawnerabundance_rg_sp$cuid) # 
        
        # fnd the corresponding acronym
        species_abbr <- sapply(X = cuids, function(cuid){
          cond <- conservationunits_decoder$cuid == cuid
          return(conservationunits_decoder$species_abbr[cond])
        })
        
        # create a dataframe to retain the benchmark information for all the CUs of
        # the species in the region
        benchSummary_region_species_df <- NULL
        biologicalStatus_region_species_df <- NULL
        
        for(i_cu in 1:length(CUs)){
          
          # i_cu <- 1
          # i_cu <- which(cuids == 728)
          
          # subset cuspawnerabundance_rg_sp
          cuspawnerabundance_rg_sp_cu <- cuspawnerabundance_rg_sp[cuspawnerabundance_rg_sp$cu_name_pse == CUs[i_cu],]
          
          # get the count
          spawnerAbundance <- cuspawnerabundance_rg_sp_cu$estimated_count
          spawnerAbundance[spawnerAbundance <= 0] <- NA
          names(spawnerAbundance) <- cuspawnerabundance_rg_sp_cu$year
          
          comment <- ""
          
          # if(all(is.na(spawnerAbundance))){
          #   comment <- paste0("There is no estimated_count data for ",species_i[1]," - ",CUs[i_cu],", in ",regionName)
          # }
          
          # This does not work so we remove the Nas instead of the odd or even years
          # if(speciesAcroHere == "PK"){  # numLags has to be set to 2 year and not one because of the odd and even CUs
          #   numLags <- 2
          # }else{
          #   numLags <- 1
          # }
          numLags <- 1
          
          # Do the process for each cycle-line
          benchSummary_df_cl <- NULL
          spawnerAbundance_cl_l <- list()
          modelCI_cl <- list()
          for(cl in 1:4){
            i <- 1:length(spawnerAbundance)
            i <- i[(i + cl - 2) %% 4 == 0]
            spawnerAbundance_cl <- spawnerAbundance[i]
            
            spawnerAbundance_cl_l[[cl]] <- spawnerAbundance_cl
            
            modelCI <- modelBoot(series = spawnerAbundance_cl, 
                                 numLags = numLags, # numLags is the lag for the autocorrelation; default is just 1 year
                                 nBoot = nBoot,
                                 benchmarks = benchmarks)  # to be able to compare
            
            modelCI_cl[[cl]] <- modelCI
            
            # place the information a dataframe
            benchSummary_df <- data.frame(region = rep(region[i_rg],length(benchmarks)),
                                          species = rep(speciesAcroHere,length(benchmarks)),
                                          cuid = rep(cuids[i_cu],length(benchmarks)),
                                          CU = rep(CUs[i_cu],length(benchmarks)),
                                          benchmark = paste0("benchmark_",benchmarks), # c('lower','upper'),
                                          method = rep('percentile',length(benchmarks)), # HS_percentiles
                                          cycle_line = cl,
                                          cycle_line_current = F)
            
            benchSummary_df$m <- c(quantile(spawnerAbundance_cl, 0.25, na.rm = T),
                                   quantile(spawnerAbundance_cl, 0.5, na.rm = T),
                                   quantile(spawnerAbundance_cl, 0.75, na.rm = T))
            benchSummary_df$m_sim <- modelCI$m
            benchSummary_df$CI025 <- modelCI$CI[1,]
            benchSummary_df$CI975 <- modelCI$CI[2,]
            benchSummary_df$benchmarks <- rep(paste(benchmarks,collapse = "-"),length(benchmarks))
            benchSummary_df$dataPointNb <- sum(!is.na(spawnerAbundance_cl))
            benchSummary_df$comment <- comment
            
            if(is.null(benchSummary_df_cl)){
              benchSummary_df_cl <- benchSummary_df
            }else{
              benchSummary_df_cl <- rbind(benchSummary_df_cl,benchSummary_df)
            }
          }
          names(modelCI_cl) <- paste0("cycle-line_",1:4)
          names(spawnerAbundance_cl_l) <- 1:4
          
          if(is.null(benchSummary_region_species_df)){
            benchSummary_region_species_df <- benchSummary_df_cl
          }else{
            benchSummary_region_species_df <- rbind(benchSummary_region_species_df,
                                                    benchSummary_df_cl)
          }
          
          #------------------------------------------------------------------
          #' Biological status probability with the current spawner abundance 
          #' for each cycle line, which is the estimated spawner abundance for
          #' each each of the last cycle.
          #' Note that here we do not define current spawner abundance as the
          #' average spawner abundance over the last generation
          #------------------------------------------------------------------
          
          condition <- conservationunits_decoder$cuid == cuids[i_cu]
          conservationunits_decoder_rg_sp_cu <- conservationunits_decoder[condition,]
          
          if(nrow(conservationunits_decoder_rg_sp_cu) == 0){
            print("This CUS is not found in conservationunits_decoder BREAK")
            print(paste(region[i_rg],species[i_sp],CUs[i_cu]))
            cat("\n")
            break
          }else if(nrow(conservationunits_decoder_rg_sp_cu) > 1){
            if(length(unique(conservationunits_decoder_rg_sp_cu$pooledcuid)) > 1){ # if == 1 there are all the same CUs for PSF
              print("There are multiple CUs with that name who don't have the same pooledcuid BREAK")
              print(paste(region[i_rg],species[i_sp],CUs[i_cu]))
              print(conservationunits_decoder_rg_sp_cu)
              cat("\n")
              break
            }
            # conservationunits_decoder_rg_sp_cu <- conservationunits_decoder_rg_sp_cu[1,,drop = F]
          }
          
          # keep track of the different versions of the CU names
          CUname_pse <- conservationunits_decoder_rg_sp_cu$cu_name_pse
          CUname_dfo <- conservationunits_decoder_rg_sp_cu$cu_name_dfo
          
          # find the high dominance year
          sum_spawner <- sapply(spawnerAbundance_cl_l,function(sa){
            return(sum(sa,na.rm = T))
          })

          # Calculate current spawner abundance:
          csa_cl <- sapply(spawnerAbundance_cl_l,function(csa){
            return(tail(csa,1))
          })
          names(csa_cl) <- gsub(".*\\.","",names(csa_cl))
          
          yrInitial <- names(csa_cl) |> as.numeric()
          yrFinal <-  names(csa_cl) |> as.numeric()
          currentSpawnerData_available <- sapply(spawnerAbundance_cl_l,function(csa){
            return(any(!is.na(csa)))
          })
          spawnerAbundance_lastGen_m <- csa_cl
          spawnerAbundance_lastGen_dataPointNb <- rep(1,length(csa_cl))
          currentSpawnerData_availableRecentEnough <- currentSpawnerData_available # we don't use generation length so the question is the same as asking if there are non-NA values
          
          # Determine the biostatus for each cycle-line
          status_percent075 <- c()
          status_percent05 <- c()
          for(cl in 1:length(spawnerAbundance_cl_l)){
            # cl <- 1
            cond_cl <- benchSummary_region_species_df$cycle_line == cl
            cond_cuid <- benchSummary_region_species_df$cuid == cuids[i_cu]
            cond_025 <- benchSummary_region_species_df$benchmark == "benchmark_0.25"
            cond_05 <- benchSummary_region_species_df$benchmark == "benchmark_0.5"
            cond_075 <- benchSummary_region_species_df$benchmark == "benchmark_0.75"
            csa <- csa_cl[cl]
            bench_025 <-  benchSummary_region_species_df$m[cond_cl & cond_cuid & cond_025]
            bench_05 <-  benchSummary_region_species_df$m[cond_cl & cond_cuid & cond_05]
            bench_075 <-  benchSummary_region_species_df$m[cond_cl & cond_cuid & cond_075]
            
            if(csa <= bench_025){
              status_percent075[cl] <- "poor"
              status_percent05[cl] <- "poor"
            }else if(csa <= bench_05 & csa <= bench_075){
              status_percent075[cl] <- "fair"
              status_percent05[cl] <- "fair"
            }else if(csa > bench_05 & csa <= bench_075){
              status_percent075[cl] <- "fair"
              status_percent05[cl] <- "good"
            }else{
              status_percent075[cl] <- "good"
              status_percent05[cl] <- "good"
            }
          }
          
          biologicalStatus_df <- data.frame(region = rep(region[i_rg],length(spawnerAbundance_cl_l)),
                                            species = rep(speciesAcroHere,length(spawnerAbundance_cl_l)),
                                            cuid = rep(cuids[i_cu],length(spawnerAbundance_cl_l)),
                                            CU = rep(CUs[i_cu],length(spawnerAbundance_cl_l)))
          biologicalStatus_df$CU_pse <- CUname_pse
          biologicalStatus_df$CU_dfo <- CUname_dfo
          biologicalStatus_df$cycle_line <- 1:4
          biologicalStatus_df$cycle_line_dominant <- sum_spawner == max(sum_spawner)
          biologicalStatus_df$current_spawner_abundance <- csa_cl
          biologicalStatus_df$yr_withData_start <- yrInitial
          biologicalStatus_df$yr_withData_end <- yrFinal
          biologicalStatus_df$yr_end_imposed <- yearCurrentAbundance
          cond <- conservationunits_decoder$cuid == cuids[i_cu]
          biologicalStatus_df$genLength <- conservationunits_decoder$gen_length[cond]
          biologicalStatus_df$genLength_available <- T
          biologicalStatus_df$dataPointNb <- sapply(spawnerAbundance_cl_l,function(sa_cl){
            return(sum(!is.na(sa_cl)))
          })
          # biologicalStatus_df$genLength_dataPointNb <- spawnerAbundance_lastGen_dataPointNb
          # biologicalStatus_df$status_percent_05_red <- status_percent_prob_05["red"]
          # biologicalStatus_df$status_percent_05_amber <- status_percent_prob_05["amber"]
          # biologicalStatus_df$status_percent_05_green <- status_percent_prob_05["green"]
          # biologicalStatus_df$status_percent_075_red <- status_percent_prob_075["red"]
          # biologicalStatus_df$status_percent_075_amber <- status_percent_prob_075["amber"]
          # biologicalStatus_df$status_percent_075_green <- status_percent_prob_075["green"]
          biologicalStatus_df$status_percent05 <- status_percent05
          biologicalStatus_df$status_percent075 <- status_percent075
          biologicalStatus_df$comment <- comment
          
          if(is.null(biologicalStatus_region_species_df)){
            biologicalStatus_region_species_df <- biologicalStatus_df
          }else{
            biologicalStatus_region_species_df <- rbind(biologicalStatus_region_species_df,
                                                        biologicalStatus_df)
          }
          
          maxS <- sapply(modelCI_cl,function(m){
            # return(quantile(x = m$benchmarkBoot[,"benchmark_0.5"],probs = .9))
            return(max(x = m$benchmarkBoot[,"benchmark_0.5"],na.rm = T))
          }) |> max()
          
          maxS <- max(c(maxS,spawnerAbundance_lastGen_m))
          
          maxS_fig <- maxS
          
          minS <- 0
          
          # 
          cond_025 <- benchSummary_df_cl$benchmark == 'benchmark_0.25'
          cond_050 <- benchSummary_df_cl$benchmark == 'benchmark_0.5'
          
          log10_scale <- T
          
          if(log10_scale){
            maxS <- log10(maxS)
            maxS_fig <- log10(maxS_fig)
            minS <- sapply(modelCI_cl,function(m){
              # return(quantile(x = m$benchmarkBoot[,"benchmark_0.5"],probs = .9))
              return(min(x = m$benchmarkBoot[,"benchmark_0.5"],na.rm = T))
            }) |> min()
            minS <- min(c(minS,csa_cl))
            minS <- log10(minS)
          }
          
          # coefficient to use to scale the graph so that extreme values do make
          # the figure too large
          coeff <- 2
          if(maxS > coeff * max(benchSummary_df_cl$m[cond_050])){
            maxS <- coeff * max(benchSummary_df_cl$m[cond_050])
          }
          
          # Figure
          if(print_fig){
            CUhere <- gsub(pattern = "/",'-',CUs[i_cu])  # in case "/" is in the CU's name
            CUhere <- gsub(pattern = "\\(","",CUhere)
            CUhere <- gsub(pattern = "\\)","",CUhere)
            
            cut_off <- 35
            if(nchar(CUhere) > cut_off){   # the figure can't print if the number of characters is too large
              CUhere <- substr(x = CUhere,start =  1,stop = cut_off) 
            }
            
            pathFile <- paste0(wd_figures,"/",regionName,"_",species_abbr[i_cu],"_",CUhere,"_",cuids[i_cu],"_",
                               "bench_percent_hist.jpeg")
            
            jpeg(file = pathFile, width = 18, height = 18, units = "cm", res = 300)
          }
          
          layout(mat = matrix(1:4,ncol = 1), heights = c(1.25,1,1,1.35))
          for(cl in 1:4){
            # cl <- 1
            
            side1 <- side3 <- .5
            xaxt <- "n"
            xlab <- main <- ""
            if(cl == 1){
              side3 <- 3
              main <-  paste0(regionName," - ",species_abbr[i_cu]," - ",CUname_pse," - ",cuids[i_cu])
            }else if(cl == 4){
              side1 <- 4.5
              xaxt <- "s"
              xlab <- "Number of fish"
            }
            
            par(mar = c(side1,4.5,side3,1))
            
            coeff_div <- sqrt(maxS_fig) |> ceiling()
            # coeff_div <- log(maxS_fig) * 2 |> ceiling()
            
            cond_cl <- benchSummary_df_cl$cycle_line == cl
            bench_low_dist <- modelCI_cl[[cl]]$benchmarkBoot[,"benchmark_0.25"]
            bench_up_dist <- modelCI_cl[[cl]]$benchmarkBoot[,"benchmark_0.5"]
            bench_low <- benchSummary_df_cl$m[cond_cl & cond_025]
            bench_up <- benchSummary_df_cl$m[cond_cl & cond_050]
            curr_sa <- spawnerAbundance_lastGen_m[cl]
            xlab <- "Number of fish"
            
            if(!log10_scale){
              breaks <- seq(0, maxS_fig, maxS_fig/coeff_div)
            }
            
            if(log10_scale){
              bench_low_dist <- log10(bench_low_dist)
              bench_up_dist <- log10(bench_up_dist)
              bench_low <- log10(bench_low)
              bench_up <- log10(bench_up)
              curr_sa <- log10(curr_sa)
              breaks <- "Sturges"
              xlab <- "Number of fish (log10)"
            }
            
            hl <- hist(x = bench_low_dist, 
                       col = paste0(status_cols['red'], 50), border = NA,
                       breaks = breaks, 
                       #breaks = maxS/100, 
                       xlim = c(minS, maxS), 
                       main = main, xlab = xlab,
                       freq = FALSE, xaxt = xaxt)
            axis(side = 1, labels = NA)
            
            hu_05 <- hist(x = bench_up_dist, 
                          col = paste0(status_cols['green'], 50), border = NA, 
                          breaks = breaks, 
                          # breaks = maxS/50,  xaxt = xaxt,
                          add = TRUE, 
                          freq = FALSE)
            
            # plot the upper and lower benchmarks
            
            segments(x0 = bench_low, y0 = 0, x1 = bench_low,  y1 = max(c(hl$density,hu_05$density)),
                     lwd = 2, col = status_cols['red'])
            segments(x0 = bench_up, y0 = 0, x1 = bench_up, y1 = max(c(hl$density,hu_05$density)),
                     lwd = 2, col = status_cols['green'])
            
            # plot the current spawner abundance
            segments(x0 = curr_sa, x1 = curr_sa, y0 = 0, y1 = max(c(hl$density,hu_05$density)), lwd = 2)
            
            # 
            text.col <- "green"
            if(biologicalStatus_df$status_percent05[cl] == "fair"){
              text.col <- "amber"
            }else if(biologicalStatus_df$status_percent05[cl] == "poor"){
              text.col <- "red"
            }
            legend("right", bty = 'n',legend = biologicalStatus_df$status_percent05[cl], 
                   text.col = status_cols[text.col], cex = 1.5)
            
            if(sum_spawner[cl] == max(sum_spawner)){
              legend("topright", bty = 'n',legend = c(biologicalStatus_df$yr_withData_end[cl],"dominant"))
            }else{
              legend("topright", bty = 'n',legend = biologicalStatus_df$yr_withData_end[cl])
            }
          }
          
          if(print_fig){
            dev.off()
          }
        } # end of fear each CU
          
        print(paste0("*** ",regionName,"_",speciesAcroHere," done ***"))
        write.csv(x = benchSummary_region_species_df, 
                  file = paste0(wd_output,"/intermediate/",regionName,"_",speciesAcroHere,"_cyclic_benchmarks_summary_percentiles.csv"),
                  row.names = F) # keep region[i_rg] and not region_i because of "Central coast" is used to name the other files and not "Central Coast"
        
        write.csv(x = biologicalStatus_region_species_df, 
                  file = paste0(wd_output,"/intermediate/",regionName,"_",speciesAcroHere,"_cyclic_biological_status_percentiles.csv"),
                  row.names = F)
        
      } # if there is data for this species
    } # end of loop for the species
  } # if there is enough data
} # end of the loop for the regions




