

rm(list = ls())
graphics.off()

library(dplyr)
# library(readxl)
# library(tidyr)

source("code/functions_general.R")

wd_X_Drive1_PROJECTS <- paste0(get_XDrive(),"1_PROJECTS")
wd_popind <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Population Methods and Analysis/population-indicators")

wd_output <- paste0(wd_popind,"/spawner-surveys/output/archive")
wd_data <- paste0(wd_popind,"/spawner-surveys/data")


# 
# Check if large changes in two EVIMI CO CUs (2026-09-23) ------
#' Details: pop meeting of 2026-09-23
#' https://docs.google.com/document/d/1jC1FHwadE7C484etj1RM6rNR5iOreu8BbQHGNja7MjE/edit?tab=t.0

nameFile_new <- "dataset2_spawner-surveys_2026-03-16"
dataset2_new <- read.csv(paste0(wd_output,"/",nameFile_new,".csv"),
                         header = T) |> filter(region %in% c("East Vancouver Island & Mainland Inlets",
                                                             "West Vancouver Island"),
                                               species_name == "Coho")

nameFile_old <- "dataset2_spawner-surveys_2025-04-15"
nameFile_old <- "dataset2_spawner-surveys_2025-04-14"
nameFile_old <- "dataset2_spawner_surveys_2025-04-10"
nameFile_old <- "dataset2_spawner-surveys_2025-09-15"
dataset2_old <- read.csv(paste0(wd_output,"/",nameFile_old,".csv"),
                         header = T) |> filter(region %in% c("Vancouver Island & Mainland Inlets",
                                                             "East Vancouver Island & Mainland Inlets",
                                                             "West Vancouver Island"),
                                               species_name == "Coho")

streams <- c("BLACK CREEK","FRANKLIN RIVER")

for(strm in streams){
  # strm <- streams[1]
  
  cond_strm_new <- dataset2_new$stream_name_pse == strm
  cond_strm_old <- dataset2_old$stream_name_pse == strm
  
  CUs <- unique(c(dataset2_new$cu_name_pse[cond_strm_new],dataset2_old$cu_name_pse[cond_strm_old]))
  
  if(length(CUs) != 2){
    print("More than two CUs - BREAK")
    break
  }
  
  layout(matrix(1:2,nrow = 1))
  
  for(cu in CUs){
    # cu <- CUs[1]
    cond_cu_new <- dataset2_new$cu_name_pse == cu
    cond_cu_old <- dataset2_old$cu_name_pse == cu
    
    data_new <- dataset2_new[cond_strm_new & cond_cu_new,]
    data_old <- dataset2_old[cond_strm_old & cond_cu_old,]
    data_new <- data_new[order(data_new$year),]
    data_old <- data_old[order(data_old$year),]
    
    y_max <- max(data_new$stream_observed_count, data_old$stream_observed_count, na.rm = T)
    
    main <- paste(cu,"CU -",strm)
    
    plot(x = data_new$year, y = data_new$stream_observed_count, type = "o", 
         lwd = 2, pch = 16, ylim = c(0, y_max * 1.1), las = 1, main = main, 
         xlab = "Year", ylab = "Count")
    points(x = data_old$year, y = data_old$stream_observed_count, type = "o", 
           lwd = 2, pch = 1, col = "red", lty = 2,  cex = 2)
    
    legend("topleft",c(nameFile_new,nameFile_old),
           lwd = 2, col = c("black","red"), pch = c(16,1), lty = c(1,2), pt.cex = c(1,2), bty = "n")
  }
}

# try the original datasets:
nameFile_new <- "All Areas NuSEDS_20251103"
nuseds_new <- read.csv(paste0(wd_data,"/",nameFile_new,".csv"),
                         header = T) |> filter(WATERBODY %in% streams, SPECIES == "Coho")

nameFile_old <- "all_areas_nuseds_2025-02-21"
nameFile_old <- "all_areas_nuseds_2025-01-03"
nameFile_old <- "All Areas NuSEDS_20240627"
nameFile_old <- "all_areas_nuseds_20240122"
nuseds_old <- read.csv(paste0(wd_data,"/",nameFile_old,".csv"),
                         header = T) |> filter(WATERBODY %in% streams, SPECIES == "Coho")

cuss_new <- read.csv(paste0(wd_data,"/Conservation Unit Census Sites_20250528.csv"),
                     header = T)


var_in_MAX_ESTIMATE <- c("NATURAL_ADULT_SPAWNERS", 
                         "NATURAL_JACK_SPAWNERS", 
                         "NATURAL_SPAWNERS_TOTAL", 
                         "ADULT_BROODSTOCK_REMOVALS", 
                         "JACK_BROODSTOCK_REMOVALS",
                         "TOTAL_BROODSTOCK_REMOVALS",    
                         "OTHER_REMOVALS", 
                         "TOTAL_RETURN_TO_RIVER")

nuseds_new$MAX_ESTIMATE <- apply(nuseds_new[,var_in_MAX_ESTIMATE], 1,
                                       max, na.rm = TRUE)
nuseds_new$MAX_ESTIMATE[is.infinite(nuseds_new$MAX_ESTIMATE)] <- NA

nuseds_old$MAX_ESTIMATE <- apply(nuseds_old[,var_in_MAX_ESTIMATE], 1,
                                 max, na.rm = TRUE)
nuseds_old$MAX_ESTIMATE[is.infinite(nuseds_old$MAX_ESTIMATE)] <- NA

for(strm in streams){
  # strm <- streams[1]
  
  cond_strm_new <- nuseds_new$WATERBODY == strm
  cond_strm_old <- nuseds_old$WATERBODY == strm
  
  POP_ID <- unique(c(nuseds_new$POP_ID[cond_strm_new],nuseds_old$POP_ID[cond_strm_old]))
  POP_ID <- rev(POP_ID)
  
  if(length(POP_ID) != 2){
    print("More than two POP_ID - BREAK")
    break
  }
  
  layout(matrix(1:2,nrow = 1))
  
  for(popid in POP_ID){
    
    # popid <- POP_ID[1]
    
    cu <- unique(cuss_new$CU_NAME[cuss_new$POP_ID == popid])
    
    cond_cu_new <- nuseds_new$POP_ID == popid
    cond_cu_old <- nuseds_old$POP_ID == popid
    
    data_new <- nuseds_new[cond_strm_new & cond_cu_new,]
    data_old <- nuseds_old[cond_strm_old & cond_cu_old,]
    data_new <- data_new[order(data_new$ANALYSIS_YR),]
    data_old <- data_old[order(data_old$ANALYSIS_YR),]
    
    y_max <- max(data_new$MAX_ESTIMATE, data_old$MAX_ESTIMATE, na.rm = T)
    
    main <- paste(cu,"CU -",strm)
    
    plot(x = data_new$ANALYSIS_YR, y = data_new$MAX_ESTIMATE, type = "o", 
         lwd = 2, pch = 16, ylim = c(0, y_max * 1.1), las = 1, main = main, 
         xlab = "Year", ylab = "Count")
    points(x = data_old$ANALYSIS_YR, y = data_old$MAX_ESTIMATE, type = "o", 
           lwd = 2, pch = 1, col = "red", lty = 2,  cex = 2)
    
    legend("topleft",c(nameFile_new,nameFile_old),
           lwd = 2, col = c("black","red"), pch = c(16,1), lty = c(1,2), pt.cex = c(1,2), bty = "n")
  }
}



