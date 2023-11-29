
#'******************************************************************************
#' The goal of the script is to conduct differentc checks
#' 
#' Files imported (from dropbox):
#' - 
#' 
#' Files produced:
#' - 
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
wd_project_dropbox <- wds_l$wd_project_dropbox
wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# The datasets to input were outputted by other scripts 
wd_data_input <- wd_output

# Import functions for this specific project
source("Code/functions.R")

# Load packages

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

# multiple regions:
region <- c(
  regions_df$Haida_Gwaii,
  regions_df$Central_coast)

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

#' Import the recruitsperspawner.csv from population-indicators/data_input or 
#' download it from the PSF database
recruitsperspawner <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[3],
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)

# Fixes spawners for the Yukon ------

#' * Fixe recruitsperspawner$spawners for the Yukon *
#' - values for recruitsperspawner$spawners are in the 1000s and without decimals 
#' - go in the original datasets and find the original values.
#' - update mismatch_diffYr_l
#' - update the dataset5
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Yukon/Data & Assessments/yukon-status/Data")
filename <- "Connorsetal2022_brood_table.csv"
yukonOriginal <- read.csv(paste(path,filename,sep ="/"),header = T)
head(yukonOriginal)
nrow(yukonOriginal) # 280
colnames(yukonOriginal)[colnames(yukonOriginal) == "BroodYear"] <- "year"
yukonOriginal <- yukonOriginal[,colnames(yukonOriginal) != "X"]

cuid_yukon <- unique(yukonOriginal$cuid)
for(cu in cuid_yukon){
  # cu <- cuid_yukon[1]
  S_original <- yukonOriginal[yukonOriginal$cuid == cu,]$S_med
  
  R_original <- yukonOriginal[yukonOriginal$cuid == cu,]$R_med
  
  spawners <- recruitsperspawner[recruitsperspawner$region == "Yukon" &
                                   recruitsperspawner$species_name == "Chinook" & 
                                   recruitsperspawner$cuid == cu,]$spawners
  
  recruits <- recruitsperspawner[recruitsperspawner$region == "Yukon" &
                                   recruitsperspawner$species_name == "Chinook" & 
                                   recruitsperspawner$cuid == cu,]$recruits
  
  
  spawners_noNA <- spawners[!is.na(spawners)]
  
  if(!identical(round(S_original),spawners_noNA)){
    print(paste("Different numbers for CU",cu))
    print(round(S_original))
    print(spawners_noNA)
  }
  
  recruits_noNA <- recruits[!is.na(recruits)]
  R_original_noNA <- R_original[!is.na(R_original)] 
  
  if(!identical(round(R_original_noNA),recruits_noNA)){
    print(paste("Different numbers for CU",cu))
    print(round(R_original))
    print(recruits_noNA)
  }
  
  recruitsperspawner[recruitsperspawner$region == "Yukon" &
                       recruitsperspawner$species_name == "Chinook" & 
                       recruitsperspawner$cuid == cu,]$spawners[!is.na(spawners)] <- round(S_original * 1000)
  
  recruitsperspawner[recruitsperspawner$region == "Yukon" &
                       recruitsperspawner$species_name == "Chinook" & 
                       recruitsperspawner$cuid == cu,]$recruits[!is.na(recruits)] <- round(R_original_noNA * 1000)
}

#' * Replace the value in dataset_5.Dec162022csv *
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Yukon/Data & Assessments/yukon-status/Output")
filename <- "dataset_5.Dec162022.csv"

dataset_5 <- read.csv(paste(path,filename,sep ="/"),header = T)
dataset_5 <- dataset_5[,colnames(dataset_5) != "X"]
head(dataset_5)

cuid_yukon <- unique(yukonOriginal$cuid)
for(cu in cuid_yukon){
  # cu <- cuid_yukon[1]
  spaw_recr <- recruitsperspawner[recruitsperspawner$region == "Yukon" &
                                   recruitsperspawner$species_name == "Chinook" & 
                                   recruitsperspawner$cuid == cu,][,c("spawners","recruits")]
  
  spaw_recr_5 <- dataset_5[dataset_5$Species == "Chinook" & 
                             dataset_5$CUID == cu,][,c("Spawners","Recruits")]
  
  spaw_recr_5 <- apply(spaw_recr_5,2,as.numeric) # was integer, which prevent from comparing values
  
  s <- round(spaw_recr$spawners/1000)
  r <- round(spaw_recr$recruits/1000)
  cond_s <- identical(s,spaw_recr_5[,"Spawners"])
  cond_r <- identical(r,spaw_recr_5[,"Recruits"])
  
  if(!cond_s | !cond_r){
    print(paste("Different numbers for CU",cu))
  }
  
  dataset_5[dataset_5$Species == "Chinook" &
              dataset_5$CUID == cu,]$Spawners <- spaw_recr$spawners
  
  dataset_5[dataset_5$Species == "Chinook" &
              dataset_5$CUID == cu,]$Recruits <- spaw_recr$recruits
}

write.csv(dataset_5,paste0(path,"/dataset_5.Nov282023.csv"),row.names = F)

# Fixes spawners for the Central Coast WAIT TO HEAR FROM THEM ------

#' * Replace the value in All-OUTPUT--nonlegacy-mode_20220222.xlsx *
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Central Coast PSE/analysis/cc-recons-2021")
filename <- "All-OUTPUT--nonlegacy-mode_20220222.xlsx"

# Wait for Eric to share rencent (2020) run reconstruction

# To update this output dataset5 for the database: https://www.dropbox.com/scl/fi/hlv4n3go8ewavb9vm7c9r/dataset_5.Dec162022.csv?rlkey=5lxtr2s1ybk7jdx8eeumbcznv&dl=0 
# Central coast age data: https://www.dropbox.com/scl/fi/y360sr7hqie2ale3uolll/All-OUTPUT-nonlegacy-mode_20220222.xlsx?rlkey=szlb0pzfscdrvj6b9y3skmw33&dl=0 


#
# Check that the data spawner abundance data matches between cuspawnerabundance and recruitsperspawner -------

#'* Are they CUs only present in one of the two datasets? *

colnamesCu <- c("region","species_name","cuid","cu_name_pse")

SpawnAbund <- unique(cuspawnerabundance[,colnamesCu])
nrow(SpawnAbund) # 464

SpawnRecru <- unique(recruitsperspawner[,colnamesCu])
nrow(SpawnRecru) # 184

#'* Are they CUs in recruitsperspawner not in cuspawnerabundance *

SpawnAbund$cuspawnerabundance <- T
SpawnRecru$recruitsperspawner <- T

mergedDF <- merge(x = SpawnRecru,y = SpawnAbund, by = colnamesCu, all.x = T)
nrow(mergedDF) # 184

mergedDF[is.na(mergedDF$recruitsperspawner)] # empty

#' CONCLUSION: all the CUs in recruitsperspawner are in cuspawnerabundance.

#'* Do the spawner abundances match? *

head(mergedDF)
mismatch_l <- list()
count <- 1
identical_l <- list()
count_identical <- 1
for(r in 1:nrow(mergedDF)){
  # r <- 1
  region <- mergedDF$region[r]
  species <- mergedDF$species[r]
  cu_name_pse <- mergedDF$cu_name_pse[r]
  cuspawnerabundanceHere <- cuspawnerabundance[cuspawnerabundance$region == region &
                                                 cuspawnerabundance$species_name == species &
                                                 cuspawnerabundance$cu_name_pse == cu_name_pse,]
  recruitsperspawnerHere <- recruitsperspawner[recruitsperspawner$region == region &
                                                 recruitsperspawner$species_name == species &
                                                 recruitsperspawner$cu_name_pse == cu_name_pse,]
  
  cuspawnerabundanceHere[,c(colnamesCu,"year","estimated_count","observed_count")]
  recruitsperspawnerHere[,c(colnamesCu,"year","spawners")]
  mergeDFHere <- merge(x = recruitsperspawnerHere[,c(colnamesCu,"year","spawners")], 
                       y = cuspawnerabundanceHere[,c(colnamesCu,"year","estimated_count","observed_count")],
                       by = c(colnamesCu,"year"), all = T)
  namesHere <- paste(region,species,cu_name_pse,sep=" - ")
  if(!identical(mergeDFHere$spawners,mergeDFHere$estimated_count)){
    mismatch_l[[count]] <- mergeDFHere
    names(mismatch_l)[count] <- namesHere
    count <- count + 1
    print(paste0("Mismatch with: ",namesHere))
  }else{
    identical_l[[count_identical]] <- mergeDFHere
    names(identical_l)[count_identical] <- namesHere
    count_identical <- count_identical + 1
  }
}
length(mismatch_l) # 135
mismatch_l[[5]]
length(identical_l) # 49
identical_l[[1]]

mismatch_remain_l <- mismatch_l

#' list of CUS with only NAs in recruitsperspawner$spawners and list of 
#' CUs with only NAs in cuspawnerabundance$estimated_count
mismatch_allNAs_recruitsperspawner_l <- list()
count_RS <- 1
mismatch_allNAs_cuspawnerabundance_l <- list()
count_cuS <- 1
for(i in 1:length(mismatch_remain_l)){
  # i <- 1
  mergeDFHere <- mismatch_remain_l[[i]]
  if(sum(!is.na(mergeDFHere$spawners)) == 0){
    mismatch_allNAs_recruitsperspawner_l[[count_RS]] <- mergeDFHere
    names(mismatch_allNAs_recruitsperspawner_l)[count_RS] <- names(mismatch_l)[i]
    count_RS <- count_RS + 1
  }else if(sum(!is.na(mergeDFHere$estimated_count)) == 0){
    mismatch_allNAs_cuspawnerabundance_l[[count_cuS]] <- mergeDFHere
    names(mismatch_allNAs_cuspawnerabundance_l)[count_cuS] <- names(mismatch_l)[i]
    count_cuS <- count_cuS + 1
  }
}
length(mismatch_allNAs_recruitsperspawner_l) # 8
length(mismatch_allNAs_cuspawnerabundance_l) # 1

mismatch_remain_l <- mismatch_remain_l[!names(mismatch_remain_l) %in% names(mismatch_allNAs_recruitsperspawner_l)]
mismatch_remain_l <- mismatch_remain_l[!names(mismatch_remain_l) %in% names(mismatch_allNAs_cuspawnerabundance_l)]
length(mismatch_remain_l) # 126

#' CUs with missing values in recruitsperspawner$spawners only in the recent years,
#' otherwise all rest of the data is identical
mismatch_NAsRecentYr_l <- list()
count <- 1
for(i in 1:length(mismatch_remain_l)){
  # i <- 1
  mergeDFHere <- mismatch_remain_l[[i]]
  lastYear <- max(mergeDFHere$year[!is.na(mergeDFHere$spawners)])
  mergeDFHereCut <- mergeDFHere[mergeDFHere$year <= lastYear,]
  if(identical(mergeDFHereCut$spawners,mergeDFHereCut$estimated_count)){
    mismatch_NAsRecentYr_l[[count]] <- mergeDFHere
    names(mismatch_NAsRecentYr_l)[count] <- names(mismatch_remain_l)[i]
    count <- count + 1
  }
}
length(mismatch_NAsRecentYr_l) # 93
mismatch_remain_l <- mismatch_remain_l[!names(mismatch_remain_l) %in% names(mismatch_NAsRecentYr_l)]
length(mismatch_remain_l) # 33

#' CUs with missing values in past years but values present are identical and 
#' the ones with contrasting values:
mismatch_NAsPastYr_l <- list()
count <- 1
mismatch_diffYr_l <- list()
count_diff <- 1
for(i in 1:length(mismatch_remain_l)){
  # i <- 1
  mergeDFHere <- mismatch_remain_l[[i]]
  lastYear <- max(mergeDFHere$year[!is.na(mergeDFHere$spawners)])
  mergeDFHereCut <- mergeDFHere[mergeDFHere$year <= lastYear,]
  mergeDFHereCut <- mergeDFHereCut[!is.na(mergeDFHereCut$spawners),]
  if(identical(mergeDFHereCut$spawners,mergeDFHereCut$estimated_count)){
    mismatch_NAsPastYr_l[[count]] <- mergeDFHere
    names(mismatch_NAsPastYr_l)[count] <- names(mismatch_remain_l)[i]
    count <- count + 1
  }else{
    mismatch_diffYr_l[[count_diff]] <- mergeDFHereCut[mergeDFHereCut$spawners != mergeDFHereCut$estimated_count,]
    names(mismatch_diffYr_l)[count_diff] <- names(mismatch_remain_l)[i]
    count_diff <- count_diff + 1
  }
}
length(mismatch_NAsPastYr_l) # 1
mismatch_NAsPastYr_l
length(mismatch_diffYr_l)    # 32
mismatch_diffYr_l[[1]]
mismatch_diffYr_l[[2]]
mismatch_diffYr_l[[3]]

mismatch_remain_l <- mismatch_remain_l[!names(mismatch_remain_l) %in% names(mismatch_NAsPastYr_l)]
mismatch_remain_l <- mismatch_remain_l[!names(mismatch_remain_l) %in% names(mismatch_diffYr_l)]
length(mismatch_remain_l) # 0

#' Final lists:
identical_l       # the list of CUs that with identical spawner abundances (49)
mismatch_l        # the list of CUs with mistmatches (135) 
mismatch_allNAs_recruitsperspawner_l # only NAs in recruitsperspawner$spawners (8)
mismatch_allNAs_cuspawnerabundance_l # only NAs in cuspawnerabundance$estimated_count (1)
mismatch_NAsRecentYr_l # CUs with missing values in recruitsperspawner$spawners only in the recent years, rest is identical (93)
mismatch_NAsPastYr_l   # CUs with missing values in past years but values present are identical (1)
mismatch_NAsPastYr_l   # CU with missing data in the past for recruitsperspawner$spawners (1)
mismatch_diffYr_l      # CUs with differing counts (32)

toExclude <- c("Central Coast","Haida Gwaii")
regionsToKeep <- sapply(X = names(mismatch_diffYr_l),FUN = function(n){
  return(sub("\\ -.*", "", n))
})
regionsToKeep <- unique(regionsToKeep)
regionsToKeep <- regionsToKeep[! regionsToKeep %in% toExclude]
printFig <- F
for(rg in regionsToKeep){
  # rg <- regionsToKeep[3]
  cu_l <- mismatch_diffYr_l[grepl(rg,names(mismatch_diffYr_l))]

  speciesHere <- sapply(X = names(cu_l),FUN = function(n){
    # n <- names(cu_l)[1]
    out <- strsplit(x = n,split = " - ")[[1]]
    out <- out[2]
    return(out)
  })
  speciesHere[grepl("Pink",speciesHere)] <- "Pink" # pool the odd and even Pink species together
  speciesHere <- unique(speciesHere)
  
  for(sp in speciesHere){
    # sp <- speciesHere[1]
    cu_lcut <- cu_l[grepl(sp,names(cu_l))]
    CU_n <- length(cu_lcut)
    points_n_v <- sapply(X = cu_lcut, FUN = function(d){nrow(d)})
    
    yrange <- sapply(X = cu_lcut, FUN = function(d){
      return(range(d$spawners))
    })
    xrange <- sapply(X = cu_lcut, FUN = function(d){
      return(range(d$estimated_count))
    })
    ymin <- min(yrange[1,])
    ymax <- max(yrange[2,])
    xmin <- min(xrange[1,])
    xmax <- max(xrange[2,])
    
    CUsHere <- sub(".*\\- ", "",names(cu_lcut))
    
    log10Trans <- F
    conditionsforLogTrans <- (rg == "Skeena" & sp == "Lake sockeye") |
                             (rg == "Vancouver Island & Mainland Inlets" & sp == "Pink")
    if(conditionsforLogTrans){
      log10Trans <- T
    }
    
    xlab <- 'Estimated counts (cuspawnerabundance)'
    ylab <- "Spawners (recruitsperspawner)"
    
    if(log10Trans){
      ymin <- log10(ymin)
      ymax <- log10(ymax)
      xmin <- log10(xmin)
      xmax <- log10(xmax)
      xlab <- paste0(xlab,' (log10)')
      ylab <- paste0(ylab,' (log10)')
    }
    
    if(printFig){
      jpeg(filename = paste0(wd_figures,"/checks_spawners_vs_estimated_counts - ",rg," - ",sp,".jpg"),
           width = 25, height = 25, units = "cm", res = 300)
    }

    plot(NULL, xlim = c(xmin,xmax), ylim = c(ymin,ymax),xlab = xlab,ylab = ylab,
         main = paste(rg,sp,sep = " - "))
    pointsCol <- rainbow(n = length(points_n_v))
    for(cu_i in 1:CU_n){
      # cu_i <- 1
      dsHere <- cu_l[[cu_i]]
      x <- cu_l[[cu_i]]$estimated_count
      y <- cu_l[[cu_i]]$spawners
      if(log10Trans){
        x <- log10(x)
        y <- log10(y)
      }
      points(x = x, y = y, col = pointsCol[cu_i], pch = 16)
      reg <- lm(y ~ x)
      lines(x = x, y = predict(object = reg), col = pointsCol[cu_i], lwd = 2)
    }
    abline(a = 0, b = 1, lwd = 2, lty = 2)
    legend("topleft",CUsHere,fill = pointsCol,bty = "n")
  }
  if(printFig){
    dev.off()
  }
}

#
# check why there are identical biological status probabilities between Smsy and Smsy80 --------

biological_status_df <- read.csv(paste0(wd_output,"/Biological_status_HBSRM_all.csv"),
                                 header = T)

biological_status_df_noNA <- biological_status_df[!is.na(biological_status_df$status_Smsy80_red),]

colHere <- colnames(biological_status_df_noNA)[grepl("status_Smsy",colnames(biological_status_df_noNA))]
colHere_Smsy80 <- colHere[grepl("80",colHere)]
colHere_Smsy <- colHere[!colHere %in% colHere_Smsy80]

for(col in colHere){
  biological_status_df_noNA[,col] <- round(biological_status_df_noNA[,col],4)
}

identicalBool <- sapply(X = 1:nrow(biological_status_df_noNA), FUN = function(r){
  # r <- 1
  DF <- biological_status_df_noNA[r,,drop = F]
  ident <- c()
  for(i in 1:3){
    ident <- c(ident,DF[,colHere_Smsy][,i] == DF[,colHere_Smsy80][,i])
  }
  if(sum(ident) == 3){
    return(TRUE)
  }else{
    return(FALSE)
  }
})

dupli_df <- biological_status_df_noNA[identicalBool,!colnames(biological_status_df_noNA) %in% c("CU_pse","CU_dfo","comment")]
nrow(dupli_df) # 46

dupli_remain_df <- dupli_df
# Central_Coast      CK Rivers Inlet

#' There are multiple cases where the match makes sense:

#' CASE 1: current spawner abundance is consistently higher than Smsy:
dupli_df_largerSmsy <- dupli_remain_df[dupli_remain_df$status_Smsy_green == 100,]
nrow(dupli_df_largerSmsy) # 10

dupli_remain_df <- dupli_remain_df[dupli_remain_df$status_Smsy_green < 100,]
nrow(dupli_remain_df) # 36

#' CASE 2: current spawner abundance is consistently < Sgen:
dupli_df_smallerSgen <- dupli_remain_df[dupli_remain_df$status_Smsy_red == 100,]
nrow(dupli_df_smallerSgen) # 9

dupli_remain_df <- dupli_remain_df[dupli_remain_df$status_Smsy_red < 100,]
nrow(dupli_remain_df) # 27

#' CASE 3: current spawner abundance is always < Smsy80
dupli_df_smallerSmsy80 <- dupli_remain_df[dupli_remain_df$status_Smsy80_green == 0,]
nrow(dupli_df_smallerSmsy80) # 27

dupli_remain_df <- dupli_remain_df[dupli_remain_df$status_Smsy80_green > 0,]
nrow(dupli_remain_df) # 0

# CONCLUSION: All good 

# -------


