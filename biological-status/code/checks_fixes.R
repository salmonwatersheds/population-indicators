
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

#' Import the recruitsperspawner.csv from population-indicators/data_input or 
#' download it from the PSF database
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)
#
# Fixes spawners for the Yukon Dataset_5 ------
#
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
dataset_5_older <- dataset_5
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

# update R_S:
dataset_5$R_S <- dataset_5$Recruits / dataset_5$Spawners

# write.csv(dataset_5,paste0(path,"/dataset_5.Nov282023.csv"),row.names = F)

# Checks:

plot(y = dataset_5$R_S, x = dataset_5$Recruits / dataset_5$Spawners, 
     ylab = "R_S", xlab = "rectruits/spawners")
abline(a = 0, b = 1)

plot(y = dataset_5$R_S, x = round(dataset_5$Recruits/1000) / round(dataset_5$Spawners/1000), 
     ylab = "R_S", xlab = "rectruits/spawners")
abline(a = 0, b = 1)

plot(y = dataset_5_older$R_S, x = dataset_5_older$Recruits / dataset_5_older$Spawners, 
     ylab = "R_S", xlab = "rectruits/spawners")
abline(a = 0, b = 1)

#
# Fixes spawners for the Central Coast Dataset_5 ------

#' Use recent (2020) run reconstruction: All-OUTPUT--nonlegacy-mode_20220222.xlsx
#' https://www.dropbox.com/scl/fi/y360sr7hqie2ale3uolll/All-OUTPUT-nonlegacy-mode_20220222.xlsx?rlkey=szlb0pzfscdrvj6b9y3skmw33&dl=0

#' And format the the file like dataset_5.Nov282023.csv for the Yukon.
 
#' Notes:
#' - Field "Escape" is “escapement” which is equal to "spawners" in dataset 5.
#' - The TR4 etc. are the “total recruits” at age 4 - Check with Eric that this is not returns!

# Import the dataset_5_Yukon for comparison of the fields:
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Yukon/Data & Assessments/yukon-status/Output")
filename <- "dataset_5.Nov282023.csv"
dataset_5_Yukon <- read.csv(paste(path,filename,sep = "/"))
head(dataset_5_Yukon)
max(dataset_5_Yukon$Year) # 2019

# Import the run reconstruction file with the updated data:
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Central Coast PSE/analysis/cc-recons-2021")
filename <- "All-OUTPUT--nonlegacy-mode_20220222.xlsx"

library(readxl)
sheet_1 <- read_excel(path = paste(path,filename,sep = "/"), sheet = 1, 
                      guess_max = 10000) # increased to avoid having TR2 returned as "logical" :https://stackoverflow.com/questions/50947838/numerical-column-in-excel-gets-converted-as-logical
sheet_1 <- as.data.frame(sheet_1)
head(sheet_1)

# check that "Total" is the sum of the total recruits at a given age (e.g. TR4)
sum <- rowSums(sheet_1[,grepl("TR",colnames(sheet_1))],na.rm = T)
tot <- sheet_1$Total
identical(round(sum,4),round(tot,4)) # all good

fieldToKeep1 <- c("SpeciesId","CU","CU_Name","BroodYear","Escape","Total")

dataset_5_all <- sheet_1[,fieldToKeep1] 

# Update fields:

# CHANGE: add column "Species"
unique(dataset_5_Yukon$Species)
unique(conservationunits_decoder$species_name)
unique(dataset_5_all$SpeciesId)
dataset_5_all$SpeciesId <- toupper(dataset_5_all$SpeciesId)
dataset_5_all$Species <- NA
SpeciesId <- unique(dataset_5_all$SpeciesId)
names(SpeciesId) <- c("Chum","Chinook","Coho","Pink (even)","Pink (odd)","Sockeye")
SpeciesId

for(spid in SpeciesId){
  dataset_5_all$Species[dataset_5_all$SpeciesId == spid] <- names(SpeciesId)[SpeciesId == spid]
}

# CHANGE: rename "BroodYear" to "Year" 
colnames(dataset_5_all)[colnames(dataset_5_all) == "BroodYear"] <- "Year"

# CHANGE: rename "Escape" "Spawners" 
colnames(dataset_5_all)[colnames(dataset_5_all) == "Escape"] <- "Spawners"

# CHANGE: rename "Total" to "Recruits"
colnames(dataset_5_all)[colnames(dataset_5_all) == "Total"] <- "Recruits"

# CHANGE: add CUID" --> not in dataset_5_all
# a. Use conservationunits_decoder: 
# Replace "Lake sockeye" and River sockeye" by "Sockeye
unique(conservationunits_decoder$species_name)
unique(dataset_5_all$Species)
conservationunits_decoder$species_name[grepl("sockeye",conservationunits_decoder$species_name)] <- "Sockeye"

unique(conservationunits_decoder$cu_name_pse)
unique(dataset_5_all$CU_Name) # --> potential issues with capital letters, --> to lower case and replace "-" and "_" by " "
conservationunits_decoder$cu_name_pse_modif <- tolower(conservationunits_decoder$cu_name_pse)
conservationunits_decoder$cu_name_pse_modif <- gsub("[_|-]"," ",conservationunits_decoder$cu_name_pse_modif)
conservationunits_decoder$cu_name_dfo_modif <- tolower(conservationunits_decoder$cu_name_dfo)
conservationunits_decoder$cu_name_dfo_modif <- gsub("[_|-]"," ",conservationunits_decoder$cu_name_dfo_modif)
dataset_5_all$CU_Name_modif <- tolower(dataset_5_all$CU_Name)
dataset_5_all$CU_Name_modif <- gsub("[_|-]"," ",dataset_5_all$CU_Name_modif)

unique(conservationunits_decoder$cu_index)
unique(dataset_5_all$CU) # --> potential issue with "_" and "-" --> remove them + "CK" --> "CN", "SEL" --> "SX_L", "SER" --> "SX_R", "PKE" --> "PKe", "PKO" --> "PKo"
conservationunits_decoder$cu_index_modif <- conservationunits_decoder$cu_index
conservationunits_decoder$cu_index_modif <- gsub("CK","CN",conservationunits_decoder$cu_index_modif)
conservationunits_decoder$cu_index_modif <- gsub("SEL","SX_L",conservationunits_decoder$cu_index_modif)
conservationunits_decoder$cu_index_modif <- gsub("SER","SX_R",conservationunits_decoder$cu_index_modif)
conservationunits_decoder$cu_index_modif <- gsub("PKE","PKe",conservationunits_decoder$cu_index_modif)
conservationunits_decoder$cu_index_modif <- gsub("PKO","PKo",conservationunits_decoder$cu_index_modif)
conservationunits_decoder$cu_index_modif <- gsub("[_|-]"," ",conservationunits_decoder$cu_index_modif)
dataset_5_all$CU_modif <- gsub("[_|-]"," ",dataset_5_all$CU)

# unique combanation of species & CUs
Species_CU_Name <- unique(dataset_5_all[,c("Species","CU_Name","CU","CU_Name_modif","CU_modif")])

# find the 
Species_CU_Name$cuid <- Species_CU_Name$cu_name_pse <- Species_CU_Name$region <- NA
Species_CU_Name$cu_index <- NA

colToKeep <- c("region","cuid","cu_name_pse","cu_index")
for(r in 1:nrow(Species_CU_Name)){
  # r <- 117
  sp_cu <- Species_CU_Name[r,]
  sp <- sp_cu$Species
  cu_name_modif <- sp_cu$CU_Name_modif
  cu_modif <- sp_cu$CU_modif
  
  out1 <- conservationunits_decoder[conservationunits_decoder$species_name == sp &
                                     conservationunits_decoder$cu_index_modif == cu_modif,][,colToKeep]
  out2 <- conservationunits_decoder[conservationunits_decoder$species_name == sp &
                                     conservationunits_decoder$cu_name_dfo_modif == cu_name_modif,][,colToKeep]
  out3 <- conservationunits_decoder[conservationunits_decoder$species_name == sp &
                                     conservationunits_decoder$cu_name_pse_modif == cu_name_modif,][,colToKeep]
  out <- rbind(out1,out2,out3)
  out <- out[!is.na(out$cuid),]
  out <- unique(out)
  
  if(nrow(out) == 0){
    out <- data.frame(region = NA, cuid = NA, cu_name_pse = NA, cu_index = NA, 
                      CU_Name_xlsx = NA)
  }else if(nrow(out) > 1){
    # manual fixes:
    if(cu_modif %in% c("CO 28","SX L 21 10")){ 
      # for CO 28: cu_name_pse is "Brim-Wahoo" not "Northern Coastal Streams" --> trust cu_modif (i.e., out1) and not cu_name_modif (i.e., out2)
      # for SX L 21 10: similar case
      out <- out1[!is.na(out1$region),]
    }
    print(r)
    print(out)
  }
  Species_CU_Name$region[r] <- out$region
  Species_CU_Name$cuid[r] <- out$cuid
  Species_CU_Name$cu_name_pse[r] <- out$cu_name_pse
  Species_CU_Name$cu_index[r] <- out$cu_index
}

unique(Species_CU_Name$region) # "Central Coast" "Haida Gwaii"   "Skeena"        "Nass"          NA
sum(!is.na(Species_CU_Name$cuid))/nrow(Species_CU_Name) # 0.96 !!!

Species_CU_Name[is.na(Species_CU_Name$cuid),]

# b. Find the cuid using decoder_centralCoast: CUs names don't necessarily match

# match dataset_5_all$CU with decoder_centralCoast$trtc_cu
# --> import the CentralCoast_CUs_final_withCUID_decoder
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1701799688987309?thread_ts=1701197234.664189&cid=CJ5RVHVCG
# - Eric: "the field trtc_cu in this decoder should help you to go from CU to cuid for the central coast".
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Central Coast PSE/analysis/central-coast-status/Data")
filename <- "CentralCoast_CUs_final_withCUID_decoder.csv"
decoder_centralCoast <- read.csv(paste(path,filename,sep = "/"))

# make some corrections and modifications
decoder_centralCoast$species[decoder_centralCoast$species == "Pink (Even)"] <- "Pink (even)"
decoder_centralCoast$species[decoder_centralCoast$species == "Pink (Odd)"] <- "Pink (odd)"
decoder_centralCoast$species[grepl("Sockeye",decoder_centralCoast$species)] <- "Sockeye"
decoder_centralCoast$trtc_cu_modif <- gsub("[_|-]"," ",decoder_centralCoast$trtc_cu)
decoder_centralCoast$culabel_modif <- tolower(decoder_centralCoast$culabel)
decoder_centralCoast$culabel_modif <- gsub("[_|-]"," ",decoder_centralCoast$culabel_modif)

Species_CU_Name$cuid_cc <- NA
for(r in 1:nrow(Species_CU_Name)){
  # r <- 3
  sp_cu <- Species_CU_Name[r,]
  sp <- sp_cu$Species
  cu_name_modif <- sp_cu$CU_Name_modif
  cu_modif <- sp_cu$CU_modif
  
  out1 <- decoder_centralCoast$cuid[decoder_centralCoast$trtc_cu_modif == cu_modif &
                                          decoder_centralCoast$species == sp_cu$Species]
  
  out2 <- decoder_centralCoast$cuid[decoder_centralCoast$culabel_modif == cu_name_modif &
                                     decoder_centralCoast$species == sp]
  
  out <- unique(c(out1,out2))

  if(length(out) == 0){
    out <- NA
  }else if(length(out) > 1){
    print(r)
    print(out)
  }
  Species_CU_Name$cuid_cc[r] <- out
}

sum(!is.na(Species_CU_Name$cuid_cc))/nrow(Species_CU_Name) # 0.42
nrow(Species_CU_Name[!(is.na(Species_CU_Name$cuid) & is.na(Species_CU_Name$cuid_cc)),])/nrow(Species_CU_Name) # 0.97

# check if there are differences:
Species_CU_Name_noNA <- Species_CU_Name[!is.na(Species_CU_Name$cuid) & !is.na(Species_CU_Name$cuid_cc),]
Species_CU_Name_noNA[Species_CU_Name_noNA$cuid != Species_CU_Name_noNA$cuid_cc,] # 0, all good

Species_CU_Name[is.na(Species_CU_Name$cuid) & is.na(Species_CU_Name$cuid_cc),]


# c. Use the decoder for Haida Gwaii: HG_conservationunits.csv
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Haida Gwaii PSE/Data & Assessments/haida-gwaii-status/Data")
filename <- "HG_conservationunits.csv"
decoder_Haida_Gwaii <- read.csv(paste(path,filename,sep = "/"))
head(decoder_Haida_Gwaii)

# make some corrections and modifications
decoder_Haida_Gwaii$species[decoder_Haida_Gwaii$species == "Pink (Even)"] <- "Pink (even)"
decoder_Haida_Gwaii$species[decoder_Haida_Gwaii$species == "Pink (Odd)"] <- "Pink (odd)"
decoder_Haida_Gwaii$species[grepl("Sockeye",decoder_Haida_Gwaii$species)] <- "Sockeye"
decoder_Haida_Gwaii$trtc_cu_modif <- gsub("[_|-]"," ",decoder_Haida_Gwaii$trtc_cu)
decoder_Haida_Gwaii$culabel_modif <- tolower(decoder_Haida_Gwaii$culabel)
decoder_Haida_Gwaii$culabel_modif <- gsub("[_|-]"," ",decoder_Haida_Gwaii$culabel_modif) 

Species_CU_Name$cuid_hg <- NA
for(r in 1:nrow(Species_CU_Name)){
  # r <- 72
  sp_cu <- Species_CU_Name[r,]
  sp <- sp_cu$Species
  cu_name_modif <- sp_cu$CU_Name_modif
  cu_modif <- sp_cu$CU_modif
  
  out1 <- decoder_Haida_Gwaii$cuid[decoder_Haida_Gwaii$trtc_cu_modif == cu_modif &
                                      decoder_Haida_Gwaii$species == sp_cu$Species]
  
  out2 <- decoder_Haida_Gwaii$cuid[decoder_Haida_Gwaii$culabel_modif == cu_name_modif &
                                      decoder_Haida_Gwaii$species == sp]
  
  out <- unique(c(out1,out2))
  out <- out[!is.na(out)]
  
  if(length(out) == 0){
    out <- NA
  }else if(length(out) > 1){
    print(r)
    print(out)
  }
  Species_CU_Name$cuid_hg[r] <- out
}

sum(!is.na(Species_CU_Name$cuid_hg))/nrow(Species_CU_Name) # 0.42
nrow(Species_CU_Name[!(is.na(Species_CU_Name$cuid) &
                         is.na(Species_CU_Name$cuid_cc) &
                         is.na(Species_CU_Name$cuid_hg)),])/nrow(Species_CU_Name) # 0.97

# check if there are differences:
Species_CU_Name_noNA <- Species_CU_Name[!is.na(Species_CU_Name$cuid) & !is.na(Species_CU_Name$cuid_hg),]
Species_CU_Name_noNA[Species_CU_Name_noNA$cuid != Species_CU_Name_noNA$cuid_hg,] # 0, all good

Species_CU_Name[is.na(Species_CU_Name$cuid) & 
                  is.na(Species_CU_Name$cuid_cc) &
                  is.na(Species_CU_Name$cuid_hg),]

# Species           CU_Name            CU     CU_Name_modif      CU_modif 
# Sockeye         Prudhomme    SX_L-19-49         prudhomme    SX L 19 49   
# Sockeye         Shawatlan    SX_L-19-54         shawatlan    SX L 19 54   
# Sockeye Babine-Early-Wild SX_L-21-02-EW babine early wild SX L 21 02 EW   
# Sockeye   Babine-Mid-Wild SX_L-21-02-MW   babine mid wild SX L 21 02 MW  

pattern <- "prudhomme"
pattern <- "prud"
pattern <- "shawatlan"
pattern <- "babine"
conservationunits_decoder[grepl(pattern,conservationunits_decoder$cu_name_pse_modif),]
conservationunits_decoder[grepl(pattern,conservationunits_decoder$cu_name_dfo_modif),]
decoder_centralCoast[grepl(pattern,decoder_centralCoast$culabel_modif),]
decoder_Haida_Gwaii[grepl(pattern,decoder_Haida_Gwaii$culabel_modif),]

# --> These are all Lake Sockeye so we move on.

# Collect all the cuids and attribute them to 
Species_CU_Name$cuid_final <- Species_CU_Name$cuid
condition_cc <- is.na(Species_CU_Name$cuid) & !is.na(Species_CU_Name$cuid_cc)
sum(condition_cc) # 1
condition_hg <- is.na(Species_CU_Name$cuid) & !is.na(Species_CU_Name$cuid_hg)
sum(condition_hg) # 0

Species_CU_Name$cuid_final[condition_cc] <- Species_CU_Name$cuid_cc[condition_cc]

dataset_5_all$CUID <- NA
for(r in 1:nrow(Species_CU_Name)){
  # r <- 1
  rg <- Species_CU_Name$region[r]
  sp <- Species_CU_Name$Species[r]
  cu_name <- Species_CU_Name$CU_Name[r]
  cu_name_pse <- Species_CU_Name$cu_name_pse[r]
  cuid <- Species_CU_Name$cuid_final[r]
  
  dataset_5_all$CUID[dataset_5_all$Species == sp & dataset_5_all$CU_Name == cu_name] <- cuid
}

# 
head(dataset_5_all)

# Add columns KF_alpha lnRS Ricker_resid and R_S
dataset_5_all$KF_alpha <- dataset_5_all$lnRS <- dataset_5_all$Ricker_resid <- NA
dataset_5_all$R_S <- dataset_5_all$Recruits/dataset_5_all$Spawners

# Select the necessary columns
dataset_5_all_cut <- dataset_5_all[,c("CUID","Species","Year","Spawners","Recruits",
                                      "KF_alpha","lnRS","Ricker_resid","R_S")]

# Export file for Central Coast:
CUs_CC <- Species_CU_Name$cuid_final[Species_CU_Name$region == "Central Coast"]
CUs_CC <- CUs_CC[!is.na(CUs_CC)]
dataset_5_CC <- dataset_5_all_cut[dataset_5_all_cut$CUID %in% CUs_CC,]
# View(dataset_5_CC)
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Central Coast PSE/analysis/central-coast-status/Output")
date <- Sys.Date()
filename <- paste0("dataset_5_",date,".csv")
# write.csv(dataset_5_CC,paste(path,filename,sep="/"),row.names = F)


# Export file for Haida Gwaii:
CUs_HG <- Species_CU_Name$cuid_final[Species_CU_Name$region == "Haida Gwaii"]
CUs_HG <- CUs_HG[!is.na(CUs_HG)]
dataset_5_HG <- dataset_5_all_cut[dataset_5_all_cut$CUID %in% CUs_HG,]
# View(dataset_5_HG)
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Haida Gwaii PSE/Data & Assessments/haida-gwaii-status/Output")
date <- Sys.Date()
filename <- paste0("dataset_5_",date,".csv")
# write.csv(dataset_5_HG,paste(path,filename,sep="/"),row.names = F)

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
# Check why there are identical biological status probabilities between Smsy and Smsy80 --------
# 
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

#
# Katy's request about Columbia Lake SX CU 1300 dataset_101 and dataset_102 ------
# https://salmonwatersheds.slack.com/archives/C01D2S4PRC2/p1701474308941739
#' - update dataset 101 file for Columbia that includes the percentile confidence
#' intervals for cuid 1300 Osoyoos lake sockeye?
#' - fields 25%_spw_lower, 25%_spw_upper, 75%_spw_lower, 75%_spw_upper are empty in database

#'* Import the most recent dataset_101 *
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Columbia/data & analysis/analysis/columbia-status/Output")
pattern <- "dataset_101"
filesList <- list.files(path = path)
filesList <- filesList[grepl(pattern,filesList)]
mostRecentF <- filesList[file.info(paste(path,filesList,sep = "/"))$mtime == max(file.info(paste(path,filesList,sep = "/"))$mtime)]
# "dataset_101.May272022.csv"
dataset_101 <- read.csv(paste(path,mostRecentF,sep = "/"),header = T)
dataset_101 <- dataset_101[,-1]
head(dataset_101)
nrow(dataset_101)

# Import another dataset_101 file from Fraser_VIMI for comparison because the headers
# are bit different:
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Fraser_VIMI/analysis/Fraser status assessment/Output")
pattern <- "dataset_101"
filesList <- list.files(path = path)
filesList <- filesList[grepl(pattern,filesList)]
mostRecentF <- filesList[file.info(paste(path,filesList,sep = "/"))$mtime == max(file.info(paste(path,filesList,sep = "/"))$mtime)]
dataset_101_VIMI <- read.csv(paste(path,mostRecentF,sep = "/"),header = T)
dataset_101_VIMI <- dataset_101_VIMI[,-1]
head(dataset_101_VIMI)

# Import the benchmarks values and biostatus:
pattern <- "biological_status_SH_percentiles"
biological_status_percentiles <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                                wd_output = wd_output,
                                                                region = region,
                                                                species_all = species_all)
pattern <- "HS_percentiles_summary"
benchmarks_summary_percentiles <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                                 wd_output = wd_output,
                                                                 region = region,
                                                                 species_all = F)

# 
benchmarks_summary_percentiles_1300 <- benchmarks_summary_percentiles[benchmarks_summary_percentiles$region == "Columbia" &
                                                                      benchmarks_summary_percentiles$cuid == 1300,]

biological_status_percentiles_1300 <- biological_status_percentiles[biological_status_percentiles$region == "Columbia"  &
                                                                    biological_status_percentiles$cuid == 1300,]

# Fill dataset_101

# CHANGE: rename "cu" by "cuid"
colnames(dataset_101)[colnames(dataset_101)== "cu"] <- "cuid"

# CHANGE: change "hist_" for "percentile_"
col_hist <- colnames(dataset_101)[grepl("hist_",colnames(dataset_101))]
colnames(dataset_101)[colnames(dataset_101) %in% col_hist] <- gsub("hist","percentile",col_hist)

# CHANGE: add percentile_red_prob for each of the three colours (USE THE 0.5 upper threshold!!!)
dataset_101$percentile_prob_red <- biological_status_percentiles_1300$status_HSPercent_05_red
dataset_101$percentile_prob_yellow <- biological_status_percentiles_1300$status_HSPercent_05_amber
dataset_101$percentile_prob_green <- biological_status_percentiles_1300$status_HSPercent_05_green

# CHANGE: add percentile status
prob_status <- as.numeric(dataset_101[,c("percentile_prob_red","percentile_prob_yellow","percentile_prob_green")])
dataset_101$percentile_status <- c("poor","fair","good")[prob_status == max(prob_status)]

# CHANGE: fill the percentile_red/yellow/green columns
colHere <- paste0("percentile_",c("red","yellow","green"))
dataset_101[,colHere] <- "#FFFFFF"
dataset_101[,colHere][prob_status == max(prob_status)] <- c("#CC0000","#FFFF00","#009900")[prob_status == max(prob_status)]

# CHANGE: add extinct
dataset_101$extinct <- NA

# CHANGE: drop location
dataset_101 <- dataset_101[,colnames(dataset_101) != "location"]

# write CSV with date:
date <- Sys.Date()
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Columbia/data & analysis/analysis/columbia-status/Output")
# write.csv(dataset_101,paste0(path,"/","dataset_101_",date,".csv"),row.names = F)


#'* Import the most recent dataset_102 *
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Columbia/data & analysis/analysis/columbia-status/Output")
pattern <- "dataset_102"
filesList <- list.files(path = path)
filesList <- filesList[grepl(pattern,filesList)]
mostRecentF <- filesList[file.info(paste(path,filesList,sep = "/"))$mtime == max(file.info(paste(path,filesList,sep = "/"))$mtime)]
# mostRecentF <- "dataset_102.May272022.csv"
dataset_102 <- read.csv(paste(path,mostRecentF,sep = "/"),header = T)
dataset_102 <- dataset_102[,-1]
head(dataset_102)
nrow(dataset_102)

# CHANGE: rename "cu" by "cuid"
colnames(dataset_102)[colnames(dataset_102) == "cu"] <- "cuid"

# CHANGE: change "hist_" for "percentile_"
col_hist <- colnames(dataset_102)[grepl("hist_",colnames(dataset_102))]
colnames(dataset_102)[colnames(dataset_102) %in% col_hist] <- gsub("hist","percentile",col_hist)

# CHANGE: fill the dataset
dataset_102$curr_spw[dataset_102$location == 1300] <- round(biological_status_percentiles_1300$current_spawner_abundance)
dataset_102$curr_spw_start_year[dataset_102$location == 1300] <- biological_status_percentiles_1300$year_first
dataset_102$curr_spw_end_year[dataset_102$location == 1300] <- 2021 # biological_status_percentiles_1300$year_last
dataset_102$X25._spw[dataset_102$location == 1300] <- benchmarks_summary_percentiles_1300$m[benchmarks_summary_percentiles_1300$benchmark == "benchmark_0.25"]
dataset_102$X75._spw[dataset_102$location == 1300] <- benchmarks_summary_percentiles_1300$m[benchmarks_summary_percentiles_1300$benchmark == "benchmark_0.5"]

# CHANGE: add columns for 95% CI of the benchmarks
dataset_102$`25%_spw_lower` <- c(benchmarks_summary_percentiles_1300[benchmarks_summary_percentiles_1300$benchmark == "benchmark_0.25","CI025"],NA)
dataset_102$`25%_spw_upper` <- c(benchmarks_summary_percentiles_1300[benchmarks_summary_percentiles_1300$benchmark == "benchmark_0.25","CI975"],NA)
dataset_102$`75%_spw_lower` <- c(benchmarks_summary_percentiles_1300[benchmarks_summary_percentiles_1300$benchmark == "benchmark_0.5","CI025"],NA)
dataset_102$`75%_spw_upper` <- c(benchmarks_summary_percentiles_1300[benchmarks_summary_percentiles_1300$benchmark == "benchmark_0.5","CI975"],NA)

date <- Sys.Date()
date <- "2023-12-07"
path <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Columbia/data & analysis/analysis/columbia-status/Output")
# write.csv(dataset_102,paste0(path,"/","dataset_102_",date,".csv"),row.names = F)

#
# Group the priors for parameter prSmax and prCV used in HBSRM.R --------
#' Priors for Smax and its CV (i.e. prSmax and prCV), which are then used to 
#' determine mu and tau of parameter b (i.e. prmub and prtaub) used in the 
#' HBSR model (cf. HBSR.R).
#' The choice for these prior vlues is explained in (Korman and English 2013):
#' https://salmonwatersheds.ca/document/lib_318/
#' (cf. p. 13).
#' These prior values are present at the top of the SDdata.txt files present in 
#' the different region-specific folders. The goal is to get the data in these 
#' files and put them all together in a single CSV file that stays in GitHub.

table_rg_sp <- CUs_toCheck <- NULL
for(i_rg in 1:length(region)){ # 
  # i_rg <- 8
  
  regionHere <- region[i_rg]
  if(regionHere == "Vancouver Island & Mainland Inlets"){
    regionHere <- "VIMI"
  }
  regionHere <- gsub(" ","_",regionHere)
  
  # set the path of the input data sets for that specific region
  wd_data_input <- paste0(wd_data_regions[,regionHere])
  
  # Returns a list with the species and the corresponding path of the _SRdata files
  # (the most up to date)
  fndata <- SRdata_path_species_fun(wd = wd_data_input,
                                    species = species,
                                    species_all = species_all)
  
  species <- fndata$species  # species is updated is was NULL or certain species do not have a file
  fndata <- fndata$SRdata
  
  if(length(species) == 0){ # in case there is no 
    
    table_sp <- data.frame(region = region[i_rg], species = NA, species_abbr = NA,
                           cu_name_pse = NA,cuid = NA, prSmax = NA, prCV = NA)
    
  }else{
    
    table_sp <- NULL
    for(i_sp in 1:length(species)){
      # i_sp <- 1
      
      # import the number of CUs
      CUs_nb <- scan(file = fndata[i_sp], nlines = 1, skip = 1)
      
      # Import the prSmax and prCV for each CU
      tableHere <- read.table(file = fndata[i_sp], header = T, skip = 2, nrows = CUs_nb)

      # Convert species acronym to match conservationunits_decoder$species_abbr
      if(species[i_sp] == "SX"){
        species_var <- c("SEL","SER")
      }else if(species[i_sp] == "PK"){
        species_var <- c("PKO","PKE")
      }else if(species[i_sp] == "CN"){
        species_var <- "CK"
      }else{
        species_var <- species[i_sp]
      }
      
      # check the name of the CUs
      tableHere$cu_name_pse <- tableHere$cuid <- tableHere$region <- tableHere$species <- NA
      for(i_cu in 1:nrow(tableHere)){
        # i_cu <- 1
        
        if(suppressWarnings(!is.na(as.numeric(tableHere$CU[i_cu])[1]))){ # if nameVariations is a cuid and not the cu name
          
          cu_cuid <- as.numeric(tableHere$CU[i_cu])
          conservationunits_decoder_rg_sp_cu <- conservationunits_decoder[conservationunits_decoder$region == region[i_rg] &
                                                                            conservationunits_decoder$cuid == cu_cuid,]
        }else{
          
          CU_here <- gsub("_"," ",tableHere$CU[i_cu])
          
          if(species[i_sp] == "PK"){
            # Import the fish counts for R and S per year for each CU (just in case we have to find the cu_name_pse for a PK)
            d <- read.table(file = fndata[i_sp], header = T, skip = 3 + CUs_nb, 
                            fill = TRUE, stringsAsFactors = FALSE)
            d$BY <- as.numeric(d$BY) # brood year
            d <- d[d$CU == tableHere$CU[i_cu],]
            spawnerAbundance <- d$Esc
            names(spawnerAbundance) <- d$BY
            nameVariations <- CU_name_variations_fun(CUname = CU_here, 
                                                     spawnerAbundance = spawnerAbundance,
                                                     speciesAcronym = species[i_sp])
            nameVariations <- nameVariations[grepl("even|odd",nameVariations)]
            
          }else{
            nameVariations <- CU_name_variations_fun(CUname = CU_here, 
                                                     speciesAcronym = species[i_sp])
          }
          
          nameVariations <- unique(nameVariations)
          
          conservationunits_decoder_rg_sp_cu <- conservationunits_decoder[conservationunits_decoder$region == region[i_rg] &
                                                                            conservationunits_decoder$species_abbr %in% species_var &
                                                                            conservationunits_decoder$cu_name_pse %in% nameVariations,]
          
        }
        
        if(nrow(conservationunits_decoder_rg_sp_cu) != 1){ # cu-specific fixes
          
          if(nameVariations[1] == "Nass-Skeena Estuary (even)" | 
             nameVariations[1] == "Skeena Estuary" |
             nameVariations[1] == "Lower Skeena" |
             nameVariations[1] == "Middle Skeena" |
             nameVariations[1] == "Portland Canal Observatory CM" |
             nameVariations[1] == "Portland Inlet CM" |
             nameVariations[1] == "Smith Inlet" |
             nameVariations[1] == "Rivers Inlet" |
             nameVariations[1] == "Spiller-Fitz-Hugh-Burke" |
             nameVariations[1] == "Bella Colla-Dean Rivers" |
             nameVariations[1] == "Hecate Lowlands" |
             nameVariations[1] == "Mussel-Kynock" |
             nameVariations[1] == "Douglas-Gardner"){
            
            # these CUs are in the wrong region. Add them in the correct region 
            # and eventually remove the duplicate row if it was already in present
            # in Skeena --> but check that the priors are the same!
            
            # For "Nass-Skeena Estuary (even)" PK: The region is not Nass but Skeena (cuid = 219)
            # For "Skeena Estuary" CM: The region is not VIMI but Skeena (cuid = 220)
            # For "Lower Skeena" CM: The region is not VIMI but Skeena (cuid = 221)
            # For "Middle Skeena" CM: The region is not VIMI but Skeena (cuid = 214)
            # For "Portland Canal Observatory CM" CM: The region is not VIMI but Nass (cuid = 406)
            # For "Portland Inlet CM" CM: The region is not VIMI but Nass (cuid = 404)
            # For "Smith Inlet" CM: The region is not VIMI but Central Coast (cuid = 500)
            # For "Rivers Inlet" CM: The region is not VIMI but Central Coast (cuid = 501)
            # For "Spiller-Fitz-Hugh-Burke" CM: The region is not VIMI but Central Coast (cuid = 503)
            # For "Bella Colla-Dean Rivers" CM: The region is not VIMI but Central Coast (cuid = 504)
            # For "Hecate Lowlands" CM: The region is not VIMI but Central Coast (cuid = 506)
            # For "Mussel-Kynoch" CM: The region is not VIMI but Central Coast (cuid = 507)
            # For "Douglas-Gardner" CM: The region is not VIMI but Central Coast (cuid = 508)
            
            conservationunits_decoder_rg_sp_cu <- conservationunits_decoder[conservationunits_decoder$species_abbr %in% species_var & 
                                                                              conservationunits_decoder$cu_name_pse %in% nameVariations,]
            
            CUs_toCheck_new <-  conservationunits_decoder_rg_sp_cu[,c("region","species_abbr","cuid","cu_name_pse")]
            CUs_toCheck_new$region_SRdata <- region[i_rg]
            CUs_toCheck_new <- CUs_toCheck_new[,c("region_SRdata","region","species_abbr","cuid","cu_name_pse")]
            CUs_toCheck_new$prSmax <- tableHere$prSmax[i_cu]
            CUs_toCheck_new$prCV <- tableHere$prCV[i_cu]
            CUs_toCheck <- rbind(CUs_toCheck,CUs_toCheck_new)
            
          }else if(nrow(conservationunits_decoder_rg_sp_cu) > 1 & length(unique(conservationunits_decoder_rg_sp_cu$pooledcuid)) == 1){
            # case of pooled CUs --> retain row with pooledcuid == cuid
            conservationunits_decoder_rg_sp_cu <- conservationunits_decoder_rg_sp_cu[conservationunits_decoder_rg_sp_cu$cuid == conservationunits_decoder_rg_sp_cu$pooledcuid,]
            
          }else if(regionHere == "VIMI" & nrow(conservationunits_decoder_rg_sp_cu) == 0){
            
            #' CUs in VIMI were pooled with the ones from Fraser in the SRdata.txt files.
            #' But it seems that all the CUs in these SRdata.text files are from 
            #' Fraser. Here is just a check that if the CU is not found in 
            #' conservationunits_decoder for the VIMI that is because it is present 
            #' in Fraser.
            
            print("In VIMI with no rows !!!")
            
            if(suppressWarnings(!is.na(as.numeric(tableHere$CU[i_cu])[1]))){ # if nameVariations is a cuid and not the cu name
              
              cu_cuid <- as.numeric(tableHere$CU[i_cu])
              conservationunits_decoder_rg_sp_cu <- conservationunits_decoder[conservationunits_decoder$region == "Fraser" &
                                                                                conservationunits_decoder$cuid == cu_cuid,]
              
            }else{
              conservationunits_decoder_rg_sp_cu <- conservationunits_decoder[conservationunits_decoder$region == "Fraser" &
                                                                                conservationunits_decoder$species_abbr %in% species_var &
                                                                                conservationunits_decoder$cu_name_pse %in% nameVariations,]
            }

            if(nrow(conservationunits_decoder_rg_sp_cu) != 1){
              print(paste("VIMI issue: i_rg:",i_rg,", i_sp:",i_sp,", i_cu:",i_cu))
              print(conservationunits_decoder_rg_sp_cu)
            }
            
            conservationunits_decoder_rg_sp_cu$cu_name_pse <- NA
            conservationunits_decoder_rg_sp_cu$cuid <- NA
            conservationunits_decoder_rg_sp_cu$region <- "Vancouver Island & Mainland Inlets"
            conservationunits_decoder_rg_sp_cu$species_name <- NA
            
            tableHere$prSmax <- NA
            tableHere$prCV <- NA
            
          }else{
            print(paste("i_rg:",i_rg,", i_sp:",i_sp,", i_cu:",i_cu))
            print(conservationunits_decoder_rg_sp_cu)
          }
        }
        
        name_pse <- conservationunits_decoder_rg_sp_cu$cu_name_pse
        cu_cuid <- conservationunits_decoder_rg_sp_cu$cuid
        region_here <- conservationunits_decoder_rg_sp_cu$region
        species_here <- conservationunits_decoder_rg_sp_cu$species_name
        species_abbr_here <- conservationunits_decoder_rg_sp_cu$species_abbr
        
        tableHere$cu_name_pse[i_cu] <- name_pse
        tableHere$cuid[i_cu] <- cu_cuid
        tableHere$species[i_cu] <- species_here
        tableHere$species_abbr[i_cu] <- species_abbr_here
        tableHere$region[i_cu] <- region_here
        
        # print("****")
        # print(name_pse)
        # print(species_here)
        # print(region_here)
      }
      
      if(is.null(table_sp)){
        table_sp <- tableHere[,c("region","species","species_abbr","cu_name_pse","cuid","prSmax","prCV")]
      }else{
        table_sp <- rbind(table_sp,tableHere[,c("region","species","species_abbr","cu_name_pse","cuid","prSmax","prCV")])
      }
    }
  }
  
  if(is.null(table_rg_sp)){
    table_rg_sp <- table_sp
  }else{
    table_rg_sp <- rbind(table_rg_sp,table_sp)
  }
}

# region with no data:
table_rg_sp[is.na(table_rg_sp$cu_name_pse),]
table_rg_sp[is.na(table_rg_sp$prSmax),]

# Check if the CUs in CUs_toCheck were already present in there correct region
# if that's the case do they have the same prior values
CUs_toCheck$nDuplicates <- CUs_toCheck$sameValPriors <- NA
for(r in 1:nrow(CUs_toCheck)){
  # r <- 1
  table_rg_sp_cut <- table_rg_sp[table_rg_sp$region == CUs_toCheck$region[r] &
                                   table_rg_sp$species_abbr == CUs_toCheck$species_abbr[r] &
                                   table_rg_sp$cu_name_pse == CUs_toCheck$cu_name_pse[r] &
                                   table_rg_sp$cuid == CUs_toCheck$cuid[r],]
  
  CUs_toCheck$nDuplicates[r] <- nrow(table_rg_sp_cut)
  
  if(nrow(table_rg_sp_cut) > 1){
    print("***")
    print(CUs_toCheck[r,])
    print(table_rg_sp_cut)
    
    if(table_rg_sp_cut$prSmax[1] != table_rg_sp_cut$prSmax[2] |
       table_rg_sp_cut$prCV[1] != table_rg_sp_cut$prCV[2]){
      CUs_toCheck$sameValPriors[r] <- F
    }else{
      CUs_toCheck$sameValPriors[r] <- T
    }
  }
}

CUs_toCheck

# Remove the row for cuid in Nass even if the values for the prior are different
#' Eric: "yes, stick with the Skeena values."
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1702660907090279?thread_ts=1702603217.471789&cid=CJ5RVHVCG

# region_SRdata region species_abbr cuid                cu_name_pse sameValPriors nDuplicates
#          Nass Skeena          PKE  219 Nass-Skeena Estuary (even)         FALSE           2

# 
table_rg_sp <- table_rg_sp[!(table_rg_sp$cuid == 219 &
                               table_rg_sp$region == "Skeena" &
                               table_rg_sp$prSmax == CUs_toCheck$prSmax &
                               table_rg_sp$prCV == CUs_toCheck$prCV),]

# write.csv(table_rg_sp,paste0(wd_data,"/priors_HBSRmodel.csv"),row.names = F)



#
# Follow up check with previous check: are they CUs with SR data by no prior value? -------
#

#' Import the prior values for the HBSR model parameters prSmax and prCV that are
#' used in HBSRM.R (the file is created in checks_fixes.R and contain the values 
#' of these priors that were originally contained in SRdata.txt files that are 
#' found in the "HBM and status" subfolders in each region-specific folders.
priors_HBSRmodel <- read.csv(paste0(wd_data,"/priors_HBSRmodel.csv"),header = T)

CUs_toCheck <- NULL
for(i_rg in 1:length(region)){
  
  # i_rg <- 5
  
  recruitsperspawner_rg <- recruitsperspawner[recruitsperspawner$region == region[i_rg],]
  
  species <- unique(recruitsperspawner_rg$species_name)
  
  species <- species[species != "Steelhead"]
  
  species_acro <- sapply(X = species,FUN = function(sp){
    species_acronym_df$species_acro[species_acronym_df$species_name == sp]
  })
  
  regionName <- region[i_rg]
  if(region[i_rg] == "Vancouver Island & Mainland Inlets"){
    regionName <- "VIMI"
  }
  
  priors_HBSRmodel_rg <- priors_HBSRmodel[priors_HBSRmodel$region == region[i_rg],]

  if(sum(!is.na(recruitsperspawner_rg$spawners)) == 0 | sum(!is.na(recruitsperspawner_rg$recruits)) == 0){
    
    print(paste0("*** There is no data in recruitsperspawner.csv for salmon in ",region[i_rg]," ***"))
    
  }else{
    
    for(i_sp in 1:length(unique(species_acro))){
      
      # i_sp <- 4
      
      speciesAcroHere <- unique(species_acro)[i_sp]
      speciesHere <- species_acronym_df$species_name[species_acronym_df$species_acro %in% speciesAcroHere]
      
      recruitsperspawner_rg_sp <- recruitsperspawner_rg[recruitsperspawner_rg$species_name %in% speciesHere,]
      
      print(paste0("*** Plot for: ",region[i_rg]," - ",speciesAcroHere," ***"))
      
      # organize the data into a year x CU for R and S:
      CUs <- unique(recruitsperspawner_rg_sp$cu_name_pse)
      CUs_cuid <- sapply(X = CUs,FUN = function(cu){unique(recruitsperspawner_rg_sp$cuid[recruitsperspawner_rg_sp$cu_name_pse == cu])})
      # unique(recruitsperspawner_rg_sp[,c("cu_name_pse","cuid")])
      nCUs <- length(CUs)
      Yrs <- min(recruitsperspawner_rg_sp$year):max(recruitsperspawner_rg_sp$year)
      nYrs <- length(Yrs)
      
      S <- R <- matrix(nrow = nYrs, ncol = nCUs, dimnames = list(Yrs,CUs))
      for(j in 1:nCUs){
        # j <- 1
        dj <- subset(recruitsperspawner_rg_sp,cu_name_pse == CUs[j])
        S[as.character(dj$year),j] <- dj$spawners # dj$Esc
        R[as.character(dj$year),j] <- dj$recruits # dj$Rec
      }
      
      # remove the row with NAs in S but not R and vice versa of a same CU 
      SR_l <- cuSR_removeNA_fun(R = R, S = S)
      R <- SR_l$R
      S <- SR_l$S
      
      # replace 0s by 1 to avoid the lm(log(R/S)~ S) to crash
      R <- apply(X = R,MARGIN = 2,FUN = function(c){
        # c <- R[,3]
        out <- c
        out[which(out == 0)] <- 1
        return(out)
      })
      S <- apply(X = S,MARGIN = 2,FUN = function(c){
        # c <- R[,3]
        out <- c
        out[which(out == 0)] <- 1
        return(out)
      })
      
      #' filter CUs with less than MinSRpts data points --> SKIP TO HAVE THE INFO ABOUT THOSE
      # CuToRemove <- c()
      # for(j in 1:ncol(S)){
      #   # j <- 1
      #   CUHere <- colnames(S)[j]
      #   if(sum(!is.na(S[,CUHere])) < MinSRpts | sum(!is.na(R[,CUHere])) < MinSRpts){
      #     CuToRemove <- c(CuToRemove,CUHere)
      #   }
      # }
      # S <- S[,!colnames(S) %in% CuToRemove, drop = F]
      # R <- R[,!colnames(R) %in% CuToRemove, drop = F]
      # CUs <- CUs[!CUs %in% CuToRemove]
      # CUs_cuid <- sapply(X = CUs,FUN = function(cu){
      #   unique(recruitsperspawner_rg_sp$cuid[recruitsperspawner_rg_sp$cu_name_pse == cu])
      # })
      # nCUs <- length(CUs)
      # 
      # # nameFile <- paste0(gsub(" ","_",region[i_rg]),"_",
      # #                    gsub(" ","_",species[i_sp]),"_",
      # #                    species_acro[i_sp],"_SR_matrices.rds")
      # SR_l$R <- R
      # SR_l$S <- S
      
      #
      species_abbr <- species_acronym_df$species_acro2_details[species_acronym_df$species_acro == species_acro[i_sp]]
      priors_HBSRmodel_rg_sp <- priors_HBSRmodel_rg[priors_HBSRmodel_rg$species_abbr %in% species_abbr,]
      
      CUs_withoutPrior <- CUs[! CUs %in% priors_HBSRmodel_rg_sp$cu_name_pse]
      CUs_withoutRS <- priors_HBSRmodel_rg_sp$cu_name_pse[!priors_HBSRmodel_rg_sp$cu_name_pse %in% CUs]
      
      colnames(recruitsperspawner_rg_sp)[colnames(recruitsperspawner_rg_sp) == "species_name"] <- "species"
      
      colInCommon <- c("region","species","cuid","cu_name_pse")
      SR_noPrior <- recruitsperspawner_rg_sp[recruitsperspawner_rg_sp$cu_name_pse %in% CUs_withoutPrior,colInCommon]
      if(nrow(SR_noPrior) > 0){
        SR_noPrior <- unique(SR_noPrior)
        SR_noPrior$nb_datapoints_RS <- sapply(X = 1:ncol(R[,CUs_withoutPrior,drop = F]), 
                                              FUN = function(c){
                                                out <- min(sum(!is.na(R[,c,drop = F])),
                                                           sum(!is.na(S[,c,drop = F])))
                                                })
        SR_noPrior$prSmax <- NA
        SR_noPrior$prCV <- NA
        SR_noPrior$SRdata <- T
        SR_noPrior$priordata <- F
        CUs_toCheck <- rbind(CUs_toCheck,SR_noPrior)
      }
      
      prior_noSR <- priors_HBSRmodel_rg_sp[priors_HBSRmodel_rg_sp$cu_name_pse %in% CUs_withoutRS,c(colInCommon,"prSmax","prCV")]
      if(nrow(prior_noSR) > 0){
        prior_noSR$nb_datapoints_RS <- NA
        prior_noSR$SRdata <- F
        prior_noSR$priordata <- T
        CUs_toCheck <- rbind(CUs_toCheck,prior_noSR)
      }
    } # species loop
  }  # if there is data for this region
} # region loop

CUs_toCheck

# CUs with no SR data:
CUs_toCheck[CUs_toCheck$priordata,]
#' - For lake-type sockeye: it is expected because we may have estimates of lake 
#' productivity to inform those priors even though there’s not SR data. For these
#' cases, we should consider applying habitat-based benchmarks, which we will dig
#'into more over the next 6 months.
#' - For the other ones: there is SR data in the SRdata.txt. Why is that data not 
#' in the database/recruitsperspawner.

# CUs with no prior estimates:
CUs_toCheck[CUs_toCheck$SRdata,]
#' - For CUs with cyclic dynamics: we won’t run the HBSM model or assess status 
#' (for now).
#' - For the other CUs: We should definitely come up with prior values following 
#' Korman and English 2023 (specific approach in pink below). Note that they start
#' with uninformative (CV = 10), and then only use CV = 1 for those CUs that had
#' convergence issues or huge CIs on the b parameter.

# 
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1702671161183209?thread_ts=1702603217.471789&cid=CJ5RVHVCG






