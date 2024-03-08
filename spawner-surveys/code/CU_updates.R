


#'******************************************************************************
#' The goal of the script is to 
#' 
#' Previous script: Fraser_salmon_CU_updates.Rmd
#' 
#' 
#' 
#' Files imported (from dropbox):
#' - 
#' 
#' Files produced: 
#' - 
#' 

#'******************************************************************************

# NOTE (to remove eventually): original script is:
# 1_nuseds_data_collationJun72023.R in:
# \X Drive\1_PROJECTS\1_Active\Fraser_VIMI\analysis\Compilation\Code

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

wds_l <- set_working_directories_fun(subDir = subDir_projects$spawner_surveys,
                                     Export_locally = F)
wd_head <- wds_l$wd_head
wd_project <- wds_l$wd_project
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS

wd_references_dropbox <- paste(wd_X_Drive1_PROJECTS,
                               wds_l$wd_project_dropbox,
                               "references",sep="/")

wd_data_dropbox <- paste(wd_X_Drive1_PROJECTS,
                         wds_l$wd_project_dropbox,
                         "data",sep="/")

wd_documents <- paste(wd_project,"documents",sep="/")

wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# Loading packages & functions
library(tidyr)

source("code/functions.R")

# Import files ------

#'* Import the most recent NuSEDS_escapement_data_collated file *
nuseds <- import_mostRecent_file_fun(wd = wd_output, 
                                     pattern = "NuSEDS_escapement_data_collated")

head(nuseds)
nrow(nuseds) # 307217

#'* Files from PSF database *

#'Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- F
update_file_csv <- F

#' Import streamlocationids to obtain the streamID 
streamlocationids <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[8],
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)

head(streamlocationids)

#' Import the conservationunits_decoder.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

head(conservationunits_decoder)

sum(is.na(conservationunits_decoder$cu_index))    # 46
sum(is.na(conservationunits_decoder$cu_name_pse))
cond <- is.na(conservationunits_decoder$cu_index)
conservationunits_decoder[cond,]

#' Import the streamspawnersurveys_output from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
streamspawnersurveys_output <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[4],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

head(streamspawnersurveys_output)


#
# Edit the NUSEDS dataset for the PSE ----

#'* Edite FULL_CU_IN for several POP_IDs * 
# Corrections in CU assignment for central coast chum from Carrie Holt
# https://salmonwatersheds.slack.com/archives/C017N5NSCJY/p1683774240661029?thread_ts=1683735939.696999&cid=C017N5NSCJY
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1705426563165399?thread_ts=1705344122.088409&cid=CJ5RVHVCG

nuseds$FULL_CU_IN_PSF <- nuseds$FULL_CU_IN

#' Import the corrections:
full_cu_l <- update_for_FULL_CU_IN_l()

for(i in 1:length(full_cu_l)){
  # i <- 1
  FULL_CU_IN_here <- names(full_cu_l)[i]
  print(FULL_CU_IN_here)
  
  POP_IDs_here <- full_cu_l[[i]]
  
  cond <- nuseds$POP_ID %in% POP_IDs_here
  nuseds$FULL_CU_IN_PSF[cond] <- FULL_CU_IN_here
}

#'* FIX: South Atnarko Lakes *
#' GFE_ID 968 for sockeye should be attributed to South Atnarko Lakes CU 
#' (cf. Population meeting from 05/03/2024)
cond <- nuseds$GFE_ID == 968 & nuseds$species_abbr %in% c("SEL","SER")
unique(nuseds$CU_name[cond]) # "NORTHERN COASTAL FJORDS"
nuseds$CU_name[cond] <- toupper("South Atnarko Lakes")
#

#'* Edits create stream_survey_quality from ESTIMATE_CLASSIFICATION *
#' cf. Table 4.5 in section 4.1.3 of the Tech Report
estim_class_nuseds <- unique(nuseds$ESTIMATE_CLASSIFICATION)
estim_class_nuseds
unique(streamspawnersurveys_output$stream_survey_quality)

nuseds$stream_survey_quality <- NA
for(ecn in estim_class_nuseds){
  # ecn <- estim_class_nuseds[1]
  cond_nuseds <- nuseds$ESTIMATE_CLASSIFICATION == ecn
  
  if(ecn == "TRUE ABUNDANCE (TYPE-1)"){
    out <- "High"
  }else if(ecn == "TRUE ABUNDANCE (TYPE-2)"){
    out <- "Medium-High"
  }else if(ecn == "TRUE ABUNDANCE (TYPE-2)"){
    out <- "Medium-High"
  }else if(ecn == "RELATIVE ABUNDANCE (TYPE-3)"){
    out <- "Medium"
  }else if(ecn == "RELATIVE ABUNDANCE (TYPE-4)"){
    out <- "Medium-Low"
  }else if(ecn %in% c("RELATIVE ABUNDANCE (TYPE-5)",
                      "RELATIVE: CONSTANT MULTI-YEAR METHODS")){
    out <- "Low"
  }else if(ecn %in% c("PRESENCE/ABSENCE (TYPE-6)",
                      "PRESENCE-ABSENCE (TYPE-6)",
                      "RELATIVE: VARYING MULTI-YEAR METHODS")){
    out <- "Low"
  }else if(ecn == "UNKNOWN"){
    out <- "Unknown"
  }else if(ecn %in% c("","NO SURVEY THIS YEAR","NO SURVEY")){
    out <- NA
  }else{
    print(ecn)
  }
  print(out)
  nuseds$stream_survey_quality[cond_nuseds] <- out
}

unique(nuseds$stream_survey_method)

#'* rename ESTIMATE_METHOD to stream_survey_method *
colnames(nuseds)[colnames(nuseds) == "ESTIMATE_METHOD"] <- "stream_survey_method"

#'* rename MAX_ESTIMATE to stream_observed_count *
colnames(nuseds)[colnames(nuseds) == "MAX_ESTIMATE"] <- "stream_observed_count"

#
# Find cuid from conservationunits_decoder -----

#'* Provide a cuid to each row in nuseds using conservationunits_decoder *
#' using:
#' - 1st: cu_index & FULL_CU_IN_PSF
#' - 2nd: (if 1st does not work): CU_name and cu_name_dfo or cu_name_dfo_pse
#' For 2nd case: 
#' nuseds$CU_name does not correspond exactly to conservationunits_decoder$cu_name_pse 
#' not to conservationunits_decoder$cu_name_dfo. Need to do some tricks to match
#' CU names:
#' - replace "  ", "-", "<<BIN>>" and "<<EXTIRPATED>>" by ""
#' - to lower case

CU_name_species <- unique(nuseds[,c("CU_name","species_abbr","FULL_CU_IN","FULL_CU_IN_PSF","CU_TYPE")])
nrow(CU_name_species) # 415

conservationunits_decoder$cu_name_pse_modif <- tolower(conservationunits_decoder$cu_name_pse)
conservationunits_decoder$cu_name_dfo_modif <- tolower(conservationunits_decoder$cu_name_dfo)
CU_name_species$CU_name_modif <- tolower(CU_name_species$CU_name)

chara <- c(" ","-","<<BIN>>","<<EXTIRPATED>>")
for(c in chara){
  conservationunits_decoder$cu_name_pse_modif <- gsub(c,"",conservationunits_decoder$cu_name_pse_modif)
  conservationunits_decoder$cu_name_dfo_modif <- gsub(c,"",conservationunits_decoder$cu_name_dfo_modif)
  CU_name_species$CU_name_modif <- gsub(c,"",CU_name_species$CU_name_modif)
}

nuseds$cuid <- NA

count <- 1
for(r in 1:nrow(CU_name_species)){
  # r <- 6
  cu_name_here <- tolower(CU_name_species$CU_name[r])
  species_here <- CU_name_species$species_abbr[r]         # "CM"  "CK"  "CO"  "PKE" "PKO" "SER" "SEL"
  cu_type_here <- CU_name_species$CU_TYPE[r]
  #cu_name_here_modif <- CU_name_species$CU_name_modif[r]
  full_cu_in_here <- CU_name_species$FULL_CU_IN[r]
  full_cu_in_psf_here <- CU_name_species$FULL_CU_IN_PSF[r]
  
  # try match with FULL_CU_IN_PSF
  cond <- conservationunits_decoder$cu_index == full_cu_in_psf_here &
    !is.na(conservationunits_decoder$cu_index)
  
  # try with FULL_CU_IN
  if(sum(cond) == 0){ 
    
    cond <- conservationunits_decoder$cu_index == full_cu_in_here &
      !is.na(conservationunits_decoder$cu_index)
    
    if(sum(cond) != 0){
      print(paste0(count,r," - FULL_CU_IN (",full_cu_in_here,") used and not FULL_CU_IN_PSF (",full_cu_in_psf_here,") --> update the decoder?"))
      count <- count + 1
    }
  }
  
  # try match with CU_name
  if(sum(cond) == 0){ 
    
    cu_name_here_modif <- CU_name_species$CU_name_modif[r]
    
    cond <- (grepl(cu_name_here_modif,conservationunits_decoder$cu_name_dfo_modif) | 
               grepl(cu_name_here_modif,conservationunits_decoder$cu_name_pse_modif)) & 
      conservationunits_decoder$species_abbr == species_here
    
  }
  
  if(sum(cond) == 0){
    
    print(paste(count,r,species_here,cu_type_here,full_cu_in_psf_here,cu_name_here,sep = " - "))
    count <- count + 1
    
  }else if(sum(cond) > 1){ # in case there are multiple rows returned
    print(r)
    print(conservationunits_decoder[cond,])
    
  }else{
    cuid_here <- conservationunits_decoder$cuid[cond]
    cond <- nuseds$FULL_CU_IN_PSF == full_cu_in_psf_here
    nuseds$cuid[cond] <- cuid_here
  }
}

# Rows for which we could not find cuid: --> all are binned --> remove them
unique(nuseds[is.na(nuseds$cuid),c("species_abbr","FULL_CU_IN_PSF","CU_TYPE","CU_name")])
nuseds <- nuseds[!is.na(nuseds$cuid),]

#'* Bring the stream-related fields *

# Check:
streamlocationids_streamCUID <- unique(streamlocationids[,c("streamid","cuid","sys_nm")])
streamspawnersurveys_output_streamCUID <- unique(streamspawnersurveys_output[,c("streamid","cuid","region")])
m <- merge(x = streamlocationids_streamCUID, 
           y = streamspawnersurveys_output_streamCUID, 
           by = c("streamid","cuid"),
           all = T)
m[is.na(m$sys_nm) | is.na(m$region),]

unique(nuseds$cuid[! nuseds$cuid %in% streamspawnersurveys_output_streamCUID$cuid])
# 1202  291  292  419  753

unique(nuseds$cuid[! nuseds$cuid %in% streamlocationids$cuid])
# 1202  291  292  419  753  181

cond <- nuseds$cuid %in% c(1202,291,292,419,753)
unique(nuseds[cond,c("species_abbr","cuid","FULL_CU_IN_PSF","CU_TYPE","CU_name")])

cond <- conservationunits_decoder$cuid %in% c(1202,291,292,419,753)
conservationunits_decoder[cond,c("region","species_abbr","cuid","cu_name_pse",
                                 "cu_index","cu_type")]


cuids <- c(1202,291,292)
conservationunits_decoder[conservationunits_decoder$cuid %in% cuids,
                          c("region","species_abbr","cuid","cu_name_pse",
                            "cu_index","cu_type")]
streamlocationids[streamlocationids$cuid %in% cuids,c("streamid","cuid")]
streamlocationids[streamlocationids$cuid %in% cuids,c("streamid","cuid")]

# use streamspawnersurveys_output





