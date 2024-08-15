


#'******************************************************************************
#' The goal of the script is to 
#' 
#' Previous script: Fraser_salmon_CU_updates.Rmd
#' 
#' 
#' Files imported (from dropbox):
#' - streamlocationids.csv
#' - conservationunits_decoder.csv
#' - streamspawnersurveys_output.csv
#' - NuSEDS_escapement_data_collated_DATE.csv
#'
#' Files produced: 
#' - nuseds_cuid_streamid_DATE.csv
#' 

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
library(dplyr) # for arrange()

source("code/functions.R")

# Import files ------

#'* Import the most recent NuSEDS_escapement_data_collated file *
nuseds <- import_mostRecent_file_fun(wd = wd_output, 
                                     pattern = "NuSEDS_escapement_data_collated")

head(nuseds)
nrow(nuseds) # 306999

nrow(unique(nuseds[,c("SPECIES_QUALIFIED","POP_ID","SYSTEM_SITE","WATERBODY")])) # 6868
nrow(unique(nuseds[,c("SPECIES_QUALIFIED","POP_ID","SYSTEM_SITE","WATERBODY","GFE_ID")])) # 6868

sum(nuseds$MAX_ESTIMATE == 0 & !is.na(nuseds$MAX_ESTIMATE)) # 0
sum(is.na(nuseds$MAX_ESTIMATE))


#'* Files from PSF database *

#'Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- F

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

sum(is.na(conservationunits_decoder$cu_index))    # 45
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

#' Import 
# surveystreams <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[9],
#                                                      fromDatabase = fromDatabase,
#                                                      update_file_csv = update_file_csv,
#                                                      wd = wd_pop_indic_data_input_dropbox)

head(streamspawnersurveys_output)

# fields regions and region to nuseds_cuid_location using cuid
regionsid_df <- unique(merge(x = unique(streamspawnersurveys_output[,c("region","streamid")]),
                             y = unique(streamlocationids[,c("regionid","streamid")]),
                             by = "streamid")[,c("region","regionid")])
#                             region regionid
#                             Skeena        1
#                               Nass        2
#                      Central Coast        3
# Vancouver Island & Mainland Inlets        5
#                             Fraser        4
#                        Haida Gwaii        6
#                           Columbia        7
#                      Transboundary       10
#                              Yukon        8
rownames(regionsid_df) <- NULL

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
cond <- nuseds$GFE_ID == 968 & nuseds$SPECIES_QUALIFIED %in% c("SEL","SER")
unique(nuseds$CU_NAME[cond]) # "NORTHERN COASTAL FJORDS"
nuseds$CU_NAME[cond] <- toupper("South Atnarko Lakes")
#

#'* Create stream_survey_quality from ESTIMATE_CLASSIFICATION *
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
  #print(out)
  nuseds$stream_survey_quality[cond_nuseds] <- out
}


#'* Fixes in the methods *
# Katy's request:
# https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1712611492405689?thread_ts=1712252256.802999&cid=C03LB7KM6JK

unique(nuseds$ESTIMATE_METHOD)

nuseds$ESTIMATE_METHOD <- gsub("Cummulative","Cumulative",nuseds$ESTIMATE_METHOD)

nuseds$ESTIMATE_METHOD[nuseds$ESTIMATE_METHOD == "Unknown"] <-  "Unknown Estimate Method"

#' Additional changes requested (cf. Population meeting April 9th 2024)
nuseds$ESTIMATE_METHOD[nuseds$ESTIMATE_METHOD == "Fixed Site Census"] <-  "Fence Count"
nuseds$ESTIMATE_METHOD[nuseds$ESTIMATE_METHOD == "Aerial"] <- "Aerial Survey"
nuseds$ESTIMATE_METHOD[nuseds$ESTIMATE_METHOD == "Fence"] <- "Fence Count"
nuseds$ESTIMATE_METHOD[nuseds$ESTIMATE_METHOD == "Insufficient Information"] <- "Unknown Estimate Method"

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

fields_nuseds_CU <- c("CU_NAME","SPECIES_QUALIFIED","FULL_CU_IN",
                      "FULL_CU_IN_PSF","CU_TYPE")

CU_name_species <- unique(nuseds[,fields_nuseds_CU])
nrow(CU_name_species) # 414 415

conservationunits_decoder$cu_name_pse_modif <- tolower(conservationunits_decoder$cu_name_pse)
conservationunits_decoder$cu_name_dfo_modif <- tolower(conservationunits_decoder$cu_name_dfo)
CU_name_species$CU_NAME_modif <- tolower(CU_name_species$CU_NAME)

chara <- c("<<bin>>","<<extirpated>>","<<extinct>>","<<de novo>>","(de novo)")
for(c in chara){
  conservationunits_decoder$cu_name_pse_modif <- gsub(c,"",conservationunits_decoder$cu_name_pse_modif)
  conservationunits_decoder$cu_name_dfo_modif <- gsub(c,"",conservationunits_decoder$cu_name_dfo_modif)
  CU_name_species$CU_NAME_modif <- gsub(c,"",CU_name_species$CU_NAME_modif)
}
conservationunits_decoder$cu_name_pse_modif <- simplify_string_fun(conservationunits_decoder$cu_name_pse_modif)
conservationunits_decoder$cu_name_dfo_modif <- simplify_string_fun(conservationunits_decoder$cu_name_dfo_modif)
CU_name_species$CU_NAME_modif <- simplify_string_fun(CU_name_species$CU_NAME_modif)

nuseds$cuid <- NA
nuseds$cu_name_pse <- NA
nuseds$cu_name_dfo <- NA
nuseds$region <- NA
nuseds$regionid <- NA

count <- 1
message_show <- T
for(r in 1:nrow(CU_name_species)){
  # r <- 6
  CU_NAME_here <- tolower(CU_name_species$CU_NAME[r])
  SPECIES_QUALIFIED_here <- CU_name_species$SPECIES_QUALIFIED[r]         # "CM"  "CK"  "CO"  "PKE" "PKO" "SER" "SEL"
  CU_TYPE_here <- CU_name_species$CU_TYPE[r]
  #CU_NAME_here_modif <- CU_name_species$CU_name_modif[r]
  FULL_CU_IN_here <- CU_name_species$FULL_CU_IN[r]
  FULL_CU_IN_PSF_here <- CU_name_species$FULL_CU_IN_PSF[r]
  
  # try match with FULL_CU_IN_PSF
  cond <- conservationunits_decoder$cu_index == FULL_CU_IN_PSF_here &
    !is.na(conservationunits_decoder$cu_index)
  
  # try with FULL_CU_IN
  if(sum(cond) == 0){ 
    
    cond <- conservationunits_decoder$cu_index == FULL_CU_IN_here &
      !is.na(conservationunits_decoder$cu_index)
    
    # signal in case FULL_CU_IN matches but not FULL_CU_IN_PSF
    if(sum(cond) != 0){
      print(paste0(count,r," - FULL_CU_IN (",FULL_CU_IN_here,") used and not FULL_CU_IN_PSF (",FULL_CU_IN_PSF_here,") --> update the decoder?"))
      count <- count + 1
    }
  }
  
  # still no match --> try match with CU_NAME with cu_name_dfo and cu_name_pse
  if(sum(cond) == 0){ 
    
    CU_NAME_here_modif <- CU_name_species$CU_NAME_modif[r]
    
    cond <- (grepl(CU_NAME_here_modif,conservationunits_decoder$cu_name_dfo_modif) | 
               grepl(CU_NAME_here_modif,conservationunits_decoder$cu_name_pse_modif)) & 
      conservationunits_decoder$species_abbr == SPECIES_QUALIFIED_here
    
    if(sum(cond) > 1){
      print("more than one choice with CU_NAME_here_modif")
      break
    }
    
  }
  
  # still no match, do nothing for now
  if(sum(cond) == 0){
    
    if(message_show){
      message_show <- F
      print("*** NuSEDS CUs without a match in conservationunits_decoder: ***")
    }
    
    print(paste(count,r,SPECIES_QUALIFIED_here,CU_TYPE_here,FULL_CU_IN_PSF_here,CU_NAME_here,sep = " - "))
    count <- count + 1
    
  }else if(sum(cond) > 1){ # in case there are multiple rows returned --> should not happens, just flag
    print(r)
    print(conservationunits_decoder[cond,])
    
  }else{
    cuid_here <- conservationunits_decoder$cuid[cond]
    cu_name_pse_here <- conservationunits_decoder$cu_name_pse[cond]
    cu_name_dfo_here <- conservationunits_decoder$cu_name_dfo[cond]
    region_here <- conservationunits_decoder$region[cond]
    regionid_here <- conservationunits_decoder$regionid[cond]
    
    cond <- nuseds$FULL_CU_IN_PSF == FULL_CU_IN_PSF_here
    nuseds$cuid[cond] <- cuid_here
    nuseds$cu_name_pse[cond] <- cu_name_pse_here
    nuseds$cu_name_dfo[cond] <- cu_name_dfo_here
    nuseds$region[cond] <- region_here
    nuseds$regionid[cond] <- regionid_here
  }
}

# Rows for which we could not find cuid: --> all are binned --> remove them
cond_cuid_na <- is.na(nuseds$cuid)
cols <- c("region","regionid","SPECIES_QUALIFIED","FULL_CU_IN","FULL_CU_IN_PSF","CU_TYPE","CU_NAME","CU_LAT","CU_LONGT")
unique(nuseds[cond_cuid_na,cols])
nrow(unique(nuseds[cond_cuid_na,cols])) # 22

# fill the region for these 
cond <- grepl("FRASER",nuseds[cond_cuid_na,]$CU_NAME)
nuseds[cond_cuid_na,]$region[cond] <- "Fraser"
nuseds[cond_cuid_na,]$regionid[cond] <- regionsid_df$regionid[regionsid_df$region == "Fraser"]

# find the region of these CUs using coordinates and google map and the PSE
for(r in 1:nrow(unique(nuseds[cond_cuid_na,cols]))){
  # r <- 3
  if(is.na(unique(nuseds[cond_cuid_na,cols])$region[r])){
    print(unique(nuseds[cond_cuid_na,cols])$CU_NAME[r])
    print(paste(unique(nuseds[cond_cuid_na,cols])$CU_LAT[r],
                unique(nuseds[cond_cuid_na,cols])$CU_LONGT[r],sep = ", "))
    
    
  }
}

cond_VIMI <- nuseds[cond_cuid_na,]$CU_NAME %in% c("SOUTH-MISCELLANEOUS<<BIN>>",
                                                  "SOUTHERN BC-CROSS-CU SUPPLEMENTATION EXCLUSION<<BIN>>",
                                                  "GREAT CENTRAL/SPROAT<<BIN>>",
                                                  "OWOSSITSA",
                                                  "PACK",
                                                  "(N)GLENDALE",
                                                  "VILLAGE BAY")

cond_Fraser <- nuseds[cond_cuid_na,]$CU_NAME %in% c("CARIBOO-SUMMER TIMING",
                                                    "FRANCOIS-LATE TIMING",
                                                    "FRANCOIS-EARLY SUMMER TIMING",
                                                    "INDIAN/KRUGER-EARLY SUMMER TIMING")

cond_HG <- nuseds[cond_cuid_na,]$CU_NAME %in% c("(P)HATCHERY EXCLUSION-PALLANT CREEK")

cond_CC <- nuseds[cond_cuid_na,]$CU_NAME %in% c("WHALEN",
                                                "OWIKENO-LATE TIMING")

cond <- cond_VIMI
nuseds[cond_cuid_na,]$region[cond] <- "Vancouver Island & Mainland Inlets"
nuseds[cond_cuid_na,]$regionid[cond] <- regionsid_df$regionid[regionsid_df$region == unique(nuseds[cond_cuid_na,]$region[cond])]

cond <- cond_Fraser
nuseds[cond_cuid_na,]$region[cond] <- "Fraser"
nuseds[cond_cuid_na,]$regionid[cond] <- regionsid_df$regionid[regionsid_df$region == unique(nuseds[cond_cuid_na,]$region[cond])]

cond <- cond_HG
nuseds[cond_cuid_na,]$region[cond] <- "Haida Gwaii"
nuseds[cond_cuid_na,]$regionid[cond] <- regionsid_df$regionid[regionsid_df$region == unique(nuseds[cond_cuid_na,]$region[cond])]

cond <- cond_CC
nuseds[cond_cuid_na,]$region[cond] <- "Central Coast"
nuseds[cond_cuid_na,]$regionid[cond] <- regionsid_df$regionid[regionsid_df$region == unique(nuseds[cond_cuid_na,]$region[cond])]

unique(nuseds[cond_cuid_na,cols])


#' TODO: troubleshoot with Katy after PSE 2.0 --> add these CUs
# nuseds <- nuseds[!is.na(nuseds$cuid),]
nrow(nuseds) # 306823 306875 306999

#
# Match locations & attribute pointid -------

#'* Match location *
#' Try to match locations in nuseds with locations in streamlocationids:

fields_nuseds_location <- c("SYSTEM_SITE","WATERBODY","GAZETTED_NAME",  # regionid is a CU-related field, not a location
                            "LOCAL_NAME_1","LOCAL_NAME_2",
                            "GFE_ID","X_LONGT","Y_LAT")

nuseds_location <- unique(nuseds[,fields_nuseds_location]) # inclide GFE_ID because there can be multiple SYSTEM_SITEs for a same CU
nrow(nuseds_location) # 2312

# Manual fix 1st:
#'-  "BARRI\xc8RE RIVER" --> "BARRIERE RIVER"
#'- "FRAN\xc7OIS LAKE" --> ""FRANCOIS LAKE"
#'
cond <- nuseds_location$SYSTEM_SITE == "BARRIERE RIVER"
nuseds_location$LOCAL_NAME_1[cond] <- "BARRIERE RIVER"
cond <- nuseds_location$SYSTEM_SITE == "EAST BARRIERE RIVER"
nuseds_location$LOCAL_NAME_1[cond] <- "EAST BARRIERE RIVER"
cond <- nuseds_location$SYSTEM_SITE == "FRANCOIS LAKE"
nuseds_location$LOCAL_NAME_1[cond] <- "FRANCOIS LAKE"

nuseds_location$field_nuseds_used <- NA
nuseds_location$sys_nm <- NA            # to record which sys_nm was used
nuseds_location$distance <- NA # the euclidean distance between the SYSTEM_SITE and sys_nm
nuseds_location$pointid <- NA
nuseds_location$latitude <- NA
nuseds_location$longitude <- NA
nuseds_location$pointid_alternative <- NA
nuseds_location$comment <- NA

streamlocationids$sys_nm_simple <- simplify_string_fun(streamlocationids$sys_nm)
# change "creeks" for "creek" but not directly from sys_nm_simple because there are cases such as "creek shore" --> "creekshore" --> "creekhore"
cond_creeks <- grepl("creeks",tolower(streamlocationids$sys_nm)) # if "creeks" is present
streamlocationids$sys_nm_simple[cond_creeks] <- gsub("creeks","creek",streamlocationids$sys_nm_simple[cond_creeks])
streamlocationids$taken <- NA

# simplify the location name fields:
fields_locations <- c("SYSTEM_SITE","WATERBODY","GAZETTED_NAME",
                      "LOCAL_NAME_1","LOCAL_NAME_2")

# Simplify the fields_locations
# and remove the "s" "creeks"
for(f in fields_locations){
  # f <- fields_locations[1]
  nuseds_location$X <- simplify_string_fun(nuseds_location[,f])
  cond_creeks <- grepl("creeks",tolower(nuseds_location[,f]))
  nuseds_location$X[cond_creeks] <- gsub("creeks","creek",nuseds_location$X[cond_creeks])
  colnames(nuseds_location)[colnames(nuseds_location) == "X"] <- paste0(f,"_simple")
}

#'* 1) 1st fill all the ones with perfect match for location names and/or coordinates *
#' Do not try to trouble shoot the other ones yet because removing the perfect 
#' match 1st remove options when multiple matches are available when using grepl
#' (which is used for trouble shooting).
# add the coordinate field
fields_locations <- c(fields_locations,"COORDINATES")

count <- 0
count_show <- 1

# streamlocationids_copy <- streamlocationids
# streamlocationids <- streamlocationids_copy

distance_threshold <-  0.1 # ~ 10km # 0.05 # ~ 5km # 0.5 # ~ 50km

# options(warn = 0)
# options(warn=1)  # print warnings as they occur
# options(warn = 2)  # treat warnings as errors

decimals <- 6

#' Make exception for location just above the threshold of 0.1 but that seem to
#' to be the correct location after visual inspection on google map.
#' Note that certain location have multiple distances, meaning that they are
#' several location with the same sys_nm but different pointid and coordinates 
#' in the PSE --> to correct later?
exception_threshold <- c("VETTER CREEK",  # distances 0.1043280 0.1043055
                         "NAKINA RIVER",  # distances 0.2548975 0.1034031
                         "COLE CREEK")    # distances 0.1130038

# manual fixes
location_names_fixes <- SYSTEM_SITE_fixes_fun()
SYSTEM_SITE_fixes <- location_names_fixes$SYSTEM_SITE
sys_nm_fixes <- location_names_fixes$sys_nm
rm(location_names_fixes)

nuseds_location$SYSTEM_SITE_corrected <- NA

for(r in 1:nrow(nuseds_location)){
  # r <- 1 
  # r <- which(nuseds_location$SYSTEM_SITE == "FULTON RIVER - ABOVE WEIR")
  
  # if(nuseds_location$SYSTEM_SITE[r] == "FULTON RIVER - ABOVE WEIR"){
  #   break
  # }
  
  # apply know correction if relevant
  if(nuseds_location$SYSTEM_SITE[r] %in% SYSTEM_SITE_fixes){
    cond <- SYSTEM_SITE_fixes == nuseds_location$SYSTEM_SITE[r]
    nuseds_location$SYSTEM_SITE_corrected[r] <- sys_nm_fixes[cond]
  }
  
  fileds_values_l <- list()
  v_i <- 1
  for(v in fields_locations){
    if(v == "COORDINATES"){
      X_LONGT <- round(nuseds_location$X_LONGT[r],decimals)
      Y_LAT <- round(nuseds_location$Y_LAT[r],decimals)
      fileds_values_l[[v_i]] <- list(X_LONGT,Y_LAT)
      names(fileds_values_l[[v_i]]) <- c("X_LONGT","Y_LAT")
    }else{
      # fileds_values_l[[v_i]] <- nuseds_location[r,v]
      if(v == "SYSTEM_SITE" & !is.na(nuseds_location$SYSTEM_SITE_corrected[r])){
        fileds_values_l[[v_i]] <- nuseds_location[r,"SYSTEM_SITE_corrected"]
      }else{
        fileds_values_l[[v_i]] <- nuseds_location[r,v]
      }
    }
    v_i <- v_i + 1
  }
  names(fileds_values_l) <- fields_locations
  
  # Exceptions for certain names
  if(fileds_values_l$SYSTEM_SITE == "BLUE LEAD CREEK - SHORE"){     # otherwise matched to QUESNEL LAKE EAST ARM-UNNAMED CREEK #1
    fileds_values_l$SYSTEM_SITE <- "BLUE LEAD CREEK-LAKE SHORE"
  }else if(fileds_values_l$SYSTEM_SITE == "KILLDOG CREEK - SHORE"){ # otherwise matched to QUESNEL LAKE EAST ARM-UNNAMED CREEK #1
    fileds_values_l$SYSTEM_SITE <- "KILLDOG CREEK-LAKE SHORE"
  }else if(fileds_values_l$SYSTEM_SITE %in% c("BLUE LEAD CREEK - SHORE",       # to prevent matching to "FULTON RIVER"
                                              "FRASER RIVER AND TRIBUTARIES",  # to prevent matching to "FRASER RIVER"
                                              "SOUTH BOISE CREEK")){           # to prevent matching to "SOUTH BOISE CREEK"
    fileds_values_l$GAZETTED_NAME <- fileds_values_l$SYSTEM_SITE  
  }
  
  # return the matches in streamlocationids
  conditions_l <- list()
  v_i <- 1
  for(v in fields_locations){
    if(v == "COORDINATES"){
      conditions_l[[v_i]] <- round(streamlocationids$longitude,decimals) == fileds_values_l[[v]]$X_LONGT &
        round(streamlocationids$latitude,decimals) == fileds_values_l[[v]]$Y_LAT
    }else{
      val_here <- simplify_string_fun(fileds_values_l[[v]])
      cond_creeks <- grepl("creeks",tolower(fileds_values_l[[v]]))
      val_here[cond_creeks] <- gsub("creeks","creek",val_here[cond_creeks])
      conditions_l[[v_i]] <- streamlocationids$sys_nm_simple == val_here
    }
    v_i <- v_i + 1
  }
  names(conditions_l) <- fields_locations
  
  # Exceptions: increase the threshold a little bit
  if(fileds_values_l$SYSTEM_SITE %in% exception_threshold){
    print(paste0("Exception for: ",fileds_values_l$SYSTEM_SITE))
    distance_threshold_to_use <- distance_threshold + 0.02
    # break
    
  }else{
    distance_threshold_to_use <- distance_threshold
  }
  
  # return the distance where matches were found
  dist_pointids_l <- distance_condition_fun(conditions_l = conditions_l,
                                            streamlocationids = streamlocationids,
                                            distance_threshold = distance_threshold_to_use,
                                            decimals = decimals)
  distances_l <- dist_pointids_l$distances_l
  conditions_l <- dist_pointids_l$conditions_l
  pointid_alternative <- dist_pointids_l$pointid_alternative
  rm(dist_pointids_l)

  # record the potential alternative pointids
  # note that it is possible that pointid_alternative is the same as pointid because
  # pointid is not not perfect (locations with slightly different coordinates can
  # have a same pointid)
  if(!is.null(pointid_alternative)){
    nuseds_location$pointid_alternative[r] <- paste0(pointid_alternative,collapse = ", ")
  }
  
  # if there are matches with spatial fields:
  if(any(!is.na(unlist(distances_l)))){
    
    # retain the field(s) with the smallest distance
    fields_used <- unlist(distances_l) == min(unlist(distances_l), na.rm = T)
    fields_used[is.na(fields_used)] <- F
    
    # indicate which nuseds field(s) was used
    nuseds_location$field_nuseds_used[r] <- paste(fields_locations[fields_used], 
                                                  collapse = " ")
    
    # select condition
    cond_toUse <- conditions_l[fields_used][[1]]
    
    # return corresponding distance
    distance <- distances_l[fields_used][[1]]
    
    # check if the distance is < distance_threshold
    # if not then do nothing for now
    if(distance > distance_threshold_to_use){
      print("Distance > distance_threshold and that should not be happening")
      break
    }
    
    # get the pointid and sys_nm
    pointid_here <- unique(streamlocationids$pointid[cond_toUse])
    sys_nm_here <- unique(streamlocationids$sys_nm[cond_toUse])
    
    # troubleshoot in case there are multiple pointid_here
    # if yes then this means they have the same distance. Take the one with 
    # the highest number of occurences and place the other one(s) in pointid_alternative
    if(length(pointid_here) > 1){
      
      # select the pointid the most prevalent
      pointid_nb <- table(streamlocationids$pointid[cond_toUse])
      pointid_here <- as.numeric(names(pointid_nb)[pointid_nb == max(pointid_nb)])
      pointid_alter <- as.numeric(names(pointid_nb)[names(pointid_nb) != pointid_here])
      sys_nm_here <- sys_nm_here[pointid_nb == max(pointid_nb)]
      nuseds_location$pointid_alternative[r] <- paste0(pointid_alternative,collapse = ", ")
      
      # check
      if(pointid_here != 1218){
        print("location 2")
        break
      }
      # cond_toUse <- cond_toUse & streamlocationids$regionid == nuseds_location$regionid[r]
    }
    
    nuseds_location$pointid[r] <- pointid_here
    
    nuseds_location$longitude[r] <- unique(round(streamlocationids$longitude[cond_toUse],decimals))
    nuseds_location$latitude[r] <- unique(round(streamlocationids$latitude[cond_toUse],decimals))
    nuseds_location$sys_nm[r] <- sys_nm_here
    nuseds_location$distance[r] <- distance
    

  }else{ # if no match, do nothing for now
    count <- count + 1
    count_percent <- round(count/nrow(nuseds_location)*100,1)
    if(count_show <= count_percent){
      print(paste("Proportion of locations not matching:",count_percent,"%"))
      count_show <- count_show + 1
    }
  }
}
print(paste("Proportion of locations not matching:",count_percent,"%")) # 4.6%

sum(is.na(nuseds_location$pointid)) # 106
sum(is.na(nuseds_location$pointid)) / nrow(nuseds_location) * 100 #  4.6
sum(!is.na(nuseds_location$pointid)) / nrow(nuseds_location) * 100 # 95.4


#'* 2) use grepl for the matching of the remaining data *
#' use spatial coordinate only to find the closest location when multiple options.
#' Check that the pointid has not been already taken in the section above.

# nuseds_location$SYSTEM_SITE_corrected <- NA

count <- 0
count_show <- 1
for(r in 1:nrow(nuseds_location)){
  # r <- 1
  # r <- which(nuseds_location$SYSTEM_SITE == "GOOSE POINT - SHORE .8KM S")
  
  if(is.na(nuseds_location$pointid[r])){
    
    # apply know correction if relevant
    if(nuseds_location$SYSTEM_SITE[r] %in% SYSTEM_SITE_fixes){
      cond <- SYSTEM_SITE_fixes == nuseds_location$SYSTEM_SITE[r]
      nuseds_location$SYSTEM_SITE_corrected[r] <- sys_nm_fixes[cond]
    }
    
    fileds_values_l <- list()
    v_i <- 1
    for(v in fields_locations){
      if(v == "COORDINATES"){
        X_LONGT <- round(nuseds_location$X_LONGT[r],decimals)
        Y_LAT <- round(nuseds_location$Y_LAT[r],decimals)
        fileds_values_l[[v_i]] <- list(X_LONGT,Y_LAT)
        names(fileds_values_l[[v_i]]) <- c("X_LONGT","Y_LAT")
      }else{
        if(v == "SYSTEM_SITE" & !is.na(nuseds_location$SYSTEM_SITE_corrected[r])){
          fileds_values_l[[v_i]] <- nuseds_location[r,"SYSTEM_SITE_corrected"]
        }else{
          fileds_values_l[[v_i]] <- nuseds_location[r,v]
        }
      }
      v_i <- v_i + 1
    }
    names(fileds_values_l) <- fields_locations
    
    # exception
    if(fileds_values_l$SYSTEM_SITE %in% c("REINECKER CREEK - SHORE",
                                          "HLINA CREEK - SHORE",
                                          "ONYX CREEK - SHORE",
                                          "BLUE RIVER - UPPER")){
      #' to prevent from matching to 
      #' "REINECKER CREEK - SHORE": "SHUSWAP LAKE-SALMON ARM-S-LAKE SHORE"
      #' "HLINA CREEK - SHORE": "SHUSWAP LAKE-MAIN ARM-LAKE SHORE"
      #' "ONYX CREEK - SHORE": "SHUSWAP LAKE-MAIN ARM-LAKE SHORE"
      #' "BLUE RIVER - UPPER": "BLUE RIVER"
      fileds_values_l$GAZETTED_NAME <- fileds_values_l$SYSTEM_SITE
      
    }
    
    # return the matches in streamlocationids
    conditions_l <- list()
    v_i <- 1
    for(v in fields_locations){
      if(v == "COORDINATES"){ # leave it but there is not match
        conditions_l[[v_i]] <- round(streamlocationids$longitude,4) == fileds_values_l[[v]]$X_LONGT &
          round(streamlocationids$latitude,4) == fileds_values_l[[v]]$Y_LAT
      }else{
        val_here <- simplify_string_fun(fileds_values_l[[v]])
        cond_creeks <- grepl("creeks",tolower(fileds_values_l[[v]]))
        val_here[cond_creeks] <- gsub("creeks","creek",val_here[cond_creeks])
        if(val_here == ""){
          conditions_l[[v_i]] <- rep(F,nrow(streamlocationids))
        }else{
          conditions_l[[v_i]] <- grepl(val_here,
                                       streamlocationids$sys_nm_simple)
        }
      }
      v_i <- v_i + 1
    }
    names(conditions_l) <- fields_locations
    
    # return the distance where matches were found
    dist_pointids_l <- distance_condition_fun(conditions_l = conditions_l,
                                              streamlocationids = streamlocationids,
                                              distance_threshold = distance_threshold,
                                              decimals = decimals)
    distances_l <- dist_pointids_l$distances_l
    conditions_l <- dist_pointids_l$conditions_l
    pointid_alternative <- dist_pointids_l$pointid_alternative
    rm(dist_pointids_l)
    
    # record the potential alternative pointids
    # note that it is possible that pointid_alternative is the same as pointid because
    # pointid is not not perfect (locations with slightly different coordinates can
    # have a same pointid)
    if(!is.null(pointid_alternative)){
      nuseds_location$pointid_alternative[r] <- paste0(pointid_alternative,collapse = ", ")
    }
    
    # If still no match, try other fixes with SYSTEM_SITE_fixes_fun()
    if(all(is.na(unlist(distances_l)))){
      
      SYSTEM_SITE_Extrafixes_l <- SYSTEM_SITE_Extrafixes_fun(SYSTEM_SITE = fileds_values_l$SYSTEM_SITE,
                                                             streamlocationids = streamlocationids)
      
      if(!is.na(SYSTEM_SITE_Extrafixes_l)[1]){

        conditions_l$SYSTEM_SITE <- SYSTEM_SITE_Extrafixes_l$cond_SYSTEM_SITE_simple_fixed
        
        # return the distance where matches were found
        dist_pointids_l <- distance_condition_fun(conditions_l = conditions_l,
                                                  streamlocationids = streamlocationids,
                                                  distance_threshold = distance_threshold,
                                                  decimals = decimals)
        distances_l <- dist_pointids_l$distances_l
        conditions_l <- dist_pointids_l$conditions_l
        pointid_alternative <- dist_pointids_l$pointid_alternative
        rm(dist_pointids_l)
        
        # record the potential alternative pointids
        # note that it is possible that pointid_alternative is the same as pointid because
        # pointid is not not perfect (locations with slightly different coordinates can
        # have a same pointid)
        if(!is.null(pointid_alternative)){
          nuseds_location$pointid_alternative[r] <- paste0(pointid_alternative,collapse = ", ")
        }
      }
    }
    
    # if there is a match
    if(any(!is.na(unlist(distances_l)))){
      
      # retain the field(s) with the smallest distance
      fields_used <- unlist(distances_l) == min(unlist(distances_l), na.rm = T)
      fields_used[is.na(fields_used)] <- F
      
      # indicate which nuseds field(s) was used
      nuseds_location$field_nuseds_used[r] <- paste(fields_locations[fields_used], 
                                                    collapse = " ")
      
      # select condition
      cond_toUse <- conditions_l[fields_used][[1]]
      
      # return corresponding distance
      distance <- distances_l[fields_used][[1]]
      
      pointid_here <- unique(streamlocationids$pointid[cond_toUse])
      
      # troubleshoot
      if(length(pointid_here) > 1){
        print("break at location 1")
        break
        #cond_toUse <- cond_toUse & streamlocationids$regionid == nuseds_location$regionid[r]
      }
      
      nuseds_location$longitude[r] <- unique(round(streamlocationids$longitude[cond_toUse],decimals))
      nuseds_location$latitude[r] <- unique(round(streamlocationids$latitude[cond_toUse],decimals))
      nuseds_location$pointid[r] <- pointid_here
      nuseds_location$sys_nm[r] <- unique(streamlocationids$sys_nm[cond_toUse])
      nuseds_location$distance[r] <- distance
      
    }else{ # if there is still no match
      
      count <- count + 1
      count_percent <- round(count/nrow(nuseds_location)*100,1)
      if(count_show <= count_percent){
        print(paste("Proportion of locations not matching:",count_percent,"%"))
        count_show <- count_show + 1
      }
    }
  }
}
print(paste("Proportion of locations not matching:",count_percent,"%")) # 1.6%
# View(nuseds_location)

cond_noMatch <- is.na(nuseds_location$pointid)
sum(cond_noMatch) # 38
nuseds_location[cond_noMatch,c("SYSTEM_SITE","GFE_ID","X_LONGT","Y_LAT","comment")]

nuseds_location$distance[!cond_noMatch][nuseds_location$distance[!cond_noMatch] > 0.01]

# little fix
cond <- nuseds_location$pointid_alternative == "" & !is.na(nuseds_location$pointid_alternative)
nuseds_location[cond,]$pointid_alternative <- NA

# Locations that did not match
cond <- is.na(nuseds_location$pointid)
nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","GFE_ID","X_LONGT","Y_LAT")]

# locations that matched
cond <- !is.na(nuseds_location$pointid)
matches <- nuseds_location[cond,]
matches[,c("SYSTEM_SITE","WATERBODY","GFE_ID","X_LONGT","Y_LAT","longitude","latitude","sys_nm","distance","pointid_alternative")]

# Matched on distance but with different SYSTEM_SITE 
cond <- matches$distance == 0
matches_dist <- matches[cond,]
matches_dist_nameNo <- matches_dist[matches_dist$SYSTEM_SITE_simple != simplify_string_fun(matches_dist$sys_nm),]
matches_dist_nameNo[,c("SYSTEM_SITE","WATERBODY","GFE_ID","sys_nm","distance","pointid_alternative")]

# Matched not on distance but with same SYSTEM_SITE 
cond <- matches$distance != 0
matches_distNo_name <- matches[cond,]
matches_distNo_name[,c("SYSTEM_SITE","WATERBODY","GFE_ID","sys_nm","distance","pointid_alternative")]

#
#' * Fill nuseds *

nuseds$pointid <- NA
nuseds$sys_nm <- NA
#nuseds$streamid <- NA
nuseds$latitude <- NA
nuseds$longitude <- NA
nuseds$distance <- NA

for(r in 1:nrow(nuseds_location)){
  # r <- 1
  pointid <- nuseds_location$pointid[r]
  sys_nm <- nuseds_location$sys_nm[r]
  latitude <- nuseds_location$latitude[r]
  longitude <- nuseds_location$longitude[r]
  distance <- nuseds_location$distance[r]
  GFE_ID <- nuseds_location$GFE_ID[r]
  
  #
  cond <- nuseds$GFE_ID == GFE_ID
  nuseds$pointid[cond] <- pointid
  nuseds$sys_nm[cond] <- sys_nm
  nuseds$latitude[cond] <- latitude
  nuseds$longitude[cond] <- longitude
  nuseds$distance[cond] <- distance
}

# Add field location_new
nuseds$location_new <- F
nuseds$location_new[is.na(nuseds$sys_nm)] <- T

sum(nuseds$location_new)/nrow(nuseds) * 100   #  0.89
sum(!nuseds$location_new)/nrow(nuseds) * 100  # 99.11

#
# Attribute streamid and define coordinates and sys_nm ------
#' It was decided to not try to match the existing streamid but instead to redefine
#' them.

#'* streamid *

nuseds$streamid <- NA

cuid_GFE_ID <- unique(nuseds[!is.na(nuseds$cuid),][,c("cuid","GFE_ID")])
streamid <- 1
for(r in 1:nrow(cuid_GFE_ID)){
  # r <- 1
  cuid <- cuid_GFE_ID$cuid[r]
  GFE_ID <- cuid_GFE_ID$GFE_ID[r]
  
  cond <- nuseds$cuid == cuid & nuseds$GFE_ID == GFE_ID
  nuseds$streamid[cond] <- streamid
  streamid <- streamid + 1
}

# only the row without a cuid should also not have a streamid:
sum(is.na(nuseds$cuid))     # 1998
sum(is.na(nuseds$streamid)) # 1998


#'* coordinates & sys_nm*

#' Add the final coordinate fields
#' - use the PSE coordinates for the matched locations if there are not duplicated
#' with other locations.
#' - otherwise, use the nuseds coordinates

# Find GFE_IDs with duplicated coordinate 
# Check duplicated coordinates
nuseds$longitude <- round(nuseds$longitude,6)
nuseds$latitude <- round(nuseds$latitude,6)

locations <- unique(nuseds[!is.na(nuseds$latitude),c("GFE_ID","longitude","latitude")])
nrow(locations) # 2274
coord_duplicated <- locations[,c("longitude","latitude")][duplicated(locations[,c("longitude","latitude")]),]
nrow(coord_duplicated) # 37

GFE_ID_duplicated <- c()
for(r in 1:nrow(coord_duplicated)){
  cond <- locations$longitude == coord_duplicated$longitude[r] &
    locations$latitude == coord_duplicated$latitude[r]
  GFE_ID_duplicated <- c(GFE_ID_duplicated,locations$GFE_ID[cond])
}

length(GFE_ID_duplicated) # 82
GFE_ID_duplicated <- unique(GFE_ID_duplicated)
length(GFE_ID_duplicated) # 71

cond <- nuseds$GFE_ID %in% GFE_ID_duplicated
unique(nuseds[cond,c("GFE_ID","SYSTEM_SITE","sys_nm","Y_LAT","X_LONGT","latitude","longitude")][order(nuseds$latitude[cond]),])

nuseds$latitude_final <- nuseds$longitude_final <- NA
nuseds$sys_nm_final <- NA

# GFE_IDs for the PSE coordinates:
cond <- !is.na(nuseds$latitude) & !nuseds$GFE_ID %in% GFE_ID_duplicated
nuseds$latitude_final[cond] <- nuseds$latitude[cond]
nuseds$longitude_final[cond] <- nuseds$longitude[cond]
nuseds$sys_nm_final[cond] <- nuseds$sys_nm[cond]

# GFE_IDs for the nuseds coordinates:
cond <- is.na(nuseds$latitude) | nuseds$GFE_ID %in% GFE_ID_duplicated
nuseds$latitude_final[cond] <- nuseds$Y_LAT[cond]
nuseds$longitude_final[cond] <- nuseds$X_LONGT[cond]
nuseds$sys_nm_final[cond] <- nuseds$SYSTEM_SITE[cond]
nuseds$pointid[cond] <- NA

# Find GFE_IDs with duplicated coordinate again
locations <- unique(nuseds[!is.na(nuseds$latitude),c("GFE_ID","longitude_final","latitude_final")])
nrow(locations) # 2274
coord_duplicated <- locations[,c("longitude_final","latitude_final")][duplicated(locations[,c("longitude_final","latitude_final")]),]
nrow(coord_duplicated) # 0

# How many point
(length(unique(nuseds$pointid)) - 1)/length(unique(nuseds$sys_nm_final)) * 100

#
# Add the survey_score field -------
#' cf. Table 4.5 in section 4.1.3 of the Tech Report
nuseds <- import_mostRecent_file_fun(wd = wd_output,pattern = "nuseds_cuid_streamid_")

estim_class_nuseds <- unique(nuseds$ESTIMATE_CLASSIFICATION)
estim_class_nuseds

nuseds$survey_score <- NA
for(ecn in estim_class_nuseds){
  # ecn <- estim_class_nuseds[1]
  cond_nuseds <- nuseds$ESTIMATE_CLASSIFICATION == ecn
  
  if(ecn == "TRUE ABUNDANCE (TYPE-1)"){
    out <- "1"
  }else if(ecn == "TRUE ABUNDANCE (TYPE-2)"){
    out <- "2"
  }else if(ecn == "RELATIVE ABUNDANCE (TYPE-3)"){
    out <- "3"
  }else if(ecn == "RELATIVE ABUNDANCE (TYPE-4)"){
    out <- "4"
  }else if(ecn %in% c("RELATIVE ABUNDANCE (TYPE-5)",
                      "RELATIVE: CONSTANT MULTI-YEAR METHODS")){
    out <- "5"
  }else if(ecn %in% c("PRESENCE/ABSENCE (TYPE-6)",
                      "PRESENCE-ABSENCE (TYPE-6)",
                      "RELATIVE: VARYING MULTI-YEAR METHODS")){
    out <- "6"
  }else if(ecn == "UNKNOWN"){
    out <- "Unknown"
  }else if(ecn %in% c("","NO SURVEY THIS YEAR","NO SURVEY")){
    out <- NA
  }else{
    print(ecn)
  }
  #print(out)
  nuseds$survey_score[cond_nuseds] <- out
}

nuseds$survey_score |> unique()

#
# Export nuseds_cuid_streamid_DATE.csv -------- 
#
date <- as.character(Sys.time())
date <- strsplit(x = date, split = " ")[[1]][1]
date <- gsub("-","",date)
write.csv(nuseds,paste0(wd_output,"/nuseds_cuid_streamid_",date,".csv"),
          row.names = F)

#





#
# FUTURE THINGS TO DO (still relevant ?) ------

#'* Rename fields *
# Older file for comparison:
# wd_here <- paste0(wd_X_Drive1_PROJECTS,"/1_Active/Fraser_VIMI/analysis/Compilation/Results")
# nusedsPrevious <- read.csv(paste0(wd_here,"/NuSEDS_escapement_data_collated_20230818.csv"),header = T)

field_toChange <- c("SYSTEM_SITE",
                    "IS_INDICATOR",
                    "SPECIES_QUALIFIED",
                    "Y_LAT","X_LONGT",
                    "AREA",
                    "MAZ_ACRO","FAZ_ACRO","JAZ_ACRO",
                    "CU_NAME"
                    #"SPECIES"
)

fields_new <- c("SYS_NM",
                "IsIndicator",
                "species_abbr",
                "yLAT","xLONG",
                "Area",
                "maz_acro","faz_acro","jaz_acro",
                "CU_name"
                #"species_abbr"
)

for(i in 1:length(field_toChange)){
  names(nuseds_final)[names(nuseds_final) == field_toChange[i]] <- fields_new[i]
}

#'* rename ESTIMATE_METHOD to stream_survey_method *
colnames(nuseds)[colnames(nuseds) == "ESTIMATE_METHOD"] <- "stream_survey_method"

#'* rename MAX_ESTIMATE to stream_observed_count *
colnames(nuseds)[colnames(nuseds) == "MAX_ESTIMATE"] <- "stream_observed_count"

