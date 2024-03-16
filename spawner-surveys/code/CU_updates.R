


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

nrow(unique(nuseds[,c("SPECIES_QUALIFIED","POP_ID","SYSTEM_SITE","WATERBODY")])) # 6910
nrow(unique(nuseds[,c("SPECIES_QUALIFIED","POP_ID","SYSTEM_SITE","WATERBODY","GFE_ID")])) # 6910

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

#' Import 
surveystreams <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[9],
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
cond <- nuseds$GFE_ID == 968 & nuseds$SPECIES_QUALIFIED %in% c("SEL","SER")
unique(nuseds$CU_NAME[cond]) # "NORTHERN COASTAL FJORDS"
nuseds$CU_NAME[cond] <- toupper("South Atnarko Lakes")
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
  #print(out)
  nuseds$stream_survey_quality[cond_nuseds] <- out
}

unique(nuseds$ESTIMATE_METHOD)

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

CU_name_species <- unique(nuseds[,c("CU_NAME","SPECIES_QUALIFIED","FULL_CU_IN",
                                    "FULL_CU_IN_PSF","CU_TYPE")])
nrow(CU_name_species) # 415

conservationunits_decoder$cu_name_pse_modif <- tolower(conservationunits_decoder$cu_name_pse)
conservationunits_decoder$cu_name_dfo_modif <- tolower(conservationunits_decoder$cu_name_dfo)
CU_name_species$CU_NAME_modif <- tolower(CU_name_species$CU_NAME)

chara <- c(" ","-","<<BIN>>","<<EXTIRPATED>>")
for(c in chara){
  conservationunits_decoder$cu_name_pse_modif <- gsub(c,"",conservationunits_decoder$cu_name_pse_modif)
  conservationunits_decoder$cu_name_dfo_modif <- gsub(c,"",conservationunits_decoder$cu_name_dfo_modif)
  CU_name_species$CU_NAME_modif <- gsub(c,"",CU_name_species$CU_NAME_modif)
}

nuseds$cuid <- NA
nuseds$region <- NA
nuseds$regionid <- NA

count <- 1
message_show <- T
for(r in 1:nrow(CU_name_species)){
  # r <- 6
  cu_name_here <- tolower(CU_name_species$CU_NAME[r])
  species_here <- CU_name_species$SPECIES_QUALIFIED[r]         # "CM"  "CK"  "CO"  "PKE" "PKO" "SER" "SEL"
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
  
  # still no match --> try match with CU_NAME
  if(sum(cond) == 0){ 
    
    cu_name_here_modif <- CU_name_species$CU_NAME_modif[r]
    
    cond <- (grepl(cu_name_here_modif,conservationunits_decoder$cu_name_dfo_modif) | 
               grepl(cu_name_here_modif,conservationunits_decoder$cu_name_pse_modif)) & 
      conservationunits_decoder$species_abbr == species_here
    
  }
  
  # still no match, no nothing for now
  if(sum(cond) == 0){
    
    if(message_show){
      message_show <- F
      print("*** NuSEDS CUs without a match in conservationunits_decoder: ***")
    }
    
    print(paste(count,r,species_here,cu_type_here,full_cu_in_psf_here,cu_name_here,sep = " - "))
    count <- count + 1
    
  }else if(sum(cond) > 1){ # in case there are multiple rows returned --> should not happens, just flag
    print(r)
    print(conservationunits_decoder[cond,])
    
  }else{
    cuid_here <- conservationunits_decoder$cuid[cond]
    region_here <- conservationunits_decoder$region[cond]
    regionid_here <- conservationunits_decoder$regionid[cond]
    
    cond <- nuseds$FULL_CU_IN_PSF == full_cu_in_psf_here
    nuseds$cuid[cond] <- cuid_here
    nuseds$region[cond] <- region_here
    nuseds$regionid[cond] <- regionid_here
  }
}

# Rows for which we could not find cuid: --> all are binned --> remove them
unique(nuseds[is.na(nuseds$cuid),c("SPECIES_QUALIFIED","FULL_CU_IN_PSF","CU_TYPE","CU_NAME")])
nuseds <- nuseds[!is.na(nuseds$cuid),]
nrow(nuseds) # 305204

#
#'* Bring the stream-related fields *

#'** CHECKS TEMPORARY **
#' 1) compare cuid between streamlocationids, streamspawnersurveys_output and 
#' surveystreams --> CU in Skeena Lake sockeye  181 Bulkley/Maxan is only in 
#' streamspawnersurveys_output
#' https://salmonwatersheds.slack.com/archives/CKNVB4MCG/p1710353792636679
#' 

#' 2) check if any cuid in nuseds are not present in streamspawnersurveys_output
#' --> CUs 1207  419  753 are missing (note that other CUs were missing and 
#' issues were fixed butI left the history of the message below for a record).
#' - Katy: 755 and 936 were removed based on this PSAC feedback
#' - Eric: 531 was removed based on Technical Working Group feedback (can't be 
#' sockeye there because there is an impassable falls)
#' - 291 and 292: FIXED (added)
#' - Katy: "streamid 10201 NORDENSKIOLD RIVER was attributed to CUID 1207 in the 
#' database. I've changed it to CUID 1202"
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1682606819442789
#' https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1709857183818209?thread_ts=1708454531.181729&cid=C03LB7KM6JK

#' 1)
streamlocationids$cuid[! streamlocationids$cuid %in% streamspawnersurveys_output$cuid] # 0
streamspawnersurveys_output$cuid[! streamspawnersurveys_output$cuid %in% streamlocationids$cuid] # 181
streamspawnersurveys_output$cuid[! streamspawnersurveys_output$cuid %in% surveystreams$cuid] # 0
surveystreams$cuid[! surveystreams$cuid %in% streamspawnersurveys_output$cuid] # 0

streamspawnersurveys_output[streamspawnersurveys_output$cuid == 181,]
surveystreams[surveystreams$cuid == 181,]
streamlocationids[streamlocationids$cuid == 181,]
conservationunits_decoder[conservationunits_decoder$cuid == 181,]

#' 2)
CUs_missing <- unique(nuseds$cuid[! nuseds$cuid %in% streamspawnersurveys_output$cuid])
CUs_missing
# 1207  419  753

# check these CUs in nuseds
cond <- nuseds$cuid %in% CUs_missing # c(1202,291,292,419,753)
unique(nuseds[cond,c("SPECIES_QUALIFIED","cuid","FULL_CU_IN_PSF","CU_TYPE","CU_NAME")])

cond <- conservationunits_decoder$cuid %in% CUs_missing # c(1202,291,292,419,753)
conservationunits_decoder[cond,c("region","species_abbr","cuid","cu_name_pse",
                                 "cu_index","cu_type")]

# Ask why these CUs are not in the streamid files:
cuids <- c(1202,1207) # c(1202,291,292)
conservationunits_decoder[conservationunits_decoder$cuid %in% cuids,
                          c("region","species_abbr","cuid","cu_name_pse",
                            "cu_index","cu_type")]
streamlocationids[streamlocationids$cuid %in% cuids,c("streamid","cuid")]
streamspawnersurveys_output[streamspawnersurveys_output$cuid %in% cuids,c("streamid","cuid")]

cond <- nuseds$cuid %in% c(1202)
cond <- nuseds$cuid == c(1202)
nuseds$MAX_ESTIMATE[cond]

cond <- nuseds$cuid %in% c(1202,1207)
unique(nuseds[cond,c("SPECIES_QUALIFIED","cuid","FULL_CU_IN_PSF","CU_TYPE","CU_NAME")])

cuids <- c(1202,1207)
cuids <- c(1207)
cond <- nuseds$cuid %in% cuids
unique(nuseds[cond,c("SPECIES_QUALIFIED","cuid","CU_NAME","FULL_CU_IN_PSF","CU_TYPE","SYSTEM_SITE")])

cond <- conservationunits_decoder$cuid %in% cuids
conservationunits_decoder[cond,c("species_abbr","cuid","cu_name_pse","cu_index","cu_type")]

cond <- streamlocationids$cuid %in% cuids 
streamlocationids[cond,]

cond <- streamspawnersurveys_output$cuid %in% cuids 
streamspawnersurveys_output[cond,c("species_name","cuid","cu_name_pse","stream_name_pse","streamid")]

cond <- surveystreams$cuid %in% cuids
surveystreams[cond,]

# https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1710349319511559?thread_ts=1708454531.181729&cid=C03LB7KM6JK
#'
#'** END **

#' Remove the the CUs that are Bin or extinct
nuseds <- nuseds[! nuseds$cuid %in% c(419,753),]

#' TEMPORARY Remove the CUs not in the streamid-related datasets
nuseds <- nuseds[! nuseds$cuid %in% c(1207),]


#'* Attribute streamids *
#' Match each CU and locations i nuseds (using WATERBODY and SYS_NM) a streamid 
#' in streamlocationids or streamspawnersurveys_output (using :

#'** CHECKS TEMPORARY **
#' 1) Compare surveystreams$stream_name_pse and streamspawnersurveys_output$stream_name_pse
#' with streamlocationids$sys_nm --> they are the same after simplifying the strings.
#' 2) compare nuseds$SYSTEM_SITE with stream_name_pse --> 

#' 1)
surveystreams$stream_name_pse[! surveystreams$stream_name_pse %in% streamspawnersurveys_output$stream_name_pse]
streamspawnersurveys_output$stream_name_pse[! streamspawnersurveys_output$stream_name_pse %in% surveystreams$stream_name_pse]

stream_name_pse_modif <- simplify_string_fun(surveystreams$stream_name_pse)

sys_nm_modif <- simplify_string_fun(streamlocationids$sys_nm)

sum( sys_nm_modif %in% stream_name_pse_modif) / length(sys_nm_modif) * 100

#' 2)
SYSTEM_SITES_modif <- unique(simplify_string_fun(nuseds$SYSTEM_SITE))

sum(! SYSTEM_SITES_modif %in% stream_name_pse_modif) / length(SYSTEM_SITES_modif) * 100 # 11.4 1940 o

# QUESTION to Eric and Katy:
# https://salmonwatersheds.slack.com/archives/CKNVB4MCG/p1710352115378629

#'** END **

#' Make a dataframe the report the cuid, streamid, the sys_nm (from streamlocationids),
#' stream_name_pse (from streamspawnersurveys_output or surveystreams), SYSTEM_SITES
#' and WaTERSHED from nuseds.
#' Simplify the strings before matching them to correct many of the typos creating 
#' miss-matches.

nrow(unique(nuseds[,c("SPECIES_QUALIFIED",'cuid',"SYSTEM_SITE","WATERBODY")]))          # 6746
nrow(unique(nuseds[,c("SPECIES_QUALIFIED",'cuid',"SYSTEM_SITE","WATERBODY","GFE_ID")])) # 6758
nrow(unique(nuseds[,c("SPECIES_QUALIFIED",'POP_ID',"SYSTEM_SITE","WATERBODY")]))          # 6818
nrow(unique(nuseds[,c("SPECIES_QUALIFIED",'POP_ID',"SYSTEM_SITE","WATERBODY","GFE_ID")])) # 6818

col_nuseds <- c("cuid","SPECIES_QUALIFIED","FULL_CU_IN","POP_ID","CU_NAME","GFE_ID",
                "SYSTEM_SITE","WATERBODY","X_LONGT","Y_LAT")

nuseds_cuid_location <- unique(nuseds[,c("regionid","SPECIES_QUALIFIED","cuid",
                                         "SYSTEM_SITE","WATERBODY",
                                         "GAZETTED_NAME","LOCAL_NAME_1","LOCAL_NAME_2",
                                         "GFE_ID","X_LONGT","Y_LAT")]) # inclide GFE_ID because there can be multiple SYSTEM_SITEs for a same CU
nrow(nuseds_cuid_location) # 6758 if including GFE_ID...

# simplify the location name fields:
fields_locations <- c("SYSTEM_SITE","WATERBODY","GAZETTED_NAME",
                      "LOCAL_NAME_1","LOCAL_NAME_2")
# manual fix 1st:
#'-  "BARRI\xc8RE RIVER" --> "BARRIERE RIVER"
#'- "FRAN\xc7OIS LAKE" --> ""FRANCOIS LAKE"
#'
cond <- nuseds_cuid_location$SYSTEM_SITE == "BARRIERE RIVER"
nuseds_cuid_location$LOCAL_NAME_1[cond] <- "BARRIERE RIVER"
cond <- nuseds_cuid_location$SYSTEM_SITE == "EAST BARRIERE RIVER"
nuseds_cuid_location$LOCAL_NAME_1[cond] <- "EAST BARRIERE RIVER"
cond <- nuseds_cuid_location$SYSTEM_SITE == "FRANCOIS LAKE"
nuseds_cuid_location$LOCAL_NAME_1[cond] <- "FRANCOIS LAKE"

for(f in fields_locations){
  # f <- fields_locations[1]
  nuseds_cuid_location$X <- simplify_string_fun(nuseds_cuid_location[,f])
  colnames(nuseds_cuid_location)[colnames(nuseds_cuid_location) == "X"] <- paste0(f,"_simple")
}

nuseds_cuid_location$streamid <- NA
nuseds_cuid_location$field_nuseds_used <- NA
nuseds_cuid_location$sys_nm <- NA            # to record which sys_nm was used
nuseds_cuid_location$distance <- NA # the euclidean distance between the SYSTEM_SITE and sys_nm
nuseds_cuid_location$comment <- NA

streamlocationids$sys_nm_simple <- simplify_string_fun(streamlocationids$sys_nm)
streamlocationids$taken <- F

# options(warn = 0)
# options(warn=1)  # print warnings as they occur
options(warn = 2)  # treat warnings as errors

#' 1) 1st fill all the one with perfect match and do not try to trouble shoot the 
#' other ones yet because removing the perfect match 1st remove options when 
#' multiple matches are available when using grepl (which is used for trouble 
#' shooting).
count <- 0
count_show <- 1

# add the coordinate field
fields_locations <- c(fields_locations,"COORDINATES")

for(r in 1:nrow(nuseds_cuid_location)){
  # r <- 15
  # r <- 381 case with multiple spatial coordinates for a same location (because several GFE_ID correspond to a same SYSTEM_SITE)
  SYSTEM_SITE_here <- nuseds_cuid_location$SYSTEM_SITE[r]
  WATERBODY_here <- nuseds_cuid_location$WATERBODY[r]
  cuid_here <- nuseds_cuid_location$cuid[r]
  X_LONGT <- round(nuseds_cuid_location$X_LONGT[r],4)
  Y_LAT <- round(nuseds_cuid_location$Y_LAT[r],4)
  
  cond_cuid <- streamlocationids$cuid == cuid_here
  cond_fields_l <- list()
  
  cond_SYSTEM_SITE <- streamlocationids$cuid == cuid_here &
    streamlocationids$sys_nm_simple == nuseds_cuid_location$SYSTEM_SITE_simple[r]
  
  cond_WATERBODY <- streamlocationids$cuid == cuid_here &
    streamlocationids$sys_nm_simple == nuseds_cuid_location$WATERBODY_simple[r]
  
  cond_coordinates <- streamlocationids$cuid == cuid_here &
    round(streamlocationids$longitude,4) == X_LONGT &
    round(streamlocationids$latitude,4) == Y_LAT

  comment <- NA
  
  if(sum(cond_SYSTEM_SITE) + sum(cond_WATERBODY) + sum(cond_coordinates) > 0){
    
    # indicate which nuseds field(s) was used
    fields_used <- c(sum(cond_SYSTEM_SITE) > 0, sum(cond_WATERBODY) > 0, sum(cond_coordinates) > 0)
    nuseds_cuid_location$field_nuseds_used[r] <- paste(fields[fields_used], collapse = "-")
    
    # select condition
    cond_toUse <- list(cond_SYSTEM_SITE,
                       cond_WATERBODY,
                       cond_coordinates)[fields_used][[1]]
    
    # calculate the distance
    distances <- distance_Euclidean_fun(x_ref = X_LONGT, y_ref = Y_LAT, 
                                        x = round(streamlocationids$longitude[cond_toUse],4),
                                        y = round(streamlocationids$latitude[cond_toUse],4))
    
    # in case there are more than one option (there should not be)
    if(sum(cond_toUse) > 1){
      #' Select the closest location: edit cond_toUse so it only retain the row 
      #' with the smallest distance
      cond_toUse_copy <- rep(FALSE,length(cond_toUse))
      toKeep <- which(cond_toUse)[distances == min(distances)]
      cond_toUse_copy[toKeep] <- TRUE
      cond_toUse <- cond_toUse_copy
      distance <- distances[distances == min(distances)] 
      comment <- "More than one match so we picked the closest location"
      
      # just to check
      if(length(distance) > 1){
        print(r)
        break
      }
      
    }else if(sum(cond_toUse) == 1){
      distance <- distances
    }
    
    # in case there are still more than one option (there should not be)
    if(sum(cond_toUse) > 1){
      print("*** STILL MORE THSAN ONE CHOICE - TO FIX ***")
      print(nuseds_cuid_location[r,])
      break
    }
    
    # fill streamlocationids if there is only one option
    if(sum(cond_toUse) == 1){
      
      # find the corresponding streamid:
      streamid_here <- streamlocationids$streamid[cond_toUse]
      
      nuseds_cuid_location$streamid[r] <- streamid_here
      nuseds_cuid_location$sys_nm[r] <- streamlocationids$sys_nm[cond_toUse]
      nuseds_cuid_location$distance[r] <- distance
      nuseds_cuid_location$comment[r] <- comment
      
      streamlocationids$taken[cond_toUse] <- T
      
    }
  }else{
    
    # 1st try without cuid and use regionid instead, which would mean we need to create a new
    regionid_here <- nuseds_cuid_location$regionid[r]
    cond_SYSTEM_SITE <- streamlocationids$regionid == regionid_here &
      streamlocationids$sys_nm_simple == nuseds_cuid_location$SYSTEM_SITE_simple[r]
    cond_WATERBODY <- streamlocationids$regionid == regionid_here &
      streamlocationids$sys_nm_simple == nuseds_cuid_location$WATERBODY_simple[r]
    cond_coordinates <- streamlocationids$regionid == regionid_here &
      round(streamlocationids$longitude,4) == X_LONGT &
      round(streamlocationids$latitude,4) == Y_LAT
    
    # indicate which nuseds field(s) was used
    fields_used <- c(sum(cond_SYSTEM_SITE) > 0, sum(cond_WATERBODY) > 0, sum(cond_coordinates) > 0)
    nuseds_cuid_location$field_nuseds_used[r] <- paste(fields[fields_used], collapse = "-")
    
    if(sum(fields_used) > 0){
      
      # select condition
      cond_toUse <- cond_SYSTEM_SITE | cond_WATERBODY | cond_coordinates
      
      # calculate the distance
      distances <- distance_Euclidean_fun(x_ref = X_LONGT, y_ref = Y_LAT, 
                                          x = round(streamlocationids$longitude[cond_toUse],4),
                                          y = round(streamlocationids$latitude[cond_toUse],4))
      
      #' Select the closest location: edit cond_toUse so it only retain the row 
      #' with the smallest distance
      cond_toUse_copy <- rep(FALSE,length(cond_toUse))
      toKeep <- which(cond_toUse)[distances == min(distances)]
      cond_toUse_copy[toKeep] <- TRUE
      cond_toUse <- cond_toUse_copy
      distance <- distances[distances == min(distances)] 
      
      # fill nuseds_cuid_location
      nuseds_cuid_location$streamid[r] <- -99
      nuseds_cuid_location$sys_nm[r] <- unique(streamlocationids$sys_nm[cond_toUse])
      nuseds_cuid_location$distance[r] <- unique(distance) # should only be one
      nuseds_cuid_location$comment[r] <- "Location exist but no streamid for its assocoation with this cuid"
      
    }else{ # if there is still no match: just signal it for now
      
      count <- count + 1
      count_percent <- round(count/nrow(nuseds_cuid_location)*100,1)
      if(count_show <= count_percent){
        print(paste("Proportion not matching:",count_percent,"%"))
        count_show <- count_show + 1
      }
    }
  }
}
print(paste("Final proportion not matching:",count_percent,"%"))

table(nuseds_cuid_location$comment)

#' Matches & not matched: Proportion and nb.:
streamid_na <- is.na(nuseds_cuid_location$streamid)
streamid_99 <- nuseds_cuid_location$streamid == -99 & !streamid_na
(sum(!streamid_na) - sum(streamid_99)) / nrow(nuseds_cuid_location) # 0.93
sum(!streamid_na) - sum(streamid_99)  # 6272
sum(streamid_na) / nrow(nuseds_cuid_location) # 0.06
sum(streamid_na)  # 433

#' Missing streamid: 
#' - proportion and number
#' - distribution of distances for those where COORDINATES was not used
#' --> there are two with distance > 1.0 --> might not be the right location.
sum(streamid_99) # 53
sum(streamid_99) / nrow(nuseds_cuid_location) # 0.008
nuseds_cuid_location_99 <- nuseds_cuid_location[streamid_99,]
table(nuseds_cuid_location_99$field_nuseds_used)
streamid_99_noCoord <- !grepl("COORDINATES",nuseds_cuid_location_99$field_nuseds_used)
hist(nuseds_cuid_location_99[streamid_99_noCoord,]$distance)
nuseds_cuid_location_99[streamid_99_noCoord,][nuseds_cuid_location_99$distance[streamid_99_noCoord] > 0.1,]





sum(is.na(nuseds_cuid_location$streamid)) # 433

table(nuseds_cuid_location$field_nuseds_used)

#' Look at the matches that were due to COORDINATES only and compare the location 
#' names --> HOW WERE THESE DISCREPANCIES CREATED?
cond <- nuseds_cuid_location$field_nuseds_used == "COORDINATES" & 
  !is.na(nuseds_cuid_location$field_nuseds_used)
nuseds_cuid_location[cond,c("SPECIES_QUALIFIED","cuid","SYSTEM_SITE","WATERBODY",
                            "GFE_ID","sys_nm")]

#' Look at cases where the closest location was selected:
#' --> there is only one case:
#' question asked during pop meeting
message <- "More than one match so we picked the closest location"
cond <- nuseds_cuid_location$comment == message &
  !is.na(nuseds_cuid_location$comment)
nuseds_cuid_location[cond,]

cond <- streamlocationids$sys_nm == "HARRIET CREEK" &
  streamlocationids$cuid == 811
streamlocationids[cond,]

#' Check the distances for the matches done without coordinate
#' QUESTIONS: THESE ARE PRETTY LARGE DISTANCES NO?
cond <- grepl("COORDINATES",nuseds_cuid_location$field_nuseds_used)
hist(nuseds_cuid_location$distance[!cond], breaks = 100, xlim = c(0,0.5))
range(nuseds_cuid_location$distance[!cond],na.rm = T)
cond <- !cond & !is.na(nuseds_cuid_location$distance) & nuseds_cuid_location$distance > 0.1 # 0.1 is roughly 3km
nuseds_cuid_location[cond,c("SPECIES_QUALIFIED","cuid","SYSTEM_SITE","WATERBODY","sys_nm","streamid","distance","comment")]



#' 2) use grepl for the matching of the remaining data
#' use spatial coordinate only to find the closest location
count <- 1
for(r in 1:nrow(nuseds_cuid_location)){
  # r <- 5  # no match
  # r <- 28 # match with one stream not already taken
  
  if(is.na(nuseds_cuid_location$streamid[r])){
    
    comment <- "Used grepl()"
    
    SYSTEM_SITE_here <- nuseds_cuid_location$SYSTEM_SITE[r]
    WATERBODY_here <- nuseds_cuid_location$WATERBODY[r]
    cuid_here <- nuseds_cuid_location$cuid[r]
    
    cond_SYSTEM_SITE <- streamlocationids$cuid == cuid_here &
      grepl(nuseds_cuid_location$SYSTEM_SITE_simple[r],streamlocationids$sys_nm_simple)
    
    cond_WATERBODY <- streamlocationids$cuid == cuid_here &
      grepl(nuseds_cuid_location$WATERBODY_simple[r],streamlocationids$sys_nm_simple)
    
    # if there is no match --> try extra tricks
    if(sum(cond_SYSTEM_SITE) + sum(cond_WATERBODY) == 0){
      
      
      
      
    }
    
    # 
    if(sum(cond_SYSTEM_SITE) + sum(cond_WATERBODY) > 0){
      
      # indicate which nuseds field(s) was used
      if(sum(cond_SYSTEM_SITE) > 0 & sum(cond_WATERBODY) > 0){
        nuseds_cuid_location$field_nuseds_used[r] <- "BOTH"
        cond_toUse <- cond_SYSTEM_SITE
      }else{
        cond <- c(any(cond_SYSTEM_SITE),any(cond_WATERBODY))
        nuseds_cuid_location$field_nuseds_used[r] <- c("SYSTEM_SITE","WATERBODY")[cond]
        cond_toUse <- list(cond_SYSTEM_SITE,cond_WATERBODY)[cond][[1]]
      }
      
      # find the corresponding streamid:
      streamid_here <- streamlocationids$streamid[cond_toUse]
      
      # fix by checking if some streamid are already taken
      if(sum(cond_toUse) > 1){
        cond_toUse <- cond_toUse & ! streamlocationids$taken
      }
      
      # if there still are multiple matches, use the spatial coordinates
      if(sum(cond_toUse) > 1){
        
        # look at GIS coordinates
        cond_nuseds <- nuseds$cuid == cuid_here &
          nuseds$SYSTEM_SITE == SYSTEM_SITE_here & 
          nuseds$WATERBODY == WATERBODY_here
        
        X_LONGT <- round(unique(nuseds$X_LONGT[cond_nuseds]),4)
        Y_LAT <- round(unique(nuseds$Y_LAT[cond_nuseds]),4)
        
        cond_toUse_new <- cond_toUse &
          round(streamlocationids$longitude,4) == X_LONGT &
          round(streamlocationids$latitude,4) == Y_LAT
        
        if(sum(cond_toUse_new) == 1){
          cond_toUse <- cond_toUse_new
          comment <- paste(comment,"More than one match so we used X_LONGT and Y_LAT",sep = " ; ")
          
        }else if(sum(cond_toUse_new) == 0){ # take the location with the smallest distance
          
          distances <- distance_Euclidean_fun(x_ref = X_LONGT, y_ref = Y_LAT, 
                                              x = streamlocationids$longitude[cond_toUse],
                                              y = streamlocationids$latitude[cond_toUse])
          
          # edit cond_toUse so it only retain the row with the smallest distance
          cond_toUse_copy <- rep(FALSE,length(cond_toUse))
          toKeep <- which(cond_toUse)[distances == min(distances)]
          cond_toUse_copy[toKeep] <- TRUE
          cond_toUse <- cond_toUse_copy
          comment_here <- "More than one match so we used X_LONGT and Y_LAT and picked the closest location"
          comment <- paste(comment,
                           paste0(comment_here," (distance = ",round(min(min(distances)),3),")"),sep = " ; ")
        }
      }
      
      if(sum(cond_toUse) == 1){
        
        # individual fixes in case the is one match but it is already taken
        if(streamlocationids$taken[cond_toUse]){
          
          #' The only remaining choice for "CHILLIWACK RIVER" in nuseds is 
          #' "CHILLIWACK/VEDDER RIVER" in streamlocationids
          if(grepl("chilliwack",nuseds_cuid_location$SYSTEM_SITE_simple[r]) & 
             cuid_here %in% c(701,710,750)){
            
            cond_toUse <- streamlocationids$cuid == cuid_here &
              grepl("chilliwack",streamlocationids$sys_nm_simple) & 
              !streamlocationids$taken
            
            comment <- paste(comment,"had to use 'chilliwack' and !streamlocationids$taken for the match", sep = " ; ")
          }
          
          #
          
        }
        
        # check that the streamid is not already used
        if(!streamlocationids$taken[cond_toUse]){
          
          streamid_here <- streamlocationids$streamid[cond_toUse]
          
          nuseds_cuid_location$streamid[r] <- streamid_here
          nuseds_cuid_location$sys_nm[r] <- streamlocationids$sys_nm[cond_toUse]
          nuseds_cuid_location$comment[r] <- comment
          
          streamlocationids$taken[cond_toUse] <- T
          
        }else{
          
          print("***")
          print("There is a match but it is already taken")
          print("Here is the row that need to be filled:")
          print(nuseds_cuid_location[r,])
          print("Here is the row that was already filled:")
          cond <- nuseds_cuid_location$streamid == streamid_here & 
            !is.na(nuseds_cuid_location$streamid)
          print(nuseds_cuid_location[cond,])
          print("Here are the options:")
          print(streamlocationids[cond_toUse,])
          
          break
        }
      }else{ # if length(streamid_here) > 1
        
        # temporary
        toSkip_SYSTEM_SITE <- c("CHILCOTIN RIVER") # c("CHILCOTIN RIVER","HARRIET CREEK")
        
        if(! SYSTEM_SITE_here %in% toSkip_SYSTEM_SITE){
          print("***")
          print("There is more than one streamid")
          print("This is the row that need to be filled:")
          print(nuseds_cuid_location[r,])
          print("These are the options:")
          print(streamlocationids[cond_toUse,])
          break
        }
       
      }
      
    }else{ # if there is still no match
      
      print(paste(count,r,cuid_here,SYSTEM_SITE_here,WATERBODY_here,sep = " - "))
      count <- count + 1
      
      if(count > 15){
        break
      }
    }
  }
}

table(nuseds_cuid_location$comment)

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


location <- "MESACHIE"
cuid <- 904
cond <- streamlocationids$cuid == cuid & grepl(simplify_string_fun(location),streamlocationids$sys_nm_simple)
streamlocationids[cond,]

cond <- nuseds$cuid == cuid & (grepl(location,nuseds$SYSTEM_SITE) | 
                                 grepl(location,nuseds$WATERBODY))
cond <-  (grepl(location,nuseds$SYSTEM_SITE) | 
            grepl(location,nuseds$WATERBODY))
unique(nuseds[cond,c("SPECIES_QUALIFIED","cuid","SYSTEM_SITE","WATERBODY","X_LONGT","Y_LAT")])
nuseds$X_LONGT[cond]


#' ASK DURING POP MEETING
location <- "HARRIET CREEK"
cuid <- 811
cond <- streamlocationids$cuid == cuid & grepl(simplify_string_fun(location),streamlocationids$sys_nm_simple)
streamlocationids[cond,]

cond <- nuseds$cuid == cuid & (grepl(location,nuseds$SYSTEM_SITE) | 
                                 grepl(location,nuseds$WATERBODY))
cond <-  (grepl(location,nuseds$SYSTEM_SITE) | 
            grepl(location,nuseds$WATERBODY))
unique(nuseds[cond,c("SPECIES_QUALIFIED","cuid","SYSTEM_SITE","WATERBODY")])
nuseds$MAX_ESTIMATE[cond]

#' They do not appear on the PSE map but are present in the list
#' I do not know which of the UPPER or LOWER I should use
#' ASK DURING POP MEETING
location <- "CHILCOTIN RIVER"
location <- "CHILCOTIN"
cuid <- 310
cond <- streamlocationids$cuid == cuid & grepl(simplify_string_fun(location),streamlocationids$sys_nm_simple)
streamlocationids[cond,]

cond <- nuseds$cuid == cuid & (grepl(location,nuseds$SYSTEM_SITE) | 
                                 grepl(location,nuseds$WATERBODY))
cond <-  (grepl(location,nuseds$SYSTEM_SITE) | 
                                 grepl(location,nuseds$WATERBODY))
unique(nuseds[cond,c("SPECIES_QUALIFIED","cuid","SYSTEM_SITE","WATERBODY")])
nuseds$MAX_ESTIMATE[cond]

#' ASK DURING POP MEETING
location <- "GOOSE POINT"
cuid <- 728
cond <- streamlocationids$cuid == cuid & grepl(simplify_string_fun(location),streamlocationids$sys_nm_simple)
streamlocationids[cond,]

cond <- nuseds$cuid == cuid & (grepl(location,nuseds$SYSTEM_SITE) | 
                                 grepl(location,nuseds$WATERBODY))
cond <-  (grepl(location,nuseds$SYSTEM_SITE) | 
            grepl(location,nuseds$WATERBODY))
unique(nuseds[cond,c("SPECIES_QUALIFIED","cuid","SYSTEM_SITE","WATERBODY")])
nuseds$MAX_ESTIMATE[cond]

nuseds$


#' Make a dataframe to report which streamid was attributed during the procedure 
#' so we make sure no streamid gets attributed multiple times
streamlocationids_used <- streamlocationids[,c("cuid","sys_nm","streamid")]
streamlocationids_used$used <- F
streamlocationids_used$field_nuseds_used <- NA

count <- 1
for(r in 1:nrow(nuseds_cuid_location)){
  # r <- 5
  sys_nm_here <- nuseds_cuid_location$SYS_NM[r]
  waterB_here <- nuseds_cuid_location$WATERBODY[r]
  cuid_here <- nuseds_cuid_location$cuid[r]
  
  # look for the exact match
  cond <- streamlocationids$sys_nm %in% c(sys_nm_here,waterB_here) &
    streamlocationids$cuid == cuid_here
  
  #' if more than one match, look for sys_nm_here only (i.e. we assume that sys_nm, 
  #' = SYSTEM_SITE) is more accurate than WATERBODY 
  if(sum(cond) > 1){
    cond <- streamlocationids$sys_nm == sys_nm_here &
      streamlocationids$cuid == cuid_here
  }
  
  #' if not match, use grepl
  if(sum(cond) == 0){
    cond <- (grepl(sys_nm_here,streamlocationids$sys_nm,fixed = T) | 
               grepl(waterB_here,streamlocationids$sys_nm,fixed = T)) &
      streamlocationids$cuid == cuid_here
    
    # same as above, only use sys_nm
    if(sum(cond) > 1){
      cond <- grepl(sys_nm_here,streamlocationids$sys_nm,fixed = T) &
        streamlocationids$cuid == cuid_here
    }
    
    # case-specific fixes
    if(sum(cond) > 1){
      
      if(sys_nm_here == "TUNO CREEK"){ 
        cond <- streamlocationids$sys_nm == "TUNO CREEK-EAST" &
          streamlocationids$cuid == cuid_here
        
      }else if(sys_nm_here == "TUNO CREEK WEST"){
        cond <- streamlocationids$sys_nm == "TUNO CREEK-WEST" &
          streamlocationids$cuid == cuid_here
        
      }else if(sys_nm_here == "THOMPSON RIVER" & cuid_here == 313){  # the other option is "SOUTH THOMPSON RIVER", which gets picked up as well 
        cond <- streamlocationids$sys_nm == "THOMPSON RIVER BELOW KAMLOOPS LAKE" &
          streamlocationids$cuid == cuid_here
        
      }
      # else if(sys_nm_here == "FANNIE COVE LEFT HAND CREEK" & cuid_here == 503){  # the other option is "SOUTH THOMPSON RIVER", which gets picked up as well 
      #   
      #   cond <- streamlocationids$sys_nm == "THOMPSON RIVER BELOW KAMLOOPS LAKE" &
      #     streamlocationids$cuid == cuid_here
      #   
      # }
    }
  }
  
  # remove spaces and spetial characters
  if(sum(cond) == 0){

    sys_nm_here_modif <- gsub(" ","",sys_nm_here)
    waterB_here_modif <- gsub(" ","",waterB_here)
    sys_nm_here_modif <- gsub("-","",sys_nm_here_modif)
    waterB_here_modif <- gsub("-","",waterB_here_modif)
    sys_nm_here_modif <- gsub("'","",sys_nm_here_modif)
    waterB_here_modif <- gsub("'","",waterB_here_modif)
    sys_nm_here_modif <- gsub("\\\\","",sys_nm_here_modif)
    waterB_here_modif <- gsub("\\\\","",waterB_here_modif)
    
    sys_nm_modif <- gsub(" ","",streamlocationids$sys_nm)
    sys_nm_modif <- gsub("-","",sys_nm_modif)
    sys_nm_modif <- gsub("'","",sys_nm_modif)
    sys_nm_modif <- gsub("\\\\","",sys_nm_modif)
    
    cond <- (grepl(sys_nm_here_modif,sys_nm_modif,fixed = T) | 
               grepl(waterB_here_modif,sys_nm_modif,fixed = T)) &
      streamlocationids$cuid == cuid_here
    
  }
  
  if(sum(cond) == 0){
    # remove " RIVER"
    if(grepl("RIVER",sys_nm_here)){
      sys_nm_here_modif <- gsub(" RIVER","",sys_nm_here)
      waterB_here_modif <- gsub(" RIVER","",waterB_here)
      
    }else if(grepl("LEFT HAND",sys_nm_here)){
      sys_nm_here_modif <- gsub("LEFT HAND","LH",sys_nm_here)
      waterB_here_modif <- gsub("LEFT HAND","LH",waterB_here)
      
    }else if(grepl("RIGHT HAND",sys_nm_here)){
      sys_nm_here_modif <- gsub("RIGHT HAND","RH",sys_nm_here)
      waterB_here_modif <- gsub("RIGHT HAND","RH",waterB_here)
      
    }
    
    cond <- (grepl(sys_nm_here_modif,streamlocationids$sys_nm,fixed = T) | 
               grepl(waterB_here_modif,streamlocationids$sys_nm,fixed = T)) &
      streamlocationids$cuid == cuid_here
    
  }

  if(sum(cond) == 0){
    
    print(paste(count,r,cuid_here,sys_nm_here,waterB_here,sep = " - "))
    count <- count + 1
    
    if(count > 10){
      break
    }
  }else if(sum(cond) > 1){
    
    print(streamlocationids[cond,c("cuid","streamid","sys_nm")])
    print(c(sys_nm_here,waterB_here))
    count <- count + 1
    
    # break
    
  }else{

    # make sure the streamid is not used already
    if(streamlocationids_used$used[cond] == F){
      
      streamlocationids_used$used[cond] <- T
      
      #fields_locations_used_here <- c("WATERBODY","SYS_NM")[c(sys_nm_here,waterB_here) %in% streamlocationids$sys_nm]
      fields_locations_used_here <- c("WATERBODY","SYS_NM")[c(any(grepl(sys_nm_here,streamlocationids$sys_nm,fixed = T)),
                                                              any(grepl(waterB_here,streamlocationids$sys_nm,fixed = T)))]
      if(length(fields_locations_used_here) == 2){
        fields_locations_used_here <- "BOTH"
      }
      
      if(length(fields_locations_used_here) == 1){
        streamlocationids_used$field_nuseds_used[cond] <- fields_locations_used_here
      }
      
      streamid_here <- streamlocationids$streamid[cond]
      cond <- nuseds$SYS_NM == sys_nm_here & nuseds$cuid == cuid_here
      nuseds$streamid[cond] <- streamid_here
      
      if(length(fields_locations_used_here) != 1){
        # break
      }
      
    }else{
      
      print("*** streamid already used: ***")
      print(streamlocationids[cond,])
      
    }
  }
}

SOUTH BAY #1 CREEK
SOUTH BAY CREEK #1
HESQUIAT HARBOUR CREEK #2
HESQUIAT HARBOUR #2 CREEKS

location <- "SOUTH THOMPSON RIVER"
cuid <- 313

location <- "HESQUIAT"
cuid <- 978

location <- "FANNIE COVE LEFT HAND CREEK"
cuid <- 503

cond <- streamlocationids$cuid == cuid
cond <- grepl(location,streamlocationids$sys_nm) & streamlocationids$cuid == cuid
streamlocationids[cond,]

cond <- grepl(location,nuseds$SYS_NM, fixed = T) & nuseds$cuid == cuid
unique(nuseds[cond,c("cuid","CU_name","WATERBODY","SYS_NM")])

cond <- grepl(tolower(location),tolower(surveystreams$cu_name_pse)) &
  surveystreams$cuid == cuid
surveystreams[cond,]

cond <- grepl(tolower("CHILCOTIN"),tolower(surveystreams$cu_name_pse), fixed = T) &
  surveystreams$cuid == cuid
surveystreams[cond,]




table(fields_locations_used)

streamlocationids$sys_nm

nuseds$SYS_NM
nuseds$cuid

streamlocationids$cuid



# FUTURE THINGS TO DO ------

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

