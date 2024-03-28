


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

fields_nuseds_CU <- c("CU_NAME","SPECIES_QUALIFIED","FULL_CU_IN",
                      "FULL_CU_IN_PSF","CU_TYPE")

CU_name_species <- unique(nuseds[,fields_nuseds_CU])
nrow(CU_name_species) # 415

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
  
  # still no match --> try match with CU_NAME with cu_name_dfo and cu_name_dfo
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
  
  # still no match, no nothing for now
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


#' TODO: troubleshoot with Katy after PSE 2.0
# nuseds <- nuseds[!is.na(nuseds$cuid),]
nrow(nuseds) # 307217

#
#'* Bring the stream-related fields *

#'** CHECKS TEMPORARY **
#' 1) compare cuid between streamlocationids, streamspawnersurveys_output and 
#' surveystreams --> CU in Skeena Lake sockeye  181 Bulkley/Maxan is only in 
#' streamspawnersurveys_output
#' https://salmonwatersheds.slack.com/archives/CKNVB4MCG/p1710353792636679
#' SOVLED

#' 2) check if any cuid in nuseds are not present in streamspawnersurveys_output
#' --> CUs 1207  419  753 are missing (note that other CUs were missing and 
#' issues were fixed but I left the history of the message below for a record).
#' - Katy: 755 and 936 were removed based on this PSAC feedback
#' - Eric: 531 was removed based on Technical Working Group feedback (can't be 
#' sockeye there because there is an impassable falls)
#' - 291 and 292: FIXED (added)
#' - Katy: "streamid 10201 NORDENSKIOLD RIVER was attributed to CUID 1207 in the 
#' database. I've changed it to CUID 1202"
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1682606819442789
#' https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1709857183818209?thread_ts=1708454531.181729&cid=C03LB7KM6JK

#' 3) compare sys_nm in streamlocationids with stream_name_pse in surveystreams and
#' streamspawnersurveys_output --> there are the same after simplification of the
#' name. ALL GOOD.

#' 1)
streamlocationids$cuid[! streamlocationids$cuid %in% streamspawnersurveys_output$cuid] # 0
streamspawnersurveys_output$cuid[! streamspawnersurveys_output$cuid %in% streamlocationids$cuid] # 181
streamspawnersurveys_output$cuid[! streamspawnersurveys_output$cuid %in% surveystreams$cuid] # 0
surveystreams$cuid[! surveystreams$cuid %in% streamspawnersurveys_output$cuid] # 0

streamspawnersurveys_output[streamspawnersurveys_output$cuid == 181,]
surveystreams[surveystreams$cuid == 181,]
streamlocationids[streamlocationids$cuid == 181,]
conservationunits_decoder[conservationunits_decoder$cuid == 181,]

cond <- nuseds$cuid == 181
nuseds[cond,]$MAX_ESTIMATE
unique(nuseds[cond,]$SYSTEM_SITE)

cond <- grepl("[M|m]axan",streamspawnersurveys_output$stream_name_pse)
sum(cond)
streamspawnersurveys_output[cond,]

#' 2)
CUs_missing <- unique(nuseds$cuid[! nuseds$cuid %in% streamspawnersurveys_output$cuid])
CUs_missing
# 1207  419  753

# check these CUs in nuseds
cond <- nuseds$cuid %in% CUs_missing # c(1202,291,292,419,753)
unique(nuseds[cond,c("SPECIES_QUALIFIED","cuid","FULL_CU_IN_PSF","CU_TYPE","CU_NAME")])

cond <- conservationunits_decoder$cuid %in% c(1207,419,753) # CUs_missing # c(1202,291,292,419,753)
conservationunits_decoder[cond,c("region","species_abbr","cuid","cu_name_pse",
                                 "cu_index","cu_type")]

cond <- surveystreams$cuid %in% c(1207,419,753) # CUs_missing # c(1202,291,292,419,753)
surveystreams[cond,]

#' Start assembling the case where new streamid have to be associated to a cuid 
#' (in new_streamids) and when a cuid has to be added to the streamid files and 
#' associated to 
new_streamids <- streamlocationids[NA,c("pointid","sys_nm","latitude","longitude",
                                          "cuid","cu_name_pse")][1,]
new_streamids$cuid_present <- NA
new_streamids$location_present <- NA

#' 1202 was rightly associated to streamid 10201 (it was cuid 1207 before)
#' - Katy changed 
cuid <- 1202
cond <- nuseds$cuid == cuid
unique(nuseds[cond,c("cuid","IndexId","SYSTEM_SITE","Y_LAT","X_LONGT")])
cond <- streamlocationids$cuid == 1202
streamlocationids[cond,c("cuid","streamid","sys_nm","latitude","longitude")]

#' But cuid 1207 is in NuSEDS but not in streamlocationids
#' - Katy changed 
cuid <- 1207
cond_nuseds <- nuseds$cuid == cuid
locations <- unique(nuseds[cond_nuseds,c("regionid","cuid","cu_name_pse","IndexId","GFE_ID","SYSTEM_SITE","Y_LAT","X_LONGT")])
# are these location already in streamlocationids ?
for(i in 1:nrow(locations)){
  
  new_streamids_new <- new_streamids[NA,][1,]
  new_streamids_new$cuid_present <- F
  new_streamids_new$sys_nm <- locations$SYSTEM_SITE[i]
  new_streamids_new$latitude <- locations$Y_LAT[i]
  new_streamids_new$longitude <- locations$X_LONGT[i]
  new_streamids_new$cuid <- locations$cuid[i]
  new_streamids_new$cu_name_pse <- locations$cu_name_pse[i]
  
  print(locations$SYSTEM_SITE[i])
  cond <- locations$SYSTEM_SITE[i] == streamlocationids$sys_nm |
    (round(streamlocationids$latitude,3) == round(locations$Y_LAT[i],3) &
       round(streamlocationids$longitude,3) == round(locations$X_LONGT[i],3))
  
  # if the location is not present in streamlocationids
  if(nrow(streamlocationids[cond,]) == 0){
    print(streamlocationids[cond,])
    cond <- locations$regionid[i] == streamlocationids$regionid
    dist <- distance_Euclidean_fun(x_ref = locations$X_LONGT[i], y_ref = locations$Y_LAT[i],
                                   x = streamlocationids$longitude[cond],
                                   y = streamlocationids$latitude[cond])
    
    print(paste(unique(streamlocationids$sys_nm[cond][which(dist ==  min(dist))]),
                round(min(dist),4),sep = " - "))
    
    new_streamids_new$location_present <- F
    
  }else{
    dist <- distance_Euclidean_fun(x_ref = locations$X_LONGT[i], y_ref = locations$Y_LAT[i],
                           x = streamlocationids$longitude[cond],
                           y = streamlocationids$latitude[cond])
    dist <- data.frame(distance = dist)
    print(cbind(streamlocationids[cond,],dist))
    
    new_streamids_new$location_present <- T
    new_streamids_new$pointid <- unique(streamlocationids$pointid[cond])
  }
  # plot_IndexId_GFE_ID_fun(IndexIds = locations$IndexId[i],
  #                         GFE_IDs = locations$GFE_ID[i],
  #                         all_areas_nuseds = nuseds)
  # legend("top",locations$SYSTEM_SITE[i])
  print("***")
  
  if(nrow(new_streamids) == 1 & is.na(new_streamids$cuid)[1]){
    new_streamids <- new_streamids_new
  }else{
    new_streamids <- rbind(new_streamids,new_streamids_new)
  }
}
#' I asked Katy to create a new streamid for the existing location for cuid 1207.
#' I said I will let her know about the two new location later.
#' https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1711060462098899?thread_ts=1708454531.181729&cid=C03LB7KM6JK
new_streamids

#' 419 --> the location exist so it is about creating a new streamid
cuid <- 419
cond_nuseds <- nuseds$cuid == cuid
locations <- unique(nuseds[cond_nuseds,c("regionid","cuid","IndexId","GFE_ID",
                                         "SYSTEM_SITE","Y_LAT","X_LONGT")])
# plot_IndexId_GFE_ID_fun(IndexIds = unique(locations$IndexId),
#                         GFE_IDs = locations$GFE_ID,
#                         all_areas_nuseds = nuseds)

new_streamids_new <- new_streamids[NA,][1,]
new_streamids_new$cuid_present <- F
new_streamids_new$sys_nm <- locations$SYSTEM_SITE[1]
new_streamids_new$latitude <- locations$Y_LAT[1]
new_streamids_new$longitude <- locations$X_LONGT[1]
new_streamids_new$cuid <- locations$cuid[1]
new_streamids_new$cu_name_pse <- locations$cu_name_pse[1]

cond <- locations$SYSTEM_SITE[1] == streamlocationids$sys_nm |
  (round(streamlocationids$latitude,3) == round(locations$Y_LAT[1],3) &
     round(streamlocationids$longitude,3) == round(locations$X_LONGT[1],3))


#' 419
cuid <- 753
cond_nuseds <- nuseds$cuid == cuid
locations <- unique(nuseds[cond_nuseds,c("regionid","cuid","IndexId","GFE_ID",
                                         "SYSTEM_SITE","Y_LAT","X_LONGT")])
# plot_IndexId_GFE_ID_fun(IndexIds = unique(locations$IndexId),
#                         GFE_IDs = locations$GFE_ID,
#                         all_areas_nuseds = nuseds)
# check if the location is in streamlocationids
cond <- streamlocationids$sys_nm == "LEVERSON CREEK"
streamlocationids[cond,]

#' 3)
stream_name_pse <- simplify_string_fun(surveystreams$stream_name_pse)
sys_nm <- simplify_string_fun(streamlocationids$sys_nm)
stream_name_pse[!stream_name_pse %in% sys_nm]
sys_nm[!sys_nm %in% stream_name_pse]

#
#'** END **

#' TEMPORARY Remove the the CUs that are Bin or extinct
# nuseds <- nuseds[! nuseds$cuid %in% c(419,753),]

#' TEMPORARY Remove the CUs not in the streamid-related datasets
# nuseds <- nuseds[! nuseds$cuid %in% c(1207),]


#'* Attribute pointid *
#' Try to match locations in nuseds with locations in streamlocationids:

fields_nuseds_location <- c("SYSTEM_SITE","WATERBODY","GAZETTED_NAME",  # regionid is a CU-related field, not a location
                            "LOCAL_NAME_1","LOCAL_NAME_2",
                            "GFE_ID","X_LONGT","Y_LAT")

nuseds_location <- unique(nuseds[,fields_nuseds_location]) # inclide GFE_ID because there can be multiple SYSTEM_SITEs for a same CU
nrow(nuseds_location) # 2311

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

distance_threshold <- 0.5 # ~ 50km

# options(warn = 0)
options(warn=1)  # print warnings as they occur
options(warn = 2)  # treat warnings as errors

decimals <- 6

for(r in 1:nrow(nuseds_location)){
  # r <- 1 
  
  # if(nuseds_location$SYSTEM_SITE[r] == "STRANDBY RIVER"){
  #   break
  # }
  
  fileds_values_l <- list()
  v_i <- 1
  for(v in fields_locations){
    if(v == "COORDINATES"){
      X_LONGT <- round(nuseds_location$X_LONGT[r],decimals)
      Y_LAT <- round(nuseds_location$Y_LAT[r],decimals)
      fileds_values_l[[v_i]] <- list(X_LONGT,Y_LAT)
      names(fileds_values_l[[v_i]]) <- c("X_LONGT","Y_LAT")
    }else{
      fileds_values_l[[v_i]] <- nuseds_location[r,v]
    }
    v_i <- v_i + 1
  }
  names(fileds_values_l) <- fields_locations

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
    if(distance > distance_threshold){
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
print(paste("Proportion of locations not matching:",count_percent,"%")) # 6.6%

table(nuseds_location$comment)
sum(is.na(nuseds_location$pointid)) # 152
sum(is.na(nuseds_location$pointid)) / nrow(nuseds_location) # 0.065
sum(!is.na(nuseds_location$pointid)) / nrow(nuseds_location) # 0.93


#'* 2) use grepl for the matching of the remaining data *
#' use spatial coordinate only to find the closest location when multiple options.
#' Check that the pointid has not been already taken in the section above.

# manual fixes
location_names_fixes <- SYSTEM_SITE_fixes_fun()
SYSTEM_SITE_fixes <- location_names_fixes$SYSTEM_SITE
sys_nm_fixes <- location_names_fixes$sys_nm
rm(location_names_fixes)

nuseds_location$SYSTEM_SITE_corrected <- NA

count <- 0
count_show <- 1
for(r in 1:nrow(nuseds_location)){
  # r <- 5
  if(is.na(nuseds_location$pointid[r])){
    
    # if(nuseds_location$SYSTEM_SITE[r] == "SWAN LAKE CREEK #2 UNNAMED"){
    #   print("Stoped")
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
        if(v == "SYSTEM_SITE" & !is.na(nuseds_location$SYSTEM_SITE_corrected[r])){
          fileds_values_l[[v_i]] <- nuseds_location[r,"SYSTEM_SITE_corrected"]
        }else{
          fileds_values_l[[v_i]] <- nuseds_location[r,v]
        }
      }
      v_i <- v_i + 1
    }
    names(fileds_values_l) <- fields_locations
    
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
print(paste("Proportion of locations not matching:",count_percent,"%")) # 1.3%
# View(nuseds_location)

cond_noMatch <- is.na(nuseds_location$pointid)
sum(cond_noMatch) # 31
nuseds_location[cond_noMatch,c("SYSTEM_SITE","GFE_ID","X_LONGT","Y_LAT","comment")]



#'* Locations not found --> Need of new locations: *

cond_noMatch <- is.na(nuseds_location$pointid)
sum(cond_noMatch) # 67
nuseds_location[cond_noMatch,c("SYSTEM_SITE","GFE_ID","X_LONGT","Y_LAT","comment")]

SYSTEM_SITE_notFound <- c()


location <- "LOWER YUKON RIVER"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

location <- "RAILWAY CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

location <- "KAWKAWA LAKE"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

location <- "MENZ CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

location <- "KOPP CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

location <- "SEVEN MILE CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

location <- "SKEENA RIVER"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

location <- "CLEARWATER CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

location <- "SWIFT RIVER"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

location <- "HARRIS CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

location <- "TAGETOCHLAIN CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

location <- "NAHOUNLI CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)

#
location <- "QUESNEL LAKE NORTH ARM - ADAMS CREEK SHORE"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T) & is.na(nuseds_location$distance)
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm","Y_LAT","X_LONGT","distance")])
location <- "QUESNEL"
location <- "ADAMS"
cond1 <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond1,]

distance_Euclidean_fun(x_ref = unique(nuseds_location[cond,]$X_LONGT)[1],
                       y_ref = unique(nuseds_location[cond,]$Y_LAT)[1],
                       x = unique(streamlocationids[cond1,]$longitude),
                       y = unique(streamlocationids[cond1,]$latitude))

streamlocationids[cond1,][which(dist == min(dist)),]

#
location <- "LAGOON CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T) & is.na(nuseds_location$distance)
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm","Y_LAT","X_LONGT","distance")])
location <- "LAGOON CREEK"
cond1 <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond1,]

dist <- distance_Euclidean_fun(x_ref = unique(nuseds_location[cond,]$X_LONGT)[1],
                               y_ref = unique(nuseds_location[cond,]$Y_LAT)[1],
                               x = unique(streamlocationids[cond1,]$longitude),
                               y = unique(streamlocationids[cond1,]$latitude))

streamlocationids[cond1,][which(dist == min(dist)),]

#' 3rd green river but distance > 110 km
location <- "GREEN RIVER"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm","Y_LAT","X_LONGT","distance")])
location <- "GREEN RIVER"
cond1 <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond1,]

distance_Euclidean_fun(x_ref = unique(nuseds_location[cond,]$X_LONGT)[1],
                       y_ref = unique(nuseds_location[cond,]$Y_LAT)[1],
                       x = unique(streamlocationids[cond1,]$longitude),
                       y = unique(streamlocationids[cond1,]$latitude))

#'1) # the name match perfectly but the distance is > 150 km
location <- "COLLISON BAY CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm","Y_LAT","X_LONGT","distance")])
location <- "COLLISON"
cond1 <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond1,]

distance_Euclidean_fun(x_ref = unique(nuseds_location[cond,]$X_LONGT),
                       y_ref = unique(nuseds_location[cond,]$Y_LAT),
                       x = unique(streamlocationids[cond1,]$longitude),
                       y = unique(streamlocationids[cond1,]$latitude))

#'1) "YUKON RIVER"
location <- "YUKON RIVER"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm","Y_LAT","X_LONGT","distance")])
location <- "YUKON RIVER"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "CAMP SLOUGH" --> not "CAMP SLOUGH" because distance is way too far
location <- "CAMP SLOUGH"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "CAMP CREEK"
location <- "CAMP-LAKE"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "FENNELL CREEK AND SASKUM CREEK"
location <- "FENNELL CREEK AND SASKUM CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "FENNELL"
location <- "SASKUM"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "LATIMER CREEK"
location <- "LATIMER CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "LATIMER"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "MILLIONAIRE CREEK"
location <- "MILLIONAIRE CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "MILLIONAIRE"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "STILIQUE CREEK"
location <- "STILIQUE CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "STILIQUE"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1)  "DRY CREEK"
location <- "DRY CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "DRY"
cond1 <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond1,]

distance_Euclidean_fun(x_ref = unique(nuseds_location[cond,]$X_LONGT),
                       y_ref = unique(nuseds_location[cond,]$Y_LAT),
                       x = unique(streamlocationids[cond1,]$longitude),
                       y = unique(streamlocationids[cond1,]$latitude))


#'1)  "JUDD SLOUGH"
location <- "JUDD SLOUGH"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "JUDD"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "PIPER CREEK"
location <- "PIPER CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "PIPER"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "TIEMPO SPAWNING CHANNEL"
location <- "TIEMPO SPAWNING CHANNEL"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE,fixed = T)
unique(nuseds_location[cond,c("LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "TIEMPO"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) WATERSHED ABOVE STAMP FALLS
location <- "WATERSHED ABOVE STAMP FALLS"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE)
unique(nuseds_location[cond,c("SYSTEM_SITE","sys_nm","Y_LAT","X_LONGT","distance")])
cond <- grepl(location,streamlocationids$sys_nm)
streamlocationids[cond,]

#'1) KINGKOWN INLET CREEKS --> there is no KINGKOWN
location <- "KINGKOWN INLET CREEKS"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE)
unique(nuseds_location[cond,c("sys_nm","Y_LAT","X_LONGT","distance")])
cond <- grepl(location,streamlocationids$sys_nm)
cond <- grepl(tolower(location),streamlocationids$sys_nm_simple)
streamlocationids[cond,]

#'3) STRANDBY RIVER
location <- "STRANDBY RIVER"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE)
unique(nuseds_location[cond,c("sys_nm","Y_LAT","X_LONGT","distance")])
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'2) BORROWMAN CREEK, BORROWMAN is not in database
location <- "BORROWMAN CREEK"
SYSTEM_SITE_notFound <- c(SYSTEM_SITE_notFound,location)
cond <- grepl(location,nuseds_location$SYSTEM_SITE)
unique(nuseds_location[cond,c("SYSTEM_SITE","sys_nm","Y_LAT","X_LONGT","distance")])
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

# remaining unmatches: (should be 0)
cond_noMatch <- cond_noMatch & !nuseds_location$SYSTEM_SITE %in% SYSTEM_SITE_notFound
nuseds_location[cond_noMatch,c("SYSTEM_SITE","GFE_ID","X_LONGT","Y_LAT","comment")]


#'* 3) find streamid *
#' use 

nuseds_location

streamlocationids

fields_nuseds_CU
fields_nuseds_location

nuseds_cuid_location <- unique(nuseds[,c("regionid","regionid","cuid",
                                         fields_nuseds_CU,
                                         fields_nuseds_location)])

nrow(nuseds_cuid_location) # 6848

nuseds$pointid <- NA
nuseds$sys_nm <- NA
nuseds$streamid <- NA
nuseds$latitude <- NA
nuseds$longitude <- NA
nuseds$distance <- NA

cuid_nuseds_toCheck <- c()

figures_print <- F
figures_show <- F

count <- 0
count_show <- 1
for(r in 1:nrow(nuseds_cuid_location)){
  # r <- 1
  
  cuid_here <- nuseds_cuid_location$cuid[r]
  SYSTEM_SITE_here <- nuseds_cuid_location$SYSTEM_SITE[r]
  GFE_ID_here <- nuseds_cuid_location$GFE_ID[r]
  
  #' 1) Find the location in nuseds_location and return corresponding pointid and
  #' sys_nm, latitude and longitude
  cond_nuseds_location <- nuseds_location$SYSTEM_SITE == SYSTEM_SITE_here &
    nuseds_location$GFE_ID == GFE_ID_here
  
  pointid_here <- unique(nuseds_location$pointid[cond_nuseds_location])
  sys_nm_here <- unique(nuseds_location$sys_nm[cond_nuseds_location])
  latitude_here <- unique(nuseds_location$latitude[cond_nuseds_location])
  longitude_here <- unique(nuseds_location$longitude[cond_nuseds_location])
  distance_here <- unique(nuseds_location$distance[cond_nuseds_location])
  
  # check
  if(length(pointid_here) > 1){
    print("more than one pointid here where it should not be")
    break
  }

  #' 2) Find streamid if possible
  if(!is.na(pointid_here) & !is.na(cuid_here)){
    
    cond_streamlocationids <- streamlocationids$sys_nm == sys_nm_here &
      streamlocationids$pointid == pointid_here &
      streamlocationids$cuid == cuid_here
    
    if(sum(cond_streamlocationids) > 0){
      streamid_here <- streamlocationids$streamid[cond_streamlocationids]
      
    }else{
      
      streamid_here <- NA
      
      #' 1) look if there are alternative pointids
      pointid_alternative_here <- unique(nuseds_location$pointid_alternative[cond_nuseds_location])
      
      if(!is.na(pointid_alternative_here)[1]){
        
        pointid_alternative_here <- unlist(strsplit(pointid_alternative_here,", "))
        pointid_alternative_here <- as.numeric(pointid_alternative_here)
        
        cond_streamlocationids_l <- lapply(pointid_alternative_here,function(pid){
          cond_pid <- streamlocationids$pointid == pid
          sys_nm_here_here <- unique(streamlocationids$sys_nm[cond_pid])
          #sys_nm_here_here <- sys_nm_here_here[sys_nm_here_here!= sys_nm_here] # just in case
          
          out <- streamlocationids$sys_nm == sys_nm_here_here &
            streamlocationids$pointid == pid &
            streamlocationids$cuid == cuid_here
          return(out)
        })
        
        
        TODO: pick one point
        
        cond_pointid_alternative <- sapply(cond_streamlocationids_l,any)

        SYSTEM_SITE_here
        pointid_here
        cuid
        streamlocationids[cond_streamlocationids_l[[1]],]
        streamlocationids[cond_streamlocationids_l[[2]],]
        
        cond <- streamlocationids$pointid %in% c(358,359,357)
        streamlocationids[cond,]
        
        if(sum(cond_pointid_alternative) == 1){
          
          cond_streamlocationids <- cond_streamlocationids_l[cond_pointid_alternative][[1]]
          
          pointid_here <- streamlocationids$pointid[cond_streamlocationids]
          sys_nm_here <- streamlocationids$sys_nm[cond_streamlocationids]
          latitude_here <- streamlocationids$latitude[cond_streamlocationids]
          longitude_here <- streamlocationids$longitude[cond_streamlocationids]
          streamid_here <- streamlocationids$streamid[cond_streamlocationids]
          
          distance_here <- distance_Euclidean_fun(x_ref = nuseds_cuid_location$X_LONGT[r],
                                                  y_ref = nuseds_cuid_location$Y_LAT[r],
                                                  x = streamlocationids$longitude[cond_streamlocationids],
                                                  y = streamlocationids$latitude[cond_streamlocationids])
          
        }else if(sum(cond_pointid_alternative) > 1){
          print("Multiple match with pointid_alternative, which is not normal")
          break
        }
      }
      
      #' 2) if still no match, try matching with 1st word
      if(sum(cond_streamlocationids) == 0){
        
        SYSTEM_SITE_here_word1 <- strsplit(SYSTEM_SITE_here," ")[[1]][1]
        cond <- streamlocationids$cuid == cuid_here & 
          grepl(simplify_string_fun(SYSTEM_SITE_here_word1),streamlocationids$sys_nm_simple)
        
        if(sum(cond) > 0){
          
          # streamlocationids[cond,]
          distances <- distance_Euclidean_fun(x_ref = nuseds_cuid_location$X_LONGT[r],
                                              y_ref = nuseds_cuid_location$Y_LAT[r],
                                              x = streamlocationids$longitude[cond],
                                              y = streamlocationids$latitude[cond])
          
          # filter with distance_threshold
          cond_distance <- distances < distance_threshold 
          cond_new <- rep(F,length(cond)) 
          cond_new[which(cond)[cond_distance]] <- T
          cond <- cond_new
          distances <- distances[cond_distance]
          
          if(length(distances) > 1){
            print("*** BREAK ***")
            break
            
          }else{
            
            distance_here <- distances
            
            cond_streamlocationids <- cond
            
            pointid_here <- streamlocationids$pointid[cond_streamlocationids]
            sys_nm_here <- streamlocationids$sys_nm[cond_streamlocationids]
            latitude_here <- streamlocationids$latitude[cond_streamlocationids]
            longitude_here <- streamlocationids$longitude[cond_streamlocationids]
            streamid_here <- streamlocationids$streamid[cond_streamlocationids]
            
            print(paste0("Matched with 1st word at r ",r," - ",SYSTEM_SITE_here," - ",sys_nm_here," - ",round(distance_here,4)))
            
          }
        }
      }
      streamid_here <- NA
    }
    
  }else{
    streamid_here <- NA
  }

  # check
  if(length(streamid_here) > 1){
    print("more than one streamid_here here where it should not be")
    break
  }
  
  #' 3) Fill nuseds
  if(is.na(cuid_here)){
    cond_nuseds_cuid <- is.na(nuseds$cuid)
  }else{
    cond_nuseds_cuid <- nuseds$cuid == cuid_here &  !is.na(nuseds$cuid)
  }
  cond_nuseds <- nuseds$SYSTEM_SITE == SYSTEM_SITE_here &
    nuseds$GFE_ID == GFE_ID_here & 
    cond_nuseds_cuid
  
  # sort(nuseds$Year[cond_nuseds])
  # unique(nuseds$SYSTEM_SITE[cond_nuseds])
  
  # check
  if(any(duplicated(sort(nuseds$Year[cond_nuseds]))) & !is.na(cuid_here)){
    # 
    print("duplicated years where it should not be")
    
    if(figures_print){
      jpeg(paste0(wd_figures,"/Tocheck_cuid_",cuid_here,"_r_",r,".jpeg"), 
           width = 20, height = 20, units = "cm", res = 200)
    }
    if(figures_show){
      plot_IndexId_GFE_ID_fun(IndexIds = unique(nuseds$IndexId[cond_nuseds]),
                              GFE_IDs = rep(unique(nuseds$GFE_ID[cond_nuseds]),length(unique(nuseds$IndexId[cond_nuseds]))),
                              all_areas_nuseds = nuseds)
      legend("top",legend = paste0("cuid = ",cuid_here,", r = ",r),bty = 'n')
    }
    if(figures_show){
      dev.off()
    }
    cuid_nuseds_toCheck <- c(cuid_nuseds_toCheck,cuid_here)
  }
  
  nuseds$pointid[cond_nuseds] <- pointid_here
  nuseds$sys_nm[cond_nuseds] <- sys_nm_here
  nuseds$streamid[cond_nuseds] <- streamid_here
  nuseds$latitude[cond_nuseds] <- latitude_here
  nuseds$longitude[cond_nuseds] <- longitude_here
  nuseds$distance[cond_nuseds] <- distance_here
  
  count <- count + 1
  count_percent <- round(count/nrow(nuseds_cuid_location)*100,1)
  if(count_show <= count_percent){
    print(paste("Proportion of locations not matching:",count_percent,"%"))
    count_show <- count_show + 1
  }
}

nuseds_final <- nuseds

cond_cuid_na <- is.na(nuseds_final$cuid)
cond_pointid_na <- is.na(nuseds_final$pointid)
cond_streamid_na <- is.na(nuseds_final$streamid)
cond_noNa <- !cond_cuid_na & !cond_pointid_na & !cond_streamid_na

sum(cond_cuid_na) / nrow(nuseds_final) * 100     # 0.65%
sum(cond_pointid_na) / nrow(nuseds_final) * 100  # 0.84%
sum(cond_streamid_na) / nrow(nuseds_final) * 100  # 6.1%
sum(cond_noNa) / nrow(nuseds_final) * 100     # 93.9%


# remove transboundary data
unique(nuseds$region)
nuseds <- nuseds[nuseds$region != "Transboundary",]


write.csv(nuseds_final,paste0(wd_output,"/nuseds_cuid_streamid.csv"),
          row.names = F)



#
# Older stuff -----

#'** CHECKS TEMPORARY **
#' 1) Compare surveystreams$stream_name_pse and streamspawnersurveys_output$stream_name_pse
#' with streamlocationids$sys_nm --> they are the same after simplifying the strings.
#' 2) compare nuseds$SYSTEM_SITE with stream_name_pse --> they are not necessarily
#' the same.

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

col_nuseds <- c("cuid","cu_name_pse","SPECIES_QUALIFIED","FULL_CU_IN","POP_ID","CU_NAME","GFE_ID",
                "SYSTEM_SITE","WATERBODY","X_LONGT","Y_LAT")

nuseds_cuid_location <- unique(nuseds[,c("regionid","CU_NAME","cu_name_pse","SPECIES_QUALIFIED","cuid",
                                         "SYSTEM_SITE","WATERBODY",
                                         "GAZETTED_NAME","LOCAL_NAME_1","LOCAL_NAME_2",
                                         "GFE_ID","X_LONGT","Y_LAT")]) # inclide GFE_ID because there can be multiple SYSTEM_SITEs for a same CU
nrow(nuseds_cuid_location) # 6848

# simplify the location name fields:
fields_locations <- c("SYSTEM_SITE","WATERBODY","GAZETTED_NAME",
                      "LOCAL_NAME_1","LOCAL_NAME_2")
# Manual fix 1st:
#'-  "BARRI\xc8RE RIVER" --> "BARRIERE RIVER"
#'- "FRAN\xc7OIS LAKE" --> ""FRANCOIS LAKE"
#'
cond <- nuseds_cuid_location$SYSTEM_SITE == "BARRIERE RIVER"
nuseds_cuid_location$LOCAL_NAME_1[cond] <- "BARRIERE RIVER"
cond <- nuseds_cuid_location$SYSTEM_SITE == "EAST BARRIERE RIVER"
nuseds_cuid_location$LOCAL_NAME_1[cond] <- "EAST BARRIERE RIVER"
cond <- nuseds_cuid_location$SYSTEM_SITE == "FRANCOIS LAKE"
nuseds_cuid_location$LOCAL_NAME_1[cond] <- "FRANCOIS LAKE"

# Simplify the fields_locations
# and remove the "s" "creeks"
for(f in fields_locations){
  # f <- fields_locations[1]
  nuseds_cuid_location$X <- simplify_string_fun(nuseds_cuid_location[,f])
  nuseds_cuid_location$X <- gsub("creeks","creek",nuseds_cuid_location$X)
  colnames(nuseds_cuid_location)[colnames(nuseds_cuid_location) == "X"] <- paste0(f,"_simple")
}

nuseds_cuid_location$streamid <- NA
nuseds_cuid_location$field_nuseds_used <- NA
nuseds_cuid_location$sys_nm <- NA            # to record which sys_nm was used
nuseds_cuid_location$distance <- NA # the euclidean distance between the SYSTEM_SITE and sys_nm
nuseds_cuid_location$comment <- NA

streamlocationids$sys_nm_simple <- simplify_string_fun(streamlocationids$sys_nm)
streamlocationids$sys_nm_simple <- gsub("creeks","creek",streamlocationids$sys_nm_simple)
streamlocationids$taken <- NA

# options(warn = 0)
# options(warn=1)  # print warnings as they occur
options(warn = 2)  # treat warnings as errors

#'* 1) 1st fill all the one with perfect match for location names and/or coordinates *
#' Do not try to trouble shoot the other ones yet because removing the perfect 
#' match 1st remove options when multiple matches are available when using grepl
#' (which is used for trouble shooting).
# add the coordinate field
fields_locations <- c(fields_locations,"COORDINATES")

count <- 0
count_show <- 1

# streamlocationids_copy <- streamlocationids
# streamlocationids <- streamlocationids_copy

cond <- is.na(nuseds_cuid_location$cuid) 
nuseds_cuid_location$comment[cond] <- "cuid not in the data base"

distance_threshold <- 1
for(r in 1:nrow(nuseds_cuid_location)){
  # r <- 1 # case with no match
  # r <- 15 case only coordinates match
  # r <- 381 case with multiple spatial coordinates for a same location (because several GFE_ID correspond to a same SYSTEM_SITE)
  
  # if(nuseds_cuid_location$cuid[r] == 811 & nuseds_cuid_location$SYSTEM_SITE[r] == "HARRIET CREEK"){
  #   break
  # }
  if(!is.na(nuseds_cuid_location$cuid[r])){

    fileds_values_l <- list()
    v_i <- 1
    for(v in c("cuid",fields_locations)){
      if(v == "COORDINATES"){
        X_LONGT <- round(nuseds_cuid_location$X_LONGT[r],4)
        Y_LAT <- round(nuseds_cuid_location$Y_LAT[r],4)
        fileds_values_l[[v_i]] <- list(X_LONGT,Y_LAT)
        names(fileds_values_l[[v_i]]) <- c("X_LONGT","Y_LAT")
      }else{
        fileds_values_l[[v_i]] <- nuseds_cuid_location[r,v]
      }
      v_i <- v_i + 1
    }
    names(fileds_values_l) <- c("cuid",fields_locations)
    
    conditions_l <- list()
    v_i <- 1
    for(v in c("cuid",fields_locations)){
      if(v == "cuid"){
        conditions_l[[v_i]] <- streamlocationids$cuid == fileds_values_l[[v]] 
      }else if(v == "COORDINATES"){
        conditions_l[[v_i]] <- round(streamlocationids$longitude,4) == fileds_values_l[[v]]$X_LONGT &
          round(streamlocationids$latitude,4) == fileds_values_l[[v]]$Y_LAT
      }else{
        conditions_l[[v_i]] <- streamlocationids$sys_nm_simple == simplify_string_fun(fileds_values_l[[v]])
      }
      v_i <- v_i + 1
    }
    names(conditions_l) <- c("cuid",fields_locations)
    
    conditions_cuid_l <- lapply(conditions_l[fields_locations],
                                  function(f){conditions_l$cuid & f})
    
    fields_used <- unlist(lapply(conditions_cuid_l,function(f){sum(f) > 0}))
    
    comment <- NULL
    
    # if there are matches with cuid & certain spatial fields:
    if(sum(fields_used) > 0){
      
      # indicate which nuseds field(s) was used
      nuseds_cuid_location$field_nuseds_used[r] <- paste(fields_locations[fields_used], 
                                                         collapse = " ")
      
      # select condition
      cond_toUse <- conditions_cuid_l[fields_used][[1]]
      
      # calculate the distance
      distances <- distance_Euclidean_fun(x_ref = fileds_values_l$COORDINATES$X_LONGT,
                                          y_ref = fileds_values_l$COORDINATES$Y_LAT, 
                                          x = round(streamlocationids$longitude[cond_toUse],4),
                                          y = round(streamlocationids$latitude[cond_toUse],4))
      
      # filter distance with distance_threshold
      distances_filtered <- distances[distances < distance_threshold]
      noMoreMatchAfterFiltering <- length(distances) > 0 & length(distances_filtered) == 0
      if(noMoreMatchAfterFiltering){
        comment <- paste0(comment,paste0("Not matched because distance > ",distance_threshold),sep = " ; ")
        nuseds_cuid_location$comment[r] <- comment
        # nuseds_cuid_location$streamid[r] <- -99    # should remain NAs for the next step
        cond_toUse <- rep(F,nrow(streamlocationids))
        
        count <- count + 1
        count_percent <- round(count/nrow(nuseds_cuid_location)*100,1)
        if(count_show <= count_percent){
          print(paste("Proportion not matching:",count_percent,"%"))
          count_show <- count_show + 1
        }

      }else{
        # only keep the conditions associated with the non-filtered locations
        cond_toUse_copy <- rep(FALSE,length(cond_toUse))
        toKeep <- which(cond_toUse)[distances %in% distances_filtered]
        cond_toUse_copy[toKeep] <- TRUE
        cond_toUse <- cond_toUse_copy
        distances <- distances_filtered
      }
    
      # in case there are more than one option
      if(sum(cond_toUse) > 1){
        
        #' Select the closest location: edit cond_toUse so it only retain the row 
        #' with the smallest distance
        cond_toUse_copy <- rep(FALSE,length(cond_toUse))
        toKeep <- which(cond_toUse)[distances == min(distances)]
        cond_toUse_copy[toKeep] <- TRUE
        cond_toUse <- cond_toUse_copy
        distance <- distances[distances == min(distances)] 
        comment <- paste0(comment,"More than one match so we picked the closest location",sep = " ; ")
        
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
        print("*** STILL MORE THAN ONE CHOICE - TO FIX ***")
        print(nuseds_cuid_location[r,])
        break
      }
      
      # fill streamlocationids if there is only one option
      if(sum(cond_toUse) == 1){
        
        if(is.null(comment)){
          comment <- ""
        }
        
        # find the corresponding streamid:
        streamid_here <- streamlocationids$streamid[cond_toUse]
        
        nuseds_cuid_location$streamid[r] <- streamid_here
        nuseds_cuid_location$sys_nm[r] <- streamlocationids$sys_nm[cond_toUse]
        nuseds_cuid_location$distance[r] <- distance
        nuseds_cuid_location$comment[r] <- comment
        
        streamlocationids$taken[cond_toUse] <- r
        
      }
    }else{ # if sum(fields_used) == 0 initially
      
      # 1st try without cuid and use regionid instead, which would mean we need to 
      # create a new streamid
      fileds_values_l$regionid <- nuseds_cuid_location$regionid[r]
      conditions_l$regonid <- streamlocationids$regionid == fileds_values_l$regionid
      
      conditions_regionid_l <- lapply(conditions_l[fields_locations],
                                        function(f){conditions_l$regonid & f})
      
      # indicate which nuseds field(s) was used
      fields_used <- unlist(lapply(conditions_regionid_l,function(f){sum(f) > 0}))
      nuseds_cuid_location$field_nuseds_used[r] <- paste(fields_locations[fields_used], 
                                                         collapse = " ")
      
      if(sum(fields_used) > 0){ # if there is at least one match
        
        # select condition(s)
        cond_toUse <- conditions_regionid_l$SYSTEM_SITE
        for(f in fields_locations[2:length(fields_locations)]){
          cond_toUse <- cond_toUse | conditions_regionid_l[[f]]
        }
        
        # calculate the distance
        distances <- distance_Euclidean_fun(x_ref = X_LONGT, y_ref = Y_LAT, 
                                            x = round(streamlocationids$longitude[cond_toUse],4),
                                            y = round(streamlocationids$latitude[cond_toUse],4))
        
        # filter distance with distance_threshold
        distances_filtered <- distances[distances < distance_threshold]
        noMoreMatchAfterFiltering <- length(distances) > 0 & length(distances_filtered) == 0
        if(noMoreMatchAfterFiltering){
          comment <- paste0(comment,paste0("Not matched because distance > ",distance_threshold),sep=" ; ")
          nuseds_cuid_location$comment[r] <- comment
          # nuseds_cuid_location$streamid[r] <- -99    # should remain NAs for the next step
          cond_toUse <- rep(F,nrow(streamlocationids))
          
          count <- count + 1
          count_percent <- round(count/nrow(nuseds_cuid_location)*100,1)
          if(count_show <= count_percent){
            print(paste("Proportion not matching:",count_percent,"%"))
            count_show <- count_show + 1
          }
          
        }else{
          # only keep the conditions associated with the non-filtered locations
          cond_toUse_copy <- rep(FALSE,length(cond_toUse))
          toKeep <- which(cond_toUse)[distances %in% distances_filtered]
          cond_toUse_copy[toKeep] <- TRUE
          cond_toUse <- cond_toUse_copy
          distances <- distances_filtered
          
          if(length(distances) > 1){
            #' Select the closest location: edit cond_toUse so it only retain the row 
            #' with the smallest distance
            cond_toUse_copy <- rep(FALSE,length(cond_toUse))
            toKeep <- which(cond_toUse)[distances == min(distances)][1] # [1] in case there are duplicated minimal values
            cond_toUse_copy[toKeep] <- TRUE
            cond_toUse <- cond_toUse_copy
            distance <- distances[distances == min(distances)][1]
            
          }else{
            distance <- distances
          }
          
          # fill nuseds_cuid_location
          nuseds_cuid_location$streamid[r] <- -99
          nuseds_cuid_location$sys_nm[r] <- unique(streamlocationids$sys_nm[cond_toUse])
          nuseds_cuid_location$distance[r] <- unique(distance) # there can be several identical distance if the same location was selected for multiple cuids
          nuseds_cuid_location$comment[r] <- paste0(comment,"Location exist but no streamid for its assocoation with this cuid",sep = " ; ")
          
        }
        
      }else{ # if there is still no match when considering regionid instead of cuid: just signal it for now
        
        count <- count + 1
        count_percent <- round(count/nrow(nuseds_cuid_location)*100,1)
        if(count_show <= count_percent){
          print(paste("Proportion not matching:",count_percent,"%"))
          count_show <- count_show + 1
        }
      }
    }
  }
}
print(paste("Final proportion not matching:",count_percent,"%")) # 5.3 %
table(nuseds_cuid_location$comment)
table(nuseds_cuid_location$field_nuseds_used)

col <- c("regionid","SPECIES_QUALIFIED","cuid","cu_name_pse","SYSTEM_SITE","field_nuseds_used","sys_nm","distance" )

#' Matches & not matched: Proportion and nb.:
streamid_na <- is.na(nuseds_cuid_location$streamid)
streamid_99 <- nuseds_cuid_location$streamid == -99 & !streamid_na
(sum(!streamid_na) - sum(streamid_99)) / nrow(nuseds_cuid_location) # 0.92
sum(!streamid_na) - sum(streamid_99)  # 6329
sum(streamid_na) / nrow(nuseds_cuid_location) # 0.05
sum(streamid_na)  # 457

#' Missing streamid: 
#' - proportion and number
#' - distribution of distances for those where COORDINATES was not used
#' --> there are two with distance > 1.0 --> might not be the right location.
sum(streamid_99) # 55
sum(streamid_99) / nrow(nuseds_cuid_location) # 0.008
nuseds_cuid_location_99 <- nuseds_cuid_location[streamid_99,]
table(nuseds_cuid_location_99$field_nuseds_used)
streamid_99_noCoord <- !grepl("COORDINATES",nuseds_cuid_location_99$field_nuseds_used)
# hist(nuseds_cuid_location_99[streamid_99_noCoord,]$distance)
# Cases where COORDINATES was not used:
nuseds_cuid_location_99[streamid_99_noCoord,col]
#' - for cuid 504 Bella Coola-Dean Rivers: 
#' https://salmonwatersheds.slack.com/archives/C017N5NSCJY/p1683774240661029?thread_ts=1683735939.696999&cid=C017N5NSCJY
# Cases where the distance is > 0.1 ~9km --> could be that multiple sites with same name:
nuseds_cuid_location_99[streamid_99_noCoord,c(col,"Y_LAT","X_LONGT")][nuseds_cuid_location_99$distance[streamid_99_noCoord] > 0.1,]
cond <- streamlocationids$sys_nm == "GREEN RIVER"
unique(streamlocationids[cond,c('latitude','longitude')])
#' GREEN RIVER 
#' - NUSEDS:            52.33052 -127.4542
#' - streamlocationids: 52.99582 -128.3810
cond <- streamlocationids$sys_nm == "TATSATUA RIVER"
unique(streamlocationids[cond,c('latitude','longitude')])
cond <- streamlocationids$sys_nm == "RAINBOW CREEK"
unique(streamlocationids[cond,c('latitude','longitude')])

#' Look at comment = 'More than one match so we picked the closest location'
cond <- nuseds_cuid_location$comment == 'More than one match so we picked the closest location' &
  !is.na(nuseds_cuid_location$comment)
nuseds_cuid_location[cond,col]
cond <- streamlocationids$sys_nm == "HARRIET CREEK" & streamlocationids$cuid == 811
streamlocationids[cond,]

#' Look at the matches that were due to COORDINATES only and compare the location 
#' names --> HOW WERE THESE DISCREPANCIES CREATED?
cond <- nuseds_cuid_location$field_nuseds_used == "COORDINATES" & 
  !is.na(nuseds_cuid_location$field_nuseds_used)
nuseds_cuid_location[cond,c("SPECIES_QUALIFIED","cuid","SYSTEM_SITE","WATERBODY",
                            "GFE_ID","sys_nm")]

#' Check the distances for the matches done without coordinate
#' QUESTIONS: THESE ARE PRETTY LARGE DISTANCES NO?
cond <- !grepl("COORDINATES",nuseds_cuid_location$field_nuseds_used) &
  !streamid_na & 
  !streamid_99
sum(cond) # 6150
sum(cond)/nrow(nuseds_cuid_location)
hist(nuseds_cuid_location$distance[!cond], breaks = 100, xlim = c(0,0.5))
range(nuseds_cuid_location$distance[!cond],na.rm = T)
nuseds_cuid_location[cond & nuseds_cuid_location$distance > 0.1,col]
sum(cond & nuseds_cuid_location$distance > 0.1)


#'* 2) use grepl for the matching of the remaining data *
#' use spatial coordinate only to find the closest location when multiple options.
#' Check that the streamid has not been already taken in the section above.

# manual fixes
location_names_fixes <- SYSTEM_SITE_fixes_fun()
SYSTEM_SITE_fixes <- location_names_fixes$SYSTEM_SITE
sys_nm_fixes <- location_names_fixes$sys_nm
rm(location_names_fixes)

#' there is no streamid associated to cuid 215 for this location. But the 
#' code will try to match it using GAZETTED_NAME = "BABINE RIVER", which 
#' is a value name of another location.
exception <- fileds_values_l$cuid == 215 & 
  fileds_values_l$SYSTEM_SITE == "BABINE RIVER - SECTION 1-3"

# Here GAZETTED_NAME = "ANDERSON LAKE", which wrongly match another location
exception <- c(exception,
               fileds_values_l$cuid == 719 &
                 fileds_values_l$SYSTEM_SITE == "LOST VALLEY CREEK - SHORE SPAWNERS")

distance_threshold <- 1
count <- 0
count_show <- 1
for(r in 1:nrow(nuseds_cuid_location)){
  # r <- 3307

  if(!is.na(nuseds_cuid_location$cuid[r]) & is.na(nuseds_cuid_location$streamid[r])){
    
    # if(grepl("#1 CREEK",nuseds_cuid_location$SYSTEM_SITE[r])){
    #   break
    # }
    # if("SWAN LAKE CREEK #2 UNNAMED" == nuseds_cuid_location$SYSTEM_SITE[r]){
    #   break
    # }

    fileds_values_l <- list()
    v_i <- 1
    for(v in c("cuid",fields_locations)){
      if(v == "COORDINATES"){
        X_LONGT <- round(nuseds_cuid_location$X_LONGT[r],4)
        Y_LAT <- round(nuseds_cuid_location$Y_LAT[r],4)
        fileds_values_l[[v_i]] <- list(X_LONGT,Y_LAT)
        names(fileds_values_l[[v_i]]) <- c("X_LONGT","Y_LAT")
      }else{
        fileds_values_l[[v_i]] <- nuseds_cuid_location[r,v]
      }
      v_i <- v_i + 1
    }
    names(fileds_values_l) <- c("cuid",fields_locations)
    
    conditions_l <- list()
    v_i <- 1
    for(v in c("cuid",fields_locations)){
      if(v == "cuid"){
        conditions_l[[v_i]] <- streamlocationids$cuid == fileds_values_l[[v]] 
      }else if(v == "COORDINATES"){ # leave it but there is not match
        conditions_l[[v_i]] <- round(streamlocationids$longitude,4) == fileds_values_l[[v]]$X_LONGT &
          round(streamlocationids$latitude,4) == fileds_values_l[[v]]$Y_LAT
      }else{
        val_here <- simplify_string_fun(fileds_values_l[[v]])
        val_here <- gsub("creeks","creek",val_here)
        if(val_here == ""){
          conditions_l[[v_i]] <- rep(F,nrow(streamlocationids))
        }else{
          conditions_l[[v_i]] <- grepl(val_here,
                                         streamlocationids$sys_nm_simple)
        }
      }
      v_i <- v_i + 1
    }
    names(conditions_l) <- c("cuid",fields_locations)
    
    conditions_cuid_l <- lapply(conditions_l[fields_locations],
                                  function(f){conditions_l$cuid & f})
    
    fields_used <- unlist(lapply(conditions_cuid_l,function(f){sum(f) > 0}))
    
    comment <- paste0(nuseds_cuid_location$comment[r],"Used grepl()",sep=" ; ")
    
    if(sum(fields_used) == 0 | sum(exception) > 0){
      
      # apply the changes identified by eye
      if(fileds_values_l$SYSTEM_SITE %in% SYSTEM_SITE_fixes){
        
        i <- which(SYSTEM_SITE_fixes == fileds_values_l$SYSTEM_SITE)
        
        #' In the case here below, the correction is not  "BABINE RIVER - SECTION 1-3"
        #' (as it is for other cuids) but 
        if(fileds_values_l$cuid == 215 & fileds_values_l$SYSTEM_SITE == "BABINE RIVER - SECTION 1-3"){
          fileds_values_l$SYSTEM_SITE <- "Babine-Sections 1 To 3" # instead of "Babine River-Sections 1 To 3"
          fileds_values_l$GAZETTED_NAME <- "" # instead of "BABINE RIVER", which will match with another location
          conditions_l$GAZETTED_NAME <- rep(F,nrow(streamlocationids))
          conditions_cuid_l$GAZETTED_NAME <- rep(F,nrow(streamlocationids))
          
        }else{
          fileds_values_l$SYSTEM_SITE <- sys_nm_fixes[i]
        }
        
        # Here GAZETTED_NAME = "ANDERSON LAKE", which wrongly match another location
        if(fileds_values_l$cuid == 719 & fileds_values_l$SYSTEM_SITE == "LOST VALLEY CREEK - SHORE SPAWNERS"){
          fileds_values_l$GAZETTED_NAME <- "" # instead of "ANDERSON LAKE", which will match with another location
          conditions_l$GAZETTED_NAME <- rep(F,nrow(streamlocationids))
          conditions_cuid_l$GAZETTED_NAME <- rep(F,nrow(streamlocationids))
        }
        
        conditions_l$SYSTEM_SITE <- simplify_string_fun(streamlocationids$sys_nm) == simplify_string_fun(fileds_values_l$SYSTEM_SITE)
        conditions_cuid_l$SYSTEM_SITE <- conditions_l$cuid & conditions_l$SYSTEM_SITE
        comment <- paste0(comment," ; SYSTEM_SITE modified manually to match")
        
        fields_used <- unlist(lapply(conditions_cuid_l,function(f){sum(f) > 0}))
        
      }else{ # try extra tricks or individual fixes
        
        cond_SYSTEM_SITE_simple_here <- F
        
        SYSTEM_SITE_here <- fileds_values_l$SYSTEM_SITE
        SYSTEM_SITE_simple_here <- simplify_string_fun(SYSTEM_SITE_here)
        SYSTEM_SITE_simple_here <- gsub("creeks","creek",SYSTEM_SITE_simple_here)
        
        #' try issues with:  RIGHT HAND to RH
        hand <- sapply(X = c("righthand","lefthand"),
                       FUN = function(c){grepl(c,SYSTEM_SITE_simple_here)})
        if(sum(hand) > 0){
          char <- names(hand)[hand]
          char_new <- c("rh","lh")[hand]
          SYSTEM_SITE_simple_here <- gsub(char,char_new,SYSTEM_SITE_simple_here)
          cond_SYSTEM_SITE_simple_here <- grepl(SYSTEM_SITE_simple_here,
                                                streamlocationids$sys_nm_simple)
        }
        
        if(sum(cond_SYSTEM_SITE_simple_here) == 0){
          #' if sill no match, try issues with:  R.H. CREEK --> CREEK R.H.
          hand <- sapply(X = c("rh","lh"),
                         FUN = function(c){grepl(c,SYSTEM_SITE_simple_here)})
          cond <- sum(cond_SYSTEM_SITE_simple_here) == 0 &
            grepl("creek",SYSTEM_SITE_simple_here) &
            sum(hand) > 0
          if(cond){
            char <- paste0(names(hand)[hand],"creek")
            char_new <- paste0("creek",names(hand)[hand])
            SYSTEM_SITE_simple_here <- gsub(char,char_new,SYSTEM_SITE_simple_here)
            cond_SYSTEM_SITE_simple_here <- grepl(SYSTEM_SITE_simple_here,
                                                  streamlocationids$sys_nm_simple)
          }
        }
        
        if(sum(cond_SYSTEM_SITE_simple_here) == 0){
          #' Cases with:
          #1 CREEK --> CREEK #1
          #2 CREEKS --> CREEK #2
          #3 CREEKS --> CREEK #3
          #4 CREEKS --> CREEK #4
          patterns <- c("#1creek","#2creeks","#2creek","#3creeks","#3creek","#4creeks","#4creek")
          patterns_new <- c("creek#1","creek#2","creek#2","creek#3","creek#3","creek#4","creek#4")
          patterns_present <- sapply(patterns,
                                     function(p){grepl(p,SYSTEM_SITE_simple_here)})
          if(sum(patterns_present) > 0){
            pattern <- patterns[patterns_present]
            if(length(pattern) > 1){ # e.g. "#2creeks" "#2creek" --> it is "#2creeks"
              pattern <- pattern[grepl("s",pattern)]
              patterns_present <- sapply(patterns,
                                         function(p){p == pattern})
            }
            pattern_new <- patterns_new[patterns_present]
            SYSTEM_SITE_simple_here <- gsub(pattern,pattern_new,SYSTEM_SITE_simple_here)
            cond_SYSTEM_SITE_simple_here <- grepl(SYSTEM_SITE_simple_here,
                                                  streamlocationids$sys_nm_simple)
          }
        }
        
        if(sum(cond_SYSTEM_SITE_simple_here) == 0){
          #' Cases with:
          patterns <- "headcreek"
          patterns_new <- "creekhead"
          patterns_present <- grepl(patterns,SYSTEM_SITE_simple_here)
          if(sum(patterns_present) > 0){
            pattern <- patterns[patterns_present]
            pattern_new <- patterns_new[patterns_present]
            SYSTEM_SITE_simple_here <- gsub(pattern,pattern_new,SYSTEM_SITE_simple_here)
            cond_SYSTEM_SITE_simple_here <- grepl(SYSTEM_SITE_simple_here,
                                                  streamlocationids$sys_nm_simple)
          }
        }
        
        if(sum(cond_SYSTEM_SITE_simple_here) == 0){
          #' Cases with:
          patterns <- "outercreek"
          patterns_new <- "creekouter"
          patterns_present <- grepl(patterns,SYSTEM_SITE_simple_here)
          if(sum(patterns_present) > 0){
            pattern <- patterns[patterns_present]
            pattern_new <- patterns_new[patterns_present]
            SYSTEM_SITE_simple_here <- gsub(pattern,pattern_new,SYSTEM_SITE_simple_here)
            cond_SYSTEM_SITE_simple_here <- grepl(SYSTEM_SITE_simple_here,
                                                  streamlocationids$sys_nm_simple)
          }
        }
        
        if(sum(cond_SYSTEM_SITE_simple_here) == 0){
          #' Cases with:
          patterns <- paste0("#",c("one","two","three","four","five","six","seven",
                                   "eight","nine"))
          patterns_new <- paste0("#",1:9)
          patterns_present <- sapply(patterns,
                                     function(p){grepl(p,SYSTEM_SITE_simple_here)})
          if(sum(patterns_present) > 0){
            pattern <- patterns[patterns_present]
            pattern_new <- patterns_new[patterns_present]
            SYSTEM_SITE_simple_here <- gsub(pattern,pattern_new,SYSTEM_SITE_simple_here)
            cond_SYSTEM_SITE_simple_here <- grepl(SYSTEM_SITE_simple_here,
                                                  streamlocationids$sys_nm_simple)
          }
        }
        
        if(sum(cond_SYSTEM_SITE_simple_here) == 0){
          #' Cases with:
          patterns <- paste0("creek",1:9)
          patterns_new <- paste0("#",1:9)
          patterns_present <- sapply(patterns,
                                     function(p){grepl(p,SYSTEM_SITE_simple_here,fixed = T)})
          if(sum(patterns_present) > 0){
            pattern <- patterns[patterns_present]
            pattern_new <- patterns_new[patterns_present]
            SYSTEM_SITE_simple_here <- gsub(pattern,pattern_new,SYSTEM_SITE_simple_here)
            cond_SYSTEM_SITE_simple_here <- grepl(SYSTEM_SITE_simple_here,
                                                  streamlocationids$sys_nm_simple)
            
          }
          
          # if still no match it could be a case such as: "CHOKE PASS CREEKS (3)" --> "CHOKE PASS CREEKS"
          if(sum(cond_SYSTEM_SITE_simple_here) == 0){
            patterns <- paste0("#",1:9)
            patterns_new <- rep("",9)
            patterns_present <- sapply(patterns,
                                       function(p){grepl(p,SYSTEM_SITE_simple_here,fixed = T)})
            if(sum(patterns_present) > 0){
              pattern <- patterns[patterns_present]
              pattern_new <- patterns_new[patterns_present]
              SYSTEM_SITE_simple_here <- gsub(pattern,pattern_new,SYSTEM_SITE_simple_here)
              cond_SYSTEM_SITE_simple_here <- grepl(SYSTEM_SITE_simple_here,
                                                    streamlocationids$sys_nm_simple)
              
            }
          }
        }
        
        #
        if(sum(cond_SYSTEM_SITE_simple_here) > 0){
          #fileds_values_l$SYSTEM_SITE <- SYSTEM_SITE_here
          conditions_l$SYSTEM_SITE <- cond_SYSTEM_SITE_simple_here
          conditions_cuid_l$SYSTEM_SITE <- conditions_l$cuid & conditions_l$SYSTEM_SITE
          fields_used <- sapply(fields_used,function(x){F})
          fields_used["SYSTEM_SITE"] <- T
          comment <- paste0(comment," ; SYSTEM_SITE modified manually to match")
        }
      }
    }
    
    # After trying all the possible fixes using cuid
    # if there are matches
    if(sum(fields_used) > 0){
      
      # indicate which nuseds field(s) was used
      nuseds_cuid_location$field_nuseds_used[r] <- paste(fields_locations[fields_used], 
                                                         collapse = " ")
      # select condition
      cond_toUse <- conditions_cuid_l[fields_used][[1]]
      
      # make sure the streamid is not already taken
      streamidTaken_noOtherOption <- sum(cond_toUse) > 0 &
        sum(cond_toUse & is.na(streamlocationids$taken)) == 0
      
      if(streamidTaken_noOtherOption){
        
        if(fileds_values_l$LOCAL_NAME_2 == "UNION PASS CREEK" & fileds_values_l$cuid %in% c(506,520,608,611)){
          #' cuids 506, 520, 608 and 611 have a SYSTEM_SITE == "LAGOON CREEK" and 
          #' LOCAL_NAME_2 == "UNION PASS CREEK", which is a different location
          #' from "LAGOON CREEK" and does not exist in streamlocationids.
          #' A new location should be created.
          
          cond_toUse < rep(F,length(cond_toUse))
          nuseds_cuid_location$comment[r] <- paste0(comment,"Location must be created",sep=";")
          
        }else if(fileds_values_l$SYSTEM_SITE == "BAKER CREEK" & fileds_values_l$cuid %in% c(904)){
          #' there are 3 different BAKER CREEK in streamlocationids.
          #' cuid 904 is present in both on them in nuseds 
          #' But only in one of them in streamlocationids.
          
          cond_toUse < rep(F,length(cond_toUse))
          nuseds_cuid_location$comment[r] <- paste0(comment,"Location exist but no streamid for its assocoation with this cuid", sep = " ; ")
          
        }else{
          # to check if that happens
          print(paste0("*** Match already taken at r = ",r,":"))
          print("Options in streamlocationids:")
          print(streamlocationids[cond_toUse,c("regionid","cuid","sys_nm","latitude",
                                               "longitude","cu_name_pse","taken")])
          print("NuSEDS series in need of a streamid:")
          print(nuseds_cuid_location[r,c("SPECIES_QUALIFIED","cuid","SYSTEM_SITE",
                                         "WATERBODY","GAZETTED_NAME","LOCAL_NAME_1",
                                         "LOCAL_NAME_2","X_LONGT","Y_LAT")])
          print("NuSEDS series that took these streamids:")
          row <- streamlocationids$taken[cond_toUse]
          print(nuseds_cuid_location[row,c("SPECIES_QUALIFIED","cuid","GFE_ID","SYSTEM_SITE",
                                           "WATERBODY","GAZETTED_NAME","LOCAL_NAME_1",
                                           "LOCAL_NAME_2","X_LONGT","Y_LAT","streamid",
                                           "distance","comment")])
          break
          
          # location <- "BAKER CREEK"
          # cond_nuseds <- nuseds_cuid_location$SYSTEM_SITE == location
          # unique(nuseds_cuid_location[cond_nuseds,c("cuid","GFE_ID","SYSTEM_SITE","WATERBODY","GAZETTED_NAME",
          #                 "LOCAL_NAME_1","LOCAL_NAME_2","X_LONGT","Y_LAT","streamid","distance")])
          # 
          # cond_streamid <- simplify_string_fun(streamlocationids$sys_nm) == simplify_string_fun(location)
          # streamlocationids[cond_streamid,c("regionid","cuid","sys_nm","latitude",
          #                                "longitude","cu_name_pse","taken")]
        }
        
      }
      cond_toUse <- cond_toUse & is.na(streamlocationids$taken)
      
      # Make sure the location(s) matched are not too far (e.g. YUKON RIVER)
      if(sum(cond_toUse) > 0){
        
        # calculate the distance
        distances <- distance_Euclidean_fun(x_ref = fileds_values_l$COORDINATES$X_LONGT,
                                            y_ref = fileds_values_l$COORDINATES$Y_LAT, 
                                            x = round(streamlocationids$longitude[cond_toUse],4),
                                            y = round(streamlocationids$latitude[cond_toUse],4))
        
        # filter distance with distance_threshold
        distances_filtered <- distances[distances < distance_threshold]
        noMoreMatchAfterFiltering <- length(distances) > 0 & length(distances_filtered) == 0
        if(noMoreMatchAfterFiltering){
          comment <- paste0(comment,paste0(" ; not matched because distance > ",
                                           distance_threshold))
          nuseds_cuid_location$comment[r] <- comment
          nuseds_cuid_location$streamid[r] <- -99
          cond_toUse <- rep(F,nrow(streamlocationids))
          
        }else{
          # only keep the conditions associated with the non-filtered locations
          cond_toUse_copy <- rep(FALSE,length(cond_toUse))
          toKeep <- which(cond_toUse)[distances %in% distances_filtered]
          cond_toUse_copy[toKeep] <- TRUE
          cond_toUse <- cond_toUse_copy
          distances <- distances_filtered
        }
      }

      # in case there are more than one option
      if(sum(cond_toUse) > 1){
        #' Select the closest location: edit cond_toUse so it only retain the row 
        #' with the smallest distance
        cond_toUse_copy <- rep(FALSE,length(cond_toUse))
        toKeep <- which(cond_toUse)[distances == min(distances)]
        cond_toUse_copy[toKeep] <- TRUE
        cond_toUse <- cond_toUse_copy
        distance <- distances[distances == min(distances)] 
        comment <- paste0(comment,"; More than one match so we picked the closest location")
        
        # just to check
        if(length(distance) > 1){
          print(paste0("*** More than one distance at r = ",r,":"))
          break
        }
        
      }else if(sum(cond_toUse) == 1){
        distance <- distances
      }
      
      # in case there are still more than one option (there should not be)
      if(sum(cond_toUse) > 1){
        print("*** STILL MORE THAN ONE CHOICE - TO FIX ***")
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
        
        streamlocationids$taken[cond_toUse] <- r
      }
      
    }else{ # sum(fields_used) == 0 : if there is still no match --> use region and not cuid
      
      #' If a location was matched but no streamid was found with this cuid -->
      #' --> need to create a new streamid
      #' But check with the distance 1st.
      match_location <- sapply(conditions_l[fields_locations],function(f){sum(f)})
      if(sum(match_location) > 0){
        
        fields_used[match_location > 0] <- T
        cond_toUse <- conditions_l[c(F,fields_used)][[1]]
        
        distances <- distance_Euclidean_fun(x_ref = fileds_values_l$COORDINATES$X_LONGT,
                                            y_ref = fileds_values_l$COORDINATES$Y_LAT, 
                                            x = round(streamlocationids$longitude[cond_toUse],4),
                                            y = round(streamlocationids$latitude[cond_toUse],4))
        distance <- unique(distances[distances == min(distances)])
        
        if(distance < distance_threshold){
          
          nuseds_cuid_location$field_nuseds_used[r] <- paste(fields_locations[fields_used], 
                                                             collapse = " ")
          nuseds_cuid_location$streamid[r] <- -99
          nuseds_cuid_location$sys_nm[r] <- unique(streamlocationids$sys_nm[cond_toUse])
          nuseds_cuid_location$comment[r] <- paste0(comment,"Location exist but no streamid for its assocoation with this cuid", sep = " ; ")
          nuseds_cuid_location$distance[r] <- distance
          
        }else{
          
          nuseds_cuid_location$comment[r] <- paste0(comment," ; Location name exists but not for this cuid and distance > ",distance_threshold)
          nuseds_cuid_location$streamid[r] <- -99
          count <- count + 1
          print(paste(count,r,fileds_values_l$cuid,fileds_values_l$SYSTEM_SITE,sep = " - "))
          
          count_percent <- round(count/nrow(nuseds_cuid_location)*100,1)
          if(count_show <= count_percent){
            print(paste("Proportion not matching:",count_percent,"%"))
            count_show <- count_show + 1
          }
        }
        

      }else{ # location was not matched, regardless of the cuid --> need to create a new locationid and streamid
        
        count <- count + 1
        print(paste(count,r,fileds_values_l$cuid,fileds_values_l$SYSTEM_SITE,sep = " - "))
        
        count_percent <- round(count/nrow(nuseds_cuid_location)*100,1)
        if(count_show <= count_percent){
          print(paste("Proportion not matching:",count_percent,"%"))
          count_show <- count_show + 1
        }
      }
    }
  }
}
print(paste("Proportion not matching:",count_percent,"%")) # 0.6%
r/nrow(nuseds_cuid_location)

col <- c("regionid","SPECIES_QUALIFIED","cuid","cu_name_pse","SYSTEM_SITE",
         "field_nuseds_used","comment")
cond_na <- is.na(nuseds_cuid_location$streamid) & !is.na(nuseds_cuid_location$cuid)
sum(cond_na)
#nuseds_cuid_location[cond_na,col]
rev(sort(table(nuseds_cuid_location$SYSTEM_SITE[cond_na])))


#'1) 
location <- "CAMP SLOUGH"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE,fixed = T)
unique(nuseds_cuid_location[cond,c(col,"LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "CAMP CREEK"
location <- "CAMP-LAKE"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'* cuid not in the databse CHECK IF THE LOCATION ARE? *

cond <- is.na(nuseds_cuid_location$cuid)
sum(cond) # 82
unique(nuseds_cuid_location[cond,c("cuid","CU_NAME","SYSTEM_SITE","Y_LAT","X_LONGT")])
nrow(unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","Y_LAT","X_LONGT")])) # 12

#'* Locations not found --> Need of new locations: *

cond <- is.na(nuseds_cuid_location$streamid) & !is.na(nuseds_cuid_location$cuid)
sum(cond) # 27
unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","Y_LAT","X_LONGT")])
nrow(unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","Y_LAT","X_LONGT")])) # 12

#'1) "CAMP SLOUGH" --> not "CAMP SLOUGH" because distance is way too far
location <- "CAMP SLOUGH"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE,fixed = T)
unique(nuseds_cuid_location[cond,c(col,"LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "CAMP CREEK"
location <- "CAMP-LAKE"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "FENNELL CREEK AND SASKUM CREEK"
location <- "FENNELL CREEK AND SASKUM CREEK"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE,fixed = T)
unique(nuseds_cuid_location[cond,c(col,"LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "FENNELL"
location <- "SASKUM"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "LATIMER CREEK"
location <- "LATIMER CREEK"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE,fixed = T)
unique(nuseds_cuid_location[cond,c(col,"LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "LATIMER"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "MILLIONAIRE CREEK"
location <- "MILLIONAIRE CREEK"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE,fixed = T)
unique(nuseds_cuid_location[cond,c(col,"LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "MILLIONAIRE"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "STILIQUE CREEK"
location <- "STILIQUE CREEK"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE,fixed = T)
unique(nuseds_cuid_location[cond,c(col,"LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "STILIQUE"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1)  "DRY CREEK"
location <- "DRY CREEK"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE,fixed = T)
unique(nuseds_cuid_location[cond,c(col,"LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "DRY"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1)  "JUDD SLOUGH"
location <- "JUDD SLOUGH"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE,fixed = T)
unique(nuseds_cuid_location[cond,c(col,"LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "JUDD"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "PIPER CREEK"
location <- "PIPER CREEK"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE,fixed = T)
unique(nuseds_cuid_location[cond,c(col,"LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "PIPER"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) "TIEMPO SPAWNING CHANNEL"
location <- "TIEMPO SPAWNING CHANNEL"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE,fixed = T)
unique(nuseds_cuid_location[cond,c(col,"LOCAL_NAME_1","sys_nm","Y_LAT","X_LONGT","distance")])
unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","WATERBODY","LOCAL_NAME_1","LOCAL_NAME_2","sys_nm")])
location <- "TIEMPO"
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'1) WATERSHED ABOVE STAMP FALLS
location <- "STAMP"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE)
unique(nuseds_cuid_location[cond,c(col,"sys_nm","Y_LAT","X_LONGT","distance")])
cond <- grepl(location,streamlocationids$sys_nm)
streamlocationids[cond,]

#'1) KINGKOWN INLET CREEKS --> there is no KINGKOWN
location <- "KINGKOWN"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE)
unique(nuseds_cuid_location[cond,c(col,"sys_nm","Y_LAT","X_LONGT","distance")])
cond <- grepl(location,streamlocationids$sys_nm)
cond <- grepl(tolower(location),streamlocationids$sys_nm_simple)
streamlocationids[cond,]

#'3) STRANDBY RIVER
location <- "STRANDBY"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE)
unique(nuseds_cuid_location[cond,c(col,"sys_nm","Y_LAT","X_LONGT","distance")])
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]

#'2) BORROWMAN CREEK, BORROWMAN is not in database
location <- "BORROWMAN"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE)
unique(nuseds_cuid_location[cond,c(col,"distance")])
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]


#'* Locations name found but distance > threshold --> it is a new location with an existing name *

unique(nuseds_cuid_location$comment)

patterns <- c("not matched because distance > 1",
              "Location name exists but not for this cuid and distance > threshold")

cond <- !is.na(nuseds_cuid_location$comment) & 
  !is.na(nuseds_cuid_location$cuid) &
  (grepl(patterns[1],nuseds_cuid_location$comment) | 
     grepl(patterns[2],nuseds_cuid_location$comment))
sum(cond) # 17
unique(nuseds_cuid_location[cond,c("cuid","SYSTEM_SITE","Y_LAT","X_LONGT","sys_nm","distance")])
nrow(unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","Y_LAT","X_LONGT")])) # 5

nuseds_cuid_location$sys_nm

#'1) CLEARWATER CREEK, which is != CLEARWATER RIVER, there is a CLEARWATER CREEK
#' but it is > the threshold (by a lot)
location <- "CLEARWATER CREEK"
cond <- grepl(location,nuseds_cuid_location$SYSTEM_SITE)
unique(nuseds_cuid_location[cond,c(col,"distance","streamid")])
cond <- grepl(location,streamlocationids$sys_nm)
cond <- grepl(simplify_string_fun(location),simplify_string_fun(streamlocationids$sys_nm))
streamlocationids[cond,]


#'* Locations found but it is not associatd with the cuid --> need to create a streamid *

patterns <- c("Location exist but no streamid for its assocoation with this cuid")
cond <- !is.na(nuseds_cuid_location$comment) &
  !is.na(nuseds_cuid_location$cuid) &
  grepl(patterns[1],nuseds_cuid_location$comment)
sum(cond) # 93
unique(nuseds_cuid_location[cond,c("cuid","SYSTEM_SITE","Y_LAT","X_LONGT","sys_nm","distance")])
nrow(unique(nuseds_cuid_location[cond,c("SYSTEM_SITE","Y_LAT","X_LONGT")])) # 12


TODO: impose a threshold for the distnace in the 2st iteration 



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
}

table(nuseds_cuid_location$comment)




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
location <- "CHILCOTIN"
cond <- grepl(simplify_string_fun(location),streamlocationids$sys_nm_simple)
streamlocationids[cond,]

cond <- nuseds$cuid == cuid & (grepl(location,nuseds$SYSTEM_SITE) | 
                                 grepl(location,nuseds$WATERBODY))
cond <-  (grepl(location,nuseds$SYSTEM_SITE) | 
                                 grepl(location,nuseds$WATERBODY))
unique(nuseds[cond,c("SPECIES_QUALIFIED","cuid","SYSTEM_SITE","WATERBODY","Y_LAT","X_LONGT")])


#' Sys_nm “COOPER INLET-FANNIE COVE RH CREEK” and “COOPER INLET-FANNIE COVE CREEKS”
#'  have exactly the same coordinates
location <- "COOPER INLET-FANNIE COVE"
cuid <- 503
cond <- streamlocationids$cuid == cuid &
  grepl(simplify_string_fun(location),streamlocationids$sys_nm_simple)
streamlocationids[cond,]

nuseds_cuid_location[c(47,551),c("regionid","cu_name_pse","SPECIES_QUALIFIED",
                                 "cuid","SYSTEM_SITE","GFE_ID","Y_LAT","X_LONGT",
                                 "sys_nm","distance","comment")]


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

