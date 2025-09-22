
#'******************************************************************************
#' The goal of the script is to produce the metadata file that contains the definition 
#' of all the fields in each of the NuSEDS data generated files:
#' - 1_NuSEDS_escapement_data_collated_DATE.csv
#' - 2_nuseds_cuid_streamid_DATE.csv
#' - dataset2_spawner-surveys_DATE.csv
#' 
#' Imported files:
#' 
#' - Data_Dictionary_NuSEDS_EN.csv                  #
#' 
#'
#' Files exported:
#' 
#' 
#' 
#' 
#' 
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

wd_hatchery <- paste(wd_X_Drive1_PROJECTS,
                     wds_l$wd_population_indicator_dropbox,
                     "hatchery-releases",
                     sep = "/")

wd_pop_indic_data_gis_dropbox <- gsub("input","gis",wd_pop_indic_data_input_dropbox)

source("code/functions.R")

options(digits = 9)



#'* Import field definitions for NUSEDS and CUSS *
fields_def <- nuseds_fields_definitions_fun(wd_references = wd_references_dropbox)

fields_def


#'* Import the 2_nuseds_cuid_streamid *
nuseds_cuid_streamid <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"),
                                                   pattern = "2_nuseds_cuid_streamid")

# create meta data file
metadata_wide <- nuseds_cuid_streamid[1,]
dataset_source <- c()

for(cn in colnames(metadata_wide)){
  val <- NA
  if(cn %in% names(fields_def$all_areas_nuseds)){
    val <- fields_def$all_areas_nuseds[cn]
    dataset_source <- c(dataset_source,"all_areas_nuseds")
  }else   if(cn %in% names(fields_def$cu_system_sites)){
    val <- fields_def$cu_system_sites[cn]
    dataset_source <- c(dataset_source,"conservation_unit_system_sites")
  }else{
    dataset_source <- c(dataset_source,"2_nuseds_cuid_streamid")
  }
  # gsub("\n","","recovery effort\nCumulative New - N/A\n")
  # gsub('\"','','recovery effort \"Cumulative New\"')
  val <- gsub("\n","",val)
  val <- gsub('"',"'",val)
  # val <- gsub('\"','*',val)
  metadata_wide[,cn] <- val
}

metadata_wide$SPECIES_QUALIFIED <- fields_def$cu_system_sites$Species_Qualified

metadata_wide$IndexId <- "Combination of SPECIES_QUALIFIED and POP_ID"
metadata_wide$Year <- "Same as ANALYSIS_YR in all_areas_nuseds"
metadata_wide$MAX_ESTIMATE <- "Calculated by PSF and previously present in NuSEDS: The maximum value (excluding NAs) among the following fields in NuSEDS: NATURAL_ADULT_SPAWNERS, NATURAL_JACK_SPAWNERS, NATURAL_SPAWNERS_TOTAL, ADULT_BROODSTOCK_REMOVALS, JACK_BROODSTOCK_REMOVALS, TOTAL_BROODSTOCK_REMOVALS, OTHER_REMOVALS, TOTAL_RETURN_TO_RIVER"
metadata_wide$coordinates_changed <- "If the coordinates were manually changed (i.e., 'Y_LAT' and 'X_LONGT' != 'latitude' and 'longitude)"
metadata_wide$StatArea <- "Pacific Fishery Management Areas"
metadata_wide$FULL_CU_IN_PSE <- "The updated NuSEDS field 'FULL_CU_IN' according to PSF's definition of Cus"
metadata_wide$stream_survey_quality <- "The quality of the survey based on ESTIMATE_CLASSIFICATION"
metadata_wide$cuid <- "The unique numeric identifier for the CU, as used in the PSE's databases"
metadata_wide$cu_name_pse <- "The display name of the CU as shown in the PSE"
metadata_wide$cu_name_dfo <- "The name of the CU used by DFO"
metadata_wide$region <- "The broad-scale region that the CU is part of in the PSE"
metadata_wide$regionid <- "The unique numeric identifier for the region"
metadata_wide$sys_nm <- "The site name as diaplyed in the PSE (derived from SYSTEM_SITE and/or WATERBODY)"
metadata_wide$streamid <- "The numeric identifier of a population = a unique cuid & location (i.e. GFE_ID) combination"

metadata_long <- data.frame(field = colnames(metadata_wide),
                            dataset = dataset_source,
                            description = unlist(metadata_wide[1,,drop = T]))

View(metadata_long)


# write files in /output/archive
date <- as.character(Sys.Date())
# date <- "2025-06-03"


# write in the /output/archive in dropbox
write.csv(x = metadata_long,
          paste0(getwd(),"/output/METADATA_2_nuseds_cuid_streamid.csv"),
          row.names = F)

write.csv(x = metadata_long,
          paste0(wd_output,"/METADATA_2_nuseds_cuid_streamid.csv"),
          row.names = F)


#
# Old try that does not work well because NuSEDS recent update (to revisit potentially) ------
#

#'* Import Data_Dictionary_NuSEDS_EN.csv *
#' the definition of the fields of the original *all_areas_nuseds.csv* 
#' source: https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6/resource/1d343cd3-5614-3bda-814b-48a08084b051
fields_nuseds <- read.csv(paste0(wd_data,"/Data_Dictionary_NuSEDS_EN.csv"), header = T)

head(fields_nuseds)
colnames(fields_nuseds) <- gsub("\\.","_",colnames(fields_nuseds))
str(fields_nuseds)

fields_nuseds <- fields_nuseds[,colnames(fields_nuseds) != "NuSEDS_Column_Letter"]

fields_nuseds$Codes <- NA  

# Add the fields created in by PSF
fields_nuseds$dataset_source <- "all areas nuseds"


#'* Import Data_Dictionary_CUSS_EN.csv *
# https://open.canada.ca/data/en/dataset/1ac00a39-4770-443d-8a6b-9656c06df6a3/resource/eb7d173f-8b66-4123-bc5d-1287a55fb0d8
fields_cuss <- read.csv(paste0(wd_data,"/Data_Dictionary_CUSS_EN.csv"), header = T)

head(fields_cuss)
colnames(fields_cuss) <- gsub("\\.","_",colnames(fields_cuss))
str(fields_cuss)

fields_cuss <- fields_cuss[,colnames(fields_cuss) != "Column_Letter"]

fields_cuss$dataset_source <- "conservation unit system sites"


# merge the two
fields_nuseds_all <- rbind(fields_nuseds,fields_cuss)


#'* Import the 2_nuseds_cuid_streamid *
nuseds_cuid_streamid <- import_mostRecent_file_fun(wd = paste0(wd_output,"/archive"),
                                                   pattern = "2_nuseds_cuid_streamid")

# new fields:
cond <- !colnames(nuseds_cuid_streamid) %in% fields_nuseds_all$Field_Name
fieldsNew <- colnames(nuseds_cuid_streamid)[cond]
fieldsNew

#
r_new <- fields_nuseds[1,]
r_new$Field_Name <- "IndexId"
r_new$Field_Definition <- "Combination of SPECIES_QUALIFIED and POP_ID"
r_new$Codes <- NA  
r_new$dataset_source <- "2_nuseds_cuid_streamid"
fields_nuseds <- rbind(fields_nuseds,r_new)

r_new <- fields_nuseds[1,]
r_new$Field_Name <- "Year"
r_new$Field_Definition <- "Same as ANALYSIS_YR"
r_new$Codes <- NA  
r_new$dataset_source <- "2_nuseds_cuid_streamid"
fields_nuseds <- rbind(fields_nuseds,r_new)

r_new <- fields_nuseds[1,]
r_new$Field_Name <- "MAX_ESTIMATE"
r_new$Field_Definition <- "Calculated by PSF and previously present in NuSEDS: The maximum value (excluding NAs) among the following fields in NuSEDS: NATURAL_ADULT_SPAWNERS, NATURAL_JACK_SPAWNERS, NATURAL_SPAWNERS_TOTAL, ADULT_BROODSTOCK_REMOVALS, JACK_BROODSTOCK_REMOVALS, TOTAL_BROODSTOCK_REMOVALS, OTHER_REMOVALS, TOTAL_RETURN_TO_RIVER"
r_new$Codes <- NA  
r_new$dataset_source <- "2_nuseds_cuid_streamid"
fields_nuseds <- rbind(fields_nuseds,r_new)

r_new <- fields_nuseds[1,]
r_new$Field_Name <- "Year"
r_new$Field_Definition <- "Same as ANALYSIS_YR"
r_new$Codes <- NA  
r_new$dataset_source <- "2_nuseds_cuid_streamid"
fields_nuseds <- rbind(fields_nuseds,r_new)

r_new <- fields_nuseds[1,]
r_new$Field_Name <- "Year"
r_new$Field_Definition <- "Same as ANALYSIS_YR"
r_new$Codes <- NA  
r_new$dataset_source <- "2_nuseds_cuid_streamid"
fields_nuseds <- rbind(fields_nuseds,r_new)

