
#'******************************************************************************
#' The goal of the script is to
#' 
#' #' Files imported (from /output --> produced in this subdir):
#' -
#' 
#' Files produced: 
#' - 
#'******************************************************************************

# 
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
source("functions_set_wd.R")
source("functions_general.R")

subDir_projects <- subDir_projects_fun()

wds_l <- set_working_directories_fun(subDir = subDir_projects$hatchery_releases,
                                     Export_locally = F)
wd_head <- wds_l$wd_head
wd_project <- wds_l$wd_project
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS

wd_data_dropbox <- paste(wd_X_Drive1_PROJECTS,
                         wds_l$wd_project_dropbox,
                         "data",sep="/")

# define wd to access population-indicators/spawner-surveys/data/conservation-units.csv

wd_spawner_surveys_data <- paste(wd_X_Drive1_PROJECTS,
                                 "1_Active/Population Methods and Analysis/population-indicators/spawner-surveys",
                                 "data",sep="/")

library(readxl)
library(tidyverse)
library(stringr)

source(paste(wd_code,"functions.R",sep = "/"))

#' *** Import the latest 

# NOTES:
# - Eric: the only trick will be translating the "STOCK_CU_INDEX" field into 
# "cuid_broodstock" AND CUID of the release site. Will have to use one of the tables in the decoder repo. 
# Let me know if you have any other questions.
# - decoder tables:
# C:\Users\bcarturan\Salmon Watersheds Dropbox\Bruno Carturan\X Drive\1_PROJECTS\1_Active\Population Methods and Analysis\decoders\tables

# decoder repo:
# C:\Users\bcarturan\Salmon Watersheds Dropbox\Bruno Carturan\X Drive\1_PROJECTS\1_Active\Population Methods and Analysis\decoders

# Import the most recent version of PSF_modified_SEP_releases_DATE.xlsx in wd_data
DFO_df <- return_file_lastVersion_fun(wd_data,pattern = "PSF_modified_SEP_releases")

#' Create a dataframe with the name of the columns in PSF_modified_SEP_releases_DATE.xlsx
#' and corresponding column names and sheets in the survey file SWP_hatchery_data_...xlsx
matchCol_df <- matching_columns_fun(wd_data = wd_data,
                                    wd_spawner_surveys_data = wd_spawner_surveys_data,
                                    DFO_df = DFO_df)

# Import the hatchery template from wd_data as a list.
fileSurvey_l <- hatchery_template_fun(wd_data = wd_data,
                                      fileSuveryname = "SWP_hatchery_data_template.xlsx")

# Import conservation-units.csv from wd_spawner_surveys_data
# TODO: eventually move the conservation-units.csv file to the population-indicators folder
conservation_units <- read.csv(paste0(wd_spawner_surveys_data,"/conservation-units.csv"),
                               header = T)

# Fill fileSurvey_l with new data
## abbreviation to convert field release_site_name in sheet DataEntry_releases:
## QUESTION: is that correct? Am I missing something?
release_site_abbrev <-        c("Cr","R","Up","Low","Sl","N","S","E","W","LK")
names(release_site_abbrev) <- c("Creek","River","Upper","Lower","Slough","North","South","East","West","Lake")

for(sheet_i in 2:length(sheetsNames)){   # The 1st sheet is to be filled by hand (QUESTION)
  
  # sheet_i <- 2
  sheetName <- names(fileSurvey_l)[sheet_i]
  sheetNew <- fileSurvey_l[[sheet_i]]
  
  # subset matchCol_df for the current sheet
  matchCol_df_cut <- matchCol_df[matchCol_df$Survey_sheet == sheetName,]
  
  # attribute to each program a facilityID (TODO: wait to hear from them to know if this is the right approach) 
  if(sheetName == "DataEntry_facilities"){
    
    # associate a facilityid to each PROGRAM_CODE
    programs <- unique(DFO_df$PROGRAM_CODE)
    facilityid <- 1:length(programs)
    prog_facilID_df <- data.frame(program = programs,
                                  facilityid = facilityid)
    
    # create an empty dataframe and start filling it
    sheetNew <- dataframe_structure_fun(df = fileSurvey_l[[sheet_i]], 
                                        nrow = nrow(prog_facilID_df))

    sheetNew$facilityid <- prog_facilID_df$facilityid
    sheetNew$program <- prog_facilID_df$program
    
    # fill the rest automatically
    for(col_i in 3:ncol(sheetNew)){
      
      # col_i <- 3
      
      
      
      
      
      
    }
    
    
    
    
    
    
    # fill the sheet accordingly
    fileSurvey_l[[sheet_i]]
    

    
    
    fileSurvey_l[[sheet_i]]$facilityid <- facilityid
    fileSurvey_l[[sheet_i]]$facilityid <- facilityid
    
  }
  
  
  
  
  for(){
    
    
    
    
  }
  
  DFO_df
  matchCol_df
  
  
  
  
  
}






# Make a new DFO_df and fill it up with the new data
SWP_new_df <- DFO_df[0,] 

for(i in 1:length(varSurvey)){
  
  # i <- 1
  varHere <- varSurvey[i]
  sheetHere <- matchCol_noNA_df$Survey_sheet[matchCol_noNA_df$Survey_colnames == varHere]
  val <- fileSurvey_l[[sheetHere]][,varHere,drop = T]
  
  if(i == 1){  # initiate SWP_new_df
    
    
    
  }
  
  # potential data manipulation
  if(varHere == "species"){
    # only keep the species, for instance 'Lake Sockeye' --> 'Sockeye
    speciesSalmon <- c("Sockeye","Chinook","Chum","Coho","Cutthroat","Pink","Steelhead")
    val <- val[val ]
    
    # Create a pattern for matching words in speciesSalmon
    # \\b is a word boundary anchor 
    pattern <- paste0("\\b", paste(speciesSalmon, collapse = "\\b|\\b"), "\\b")
    
    # Extract matching words for each element in val
    matched_words <- str_extract_all(val, pattern)
    val <- unlist(matched_words)
    
  }else if(varHere == "release_site_name"){
    
    # abbreviate names, e.g., 'Adams River Upper' --> 'Adams R Up'
    val <- sapply(X = val, FUN = function(v){
      
      # v <- val[3]
      for(j in 1:length(release_site_abbrev)){
        # j <- 1
        abb_here <- release_site_abbrev[j]
        w_here <- names(release_site_abbrev)[j]
        v <- gsub(pattern = w_here, replacement = abb_here, x = v)
      }
      return(v)
    })
    
  }else if(varHere == "release_date"){
    
    # only keep the year, e.g., '19920814' --> '1992'
    out <- substr(x = val,start = 1, stop = 4)
    out <- as.numeric(out)
    allDates <- out %in% 1950:2099
    if(!all(allDates <- out %in% 1950:2099)){
      print(paste("The following dates in",sheetHere,"/",varHere," have to be checked:"))
      print(out[!allDates])
    }
    val <- out
  }
  
  # fill SWP_new_df
  varHere_swp <- matchCol_noNA_df$PSF_colnames[matchCol_noNA_df$Survey_colnames == varHere]
  
  
  Create SWP_new_df here depending on number of rows
  SWP_new_df[,SWP_new_df] <- val
  
  
  
  
}





# translating the "STOCK_CU_INDEX" field into "cuid_broodstock"
DFO_df$STOCK_CU_ID



# Combine DFO_df and SWP_new_df 
#' QUESTIONS:
#' - shoud these two be combined?
#' - should I implement a check up to make sure the data is not already present?
#' 

# Older stuff ----

# Data obtained from DFO:
dataDFO <- read_excel(paste0(wd_data,"/PSF_modified_SEP_releases_2023.xlsx"),
                      sheet = 1)

head(dataDFO)
colnames(dataDFO)
# apply(X = dataDFO, MARGIN = 2, FUN = unique)
dataDFO$STOCK_CU_INDEX

# Template:
templSheet1 <- read_excel(paste0(wd_data,"/SWP_hatchery_data_template.xlsx"),
                          sheet = "DataProvider")
templSheet2 <- read_excel(paste0(wd_data,"/SWP_hatchery_data_template.xlsx"),
                          sheet = "DataEntry_facilities")
templSheet3 <- read_excel(paste0(wd_data,"/SWP_hatchery_data_template.xlsx"),
                          sheet = "DataEntry_facilitiescuids")
templSheet4 <- read_excel(paste0(wd_data,"/SWP_hatchery_data_template.xlsx"),
                          sheet = "DataEntry_releases")
# Sheet 1 -----
# QUESTION: do I need to fill sheet one as well?
templSheet1
dim(templSheet1)
fields_s1 <- templSheet1$`Pacific Salmon Foundation Salmon Watersheds Program` 
fields_s1 <- fields_s1[!is.na(fields_s1)]
fields_s1 <- fields_s1[3:length(fields_s1)]

# "First Name"         --> ???
# "Last Name"          --> ???
# "Organization"       --> ??? 
# "Email"              --> ???
# "Metadata file name" --> ???
unique(dataDFO$PROJ_NAME)
unique(dataDFO$REL_CU_NAME)
unique(dataDFO$RELEASE_SITE_NAME)

# Sheet 2 ----
colnames(templSheet2)

# "facilityid" ??? --> need a look up file, 
unique(dataDFO$)

# "program"  ???
unique(dataDFO$PROGRAM_CODE) # --> no, "program" is for instance: "Public Involvement Unit"

# "project"
unique(dataDFO$PROJ_NAME)

# "facilityname"
unique(dataDFO$FACILITY_NAME)  # ???

# "CUID"  --> not in the more recent template
unique(dataDFO$STOCK_CU_ID)   # cuid_broodstock 

# "facility_latitude"
unique(dataDFO$FACILITY_LATITUDE)

# "facility_longitude
unique(dataDFO$FACILITY_LONGITUDE)

# startyear"
unique(dataDFO$START_DATE)

# "endyear" 
unique(dataDFO$END_DATE)

# Sheet 3 -----
colnames(templSheet3)

# "facilityID" ???

# "CUID" 
unique(dataDFO$STOCK_CU_ID)

# sheet 4 -------

colnames(templSheet4)

# "species"
unique(dataDFO$SPECIES_NAME)

# "release_site_latitude"
unique(dataDFO$REL_LATITUDE)

# "release_site_longitude"
unique(dataDFO$REL_LONGITUDE)

# "release_site_name"
unique(dataDFO$RELEASE_SITE_NAME)

# "release_stage"
unique(dataDFO$RELEASE_STAGE_NAME)

# "release_site_CUID"
unique(dataDFO$REL_CU_INDEX)

# "facilityID" ???
unique(dataDFO$ID)

# "cuid_broodstock"        
# "release_date"           
# "total_release" 








