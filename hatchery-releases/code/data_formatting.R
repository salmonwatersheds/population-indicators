
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

#'** Import conservation-units.csv from wd_spawner_surveys_data **
#' This file comes from the PSF database all allows to match the DFO STOCK_CU_INDEX
#' with the PSF 'cuid' (or 'CUID') with the field 'cu_index' (= STOCK_CU_INDEX)
# TODO: eventually move the conservation-units.csv file to the population-indicators folder
conservation_units <- read.csv(paste0(wd_spawner_surveys_data,"/conservation-units.csv"),
                               header = T)

#'** Import the most recent version of PSF_modified_SEP_releases_DATE.xlsx in wd_data **
DFO_df_all <- return_file_lastVersion_fun(wd_data,pattern = "PSF_modified_SEP_releases")

#' 1) Deal with NAs in STOCK_CU_INDEX
#' FOR NOW: --> remove the rows without values for STOCK_CU_INDEX 
#' TODO: wait to hear from them and adjust accordingly
#' Eric: "It looks like the last time the data was wrangled we just didn't include
#' these ones. However, if you're feeling ambitious you can use the STOCK_GFE_ID
#' to match the GFE_ID here:
#' https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6/resource/0ba6773e-ddf6-3012-87bf-df707eb1ec4c 
#' (you'll have to translate the SPECIES_QUALIFIED in the opendata file to 
#' SPECIES_NAME in the PSF_modified_SEP_releases_2023 file e.g. CK-> Chinook)".
DFO_df <- DFO_df_all
DFO_df <- DFO_df[!is.na(DFO_df$STOCK_CU_INDEX),]
nrow(DFO_df) # 32642

#' 2) STOCK_CU_INDEX (CU of the population of origine) and REL_CU_INDEX (CU of 
#' the releasing site)
#' Steph and Eric: "we assume that REL_CU_INDEX = STOCK_CU_INDEX when NAs are present 
#' in REL_CU_INDEX".
nrow(DFO_df[is.na(DFO_df$REL_CU_INDEX),])
DFO_df$REL_CU_INDEX[is.na(DFO_df$REL_CU_INDEX)] <- DFO_df$STOCK_CU_INDEX[is.na(DFO_df$REL_CU_INDEX)]

#' 3) "Seapen" released (RELEASE_STAGE_NAME == "Seapen")
#' Steph and Eric: "some cases REL_CU_INDEX = blank is for seapen released, since
#' maybe in those cases the release can’t be assigned to a CU? But in those cases
#'  do we assume the fish will return to the broodstock CU? Yes."
#' TODO: deal with it later.
# View(DFO_df_all[grepl("Seapen",DFO_df_all$RELEASE_STAGE_NAME),])

#' 4) Remove the CUs in STOCK_CU_INDEX and REL_CU_INDEX that are not present in 
#' the PSF database 
#' (i.e., conservation-units.csv):
#' - CK-9002, CK-9005, CK-9006, CK-9007, CK-9008 # Eric: These aren't real CUs. They are hatchery-only population so we don't show them on the PSE
#' - SEL-15-03                                   # Eric: This a CU that the central coast Nations said is not a CU. So we don't show on the PSE
CUToRemove <- unique(DFO_df$STOCK_CU_INDEX[! DFO_df$STOCK_CU_INDEX %in% conservation_units$cu_index])
CUToRemove
DFO_df <- DFO_df[! DFO_df$STOCK_CU_INDEX %in% CUToRemove,]
DFO_df <- DFO_df[! DFO_df$REL_CU_INDEX %in% CUToRemove,]


#' ** Import the hatchery template from wd_data as a list **
filePSF_l <- hatchery_template_fun(wd_data = wd_data,
                                      filePSFname = "SWP_hatchery_data_template.xlsx")


#' Create a dataframe with the name of the columns in PSF_modified_SEP_releases_DATE.xlsx
#' and corresponding column names and sheets in the survey file SWP_hatchery_data_...xlsx
matchCol_df <- matching_columns_fun(wd_data = wd_data,
                                    wd_spawner_surveys_data = wd_spawner_surveys_data,
                                    DFO_df = DFO_df)

# make a copy of filePSF_l that is going to be filled
filePSFnew_l <- filePSF_l
#' TODO: correct/clear the 1st sheet (?)

# Fill filePSF_l with new data
for(sheet_i in 2:length(names(filePSF_l))){   # The 1st sheet is to be filled by hand (QUESTION)
  
  # sheet_i <- 2
  sheetName <- names(filePSF_l)[sheet_i]
  sheetNew <- filePSF_l[[sheet_i]]
  
  # subset matchCol_df for the current sheet
  # matchCol_df_cut <- matchCol_df[matchCol_df$PSF_sheet == sheetName,]
  
  # 
  field_PSF <- matchCol_df$PSF_colnames[matchCol_df$PSF_sheet == sheetName]
  field_DFO <- matchCol_df$DFO_colnames[matchCol_df$PSF_sheet == sheetName]
  field_DFO <- field_DFO[!is.na(field_DFO)]
  
  if(sheetName == "DataEntry_facilities"){ # sheet 2
    
    #
    sheetNew <- DFO_df[,field_DFO]
    
    colnames(sheetNew) <- field_PSF[field_PSF != "facilityid"]
    
    # only keep the year, e.g., '19920814' --> '1992'
    for(ycol in c("startyear","endyear")){
      # ycol <- c("startyear","endyear")[1]
      date <- sapply(X = sheetNew[,ycol], 
                     FUN = function(d){substr(x = d,start = 1, stop = 4)})
      date <- as.numeric(date)
      allDates <- date %in% 1900:2100
      if(!all(allDates)){
        print(paste0("The following dates in '",sheetName,"/",ycol,"' have to be checked:"))
        print(date[!allDates])
      }
      sheetNew[,ycol] <- date
    }
    
    #' Define the starting and ending years for each program-project-facilityname 
    #' combination
    sheetNew_l <- list()
    count <- 1
    for(fn in unique(sheetNew$facilityname)){
      # fn <- unique(sheetNew$facilityname)[1]
      sheetNew_fn <- sheetNew[sheetNew$facilityname == fn,]
      for(pg in unique(sheetNew_fn$program)){
        # pg <- unique(sheetNew_fn$program)[1]
        sheetNew_fn_pr <- sheetNew_fn[sheetNew_fn$program == pg,]
        for(pj in unique(sheetNew_fn_pr$project)){
          # pj <- unique(sheetNew_fn_pr$project)[1]
          sheetNew_fn_pr_pj <- sheetNew_fn_pr[sheetNew_fn_pr$project == pj,]
          startyear_min <- min(sheetNew_fn_pr_pj$startyear)
          endyear_max <- max(sheetNew_fn_pr_pj$endyear)
          slice <- sheetNew_fn_pr_pj[1,]
          slice$startyear <- startyear_min
          slice$endyear <- endyear_max
          sheetNew_l[[count]] <- slice
          count <- count + 1
        }
      }
    }
    sheetNew <- do.call(rbind,sheetNew_l)
    
    #' In "program" replace the acronyms by the full names
    sheetNew$program <- program_acronym_fun(prog_acro =  sheetNew$program)
    # sheetNew[sheetNew$program == "NA",]  # QUESTION: is that noramal?
    # sheetNew[sheetNew$facilityname == "Omega Pacific H",]
    
    #' Define facilityid/facilityID:
    #' --> facilityid uniquely identifies “program-project-facilityname-facility_latitude-facility_longitude-startyear-endyear”
    #' --> facilityid only lives in this file so it can be defined here.
    sheetNew$facilityid <- 1:nrow(sheetNew)

    # Reorder columns
    sheetNew <- sheetNew[,field_PSF]
    
  }else if(sheetName == "DataEntry_facilitiescuids"){
    
    #' find the facilityid/ID corresponding to each unique combination of 
    #' program-project-facilityname:
    field_DFO <- c("PROGRAM_CODE","PROJ_NAME","FACILITY_NAME","REL_CU_INDEX")
    sheetNew <- DFO_df[,field_DFO]
    colnames(sheetNew) <- c("program","project","facilityname","cu_index") # "cu_index" in conservation_units = the CU_INDEX
    sheetNew$program <- program_acronym_fun(prog_acro =  sheetNew$program)
    sheetNew <- merge(x = sheetNew, 
                      y = filePSFnew_l$DataEntry_facilities[,c("facilityid","program","project","facilityname")],
                      by = c("program","project","facilityname"), 
                      all = T)
    
    # remove uncessary columns and update their names
    sheetNew <- sheetNew[,c("facilityid","cu_index")]  
    colnames(sheetNew)[colnames(sheetNew) == "facilityid"] <- "facilityID"
    
    # order rows
    sheetNew <- sheetNew[order(sheetNew$facilityID),]
    
    # remove duplicated rows
    sheetNew <- unique(sheetNew)
    
    # convert CUID (i.e. REL_CU_INDEX) to the PSF cuid
    sheetNew$CUID <- cui_cu_index_conservation_units_fun(cu_index = sheetNew$cu_index,
                                                         conservation_units = conservation_units)
    
    #' TODO: wait to hear about "The value 'CM-9004' does not have a match so NA is given instead."
    
    # retain the desired columns
    sheetNew <- sheetNew[,c("facilityID","CUID")]
    
  }else if(sheetName == "DataEntry_releases"){
    
    #
    sheetNew <- DFO_df[,field_DFO]
    colnames(sheetNew) <- field_PSF
    
    #' find the facilityid/ID corresponding to each unique combination of 
    #' program-project-facilityname:
    field_DFO <- c(field_DFO,c("PROGRAM_CODE","PROJ_NAME")) # add the fields to be able to match the facilityID
    
    sheetNew <- cbind(sheetNew, DFO_df[,c("PROGRAM_CODE","PROJ_NAME","FACILITY_NAME")])
    
    colnames(sheetNew)[colnames(sheetNew) == "PROGRAM_CODE"] <- "program"
    colnames(sheetNew)[colnames(sheetNew) == "PROJ_NAME"] <- "project"
    colnames(sheetNew)[colnames(sheetNew) == "facilityID"] <- "facilityname"
    
    # place the program acronyms by their names
    sheetNew$program <- program_acronym_fun(prog_acro =  sheetNew$program)
    
    # add facilityid by merging with sheet DataEntry_facilities
    sheetNew <- merge(x = sheetNew, 
                      y = filePSFnew_l$DataEntry_facilities[,c("facilityid","program","project","facilityname")],
                      by = c("program","project","facilityname"), 
                      all = T)
    colnames(sheetNew)[colnames(sheetNew) == "facilityid"] <- "facilityID"
    
    # drop columns 'program', 'project' and 'facilityname'
    sheetNew <- sheetNew[,! colnames(sheetNew) %in% c('program','project','facilityname')]
    
    # reorder columns and rows
    sheetNew <- sheetNew[field_PSF]
    sheetNew <- sheetNew[order(sheetNew$facilityID),]
    
    # there are duplicated columns:
    View(sheetNew[duplicated(sheetNew),])
    View(unique(sheetNew))
    r_unique <- rownames(unique(sheetNew))
    r_unique_no <- rownames(sheetNew)[!rownames(sheetNew) %in% r_unique]
    View(sheetNew[r_unique_no,])
    
    
    # "release_site_name" ("RELEASE_SITE_NAME")
    # QUESTION: unabbreviate names, e.g., 'Adams R Up' --> 'Adams River Upper'
    #' TODO: QUESTION: Should I do it? is that correct? Am I missing something?
    # release_site_abbrev <-        c("Cr","R","Up","Low","Sl","N","S","E","W","LK")
    # names(release_site_abbrev) <- c("Creek","River","Upper","Lower","Slough","North","South","East","West","Lake")
    
    
    # "release_site_CUID" ("STOCK_CU_INDEX") is "cuid" in conservation_units.csv
    sheetNew$cu_index <- sheetNew$release_site_CUID
    sheetNew$release_site_CUID <- convert_column_match_fun(df = sheetNew, 
                                                           decoder_df = conservation_units, 
                                                           colToConvert = "cu_index",
                                                           colMatchingVal = "cuid")
    sheetNew <- sheetNew[colnames(sheetNew) != "cu_index"]
    # remove NAs - QUICK FIX FOR NOW
    #' TODO: wait to hear from them to know what to do with these CUs
    sheetNew <- sheetNew[!is.na(sheetNew$release_site_CUID),]
    
    
    # "facilityID" (from "FACILITY_NAME")
    sheetNew$facilityname <- sheetNew$facilityID
    sheetNew$facilityID <- convert_column_match_fun(df = sheetNew, 
                                                    decoder_df = facilityNameID_df, 
                                                    colToConvert = "facilityname",
                                                    colMatchingVal = "facilityid")
    sheetNew <- sheetNew[colnames(sheetNew) != "facilityname"]
    
    
    # "cuid_broodstock"
    colHere <- "STOCK_CU_INDEX"
    colnames(sheetNew)[colnames(sheetNew) == colHere][2] <- field_PSF[field_DFO == colHere][2]
    #' TODO: find a way to fill that column, ask how to do that?
    
    # colnames(sheetNew)
    # field_PSF
    # field_DFO
    # cbind(field_DFO,field_PSF)
    # unique(sheetNew$SPECIES_NAME)
    # unique(DFO_df_all$SPECIES_NAME)
    # colSelected <- c("SPECIES_NAME","RUN_NAME","STOCK_NAME","STOCK_GFE_ID","STOCK_GFE_NAME","STOCK_POP_ID","STOCK_POP_NAME","STOCK_CU_ID","STOCK_CU_NAME")
    # View(unique(DFO_df_all[,colSelected]))
    # DFO_df_all
    # matchCol_df
  }
  filePSFnew_l[[sheet_i]] <- sheetNew
}



unique(data.frame(v1 = c("A","B","A","C"),
                  v2 = c(3,3,3,1),
                  v3 = c(4,3,NA,1))) 
    
AFS-Aboriginal Fisheries Strategy
CDP-Community Economic Development Program
DPI-Designated Public Involvement
OPS-Major Operations
PIP-Public Involvement Program

    
# CK-9002
# CK-9005
# CK-9006
# CK-9007
# CK-9008
# These aren't real CUs. They are hatchery-only population so we don't show them on the PSE

# SEL-15-03
# This a CU that the central coast Nations said is not a CU. So we don't show on the PSE
    
    
    
# NOTES:
# - Eric: the only trick will be translating the "STOCK_CU_INDEX" field into 
# "cuid_broodstock" AND CUID of the release site. Will have to use one of the tables in the decoder repo. 
# Let me know if you have any other questions.
# - decoder tables:
# C:\Users\bcarturan\Salmon Watersheds Dropbox\Bruno Carturan\X Drive\1_PROJECTS\1_Active\Population Methods and Analysis\decoders\tables

# decoder repo:
# C:\Users\bcarturan\Salmon Watersheds Dropbox\Bruno Carturan\X Drive\1_PROJECTS\1_Active\Population Methods and Analysis\decoders
    
    
# QA/QC
# QUESTIONS: are those multiple relationship normal?
printDF <- T
relationships_twoCol_df_fn(df = DFO_df,col1 = "FACILITY_NAME",col2 = "PROGRAM_CODE", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "FACILITY_NAME",col2 = "PROJ_NAME", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "PROGRAM_CODE",col2 = "PROJ_NAME", printDF = printDF) # one to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "FACILITY_NAME",col2 = "PROJ_NAME", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col2 = "FACILITY_NAME",col1 = "PROJ_NAME", printDF = printDF)

relationships_twoCol_df_fn(df = DFO_df,col1 = "FACILITY_NAME",col2 = "START_DATE", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "PROGRAM_CODE",col2 = "START_DATE", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "PROJ_NAME",col2 = "START_DATE", printDF = printDF) # many to many

DFO_df$START_END_DATE <- paste(DFO_df$START_DATE,DFO_df$END_DATE,sep = "_")
relationships_twoCol_df_fn(df = DFO_df,col1 = "PROGRAM_CODE",col2 = "START_END_DATE", printDF = printDF) # many to many
relationships_twoCol_df_fn(df = DFO_df,col1 = "PROJ_NAME",col2 = "START_END_DATE", printDF = printDF) # many to many

DFO_df$PROGRAM_CODE_PROJ_NAME <- paste(DFO_df$PROGRAM_CODE,DFO_df$PROJ_NAME,sep = " _ ")
relationships_twoCol_df_fn(df = DFO_df,
                           col1 = "PROGRAM_CODE_PROJ_NAME",
                           col2 = "START_END_DATE", 
                           printDF = printDF) # many to many

DFO_df$PROGRAM_CODE_PROJ_NAME_FACILITY_NAME <- paste(DFO_df$PROGRAM_CODE_PROJ_NAME,DFO_df$FACILITY_NAME,sep = " _ ")
relationships_twoCol_df_fn(df = DFO_df,
                           col1 = "PROGRAM_CODE_PROJ_NAME_FACILITY_NAME",
                           col2 = "START_END_DATE", 
                           printDF = printDF) # many to many

sum(is.na(DFO_df$END_DATE))   # 0
sum(is.na(DFO_df$START_DATE)) # 0

sum(is.na(DFO_df$FACILITY_LATITUDE)) #  2043
sum(is.na(DFO_df$FACILITY_LONGITUDE)) #  2043
unique(DFO_df$FACILITY_NAME[is.na(DFO_df$FACILITY_LATITUDE)]) # facilities withuot GIS coordinates
    
    
    
    
    
    
    
    
    
    
    # associate a facilityid to each PROGRAM_CODE
    facilityname <- unique(DFO_df$FACILITY_NAME)
    facilityid <- 1:length(facilityname)
    prog_facilID_df <- data.frame(facilityname = facilityname,
                                  facilityid = facilityid)
    
    # create an empty dataframe and start filling it
    sheetNew <- dataframe_structure_fun(df = filePSF_l[[sheet_i]], 
                                        colnamesToKeep = c("facilityid","facilityname"),
                                        nrow = nrow(prog_facilID_df))

    sheetNew$facilityid <- prog_facilID_df$facilityid
    sheetNew$facilityname <- prog_facilID_df$facilityname
    
    # fill the rest automatically
    colNamesRemaining <- colnames(filePSF_l[[sheet_i]])[!colnames(filePSF_l[[sheet_i]]) %in% c("facilityid","facilityname")]
    for(col_i in 1:(length(colNamesRemaining)-1)){  # -1 because "startyear" and "endyear" are treated together
    #for(col_i in 1:4){
      # col_i <- 1
      field_PSF <- colNamesRemaining[col_i]
      field_DFO <- matchCol_df$DFO_colnames[matchCol_df$PSF_colnames == field_PSF &
                                            matchCol_df$PSF_sheet == sheetName]
      
      # match the other fields with 'facilityname' except 'project' that must be 
      # match with 'program', because 'project' has a MANY TO ONE relationship with
      # 'program' but a MANY TO MANY with 'facilityname'
      #' TODO: make sure you get confirmation of these associations
      field_PSF_ref <- "facilityname"
      if(field_PSF == "project"){
        field_PSF_ref <- "program"
      }else if(field_PSF %in% c("startyear","endyear")){
        # field_PSF_ref <- "project"
        field_PSF_ref <- c("project","program","facilityname")
      }
      # field_DFO_ref <- matchCol_df$DFO_colnames[matchCol_df$PSF_colnames == field_PSF_ref & 
      #                                            matchCol_df$PSF_sheet == sheetName]
      field_DFO_ref <- matchCol_df$DFO_colnames[matchCol_df$PSF_colnames %in% field_PSF_ref & 
                                                  matchCol_df$PSF_sheet == sheetName]
      
      # make a copy of DFO_df, keep only FACILITY_NAME and focal field
      DFO_df_cut <- DFO_df[,c(field_DFO_ref,field_DFO)]
      
      # relationships_twoCol_df_fn(df = DFO_df_cut, col1 = field_DFO_ref,col2 = field_DFO)
      # relationships_twoCol_df_fn(df = DFO_df_cut, col2 = field_DFO_ref,col1 = field_DFO)
      
      # rename columns to match PSF colnames by making sure to preserve the order
      # of the columns in case several are present
      for(col_psf_here in field_PSF_ref){
        # col_psf_here <- field_PSF_ref[3]
        field_DFO_ref_here <- matchCol_df$DFO_colnames[matchCol_df$PSF_colnames == col_psf_here & 
                                                       matchCol_df$PSF_sheet == sheetName]
        colnames(DFO_df_cut)[colnames(DFO_df_cut) == field_DFO_ref_here] <- col_psf_here
      }
      
      #
      colnames(DFO_df_cut)[colnames(DFO_df_cut) == field_DFO] <- field_PSF
      
      if(field_PSF == "startyear"){
        
        # combine columns "startyear" and "endyear"
        field_DFO_endDate <- matchCol_df$DFO_colnames[matchCol_df$PSF_colnames == "endyear" & 
                                                      matchCol_df$PSF_sheet == sheetName]
        DFO_df_cut$endyear <- DFO_df[,field_DFO_endDate,drop = T]
        
        DFO_df_cut$startEndYear <- paste(DFO_df_cut$startyear,DFO_df_cut$endyear,sep = "_")
      }
      
      # remove duplicated rows
      DFO_df_cut <- unique(DFO_df_cut)
      
      # merge sheetNew and DFO_df_cut by facilityname
      sheetNew <- merge(x = sheetNew,y = DFO_df_cut, by = field_PSF_ref, all = T)
      # View(sheetNew)
      
      # finalise the dates and table plus check years make sense
      if(field_PSF == "startyear"){
        
        PSFfieldsToKeep <- matchCol_df$PSF_colnames[matchCol_df$PSF_sheet == sheetName]
        sheetNew <- sheetNew[,PSFfieldsToKeep]
        
        for(ycol in c("startyear","endyear")){
          
          # ycol <- c("startyear","endyear")[1]
          
          # only keep the year, e.g., '19920814' --> '1992'
          date <- sapply(X = sheetNew[,ycol], 
                         FUN = function(d){substr(x = d,start = 1, stop = 4)})
          date <- as.numeric(date)
          allDates <- date %in% 1900:2100
          if(!all(allDates)){
            print(paste0("The following dates in '",sheetName,"/",field_PSF,"' have to be checked:"))
            print(date[!allDates])
          }
          DFO_df_cut[,field_PSF] <- date
          
          
        }
        
      }
      
      

      
      
    }
  }else if(){
    
  }

  
  
  
  
}






# Make a new DFO_df and fill it up with the new data
SWP_new_df <- DFO_df[0,] 

for(i in 1:length(varSurvey)){
  
  # i <- 1
  varHere <- varSurvey[i]
  sheetHere <- matchCol_noNA_df$PSF_sheet[matchCol_noNA_df$PSF_colnames == varHere]
  val <- filePSF_l[[sheetHere]][,varHere,drop = T]
  
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
  varHere_swp <- matchCol_noNA_df$PSF_colnames[matchCol_noNA_df$PSF_colnames == varHere]
  
  
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








