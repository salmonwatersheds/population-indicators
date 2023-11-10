
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

library(xlsx)
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
#' - CM-9004                                     # Eric: a CU that no longer exists
CUToRemove <- c("CK-9002","CK-9005","CK-9006","CK-9007","CK-9008","SEL-15-03","CM-9004")
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
    
    # retain the desired columns
    sheetNew <- sheetNew[,c("facilityID","CUID")]
    
  }else if(sheetName == "DataEntry_releases"){
    
    #
    sheetNew <- DFO_df[,field_DFO]
    colnames(sheetNew) <- field_PSF
    
    #' find the facilityid/ID corresponding to each unique combination of 
    #' program-project-facilityname:
    field_DFO_forFacilityID <- c("PROGRAM_CODE","PROJ_NAME","FACILITY_NAME")

    sheetNew <- cbind(sheetNew, DFO_df[,field_DFO_forFacilityID])
    
    colnames(sheetNew)[colnames(sheetNew) == "PROGRAM_CODE"] <- "program"
    colnames(sheetNew)[colnames(sheetNew) == "PROJ_NAME"] <- "project"
    colnames(sheetNew)[colnames(sheetNew) == "facilityID"] <- "facilityname"
    
    #' there are duplicated columns:
    #' TODO: remove them from now but deal with it with Katy and co.
    # nrow(sheetNew) # 31830
    # nrow(DFO_df)   # 31830
    # sum(duplicated(sheetNew)) # 134
    # sum(duplicated(DFO_df))   # 0
    # sum(duplicated(DFO_df[,c(field_DFO,field_DFO_forFacilityID)]))   # 134
    # 
    # rowsDuplicating <- which(duplicated(sheetNew) | duplicated(sheetNew, fromLast = TRUE))
    # View(sheetNew[rowsDuplicating,])
    # View(DFO_df[rowsDuplicating,field_DFO])
    # View(DFO_df[rowsDuplicating,])
    # 
    # sum(duplicated(DFO_df)) # 0
    # sum(duplicated(DFO_df[,colnames(DFO_df) != "MRP_TAGCODE"]))     # 35
    # sum(duplicated(DFO_df[,colnames(DFO_df) != "MRP_TAGCODE"])) # 0   ?!
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","RELEASE_COMMENT")]))  # 48
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","BROOD_YEAR")]))       # 41
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","AVE_WEIGHT")]))       # 40
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","NoTagClip")]))        # 39
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","RELEASE_COMMENT","BROOD_YEAR","AVE_WEIGHT","NoTagClip")]))  # 69
    # 
    # 
    # remainingCol <- colnames(DFO_df)[! colnames(DFO_df) %in% c(field_DFO,field_DFO_forFacilityID)]
    # for(c in remainingCol){
    #   DFO_df_cut <- DFO_df[,colnames(DFO_df) != c]
    #   if(sum(duplicated(DFO_df_cut)) > 0){
    #     print(c)
    #   }
    # }
    # colSelected <- c("MRP_TAGCODE","RELEASE_COMMENT","BROOD_YEAR","AVE_WEIGHT","NoTagClip","START_DATE","END_DATE","PURPOSE_CODE","AVE_LENGTH")
    # remainingCol <- colnames(DFO_df)[! colnames(DFO_df) %in% c(field_DFO,field_DFO_forFacilityID,colSelected)]
    # threshold <- sum(duplicated(DFO_df[,!colnames(DFO_df) %in% colSelected]))
    # for(c in remainingCol){
    #   # c <- "RELEASE_COMMENT"
    #   c_here <- c(c,colSelected)
    #   DFO_df_cut <- DFO_df[,!colnames(DFO_df) %in% c_here]
    #   nb_duplicates <- sum(duplicated(DFO_df_cut))
    #   if(nb_duplicates > threshold){
    #     print(paste0(nb_duplicates," duplicated; columns: "))
    #     print(c_here)
    #     print("")
    #   }
    # }
    # more columns must be removed to reach the 134 number of duplicated rows...
    
    # replace the program acronyms by their names
    sheetNew$program <- program_acronym_fun(prog_acro =  sheetNew$program)
    
    # add facilityid by merging with sheet DataEntry_facilities
    sheetNew <- merge(x = sheetNew, 
                      y = filePSFnew_l$DataEntry_facilities[,c("facilityid","program","project","facilityname")],
                      by = c("program","project","facilityname"), 
                      all = T)
    colnames(sheetNew)[colnames(sheetNew) == "facilityid"] <- "facilityID"
    
    # drop columns 'program', 'project' and 'facilityname'
    sheetNew <- sheetNew[,!colnames(sheetNew) %in% c('program','project','facilityname')]
    
    # replace STOCK_CU_INDEX and REL_CU_INDEX values by the PSF CUID
    sheetNew$release_site_CUID <- cui_cu_index_conservation_units_fun(cu_index = sheetNew$release_site_CUID, 
                                                                      conservation_units = conservation_units)
    sheetNew$cuid_broodstock <- cui_cu_index_conservation_units_fun(cu_index = sheetNew$cuid_broodstock, 
                                                                    conservation_units = conservation_units)
    
    # reorder columns and rows
    sheetNew <- sheetNew[field_PSF]
    sheetNew <- sheetNew[order(sheetNew$facilityID),]
    
    # remove duplicted rows (SEE TODO ABOVE ABOUT THAT)
    # nrow(DFO_df)
    # nrow(sheetNew) - nrow(unique(sheetNew))
    sheetNew <- unique(sheetNew)
    # sheetNew[sheetNew$cuid_broodstock != sheetNew$release_site_CUID,]
    
    # "release_site_name" ("RELEASE_SITE_NAME")
    # QUESTION: unabbreviate names, e.g., 'Adams R Up' --> 'Adams River Upper'
    #' TODO: QUESTION: Should I do it? is that correct? Am I missing something?
    # release_site_abbrev <-        c("Cr","R","Up","Low","Sl","N","S","E","W","LK")
    # names(release_site_abbrev) <- c("Creek","River","Upper","Lower","Slough","North","South","East","West","Lake")
    
  }
  filePSFnew_l[[sheet_i]] <- sheetNew
}



# export the file
date <- Sys.Date()
date <- gsub(pattern = "-",replacement = "",x = date)

for(sh_i in 1:length(names(filePSFnew_l))){
  # sh_i <- 1
  if(sh_i == 1){
    append <- F
  }else{
    append <- T
  }
  sheetName <- names(filePSFnew_l)[sh_i]
  write.xlsx(filePSFnew_l[sheetName], 
             file = paste0(wd_data_dropbox,"/SWP_hatchery_data_TBR",date,".xlsx"),
             sheetName = sheetName, 
             row.names = FALSE,
             append = append)
}


# OLD NOTES: -------
# - Eric: the only trick will be translating the "STOCK_CU_INDEX" field into 
# "cuid_broodstock" AND CUID of the release site. Will have to use one of the tables in the decoder repo. 
# Let me know if you have any other questions.
# - decoder tables:
# C:\Users\bcarturan\Salmon Watersheds Dropbox\Bruno Carturan\X Drive\1_PROJECTS\1_Active\Population Methods and Analysis\decoders\tables

# decoder repo:
# C:\Users\bcarturan\Salmon Watersheds Dropbox\Bruno Carturan\X Drive\1_PROJECTS\1_Active\Population Methods and Analysis\decoders
    

# QA/QC database relationship related ------
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

