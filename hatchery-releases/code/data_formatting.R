
#'******************************************************************************
#' The goal of the script is to import the PSF_modified_SEP_releases.xlsx file 
#' from DFO and organise it to match the structure in
#' SWP_hatchery_data_template.xlsx.
#' 
#' #' Files imported:
#' 
#' - PSF_modified_SEP_releases_2023.xlsx    # (from DFO)
#' - conservationunits_decoder.csv          # (from PSE database)
#' - SWP_hatchery_data_template.xlsx        # use for formatting the output dataset
#' 
#' Files exported:
#' 
#' - SWP_hatchery_data_DATE.xlsx #
#' - cuid_broodstock_multi.csv   # to check cases where there are multiple cuid_broodstock
#'                               # for a same release_site_name-release_stage-release_site_CUID-release_date
#'                               # combination.
#' 
#' 
#' 
#'******************************************************************************

# 
rm(list = ls())
graphics.off()

options(java.parameters = "- Xmx1024m") # to be able to export a large excel file


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

wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# Define wd to access population-indicators/spawner-surveys/data/conservation-units.csv

wd_spawner_surveys_data <- paste(wd_X_Drive1_PROJECTS,
                                 "1_Active/Population Methods and Analysis/population-indicators/spawner-surveys",
                                 "data",sep="/")

library(xlsx)
library(readxl)
library(tidyverse)
library(stringr)

source(paste(wd_code,"functions.R",sep = "/"))

#
# Import datasets --------

#'* Import conservation-units.csv from wd_spawner_surveys_data *
#' This file comes from the PSF database all allows to match the DFO STOCK_CU_INDEX
#' with the PSF 'cuid' (or 'CUID') with the field 'cu_index' (= STOCK_CU_INDEX)
#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = F,
                                                   update_file_csv = F,
                                                   wd = wd_pop_indic_data_input_dropbox)

#'* Import the most recent version of PSF_modified_SEP_releases_DATE.xlsx in wd_data *
DFO_df_all <- return_file_lastVersion_fun(wd_data = wd_data,
                                          pattern = "PSF_modified_SEP_releases")

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

#' 2) STOCK_CU_INDEX (CU of the population of origin) and REL_CU_INDEX (CU of 
#' the releasing site)
#' Steph and Eric: "we assume that REL_CU_INDEX = STOCK_CU_INDEX when NAs are present 
#' in REL_CU_INDEX".
nrow(DFO_df[is.na(DFO_df$REL_CU_INDEX),])
DFO_df$REL_CU_INDEX[is.na(DFO_df$REL_CU_INDEX)] <- DFO_df$STOCK_CU_INDEX[is.na(DFO_df$REL_CU_INDEX)]

#' 3) "Seapen" released (RELEASE_STAGE_NAME == "Seapen")
#' Steph and Eric: "some cases REL_CU_INDEX = blank is for seapen released, since
#' maybe in those cases the release can’t be assigned to a CU? But in those cases
#' do we assume the fish will return to the broodstock CU? Eric: Yes."
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
nrow(DFO_df) # 31830


#' * Import the hatchery template from wd_data as a list *
filePSF_l <- hatchery_template_fun(wd_data = wd_data,
                                   filePSFname = "SWP_hatchery_data_template.xlsx")

#
# Create SWP_hatchery_data_DATE.xlsx  ----------
#' Create a dataframe with the name of the columns in PSF_modified_SEP_releases_DATE.xlsx
#' and corresponding column names and sheets in the survey file SWP_hatchery_data_...xlsx
matchCol_df <- matching_columns_fun(wd_data = wd_data,
                                    wd_spawner_surveys_data = wd_spawner_surveys_data,
                                    DFO_df = DFO_df)

# make a copy of filePSF_l that is going to be filled
filePSFnew_l <- filePSF_l

#' Make a dataframe to report the case where there are multiple cuid_broodstock 
#' for a same release_site_name-release_stage-release_site_CUID-release_date
#' combination --> to send to Katy,
cuid_broodstock_multi <- NULL

# Fill filePSF_l with new data
for(sheet_i in 2:length(names(filePSF_l))){   # The 1st sheet is to be filled by hand or not at all (QUESTION)
  
  # sheet_i <- 2
  sheetName <- names(filePSF_l)[sheet_i]
  sheetNew <- filePSF_l[[sheet_i]]
  
  # subset matchCol_df for the current sheet
  # matchCol_df_cut <- matchCol_df[matchCol_df$PSF_sheet == sheetName,]
  
  # 
  field_PSF <- matchCol_df$PSF_colnames[matchCol_df$PSF_sheet == sheetName]
  field_DFO <- matchCol_df$DFO_colnames[matchCol_df$PSF_sheet == sheetName]
  field_DFO <- field_DFO[!is.na(field_DFO)]
  #
  if(sheetName == "DataEntry_facilities"){ # sheet 2
    
    #
    sheetNew <- DFO_df[,field_DFO]
    
    colnames(sheetNew) <- field_PSF[field_PSF != "facilityid"] # "program" "project" "facilityname" "facility_latitude" "facility_longitude" "startyear"  "endyear"  
    
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
    # sheetNew[sheetNew$program == "NA",]  # QUESTION: is that normal?
    # sheetNew[sheetNew$facilityname == "Omega Pacific H",]
    
    #' Define facilityid/facilityID:
    #' Katy: facilityid uniquely identifies “program-project-facilityname-facility_latitude-facility_longitude-startyear-endyear”
    #' Katy: facilityid only lives in this file so it can be defined here.
    sheetNew$facilityid <- 1:nrow(sheetNew)

    # Reorder columns
    sheetNew <- sheetNew[,field_PSF]
    
    # check if multiple coordinate values are attributed to a unique facilityname
    sheetNew_cut <- sheetNew[,c("facilityname","facility_latitude","facility_longitude")]
    for(fn_i in 1:length(unique(sheetNew_cut$facilityname))){
      # fn_i <- 1
      fn_here <- unique(sheetNew_cut$facilityname)[fn_i]
      sheetNew_cut2 <- sheetNew_cut[sheetNew_cut$facilityname == fn_here,]
      sheetNew_cut2 <- unique(sheetNew_cut2)
      if(nrow(sheetNew_cut2) > 1){
        print(" WARNING: The following facility has multiple coordinate values (and should not):")
        print(sheetNew_cut2)
      }
    }
    
    
  }else if(sheetName == "DataEntry_facilitiescuids"){
    
    #' find the facilityid/ID corresponding to each unique combination of 
    #' program-project-facilityname:
    field_DFO <- c("PROGRAM_CODE","PROJ_NAME","FACILITY_NAME","REL_CU_INDEX")
    sheetNew <- DFO_df[,field_DFO]
    colnames(sheetNew) <- c("program","project","facilityname","cu_index") # "cu_index" in conservationunits_decoder = the CU_INDEX
    sheetNew$program <- program_acronym_fun(prog_acro =  sheetNew$program)
    sheetNew <- merge(x = sheetNew, 
                      y = filePSFnew_l$DataEntry_facilities[,c("facilityid","program","project","facilityname")],
                      by = c("program","project","facilityname"), 
                      all = T)
    
    # remove unnecessary columns and update their names
    sheetNew <- sheetNew[,c("facilityid","cu_index")]  
    colnames(sheetNew)[colnames(sheetNew) == "facilityid"] <- "facilityID"
    
    # order rows
    sheetNew <- sheetNew[order(sheetNew$facilityID),]
    
    # remove duplicated rows
    sheetNew <- unique(sheetNew)
    
    # convert CUID (i.e. REL_CU_INDEX) to the PSE cuid
    sheetNew$CUID <- cui_cu_index_conservation_units_fun(cu_index = sheetNew$cu_index,
                                                         conservation_units = conservationunits_decoder)
    
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
    
    #' there are duplicated rows:
    #' TODO: remove them from now but deal with it with Katy and co.
    # nrow(sheetNew) # 31830
    # nrow(DFO_df)   # 31830
    # sum(duplicated(sheetNew)) # 134
    # sum(duplicated(DFO_df))   # 0
    # 
    # rowsDuplicating <- which(duplicated(sheetNew) | duplicated(sheetNew, fromLast = TRUE))
    # View(sheetNew[rowsDuplicating,])
    # View(DFO_df[rowsDuplicating,field_DFO])
    # View(DFO_df[rowsDuplicating,])
    # 
    # sum(duplicated(DFO_df)) # 0
    # sum(duplicated(DFO_df[,colnames(DFO_df) != "MRP_TAGCODE"]))     # 35
    # sum(duplicated(DFO_df[,colnames(DFO_df) != "RELEASE_COMMENT"])) # 0   ?!
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","RELEASE_COMMENT")]))  # 48
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","BROOD_YEAR")]))       # 41
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","AVE_WEIGHT")]))       # 40
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","NoTagClip")]))        # 39
    # sum(duplicated(DFO_df[,! colnames(DFO_df) %in% c("MRP_TAGCODE","RELEASE_COMMENT","BROOD_YEAR","AVE_WEIGHT","NoTagClip")]))  # 69
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
    # threshold <- sum(duplicated(DFO_df[,!colnames(DFO_df) %in% colSelected])) # 100
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
                                                                      conservation_units = conservationunits_decoder)
    sheetNew$cuid_broodstock <- cui_cu_index_conservation_units_fun(cu_index = sheetNew$cuid_broodstock, 
                                                                    conservation_units = conservationunits_decoder)
    
    # reorder columns and rows
    sheetNew <- sheetNew[field_PSF]
    sheetNew <- sheetNew[order(sheetNew$facilityID),]
    
    # check if multiple coordinate values are attributed to a unique release_site_name
    sheetNew_cut <- sheetNew[,c("release_site_name","release_site_latitude","release_site_longitude")]
    for(rs_i in 1:length(unique(sheetNew_cut$release_site_name))){
      # rs_i <- 1
      rs_here <- unique(sheetNew_cut$release_site_name)[rs_i]
      sheetNew_cut2 <- sheetNew_cut[sheetNew_cut$release_site_name == rs_here,]
      sheetNew_cut2 <- unique(sheetNew_cut2)
      if(nrow(sheetNew_cut2) > 1){
        print("WARNING: in DataEntry_releases")
        print("The following release site has multiple coordinate values (and should not):")
        print(sheetNew_cut2)
      }
    }
    
    #' sum total_release for a same combination of (i) release_site_name, 
    #' (ii) release_stage, (iii) release_site_CUID and (iv) release_date:
    nrow(sheetNew) # 31830
    sheetNew_l <- list()
    count <- 1
    for(rsn in unique(sheetNew$release_site_name)){
      # rsn <- unique(sheetNew$release_site_name)[1]
      sheetNew_rsn <- sheetNew[sheetNew$release_site_name == rsn,]
      for(rs in unique(sheetNew_rsn$release_stage)){
        # rs <- unique(sheetNew_rsn$release_stage)[1]
        sheetNew_rsn_rs <- sheetNew_rsn[sheetNew_rsn$release_stage == rs,]
        for(cuid in unique(sheetNew_rsn_rs$release_site_CUID)){
          # cuid <- unique(sheetNew_rsn_rs$release_site_CUID)[1]
          sheetNew_rsn_rs_cuid <- sheetNew_rsn_rs[sheetNew_rsn_rs$release_site_CUID == cuid,]
          for(rd in unique(sheetNew_rsn_rs_cuid$release_date)){
            # rd <- unique(sheetNew_rsn_rs_cuid$release_date)[1]
            sheetNew_rsn_rs_cuid_rd <- sheetNew_rsn_rs_cuid[sheetNew_rsn_rs_cuid$release_date == rd,]
            slice <- sheetNew_rsn_rs_cuid_rd[1,,drop = F]
            slice$total_release <- sum(sheetNew_rsn_rs_cuid_rd$total_release)
            sheetNew_l[[count]] <- slice
            count <- count + 1
            # CHECK UP
            if(length(unique(sheetNew_rsn_rs_cuid_rd$cuid_broodstock)) > 1){
              print("WARNING: in DataEntry_releases")
              print("The following release_site_name-release_stage-release_site_CUID-release_date combination has multiple cuid_broodstock:")
              print("Ask Katy to know what to do")
              print(sheetNew_rsn_rs_cuid_rd)
              
              if(is.null(cuid_broodstock_multi)){
                cuid_broodstock_multi <- sheetNew_rsn_rs_cuid_rd
              }else{
                cuid_broodstock_multi <- rbind(cuid_broodstock_multi,sheetNew_rsn_rs_cuid_rd)
              }
            }
          }
        }
      }
    }
    sheetNew <- do.call(rbind,sheetNew_l)
    nrow(sheetNew) # 22441
    
    # remove duplicted rows (SEE TODO ABOVE ABOUT THAT)
    # nrow(DFO_df)
    # nrow(sheetNew) - nrow(unique(sheetNew))
    sheetNew <- unique(sheetNew)
    # sheetNew[sheetNew$cuid_broodstock != sheetNew$release_site_CUID,]
    
    # "release_site_name" ("RELEASE_SITE_NAME")
    # QUESTION: unabbreviate names, e.g., 'Adams R Up' --> 'Adams River Upper' --> wait to hear from Katy
    #' TODO: QUESTION: Should I do it? is that correct? Am I missing something?
    # release_site_abbrev <-        c("Cr","R","Up","Low","Sl","N","S","E","W","LK")
    # names(release_site_abbrev) <- c("Creek","River","Upper","Lower","Slough","North","South","East","West","Lake")
    
  }
  filePSFnew_l[[sheet_i]] <- sheetNew
}


#' 
colOrder <- c("species","release_site_latitude","release_site_longitude","facilityID",
              "release_site_name","release_stage","release_site_CUID","release_date",
              "cuid_broodstock","total_release")

# write.csv(cuid_broodstock_multi[,colOrder],paste0(wd_output,"/cuid_broodstock_multi.csv"),
#           row.names = F)

#' In sheet DataEntry_releases, remove the row with NA values for release_site_latitude
#' and release_site_longitude and places these in a new additional sheet
toKeep <- !is.na(filePSFnew_l$DataEntry_releases$release_site_latitude) & 
  !is.na(filePSFnew_l$DataEntry_releases$release_site_latitude)
toKeepNot <- !toKeep
DataEntry_releases_noNA <- filePSFnew_l$DataEntry_releases[toKeep,]
DataEntry_releases_NA <- filePSFnew_l$DataEntry_releases[toKeepNot,]
filePSFnew_l$DataEntry_releases <- DataEntry_releases_noNA
filePSFnew_l$DataEntry_releases_NAcoord <- DataEntry_releases_NA

#' In sheet DataEntry_facilitiescuids, remove the facilites (i.e., facilityID) 
#' that do not have coordinate in sheet DataEntry_facilities and places these in 
#' a new additional sheet

cond <- !is.na(filePSFnew_l$DataEntry_facilities$facility_latitude) &
  !is.na(filePSFnew_l$DataEntry_facilities$facility_longitude)
facilityIDtoKeep <- filePSFnew_l$DataEntry_facilities$facilityid[cond]

cond <- filePSFnew_l$DataEntry_facilitiescuids$facilityID %in% facilityIDtoKeep
DataEntry_facilitiescuids_noNA <- filePSFnew_l$DataEntry_facilitiescuids[cond,]
DataEntry_facilitiescuids_NA <- filePSFnew_l$DataEntry_facilitiescuids[!cond,]

filePSFnew_l$DataEntry_facilitiescuids <- DataEntry_facilitiescuids_noNA
filePSFnew_l$DataEntry_facilitiescuids_NAcoord <- DataEntry_facilitiescuids_NA

#
# Ad column release_type_pse in DataEntry_releases ------ 
#' Cf. Pop meeting April 3rd 2024
#' https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing

release_type_df <- release_type_pse_fun()

filePSFnew_l$DataEntry_releases$release_type_pse <- NA

for(i in 1:nrow(release_type_df)){
  rs <- release_type_df$release_stage[i]
  rss <- release_type_df$release_type_pse[i]
  cond <- filePSFnew_l$DataEntry_releases$release_stage == rs
  filePSFnew_l$DataEntry_releases$release_type_pse[cond] <- rss
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
  sheet <- as.data.frame(filePSFnew_l[[sheetName]])
  write.xlsx(sheet, 
             file = paste0(wd_output,"/SWP_hatchery_data_",date,".xlsx"),
             sheetName = sheetName, 
             row.names = FALSE,
             append = append,
             showNA = T)
  print(sh_i)
}

# Notes for Katy
# - 1) I implemented a CHECK to check that facilityname have a unique combination of facility_latitude and facility_longitude --> they do
# - 2) I implemented a CHECK to check that release_site_name have a unique combination of release_site_latitude and release_site_longitude --> they do
# - 3) I sum total_release for a same combination of (i) release_site_name, (ii) release_stage, (iii) release_site_CUID and (iv) release_date
# - 4) I implemented a CHECK in 3) above to very if multiple cuid_broodstock are present in a single combinations --> THERE ARE (wait to hear from Katy)
# - 5) In sheet DataEntry_releases, I removed the row with NA values for release_site_latitude and release_site_longitude and places these in a new additional sheet called DataEntry_releases_NAcoord
# - 6) In sheet DataEntry_facilitiescuids, I removed the facilites (i.e., facilityID) that do not have coordinate in sheet DataEntry_facilities and placed these in a new additional sheet called DataEntry_facilitiescuids_NAcoord
# - 7) I cannot do anything about the 1st sheet ??? progratically, it has to be copy pasted by hand from the template and then filled by hand

#
# Edit release_type_pse for Transboundary and steelhead (ONE TIME FIX?) -----------

#' #'Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- F

#' Import dataset384_output
dataset384_output <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[12],
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)

nrow(dataset384_output) # 1480
head(dataset384_output)
unique(dataset384_output$species_name)
unique(dataset384_output$region)

#'* create location_name_pse *
dataset384_output$location_name_pse <- dataset384_output$locationname

cond <- grepl(" [R|r]",dataset384_output$location_name_pse) & !grepl(" RIVER",dataset384_output$location_name_pse)
dataset384_output$location_name_pse[cond]
dataset384_output$location_name_pse[cond] <- gsub(" R"," River",dataset384_output$location_name_pse[cond])

cond <- grepl(" Lk",dataset384_output$location_name_pse)
dataset384_output$location_name_pse[cond]
dataset384_output$location_name_pse[cond] <- gsub(" Lk"," Lake",dataset384_output$location_name_pse[cond])

cond <- grepl(" Cr",dataset384_output$location_name_pse)
dataset384_output$location_name_pse[cond]
dataset384_output$location_name_pse[cond] <- gsub(" Cr"," Creek",dataset384_output$location_name_pse[cond])

cond <- grepl(" Pd",dataset384_output$location_name_pse)
dataset384_output$location_name_pse[cond]

cond <- grepl(" In",dataset384_output$location_name_pse)
dataset384_output$location_name_pse[cond]

cond <- grepl(" Ch",dataset384_output$location_name_pse)
dataset384_output$location_name_pse[cond]


#'* Add release_type_pse *
release_type_df <- release_type_pse_fun()

dataset384_output$release_type_pse <- NA

for(i in 1:nrow(release_type_df)){
  rs <- release_type_df$release_stage[i]
  rss <- release_type_df$release_type_pse[i]
  cond <- dataset384_output$release_stage == rs
  dataset384_output$release_type_pse[cond] <- rss
}


#'* create the different datasets *
dataset384_output_SH <- dataset384_output[dataset384_output$species_name == "Steelhead",]
nrow(dataset384_output_SH) # 1369

dataset384_output_TB <- dataset384_output[dataset384_output$region == "Transboundary",]
nrow(dataset384_output_TB) # 111

dataset384_output_rest <- dataset384_output[dataset384_output$region != "Transboundary",]
dataset384_output_rest <- dataset384_output_rest[dataset384_output_rest$species_name  != "Steelhead",]

unique(dataset384_output_SH$release_stage)
unique(dataset384_output_TB$release_stage)
unique(dataset384_output_rest$release_stage)

unique(dataset384_output_SH$release_type_pse)
unique(dataset384_output_TB$release_type_pse)

#' * Corrections *
#' Remove the following two data points from dataset384_output_SH
cond <- dataset384_output_SH$release_stage %in% c("Catchable","2 years") # get ride of them PLUS they don't match the value in the PSE
dataset384_output_SH[cond,]
dataset384_output_SH <- dataset384_output_SH[!cond,]

# export the file
date <- Sys.Date()
date <- gsub(pattern = "-",replacement = "",x = date)
#date <- "20240404"
write.csv(dataset384_output_SH,paste0(wd_output,"/dataset384_output_SH_",date,".csv"),
          row.names = F)
write.csv(dataset384_output_TB,paste0(wd_output,"/dataset384_output_TB_",date,".csv"),
          row.names = F)

#
# Add location_name_pse to sheet DataEntry_releases in SWP_hatchery_data_20240404.xlsx (ONE TIME FIX?) -----
#' 
DataEntry_relase <- read.xlsx(file = paste0(wd_output,"/SWP_hatchery_data_20240404.xlsx"),
                              sheetName = "DataEntry_releases")

DataEntry_relase$location_name_pse <- DataEntry_relase$release_site_name

# Replace "/" by " " (it is important to do it 1st)
DataEntry_relase$location_name_pse <- gsub("/"," ",DataEntry_relase$location_name_pse)

# Replace "+" by " + " (it is important to do it 1st)
DataEntry_relase$location_name_pse <- gsub("+"," + ",DataEntry_relase$location_name_pse, 
                                           fixed = T)

# Remove the double spaces
DataEntry_relase$location_name_pse <- gsub("  "," ",DataEntry_relase$location_name_pse)

# replace the abbreviations by their full name
abbreviations_df # in functions_general.R

for(r in 1:nrow(abbreviations_df)){
  # r <- 1
  full_name <- character_replace_fun(charToChange = abbreviations_df$abbrevation[r],
                                     charNew = abbreviations_df$word_full[r], 
                                     name_vector = DataEntry_relase$location_name_pse,
                                     print = F)
  
  DataEntry_relase$location_name_pse <- full_name$name_vector_new
}

unique(DataEntry_relase$location_name_pse)

# Change the Acronyms:
# https://www.marinescience.psf.ca/wp-content/uploads/2023/05/LFR_ReleaseStrategyEvaluationBC_16July2021-Cover-Screen.pdf
# https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40594361.pdf
acronyms_df # in functions_general.R

for(r in 1:nrow(acronyms_df)){
  # r <- 1
  char <- acronyms_df$acronym[r]
  cond <- grepl(char,DataEntry_relase$location_name_pse)
  unique(DataEntry_relase$location_name_pse[cond])
  char_new <- paste0("(",acronyms_df$word_full[r],")")
  DataEntry_relase$location_name_pse[cond] <- gsub(char,char_new,DataEntry_relase$location_name_pse[cond])
}

# Extra corrections:
char <- "@Duncan"  # 
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Duncan)",DataEntry_relase$location_name_pse[cond])

char <- "-use6501"  #
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char,"",DataEntry_relase$location_name_pse[cond])

char <- "Culvert 150 Creek"
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char,"Culvert Creek",DataEntry_relase$location_name_pse[cond])


# Check 
View(unique(data.frame(DataEntry_relase$location_name_pse)))

# QUESTION: what to do with these ones?
# https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1714522371999499?thread_ts=1712267027.385489&cid=C03LB7KM6JK
#' - Three B Channel --> ???
#' - Br 100 Swamp  --> Bridge 100 Swamp???
#' - 28 Mile Creek --> CORRECT
#' - Branch 10 Creek --> CORRECT
#' - Ed Leon Slough --> CORRECT
#' - Ink Lake --> CORRECT

pattern <- "Cowichan R (Duncan)"
cond <- grepl(pattern,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
unique(DataEntry_relase[cond,c("location_name_pse","release_site_latitude","release_site_longitude")])


# remove Transboundary data
cuid_toRemove <- conservationunits_decoder$cuid[conservationunits_decoder$region == "Transboundary"]
DataEntry_relase <- DataEntry_relase[! DataEntry_relase$cuid_broodstock %in% cuid_toRemove,]

#
date <- Sys.Date()
date <- gsub(pattern = "-",replacement = "",x = date)
date <- "20240404"
write.csv(DataEntry_relase,paste0(wd_output,"/DataEntry_relase_noTB_",date,".csv"),
          row.names = F)

# related slack thread:
# https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1713375227574009?thread_ts=1712267027.385489&cid=C03LB7KM6JK

#
# CHECK: Compare dataset384_output_TB to the TB in SWP_hatchery_data_20240404.xlsx -----
#' 
#' Related slack thread: TODO: still need to be addressed.
#' https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1713377494509899

DataEntry_relase <- read.xlsx(file = paste0(wd_output,"/SWP_hatchery_data_20240404.xlsx"),
                              sheetName = "DataEntry_releases")
head(DataEntry_relase)

#' Check if there are cuid in dataset384_output that are not in 
#' SWP_hatchery_data_20240404.xlsx
unique(dataset384_output_TB$cuid)[!unique(dataset384_output_TB$cuid) %in% unique(DataEntry_relase$cuid_broodstock)] 
#' There are two: 1028 1033
#' This is not unexpected because these come from another source than the DFO one.

#' Check if there are facilityid in SWP_hatchery_data_20240404.xlsx not in 
#' dataset384_output
#' --> need to create the field region using conservationunits_decoder
cuid_TB <- unique(conservationunits_decoder$cuid[conservationunits_decoder$region == "Transboundary"])
DataEntry_relase_TB <- DataEntry_relase[DataEntry_relase$cuid_broodstock %in% cuid_TB,]
unique(DataEntry_relase_TB$cuid_broodstock)[!unique(DataEntry_relase_TB$cuid_broodstock) %in% unique(dataset384_output_TB$cuid)] 
#' There is one: 1017, which is not normal.
DataEntry_relase_TB[DataEntry_relase_TB$cuid_broodstock == 1017,]
# TO REMOVE there is an issue with the facilityID (cf. Population meeting 16/04/2024)
# https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing
# I told Katy to do so there:
# 

# Make a simpler column for species name in dataset384_output
dataset384_output_TB$species_name_simple <- dataset384_output_TB$species_name
dataset384_output_TB$species_name_simple <- gsub("Lake s","S",dataset384_output_TB$species_name_simple)
dataset384_output_TB$species_name_simple <- gsub("River s","S",dataset384_output_TB$species_name_simple)

colnames(dataset384_output_TB)
colnames(DataEntry_relase_TB)

unique(dataset384_output_TB$locationname)
unique(DataEntry_relase_TB$release_site_name)

#' * Fix Tatsaminie vs. Tatsaminie LK *
#' Coordinate differ a bit
#' For Tatsaminie issue between VESTA and Katy --> there were multiple locations 
#' combined into one.
#' https://www.dropbox.com/s/mllqe0jfk5el1yn/Transboundary_Hatchery%20Data%20Sources%20%26%20Processing.docx?dl=0
unique(dataset384_output_TB[,c("latitude","longitude")][dataset384_output_TB$locationname == "Tatsamenie",])
unique(DataEntry_relase_TB[,c("release_site_latitude","release_site_longitude")][DataEntry_relase_TB$release_site_name == "Tatsamenie Lk",])
DataEntry_relase_TB$release_site_name[DataEntry_relase_TB$release_site_name == "Tatsamenie Lk"] <- "Tatsamenie"

#' * Compare the datasets *
col_384 <-       c("species_name_simple","cuid","locationname","release_stage")  # facilityid
col_DataEntry <- c("species","cuid_broodstock","release_site_name","release_stage") # facilityID

cuid_facility_yr_384 <- unique(dataset384_output_TB[,col_384])
cuid_facility_yr_DataEntry <- unique(DataEntry_relase_TB[,col_DataEntry])

# Data in common:
data_merged_all <- merge(x = dataset384_output_TB[,c(col_384,"year","total_release")], 
                        y = DataEntry_relase_TB[,c(col_DataEntry,"release_date","total_release")],
                        by.x = c(col_384,"year"), by.y = c(col_DataEntry,"release_date"), all = T)
data_merged_all

# Data in dataset384_output_TB not in DataEntry_relase_TB
cond_384_DERno <- !is.na(data_merged_all$total_release.x) & is.na(data_merged_all$total_release.y)
data_merged_all[cond_384_DERno,c("species_name_simple","cuid","locationname","release_stage","year",
                                         "total_release.x","total_release.y")]
sum(cond_384_DERno)/nrow(data_merged_all) * 100 # 77.19

# Data in DataEntry_relase_TB not in dataset384_output_TB 
cond_384no_DER <- is.na(data_merged_all$total_release.x) & !is.na(data_merged_all$total_release.y)
data_merged_all[cond_384no_DER,c("species_name_simple","cuid","locationname","release_stage","year",
                                         "total_release.x","total_release.y")]
#' As mentioned above this data point can be ignore because there is an issue with
#' the facilityID
sum(cond_384no_DER)/nrow(data_merged_all) * 100 # 0.9

# Data in both
cond_384_DER <- !is.na(data_merged_all$total_release.x) & !is.na(data_merged_all$total_release.y)
data_common <- data_merged_all[cond_384_DER,]
data_common
sum(cond_384_DER)/nrow(data_merged_all) * 100 # 20,2

# Data in both that does not match
cond_diff <- data_common$total_release.x != data_common$total_release.y
data_common[cond_diff,]
sum(cond_diff)/nrow(data_merged_all) * 100 # 8.0

cond <- dataset384_output_TB$year == 2015 &
  dataset384_output_TB$locationname == "Tatsamenie" 
dataset384_output_TB[cond,]

cond <- DataEntry_relase_TB$release_date == 2015 &
  DataEntry_relase_TB$release_site_name == "Tatsamenie" 
DataEntry_relase_TB[cond,]

# 
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

# OLDER CODE -----

# cond <- grepl(" [R|r]",DataEntry_relase$location_name_pse) & !grepl(" RIVER",DataEntry_relase$location_name_pse)
cond <- grepl(" [R|r]",DataEntry_relase$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" [R|r]",l),DataEntry_relase$location_name_pse)
}
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" R"," River",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Lk",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Lk"," Lake",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Cr",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Cr"," Creek",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Cv",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Cv"," Cove",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Pd",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Pd"," Pond",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Ch",DataEntry_relase$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" Ch",l),DataEntry_relase$location_name_pse)
}
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Ch"," Channel",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" In",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" In"," Inlet",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Sl",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Sl"," Slough",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Is",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Is"," Island",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Strm",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Strm"," Stream",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Pk",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Pk"," Peak",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Sp",DataEntry_relase$location_name_pse) & !grepl(" Spit",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Sp"," Spawning",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Cst",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Cst"," Coast",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Val",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Val"," Valley",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Sd",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Sd"," Sound",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Est",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Est"," Estuary",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Wtshd",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Wtshd"," Watershed",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Tribs",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Tribs"," Tributaries",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Trib",DataEntry_relase$location_name_pse) & !grepl(" Tributaries",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Trib"," Tributary",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Div",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Div"," Division",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Hb",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Hb"," Harbour",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Fwy",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Fwy"," Freeway",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" Msh",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" Msh"," Marsh",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" N",DataEntry_relase$location_name_pse) & !grepl(" No",DataEntry_relase$location_name_pse) & !grepl(" Na",DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" N"," North",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" S",DataEntry_relase$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" S",l),DataEntry_relase$location_name_pse)
}
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" S"," South",DataEntry_relase$location_name_pse[cond])

cond <- grepl(" E",DataEntry_relase$location_name_pse)
for(l in letters){
  cond <- cond & !grepl(paste0(" E",l),DataEntry_relase$location_name_pse)
}
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(" E"," East",DataEntry_relase$location_name_pse[cond])


char <- "UPFR"
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Upper Fraser)",DataEntry_relase$location_name_pse[cond])

char <- "LWFR"
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Lower Fraser)",DataEntry_relase$location_name_pse[cond])

char <- "JNST"
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Johnstone Strait)",DataEntry_relase$location_name_pse[cond])

char <- "TOMF"  # Upper Thompson ?!
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Upper Thompson)",DataEntry_relase$location_name_pse[cond])

char <- "TOMM"  #= Lower Thompson ?!
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Lower Thompson)",DataEntry_relase$location_name_pse[cond])

char <- "CCST"  # 
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Central Coast)",DataEntry_relase$location_name_pse[cond])

char <- "SKNA"  # 
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Skeena River)",DataEntry_relase$location_name_pse[cond])

char <- "SWVI"  # 
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Southwest Vancouver Island)",DataEntry_relase$location_name_pse[cond])

char <- "NWVI"  #
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Northwest Vancouver Island)",DataEntry_relase$location_name_pse[cond])

char <- "GSMN"  # 
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Strait of Georgia Mainland)",DataEntry_relase$location_name_pse[cond])

char <- "GSVI"  # 
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Strait of Georgia Vancouver Island)",DataEntry_relase$location_name_pse[cond])

char <- "QCI"  # "Queen Charlotte Islands", which is now Haida Gwaii
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (Haida Gwaii)",DataEntry_relase$location_name_pse[cond])

char <- "NCST"  #
cond <- grepl(char,DataEntry_relase$location_name_pse)
unique(DataEntry_relase$location_name_pse[cond])
DataEntry_relase$location_name_pse[cond] <- gsub(char," (North Coast)",DataEntry_relase$location_name_pse[cond])

