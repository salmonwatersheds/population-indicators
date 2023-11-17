
#' Function that returns the column names in the PSF file 
#' (e.g., PSF_modified_SEP_releases_2023.xlsx), and the corresponding column 
#' names and sheet name in the survery files (e.g., SWP_hatchery_data_template.xlsx).
#' wd_spawner_surveys_data is where conservation-units.csv
#' TODO: place conservation-units.csv in the population-indicators head folder?
matching_columns_fun <- function(wd_data,wd_spawner_surveys_data,DFO_df = NA){
  
  
  if(is.na(DFO_df)[1]){
    # import most recent PSF file:
    DFO_df <- return_file_lastVersion_fun(wd_data,
                                          pattern = "PSF_modified_SEP_releases")
  }

  # Make a dataframe with all the fields to match
  DataEntry_facilities <- c("facilityid","program","project","facilityname",
                            "facility_latitude","facility_longitude",
                            "startyear","endyear")
  DataEntry_facilitiescuids <- c("facilityID","CUID")
  DataEntry_releases <- c("species","release_site_latitude","release_site_longitude",
                          "release_site_name","release_stage","release_site_CUID",
                          "facilityID","cuid_broodstock","release_date","total_release")
  
  out <- data.frame(PSF_colnames = c(DataEntry_facilities,
                                        DataEntry_facilitiescuids,
                                        DataEntry_releases),
                    PSF_sheet = c(rep("DataEntry_facilities",length(DataEntry_facilities)),
                                     rep("DataEntry_facilitiescuids",length(DataEntry_facilitiescuids)),
                                     rep("DataEntry_releases",length(DataEntry_releases))),
                    DFO_colnames = NA,
                    comments = NA)
  
  
  #' ** Fields in DataEntry_facilities **
  
  i <- 1
  col <- out$PSF_colnames[i] # facilityid
  out$comments[out$PSF_colnames == col] <- "does it matter which program get which facilityid?"

  i <- i + 1
  col <- out$PSF_colnames[i] # program
  out$DFO_colnames[out$PSF_colnames == col] <- "PROGRAM_CODE"
  out$comments[out$PSF_colnames == col] <- "Is there a way to convert the program name to its code?"
  
  i <- i + 1
  col <- out$PSF_colnames[i] # project
  out$DFO_colnames[out$PSF_colnames == col] <- "PROJ_NAME"

  i <- i + 1
  col <- out$PSF_colnames[i] # facilityname
  out$DFO_colnames[out$PSF_colnames == col] <- "FACILITY_NAME"
  
  i <- i + 1
  col <- out$PSF_colnames[i] # facility_latitude
  out$DFO_colnames[out$PSF_colnames == col] <- "FACILITY_LATITUDE"

  i <- i + 1
  col <- out$PSF_colnames[i] # facility_longitude
  out$DFO_colnames[out$PSF_colnames == col] <- "FACILITY_LONGITUDE"

  i <- i + 1
  col <- out$PSF_colnames[i] # startyear
  out$DFO_colnames[out$PSF_colnames == col] <- "START_DATE"
  out$comments[out$PSF_colnames == col] <- "only keep the year, e.g., '19920814' --> '1992'"
  
  i <- i + 1
  col <- out$PSF_colnames[i] # endyear
  out$DFO_colnames[out$PSF_colnames == col] <- "END_DATE"
  out$comments[out$PSF_colnames == col] <- "only keep the year, e.g., '19920814' --> '1992'"
  
  
  #' ** DataEntry_facilitiescuids **
  
  # facilityID = facilityid', see comment above
  i <- i + 1
  col <- out$PSF_colnames[i] # facilityID
  out$DFO_colnames[out$PSF_colnames == col] <- "FACILITY_NAME"
  out$comments[out$PSF_colnames == col] <- "convert FACILITY_NAME to facilityID with 'DataEntry_facilities/facilityid'"
  
  i <- i + 1
  col <- out$PSF_colnames[i] # CUID
  out$DFO_colnames[out$PSF_colnames == col] <- "REL_CU_INDEX" # not "STOCK_CU_INDEX" confirmed by Eric and Katy
  out$comments[out$PSF_colnames == col] <- "same as DataEntry_releases/release_site_CUID; convert to STOCK_CU_INDEX using conservation-units.csv"
  
  
  #' ** Fields in DataEntry_releases **
  
  i <- i + 1
  col <- out$PSF_colnames[i] # species
  out$DFO_colnames[out$PSF_colnames == col] <- "SPECIES_NAME"
  out$comments[out$PSF_colnames == col] <- "only keep the species, for instance 'Lake Sockeye' --> 'Sockeye'"
  
  i <- i + 1
  col <- out$PSF_colnames[i] # release_site_latitude
  out$DFO_colnames[out$PSF_colnames == col] <- "REL_LATITUDE"

  i <- i + 1
  col <- out$PSF_colnames[i] # release_site_longitude
  out$DFO_colnames[out$PSF_colnames == col] <- "REL_LONGITUDE"
  
  i <- i + 1
  col <- out$PSF_colnames[i] # release_site_name
  out$DFO_colnames[out$PSF_colnames == col] <- "RELEASE_SITE_NAME"
  out$comments[out$PSF_colnames == col] <- "unabbreviate names, e.g., 'Adams R Up' --> 'Adams River Upper'"  # QUESTION: do I need to do that or abbreviations are ok?
  
  i <- i + 1
  col <- out$PSF_colnames[i] # release_stage
  out$DFO_colnames[out$PSF_colnames == col] <- "RELEASE_STAGE_NAME"

  i <- i + 1
  col <- out$PSF_colnames[i] # release_site_CUID
  out$DFO_colnames[out$PSF_colnames == col] <- "REL_CU_INDEX"  # not "STOCK_CU_INDEX" 
  out$comments[out$PSF_colnames == col] <- "same as DataEntry_facilitiescuids/CUID; convert to STOCK_CU_INDEX using conservation-units.csv"
  
  # facilityID = facilityid', see comment above 
  i <- i + 1
  col <- out$PSF_colnames[i] # facilityID
  out$comments[out$PSF_colnames == col] <- "make sure it matches DataEntry_facilities/facilityid"
  
  i <- i + 1
  col <- out$PSF_colnames[i] # cuid_broodstock
  out$DFO_colnames[out$PSF_colnames == col] <- "STOCK_CU_INDEX"
  out$comments[out$PSF_colnames == col] <- "can differ from release_site_CUID ; use STOCK_CU_INDEX ???"
  
  #' QUESTIONS:
  #' - Does the STOCK_CU_INDEX in DFO file corresponds to cuid_broodstock or 
  #' release_site_CUID? (in case these two differ)? (Note that the correspondence 
  #' is done using conservation-units.csv.)
  #' - If cuid_broodstock != release_site_CUID, and indeed STOCK_CU_INDEX corresponds
  #' to cuid_broodstock, how to find release_site_CUID?
  
  i <- i + 1
  col <- out$PSF_colnames[i] # release_date
  out$DFO_colnames[out$PSF_colnames == col] <- "RELEASE_YEAR"
  
  i <- i + 1
  col <- out$PSF_colnames[i] # total_release
  out$DFO_colnames[out$PSF_colnames == col] <- "TotalRelease"

  # unique(DFO_df$TotalRelease)

  # unique(DFO_df$TotalRelease)
  
  # release_site_CUID --> DFO_df$REL_GFE_ID ? QUESTION

  # unique(DFO_df$STOCK_CU_INDEX)  # "SEL-19-60" "CK-60"     "CM-4"      "CO-13" ...  # ERic: to translate to cuid_broodstock == release_site_CUID (?) 
  # unique(DFO_df$STOCK_CU_ID)     # 7170 7308 7318 7361 7283 7277 7321 7360   NA... LOOK LIKE STREAM IDs --> 
  # unique(DFO_df$REL_CU_INDEX)    # "SEL-19-60" NA "CM-4" "CO-13" "CK-34"... 
  # unique(DFO_df$REL_CU_NAME)     # "TANKEEAH RIVER" NA "GEORGIA STRAIT"...
  # QUESTION: how come there is no CUID or cuid in PSF_modified_SEP_releases_2023.xlsx ?!
  
  # C:\Users\bcarturan\Salmon Watersheds Dropbox\Bruno Carturan\X Drive\1_PROJECTS\1_Active\Population Methods and Analysis\population-indicators\spawner-surveys\data
  # conservation-units.csv cu_index == DFO_df$STOCK_CU_INDEX QUESTION
  
  # or Full.CU.Index in Appendix 1.csv  == DFO_df$STOCK_CU_INDEX QUESTION
  
  return(out)
}

#' Function that returns the most recent file from the wd_data repository and using
#' part of the file name (i.e., "pattern")
return_file_lastVersion_fun <- function(wd_data,pattern){
  
  # import most recent PSF file:
  files_list <- list.files(wd_data)
  files_list <- files_list[grepl(pattern = pattern,files_list)]
  
  # in case there is no file
  if(length(files_list) == 0){
    print(paste0("There is no file with pattern '",pattern,"' in ",wd_data))
    break
  }else{
    # if multiple files, select the one with the most recent date modified:
    if(length(files_list) > 1){
      files_dates <- file.info(paste(wd_data,files_list,sep="/"))$mtime
      files_list <- files_list[files_dates == max(files_dates)]
    }
    print(paste("The file selected is: ",files_list))
  }
  
  file_output <- read_excel(paste(wd_data,files_list,sep="/"))
  
  return(file_output)
}


#' Function returning a list of the sheets (as data frames) of the PSF hatchery 
#' template .xlsx file (the default file is SWP_hatchery_data_template.xlsx).
hatchery_template_fun <- function(wd_data,
                                  filePSFname = "SWP_hatchery_data_template.xlsx"){
  
  filePSF_l <- list()
  sheetsNames <- c(
    "DataProvider",
    "DataEntry_facilities",
    "DataEntry_facilitiescuids",
    "DataEntry_releases"
  )
  
  for(i in 1:length(sheetsNames)){
    filePSF_l[[i]] <- read_excel(paste(wd_data,filePSFname,sep = "/"),
                                    sheet = sheetsNames[i])
  }
  names(filePSF_l) <- sheetsNames
  return(filePSF_l)
}


#' Function that takes a vector of 'cu_index' values (or the corresponding DFO 
#' "STOCK_CU_INDEX' and "REL_CU_INDEX") and return the corresponding PSF "cuid"
#' and vice versa. The function requires to supply the conservation_units.csv
#' which comes from the PSF database.
# cu_index <- DFO_df$REL_CU_INDEX
# cuid <- NA
# cu_index <- sheetNew$cu_index
# conservation_units <- conservationunits_decoder
cui_cu_index_conservation_units_fun <- function(cuid = NA, cu_index = NA,
                                                conservation_units){
  
  if(is.na(cuid)){
    input <- cu_index
    colInput <- "cu_index"
    colOutput <- "cuid"
  }else{
    input <- cuid 
    colInput <- "cuid" 
    colOutput <- "cu_index"
  }
  output <- rep(NA,length(input))
  
  for(i in 1:length(unique(input))){
    # i <- 1
    val <- unique(input)[i]
    newVal <- conservation_units[,colOutput][conservation_units[,colInput] == val]
    newVal <- newVal[!is.na(newVal)]
    if(length(newVal) == 0){ # there is not corresponding value
      print(paste0("The value '",val,"' does not have a match so NA is given instead."))
      newVal <- NA
    }
    output[input == val] <- newVal
  }
  
  print(paste0("Returning '",colOutput,"' values."))
  return(output)
}

#' Function that converts the value in df$colToConvert into matching values in 
#' decoder_df$colMatchingVal. The function returns a data frame with the single 
#' column named after "colMatchingVal".
#' - df: the data frame with the column to convert "colToConvert"
#' - decoder_df: the data frame with both the columns "colToConvert" and "colMatchingVal"
# df <- sheetNew
# decoder_df <- conservation_units
# colToConvert <- "cu_index"
# colMatchingVal <- "cuid"
convert_column_match_fun <- function(df,decoder_df,colToConvert,colMatchingVal){
  df_out <- df
  decoder_df_cut <- decoder_df[,c(colToConvert,colMatchingVal)]
  decoder_df_cut <- unique(decoder_df_cut)
  df_out <- merge(x = df_out, 
                  y = decoder_df,
                  by = colToConvert, all.x = T)
  df_out <- df_out[,colMatchingVal,drop = F]
  return(df_out)
}

#' Function that returns a vector of program names corresponding to the supplied 
#' vector of acronyms provided and vice versa.
# prog_acro <- DFO_df$PROGRAM_CODE
# prog_name <- NA
program_acronym_fun <- function(prog_acro = NA, prog_name = NA){
  
  progNameAcro_df <- data.frame(prog_acro = c("AFS","CDP","DPI","OPS","PIP","NA"),
                                prog_name = c("Aboriginal Fisheries Strategy",
                                              "Community Economic Development Program",
                                              "Designated Public Involvement",
                                              "Major Operations",
                                              "Public Involvement Program",
                                              "NA"))
  
  if(is.na(prog_acro)[1]){
    input <- prog_fullName
    inputCol <- "prog_name"
    outputCol <- "prog_acro"
  }else{
    input <- prog_acro
    inputCol <- "prog_acro"
    outputCol <- "prog_name"
  }
  output <- rep(NA,length(input))
  
  # place potential NA by "NA
  input[is.na(input)] <- "NA"
  
  # check of these values are all present in progNameAcro_df
  inputValUnique <- unique(input)
  inputValUnique_notInDf <- inputValUnique[! inputValUnique %in% progNameAcro_df[,inputCol]]
  
  if(length(inputValUnique_notInDf) > 0){
    if(is.na(prog_acro)[1]){
      print(paste0("The following program name(s) is(are) not the list of know programs: '",inputValUnique_notInDf,"'"))
      print("Consequently no acronym is given.")
    }else{
      print(paste0("The following program acronym(s) is(are) not the list of know programs: '",inputValUnique_notInDf,"'"))
      print("Consequently no program name is given.")
    }
    
    progNameAcro_df_add <- data.frame(prog_acro = inputValUnique_notInDf,
                                      prog_name = inputValUnique_notInDf)
    progNameAcro_df <- rbind(progNameAcro_df,progNameAcro_df_add)
  }
  
  # match each input value with its corresponding output
  for(i in 1:length(unique(input))){
    # i <- 1
    val <- unique(input)[i]
    valNew <- progNameAcro_df[,outputCol][progNameAcro_df[,inputCol] == val]
    output[input == val] <- valNew
  }
  
  return(output)
}


