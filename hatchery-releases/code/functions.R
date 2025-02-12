
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
#' USE import_mostRecent_file_fun() instead in population-indicators/code/functions_general.R
# return_file_lastVersion_fun <- function(wd_data,pattern){
#   
#   # import most recent PSF file:
#   files_list <- list.files(wd_data)
#   files_list <- files_list[grepl(pattern = pattern,files_list)]
#   
#   # in case there is no file
#   if(length(files_list) == 0){
#     print(paste0("There is no file with pattern '",pattern,"' in ",wd_data))
#     break
#   }else{
#     # if multiple files, select the one with the most recent date modified:
#     if(length(files_list) > 1){
#       files_dates <- file.info(paste(wd_data,files_list,sep="/"))$mtime
#       files_list <- files_list[files_dates == max(files_dates)]
#     }
#     print(paste("The file selected is: ",files_list))
#   }
#   
#   file_output <- read_excel(paste(wd_data,files_list,sep="/"))
#   
#   return(file_output)
# }


#' Function returning a list of the sheets (as data frames) of the PSF hatchery 
#' template .xlsx file (the default file is SWP_hatchery_data_template.xlsx).
hatchery_template_fun <- function(wd_data,
                                  filePSFname = "SWP_hatchery_data_template.xlsx",
                                  asDataFrame = F){
  
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
    if(asDataFrame){
      filePSF_l[[i]]<- as.data.frame(filePSF_l[[i]])
    }
  }
  names(filePSF_l) <- sheetsNames
  return(filePSF_l)
}


#' Function that takes a vector of 'cu_index' values (or the corresponding DFO 
#' "STOCK_CU_INDEX' and "REL_CU_INDEX") and return the corresponding PSF "cuid"
#' and vice versa. The function requires to supply the conservationunits_decoder.csv
#' which comes from the PSF database.
# cu_index <- DFO_df$REL_CU_INDEX
# cuid <- NA
# cu_index <- sheetNew$cu_index
# conservation_units <- conservationunits_decoder
cuid_cu_index_conservation_units_fun <- function(cuid = NA, cu_index = NA,
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
    # i <- which(grepl("PK",unique(input)))[1]
    val <- unique(input)[i]
    cond <- conservation_units[,colInput] == val & !is.na(conservation_units[,colInput])
    newVal <- conservation_units[,colOutput][cond]
    # newVal <- newVal[!is.na(newVal)]
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

#' Function to create the field "release_type_simple" from "release_stage":
release_type_pse_fun <- function(){
  
  release_type_df <- data.frame(release_stage = c("Fed Fry","Fed Fall",
                                                  "Smolt 1+","Smolt 0+",
                                                  "Unfed","Chan Fry",
                                                  "Eyed Egg",
                                                  "Seapen 0+","Seapen","Seapen 1+",
                                                  "Nat Fry",
                                                  "Nat Sm 0+",
                                                  "Nat Emerg",
                                                  "Nat Sm 1+",
                                                  "Seapen 2+",
                                                  "Fall Fry",
                                                  "Parr",
                                                  "Fry",
                                                  "Smolt",
                                                  "Fingerling",
                                                  "Yearling",
                                                  "Egg",
                                                  "Unknown"),
                                release_type_pse = c("Fry","Fry",
                                                     "Smolt","Smolt",
                                                     "Fry","Fry",
                                                     "Egg",
                                                     "Seapen","Seapen","Seapen",
                                                     "Fry",
                                                     "Smolt",
                                                     "Fry",
                                                     "Smolt",
                                                     "Seapen",
                                                     "Fry",
                                                     "Smolt",
                                                     "Fry",
                                                     "Smolt",
                                                     "Smolt",
                                                     "Smolt",
                                                     "Egg",
                                                     "Unknown"
                                                     ))
  
  release_type_df <- release_type_df[order(release_type_df$release_type_pse),]
  
  return(release_type_df)
}

#' Function to find the cuid and cu_index (NuSEDS's FULL_CU_IN) of the DFO's 
#' hatchery dataset using (1) the cleaned PSE's NuSEDS dataset and (2) region 
#' and CU PSE's geo files.
# nuseds_cleaned <- nuseds
# hatchery_DFO <- DFO_df_all
# cu_index_type <- "REL_CU_INDEX" # "STOCK_CU_INDEX"
# percent_increment <- 1
find_CU_hatchery_fun <- function(hatchery_DFO,   # DFO's hatchery file
                                 nuseds_cleaned, # PSF cleaned NuSEDS data with cuid
                                 cu_index_type = c("STOCK_CU_INDEX","REL_CU_INDEX"),
                                 DFO_All_Streams_Segments,
                                 CUs_gdb, 
                                 regions_shp,
                                 percent_increment = 1){
  
  #' Add the column species_qualified to be able to differentiate between PKE and
  #' PKO to avoid issues when attributing cu_index to the time series. Note that 
  #' we could attempt to also differentiate between SER and SEL but that is not 
  #' necessary and it could be a bit of a pain when dealing with NAs in the different 
  #' informative fields.
  hatchery_DFO$species_qualified <- NA
  hatchery_DFO$species_qualified[hatchery_DFO$SPECIES_NAME == "Coho"] <- "CO"
  hatchery_DFO$species_qualified[hatchery_DFO$SPECIES_NAME == "Chinook"] <- "CK"
  hatchery_DFO$species_qualified[hatchery_DFO$SPECIES_NAME == "Sockeye"] <- "SE"
  hatchery_DFO$species_qualified[hatchery_DFO$SPECIES_NAME == "Chum"] <- "CM"
  hatchery_DFO$species_qualified[hatchery_DFO$SPECIES_NAME == "Steelhead"] <- "SH"
  cond_PKE <- hatchery_DFO$SPECIES_NAME == "Pink" & hatchery_DFO$BROOD_YEAR %% 2 == 0
  cond_PKO <- hatchery_DFO$SPECIES_NAME == "Pink" & hatchery_DFO$BROOD_YEAR %% 2 == 1  
  hatchery_DFO$species_qualified[cond_PKE] <- "PKE"
  hatchery_DFO$species_qualified[cond_PKO] <- "PKO"
  
  # select the rows with NAs for cu_index_type selected:
  cond_na <- is.na(hatchery_DFO[,cu_index_type])
  sum(cond_na) # 2374 2981
  sum(cond_na)/nrow(hatchery_DFO) * 100 # 6.8%
  
  col_cu <- c("SPECIES_NAME","species_qualified")
  
  # select the appropriate columns
  if(cu_index_type == "STOCK_CU_INDEX"){
    
    col_cu <- c(col_cu,"STOCK_NAME","STOCK_CU_INDEX","RUN_NAME",
                "STOCK_GFE_NAME","STOCK_GFE_ID","STOCK_LATITUDE","STOCK_LONGITUDE")
    
  }else if(cu_index_type == "REL_CU_INDEX"){
    
    # create 
    hatchery_DFO$REL_GFE_NAME <- sapply(hatchery_DFO$REL_GFE_ID,function(gfeid){
      if(!is.na(gfeid)){
        cond <- DFO_All_Streams_Segments$ID == gfeid
        if(any(cond)){
          out <- DFO_All_Streams_Segments$NME[cond] |> unique()
        }else{
          out <- NA
        }
      }else{
        out <- NA
      }
      return(out)
    })
    
    col_cu <- c(col_cu,"REL_CU_NAME","REL_CU_INDEX","RUN_NAME",
                "REL_GFE_NAME","REL_GFE_ID","REL_LATITUDE","REL_LONGITUDE")
  }
  
  col_CU_INDEX <- col_cu[grepl("CU_INDEX",col_cu)]
  col_X_LONGT <- col_cu[grepl("LONGIT",col_cu)]
  col_Y_LAT <- col_cu[grepl("LATIT",col_cu)]
  col_GFE_ID <- col_cu[grepl("GFE_ID",col_cu)]
  col_GFE_NAME <- col_cu[grepl("GFE_NAME",col_cu)]
  col_NAME <- "NAME"
  if(cu_index_type == "STOCK_CU_INDEX"){
    col_NAME <- paste0("STOCK_",col_NAME)
    col_cuid <- "cuid_broodstock"
    col_comment <- "comment_broodstock"
  }else if(cu_index_type == "REL_CU_INDEX"){
    col_NAME <- paste0("REL_CU_",col_NAME)
    col_cuid <- "cuid_release_site"
    col_comment <- "comment_release_site"
  }
  
  # Create a dataframe with (1) CU_INDEX is NA and (2) unique species, stock/CU,
  # and GFE_ID (= location)
  hatchery_DFO_pop_NA <- unique(hatchery_DFO[cond_na,col_cu])
  unique(hatchery_DFO[!cond_na,col_cu])
  nrow(hatchery_DFO_pop_NA) 
  nrow(unique(hatchery_DFO[,col_cu]))
  
  head(hatchery_DFO_pop_NA)
  sum(is.na(hatchery_DFO$STOCK_NAME))   # 0 <- field to use over STOCK_POP_NAME (lots of NAs) and STOCK_GFE_NAME (a few NAs)
  sum(is.na(hatchery_DFO[,col_GFE_ID])) # 3
  
  # CUs_gdb <- st_buffer(x = CUs_gdb_full, dist = .1)   # takes too much time       
  
  # plot(CUs_gdb_rg_sp[1], col = alpha("green",.2))
  # plot(st_buffer(x = CUs_gdb_rg_sp,dist = .1)[1])
  # plot(st_simplify(x = CUs_gdb[cond_sp & cond_rg,][1,][1],dTolerance = .001),
  #      col = alpha("red",.2))
  
  # only retain the fields that are useful
  nuseds_pop <- unique(nuseds_cleaned[,c("SPECIES","cuid","cu_name_dfo","cu_name_pse","CU_NAME","FULL_CU_IN",
                                         "GFE_ID","SYSTEM_SITE","WATERBODY","GAZETTED_NAME","sys_nm_final",
                                         "X_LONGT","Y_LAT")])
  
  nrow(nuseds_pop) # 6848
  
  # 
  count_percent_threshold <- 1
  hatchery_DFO_pop_NA$cuid <- NA
  hatchery_DFO_pop_NA$comment <- NA
  n_row <- nrow(hatchery_DFO_pop_NA)
  for(r in 1:n_row){
    # r <- 603
    # r <- which(hatchery_DFO_pop_NA$SPECIES_NAME == "Sockeye" & hatchery_DFO_pop_NA$REL_GFE_ID == 10071)
    
    count_percent <- (r/n_row) * 100
    if(count_percent_threshold < count_percent){
      print(paste0("*** Progress: ",count_percent_threshold,"% ***"))
      count_percent_threshold <- count_percent_threshold + percent_increment
    }
    
    # print(r)
    
    # hatchery_DFO_pop_NA[r,]
    SPECIES_NAME <- hatchery_DFO_pop_NA$SPECIES_NAME[r]
    NAME <- hatchery_DFO_pop_NA[r,col_NAME] # field is useless because there is no match at all with NuSEDS
    GFE_ID <- hatchery_DFO_pop_NA[r,col_GFE_ID]
    RUN_NAME <- hatchery_DFO_pop_NA$RUN_NAME[r]
    GFE_NAME <- hatchery_DFO_pop_NA[r,col_GFE_NAME]
    LONGITUDE <- hatchery_DFO_pop_NA[r,col_X_LONGT]
    LATITUDE <- hatchery_DFO_pop_NA[r,col_Y_LAT]
    
    # Case that is no solvable
    keepGoing <- T
    comment <- NA
    if(is.na(GFE_ID) & is.na(LONGITUDE) & is.na(LATITUDE)){
      keepGoing <- F # FAILURE 
      hatchery_DFO_pop_NA$comment[r] <- "no STOCK GFE_ID and coordinates - FAILURE"
    }
    
    #' 1) use GFE_ID
    
    #' 1)1. if there is no GFE_ID but there are coordinates --> try to find the GFE_ID
    if(is.na(GFE_ID) & !is.na(LONGITUDE) & !is.na(LATITUDE)){
      
      # try to find the GFE_ID in NuSEDS
      cond_coord <- round(as.numeric(nuseds$X_LONGT),4) == round(LONGITUDE,4) &
        !is.na(nuseds$X_LONGT) & 
        round(as.numeric(nuseds$Y_LAT),4) == round(LATITUDE,4) & 
        !is.na(nuseds$Y_LAT)
      
      if(any(cond_coord)){
        GFE_ID <- unique(nuseds$GFE_ID[cond_coord])
        if(length(GFE_ID) > 1){
          print("More than one GFE_ID in NuSEDS")
          break
        }
        
      }else{  # try with the GFE_ID stream file provided by DFO (emailed from Wu Zhipeng, DFO, 09/04/2024)
        
        cond_coord <- round(as.numeric(DFO_All_Streams_Segments$X_LONGT),4) == round(LONGITUDE,4) &
          !is.na(DFO_All_Streams_Segments$X_LONGT) & 
          round(as.numeric(DFO_All_Streams_Segments$Y_LAT),4) == round(LATITUDE,4) & 
          !is.na(DFO_All_Streams_Segments$Y_LAT)
        
        if(any(cond_coord)){
          GFE_ID <- unique(DFO_All_Streams_Segments$ID[cond_coord])
          if(length(GFE_ID) > 1){
            print("More than one GFE_ID in DFO_All_Streams_Segments")
            break
          }else{
            comment <- paste(comment,"GFE_ID not found in NuSEDS and DFO streams file", sep = "; ")
          }
        }
      }
    }
    
    #' 1)2. use GFE_ID and species to find the corresponding CU(s)
    if(!is.na(GFE_ID)){ # can be redundant with previous step for cases where GFE_ID had to be found with STOCK coordinates
      
      # check if GFE_ID is in NuSEDS
      cond_gfe_id <- nuseds_pop$GFE_ID == GFE_ID
      
      if(any(cond_gfe_id)){ # else (i.e. "GFE_ID not in NuSEDS"): need to go to step 2)
        
        cond_sp <- nuseds_pop$SPECIES == SPECIES_NAME
        
        if(!any(cond_sp & cond_gfe_id)){ # must go to 2)
          
          comment <- paste(comment,"no SPECIES & GFE_ID combo in NuSEDS", sep = "; ")
          
        }else{
          
          FULL_CU_IN <- nuseds_pop$FULL_CU_IN[cond_sp & cond_gfe_id]
          cuid <- nuseds_pop$cuid[cond_sp & cond_gfe_id]
          cu_name_pse <- nuseds_pop$cu_name_pse[cond_sp & cond_gfe_id]
          
          if(length(FULL_CU_IN) > 1 | length(cuid) > 1){
            
            if(SPECIES_NAME == "Pink"){ # check if odd or even population
              
              even <- hatchery_DFO_pop_NA$species_qualified[r] == "PKE"
              
              if(even){
                cond <- grepl("PKE",FULL_CU_IN)
              }else{
                cond <- grepl("PKO",FULL_CU_IN)
              }
              FULL_CU_IN <- FULL_CU_IN[cond]
              cuid <- cuid[cond]
            }
          }
          
          if(length(FULL_CU_IN) > 1 | length(cuid) > 1){ # if still > 1
            
            # there are three potential PSE CUs matching this one: 
            # - CU 760 (Adams-Early Summer) (previously CU 751 which got split into 760 and 761)
            # - CU 739 (Shuswap-Late (cyclic)
            # - CU 738 (Momich-Early Summer) --> if using CU geo database
            # but actually, NAME = "Momich+Cayenne", which corresponds to CU 761: cu_name_pse = Momich-Early Summer
            cond_1 <- SPECIES_NAME == "Sockeye" & GFE_ID == 223 & RUN_NAME == "Summer"
            
            # here there is the choice between Middle Fraser River (Summer 5-2) vs. Middle Fraser River (Spring 5-2)
            cond_2 <- SPECIES_NAME == "Chinook" & GFE_ID == 290 & RUN_NAME == "Spring"
            
            if(cond_1){  # r = 132
              cuid <- 761
              FULL_CU_IN <- CUs_gdb$FULL_CU_IN[CUs_gdb$CUID == cuid]
              comment <- paste(comment,"manual attribution to CU Middle Fraser River (Spring 5-2)",sep = "; ")
              
            }else if(cond_2){
              cuid <- cuid[grepl("Spring ",cu_name_pse)]
              FULL_CU_IN <- FULL_CU_IN[grepl("Spring ",cu_name_pse)]
              cu_name_pse <- cu_name_pse[grepl("Spring ",cu_name_pse)]
              comment <- paste(comment,"manual attribution to CU 761 Momich-Early Summer",sep = "; ")
              
            }else{
              print("more than one FULL_CU_IN for SPECIES & GFE_ID combo in NuSEDS")
              break
              
              hatchery_DFO_pop_NA[r,]
              cond <- hatchery_DFO$SPECIES_NAME == SPECIES_NAME &
                hatchery_DFO[,col_GFE_ID] == GFE_ID & !is.na(hatchery_DFO[,col_GFE_ID]) &
                hatchery_DFO$RUN_NAME == RUN_NAME & !is.na(hatchery_DFO$RUN_NAME)
              hatchery_DFO[cond,]
              hatchery_DFO[cond,]$RELEASE_YEAR
              nuseds_pop[cond_sp & cond_gfe_id,]
            }
            
          }
          
          if(length(FULL_CU_IN) == 1 | length(cuid) == 1){
            hatchery_DFO_pop_NA[r,col_CU_INDEX] <- FULL_CU_IN
            hatchery_DFO_pop_NA$cuid[r] <- cuid
            hatchery_DFO_pop_NA$comment[r] <- paste(comment,"Matched with GFE_ID and species", sep = "; ")
            keepGoing <- F # SUCCESS!
          }
        }
      }
    } # end of trying to find CU with GFE_ID
    
    #' 2)1. if step above did not work, use coordinates and shape files
    if(keepGoing){
      
      # if coordinates stock are not available: try to find them in NuSEDS
      if(!is.na(GFE_ID) & (is.na(LONGITUDE) | is.na(LATITUDE))){
        
        # try to find the coordinates in Nuseds
        cond_gfe_id <- nuseds_pop$GFE_ID == GFE_ID
        
        if(any(cond_gfe_id)){
          LATITUDE <- nuseds_pop$Y_LAT[cond_gfe_id]
          LONGITUDE <- nuseds_pop$X_LONGT[cond_gfe_id]
          
          if(is.na(LATITUDE) | is.na(LONGITUDE)){
            comment <- paste(comment,"GFE_ID in NuSEDS but coordinates are NAs", sep = "; ")
          }else{
            comment <- paste(comment,"GFE_ID in NuSEDS and coordinates available", sep = "; ")
          }
          
        }else{
          comment <- paste(comment,"GFE_ID not in NuSEDS", sep = "; ")
        }
      }
      
      # if coordinates stock are not available: try to find them in DFO streams file
      if(!is.na(GFE_ID) & (is.na(LONGITUDE) | is.na(LATITUDE))){
        # 
        cond_gfe_id <- DFO_All_Streams_Segments$ID == GFE_ID
        
        if(any(cond_gfe_id)){
          LATITUDE <- as.numeric(DFO_All_Streams_Segments$Y_LAT[cond_gfe_id])
          LONGITUDE <- as.numeric(DFO_All_Streams_Segments$X_LONGT[cond_gfe_id])
          
          if(is.na(LATITUDE) | is.na(LONGITUDE)){
            comment <- paste(comment,"GFE_ID in DFO streams file but coordinates are NAs - FAILURE", sep = "; ") # # FAILURE
            hatchery_DFO_pop_NA$comment[r] <- comment
          }else{
            comment <- paste(comment,"GFE_ID in DFO streams file and coordinates available", sep = "; ")
          }
          
        }else{
          comment <- paste(comment,"GFE_ID not in DFO streams file - FAILURE", sep = "; ") # FAILURE
          hatchery_DFO_pop_NA$comment[r] <- comment
        }
      }
      
      if(!is.na(LONGITUDE) & !is.na(LATITUDE)){
        
        hatchery_DFO_pop_NA[r,col_Y_LAT] <- LATITUDE
        hatchery_DFO_pop_NA[r,col_X_LONGT] <- LONGITUDE
        
        # Find the region
        # https://stackoverflow.com/questions/75669453/r-check-to-see-if-coordinates-fall-inside-outside-of-a-shapefile-polygon
        point <- st_as_sf(hatchery_DFO_pop_NA[r,], 
                          coords = c(col_X_LONGT,col_Y_LAT), crs = 4269)
        
        layer_rg <- st_intersects(point, regions_shp)
        if(length(layer_rg[[1]]) == 0){ # no match, try to buffer
          layer_rg <- st_intersects(point, st_buffer(x = regions_shp, dist = .001))
        }
        if(length(layer_rg[[1]]) == 0){ # no match, try to buffer
          layer_rg <- st_intersects(point, st_buffer(x = regions_shp, dist = .01))
        }
        if(length(layer_rg[[1]]) == 0){ # no match, try to buffer
          layer_rg <- st_intersects(point, st_buffer(x = regions_shp, dist = .05))
        }
        if(length(layer_rg[[1]]) == 0){ # no match, try to buffer
          layer_rg <- st_intersects(point, st_buffer(x = regions_shp, dist = .1))
        }
        if(length(layer_rg[[1]]) == 0){ # no match, try to buffer
          layer_rg <- st_intersects(point, st_buffer(x = regions_shp, dist = .2))
        }
        if(length(layer_rg[[1]]) == 0){ # no match, try to buffer
          layer_rg <- st_intersects(point, st_buffer(x = regions_shp, dist = .4))
        }
        # this is for a release location fall right in the middle between VIMI and
        # the Fraser but the corresponding stock location is in VIMI at
        # HOWE SOUND-BURRARD INLET
        cond_rg_1 <- SPECIES_NAME == "Coho" & RUN_NAME == "Fall" & !is.na(RUN_NAME) &
          GFE_ID == 31815 & !is.na(GFE_ID) &
          length(layer_rg[[1]]) == 2
        
        if(cond_rg_1){
          layer_rg[[1]] <- layer_rg[[1]][regions_shp$regionname[layer_rg[[1]]] == "Vancouver Island & Mainland Inlets"]
        }   
        
        if(length(layer_rg[[1]]) > 1){
          
          print("Issue with finding region even after buffering: more than one choice")
          break
          
          layer_rg <- st_intersects(point, st_buffer(x = regions_shp, dist = .4))
          layer_rg
          
          plot(st_geometry(regions_shp))
          plot(st_geometry(regions_shp[layer_rg[[1]],]))
          plot(st_geometry(point), add = T, col = "red", pch = 16, cex = 2)
          
          hatchery_DFO_pop_NA[r,]
          
          regions_here <- regions_shp$regionname[layer_rg[[1]]]
          regions_here
          
        }
        
        if(length(layer_rg[[1]]) == 0){ # still no match, 
          print("Issue with finding region even after buffering")
          break
          
          layer_rg <- st_intersects(point, st_buffer(x = regions_shp, dist = .2))
          plot(st_geometry(regions_shp))
          plot(st_geometry(point), add = T, col = "red", pch = 16, cex = 2)
          
        }else{
          layer_rg <- layer_rg[[1]]
        }
        rg <- regions_shp$regionname[layer_rg]
        
        # check
        # plot(st_geometry(regions_shp))
        # plot(st_geometry(regions_shp[layer_rg,]))
        # plot(st_geometry(point), add = T, col = "red", pch = 16, cex = 3)
        
        # filter region and species
        sp <- SPECIES_NAME
        if(SPECIES_NAME == "Sockeye"){
          sp <- c("River sockeye","Lake sockeye")
        }else if(SPECIES_NAME == "Pink"){
          sp <- c("Pink odd","Pink even")
        }
        
        cond_rg_sp <- CUs_gdb$region %in% rg & CUs_gdb$species %in% sp
        
        if(!any(cond_rg_sp)){
          
          if(SPECIES_NAME == "Steelhead"){
            comment <- paste(comment,"No region - species combo in CUs_gdb with Steelhead - FAILURE", sep = "; ")
            hatchery_DFO_pop_NA$comment[r] <- comment
            
          }else if(rg == "Yukon"){
            comment <- paste(comment,"Yukon region not in CUs gdb - FAILURE", sep = "; ")
            hatchery_DFO_pop_NA$comment[r] <- comment
            
          }else{
            print("Issue with region - species combo in CUs_gdb")
            break
          }
          
        }else{
          
          CUs_gdb_rg_sp <- CUs_gdb[cond_rg_sp,]
          
          layer_CU <- st_intersects(point, CUs_gdb_rg_sp)
          if(length(layer_CU[[1]]) == 0){ # no match, try to buffer
            layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = .05))
          }
          if(length(layer_CU[[1]]) == 0){ # no match, try to buffer
            layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = .1))
          }
          if(length(layer_CU[[1]]) == 0){ # no match, try to buffer
            layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = .2))
          }
          if(length(layer_CU[[1]]) == 0){ # no match, try to buffer
            layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = .3)) # r = 163
          }
          if(length(layer_CU[[1]]) == 0){ # no match, try to buffer
            layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = .4)) # r = 931
          }
          # if(length(layer_CU[[1]]) == 0){ # no match, try to buffer
          #   layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = .95)) # r = 195
          #   it was decided to not do anything for this SH CU: see cond_1 just below
          # }
          
          if(length(layer_CU[[1]]) == 0){ # still no match, 
            
            # in the case below (r = 195), dist must = .95 to intersect. It is probably a rainbow trout, so to exclude
            cond_1 <- SPECIES_NAME == "Steelhead" & NAME == "Eagle R" & !is.na(NAME)
            
            # in the case below (r = 284), dist must = .6 to intersect. It is probably a kokanee, so to exclude
            cond_2 <- SPECIES_NAME == "Sockeye" & NAME == "Mission Cr/OKAN" & !is.na(NAME)
            
            # in the case below (r = 336), dist must = 1.2 to intersect. It is probably a rainbow trout, so to exclude
            cond_3 <- SPECIES_NAME == "Steelhead" & NAME == "Shuswap R Middle" & !is.na(NAME)
            
            # in the case below (r = 366), dist must = 1.5 to intersect. It is not a kokanee because RUN_NAME = "fall" but location too far from the Salmon River near Shuswap Lake
            cond_4 <- SPECIES_NAME == "Sockeye" & NAME == "Salmon R/UPFR" & !is.na(NAME)
            
            # It is the Sockeye summer run released in Mission Creek in Kelowna or the "COLUMBIA RIVER just below the border
            # --> increase radius
            cond_5 <- SPECIES_NAME == "Sockeye" & is.na(NAME) & GFE_NAME %in% c("MISSION CREEK","COLUMBIA RIVER") & !is.na(GFE_NAME)
            
            # "Release coordinates too far from CU area
            cond_6 <- SPECIES_NAME == "Sockeye" & is.na(NAME) & GFE_ID == 146 & !is.na(GFE_ID)
            
            if(cond_1 | cond_3){
              comment <- paste(comment,"STOCK coordinates too far from CU area; probably rainbow trout - FAILURE",sep = "; ")
              hatchery_DFO_pop_NA$comment[r] <- comment
              
            }else if(cond_2){
              comment <- paste(comment,"STOCK coordinates too far from CU area; probably Kokanee - FAILURE",sep = "; ")
              hatchery_DFO_pop_NA$comment[r] <- comment
              
            }else if(cond_4){
              comment <- paste(comment,"STOCK coordinates too far from CU area; not a Kokanee but STOCK coordinates too far from potential CU - FAILURE",sep = "; ")
              hatchery_DFO_pop_NA$comment[r] <- comment
              
            }else if(cond_5){
              # find the
              layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = 2)) #
              
            }else if(cond_6){
              comment <- paste(comment,"Releate coordinates too far from CU area - FAILURE",sep = "; ")
              hatchery_DFO_pop_NA$comment[r] <- comment
              
            }else{
              
              print("Issue finding CU(s) in CUs_gdb even after buffering")
              break
              
              dist <- 1.5
              layer_CU <- st_intersects(point, st_buffer(x = CUs_gdb_rg_sp, dist = dist),) # 
              layer_CU
              layer_CU <- layer_CU[[1]]
              
              plot(st_geometry(regions_shp[layer_rg,]))
              plot(st_geometry(CUs_gdb_rg_sp[layer_CU[1],]), add = T, col = alpha("red",.3))
              plot(st_geometry(CUs_gdb_rg_sp[layer_CU[2],]), add = T, col = alpha("blue",.3))
              plot(st_geometry(point), add = T, col = "red", pch = 16, cex = 2)
              plot(st_geometry(st_buffer(CUs_gdb_rg_sp[layer_CU[1],],dist = dist)), 
                   add = T, col = alpha("green",.3))
              plot(st_geometry(st_as_sf(hatchery_DFO_pop_NA[r,], 
                                        coords = c("REL_LONGITUDE","REL_LATITUDE"), crs = 4269)),
                   add = T, col = "black", pch = 17, cex = 2)
              
              for(i in 1:nrow(CUs_gdb_rg_sp)){
                plot(st_geometry(CUs_gdb_rg_sp[i,]), add = T, col = alpha("red",.3))
              }
              
              hatchery_DFO_pop_NA[r,]
              
              CUs_gdb_rg_sp[layer_CU,]
              
              cond <- hatchery_DFO$SPECIES_NAME == SPECIES_NAME & 
                hatchery_DFO[,col_GFE_ID] == GFE_ID & !is.na(hatchery_DFO[,col_GFE_ID]) &
                hatchery_DFO[,col_NAME] == NAME
              hatchery_DFO[cond,]
              
              cond
              
            }
          }
          
          if(length(layer_CU[[1]]) > 0){
            print(layer_CU)
            
            layer_CU <- layer_CU[[1]]
            # if there is more than one CUs
            if(length(layer_CU) > 1){
              
              # 
              CU_NAMEs <- sapply(layer_CU,function(l){
                return(CUs_gdb_rg_sp$CU_NAME[l])
              })
              
              FULL_CU_INs <- sapply(layer_CU,function(l){
                return(CUs_gdb_rg_sp$FULL_CU_IN[l])
              })
              
              CUIDs <- sapply(layer_CU,function(l){
                return(CUs_gdb_rg_sp$CUID[l])
              })
              
              cu_name_pses <- sapply(layer_CU,function(l){
                return(CUs_gdb_rg_sp$cu_name_pse[l])
              })
              
              # about the assumptions:
              # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1738112709281789?thread_ts=1734047406.340689&cid=CJ5RVHVCG
              
              # Assumption 1 (r = 23): Vernon SEL
              # POP_NAME = "SEBALHALL CREEK SOCKEYE"
              cond_1 <- SPECIES_NAME == "Sockeye" & 
                CU_NAMEs[1] == "East Vancouver Island & Georgia Strait" & # SER
                CU_NAMEs[2] == "Vernon"                                   # SEL
              
              # Assumption 2 (r = 36): Marie
              # POP_NAME = " MARIE LAKE SOCKEYE"
              cond_2 <- SPECIES_NAME == "Sockeye" & 
                CU_NAMEs[1] == "North Haida Gwaii" &  # SER
                CU_NAMEs[2] == "Marie"                # SEL
              
              #' in case the answer is in the RUN_NAME, e.g., 
              #' CU_NAMEs = "East Vancouver Island Winter" "East Vancouver Island Summer" & RUN_NAME = "Summer"
              cond_3 <- grepl(simplify_string_fun(hatchery_DFO_pop_NA$RUN_NAME[r]),
                              simplify_string_fun(CU_NAMEs))
              
              #' Case with SH Kispiox R, with potential choices:
              #' - CU_NAME = Kispiox vs. Middle Skeena --> the former
              cond_4 <- SPECIES_NAME == "Steelhead" & NAME == "Kispiox R" & !is.na(NAME)
              
              # if(all(is.na(FULL_CU_INs))){  # because there are cuid, which ultimately what we want
              #   layer_CU <- layer_CU[1]
              #   comment <- paste0(comment,"more than one CUs layer but FULL_CU_INs is NA in both case")
              # }
              
              cond_5 <- SPECIES_NAME == "Sockeye" & NAME == "Coquitlam R" & !is.na(NAME)
              # three option:  Widgeon (river-type), Pitt-Early Summer, Coquitlam-Early Summer --> the latter
              
              # Orford Bay is in CU Southern Mainland-Southern Fjords_FA_0.x
              cond_6 <- SPECIES_NAME == "Chinook" & identical(CU_NAMEs,c("Southern Mainland-Georgia Strait_FA_0.x","Southern Mainland-Southern Fjords_FA_0.x"))
              if(cond_6){
                cond <- hatchery_DFO$SPECIES_NAME == SPECIES_NAME & 
                  hatchery_DFO[,col_GFE_ID] == GFE_ID & !is.na(hatchery_DFO[,col_GFE_ID]) 
                cond_6 <- cond_6 & unique(hatchery_DFO[cond,]$RELEASE_SITE_NAME) == "Orford Bay"
              }
              
              # Orford Bay is in CU Bute Inlet
              cond_7 <- SPECIES_NAME == "Chum" & identical(CU_NAMEs,c("Bute Inlet","Georgia Strait"))
              if(cond_7){
                cond <- hatchery_DFO$SPECIES_NAME == SPECIES_NAME & 
                  hatchery_DFO[,col_GFE_ID] == GFE_ID & !is.na(hatchery_DFO[,col_GFE_ID]) 
                cond_7 <- cond_7 & unique(hatchery_DFO[cond,]$RELEASE_SITE_NAME) == "Orford Bay"
              }
              
              # Orford Bay is in CU Southern Coastal Streams-Queen Charlotte Strait-Johnstone Strait-Southern Fjords
              cond_8 <- SPECIES_NAME == "Coho" & identical(CU_NAMEs,c("Georgia Strait Mainland","Southern Coastal Streams-Queen Charlotte Strait-Johnstone Strait-Southern Fjords"))
              if(cond_8){
                cond <- hatchery_DFO$SPECIES_NAME == SPECIES_NAME & 
                  hatchery_DFO[,col_GFE_ID] == GFE_ID & !is.na(hatchery_DFO[,col_GFE_ID]) 
                cond_8 <- cond_8 & unique(hatchery_DFO[cond,]$RELEASE_SITE_NAME) == "Orford Bay"
              }
              
              # select PKE vs. PKO
              cond_9 <- hatchery_DFO_pop_NA$species_qualified[r] %in% c("PKO","PKE")
              
              # In this case, RELEASE_SITE_NAME = "Mill Bay", which is connected to 
              # East Vancouver Island-Cowichan & Koksilah_FA_0.x
              cond_10 <- SPECIES_NAME == "Chinook" & identical(CU_NAMEs,c("East Vancouver Island-Cowichan & Koksilah_FA_0.x","East Vancouver Island-Goldstream_FA_0.x"))
              
              # In this case, REL_GFE_NAME = "COQUITLAM RIVER", which is connected to 
              # the Fraser River, which is connected to Lower Fraser River-Upper Pitt_SU_1.3 and NOT connected to Boundary Bay_FA_0.3
              cond_11 <- SPECIES_NAME == "Chinook" & identical(CU_NAMEs,c("Boundary Bay_FA_0.3","Lower Fraser River-Upper Pitt_SU_1.3"))
              
              # the point here is in the water near Rivers Inlet CU
              cond_12 <- SPECIES_NAME == "Chinook" & identical(CU_NAMEs,c("Wannock","Rivers Inlet"))
              
              # here RUN_NAME = "Hybrid" so there is no way of knowing if it is the summer vs winter CU
              # unless maybe using dates but not done so far.
              # So we select the Summer one
              cond_13 <- SPECIES_NAME == "Steelhead" & identical(CU_NAMEs,c("South Coast Winter","South Coast Summer"))
              
              # The two CUs overlap, but the RUN_NAME = "Spring" --> Early Stuart Timing
              cond_14 <- SPECIES_NAME == "Sockeye" & identical(CU_NAMEs,c("Takla/Trembleur/Stuart-Summer Timing","Takla/Trembleur-Early Stuart Timing"))
              if(cond_14){
                cond <- hatchery_DFO$SPECIES_NAME == SPECIES_NAME & 
                  hatchery_DFO[,col_X_LONGT] == LONGITUDE & !is.na(hatchery_DFO[,col_X_LONGT]) &
                  hatchery_DFO[,col_Y_LAT] == LATITUDE & !is.na(hatchery_DFO[,col_Y_LAT])
                cond_14 <- cond_14 & unique(hatchery_DFO$RUN_NAME[cond] == "Spring")
              }
              
              if(cond_1 | cond_2){
                layer_CU <- layer_CU[2]
                
              }else if(sum(cond_3) == 1){ # there should one and only one option
                layer_CU <- layer_CU[cond_3]
                
              }else if(cond_4){
                layer_CU <- layer_CU[grepl("Kispiox",CUs_gdb_rg_sp$CU_NAME[layer_CU])]
                
              }else if(cond_5){
                cond <- grepl("Coquitlam",cu_name_pses)
                layer_CU <- layer_CU[cond]
                
              }else if(cond_6){
                layer_CU <- layer_CU[grepl("Mainland-Southern",CU_NAMEs)]
                
              }else if(cond_7){
                layer_CU <- layer_CU[grepl("Bute Inlet",CU_NAMEs)]
                
              }else if(cond_8){
                layer_CU <- layer_CU[grepl("Streams-Queen Charlotte Strait-Johnstone",CU_NAMEs)]
                
              }else if(cond_9){
                layer_CU <- layer_CU[grepl(hatchery_DFO_pop_NA$species_qualified[r],FULL_CU_INs)]
                
              }else if(cond_10){
                layer_CU <- layer_CU[grepl("Cowichan & Koksilah",CU_NAMEs)]
                
              }else if(cond_11){
                layer_CU <- layer_CU[grepl("Lower Fraser River-Upper",CU_NAMEs)]
                
              }else if(cond_12){
                layer_CU <- layer_CU[grepl("Rivers Inlet",CU_NAMEs)]
                
              }else if(cond_13){
                layer_CU <- layer_CU[grepl("Summer",CU_NAMEs)]
                
              }else if(cond_14){
                layer_CU <- layer_CU[grepl("Early Stuart Timing",CU_NAMEs)]
                
              }else{
                
                # the location is south of the Fraser river and the CUs are above
                cond_exception_1 <- SPECIES_NAME == "Chinook" & all(CU_NAMEs %in% c("Lower Fraser River_FA_0.3","Lower Fraser River_SU_1.3","Maria Slough_SU_0.3"))
                
                # the location is away from the three CUs
                cond_exception_2 <- SPECIES_NAME == "Sockeye" & all(CU_NAMEs %in% c("Widgeon","Pitt-Early Summer Timing"," "))
                
                # the location is away from the three CUs
                cond_exception_3 <- SPECIES_NAME == "Chinook" & all(CU_NAMEs %in% c("Lower Thompson_SP_1.2","South Thompson_SU_0.3","South Thompson_SU_1.3"))
                
                # the location in the two CUs
                cond_exception_4 <- SPECIES_NAME == "Chum" & all(CU_NAMEs %in% c("Bella Coola-Dean Rivers","Bella Coola River-Late"))
                
                # the location in the two CUs
                cond_exception_5 <- SPECIES_NAME == "Chinook" & all(CU_NAMEs %in% c("West Vancouver Island-South_FA_0.x","East Vancouver Island-Goldstream_FA_0.x"))
                
                # the location in the two CUs
                cond_exception_6 <- SPECIES_NAME == "Coho" & all(CU_NAMEs %in% c("Haida Gwaii-East","Haida Gwaii-Graham Island Lowlands"))
                
                # the location in the two CUs
                cond_exception_7 <- SPECIES_NAME == "Chum" & all(CU_NAMEs %in% c("Skidegate","East HG"))
                
                cond_exception <- cond_exception_1 | cond_exception_2 | cond_exception_3 |
                  cond_exception_4 | cond_exception_5 | cond_exception_6 | cond_exception_7
                
                if(!cond_exception){
                  print("more than one CUs")
                  break
                  
                  plot(st_geometry(regions_shp[layer_rg,]))
                  plot(st_geometry(CUs_gdb_rg_sp[layer_CU[1],]), add = T, col = alpha("red",.3))
                  plot(st_geometry(CUs_gdb_rg_sp[layer_CU[2],]), add = T, col = alpha("blue",.3))
                  plot(st_geometry(CUs_gdb_rg_sp[layer_CU[3],]), add = T, col = alpha("green",.3))
                  plot(st_geometry(point), add = T, col = "red", pch = 16, cex = 1)
                  plot(st_geometry(st_as_sf(hatchery_DFO_pop_NA[r,], 
                                            coords = c("REL_LONGITUDE","REL_LATITUDE"), crs = 4269)),
                       add = T, col = "black", pch = 17, cex = 2)
                  
                  hatchery_DFO_pop_NA[r,]
                  
                  CUs_gdb_rg_sp[layer_CU,]
                  CUs_gdb_rg_sp[layer_CU,]$cu_name_pse
                  
                  cond <- hatchery_DFO$SPECIES_NAME == SPECIES_NAME & 
                    hatchery_DFO[,col_GFE_ID] == GFE_ID & !is.na(hatchery_DFO[,col_GFE_ID]) &
                    hatchery_DFO[,col_NAME] == NAME & !is.na(hatchery_DFO[,col_NAME])
                  
                  cond <- hatchery_DFO$SPECIES_NAME == SPECIES_NAME & 
                    hatchery_DFO[,col_GFE_ID] == GFE_ID & !is.na(hatchery_DFO[,col_GFE_ID]) 
                  
                  cond <- hatchery_DFO$SPECIES_NAME == SPECIES_NAME & 
                    hatchery_DFO[,col_X_LONGT] == LONGITUDE & !is.na(hatchery_DFO[,col_X_LONGT]) &
                    hatchery_DFO[,col_Y_LAT] == LATITUDE & !is.na(hatchery_DFO[,col_Y_LAT])
                  
                  hatchery_DFO[cond,]
                  hatchery_DFO[cond,]$RELEASE_SITE_NAME |> unique()
                  
                  cond <- nuseds$SPECIES == SPECIES_NAME & nuseds$CU_NAME %in% CU_NAMEs
                  nuseds[cond,]
                  
                }else{
                  comment <- paste(comment,"intersect multiple CUs - FAILURE", sep = "; ")
                }
                
              }
            } # if length(layer_CU) > 1 --> more than one CU
            
            if(length(layer_CU) == 1){
              
              CU_NAME <- CUs_gdb_rg_sp$CU_NAME[layer_CU]
              FULL_CU_IN <- CUs_gdb_rg_sp$FULL_CU_IN[layer_CU]
              cuid <- CUs_gdb_rg_sp$CUID[layer_CU]
              
              if(is.na(cuid)){ # try the decoder
                # comment <- paste(comment,"Matched with geocoordinates and species but FULL_CU_IN is NA - FAILURE", sep = "; ")
                print("cuid is NAs from gdb file")
                break
              }
              
              comment <- paste(comment,"Matched with geocoordinates and species", sep = "; ")
              
              hatchery_DFO_pop_NA[r,col_CU_INDEX] <- FULL_CU_IN
              hatchery_DFO_pop_NA$cuid[r] <- cuid
              hatchery_DFO_pop_NA$comment[r] <- comment
              keepGoing <- F # SUCCESS!
              
              # check
              # plot(st_geometry(CUs_gdb_rg_sp[layer_CU,]))
              # plot(st_geometry(point), add = T, col = "red", pch = 16, cex = 3)
              
            }else{
              
              hatchery_DFO_pop_NA$comment[r] <- comment
              
            }
          } # if CU(s) found
        } # if region - species combo in CUs_gdb
      } # if coordinates were found
    }  # end of 2)
  }
  
  hatchery_DFO_pop_NA$comment <- gsub("NA; ","",hatchery_DFO_pop_NA$comment)
  
  table(hatchery_DFO_pop_NA$comment)
  
  # x <- sapply(hatchery_DFO_pop_NA$cuid,function(cui){
  #   if(!is.na(cui)){
  #     cond <- conservationunits_decoder$cuid == cui & !is.na(conservationunits_decoder$cuid)
  #     if(any(cond)){
  #       out <- conservationunits_decoder$cu_index[cond]
  #     }else{
  #       out <- NA
  #     }
  #   }else{
  #     out <- NA
  #   }
  #   return(out)
  # })
  # identical(hatchery_DFO_pop_NA$STOCK_CU_INDEX,x)
  # cbind(hatchery_DFO_pop_NA$STOCK_CU_INDEX,x)
  
  colnames(hatchery_DFO_pop_NA)[colnames(hatchery_DFO_pop_NA) == "cuid"] <- col_cuid
  colnames(hatchery_DFO_pop_NA)[colnames(hatchery_DFO_pop_NA) == "comment"] <- col_comment
  
  if(r == n_row){
    print(paste0("*** Progress: 100% ***"))
  }
  
  return(hatchery_DFO_pop_NA)
}


