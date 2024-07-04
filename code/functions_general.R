
#'******************************************************************************
#' The goal of the script is to provide general functions that can be used in 
#' several projects.
#'******************************************************************************


#' Function that takes a dataframe, the column names to keep a desired number of
#' rows and returns a new dataframe filled with NAs, with the same column names,
#' the desired number of rows and NAs values.
# df <- fileSurvey_l[[sheet_i]]
dataframe_structure_fun <- function(df,colnamesToKeep = NA,nrow = NA){
  
  if(is.na(nrow)){
    nrow <- nrow(df)
  }
  
  if(is.na(colnamesToKeep)[1]){
    colnamesToKeep <- colnames(df)
  }
  
  out <- as.data.frame(matrix(NA,nrow =nrow, ncol = length(colnamesToKeep)))
  colnames(out) <- colnamesToKeep
  return(out)
}

#' Function that looks if values in the 1st column/field of the provided 
#' dataframe is associated with multiple values in the second column/field.
#' This function can be used for QA/QC.
# df <- DFO_df
# col1 <- "PROJ_NAME"
# col2 <- "PROGRAM_CODE"
relationships_twoCol_df_fn <- function(df,col1,col2,printDF = F,n_eg = 3){
  
  df <- as.data.frame(df)
  df <- df[,c(col1,col2)]
  df <- na.omit(df)
  df <- unique(df)
  
  # col2 inside col1
  col1ToCol2 <- NA    # the type of relationship in that direction
  messagePrinted <- F
  n_eg_count <- 1
  for(c in unique(df[,col1])){
    # c <- unique(df[,col1])[1]
    df_cut <- df[df[,col1] == c,]
    if(nrow(df_cut) > 1){
      if(!messagePrinted){
        print(paste0("The are potentially multiple values of '",col2,"' for a single value in '",col1,"'."))
        messagePrinted <- T
        col1ToCol2 <- "many"
      }
      if(printDF & n_eg_count <= n_eg){
        print(df_cut)
        n_eg_count <- n_eg_count + 1
      }
    }
  }
  if(!messagePrinted){
    print(paste0("The is a unique value of '",col2,"' for each value in '",col1,"'."))
    col1ToCol2 <- "one"
  }
  
  # col1 inside col2
  messagePrinted <- F
  col2ToCol1 <- NA    # the type of relationship in that direction
  n_eg_count <- 1
  for(c in unique(df[,col2])){
    # c <- unique(df[,col2])[1]
    df_cut <- df[df[,col2] == c,]
    if(nrow(df_cut) > 1){
      if(!messagePrinted){
        print(paste0("The are potentially multiple values of '",col1,"' for a single value in '",col2,"'."))
        messagePrinted <- T
        col2ToCol1 <- "many"
      }
      if(printDF & n_eg_count <= n_eg){
        print(df_cut)
        n_eg_count <- n_eg_count + 1
      }
    }
  }
  if(!messagePrinted){
    print(paste0("The is a unique value of '",col1,"' for each value in '",col2,"'."))
    col2ToCol1 <- "one"
  }
  
  # conclusion:
  if(col1ToCol2 == "many" & col2ToCol1 == "many"){
    col1Col2Relationship <- "MANY TO MANY"
  }else if(col1ToCol2 == "many" & col2ToCol1 == "one"){
    col1Col2Relationship <- "ONE TO MANY"
  }else if(col1ToCol2 == "one" & col2ToCol1 == "many"){
    col1Col2Relationship <- "MANY TO ONE"
  }else if(col1ToCol2 == "one" & col2ToCol1 == "one"){
    col1Col2Relationship <- "ONE TO ONE"
  }
  print(paste0("CONCLUSION: The relatinship between '",col1,"' and '",col2,"' is: ",col1Col2Relationship))
}

# Function to compute the geometric mean accounting for NAs values
# x <- c(450, NA, 350, 350)
mean_geom_fun <- function(x){
  # exp(mean(log(x),na.rm = T))  # other equivalent formula 
  if(sum(!is.na(x))> 0){
    gm <- prod(x, na.rm = T)^(1/sum(!is.na(x)))
  }else{
    gm <- NA
  }
  return(gm)
}

# Function that returns a data frame of the fish species names (as column names)
# and corresponding acronym.
# BSC: figure out what these are exactly --> to double check
species_acronym_fun <- function(){
  
  # species_acronym <- data.frame(
  #   name = c("Chinook","Chum","Coho","Pink","Sockeye","Steelhead","Cutthroat"),
  #   acronym = c("CK","CM","CO","PK","SX","SH","CT"))
  
  # species_acronym <- data.frame(
  #   Chinook = "CK",
  #   Chum = "CM",         
  #   Coho = "CO",         
  #   Pink = "PK",
  #   Sockeye = 'SX',
  #   Steelhead = "SH",   
  #   Cutthroat = "CT")   
  
  species_acronym <- data.frame(
    species_name = c("Chinook","Chum","Coho","Sockeye","Lake sockeye","River sockeye","Pink","Pink (even)","Pink (odd)","Steelhead","Cutthroat"),
    species_acro = c("CK","CM","CO","SX","SX","SX","PK","PK","PK","SH","CT"),
    species_acro_details = c("CK","CM","CO","SX","LSX","RSX","PK","PKE","PKO","SH","CT"),
    species_acro2 = c("CK","CM","CO","SE","SE","SE","PK","PK","PK","SH","CT"),
    species_acro2_details = c("CN","CM","CO","SE","SEL","SER","PK","PKE","PKO","SH","CT"))
    
  return(species_acronym)
}

#' Function that receives a vector of population size time series with year as 
#' the names of each value and returns if the series had years only in even or 
#' odd years or both.
# x <- spawnerAbundance
# even_odd_time_series_fun(x)
even_odd_time_series_fun <- function(x){
  
  years <- as.numeric(names(x))
  names(years) <- sapply(X = years, FUN = function(y){
    if(y %% 2 == 1){
      out <- "odd"
    }else if(y %% 2 == 0){
      out <- "even"
    }
    return(out)
  })
  
  x_odd <- x[names(years) == "odd"]
  x_even <- x[names(years) == "even"]
  
  if(sum(!is.na(x_odd)) > 0){
    valuePresentInOddyears <- T
  }else{
    valuePresentInOddyears <- F
  }
  if(sum(!is.na(x_even)) > 0){
    valuePresentInEvenyears <- T
  }else{
    valuePresentInEvenyears <- F
  }
  
  if(valuePresentInOddyears & valuePresentInEvenyears){
    output <- "both"
  }else if(!valuePresentInOddyears & valuePresentInEvenyears){
    output <- "even"
  }else if(valuePresentInOddyears & !valuePresentInEvenyears){
    output <- "odd"
  }
  
  return(output)
}

#' Generation length estimates from the Tech-Report for when the value is not 
#' available for given CU:
#' "Where CU-specific data on age-at-return are unavailable, we assume 
#' generation lengths of 
#' - 5 years for Chinook CUs, 
#' - 4 years for coho CUs, 
#' - 4 years for chum CUs, 
#' - 4 years for sockeye CUs"
#' - Pink salmon have a consistent 2-year age-at-return 
generationLengthEstiamte_df <- data.frame(species   = c("CM","CK","CO","SX","PK"),
                                          genLength = c(4,5,4,4,2))

#' Function that retrieves datasets directly from the PSF database. Your EXTERNAL
#' IP address must have been entered in the list of approved IP addressed (as Katy).
#' For more detailed instructions see accessing-swp-database.pdf in:
#' X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/Resources/Accessing database
#' A password is asked.
#'dsn_database = 
#'  - "salmondb_prod": where 2.0 data updates and new dataset schemas are.
#'  - "salmondb_legacy": the live/legacy PSE datasets
#' https://salmonwatersheds.slack.com/archives/CKNVB4MCG/p1715195842788699
#' https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1715195833836169
# name_dataset <- "Appdata.vwdl_conservationunits_decoder" # conservationunits_decoder.csv (has generation length for current spawner abundance calc)
# name_dataset <- "Appdata.vwdl_dataset1cu_output"         # cuspawnerabundance.csv (has CU-level spawner abundance for calculating current spawner abundance for biostatus assessment)
# name_dataset <- "Appdata.vwdl_dataset5_output"           # recruitsperspawner.csv (has R-S data for fitting HBSR models for benchmarks)
retrieve_data_from_PSF_databse_fun <- function(dsn_database = "salmondb_prod",
                                               dsn_hostname = "data.salmonwatersheds.ca",
                                               dsn_port = "5432",
                                               dsn_uid = "salmonwatersheds",
                                               name_dataset){
  
  require(RPostgreSQL)
  
  dsn_pwd <- readline(prompt="Enter database password: " )
  
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    print("Connecting to Database...")
    connec <- dbConnect(drv,
                        dbname = dsn_database,
                        host = dsn_hostname,
                        port = dsn_port,
                        user = dsn_uid,
                        password = dsn_pwd)
    print("Database Connected!")
  },error=function(cond) {
    print("Unable to connect to Database.")
  })
  
  dataset <- dbGetQuery(
    conn = connec,
    statement = paste("SELECT * FROM",name_dataset)
  )
  
  return(dataset)
}

#' Function to return one of the datasets (as a data frame) from the PSF database.
#' The function replace the values "-989898" by NA.
#'- nameDataSet: the name of the dataset, either in the database or the corresponding CSV file
#'- fromDatabase: if TRUE, the dataset is pulled directly from the database,not the CSV file
#'- update_file_csv: if TRUE (and fromDatabase is TRUE), the CSV file is updated
#'- wd: where the CSV files are located.
# nameDataSet <- datasetsNames_database_fun()$name_DB[1]
# https://www.dropbox.com/scl/fi/fdkzawgh1s2805gzb009l/accessing-swp-database.pdf?rlkey=2shj9aginlg06jvw29csmxg7t&dl=0
datasets_database_fun <- function(nameDataSet, fromDatabase = F, update_file_csv = F,
                                  wd){

  datasetsNames_database <- datasetsNames_database_fun()
  
  if(nameDataSet %in% datasetsNames_database$name_DB){
    nameFileDB <- nameDataSet
    nameFileCVS <- datasetsNames_database$name_CSV[datasetsNames_database$name_DB == nameDataSet]
  }else if(nameDataSet %in% datasetsNames_database$name_CSV){
    nameFileCVS <- nameDataSet
    nameFileDB <- datasetsNames_database$name_DB[datasetsNames_database$name_CSV == nameDataSet]
  }else{
    print("The name of the dataset entered is not in datasetsNames_database_fun().")
    output <- NA
  }
  
  if(fromDatabase){
    output <- retrieve_data_from_PSF_databse_fun(name_dataset = nameFileDB)
    # replace -989898 values by NA
    for(col in colnames(output)){
      # col <- colnames(conservationunits_decoder)[11]
      output[,col][output[,col] == -989898] <- NA
    }
    
    if(update_file_csv){
      write.csv(output, paste(wd,nameFileCVS,sep = "/"), row.names = F)
    }
  }else{
    output <- read.csv(paste(wd,nameFileCVS,sep = "/"), header = T)
  }
  return(output)
}

#' Function that returns a data frame of the names of the datasets that can be 
#' pulled from the database and the name of the corresponding CSV files already 
#' downloaded. 
#' https://www.dropbox.com/scl/fi/fdkzawgh1s2805gzb009l/accessing-swp-database.pdf?rlkey=2shj9aginlg06jvw29csmxg7t&dl=0
datasetsNames_database_fun <- function(){
  
  out_df <- data.frame(
    name_DB = c(
      "appdata.vwdl_conservationunits_decoder",
      "appdata.vwdl_dataset1cu_output",
      "appdata.vwdl_dataset5_output",
      "appdata.vwdl_streamspawnersurveys_output",
      "appdata.vwdl_setr_appendix4",
      "appdata.vwdl_catchrunsize_output",
      "appdata.vwdl_conservationunits",
      "appdata.vwstreamlocationids",
      "appdata.vwdl_surveystreams",
      "appdata.vwdl_dataset391_output",
      "appdata.vwdl_dataset380_output",
      "appdata.vwdl_dataset384_output",
      "appdata.vwdl_dataset101_output",
      "appdata.vwdl_catchrunsize_output",
      "appdata.vwdl_dataset1cu_output",
      "appdata.vwdl_dataset103_output",
      "appdata.vwdl_dataset202_output",
      "appdata.vwdl_dataset390_output",
      "appdata.vwdl_dataset102_output"
      ),
    
    name_CSV = c(
      "conservationunits_decoder.csv",
      "cuspawnerabundance.csv",
      "recruitsperspawner.csv",
      "streamspawnersurveys_output.csv",     # dataset_1part2
      "setr_appendix4.csv",
      "catchrunsize_output.csv",
      "conservationunits.csv",
      "streamlocationids.csv",
      "surveystreams.csv",
      "dataset391_output.csv",
      "dataset380_output.csv",
      "dataset384_output.csv",
      "dataset101_output.csv",
      "catchrunsize_output.csv",
      "dataset1cu_output.csv",              # dataset_1part1
      "dataset103_output.csv",
      "dataset202_output.csv",
      "dataset390_output.csv",
      "dataset102_output.csv"
      ))
  
  out_df$index <- 1:nrow(out_df)
  
  return(out_df)
}

#' Function to delete all the files with a given pattern.
# wd <- wd_output
# patterns <- c("benchmarks_summary","CUs_names")
deleteFiles_fun <- function(wd,patterns){
  
  Q <- readline(paste0("Are you sure you want to delete the datafiles having this pattern: '",patterns,"'?\nPress 'Y' for YES or 'N' for NO: "))
  
  if(Q == "Y"){
    
    files <- list.files(path = wd)
    toKeep <- sapply(X = patterns, 
                     FUN = function(p){
                       out <- files[grepl(p,files)]
                       return(out)
                     }
    )
    # convert the matrix to a vector
    toKeep <- c(toKeep)
    
    # delete the files:
    print(paste0("The following files were deleted from ",wd))
    print(toKeep)
    
    file.remove(paste(wd,toKeep,sep = "/"))
  }
}

#' Function to remove rows in 'dataframe' base on the combination of values in 
#' the fields of the other dataframe 'toRemove' (fields must macth between the two
#' dataframes). The function returns the the dataframe without the corresponding
#' rows.
# dataframe <- all_areas_nuseds
remove_rows_fields_fun <- function(dataframe,toRemove,fields = NA){
  
  nrow_all <- nrow(dataframe)
  if(all(is.na(fields))){
    fields_here <- colnames(toRemove)
  }else{
    fields_here <- fields
  }
  
  condition <- sapply(X = 1:nrow(toRemove), FUN = function(r){
    
    cond <- sapply(X = fields_here, function(f){
      # f <- fields_here[1]
      cond <- dataframe[,f] == toRemove[r,f]
      return(cond)
    })
    cond <- apply(X = cond, MARGIN = 1, FUN = all)
    return(cond)
  })
  condition <- apply(X = condition, MARGIN = 1, FUN = any)
  
  out <- dataframe[!condition,]
  
  nrow_cut <- nrow(out)
  print(paste0("Number of rows removed from dataframe = ",nrow_all - nrow_cut))
  
  return(out)
}

#' 
remove_rows_fields_parallel_fun <- function(dataframe,toRemove,cores_nb = 1){
  
  require(parallel)
  # Documentation
  # https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
  
  if(Sys.info()["sysname"] == "Windows"){
    
  }else{
    
    #' NOT TESTED
    trackRecord_l <- mclapply(cl = cl, X = IndexIds, fun = function(iid){

      return(trackRecord)
    })
  }
  
}

#' Function to check if multiple instances of field_1 exist for a single instance of 
#' field_2.
# dataset <- conservation_unit_system_sites
# fields_1 <- c("IndexId","GFE_ID")
# fields_2 <- c("CU_NAME","CU_ACRO","CU_LAT","CU_LONGT","CU_TYPE","CU_INDEX",
#              "MAP_LABEL","FAZ_ACRO","MAZ_ACRO","JAZ_ACRO",
#              "SPECIES","SPECIES_QUALIFIED","species_acronym_ncc","POP_ID",
#              "FULL_CU_IN","SBJ_ID")
association_twoFields_fun <- function(fields_1,fields_2,dataset,silence = F){
  
  output <- expand.grid("fields_1" = fields_1, "fields_2" = fields_2)
  output <- output[order(output$fields_1),]
  output$association <- NA
  
  for(f1 in fields_1){
    # f1 <- fields_1[1]
    f1_vals <- unique(dataset[,f1])
    if(!silence){
      print(paste0("Check for ",f1,":"))
    }
    
    for(f2 in fields_2){
      # f2 <- fields_2[1]
      
      multipleVals <- sapply(X = f1_vals,FUN = function(f1_val){
        cond <- dataset[,f1] == f1_val
        f2_val <- unique(dataset[cond,f2])
        return(length(f2_val) > 1)
      })
      
      cond <- output$fields_1 == f1 & output$fields_2 == f2
      
      if(any(multipleVals)){
        if(!silence){
          print(paste0("- ",f2,": There are multiple assocations"))
        }
        output$association[cond] <- "multiple"
      }else{
        if(!silence){
          print(paste0("- ",f2,": No multiple assocations"))
        }
        output$association[cond] <- "single"
      }
    }
    if(!silence){
      print("***")
    }
  }
  return(output)
}

#' Function that find alternative CU names that are in the PSE
#' 
# CUname <- cu_notFound
# speciesAcronym <- spAcroHere
# CUname = nameNotFound[i_cu]
# speciesAcronym = species[i_sp]
CU_name_variations_fun <- function(CUname,spawnerAbundance = NA,speciesAcronym = NA){
  
  species_acronym_df <- species_acronym_fun()
  
  # quick fixes for now
  #' TODO fix that in the data base? Or anywhere else?
  if(CUname[1] == "Mussel-Kynock"){
    CUname <- c(CUname,"Mussel-Kynoch")
  }else if(CUname[1] == "Bella Colla-Dean Rivers"){
    CUname <- c(CUname,"Bella Coola-Dean Rivers")
  }else if(grepl(pattern = "NCC",x = CUname[1])){
    CUname_bis <- gsub(pattern = "NCC",
                       replacement = "North & Central Coast",
                       x = CUname[1])
    CUname_bis <- gsub(pattern = " timing",replacement = "",CUname_bis)
    # CUname_bis <- gsub(pattern = "-late",replacement = "-Late",CUname_bis)
    # CUname_bis <- gsub(pattern = "-early",replacement = "-Early",CUname_bis)
    CUname <- c(CUname,CUname_bis)
  }else if(grepl(pattern = "N ",x = CUname[1])){
    CUname <- c(CUname,gsub(pattern = "N ",replacement = "Northern ",x = CUname[1]))
  }else if(CUname[1] %in% c("Northern Coastal Fjords","Northern Coastal Streams")){
    CUname <- c(CUname,paste(CUname,"(river-type)"))
  }else if(CUname[1] == "Anderson/Seton Early Summer"){
    CUname <- c(CUname,"Anderson-Seton-Early Summer")
  }else if(grepl(pattern = " Early Summer",x = CUname[1])){
    CUname <- c(CUname,gsub(pattern = " Early Summer", replacement = "-Early Summer",x = CUname[1]))
  }else if(grepl(pattern = " Summer",x = CUname[1])){
    CUname <- c(CUname,gsub(pattern = " Summer", replacement = "-Summer",x = CUname[1]))
  }else if(CUname[1] == "Harrison River"){
    CUname <- c(CUname,"Harrison River (river-type)")
  }else if(CUname[1] %in% c("Seton Late","Seton-Late")){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      paste(CUname,"(de novo)",sep = " ")})
    CUname <- c(CUname,CUname_bis)
  }else if("HG-EAST" %in% CUname){
    CUname <- c(CUname,"East Haida Gwaii")
  }else if("HG-WEST" %in% CUname){
    CUname <- c(CUname,"West Haida Gwaii")
  }else if(CUname == "MARIAN"){
    CUname <- c(CUname,"Marian/Eden")
    # conservationunits_decoder$cu_name_pse[grepl("Marian",conservationunits_decoder$cu_name_pse)]
  }else if(CUname == "Swan"){
    CUname <- c(CUname,"Swan/Club")
  }else if(CUname == "Middle Yukon River and tributaries"){
    CUname <- c(CUname,"Middle Yukon")
  }else if(CUname == "White and tributaries"){
    CUname <- c(CUname,"White")
  }else if(CUname == "Northern Yukon River and tributaries"){
    CUname <- c(CUname,"Northern Yukon")
  }else if(CUname == "Upper Yukon River"){
    CUname <- c(CUname,"Upper Yukon")
  }else if(CUname == "Yukon River-Teslin headwaters"){
    CUname <- c(CUname,"Teslin")
  }else if(CUname == "Southwest West Vancouver Island"){
    CUname <- c(CUname,"Southwest & West Vancouver Island")
  }
  
  if(grepl(pattern = "[E|e]ven",x = CUname[1])){
    CUname <- c(CUname,gsub(pattern = "[E|e]ven",replacement = "(even)",x = CUname[1]))
  }
  if(grepl(pattern = "[O|o]dd",x = CUname[1])){
    CUname <- c(CUname,gsub(pattern = "[O|o]dd",replacement = "(odd)",x = CUname[1]))
  }
  if(grepl(pattern = "Coola Dean",x = CUname[1])){  # not a if else because goes with (odd) and odd as well
    CUname_bis <- sapply(X = CUname, FUN = function(cu){               # there are potentially multiple values in CUname
      gsub(pattern = "Coola Dean",replacement = "Coola-Dean",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "Homathko-Klinaklini-Rivers-Smith-Bella",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Rivers-Smith-Bella",replacement = "Smith-Rivers-Bella",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "Francois/Fraser",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Francois/Fraser",replacement = "Francois-Fraser",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "migrating Late",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "migrating Late",replacement = "Migrating-Late",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "-upstream",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "-upstream",replacement = "-Upstream",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "-downstream",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "-downstream",replacement = "-Downstream",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "-late",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "-late",replacement = "-Late",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "-early",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "-early",replacement = "-Early",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = " Late",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = " Late",replacement = "-Late",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = " Early",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = " Early",replacement = "-Early",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "/",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "/",replacement = "-",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = " ES",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = " ES",replacement = "-Early Summer",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "HG",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "HG",replacement = "Haida Gwaii",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "GRAHAM-ISLAND-LOWLANDS",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "GRAHAM-ISLAND-LOWLANDS",replacement = "Graham Island Lowlands",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl("Haida Gwaii",CUname[1]) & speciesAcronym == "PK"){
    oddEven <- even_odd_time_series_fun(x = spawnerAbundance)
    CUname_bis <- paste0(CUname," (",oddEven,")")
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = " Observatory",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = " Observatory",replacement = "-Observatory",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  # remove the species acronym present in certain CU names
  if(sum(sapply(X = unique(species_acronym_df$species_acro),   
                FUN = function(spa){
                  grepl(pattern = paste0(" ",spa),x = CUname[1])}))){
    
    
    spAcroTF <- sapply(X = species_acronym_df$species_acro, 
                       FUN = function(spa){
                         grepl(pattern = paste0(" ",spa),x = CUname[1])})
    
    patternToRemove <- paste0(" ",species_acronym_df$species_acro[spAcroTF])
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = patternToRemove,replacement = "",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "Portland Sound",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Portland Sound",replacement = "Portland Sound-Observatory Inlet-Lower Nass",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- c(CUname,CUname_bis)
  }
  if(grepl(pattern = "Portland Sound Observatory Inlet CO",x = CUname[1])){
    CUname <- c(CUname,"Portland Sound-Observatory Inlet-Portland Canal")
  }
  if(grepl(pattern = "Nass Portland Observatory",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Nass Portland Observatory",replacement = "Nass-Portland-Observatory",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "Mid ",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Mid ",replacement = "Middle ",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "Skeena Large",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Skeena Large",replacement = "Skeena-Large",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "Skeena Main Tribs",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Skeena Main Tribs",replacement = "Skeena-Mainstem Tributaries",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "Upper Bulkley",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Upper Bulkley",replacement = "Upper Bulkley River",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "Lower Skeena Odd",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Lower Skeena Odd",replacement = "Lower Skeena River (odd)",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "Middle Upper",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Middle Upper",replacement = "Middle-Upper",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "-Enhanced",x = CUname[1])){
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "-Enhanced",replacement = " (enhanced)",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }  
  if(grepl(pattern = "Babine",x = CUname[1]) & !grepl(pattern = "Enhanced",x = CUname[1]) & # otherwise it is Babine (enhanced)
     speciesAcronym %in% c("SX","SEL","SER")){             # otherwise it Babine for SH
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Babine",replacement = "Babine/Onerka",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "Kitwancool",x = CUname[1])){             # otherwise it Babine for SH
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Kitwancool",replacement = "Gitanyow (Kitwanga/Kitwancool)",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "Mcdonell",x = CUname[1])){             # otherwise it Babine for SH
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Mcdonell",replacement = "Mcdonell/Dennis/Aldrich",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "Morice",x = CUname[1]) & speciesAcronym %in% c("SX","SEL","SER")){     # otherwise it is Morice for SH
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "Morice",replacement = "Morice/Atna",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "SkeenaRivHigh",x = CUname[1])){     # otherwise it is Morice for SH
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "SkeenaRivHigh",replacement = "Skeena River-High Interior (river-type)",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  if(grepl(pattern = "SkeenaRivHigh",x = CUname[1])){     # otherwise it is Morice for SH
    CUname_bis <- sapply(X = CUname, FUN = function(cu){         
      gsub(pattern = "SkeenaRivHigh",replacement = "Skeena River-High Interior (river-type)",x = cu)})
    names(CUname_bis) <- NULL
    CUname <- unique(c(CUname,CUname_bis))
  }
  
  # conservationunits_decoder_cut <- conservationunits_decoder[conservationunits_decoder$species_abbr %in% speciesName,]
  # sort(unique(conservationunits_decoder_cut$cu_name_pse))
  # sort(unique(conservationunits_decoder_cut$cu_name_dfo))
  # conservationunits_decoder$cu_name_pse[grepl("[e|E]nhanced",conservationunits_decoder$cu_name_pse)]
  # conservationunits_decoder[grepl("[e|E]nhanced",conservationunits_decoder$cu_name_pse),]
  # conservationunits_decoder[grepl("Smith Inlet",conservationunits_decoder$cu_name_pse),]
  # gsub(pattern = "_",replacement = " ",x = CUs[i])
  
  return(CUname)
}

#' Function to return the last version of a file whose name contains the given
#' pattern.
import_mostRecent_file_fun <- function(wd,pattern){
  
  files_c <- list.files(wd)
  files_c <- files_c[grepl(x = files_c, 
                           pattern = pattern)]
  
  if(length(files_c) == 0){
    print("File not found.")
    out <- NA
  }else{
    file.mtime <- file.mtime(paste(wd,files_c,sep="/"))
    file <- files_c[file.mtime == max(file.mtime)]
    print(paste0("File imported: ",file," ; Date modified: ", max(file.mtime)))
    out <- read.csv(paste(wd,file,sep = "/"),header = T)
  }
  return(out)
}

#' Function used to lower case characters and remove special characters from 
#' strings. It is used to strings of characters.
simplify_string_fun <- function(string){
  
  string <- tolower(string)
  string <- gsub(" ","",string)
  string <- gsub("-","",string)
  string <- gsub("_","",string)
  string <- gsub("'","",string)
  string <- gsub("\\\\","",string)
  string <- gsub("\\(","",string)
  string <- gsub(")","",string)
  string <- gsub("\\[","",string)
  string <- gsub("]","",string)
  string <- gsub("\\.","",string)
  string <- gsub("/","",string)

  return(string)
}

#' Function to compute the euclidean distance between a reference point
#' and points whose coordinates are provided 
distance_Euclidean_fun <- function(x_ref,y_ref,x,y){
  x_ref <- as.numeric(x_ref)
  y_ref <- as.numeric(y_ref)
  x <- as.numeric(x)
  y <- as.numeric(y)
  out <- sqrt((x_ref - x)^2 + (y_ref - y)^2)
  return(out)
}

#' Function to calculate the centroids of points in a 2-dimensional space
# x <- y <- sample(1:20,5,replace = T)
centroid_2D_fun <- function(x,y){
  out <- list(mean(x),mean(y))
  names(out) <- c("x","y")
  return(out)
}

#' Function to replace a given character by another making sure the character 
#' to replace cannot be part of a anothe word that should bot be modified 
#' (e.g. " Ch" to be replaced by "Channel" when alone, but not when in 
#' "Chilliwack"). The function takes a vector of characters and returns the 
#' same vector but corrected. It also returns a boolean vector of the locations
#' in the vector of string characters that were changed. 
#' To use with abbreviations_df.
character_replace_fun <- function(charToChange, charNew, name_vector, print = F){
  
  # charToChange <- "R"
  # charNew <- "River"
  # name_vector <- c("Simpson Cr","Nechako R","Tseax R","West Road R")
  
  name_vector_split <- strsplit(x = name_vector, split = " ")
  
  name_vector_new <- lapply(X = name_vector_split, FUN = function(chr){
    # chr <- name_vector_split[[2]]
    chr[chr == charToChange] <- charNew
    out <- paste(chr,collapse = " ")
    return(out)
  })
  name_vector_new <- unlist(name_vector_new)
  
  cond <- grepl(charNew,name_vector_new)
  
  if(print){
    print("Words concerned before the change:")
    print(unique(name_vector[cond]))
    
    print("Words concerned after the change:")
    print(unique(name_vector_new[cond]))
  }
  
  out <- list(name_vector_new,name_vector)
  names(out) <- c("name_vector_new","name_vector_old")
  return(out)
}


#' Abbreviations found in locations (specifically for the hatchery data; to complete
#' eventually). To use with character_replace_fun().
abbreviations_df <- data.frame(
  abbrevation = c("R","Lk","Cr","Cv","Pd","Pds","Ch","In","Sl",
                  "Is","Strm","Strms","Pk","Sp","Cst","Val","Sd",
                  "Est","Wtshd","Tribs","Trib","Div",
                  "Hb","Fwy","Msh","N","S","E",
                  "Brk","Hd","L", "Pt","Rd"),
  word_full = c("River","Lake","Creek","Cove","Pond","Ponds","Channel","Inlet","Slough",
                "Island","Stream","Streams","Peak","Spwaning","Coast","Valley","Sound",
                "Estuary","Watershed","Tributaries","Tributary","Division",
                "Harbour","Freeway","Marsh","North","South","East",
                "Brook","Head","Little","Point","Road")
)

#' Acronyms found in locations (specifically for the hatchery data; to complete
#' eventually). To use with character_replace_fun().
# https://www.marinescience.psf.ca/wp-content/uploads/2023/05/LFR_ReleaseStrategyEvaluationBC_16July2021-Cover-Screen.pdf
# https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40594361.pdf
acronyms_df <- data.frame(
  acronym = c("UPFR","LWFR","JNST",
              "TOMF","TOMM",
              "CCST","SKNA",
              "SWVI","NWVI",
              "GSMN","GSVI",
              "QCI",
              "NCST",
              "YUKN"),
  word_full = c("Upper Fraser","Lower Fraser","Johnstone Strait",
                "Upper Thompson","Lower Thompson",               # ?!
                "Central Coast","Skeena River",
                "Southwest Vancouver Island","Northwest Vancouver Island",
                "Strait of Georgia Mainland","Strait of Georgia Vancouver Island",
                "Haida Gwaii",  # QCI stands for "Queen Charlotte Islands", which is now Haida Gwaii
                "North Coast",
                "Yukon")
)


#' Function that takes coordinates in degree, minute seconds (separated by " ")
#' and returns them in decimal format.
# angle <- SFU_stream$Latitude 
angle_to_dec_fun <- function(angle){
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split = ' '))
  x <- apply(x, 1, function(y){
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

# Function taking a vector of colours names and return a vector of the same colours
# but with more transparency added
colour_transparency_fun <- function(colours,alpha=0.35){
  col.rgb <- col2rgb(colours)
  colnames(col.rgb) <- colours
  output <- c()
  for(i in 1:length(col.rgb[1,])){
    output[i] <- rgb(red = col.rgb[1,i],
                     green = col.rgb[2,i],
                     blue = col.rgb[3,i], 
                     maxColorValue = 255,alpha = alpha*255)
  }
  return(output)
}

