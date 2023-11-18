
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
    species_acro = c("CK","CM","CO","SX","SX","SX","PK","PK","PK","SH","CT"))
  
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

