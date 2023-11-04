
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
relationships_twoCol_df_fn <- function(df,col1,col2,printDF = F){
  
  df <- as.data.frame(df)
  df <- df[,c(col1,col2)]
  df <- na.omit(df)
  df <- unique(df)
  
  messagePrinted <- F
  for(c in unique(df[,col1])){
    # c <- unique(df[,col1])[1]
    df_cut <- df[df[,col1] == c,]
    if(nrow(df_cut) > 1){
      if(!messagePrinted){
        print(paste0("The are potentially multiple values of '",col2,"' for a single value in '",col1,"'."))
        messagePrinted <- T
      }
      if(printDF){
        print(df_cut)
      }
    }
  }
  if(!messagePrinted){
    print(paste0("The is a unique value of '",col2,"' for each value in '",col1,"'."))
  }
}


