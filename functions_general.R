
#'******************************************************************************
#' The goal of the script is to provide general functions that can be used in 
#' several projects.
#'******************************************************************************


#' Function that takes a dataframe and a desired number of rows and returns
#' a new dataframe filled with NAs, with the same column names, the desired
#' number of rows and NAs values.
# df <- fileSurvey_l[[sheet_i]]
dataframe_structure_fun <- function(df,nrow = NA){
  
  if(is.na(nrow)){
    nrow <- nrow(df)
  }
  
  out <- as.data.frame(matrix(NA,nrow =nrow, ncol = ncol(df)))
  colnames(out) <- colnames(df)
  return(out)
}


