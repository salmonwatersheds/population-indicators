
#'******************************************************************************
#' The goal of the script is to produce functions that help setting the working 
#' directories for the different projects.
#'******************************************************************************


#' Function to return a list if the names of the (sub) directories of the 
#' different projects (e.g. "biological-status").
subDir_projects_fun <- function(){
  
  subDir_options <- c("biological-status",
                      "hatchery-releases",
                      "spawner-abundance",
                      "spawner-surveys",
                      "timing")
  
  # make a list
  out_l <- lapply(X = subDir_options, FUN = function(x){x})
  
  names(out_l) <- gsub("-","_",subDir_options)
  
  return(out_l)
}

#' Function to set up the directories and sub directories for the project.
#' subDir is the name of the focal project (e.g. "biological-status").
#' The function also helps dealing with switching from the different projects 
#' without havig to restart RStudio.
#' Export_locally: if TRUE, figures and tables are export locally in /figures and 
#' /output, if FALSE, they are exported in the corresponding repository in the 
#' PSF dropbox.
#' e.g. subDir <- subDir_projects_fun()$biological_status
set_working_directories_fun <- function(subDir = NA, Export_locally = T){
  
  # set the path of the project's head 
  dirhead <- "population-indicators"
  path_ahead <- sub(pattern = paste0("\\",dirhead,".*"),replacement = "", x = getwd())
  wd_head <- paste0(path_ahead,dirhead)   # th path to /population-indicators
  
  # Import the path of for the dropbox X_Drive/1_PROJECTS
  setwd(wd_head)
  # The pass ../Salmon Watersheds Dropbox/user_name/X Drive/1_PROJECTS.
  # The pass is personal and must be copy past in wd_X_Drive1_PROJECTS.txt
  # e.g.: "C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS"
  # TODO: find generic solution
  wd_X_Drive1_PROJECTS <- readLines( "wd_X_Drive1_PROJECTS.txt")
  
  # get the name of the sub directories for the different projects
  subDir_options_l <- subDir_projects_fun()
  
  if(is.na(subDir)[1]){
    
    print("Please provide the name of the project for subDir (you can use subDir_projects_fun() to find the exact spelling.)")
    
  }else if(!subDir %in% unlist(subDir_options_l)){
    
    print("The name of the project you provided for subDir is incorrect (you can use subDir_projects_fun() to find the exact spelling.)")
    
  }else{
    
    wd_project <-  paste(wd_head,subDir,sep = "/")       # the path of this sub repo
    setwd(dir = wd_project)
    print("Working directories are set to:")
    print(wd_project)
    
  }
  
  # Define subsubdirectories:
  wd_code <- paste0(getwd(),"/code")
  wd_data <- paste0(getwd(),"/data")
  
  print(wd_code)
  print(wd_data)
  
  
  # figures and datasets generated are
  if(Export_locally){
    wd_figures <- paste0(getwd(),"/figures")
    wd_output <- paste0(getwd(),"/output")
  }else{
    
    # The dropbox path to the /population-indicators/biological-status folder:
    wd_project_dropbox <- paste0("1_Active/Population Methods and Analysis/population-indicators/",subDir)
    
    wd_figures <- paste0(wd_X_Drive1_PROJECTS,"/",wd_project_dropbox,"/figures")
    wd_output <- paste0(wd_X_Drive1_PROJECTS,"/",wd_project_dropbox,"/output")
  }
  
  print(wd_figures)
  print(wd_output)
  
  out_l <- list(wd_head,wd_code,wd_data,wd_figures,wd_output,
                wd_X_Drive1_PROJECTS,wd_project_dropbox)
  names(out_l) <- list("wd_head","wd_code","wd_data","wd_figures","wd_output",
                       "wd_X_Drive1_PROJECTS","wd_project_dropbox")
  return(out_l)
}

# Function that returns a data frame with the path leading to the repository where 
# input data (i.e., run reconstructions) is located for each region.
# wd_Data_input_root is the root directory common to all the regions.
# BSC: we probably will need to organize these datasets better
wd_data_regions_fun <- function(wd_root = ""){
  
  wd_data_regions <- data.frame(
    Central_coast = paste0(wd_root,"/1_Active/Central Coast PSE/analysis/central-coast-status/HBM and status"),
    Columbia = paste0(wd_root,"/1_Active/Columbia/data & analysis/analysis/columbia-status"), # BSC: ? no HBM and status folder... to check at some point
    Fraser = paste0(wd_root,"/1_Active/Fraser_VIMI/analysis/fraser-status/HBM and status"),
    # Fraser = paste0(wd_root,"/Fraser_VIMI/analysis/fraser-status/HBM and status"),
    Haida_Gwaii = paste0(wd_root,"/1_Active/Haida Gwaii PSE/Data & Assessments/haida-gwaii-status/HBM and status"),
    Nass = paste0(wd_root,"/1_Active/Nass/assessments/2_population/nass-status/HBM and status"),
    # Nass = paste0(wd_root,"/Nass/assessments/2_population/nass-status/HBM and status"),
    Skeena = paste0(wd_root,"/1_Active/Skeena Updates/skeena-status/HBM and status"),
    # Skeena = paste0(wd_root,"/Skeena Updates/skeena-status/HBM and status"),
    Yukon = paste0(wd_root,"/1_Active/Yukon/Data & Assessments/yukon-status/HBM-and-status"))
  
  return(wd_data_regions)
}


