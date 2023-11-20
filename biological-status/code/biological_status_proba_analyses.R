
#'******************************************************************************
#' The goal of the script is to analyse the biological status probabilities 
#' obtained from the HBSRM analysis.
#' 
#' Files imported (from ):
#' - region_species_biological_status.csv (created in benchmarks_HBSRM.R)
#' 
#' Files produced: 
#' - Biological_status_HBSRM_all.csv
#' 
#' Notes:
#' - 
#' 
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
source("code/functions_set_wd.R")
source("code/functions_general.R")

# return the name of the directories for the different projects:
subDir_projects <- subDir_projects_fun()

wds_l <- set_working_directories_fun(subDir = subDir_projects$biological_status,
                                     Export_locally = F)
wd_head <- wds_l$wd_head
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS
wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# The datasets to input were outputted by other scripts 
wd_data_input <- wd_output

# Import functions for this specific project
source("Code/functions.R")



# Import species names and acronyms
species_acronym <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#------------------------------------------------------------------------------#
# Analyses
#------------------------------------------------------------------------------#

#' Import the files for all regions, species and CUs and rbind them to a single 
#' dataframe.
region <- as.character(regions_df[1,])
region <- region[region != "Columbia"]
species <- c(
  species_acronym$Sockeye,    
  species_acronym$Pink,
  species_acronym$Coho
  #species_acronym$Cutthroat,
  #species_acronym$Chum
)
# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- TRUE
pattern <- "biological_status"
region <- gsub(" ","_",region)

biological_status_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)


write.csv(biological_status_df,paste0(wd_output,"/Biological_status_HBSRM_all.csv"),
          row.names = F)
head(biological_status_df)
colnames(biological_status_df)
nrow(biological_status_df) # 130
unique(biological_status_df$comment)

# discrepancies in CU names
sum(gsub("_"," ",biological_status_df$CU) != biological_status_df$CU_pse)/nrow(biological_status_df)
sum(gsub("_"," ",biological_status_df$CU) != biological_status_df$CU_dfo)/nrow(biological_status_df)
sum(biological_status_df$CU_pse != biological_status_df$CU_dfo)/nrow(biological_status_df)

# CUs with "Only NAs in cuspawnerabundance.csv for this CU" 
biological_status_df[!is.na(biological_status_df$comment) & biological_status_df$comment == "Only NAs in cuspawnerabundance.csv for this CU" ,]

# CUs with "Not recent enough data"
biological_status_df[!is.na(biological_status_df$comment) & grepl("Not recent enough data",biological_status_df$comment),]

# rest of the CUs with data:
biological_status_df <- biological_status_df[is.na(biological_status_df$comment) | biological_status_df$comment == "",]
nrow(biological_status_df) # 110

# CUs that have contrasting biological status between Smsy80 and Smsy:
colnamesSelect <- c("region","species","CU",colnames(biological_status_df)[grepl("Smsy_",colnames(biological_status_df))])

biological_status_df$status_Smsy <- sapply(X = 1:nrow(biological_status_df), 
                                          FUN = function(r){
                                            # r <- 1
                                            slice <- biological_status_df[r,colnames(biological_status_df)[grepl("Smsy_",colnames(biological_status_df))]]
                                            out <- c("red","amber","green")[slice == max(slice)]
                                            return(out)
                                          })

biological_status_df$status_Smsy80 <- sapply(X = 1:nrow(biological_status_df), 
                                           FUN = function(r){
                                             # r <- 1
                                             slice <- biological_status_df[r,colnames(biological_status_df)[grepl("Smsy80_",colnames(biological_status_df))]]
                                             out <- c("red","amber","green")[slice == max(slice)]
                                             return(out)
                                           })

# 
biological_status_df[biological_status_df$status_Smsy != biological_status_df$status_Smsy80,]
# that's not a lot of CUs!


#' Import the associated benchmark values
pattern <- "benchmarks_summary"

benchmarks_summary_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                       wd_output = wd_output,
                                                       region = region,
                                                       species_all = F)

head(benchmarks_summary_df)
nrow(unique(benchmarks_summary_df[,c("region","species","CU")])) # 146








