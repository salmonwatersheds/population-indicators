
#'******************************************************************************
#' The goal of the script is to conduct different sensivity analyses
#' 
#' Files imported (from dropbox):
#' - 
#' 
#' Files produced:
#' - 
#'******************************************************************************

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
wd_project_dropbox <- wds_l$wd_project_dropbox
wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# The datasets to input were outputted by other scripts 
wd_data_input <- wd_output

# Import functions for this specific project
source("Code/functions.R")

# Load packages

# Paths to the repositories containing the run reconstruction datasets for each 
# region.
wd_data_regions <- wd_data_regions_fun(wd_root = wd_X_Drive1_PROJECTS)

# Import species names and acronyms
species_acronym_df <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#------------------------------------------------------------------------------#
# Selection of region(s) and species and benchmark %
#------------------------------------------------------------------------------#

# Choosing the region
# BSC: This will have to eventually be automatized and eventually allows for 
# multiple regions to be passed on.
region <- regions_df$Fraser

# multiple regions:
region <- c(
  regions_df$Haida_Gwaii,
  regions_df$Central_coast)

# all the regions
region <- as.character(regions_df[1,])

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "CK"],    
             species_acronym_df$species_name[species_acronym_df$species_acro == "SX"])

# If we do not specify the species: all the species that have a _SRdata files are 
# returned: 
# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- T

#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()


#' Sensitivity concerning the HS percentile benchmark approach ------
#' 20 years is agreed minimum number of years required to calculate benchmarks.
#' But does a lower number affect the variability of the benchmarks?

library(parallel)
detectCores()
detectCores(logical = FALSE)

# The benchmarks to use
benchmarks <- c(0.25, 0.75) # was c(0.25, 0.5)

#' Import the cuspawnerabundance.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' To calculating current spawner abundance for biostatus assessment
fromDatabase <- F
update_file_csv <- F

cuspawnerabundance <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[2],
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)
head(cuspawnerabundance)

cuspawnerabundance <- cuspawnerabundance[cuspawnerabundance$species_name != "Steelhead",]

#'* Question: How many CU are we loosing by imposing a certain nb of data point? *

# return the CUs that have a least of minimum number of data points
yr_nb <- c(3,5,10,15,20)

nb_cu_dataPt_df <- NULL
for(rg in unique(cuspawnerabundance$region)){
  # rg <- unique(cuspawnerabundance$region)[1]
  cuspawnerabundance_rg <- cuspawnerabundance[cuspawnerabundance$region == rg,]
  for(sp in unique(cuspawnerabundance_rg$species_name)){
    # sp <- unique(cuspawnerabundance_rg$species_name)[1]
    cuspawnerabundance_rg_sp <- cuspawnerabundance_rg[cuspawnerabundance_rg$species_name == sp,]
    for(cu in unique(cuspawnerabundance_rg_sp$cu_name_pse)){
      # cu <- unique(cuspawnerabundance_rg_sp$cu_name_pse)[1]
      cuspawnerabundance_rg_sp_cu <- cuspawnerabundance_rg_sp[cuspawnerabundance_rg_sp$cu_name_pse == cu,]
      ndDataPt <- sum(!is.na(cuspawnerabundance_rg_sp_cu$estimated_count))
      newRow <- data.frame(region = rg,
                           species_name = sp,
                           cu_name_pse = cu,
                           nb_dataPt = ndDataPt)
      if(is.null(nb_cu_dataPt_df)){
        nb_cu_dataPt_df <- newRow
      }else{
        nb_cu_dataPt_df <- rbind(nb_cu_dataPt_df,newRow)
      }
    }
  }
}

sort(unique(nb_cu_dataPt_df$nb_dataPt))

# remove CUs with no data points
nb_cu_dataPt_df <- nb_cu_dataPt_df[nb_cu_dataPt_df$nb_dataPt > 0,]

sapply(X = yr_nb, FUN = function(yr){
  # yr <- yr_nb[4]
  nb_cu_dataPt_df_cut <- nb_cu_dataPt_df[nb_cu_dataPt_df$nb_dataPt >= yr,]
  out <- round(percentDiff <- 100 - nrow(nb_cu_dataPt_df_cut)/nrow(nb_cu_dataPt_df)*100,1)
  names(out) <- paste0(yr,"yr")
  return(out)
})

sapply(X = yr_nb, FUN = function(yr){
  # yr <- yr_nb[4]
  nb_cu_dataPt_df_cut <- nb_cu_dataPt_df[nb_cu_dataPt_df$nb_dataPt >= yr,]
  out <- nrow(nb_cu_dataPt_df) - nrow(nb_cu_dataPt_df_cut)
  names(out) <- paste0(yr,"yr")
  return(out)
})


count <- c()
for(i in 1:max(nb_cu_dataPt_df$nb_dataPt)){
  count <- c(count,nrow(nb_cu_dataPt_df[nb_cu_dataPt_df$nb_dataPt > i,]))
}
names(count) <- 1:max(nb_cu_dataPt_df$nb_dataPt)
colours <- rep("grey60",length(count))
colours[as.numeric(names(count)) %in% c(10,15,20)] <- "red"

barplot(count, ylab = " Number of CUs", xlab = "minimum number of data points allowed",
        axis.lty=1, col = colours)


#'* Question: What is the effect of reducing nBoot (= 10000) in modelBoot() on *
#'* the CV of the medium benchmarks and their CI ? *

nb_cu_dataPt_df_20 <- nb_cu_dataPt_df[nb_cu_dataPt_df$nb_dataPt >= 20,]

sort(unique(nb_cu_dataPt_df$nb_dataPt))

nb_dataPt <- c(13,20,40,60)
nBoot_v <-  c(500,1000,3000,5000,10000) # c(10,100,1000,5000,10000)
cores_nb <- 8
nloops <- 100
printFig <- T

for(j in nb_dataPt){
  benchSummary_nBoot_ll <- list()
  for(i in 1:nloops){
    # i <- 1
    
    nb_dataPt_here <- j
    nb_cu_dataPt_df_here <- nb_cu_dataPt_df[nb_cu_dataPt_df$nb_dataPt == nb_dataPt_here,]
    
    region <- nb_cu_dataPt_df_here$region[1]
    species <- nb_cu_dataPt_df_here$species_name[1]
    CU <- nb_cu_dataPt_df_here$cu_name_pse[1]
    
    benchSummary_nBoot_l <- sensitivity_nBoot_HSPercentBM_fun(region = region,
                                                              species = species,
                                                              CU = CU,
                                                              cuspawnerabundance = cuspawnerabundance,
                                                              nBoot_v = nBoot_v,
                                                              cores_nb = cores_nb)
    benchSummary_nBoot_ll[[i]] <- benchSummary_nBoot_l
    names(benchSummary_nBoot_ll)[i] <- i
  }
  
  benchSummary_nBoot_CV <- sensitivity_nBoot_HSPercentBM_CV_fun(benchSummary_nBoot_ll)
  
  sensitivity_HSPercentBM_CV_figure_fun(benchSummary_nBoot_CV,xvar = "nBoot",wd_figures,printFig = printFig)
}


#'* Question: how much the benchmark median and CI vary as a function of yr_nb *
# yr_nb is the minimual number of year in a time series required to calculate the
# benchmarks with the percentile method.

nb_cu_dataPt_df_20 <- nb_cu_dataPt_df[nb_cu_dataPt_df$nb_dataPt >= 20,]

yr_nb <- c(10,15,20,30)
nb_dataPt <- c(40,50,60,68)

for(j in nb_dataPt){
  # j <- nb_dataPt[1]
  nb_dataPt_here <- j
  nb_cu_dataPt_df_here <- nb_cu_dataPt_df[nb_cu_dataPt_df$nb_dataPt == nb_dataPt_here,]
  
  region <- nb_cu_dataPt_df_here$region[1]
  species <- nb_cu_dataPt_df_here$species_name[1]
  CU <- nb_cu_dataPt_df_here$cu_name_pse[1]
  
  benchSummary_nbYear_df <- sensitivity_nbYear_HSPercentBM_fun(region = region,
                                                               species = species,
                                                               CU = CU, 
                                                               nloop = 100, 
                                                               cuspawnerabundance = cuspawnerabundance,
                                                               yr_nb = yr_nb, 
                                                               nBoot = 10000,
                                                               cores_nb = cores_nb)
  
  benchSummary_nbYear_CV <- sensitivity_nbYear_HSPercentBM_CV_fun(benchSummary_nbYear_df)
  
  
  sensitivity_HSPercentBM_CV_figure_fun(benchSummary_nbYear_CV, 
                                        wd_figures, 
                                        printFig = T,
                                        xvar = "dataPointNb")
}



#
# OLDER STUFF ------
#
benchSummary_nBoot_ll <- list()
for(i in 1:length(nb_dataPt)){
  # i <- 1
  
  nb_dataPt_here <- nb_dataPt[i]
  nb_cu_dataPt_df_here <- nb_cu_dataPt_df[nb_cu_dataPt_df$nb_dataPt == nb_dataPt_here,]
  
  region <- nb_cu_dataPt_df_here$region[1]
  species <- nb_cu_dataPt_df_here$species_name[1]
  CU <- nb_cu_dataPt_df_here$cu_name_pse[1]
  
  benchSummary_nBoot_l <- sensitivity_nBoot_HSPercentBM_fun(region = region,
                                                            species = species,
                                                            CU = CU,
                                                            cuspawnerabundance = cuspawnerabundance,
                                                            nBoot_v = nBoot_v,
                                                            cores_nb = cores_nb)
  benchSummary_nBoot_ll[[i]] <- benchSummary_nBoot_l
  names(benchSummary_nBoot_ll)[i] <- nb_dataPt_here
}

layout(matrix(1:length(nb_dataPt),ncol = 2,byrow = T))
for(i in 1:length(nb_dataPt)){
  # i <- 1
  
  # if(i == length(nb_dataPt)){
  #   side1 <- 4.5
  #   xaxt <- "s"
  # }else{
  #   side1 <- 0.5
  #   xaxt <- "n"
  # }
  xaxt <- "s"
  side1 <- 4.5
  par(mar = c(side1,4.5,3,.5))
  sensitivity_nBoot_HSPercentBM_figure_fun(benchSummary_nBoot_ll[[i]],xaxt = xaxt)
  mtext(text = paste0(benchSummary_nBoot_ll[[i]][[1]]$region[1]," - ",
                      benchSummary_nBoot_ll[[i]][[1]]$species[1]," - ",
                      benchSummary_nBoot_ll[[i]][[1]]$CU[1],
                      " (n = ",benchSummary_nBoot_ll[[i]][[1]]$dataPointNb[1],")"),
        side = 3, line = .8, cex = 1.2)
}

#'* Answer: it is hard to see, we need to replicate that a few time to see how *
#'* the medium benchamrks and their CIs vary.*







