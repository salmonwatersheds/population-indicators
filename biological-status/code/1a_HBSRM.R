
#'******************************************************************************
#' The goal of the script is to conduct a hierarchical Bayesian spawner recruits 
#' (HBSR) R analysis for all regions and conservation units.
#' Code adapted from Korman and English (2013).
#' 
#' Files imported (from dropbox):
#' - recruitsperspawner.csv (from database)
#' - conservationunits_decoder (from database)
#' - data/priors_HBSRmodel.csv (created in checks_fixes.R which assemble the 
#' SRdata.txt files present in the "HBM and status" subfolders in each 
#' region-specific folders in dropbox)
#' 
#' Files produced: 
#' - output/intermediate/REGION_SPECIES_SR_matrices.rds                 # SR recruits matrices data used to fit the HBSRM
#' - output/intermediate/REGION_SPECIES_HBSRM_posteriors_priorShift.rds # Posterior distributions of the HBSRM `a_i`, `b_i`, `mu_a` and `sigma_b_i`
#' - output/intermediate/REGION_SPECIES_HBSRM_convDiagnostic.csv        # Gelman and Rubin (1992)'s convergence diagnostic of the MCMC output
#' 
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
wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

wd_pop_data_dropbox <- paste(wd_X_Drive1_PROJECTS,
                              wds_l$wd_population_indicator_dropbox,
                              "biological-status/data", sep = "/")

# wd_output <- gsub("/output","/output_NORMAL_DIST",wd_output)

# Import functions for this specific project
source("Code/functions.R")

# Load packages
library(R2jags)  # Provides wrapper functions to implement Bayesian analysis in JAGS.
library(modeest) # Provides estimators of the mode of univariate data or univariate distributions. ??? needed ?
library(tidyverse)

# Paths to the repositories containing the run reconstruction datasets for each 
# region.
wd_data_regions <- wd_data_regions_fun(wd_root = wd_X_Drive1_PROJECTS)

# Import species names and acronyms
species_acronym_df <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

#' Import the recruitsperspawner.csv from population-indicators/data_input or 
#' download it from the PSF database
fromDatabase <- update_file_csv <- F

recruitsperspawner <- datasets_database_fun(nameDataSet = "recruitsperspawner.csv",
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)

#' Import the conservationunits_decoder.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- datasets_database_fun(nameDataSet = "conservationunits_decoder.csv",
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)
# colnames(conservationunits_decoder)[colnames(conservationunits_decoder) == "species_abbr"] <- "species_qualified"

# remove the SMU-related information because that causes issues lower
nrow(conservationunits_decoder) # 470
conservationunits_decoder <- conservationunits_decoder[,!grepl("smu",colnames(conservationunits_decoder))]
conservationunits_decoder <- unique(conservationunits_decoder)
nrow(conservationunits_decoder) # 469

#'* External values for prior for Smax *

#'1) Skeena SEL for which prSmax is estimated with a photosynthetic-based
# approach (from Korman & English 2013; cf. Table SX.1 p. 33)
cuid_Skeena_SEL <- c(171,191,192,193,175,176,183,177,178,185,197,187)

prior_Korman_English_2013 <- data.frame(cuid = cuid_Skeena_SEL)
prior_Korman_English_2013$region <- sapply(prior_Korman_English_2013$cuid,function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(conservationunits_decoder$region[cond])
})
prior_Korman_English_2013$species_qualified <- sapply(prior_Korman_English_2013$cuid,function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(conservationunits_decoder$species_qualified[cond])
})
prior_Korman_English_2013$cu_name_pse <- sapply(prior_Korman_English_2013$cuid,function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(conservationunits_decoder$cu_name_pse[cond])
})

prior_Korman_English_2013 <- prior_Korman_English_2013[,c("region","species_qualified","cu_name_pse","cuid")]

# defined CV: in p. 13
# 0.3 for all SE CUs, except for the four Babine Systems for which it is 1
prior_Korman_English_2013$prCV <- 0.3

#' Note1: CU Swan/Club (cuid 188) was also in this list above but the CU
#' was assigned to the Stephens CU, following DFO update
#' The change in the PSE was made in August 2024
#' https://salmonwatersheds.slack.com/archives/CPD76USTU/p1726165574970689

#' Note 2:  There is no values given for Asitika cuid 190 in the report, 
#' it was removed from the list.

prior_Korman_English_2013$prSmax <- NA
cond <- prior_Korman_English_2013$cu_name_pse == "Alastair"
prior_Korman_English_2013$prSmax[cond] <- 23437
cond <- prior_Korman_English_2013$cu_name_pse == "Azuklotz"
prior_Korman_English_2013$prSmax[cond] <- 5933
cond <- prior_Korman_English_2013$cu_name_pse == "Bear"
prior_Korman_English_2013$prSmax[cond] <- 40532
cond <- prior_Korman_English_2013$cu_name_pse == "Damshilgwit"
prior_Korman_English_2013$prSmax[cond] <- 2000
cond <- prior_Korman_English_2013$cu_name_pse == "Johnston"
prior_Korman_English_2013$prSmax[cond] <- 4125
cond <- prior_Korman_English_2013$cu_name_pse == "Kitsumkalum"
prior_Korman_English_2013$prSmax[cond] <- 20531
cond <- grepl("Kitwancool",prior_Korman_English_2013$cu_name_pse)
prior_Korman_English_2013$prSmax[cond] <- 36984
cond <- prior_Korman_English_2013$cu_name_pse == "Lakelse"
prior_Korman_English_2013$prSmax[cond] <- 35916
cond <- grepl("Mcdonell",prior_Korman_English_2013$cu_name_pse)
prior_Korman_English_2013$prSmax[cond] <- 4072
cond <- grepl("Morice",prior_Korman_English_2013$cu_name_pse)
prior_Korman_English_2013$prSmax[cond] <- 191362
cond <- prior_Korman_English_2013$cu_name_pse == "Stephens"
prior_Korman_English_2013$prSmax[cond] <- 7069
cond <- prior_Korman_English_2013$cu_name_pse == "Motase"
prior_Korman_English_2013$prSmax[cond] <- 1764
  
# File of prior values for Smax for SEL from Atlas et al. 2025
# https://github.com/DylanMG/DL-CC-sockeye/blob/main/output/data/SmaxPRs.txt
prior_Atlas_et_al_2025 <- read.table(paste0(wd_pop_indic_data_input_dropbox,"/SmaxPRs.txt"), 
                         header = T)
prior_Atlas_et_al_2025 <- prior_Atlas_et_al_2025[,c("region","population","SmaxPR","SmaxPR_SD")]
prior_Atlas_et_al_2025$population <- gsub("_"," ",prior_Atlas_et_al_2025$population)
cu_pse_simple <- simplify_string_fun(conservationunits_decoder$cu_name_pse)
prior_Atlas_et_al_2025$cuid <- sapply(prior_Atlas_et_al_2025$population,function(cu){
  # cu <- "babine"
  # cu <- priors$population[1]
  
  if(grepl("tuno",cu)){
    cu <- strsplit(cu," ")[[1]]
    cu <- paste0(cu[1],"creek",cu[2])
  }
  if(grepl("freeda",cu)){
    cu <- "freeda"
  }
  if(grepl("mary cove",cu)){
    cu <- "mary cove creek"
  }
  if(grepl("shawaltan",cu)){
    cu <- "shawatlan"
  }
  if(grepl("babine",cu)){
    cu <- "babine/onerka"
  }
  if(grepl("lowe",cu)){
    cu <- "Lowe/Simpson/Weir"
  }
  cond <- cu_pse_simple == cu & 
    conservationunits_decoder$species_qualified == "SEL"
  
  if(!any(cond)){
    cond <- grepl(cu,cu_pse_simple) & 
      conservationunits_decoder$species_qualified == "SEL"
  }
  if(!any(cond)){
    cond <- cu_pse_simple == simplify_string_fun(cu) & 
      conservationunits_decoder$species_qualified == "SEL"
  }
  if(!any(cond)){
    out <- NA
  }else if(cu %in% c("babine/onerka","mcdonell","morice")){
    out <- unique(conservationunits_decoder$pooledcuid[cond])
  }else{
    out <- conservationunits_decoder$cuid[cond]
  }
  return(out)
})

cond_NA <- is.na(prior_Atlas_et_al_2025$cuid)
prior_Atlas_et_al_2025[cond_NA,]
#      region population SmaxPR cuid
# low.coastal     hauyat   4906   NA
#    interior       swan  56574   NA --> to add to Stephens eventually

prior_Atlas_et_al_2025$cu_name_pse <- sapply(prior_Atlas_et_al_2025$cuid,function(cuid){
  if(is.na(cuid)){
    out <- NA
  }else{
    cond <- conservationunits_decoder$cuid == cuid
    out <- conservationunits_decoder$cu_name_pse[cond]
  }
  return(out)
})

prior_Atlas_et_al_2025$species_qualified <- sapply(prior_Atlas_et_al_2025$cuid,function(cuid){
  if(is.na(cuid)){
    out <- NA
  }else{
    cond <- conservationunits_decoder$cuid == cuid
    out <- conservationunits_decoder$species_qualified[cond]
  }
  return(out)
})

# Remove the one population that does not match
cond_NA <- is.na(prior_Atlas_et_al_2025$cu_name_pse)
prior_Atlas_et_al_2025[cond_NA,]
prior_Atlas_et_al_2025 <- prior_Atlas_et_al_2025[!cond_NA,]

prior_Atlas_et_al_2025$region <- sapply(prior_Atlas_et_al_2025$cuid,function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  out <- conservationunits_decoder$region[cond]
  return(out)
})

# Determine the CV from SD
prior_Atlas_et_al_2025$SmaxPR_CV <- prior_Atlas_et_al_2025$SmaxPR_SD / prior_Atlas_et_al_2025$SmaxPR

# Compare the two datasets
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1748973321001659?thread_ts=1748970773.018779&cid=CJ5RVHVCG
prior_compare <- merge(x = prior_Korman_English_2013,
                       y = prior_Atlas_et_al_2025, 
                       by = c("region","species_qualified","cu_name_pse","cuid"))

par(mar = c(2,4,3,1))
layout(matrix(1:12,byrow = T, ncol = 4))
for(r in 1:nrow(prior_compare)){
  # r <- 1
  cuid <- prior_compare$cuid[r]
  cu_name_pse <- prior_compare$cu_name_pse[r]
  
  cond <- prior_compare$cuid == cuid
  S_altas <- prior_compare$SmaxPR[cond]
  S_Korman <- prior_compare$prSmax[cond]
  
  cond <- recruitsperspawner$cuid == cuid
  S_mean <- mean_geom_fun(x = recruitsperspawner$spawners[cond])
  S_max <- max(recruitsperspawner$spawners[cond], na.rm = T)
  
  hist(x = recruitsperspawner$spawners[cond], xlab = "", 
       xlim = c(0,max(S_max,S_altas,S_Korman)),
       main = paste(cu_name_pse,cuid,sep = " - "))
  segments(x0 = c(S_mean,S_altas,S_Korman),x1 = c(S_mean,S_altas,S_Korman),
           y0 = 0, y1 = 40, lwd = 2, col = c("black","red","blue"))
}
plot.new()
legend("center",c("geo mean","Atlas et al. 2025","Korman & English 2013"),
       col = c("black","red","blue"), lwd = 2, bty = "n")

# Decision: We use Atlas et al. 2025 when there is conflict between the two CUs
prior_Korman_English_2013$source <- "Korman & English 2013"
prior_Atlas_et_al_2025$source <- "Atlas et al. 2025"
prior_extra <- prior_Atlas_et_al_2025
colnames(prior_extra)[colnames(prior_extra) == "SmaxPR"] <- "prSmax" 
colnames(prior_extra)[colnames(prior_extra) == "SmaxPR_CV"] <- "prCV" 
prior_extra <- prior_extra[,c("region","species_qualified","cu_name_pse","cuid","prSmax","prCV","source")]

# the only CU in English & Korman and not in Atlas et al.
cond <- !prior_Korman_English_2013$cuid %in% prior_extra$cuid
prior_Korman_English_2013[cond,]

prior_extra <- rbind(prior_extra,prior_Korman_English_2013[cond,])

#' Remove Swan and Stephens TEMPORARY
#' From Pop meeting 02/06/2025:
#' the R per S data has not been update as recently as the estimated S and the 
#' values are low for S in the R per S in comparison.
cond <- prior_extra$cu_name_pse %in% c("Swan","Stephens")
prior_extra <- prior_extra[!cond,]

prior_extra <- prior_extra %>%
  arrange(region,cu_name_pse)

prior_extra$species_name <- sapply(prior_extra$cuid,function(cuid){
  cond <- conservationunits_decoder$cuid == cuid
  return(conservationunits_decoder$species_name[cond])
})

prior_extra <- prior_extra[,c("region","species_name","species_qualified",
                              "cu_name_pse","cuid","prSmax","prCV","source")]

write.csv(prior_extra,paste0(wd_data,"/priors_Smax.csv"), row.names = F)
write.csv(prior_extra,paste0(wd_pop_data_dropbox,"/priors_Smax.csv"), row.names = F)

#------------------------------------------------------------------------------#
# Selection of region(s) and species
#------------------------------------------------------------------------------#

# option to show the SR plot
show_figures <- F

# Choose one region
region <- regions_df$Yukon

# multiple regions:
region <- c(
  regions_df$WVI,
  regions_df$EVIMI)

# all the regions
region <- as.character(regions_df[1,])
# region <- region[region != "Columbia"]

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
cond_sp <- species_acronym_df$species_qualified_simple == "CK" | 
           species_acronym_df$species_qualified_simple == "SE"
cond_sp <- species_acronym_df$species_qualified_simple == "SE"
species <- species_acronym_df$species_name_simple[cond_sp] |> unique()

# For all species
species <- species_acronym_df$species_name_simple |> unique()

#' Import the prior values for the HBSR model parameters prSmax and prCV that are
#' used in HBSRM.R (the file is created in checks_fixes.R and contains the values 
#' of these priors that were originally contained in SRdata.txt files that are 
#' found in the "HBM and status" subfolders in each region-specific folders.
#' NOT USED ANYMORE
# priors_HBSRmodel <- read.csv(paste0(wd_data,"/priors_HBSRmodel.csv"),header = T)

# Set first brood year, "-99" for no constraint
FBYr <- -99

# Set minimum nb of SR data points required to be included in the analysis
MinSRpts <- 3 

# Set the HBSR number of simulations, burning runs and chains:
n.iter <- 50000 # 100000  # --> 10000 only
n.burnin <- 3000 # 5000  # 
n.chains <- 6     # 

# The Gelman and Rubin's convergence of parameters Rc
# if Rc > Rc_max_cut --> prCV = 1 instead of 10 
Rc_max_cut <- 1.1 # 1.2

# options(warn=1)  # print warnings as they occur
# options(warn = 2)  # treat warnings as errors
options(warn = 0)  # 

#----------------------------------------------------------------------------#
# Read in Stock-Recruit Data, run the HBSR model and output parameter estimates
#----------------------------------------------------------------------------#

for(i_rg in 1:length(region)){
  # i_rg <- 1
  
  cond_rs_rg <- recruitsperspawner$region == region[i_rg]
  # recruitsperspawner_rg <- recruitsperspawner[recruitsperspawner$region == region[i_rg],]
  
  species_acro <- sapply(X = species,FUN = function(sp){
    cond <- species_acronym_df$species_name == sp
    return(species_acronym_df$species_qualified_simple[cond])
  })
  
  regionName <- region[i_rg]
  # if(region[i_rg] == "Vancouver Island & Mainland Inlets"){
  #   regionName <- "VIMI"
  # }
  if(region[i_rg] == "West Vancouver Island"){
    regionName <- "WVI"
  }
  if(region[i_rg] == "East Vancouver Island & Mainland Inlets"){
    regionName <- "EVIMI"
  }
  
  cond <- all(is.na(recruitsperspawner$spawners[cond_rs_rg])) | 
          all(is.na(recruitsperspawner$recruits[cond_rs_rg]))
  
  if(cond){
    
    print(paste0("*** There is no data in recruitsperspawner.csv for salmon in ",region[i_rg]," ***"))
    
  }else{
    
    for(i_sp in 1:length(species_acro)){
      # i_sp <- 4
      
      speciesAcroHere <- species_acro[i_sp]
      
      cond <- species_acronym_df$species_qualified_simple == speciesAcroHere
      speciesHere <- species_acronym_df$species_name[cond] |> unique()
      
      cond_rs_rg_sp <- cond_rs_rg & recruitsperspawner$species_name %in% speciesHere
      
      if(sum(cond_rs_rg_sp) == 0){
        
        print(paste0("The species ",speciesHere," is not present in ",region[i_rg] ," in the dataset used."))
        
      }else{
        
        cond_cud_rg_sp <- conservationunits_decoder$region == region[i_rg] &
          conservationunits_decoder$species_name %in% speciesHere
        
        print(paste0("*** Region - species: ",region[i_rg]," - ",speciesAcroHere," ***"))
        
        #'* Define the spawner (S) and recruits (R) matrices *
        
        # organize the data into a year x CU for R and S:
        CUs <- unique(recruitsperspawner$cu_name_pse[cond_rs_rg_sp])

        nCUs <- length(CUs)
        Yrs <- min(recruitsperspawner$year[cond_rs_rg_sp]):max(recruitsperspawner$year[cond_rs_rg_sp])
        nYrs <- length(Yrs)
        
        S <- R <- matrix(nrow = nYrs, ncol = nCUs, dimnames = list(Yrs,CUs))
        for(j in 1:nCUs){
          # j <- 1
          dj <- subset(recruitsperspawner[cond_rs_rg_sp,],cu_name_pse == CUs[j])
          S[as.character(dj$year),j] <- dj$spawners # dj$Esc
          R[as.character(dj$year),j] <- dj$recruits # dj$Rec
        }
        
        # remove the row with NAs in S but not R and vice versa of a same CU
        SR_l <- cuSR_removeNA_fun(R = R, S = S)
        R <- SR_l$R
        S <- SR_l$S
        
        # replace 0s by 1 to avoid the lm(log(R/S)~ S) to crash
        R <- apply(X = R,MARGIN = 2,FUN = function(c){
          # c <- R[,3]
          out <- c
          out[which(out == 0)] <- 1
          return(out)
        })
        S <- apply(X = S,MARGIN = 2,FUN = function(c){
          # c <- R[,3]
          out <- c
          out[which(out == 0)] <- 1
          return(out)
        })
        
        #' filter CUs with less than MinSRpts data points 
        CuToRemove <- c()
        for(j in 1:ncol(S)){
          # j <- 1
          CUHere <- colnames(S)[j]
          if(sum(!is.na(S[,CUHere])) < MinSRpts | sum(!is.na(R[,CUHere])) < MinSRpts){
            CuToRemove <- c(CuToRemove,CUHere)
          }
        }
        S <- S[,!colnames(S) %in% CuToRemove, drop = F]
        R <- R[,!colnames(R) %in% CuToRemove, drop = F]
        CUs <- CUs[!CUs %in% CuToRemove]

        nCUs <- length(CUs)
        SR_l$R <- R
        SR_l$S <- S
        
        #'* Separate the process for cyclic and non-cyclic CUs *
        cond <- grepl("cyclic",colnames(R))
        CU_cyclic <- colnames(R)[cond]
        CU_cyclicNO <- colnames(R)[!cond]
        
        for(i_CUs in 1:2){
          # i_CUs <- 1
          CUs_here <- list(CU_cyclicNO,CU_cyclic)[[i_CUs]]
          
          if(length(CUs_here) > 0){
            
            R <- SR_l$R[,CUs_here,drop = F]
            S <- SR_l$S[,CUs_here,drop = F]
            
            SR_l_here <- list()
            SR_l_here$R <- R
            SR_l_here$S <- S
            
            if(any(grepl("cyclic",CUs_here))){
              cyclic_filen <- "_cyclic"
            }else{
              cyclic_filen <- ""
            }
            
            file_name <- paste0(wd_output,"/intermediate/",gsub(" ","_",regionName),"_",
                                speciesAcroHere,cyclic_filen,"_SR_matrices.rds")
            saveRDS(SR_l_here,file = file_name)
            
            nCUs <- length(CUs_here)
            
            CUs_cuid <- sapply(CUs_here,function(cu){
              cond_cu <- conservationunits_decoder$cu_name_pse == cu
              cond_rg <- conservationunits_decoder$region == region[i_rg]
              cond_sp <- conservationunits_decoder$species_name %in% speciesHere
              val <- conservationunits_decoder$cuid[cond_rg & cond_sp & cond_cu]
              if(length(val) > 1){ # in case of pooled CUs
                val <- conservationunits_decoder$pooledcuid[cond_rg & cond_sp & cond_cu]
                val <- unique(val)
              }
              return(val)
            })
            
            #'* Set priors on b: prSmax and prCV *
            # select the prior for these CUs and check those that do not have values
            # priors_HBSRmodel_rg_sp <- priors_HBSRmodel[priors_HBSRmodel$region == region[i_rg] &
            #                                              priors_HBSRmodel$species %in% speciesHere,]
            # THIS priors_HBSRmodel IS AN OLD LIST COMING FROM TXT FILES FOR WHICH THE CREATING CODE IS 
            # LOST.
            CUs_priors <- prior_beta_Ricker_fun(cuid = CUs_cuid,
                                                conservationunits_decoder = conservationunits_decoder,
                                                Sm = S,
                                                prior_extra = prior_extra) # provide an extra list of CUs with prior available
            
            # Define corresponding parameters to feed the HBSRM:
            prSmax <- CUs_priors$prSmax
            prCV <- CUs_priors$prCV
            prmub <- log(1/prSmax)    # convert mean prior on Smax to log b for winbugs model
            # prtaub <- 1/prCV^2				# convert from cv to tau --> BSC: should be prtaub <- prmub^2/prCV^2 ???
            prtaub <- prmub^2/prCV^2
            
            #### Estimate a and b by linreg and plot
            if(show_figures){
              if(.Platform$OS.type == "windows"){      # BSC: what to do with Linux? : x11()?
                windows()                              # Also do we want it to open in a new window vs in Rstudio?
              }else{
                quartz()
              }
            }
            
            #------------------------------------------------------------------------------#
            #  Bayes model defined with rjags
            #------------------------------------------------------------------------------#
            
            # Replace NAs with a value so that the model can run. Note that obs_lnRS still
            # has NAs, so the it does not matter what pred_lnRS is for these cases because
            # they are not considered in the likelihood calcutation (due to NAs in obs_lnRS).
            S[is.na(S)] <- -99    # could put any value
            nYrs <- nrow(S)
            
            # jags inputs:
            jags.data <- list(
              nCUs = nCUs,
              nYrs = nYrs,
              obs_lnRS = log(R/S),  # data on observed log(recruits/spawners) - matrix nYrs x nCU
              S = S,         # data on observed spawners - matrix nYrs x nCU 
              prmub = prmub, # prior on mu for b
              prtaub = prtaub # prior on tau for b
            )
            
            jags.parms <- c("a", "b", 
                            "sd",     # the sd for the loglikelihood function
                            "mu_a", "sd_a")
            
            jags_m_here <- HBSRM_JAGS_fun(model_name = "Ricker",
                                          modelFilename = NA,
                                          jags.data = jags.data, 
                                          jags.parms = jags.parms, 
                                          n.thin = 10,             # to keep every 10th data point
                                          n.burnin = n.burnin,
                                          n.chains = n.chains)
            print("")
            
            # check convergence of parameters with Gelman and Rubin's Rc
            convergence <- gelman.diag(jags_m_here$post)$psrf
            cond_deviance <- rownames(convergence) == "deviance"
            convergence <- convergence[!cond_deviance,] # remove the "deviance" column because it is not a model parameter
            
            if(any(convergence[,"Point est."] > Rc_max_cut)){
              
              # convergence[ rownames(convergence) == "sd_a","Point est."] <- 1.2
              # convergence[ rownames(convergence) == "b[3]","Point est."] <- 1.2
              # convergence[ rownames(convergence) == "sd[3]","Point est."] <- 1.2
              # convergence[ rownames(convergence) == "b","Point est."] <- 1.2
              
              # Return the parameters with Point estimate > Rc_max_cut
              cond_issue <- convergence[,"Point est."] > Rc_max_cut
              para_issue <- rownames(convergence)[cond_issue]
              
              # separate the alpha and the other CU-specific parameters
              para_issue_alpha <- para_issue[para_issue %in% c("mu_a","sd_a")]
              para_issue_others <- para_issue[! para_issue %in% para_issue_alpha ]
              
              # If there are convergence issue associated with parameters other 
              # than mu_a and sd_a (it is ok for those hyper parameters to not 
              # converge as long as alpha do).
              if(length(para_issue_others) > 0){
                
                if(any(grepl("]",para_issue_others))){ # more than one CU in the set (e.g. "a[1]", "a[2]")
                  # Find the CUs concerned
                  CU_nb <- sapply(X = para_issue_others, function(c){
                    out <- gsub("b","",c)
                    out <- gsub("a","",out)
                    out <- gsub("sd","",out)
                    out <- gsub("\\[","",out)
                    out <- gsub("]","",out)
                    return(as.numeric(out))
                  })
                  
                  CU_nb <- sort(unique(CU_nb))
                  
                }else{ # only one CU in the set (e.g. "a")
                  CU_nb <- 1
                }
                
                CUs_concerned <- CUs_here[CU_nb]
                
                print(paste0("*** Convergence issue with model parameters: ",
                             region[i_rg]," ",species_acro[i_sp]," ",cyclic_filen,": ",
                             paste0(CUs_concerned,collapse = " ; ")," for:",
                             paste0(c(para_issue_others,para_issue_alpha),collapse = " ; ")))
                
                # Provide a more informative prCV = 1
                cond <- CUs_priors$cu_name_pse %in% CUs_concerned
                CUs_priors$prCV[cond] <- 1
                
                # Define corresponding parameters to feed the HBSRM:
                prCV <- CUs_priors$prCV
                prtaub <- prmub^2/prCV^2
                
                # jags inputs:
                jags.data <- list(
                  nCUs = nCUs,
                  nYrs = nYrs,
                  obs_lnRS = log(R/S),  # data on observed log(recruits/spawners) - matrix nYrs x nCU
                  S = S,         # data on observed spawners - matrix nYrs x nCU 
                  prmub = prmub, # prior on mu for b
                  prtaub = prtaub # prior on tau for b
                )
                
                ptm <- proc.time()
                
                jags_m_here <- HBSRM_JAGS_fun(model_name = "Ricker",
                                              modelFilename = NA,
                                              jags.data = jags.data, 
                                              jags.parms = jags.parms, 
                                              n.thin = 10,             # to keep every 10th data point
                                              n.burnin = n.burnin,
                                              n.chains = n.chains)
                print("")
                
                convergence <- gelman.diag(jags_m_here$post)$psrf
                cond_deviance <- rownames(convergence) == "deviance"
                convergence <- convergence[!cond_deviance,]
                
                cond_issue <- convergence[,"Point est."] > Rc_max_cut
                para_issue <- rownames(convergence)[cond_issue]
                para_issue <- para_issue[! para_issue %in% c("mu_a","sd_a")]
                
                if(length(para_issue) > 0){
                  print("*** There is still convergence issue!")
                }else{
                  print("*** Convergence Solved!")
                }
              }
            } # End of if there are convergence issues
            
            # endtime <- proc.time() - ptm
            # endtime[3]/60
            
            post <- jags_m_here$post
            
            # 
            file_name <- paste0(wd_output,"/intermediate/",gsub(" ","_",regionName),"_",
                                speciesAcroHere,cyclic_filen,"_HBSRM_posteriors_priorShift.rds")
            saveRDS(post, file = file_name)
            
            ##### INFERENCE #####
            
            # Gelman and Rubin's convergence diagnostic
            # remove the "deviance" column because it is not a model parameter
            for(l in 1:length(post)){
              # l <- 1
              post[[l]] <- post[[l]][,colnames(post[[l]]) != "deviance"] 
            }
            convDiagnostic <- as.data.frame(gelman.diag(post, multivariate = F)$psrf)
            convDiagnostic$parameter <- rownames(convDiagnostic)
            convDiagnostic$cu_name_pse <- NA
            convDiagnostic$cuid <- NA
            convDiagnostic$region <- NA
            convDiagnostic$species_name <- NA
            convDiagnostic$species_acro <- NA
            convDiagnostic$nb <- NA           # the number of the CU for the parameters (e.g. 3 for b[3])
            convDiagnostic$prSmax <- convDiagnostic$prCV <- NA
            
            speciesAcroHere_file <- speciesAcroHere
            for(r in 1:nrow(convDiagnostic)){
              # r <- 2
              nbHere <- rownames(convDiagnostic)[r]
              nbHere <- sub(".*\\[", "", nbHere) # remove everything before "[" including "["
              nbHere <- sub("\\].*", "", nbHere) #
              
              if(!nbHere %in% c("mu_a",'sd_a')){
                if(length(CUs) == 1){  # there is not number associated with the parameter, e.g. "a" and not "a[1]"
                  nbHere <- 1
                }else{
                  nbHere <- as.numeric(nbHere)
                }
                CUHere <- names(CUs_cuid)[nbHere]
                cuidHere <- CUs_cuid[nbHere]
                prSmaxHere <- round(prSmax[nbHere],2)
                prCVHere <- prCV[nbHere]
                sp_nameHere <- conservationunits_decoder$species_name[conservationunits_decoder$cuid == CUs_cuid[nbHere]]
                
              }else{
                nbHere <- sp_nameHere <- CUHere <- cuidHere <- prSmaxHere <- prCVHere <- ""
              }
              
              # TEMPORARY
              # simplify species_name as in PSE data meeting December 11 2024
              # To remove eventually when conservationunits_decoder$species_name 
              # changed as well.
              if(grepl("[s|S]ockeye",sp_nameHere)){
                
                if(grepl("Lake",sp_nameHere)){
                  speciesAcroHere <- "SEL"
                }else if(grepl("River",sp_nameHere)){
                  speciesAcroHere <- "SER"
                }
                sp_nameHere <- "Sockeye"
                
              }else if(grepl("Pink",sp_nameHere)){
                
                if(grepl("odd",sp_nameHere)){
                  speciesAcroHere <- "PKO"
                }else if(grepl("even",sp_nameHere)){
                  speciesAcroHere <- "PKE"
                }
                sp_nameHere <- "Pink"
                
              }
              
              convDiagnostic$cu_name_pse[r] <- CUHere
              convDiagnostic$cuid[r] <- cuidHere
              convDiagnostic$region[r] <- region[i_rg]
              convDiagnostic$species_name[r] <- sp_nameHere
              convDiagnostic$species_qualified[r] <- speciesAcroHere
              convDiagnostic$nb[r] <- nbHere
              convDiagnostic$prSmax[r] <- prSmaxHere
              convDiagnostic$prCV[r] <- prCVHere
            }
            
            rownames(convDiagnostic) <- NULL
            
            # order rows:
            for(char in c("a\\[","b\\[","sd\\[")){
              # char <- "a\\["
              dfToSort <- convDiagnostic[grepl(char,convDiagnostic$parameter) & !is.na(convDiagnostic$cu_name_pse),]
              dfToSort$nb <- as.numeric(dfToSort$nb)
              dfToSort <- dfToSort[order(dfToSort$nb),]
              convDiagnostic[grepl(char,convDiagnostic$parameter) & !is.na(convDiagnostic$cu_name_pse),] <- dfToSort
            }
            
            convDiagnostic <- convDiagnostic[,c("region","species_name","species_qualified","cuid","cu_name_pse",
                                                "parameter","Point est.","Upper C.I.",
                                                "prSmax","prCV")]
            
            file_name <- paste0(wd_output,"/intermediate/",gsub(" ","_",regionName),"_",
                                speciesAcroHere_file,cyclic_filen,"_HBSRM_convDiagnostic.csv")
            
            write.csv(convDiagnostic,file = file_name,row.names = F)
            
            # post_m <- as.matrix(post, chain = F)
            # post_m <- post_m[,colnames(post_m) != "deviance"] 
            # model.probs <- round(cbind(est = colMeans(post_m),
            #                            sd = apply(post_m,2,sd),
            #                            ci = t(apply(post_m,2,quantile,c(.025,.975)))),
            #                      digits = 8)
            
          } # if there are CU in list(CU_cyclicNO,CU_cyclic)[[i_CUs]]
        } # end of process for non-cyclic and cyclic CUs
      } # end of if there is data for this species in this region
    } # species loop
  }  # if there is data for this region
} # region loop


