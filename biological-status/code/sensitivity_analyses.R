
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

#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

#' Import the cuspawnerabundance.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' To calculating current spawner abundance for biostatus assessment
fromDatabase <- F
update_file_csv <- F

cuspawnerabundance <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[2],
                                            fromDatabase = fromDatabase,
                                            update_file_csv = update_file_csv,
                                            wd = wd_pop_indic_data_input_dropbox)

#' Import the conservationunits_decoder.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

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

#
# Sensitivity concerning the HS percentile benchmark approach ------
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
# 3yr  5yr 10yr 15yr 20yr 
# 0.0  0.0  0.0  2.7  7.2 

sapply(X = yr_nb, FUN = function(yr){
  # yr <- yr_nb[4]
  nb_cu_dataPt_df_cut <- nb_cu_dataPt_df[nb_cu_dataPt_df$nb_dataPt >= yr,]
  out <- nrow(nb_cu_dataPt_df) - nrow(nb_cu_dataPt_df_cut)
  names(out) <- paste0(yr,"yr")
  return(out)
})
# 3yr  5yr 10yr 15yr 20yr 
# 0    0    0    6   16 

count <- c()
for(i in 1:max(nb_cu_dataPt_df$nb_dataPt)){
  count <- c(count,nrow(nb_cu_dataPt_df[nb_cu_dataPt_df$nb_dataPt > i,]))
}
names(count) <- 1:max(nb_cu_dataPt_df$nb_dataPt)
colours <- rep("grey60",length(count))
colours[as.numeric(names(count)) %in% c(10,15,20)] <- "red"

printFig <- T
if(printFig){
  jpeg(paste0(wd_figures,"/Sensitivity_dataPointNb_cut.jpg"),
       width = 30, height = 20, units = "cm", res = 300)
}
par(mar=c(5,5,0.5,0.5))
barplot(count, ylab = " Number of CUs", xlab = "minimum number of data points allowed",
        axis.lty=1, col = colours)
if(printFig){
  dev.off()
}

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

printFig <- F
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
                                        printFig = printFig,
                                        xvar = "dataPointNb")
}



#
# Sensitivity: a ~ N(mu_a, tau_a) vs. a ~ LN(log(mu_a), tau_a) ------
#' Comparison of HBSR model parameter, benchmarks and biostatus between the two
#' options to model the distribution of 'a' (the growth of the population at small 
#' values for S --> a ~ log(R/S)). There was a debate to know which distribution
#' to use. 
#' Results for each model version:
#' - a ~ LN(log(mu_a), tau_a) : /output
#' - a ~ N(mu_a, tau_a) : /output_NORMAL_DIST

#' 1) Collect the posterio distribution of the model parameters generated in 
#' HBSRM.R:

wd_outputs <- c(wd_output, 
                gsub("/output","/output_NORMAL_DIST",wd_output))
postDist_l <- list(NULL,NULL)
names(postDist_l) <- c("output","output_NORMAL_DIST")

for(wdo in wd_outputs){
  # wdo <- wd_outputs[1]
  wd_data_input <- wdo
  
  for(i_rg in 1:length(region)){
    
    # i_rg <- 3
    
    region_i <- gsub("_"," ",region[i_rg])
    if(region_i == "Central coast"){
      region_i <- "Central Coast"
    }
    
    if(region[i_rg] == "Vancouver Island & Mainland Inlets"){
      regionName <- "VIMI"
    }else{
      regionName <- regionName <- gsub(" ","_",region[i_rg])
    }
    
    # return species for which data is available in that region
    files_list <- list.files(wd_data_input)
    files_s <- files_list[grepl(pattern = "_posteriors_priorShift",files_list)]
    files_s <- files_s[grepl(pattern = regionName,files_s)]
    species <- unique(sub("_HBSRM_posteriors_priorShift.*", "", files_s))
    species <- gsub(pattern = paste0(regionName,"_"), replacement = "", x = species)
    
    if(length(species) == 0){
      
      print(paste0("*** There is no data in region ",region[i_rg]," ***"))
      
    }else{
      
      for(i_sp in 1:length(species)){
        
        # i_sp <- 1
        
        speciesHere <- species_acronym_df$species_name[species_acronym_df$species_acro == species[i_sp]]
        
        cuspawnerabundance_rg_sp <- cuspawnerabundance[cuspawnerabundance$region == region_i &
                                                         cuspawnerabundance$species_name %in% speciesHere,]
        
        conservationunits_decoder_rg_sp <- conservationunits_decoder[conservationunits_decoder$region == region_i &
                                                                       conservationunits_decoder$species_name%in% speciesHere,]
        
        # Import the HBSRM outputs, i.e., the posterior distributions of:
        post <- readRDS(paste0(wd_data_input,"/",regionName,"_",species[i_sp],"_HBSRM_posteriors_priorShift.rds"))
        
        # Import the S and R matrices used for fitting the HBSR model:
        SRm <- readRDS(paste0(wd_data_input,"/",regionName,"_",species[i_sp],"_SR_matrices.rds"))
        
        # Find the nb of CUs
        CUs <- colnames(SRm$R)
        nCUs <-length(CUs)
        
        # find the corresponding cuid 
        cuids <- sapply(X = CUs, function(cu){
          # cu <- CUs[6]
          regionHere <- region_i
          conservationunits_decoder_cut <- conservationunits_decoder[conservationunits_decoder$region == regionHere &
                                                                       conservationunits_decoder$species_name %in% speciesHere & 
                                                                       conservationunits_decoder$cu_name_pse == cu,]
          
          # we could probably simply use pooledcuid
          if(nrow(conservationunits_decoder_cut) == 1){
            out <- conservationunits_decoder_cut$cuid
          }else{
            out <- unique(conservationunits_decoder_cut$pooledcuid)
          }
          
          if(length(out) == 0){
            print(paste("There is no cuid for:",region[i_rg],"-",speciesHere,"-",cu))
          }
          return(out)
        })
        
        nchains <- length(post) # 6 chains
        # parameter names
        pnames <- colnames(post[[1]])
        
        # 
        post_df <- do.call(rbind,post)
        post_df <- post_df[,colnames(post_df) != "deviance"]
        
        output <- data.frame(region = region_i,
                             species = speciesHere[1],
                             species_acro = species[i_sp],
                             cu_name_pse = CUs,
                             cuid = cuids)
        
        rownames(output) <- NULL
        for(p in c("a","b","mu_a","sd_a","sd")){
          # p <- "mu_a" p <- "a" p <- "b"
          if(p %in% c("mu_a","sd_a")){
            var_m <- median(post_df[,p], na.rm = T)
            var_CI <- quantile(x = post_df[,p], probs = c(.025,.975),na.rm = T)
            para_df <- data.frame(cu_name_pse = CUs,
                                  m = var_m, 
                                  CI025 = var_CI[1],
                                  CI975 = var_CI[2])
            colnames(para_df)[2:ncol(para_df)] <- paste0(p,"_",colnames(para_df)[2:ncol(para_df)])
            
          }else{ # need to return CU-specific values
            para_df <- lapply(X = CUs,FUN = function(cu){
              # cu <- CUs[1]
              if(length(CUs) == 1){
                var_name <- p
              }else{
                count <- which(cu == CUs)
                var_name <- paste0(p,"[",count,"]")
              }
              var_m <- median(post_df[,var_name], na.rm = T)
              var_CI <- quantile(x = post_df[,var_name], probs = c(.025,.975),na.rm = T)
              out <- data.frame(cu_name_pse = cu,
                                m = var_m, 
                                CI025 = var_CI[1],
                                CI975 = var_CI[2])
              colnames(out)[2:ncol(out)] <- paste0(p,"_",colnames(out)[2:ncol(out)])
              rownames(out) <- NULL
              return(out)
            })
            para_df <- do.call(rbind,para_df)
          }
          row.names(para_df) <- NULL
          output <- merge(x = output, y = para_df, by = "cu_name_pse")
        }
        output <- output[,c("region","species","species_acro","cuid","cu_name_pse",
                            colnames(output)[6:ncol(output)])]
        
        if(is.null(postDist_l[[which(wdo == wd_outputs)]])){
          postDist_l[[which(wdo == wd_outputs)]] <- output
        }else{
          postDist_l[[which(wdo == wd_outputs)]] <- rbind(postDist_l[[which(wdo == wd_outputs)]],
                                                          output)
        }
      } # end of for each species
    }
  } # end of for each region
} # end of for each wd_outputs

# ignore the warnings

postDist_l$output

sensitivity_RickerParam_LN_fun(postDist_l = postDist_l)

sensitivity_RickerParam_LN_fun <- function(postDist_l){
  
  dataN <- postDist_l$output_NORMAL_DIST
  dataLN <- postDist_l$output
  
  parameters <- c("mu_a","sd_a","a","b")
  
  for(rg in unique(postDist_l$output$region)){
    # rg <-  unique(postDist_l$output$region)[1]
    condition <- dataN$region == rg
    dataNrg <- dataN[condition,]
    dataLNrg <- dataLN[condition,]
    
    for(sp in unique(dataLNrg$species)){
      # sp <- dataLNrg$species[1]
      condition <- dataNrg$species == sp
      dataNrgsp <- dataNrg[condition,]
      dataLNrgsp <- dataLNrg[condition,]
      
      dataNrgsp <- dataNrgsp[order(dataNrgsp$cuid),]
      dataLNrgsp <- dataLNrgsp[order(dataLNrgsp$cuid),]
      
      cuids <- dataNrgsp$cuid
      ylim <- c(.5,length(cuids) + .5)
      ymid <- ylim[1] + (ylim[2] - ylim[1])/2
      
      layout(matrix(c(rep(1,length(parameters)),2:(length(parameters) + 1)),
                    nrow = 2, byrow = T), 
             widths = c(1.25,rep(1,length(parameters) - 2),1.05), 
             heights = c(.08,1))
      par(mar = rep(0,4))
      plot(NA, ylim = c(0,1), xlim = c(0,1), xaxt = 'n', yaxt = 'n', bty = 'n',
           xlab = '', ylab = '')
      legend("center",paste(rg,sp,sep=' - '), bty = 'n', cex = 2.5)
      for(p in parameters){
        # p <- parameters[]
        # p <- "a"
        count <- which(p == parameters)
        
        if(count == 1){
          side2 <- 5
          side4 <- .5
        }else if(count == length(parameters)){
          side2 <- .5
          side4 <- 2.5
        }else{
          side2 <- .5
          side4 <- .5
        }
        
        N_m <- dataNrgsp[,paste0(p,"_m")]
        LN_m <- dataLNrgsp[,paste0(p,"_m")]
        N_CI025 <- dataNrgsp[,paste0(p,"_CI025")]
        LN_CI025 <- dataLNrgsp[,paste0(p,"_CI025")]
        N_CI975 <- dataNrgsp[,paste0(p,"_CI975")]
        LN_CI975 <- dataLNrgsp[,paste0(p,"_CI975")]
        
        xmin <- min(N_CI025,LN_CI025)
        xmax <- max(N_CI975,LN_CI975)
        
        offset <- 0.1
        
        par(mar = c(4.5,side2,0.5,side4))
        plot(NA, xlim = c(xmin,xmax), ylim = ylim, xlab = p, ylab = "", yaxt = 'n')
        if(count == 1){
          axis(side = 2, at = 1:length(cuids), labels = cuids, las = 1)
          mtext(text = "CUID",side = 2, cex = .8, line = 3)
        }
        if(count == length(parameters)){ # axis 4 for 'N' and 'NL'
          axis(side = 4, at = 1:length(cuids) + offset, labels = rep("N",length(cuids)), 
               las = 1)
          axis(side = 4, at = 1:length(cuids) - offset, labels = rep("LN",length(cuids)), 
               las = 1)
        }
        # mean value over all the points
        segments(x0 = mean(c(N_m,LN_m)), x1 = mean(c(N_m,LN_m)), 
                 y0 = 0, y1 = length(cuids) + 1, lwd = 2, col = "grey60")
        if(p %in% c('a','mu_a')){
          segments(x0 = 0, x1 = 0, y0 = 0, y1 = length(cuids) + 1, lwd = 2, col = "red")
        }
        
        if(p %in% c("a","b")){
          # median of the parameter for each CU and model version
          
          col_N <- rep("black",length(cuids))
          col_LN <- rep("black",length(cuids))
          
          if(p == 'a'){
            col_N[N_m < 0 | N_CI025 < 0] <- "red"
            col_LN[LN_m < 0 | LN_CI025 < 0] <- "red"
          }
          points(x = N_m, y = 1:length(cuids) + offset, pch = 16, cex = 2, col = col_N)
          points(x = LN_m, y = 1:length(cuids) - offset, pch = 16, cex = 2, col = col_LN)
          segments(x0 = N_CI025, x1 = N_CI975, lwd = 2, col = col_N,
                   y0 = 1:length(cuids) + offset, y1 = 1:length(cuids) + offset)
          segments(x0 = LN_CI025, x1 = LN_CI975, lwd = 2, col = col_LN,
                   y0 = 1:length(cuids) - offset, y1 = 1:length(cuids) - offset)
        }else{
          # median of the parameter for each model version
          if(p == 'mu_a'){
            col_N[N_m < 0 | N_CI025 < 0] <- "red"
            col_LN[LN_m < 0 | LN_CI025 < 0] <- "red"
          }
          points(x = N_m[1], y = ymid + offset, pch = 16, cex = 2, col = col_N)
          points(x = LN_m[1], y = ymid - offset, pch = 16, cex = 2, col = col_LN)
          segments(x0 = N_CI025[1], x1 = N_CI975[1], lwd = 2, col = col_N,
                   y0 = ymid + offset, y1 = ymid + offset)
          segments(x0 = LN_CI025[1], x1 = LN_CI975[1], lwd = 2, col = col_LN,
                   y0 = ymid - offset, y1 = ymid - offset)
        }
      }
    }
  }
}















