
# 
rm(list = ls())
graphics.off()

# Set directory to /biological-status
if(!grepl(pattern = "biological-status", x = getwd())){
  setwd(dir = paste0(getwd(),"/biological-status"))
}

# Import functions
source("Code/functions.R")

# Load packages
library(R2jags)  # Provides wrapper functions to implement Bayesian analysis in JAGS.
library(modeest) # Provides estimators of the mode of univariate data or univariate distributions.

# Define subdirectories:
wd_code <- paste0(getwd(),"/code")
wd_data <- paste0(getwd(),"/data")

# The pass ../Salmon Watersheds Dropbox/user_name/X Drive/1_PROJECTS.
# The pass is personal and must be copy past in wd_X_Drive1_PROJECTS.txt
# e.g.: "C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS"
wd_X_Drive1_PROJECTS <- readLines( "wd_X_Drive1_PROJECTS.txt")

# Define subdirectories:
wd_code <- paste0(getwd(),"/code")
wd_data <- paste0(getwd(),"/data")

# figures and datasets generated are 
Export_locally <- T
if(Export_locally){
  wd_figures <- paste0(wd_X_Drive1_PROJECTS,"/figures")
  wd_output <- paste0(getwd(),"/output")
}else{
  wd_biological_status <- "Population Methods and Analysis/population-indicators/biological-status"
  wd_figures <- paste0(wd_X_Drive1_PROJECTS,"/",wd_biological_status,"/figures")
  wd_output <- paste0(wd_X_Drive1_PROJECTS,"/",wd_biological_status,"/output")
}

# Paths to the repositories containing the run reconstruction datasets for each 
# region.
wd_data_regions <- wd_data_regions_fun(wd_root = wd_X_Drive1_PROJECTS)

# Import species names and acronyms
species_acronym <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#------------------------------------------------------------------------------#
# Selection of region(s) and species
#------------------------------------------------------------------------------#

# option to export the figures
print_fig <- F

# Choosing the region
# BSC: This will have to eventually be automatized and eventually allows for 
# multiple regions to be passed on.
region <- regions_df$Fraser
region <- regions_df$Yukon
region <- regions_df$Nass

# multiple regions:
region <- c(
  regions_df$Fraser,
  regions_df$Yukon,
  regions_df$Nass)

region <- c(
  regions_df$Central_coast,
  regions_df$Haida_Gwaii,
  regions_df$Skeena)

# set the path of the input data sets for that specific region
# wd_data_input <- paste0(wd_data_regions[,region])   # BSC: if we end up having the posterior_priorShift.rds file in dropbox
wd_data_input <- wd_output                          # if they are there

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set Species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
Species <- c(
             species_acronym$Sockeye,    
             species_acronym$Pink,
             species_acronym$Coho
             #species_acronym$Cutthroat,
             #species_acronym$Chum
             )

# If we do not specify the species: all the species that have a _SRdata files are 
# returned: 
# note that Species_all take precedence over Species in SRdata_path_Species_fun()
Species_all <- TRUE


for(i_rg in 1:length(region)){
  
  # i_rg <- 1
  
  if(Species_all){
    files_list <- list.files(wd_data_input)
    files_s <- files_list[grepl(pattern = "_posteriors_priorShift",files_list)]
    files_s <- files_s[grepl(pattern = region[i_rg],files_s)]
    Species <- unique(sub("_posteriors_priorShift.*", "", files_s))
    Species <- gsub(pattern = paste0(region[i_rg],"_"), replacement = "", x = Species)
  }
  
  # BSC: if case Species is not NULL, write code to remove species for which there
  # is not dataset for that specific region
  
  for(i_sp in 1:length(Species)){
    
    # i_sp <- 1
    
    # Import the HBSRM outputs, i.e., the posterior distribtions of:
    # - mu_a and sigma_a: with CU-level instrinsic productivity ai ~ N(mu_a,sigma_a)
    # - with bi the CU-level density dependance parameter bi ~ logN(log(1/Smaxi),sigma_bi), with Smaxi being the max(S) of that CU i
    # in the datasets "ma_a" = "ma_a", "sigma_a" = "sd_a", "sigma_bi" = "sd[i]"
    post <- readRDS(paste0(wd_data_input,"/",region[i_rg],"_",Species[i_sp],"_posteriors_priorShift.rds"))
    
    # find the nb of CUs
    CUs <- read.csv(paste0(wd_data_input,"/",region[i_rg],"_",Species[i_sp],"_CUs_names.csv"),
                    header = T,stringsAsFactors = F)
    CUs <- CUs$CU
    nCUs <-length(CUs)
    
    nchains <- length(post) # 6 chains
    # parameter names
    pnames <- colnames(post[[1]])
    
    #-----------------------------------------------------------------------------#
    # Check convergence of chains
    #-----------------------------------------------------------------------------#
    
    # The Gelman-Rubin metric : R_hat ; if R_hat < 1.1 all is good
    r.hat <- gelman.diag(x = post, multivariate = F)
    if(sum(r.hat[[1]][, 1] > 1.1, na.rm = T) > 0){
      warning(paste("Some convergence issues for parameters: \n", 
                    paste0(region[i_rg]," - ",Species[i_sp]," - ",pnames[which(r.hat[[1]][, 1] > 1.1)])))
      print(r.hat)
    }
    if(sum(is.na(r.hat[[1]][, 1])) > 0){  # BSC: I added that for Fraser PK 
      warning(paste("Some convergence issues for parameters: \n", 
                    paste0(region[i_rg]," - ",Species[i_sp]," - ",pnames[which(is.na(r.hat[[1]][, 1]))])))
      print(r.hat)
    }
    
    #-----------------------------------------------------------------------------#
    # Calculate benchmarks
    #-----------------------------------------------------------------------------#
    
    # Unlist different chains of the posterior
    # 6 chains x nb iteration (?) x nb parameters
    post.arr <- array(
      data = NA, 
      dim = c(nchains, nrow(post[[1]]), ncol(post[[1]])), 
      dimnames = list(paste0("chain", 1:length(post)), NULL, pnames))
    
    for(i in 1:length(post)){  # for each chain
      post.arr[i, , ] <- post[[i]]
    }
    
    # Calculate benchmarks for all mcmc draws to account for correlation between a and b
    # nb CUs x nb different parameters (i.e., 5) x nb chains x nb iterations
    SR_bench <- array(
      data = NA,
      dim = c(nCUs, 5, length(post), nrow(post[[1]])),
      dimnames = list(CUs, 
                      c("a", "b", "sig", "Smsy", "Sgen"), 
                      paste0("chain", 1:length(post)), 
                      NULL))
    
    for(i in 1:nCUs){
      if(nCUs == 1){
        SR_bench[i, "a", , ] <- post.arr[, , which(pnames == "a")]      # matrix nb chains x nb mcmc draws --> all the values for that parameter
        SR_bench[i, "b", , ] <- post.arr[, , which(pnames == "b")]
        SR_bench[i, "sig", , ] <- post.arr[, , which(pnames == "sd")]   # sigma_bi
      }else{
        SR_bench[i, "a", , ] <- post.arr[, , which(pnames == paste0("a[", i, "]"))]
        SR_bench[i, "b", , ] <- post.arr[, , which(pnames == paste0("b[", i, "]"))]
        SR_bench[i, "sig", , ] <- post.arr[, , which(pnames == paste0("sd[", i, "]"))]
      }
    }
    
    # Calculate Smsy & Sgen (this takes a few mins...think of vectorizing/parallelizing)
    # Uses 1_functions.R which is different from previous versions by estimating Smsy
    # directly using the methods of Scheuerell (2016).
    
    for(i in 1:nCUs){
      # i <- 1
      # i <- 3   # issue with Sgen in Fraser CO CU nb 3
      for(j in 1:length(post)){ # for each chain
        # j <- 1
        # Smsy (function can handle vectors)
        SR_bench[i, "Smsy", j, ] <- calcSmsy(a = SR_bench[i, "a", j, ], 
                                             b = SR_bench[i, "b", j, ])
        
        # Sgen (function not currently set up to handle vectors..think of updating this)
        for(k in 1:nrow(post[[1]])){   # for each mcmc draw
          # k <- 1
          SR_bench[i, "Sgen", j, k] <- calcSgen(
            Sgen.hat = 0.5 * SR_bench[i, "Smsy", j, k], 
            theta = c(
              a = SR_bench[i, "a", j, k], 
              b = SR_bench[i, "b", j, k],
              sig = SR_bench[i, "sig", j, k]),
            Smsy = SR_bench[i, "Smsy", j, k])
        }
      }
    }
    
    # median(SR_bench[3,"Sgen",,],na.rm = T)
    
    #------------------------------------------------------------------------------#
    # Plot the benchmark posteriors
    #------------------------------------------------------------------------------#
    
    # library(MCMCglmm) # For poster.mode function
    
    # Import the S and R matrices used for fitting the HBSR model:
    # BSC: the wd here will eventually have to be set to the final repo for the 
    # exported datasets.
    SRm <- readRDS(paste0(wd_data_input,"/",region[i_rg],"_",Species[i_sp],"_SR_matrices.rds"))
    
    # Compare median/quantiles (medQuan) and HPD/HPDI (HPD)
    # if(print_fig){
    #   pathFile <- paste0(wd_figures,"/",region,"_",Species[i_sp],"_benchmark_posteriors.pdf")
    #   pdf(file = pathFile, width = 8.5, height = 11)
    # }
    
    statusCols <- c(g = "#8EB687", a = "#DFD98D", r = "#9A3F3F")  # BSC: those are not colour blind friendly
    # par(mfrow = c(3,2), mar = c(4, 4, 5, 1), oma = c(3,3,1,0))
    # layout(matrix(data = 1:(nCUs * 2), nrow = nCUs, byrow = T))
    for(i in 1:nCUs){
      
      # i <- 1
      
      if(print_fig){
        CUhere <- gsub(pattern = "/",'-',CUs[i])  # in case "/" is in the CU's name
        pathFile <- paste0(wd_figures,"/",region[i_rg],"_",Species[i_sp],"_",CUhere,
                           "_benchmark_posteriors.jpeg")
        
        # pdf(file = pathFile, width = 8.5, height = 11)
        # pdf(file = pathFile, width = 15, height = 7.5)
        jpeg(file = pathFile, width = 30, height = 14, units = "cm", res = 300)
      }
      
      layout(matrix(data = 1:2, nrow = 1))
      par(mar = c(4.5,4.5,4,2))
      
      # max spawners for that CU for plotting purposes
      maxS <- max(SRm$S[, i], na.rm = TRUE) * 1.5
      
      #----------------------
      # Plot SR relationship
      #----------------------
      # Create dummy vector of spawner abundance
      dummy_spawners <- seq(0, maxS, length.out = 200)
      
      # Calculate posterior prediction for recruits based on a and b
      dummy_recruits_full <- array(NA, dim = c(200, nchains*nrow(post[[1]])))
      for(j in 1:200){
        # R = S*exp(a - b*S)
        dummy_recruits_full[j, ] <- dummy_spawners[j] * exp(c(SR_bench[i, "a", , ]) - 
                                                              c(c(SR_bench[i, "b", , ])) * 
                                                              dummy_spawners[j])
      }
      
      # Summarize predicted recruits using original method and HPD for comparison
      dummy_recruits <- list(
        medQuan = apply(dummy_recruits_full, 1, medQuan),
        HPD = apply(dummy_recruits_full, 1, HPD, xmax = maxS)
      )
      
      # Plot SR relationship with predicted recruits and 95% CI
      plot(x = dummy_spawners, y = dummy_recruits[[1]][1,], type = "n", 
           xlim = c(0, maxS/1.5), ylim = c(0, max(SRm$R[, i], na.rm = TRUE)), 
           xlab = "Spawners", ylab = "Recruits", bty = "l")
      mtext(side = 3, adj = 0, line = 2.5, CUs[i])
      points(SRm$S[, i], SRm$R[, i])
      for(j in 1:2){
        lines(dummy_spawners, dummy_recruits[[j]][1, ], lty = c(2,1)[j], lwd = 2)
        lines(dummy_spawners, dummy_recruits[[j]][2, ], lty = c(2,1)[j])
        lines(dummy_spawners, dummy_recruits[[j]][3, ], lty = c(2,1)[j])
      }
      
      #----------------------
      # Plot benchmarks
      #----------------------
      benchSummary <- list(
        Sgen = rbind(
          medQuan = medQuan(SR_bench[i, "Sgen", , ]),
          HPD = HPD(SR_bench[i, "Sgen", , ])),
        Smsy = rbind(
          medQuan = medQuan(SR_bench[i, "Smsy", , ]),
          HPD = HPD(SR_bench[i, "Smsy", , ]))
      )
      
      u <- par('usr')
      for(j in 1:2){    # median, CI
        for(k in 1:2){  # Sgen, Smsy
          abline(v = benchSummary[[k]][j, 1], lty = c(2,1)[j], 
                 col = statusCols[c('r', 'g')[k]])
          # abline(v = benchSummary[[k]][j, 2], lty = c(2,1)[j], col = statusCols[c('r', 'g')[k]])
          # abline(v = benchSummary[[k]][j, 3], lty = c(2,1)[j], col = statusCols[c('r', 'g')[k]])
          y <- u[4] + c(c(0.05, 0.02)[k] + c(0, 0.05)[j])*(u[4]- u[3])
          points(x = benchSummary[[k]][j, 1],y = y, 
                 col = statusCols[c('r', 'g')[k]], pch = 19, xpd = NA)
          segments(x0 = benchSummary[[k]][j, 2], x1 = benchSummary[[k]][j, 3], 
                   y0 = y, y1 = y, 
                   col = statusCols[c('r', 'g')[k]], 
                   lty = c(2,1)[j], lwd = 2, xpd = NA)
        }}
      
      legend("topright", lty = c(2,1, NA, NA), pch = c(NA, NA, 19, 19), 
             col = c(1, 1, statusCols['r'], statusCols['g']), 
             legend = c("median and quantiles", "HPD", "Sgen", "Smsy"), bg = "white")
      
      
      # Histograms of posterior mcmc draws:
      Sgen <- SR_bench[i, "Sgen", , ]
      Sgen <- Sgen[which(Sgen <= maxS)]
      
      Smsy <- SR_bench[i, "Smsy", , ]
      Smsy <- Smsy[which(Smsy <= maxS)]
      
      dens <- list(density(SR_bench[i, "Sgen", , ], from = 0, to = maxS/1.5, na.rm = T), # BSC: I had to add na.rm = T
                   density(SR_bench[i, "Smsy", , ], from = 0, to = maxS/1.5, na.rm = T))
      
      
      h1 <- hist(x = Sgen, col = paste0(statusCols['r'], 50), border = NA,
                 breaks = seq(0, maxS, maxS/50), xlim = c(0, maxS), main = "", 
                 freq = FALSE, xlab = "Benchmarks")
      h2 <- hist(x = Smsy, col = paste0(statusCols['g'], 50), border = NA, 
                 breaks = seq(0, maxS, maxS/50), add = TRUE, 
                 freq = FALSE)
      
      # Plot density lines and benchmark estimates
      u <- par('usr')
      for(k in 1:2){
        lines(dens[[k]], lwd = 2, col = statusCols[c('r', 'g')[k]])
        for(j in 1:2){
          abline(v = benchSummary[[k]][j, 1], col = statusCols[c('r', 'g')[k]], 
                 lty = c(2,1)[j])
          # abline(v = benchSummary[[k]][j, 2:3], col = statusCols[c('r', 'g')[k]], lty = c(2,1)[j])
          
          y <- u[4] + c(c(0.05, 0.02)[k] + c(0, 0.05)[j])*(u[4]- u[3])
          points(x = benchSummary[[k]][j, 1],y = y, col = statusCols[c('r', 'g')[k]],
                 pch = 19, xpd = NA)
          segments(x0 = benchSummary[[k]][j, 2], x1 = benchSummary[[k]][j, 3], 
                   y0 = y, y1 = y, 
                   col = statusCols[c('r', 'g')[k]], lty = c(2,1)[j], lwd = 2, xpd = NA)
        }
      }
      
      if(print_fig){
        dev.off()
      }
      # Add spawner points along bottom for reference
      #points(x = d$Esc[d$CU == keepCU[i]], y = rep(0, length(which(d$CU == keepCU[i]))))
    }
    # # Add legend
    # plot(1,1,"n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    # legend("top", fill = c(statusCols['r'], statusCols['g']), legend = c("Sgen", "Smsy"), bty = "n", border = NA, cex = 1.5)
    # legend("center", lty = c(2,1), title = "Posterior summary", legend = c("median", "HPD"), bty = "n", border = NA, cex = 1.5)
  }
}








