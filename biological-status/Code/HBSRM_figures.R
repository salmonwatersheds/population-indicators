


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
wd_Code <- paste0(getwd(),"/Code")
wd_Data <- paste0(getwd(),"/Data")
wd_Figures <- paste0(getwd(),"/Figures")
wd_Output <- paste0(getwd(),"/Output")

# BSC: this below need to be automatized, which is tricky because our names are 
# placed in the dropbox path that leads to the datasets...
wd_Data_input_root <- "C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS"

# Paths to the repositories containing the run reconstruction datasets for each 
# region.
wd_data_regions <- wd_data_regions_fun(wd_root = wd_Data_input_root)

# Import species names and acronyms
species_acronym <- species_acronym_fun()

# Import region names
regions <- regions_fun()

#------------------------------------------------------------------------------#
# User choices
#------------------------------------------------------------------------------#

# option to export the figures
print_fig <- F

# Choosing the region
# BSC: This will have to eventually be automatized and eventually allows for 
# multiple regions to be passed on.
region <- regions$Fraser
region <- regions$Yukon
region <- regions$Nass

# set the path of the input data sets for that specific region
wd_Data_input <- paste0(wd_data_regions[,region])   # BSC: if we end up having the posterior_priorShift.rds file in dropbox
wd_Data_input <- wd_Output                          # if they are there

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

# BSC: TODO: make a function that check the species for which there is a posterior_priorShift.rds
# file.


for(i_sp in 1:length(Species)){
  
  # i_sp <- 1

  # Import the HBSRM outputs:
  post <- readRDS(paste0(wd_Data_input,"/",region,"_",Species[i_sp],"_posteriors_priorShift.rds"))
  
  # find the nb of CUs
  nCUs <- sum(grepl(pattern = "a\\[", x = colnames(post[[1]])))
  
  #------------------------------------------------------------------------------#
  # Check convergence of chains
  #------------------------------------------------------------------------------#
  
  nchains <- length(post) # 6 chains
  # parameter names
  pnames <- colnames(post[[1]])
  # 
  r.hat <- gelman.diag(x = post, multivariate = F)
  if(sum(r.hat[[1]][, 1] > 1.1) > 0){
    warning(paste("Some convergence issues for parameters: \n", pnames[which(r.hat[[1]][, 1] > 1.1)]))
  }
  
  #------------------------------------------------------------------------------#
  # Calculate benchmarks
  #------------------------------------------------------------------------------#
  
  # Unlist different chains of the posterior
  post.arr <- array(
    data = NA, 
    dim = c(length(post), nrow(post[[1]]), ncol(post[[1]])), 
    dimnames = list(paste0("chain", 1:length(post)), NULL, pnames))
  
  for(i in 1:length(post)){
    post.arr[i, , ] <- post[[i]]
  }
  
  # Calculate benchmarks for all mcmc draws to account for correlation between a and b
  SR_bench <- array(
    data = NA,
    dim = c(nCUs, 5, length(post), nrow(post[[1]])),
    dimnames = list(keepCU, c("a", "b", "sig", "Smsy", "Sgen"), 
                    paste0("chain", 1:length(post)), NULL))
  
  for(i in 1:nCU){
    SR_bench[i, "a", , ] <- post.arr[, , which(pnames == paste0("a[", i, "]"))]
    SR_bench[i, "b", , ] <- post.arr[, , which(pnames == paste0("b[", i, "]"))]
    SR_bench[i, "sig", , ] <- post.arr[, , which(pnames == paste0("sd[", i, "]"))]
  }
  
  # Calculate Smsy & Sgen (this takes a few mins...think of vectorizing/parallelizing)
  # Uses 1_functions.R which is different from previous versions by estimating Smsy
  # directly using the methods of Scheuerell (2016).
  
  for(i in 1:nCU){
    for(j in 1:length(post)){
      
      # Smsy (function can handle vectors)
      SR_bench[i, "Smsy", j, ] <- calcSmsy(a = SR_bench[i, "a", j, ], b = SR_bench[i, "b", j, ])
      
      # Sgen (function not currently set up to handle vectors..think of updating this)
      for(k in 1:nrow(post[[1]])){
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
  

  
  # maximum nb of CUs
  # MaxStocks <- scan(file = fndata[i],nlines = 1,skip = 1)
  
  # for each CU..
  CUs <- unique(d$CU)
  for(i_cu in 1:length(CUs)){
    
    # i_cu <- 1
    CU <-CUs[i_cu]
    d_sub <- subset(x = d, subset = CU == CU)
    
    # alpha <- sx.post[,16]; beta <- sx.post[,65] # ?! what are those big numbers in there?
    alpha <- post[,i_cu]
    beta <- post[,i_cu + length(unique(d$CU))]
    
    # 
    spw <- seq(0,max(d_sub$Esc, na.rm = T),100)
    
    # 
    recruits <- array(NA, dim = c(1,length(spw), 10000))
    
    # 
    for(j in 1:10000){
      iter <- sample(length(alpha),1)
      recruits[,,j] <- spw * exp(alpha[iter] - beta[iter] * spw) # Ricker model
    }
    
    # 
    rec.m <- apply(recruits,c(1,2),median)/1000
    rec.u <- apply(recruits,c(1,2),quantile,probs = 0.975)/1000
    rec.l <- apply(recruits,c(1,2),quantile,probs = 0.025)/1000
    
    if(print_fig){
      # jpeg("LongSR.jpg", width=8, height=4,units="in",res=200)
      # BSC: not sure if we want to export the figures in the github repo.
      jpeg(paste0(wd_Figures,"/",region,"_",Species[i],"_",unique(d$CU)[i_cu],".jpg"), 
           width = 8, height = 4,units = "in",res = 200)
    }
    
    par(mfrow = c(1,1),mar = c(1.5,2.5,1,1.5),oma = c(2,2,2,1), new = F)
    plot(x = d_sub$Esc / 1000,y = d_sub$Rec/1000,
         bty='l', xlab="", ylab="", xaxt="n", yaxt="n", col = "white")
    axis(1)
    axis(2,las = 2)
    # mtext("Sockeye - Long",3,cex=0.85,line=1)
    species_full <- colnames(species_acronym)[which(species_acronym == Species[i])]
    mtext(paste0(species_full," - ",CU),3,cex=0.85,line=1)
    mtext("Spawners (000s)",1,outer=T,line=1)
    mtext("Recruits (000s)",2,outer=T,line=1)
    
    
    # .95 CI
    polygon(x = c(spw/1000,rev(spw/1000)),y = c(rec.u,rev(rec.l)),
            col = grey(0.9),border = NA)
    
    # data points 
    points(x = d_sub$Esc / 1000,y = d_sub$Rec/1000)
    
    # median line
    lines(x = spw/1000, y = rec.m, lwd = 2)
    
    # adjustcolor(grey(0.9), alpha.f = 0.9)
    
    
    # Bench marks
    abline(v =41.4,col="red",lwd=2)
    abline(v =29.8,col="red",lty=2)
    abline(v =76.7,col="red",lty=2)
    
    abline(v=82.8,col="dark green",lwd=2)
    abline(v=59.7,col="dark green",lty=2)
    abline(v=153.4,col="dark green",lty=2)
    
    
    
    if(print_fig){
      dev.off()
    }
    
  }
  
  
  # obtain the CUs names:
  unique(sp.rs$CU)
  
  
  # sockeye
  #long
  Long.cu <- subset(sx.rs, CU=="Long")
  
  #owikeno
  Owikeno.cu <- subset(sx.rs, CU=="Owikeno")
  
  #backland
  Backland.cu <- subset(sx.rs, CU=="Backland")
  
  #backland
  Canoona.cu
  
  
}






