


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
wd_Data_input <- paste0(wd_data_regions[,region])

# Set species and constraints on analysis (first brood year and min # of SR data points)
# BSC: possibility to select one or more species.
# Option to set Species to NULL; in that case all script looks inside the repository
# and import the files present for the species.
# If we specify the species:
Species <- c(species_acronym$Sockeye,    
             species_acronym$Pink,
             species_acronym$Cutthroat,
             species_acronym$Chum)

# If we do not specify the species:
Species <- NULL

# Returns a list with the species and the corresponding path of the _SRdata files
# (the most up to date)
fndata <- SRdata_path_Species_fun(wd = wd_Data_input, Species = Species)

Species <- fndata$Species  # species is updated is was NULL or certain species do not have a file
fndata <- fndata$SRdata

# Set first brood year, "-99" for no constraint
FBYr <- -99

# Set minimum # of SR data points required to be included in the analysis
MinSRpts <- 3 

#----------------------------------------------------------------------------#
# Read in Stock-Recruit Data, run the HBSR model and output parameter estimates
#----------------------------------------------------------------------------#

for(i in 1:length(Species)){
  #------------------------------------------------------------------------------#
  # Plot SR relationship ----
  #------------------------------------------------------------------------------#
  
  # BSC: it is possible to group this above code with the one below in the same loop.
  # Now it is grouped.
  # Note that keeping everything in one loop is simpler because we filter the 
  # _SPregion <- regions$Fraser...csv datasets and eventually updated the names 
  # in CUs. How come this is not done again here by the way?
  
  # for(i in 1:length(Species)){
  
  # i <- 1
  # Import the ...
  # could also simply use mypost
  post <- read.table(file = paste0(wd_Output,"/",region,".",Species[i],".post.out"),
                     header = T)
  
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






