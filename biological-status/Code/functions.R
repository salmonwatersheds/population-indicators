
# Function that calculates the lower stock-recruitment benchmark, Sgen (i.e. the 
# spawner abundance that leads to Smsy in one generation with no harvest and 
# assuming constant environmental conditions). 
#' This function calculates Sgen1, or the spawner abundances that would result 
#' in recovery to Smsy within one generation. 
#' 
#' @param Sgen.hat A proposed numeric value for Sgen
#' @param theta A numeric vector containing the estimated parameters a, b, and
#' sigma from the Ricker function fit to data
#' @param Smsy The calculated value of Smsy based on theta, from the calcSmsy
#' function.
#' @return Returns the numeric value of Sgen1.
#' 
#' @examples 
#' 
calcSgen <- function(Sgen.hat, theta, Smsy){
  
  fit <- optimize(f = Sgen.optim, interval = c(0, Smsy), theta = theta, Smsy = Smsy)
  
  # Give warning if Sgen1 is at upper or lower bound
  if(round(fit$minimum) == 0){
    warning("Sgen1 at lower bound of zero")
  }
  
  if(round(fit$minimum) == round(Smsy)){
    warning("Lower benchmark greater than upper benchmark (Sgen1 > Smsy). Set to NA.")
    return(Sgen1 = NA)
  } else {
    return(Sgen1 = as.numeric(fit$minimum))
  }
}

# Function that calculates the upper stock-recruitment benchmark = the maximum 
# Sustainability Yield (Smsy)
#' This function calculates Smsy, or the spawner abundance projected to 
#' maintain long-term maximum sustainable yield from a population with 
#' Ricker dynamics. It applied the explicit solution for Smsy given by
#' Scheuerell (2016), PeerJ, DOI 10.7717/peerj.1623
#' This function uses the lambertW0 function from the `lamW` library.
#'
#' @param a A numeric value giving Ricker parameter a (or log alpha), estimated
#' from the observed spawner-recruitment data.
#' @param b A numeric value giving Ricker parameter b (strength of density 
#' dependence; units spawners^(-1)), estimated from the observed spawner-
#' recruitment data.
#' @return Returns the value of Smsy.
#'
#' @examples
#' #

calcSmsy <- function(a, b) {
  
  require(lamW) # for the Lambert-W Function
  
  Smsy = (1 - lamW::lambertW0(exp(1 - a))) / b
  
  return(as.numeric(Smsy))
}

# Function to calculate highest posterior density (HPD) and HPD interval
HPD <- function(x, xmax = NA, na.rm = TRUE,n = 5000){
  if(is.na(xmax)){
    xmax <- max(x, na.rm = na.rm)
  }
  dens <- density(x, from = 0, to = xmax, na.rm = na.rm, n = n)
  m <- dens$x[which(dens$y == max(dens$y))][1]
  
  mCI <- HPDinterval(mcmc(c(x)), prob = 0.95)[1,]
  
  output <- c(m, mCI)
  names(output) <- c("m","mCI")
  return(output)
}

# Function which computes linear-regression estimates of parameters and MSY 
# parameters (i.e., the simple linearized Ricker model) and plots. The function 
# returns a list of the coefficients a, b and sigma.
# Nyrs: the number of years for each CU
# S and R:  year x CU matrices of fish counts for spawners and recruiters 
# LNRS: log(R/S)     BSC: could be created inside the function with R and S to limit the number of parameters to pass in
# StNames: the same of the CUs
linRegRicker_fun <- function(S, R, plot_figures = T){
  
  CUs <- colnames(S)
  nCUs <- ncol(S)
  a <- vector(length = nCUs)  # Vector to store productivity parameter
  b <- rep(NA, nCUs)           # Vector to store density-dependence parameter
  sigma <- rep(NA, nCUs)         # Vector to store estimates of sigma
  
  # in case we want to plot:
  if(nCUs < 5){
    ngrows <- nCUs
    ngcol <- 1
  }else if(5 <= nCUs & nCUs < 9){
    ngrows <- 4
    ngcol <- 2
  }else if(9 <= nCUs & nCUs < 13){
    ngrows <- 4
    ngcol <- 3
  }else if(13 <= nCUs & nCUs < 17){
    ngrows <- 4
    ngcol <- 4
  }else if(17 <= nCUs & nCUs < 20){
    ngrows <- 4
    ngcol <- 5
  }
  
  par(mfcol = c(ngrows,ngcol),
      mai = c(.4,.3,.3,.2),     # size of margin size in inches
      omi = c(0.1,0.1,0.1,0.1)) # size outer margins in inches
  
  for(i in 1:nCUs){
    
    Rcu <- R[,i, drop = F]            # remove the NAs that are now present (to address SP's comment in HBSRM.R)
    Rcu <- Rcu[!is.na(Rcu),,drop = F]   
    Scu <- S[,i, drop = F]            
    Scu <- Scu[!is.na(Scu), drop = F]
    
    LnRS <- log(Rcu/Scu)   # simple linearized Ricker model
    
    reg <- lm(LnRS ~ Scu)
    
    # Extract fitted parameters
    a[i] <- reg$coefficients[1]           #;tau[i]=as.double(1/sd(reg$residuals)^2)
    b[i] <- abs(reg$coefficients[2])
    sigma[i] <- summary(reg)$sigma
    
    output <- list(a = a, b = b, sigma = sigma)
    
    # Compute production parameters
    b1 <- a[i]/b[i]
    Prod <- round(exp(a[i]),digits = 1)
    Smsy <- round(b1 * (0.5 - 0.07 * a[i]), digits = 0)
    Smax <- round(b1 / a[i], digits = 0)
    Uopt <- round(0.5 * a[i] - 0.07 * a[i]^2, digits = 2)
    
    print(paste0("*** ",CUs[i]," ***"))
    print(do.call(what = cbind,args = list(a = a[i], b = b[i], sigma = sigma[i])))
    print(c("Prod=",Prod))
    print(c("Smsy=",Smsy))
    print(c("Smax=",Smax))
    print(c("Uopt=",Uopt))
    print("")
    
    # Plot basic SR curves and data
    # plot(S[,i], LNRS[1:Nyrs[i],i],bty='l',xlab="Spawners",xlim=c(0,max(S)),ylim=c(0,max(LNRS)),ylab="Ln(R/S)",main=CUs[i])
    # abline(reg,lty=1,lwd=2)
    
    if(plot_figures){
      
      Sx <- seq(0,max(Scu),by = 100)
      pR <- Sx * exp(a[i] - b[i] * Sx)
      plot(x = Scu,y = Rcu,
           bty = 'l',xlab = "",ylab = "",
           xlim = c(0,max(Scu)),ylim = c(0,max(Rcu)),
           main = CUs[i])
      lines(pR ~ Sx,lty = 1,lwd = 2)
      abline(a = 0,b = 1,lty = 2)
    }
    
  }
  output <- list(a = a, b = b, sigma = sigma)
  return(output)
}

# Function to calculate posterior median and quantiles
# * THis is the current method used in the PSE*
medQuan <- function(x, na.rm = TRUE){
  m <- median(x, na.rm = na.rm)
  mCI <- quantile(x, probs = c(0.025, 0.975), na.rm = na.rm)
  output <- c(m, mCI)
  names(output) <- c("m","mCI")
  return(output)
}

# Function that returns the name of the regions. This is to ensure that no spelling
# mistakes are make.
regions_fun <- function(){
  
  regions <- data.frame(
    Central_coast = 'Central_coast',
    Columbia = "Columbia", 
    Fraser = 'Fraser',
    Haida_Gwaii = 'Haida_Gwaii',
    Nass = 'Nass',
    Skeena = 'Skeena',
    Yukon = 'Yukon')
  
  return(regions)
}

#' Optimization routine for calculation of Sgen
#' This function calculates the likelihood of residuals between the projected
#' recruits from an estimated Sgen.hat and a known value of Smsy, to be used in
#' the optimization of Sgen in the calcSgen function. This function is based on
#' the Sgen.optim function in the samSim package (Freshwater et al. 2018).
#' 
#' @param Sgen.hat A proposed numeric value for Sgen
#' @param theta A numeric vector containing the estimated parameters a, b, and
#' sigma from the Ricker function fit to data
#' @param Smsy The calculated value of Smsy based on theta, from the calcSmsy
#' function.
#' @return Returns the negative log likelihood of the residuals.

Sgen.optim <- function (Sgen.hat, theta, Smsy) {
  # # Add warning and adjustment for non-zero spawner abundances
  # if(any(Sgen.hat < 0.00001)){
  # 	Sgen.hat[Sgen.hat < 0.00001] <- 0.0001
  # 	print(c("Sgen.hat abundance must be > 0. Negative values replaced w/ small positive"))
  # }
  # 
  # if(any(Smsy < 0.00001)){
  # 	Smsy[Smsy < 0.00001] <- 0.0001
  # 	print(c("Smsy must be > 0. Negative values replaced w/ small positive"))
  # }
  
  a <- theta[1]
  b <- theta[2]
  sig <- exp(theta[3])
  
  # Compute projected recruits based on Sgen.hat
  Smsy.hat <- Sgen.hat * exp(a - b * Sgen.hat) 
  
  # Calculate residuals and negative log likelihood
  epsilon <- log(Smsy) - log(Smsy.hat)
  nloglike <- - sum(dnorm(x = epsilon, mean = 0, sd = sig, log = TRUE))	
  
  return(nloglike)
}

# Function that returns a data frame of the fish species names (as column names)
# and corresponding acronym.
# BSC: figure out what these are exactly --> to double check
species_acronym_fun <- function(){
  
  # species_acronym <- data.frame(
  #   name = c("Chinook","Chum","Coho","Pink","Sockeye","Steelhead","Cutthroat"),
  #   acronym = c("CK","CM","CO","PK","SX","SH","CT"))
  
  species_acronym <- data.frame(
    Chinook = "CK",
    Chum = "CM",         # to check
    Coho = "CO",         # to check
    Pink = "PK",
    Sockeye = 'SX',
    Steelheqd = "SH",   # to check
    Cutthroat = "CT")   # to check
  
  return(species_acronym)
}

# Function that returns a list of two data frame from the _SRdata.txt for a given 
# species in a given region. The path of the 
# path_file <- fndata[i]  # the path of the _SRdata.text file
# MinSRpts: the minimum number of year (= data points) required 
# wd_Data: the biological-status/Data folder 
SRdata_fun <- function(path_file, wd_Data, MinSRpts = 3){
  
  # BSC: I replaced "stock" with "CU" in the code?
  CUs_nb <- scan(file = path_file, nlines = 1, skip = 1)
  
  # Import the prSmax and prCV for each CU
  d_prior <- read.table(file = path_file, header = T, skip = 2, nrows = CUs_nb)
  # **SP: These priors differ among stocks. Where do they come from?
  
  # Import the fish counts for R and S per year for each CU
  d0 <- read.table(file = path_file, header = T, skip = 3 + CUs_nb, 
                   fill = TRUE, stringsAsFactors = FALSE)
  d0$BY <- as.numeric(d0$BY) # brood year
  
  # Only retain rows with values
  d <- subset(x = d0, subset =  !is.na(Rec) & !is.na(Esc) & BY >= FBYr & Esc > 0) # BSC: why  Esc > 0  and not >= 0?
  
  # get the names of the CUs
  StNames <- as.character(unique(d$CU)) # name of CUs; as.character() is used because certain CUs have a number for name
  StNames <- unique(d$CU)
  
  # in case the CUID is given, replace it by the corresponding name of the CU(s)
  areThereCUID <- suppressWarnings(!is.na(as.numeric(StNames)))
  if(sum(areThereCUID) > 0){
    CUIDsHere <- as.numeric(StNames[areThereCUID])
    # Import Appendix 1: table from the tech report:
    CUIDs <- read.csv(paste0(wd_Data,"/appendix1.csv"),header = T, 
                      stringsAsFactors = F)
    # filter/subset
    CUIDs_cut <- CUIDs[CUIDs$CUID %in% CUIDsHere,]
    
    # preserve the order because %in% does not
    CUIDsHere_names <- CUIDs_cut[order(CUIDsHere),]$Conservation.Unit   
    StNames[areThereCUID] <- CUIDsHere_names
    
    # update d and d0_prior
    for(j in 1:length(CUIDsHere)){
      d$CU[d$CU == CUIDsHere[j]] <- CUIDsHere_names[j]
      d_prior$CU[d_prior$CU == CUIDsHere[j]] <- CUIDsHere_names[j]
    }
  }
  
  # nb of year per CU
  Nyrs <- tapply(X = d, INDEX = d$CU, FUN = nrow) 
  
  # to check the range of years for each CU:
  # tapply(X = d$BY, INDEX = d$CU, FUN = function(x){range(x)})
  
  # retain CUs with enough data points (i.e., Nyrs >= MinSRpts)
  StNames <- StNames[which(Nyrs >= MinSRpts)]
  
  # update objects
  d <- subset(d, CU %in% StNames)
  d_prior <- subset(d_prior, CU %in% StNames)
  
  # 
  output <- list(d,d_prior)
  names(output) <- c("counts","priors")
  
  return(output)
}

# Function that returns the input _SRdata.txt file names from there region-specific 
# repository, which is defined by wd.
# wd <- wd_Data_input
SRdata_path_Species_fun <- function(wd, Species = NA, Species_all = T){
  
  # Import the most recent individual fish counts for the region and species selected.
  # Note that the region path should be contained in wd already.
  files_list <- list.files(wd)
  
  # In the case we did not specify the Species, find those that have data:
  if(Species_all){                           
    files_s <- files_list[grepl(pattern = "_SRdata",files_list)]
    # get the species present:
    Species <- unique(sub("_SRdata.*", "", files_s))
  }
  
  # Return the corresponding files, selecting the most recent ones eventually:
  SRdata <- sapply(X = Species, FUN = function(s){
    # s <- Species[3]
    files_s <- files_list[grepl(pattern = paste0(s,"_SRdata"),files_list)]
    
    # if multiple files, select the one with the most recent date modified:
    if(length(files_s) > 1){
      files_dates <- file.info(paste(wd,files_s,sep="/"))$mtime
      files_s <- files_s[files_dates == max(files_dates)]
    }
    # if no file is present --> NA
    if(length(files_s) == 0){
      files_s <- NA
      print(paste0("Species ",s," does not have a file."))
    }
    return(files_s)
  })
  
  SRdata <- rbind(SRdata)           # coerce into an array
  SRdata <- SRdata[!is.na(SRdata)]  # remove species with that do not have file.
  
  # Get the full path:
  SRdata <- paste(wd_Data_input,SRdata,sep = "/")
  
  output <- list(Species,SRdata)
  names(output) <- c("Species","SRdata")
    
  return(output)
}

# Function that returns a data frame with the path leading to the repository where 
# input data (i.e., run reconstructions) is located for each region.
# wd_Data_input_root is the root directory common to all the regions.
# BSC: we probably will need to organize these datasets better
wd_data_regions_fun <- function(wd_root = ""){
  
  wd_data_regions <- data.frame(
    Central_coast = paste0(wd_root,"/1_Active/Central Coast PSE/analysis/central-coast-status/HBM and status"),
    Columbia = paste0(wd_root,"/1_Active/Columbia/data & analysis/analysis/columbia-status"), # BSC: ? no HBM and status folder... to check at some point
    Fraser = paste0(wd_root,"/Fraser_VIMI/analysis/fraser-status/HBM and status"),
    Haida_Gwaii = paste0(wd_root,"/1_Active/Haida Gwaii PSE/Data & Assessments/haida-gwaii-status/HBM and status"),
    Nass = paste0(wd_root,"/Nass/assessments/2_population/nass-status/HBM and status"),
    Skeena = paste0(wd_root,"/Skeena Updates/skeena-status/HBM and status"),
    Yukon = paste0(wd_root,"/1_Active/Yukon/Data & Assessments/yukon-status/HBM-and-status"))
  
  return(wd_data_regions)
}



