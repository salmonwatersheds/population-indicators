###############################################################################
# Calculation of long-term and 3-generation trends in CU-level spawner abundance
# 
# inputs: spawner abundance from the database
# outputs: dataset202 and dataset 391
# 
# Stephanie Peacock
# January 17, 2024
###############################################################################
library(dplyr)

source("code/functions_general.R")


###############################################################################
# Load input data from database
###############################################################################

# CU-level spawner abundance data
spawners <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset1cu_output")
spawners$estimated_count[which(spawners$estimated_count == -989898)] <- NA
 

# CU list (includes gen length info for running avg)
cu_list <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_conservationunits_decoder") %>%
  select(region, species_abbr, pooledcuid, cuid, cu_name_pse, gen_length)

###############################################################################
# Functions (to be moved to code/functions_general.R ?) 
###############################################################################

#------------------------------------------------------------------------------
# Smooth abundance using running geometric mean over generation length
#------------------------------------------------------------------------------

gen_smooth <- function(
    abund, # Estimate of abundance (run size or spawner abundance) in order of increasing year
    years, # Ordered vector of years that correspond to abund
    genLength # generation length for geometric smoothing
){
  
  
  g <- as.numeric(genLength)
  yrs <- sort(unique(years))
  n.yrs <- length(yrs)
  
  # Run checks
  if(length(yrs[1]:max(yrs)) != n.yrs){
    stop("Vector of years must be continuous.")
  }
  
  if(length(abund) != n.yrs){
    stop("Length of abundance does not match length of unique years.")
  }
  
  # Set up vector to store smoothed abundance
  smoothedAbund <- rep(NA, n.yrs)
  
  for(k in 1:n.yrs){ # For each year
    
    smoothYrs <- c(max(yrs[1], yrs[k] - g + 1):yrs[k]) # define previous years over which to smooth
    
    # Unweighted geometric mean
    S <- abund[which(yrs %in% smoothYrs)] + 0.01
    # Add 0.01 to spawners so that geometric mean is not zero for multiple years if there is an observation of zero?
    
    
    N <- sum(!is.na(S)) # number of years with data
    if(N > 0){ # If there are no data, leave as NA
      smoothedAbund[k] <- prod(S, na.rm = TRUE) ^ (1/N)
    }
  } # end k years
  
  return(smoothedAbund)
}

###############################################################################
# Create structure of output dataset
###############################################################################

cuid <- unique(cu_list$pooledcuid)

spawner_trends <- data.frame(
  cuid = cuid,
  species = cu_list$species_abbr[match(cuid, cu_list$pooledcuid)]
)

###############################################################################
# Calculate long-term trends
###############################################################################

for(i in 1:cuid){
  spawners.i <- subset(spawners, cuid == cuid[i])
  
  # Ensure dataframe is in increasing year
  spawners.i <- spawners.i[order(spawners.i$year), ]
  
  # Truncate to latest year of data
  last.year <- max(spawners.i$year[!is.na(spawners.i$estimated_count)])
  spawners.i <- spawners.i[which(spawners.i$year <= last.year), ]
  
  # Smooth spawner abundance using running mean
  smooothedSpawners.i <- gen_smooth(
    abund = spawners.i$estimated_count, 
    years = spawners.i$year, 
    genLength = unique(cu_list$gen_length[which(cu_list$pooledcuid == cuid[i])])
  )
  
  # Log smoothed spawner abundance
  logSmooothedSpawners.i <- log(smooothedSpawners.i + 0.001) # Add a small number??
  
  
 # Calculate long-term trend
  lm1 <- lm(logSmooothedSpawners.i ~ spawners.i$year)
  lines(spawners.i$year, predict(lm1, newdata = data.frame(which(!is.na(spawners.i$year)))
  
  # Plotting
  plot(spawners.i$year, spawners.i$estimated_count, col = grey(0.8), "o")
  lines(spawners.i$year, smooothedSpawners.i)
  
  plot(spawners.i$year, log(smooothedSpawners.i), "o", pch = 19, cex = 0.8, ylog = TRUE)
 
  
}
