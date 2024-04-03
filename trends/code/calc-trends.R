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
library(zoo) # for rollmean function

source("code/functions_general.R")


###############################################################################
# Load input data from database
###############################################################################

# CU-level spawner abundance data
spawners <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset1cu_output")
spawners$estimated_count[which(spawners$estimated_count == -989898)] <- NA

# Filter out no estimated spawner abudnance
spawners <- spawners %>% filter(!is.na(estimated_count))

# CU list (includes gen length info for running avg)
cu_list <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_conservationunits_decoder") %>%
  select(region, species_abbr, pooledcuid, cuid, cu_name_pse, gen_length)

###############################################################################
# Functions (to be moved to code/functions_general.R ?) 
###############################################################################

# #------------------------------------------------------------------------------
# # Smooth abundance using running mean over generation length
# # ** Note: not geometric mean because spawners are already on log scale
# #------------------------------------------------------------------------------
# 
# gen_smooth <- function(
#     abund, # Estimate of abundance (run size or spawner abundance) in order of increasing year
#     years, # Ordered vector of years that correspond to abund
#     genLength # generation length for geometric smoothing
# ){
#   
#   
#   g <- as.numeric(genLength)
#   yrs <- sort(unique(years))
#   n.yrs <- length(yrs)
#   
#   # Run checks
#   if(length(yrs[1]:max(yrs)) != n.yrs){
#     stop("Vector of years must be continuous.")
#   }
#   
#   if(length(abund) != n.yrs){
#     stop("Length of abundance does not match length of unique years.")
#   }
#   
#   # Set up vector to store smoothed abundance
#   smoothedAbund <- rep(NA, n.yrs)
#   
#   for(k in 1:n.yrs){ # For each year
#     
#     smoothYrs <- c(max(yrs[1], yrs[k] - g + 1):yrs[k]) # define previous years over which to smooth
#     
#     # Unweighted geometric mean
#     S <- abund[which(yrs %in% smoothYrs)] + 0.01
#     # Add 0.01 to spawners so that geometric mean is not zero for multiple years if there is an observation of zero?
#     
#     
#     N <- sum(!is.na(S)) # number of years with data
#     if(N > 0){ # If there are no data, leave as NA
#       smoothedAbund[k] <- prod(S, na.rm = TRUE) ^ (1/N)
#     }
#   } # end k years
#   
#   return(smoothedAbund)
# }

# Can just use standard running mean functions


###############################################################################
# Create structure of output dataset
###############################################################################

cuid <- unique(spawners$cuid)

spawner_trends <- data.frame(
  region = cu_list$region[match(cuid, cu_list$pooledcuid)],
  cuid = cuid,
  species = cu_list$species_abbr[match(cuid, cu_list$pooledcuid)],
  cu_name_pse = cu_list$cu_name_pse[match(cuid, cu_list$pooledcuid)],
  trend_LT = NA,
  trend_3gen = NA
)

###############################################################################
# Calculate long-term trends
###############################################################################

head(spawners)

# Add year range
spawner_trends <- spawner_trends %>% left_join(
  spawners %>% 
  select(cuid, year, estimated_count) %>%
  filter(!is.na(estimated_count)) %>%
  group_by(cuid) %>%
  summarise(first_year = min(year), last_year = max(year))
)



for(i in 1:nrow(spawner_trends)){
  
  spawners.i <- subset(spawners, cuid == spawner_trends$cuid[i]) 
  
  # Ensure dataframe is in increasing year
  x <- spawner_trends$first_year[i]:spawner_trends$last_year[i]
  y <- rep(NA, length(x))
  y[match(spawners.i$year, x)] <- spawners.i$estimated_count
  
  log.y <- log(y + 0.01)
  
  # Smooth spawner abundance using running mean
  smooth.y <- rollmean(
    log.y, 
    k = cu_list$gen_length[cu_list$cuid == spawner_trends$cuid[i]], 
    na.pad = TRUE, 
    align = "right")
  
  # Calculate long-term trend
  lm1 <- lm(smooth.y ~ x)
  lines(x, predict(lm1, newdata = data.frame(x)), col = 4)
  
  # Plotting
  plot(spawners.i$year, spawners.i$estimated_count, col = grey(0.8), "o")
  lines(spawners.i$year, smooothedSpawners.i)
  
  plot(spawners.i$year, log(smooothedSpawners.i), "o", pch = 19, cex = 0.8, ylog = TRUE)
 
  
}
