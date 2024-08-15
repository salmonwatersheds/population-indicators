
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
