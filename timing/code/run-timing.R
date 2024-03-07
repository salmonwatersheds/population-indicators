###############################################################################
# This code updates the legacy PSE run-timing data to:
# 1. Include more data compiled by Sam Wilson;
# 2. Introduce a data quality score for run timing for each CU;
# 3. Allow for skewed distributions or other non-normal distributions.
#
# Steph Peacock
# Mar 6, 2024
###############################################################################


library(dplyr)
source("code/functions_general.R")

###############################################################################
# Read in data
###############################################################################

Dropbox_directory <- "/Users/stephaniepeacock/Salmon\ Watersheds\ Dropbox/Stephanie\ Peacock/X\ Drive/1_PROJECTS/1_Active/Population\ Methods\ and\ Analysis/population-indicators"

regions <- c("yukon", "transboundary", "haida gwaii", "nass", "skeena", "central coast", "vancouver island and mainland inlets", "fraser", "columbia")
dat <- read.csv(paste0(Dropbox_directory, "/timing/data/3Life_cycle_timing_by_CU.csv")) %>%
  select(species, culabel, region, rt_start, rt_peak, rt_end, rt_dat_qual) %>%
  arrange(factor(region, levels = regions), species, culabel)

cu_decoder <- read.csv(paste0(Dropbox_directory, "/data-input/conservationunits_decoder.csv"))

dat$cuid <- conservationunit_decoder$cuid[match(paste(dat$species, dat$culabel), paste(cu_decoder$species_abbr, cu_decoder$cu_name_pse))]

# Are there any CUs that don't have a cuid?
head(dat[is.na(dat$cuid),]) # Two CUs that aren't in PSE; just remove
dat <- dat[!is.na(dat$cuid), ]

# Remove CUs that don't have any run timing data
dat <- dat[which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) != 3), ]

# How many CUs do we hav run timing for?
dim(dat) # 337!

sum(is.na(dat$rt_peak))
sum(is.na(dat$rt_start)) # all have at least a start
sum(is.na(dat$rt_end)) # 94 don't have an end
sum(is.na(dat$rt_dat_qual))

# Has start and end but no peak - assume norma;l:
dat[,]


cases <- list(
  all = as.numeric(which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 0)), # Has start, peak, and end
  startend = as.numeric(which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 1 & is.na(dat$rt_peak))), # Has start, end, but NO PEAK -> Assume normal with peak mid-way between start and end
  # startpeak = which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 1 & is.na(dat$rt_end)), # None
  # endpeak None
  # peak = which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 2 & !is.na(dat$rt_peak)), # has just peak -> No CUS
  start = as.numeric(which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 2 & !is.na(dat$rt_start))) # Has just start -> Many CUs, but can we use this?
)

# 94 CUs have a start run timing but no peak or end...can we show this? Fill in using average sd for similar cus.
length( which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 2 & !is.na(dat$rt_start)))

# These are the groupings that we want to define sd for
unique(paste(dat$region, dat$species)[which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 2 & !is.na(dat$rt_start))])
speciesCodes <- c("CK", "CM", "CO", "PK", "SEL", "SER", "SH")
timing.sd <- expand.grid(speciesCodes, regions) %>%
  rename(species = "Var1", region = "Var2")
timing.sd$sd <- NA

for(s in 1:length(speciesCodes)){
  for(r in 1:length(regions)){
    ind.sr <- which(grepl(speciesCodes[s], dat$species) & dat$region == regions[r])
    if(sum(ind.sr %in% cases[[1]]) != 0){ # If there is at least one CU that has all three data points, use that one
      timing.sd$sd[timing.sd$species == speciesCodes[s] & timing.sd$region == regions[r]] <- mean(c(abs(dat$rt_start[ind.sr] - dat$rt_peak[ind.sr])/1.96, (dat$rt_end[ind.sr] - dat$rt_peak[ind.sr])/1.96), na.rm = TRUE)
    } else if(sum(ind.sr %in% cases[[2]]) != 0){
      timing.sd$sd[timing.sd$species == speciesCodes[s] & timing.sd$region == regions[r]] <- mean(c(abs(dat$rt_start[ind.sr] - dat$rt_end[ind.sr])/(2*1.96)), na.rm = TRUE)
    }
  }
}
###############################################################################
# Create empty data frame to store output
###############################################################################

DOY <- c(1:365)
n.cuid <- length(unique(dat$cuid))
cuid <- unique(dat$cuid)

# generate template for flat file (all combos of cuid and DOY)
dat.out <- expand.grid(DOY, dat$cuid) %>%
  dplyr::rename(DOY = "Var1", cuid = "Var2")
head(dat.out)
str(dat.out)

for(i in 1:n.cuid){
  
  dat.i <- dat[dat$cuid == cuid[i], c("rt_start", "rt_peak", "rt_end")]
  
  if(sum(is.na(dat.i)) == 0){ # If we have all three data points
    # Left hand side
    sd_left <- (dat.i$rt_peak - dat.i$rt_start)/1.96
    left <- dnorm(x = c(1:round(dat.i$rt_peak)), mean = dat.i$rt_peak, sd = sd_left)
    
    # Right hand side
    sd_right <- (dat.i$rt_end - dat.i$rt_peak)/1.96
    right <- dnorm(x = c(round(dat.i$rt_peak):365), mean = dat.i$rt_peak, sd = sd_right)
    
    together <- c(left/max(left), (right/max(right))[2:length(right)])
    # plot(c(1:365), together, "l")
  
  dat.out[dat.out$cuid == i, ] <- together
  
  rm(together, sd_left, sd_right, left, right)
  
  } else if(sum(is.na(dat.i)) == 1 & !is.na(dat.i$rt_peak)){ # If we have peak and start
    # Assume equal tails
    sd <- (dat.i$rt_peak - mean(dat.i[, c(1,3)], na.rm = TRUE))/1.96
    
  }
}
