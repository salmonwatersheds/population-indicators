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
library(stringr)
source("code/functions_general.R")
source("code/colours.R")

###############################################################################
# Read in data
###############################################################################

Dropbox_directory <- "/Users/stephaniepeacock/Salmon\ Watersheds\ Dropbox/Stephanie\ Peacock/X\ Drive/1_PROJECTS/1_Active/Population\ Methods\ and\ Analysis/population-indicators"

regions <- c("yukon", "transboundary", "haida gwaii", "nass", "skeena", "central coast", "vancouver island and mainland inlets", "fraser", "columbia")
dat <- read.csv(paste0(Dropbox_directory, "/timing/data/3Life_cycle_timing_by_CU.csv")) %>%
  select(species, culabel, region, rt_start, rt_peak, rt_end, rt_dat_qual) %>%
  arrange(factor(region, levels = regions), species, culabel)

cu_decoder <- read.csv(paste0(Dropbox_directory, "/data-input/conservationunits_decoder.csv"))

dat$cuid <- cu_decoder$pooledcuid[match(paste(dat$species, dat$culabel), paste(cu_decoder$species_abbr, cu_decoder$cu_name_pse))]

# Remove any duplicates (for some reason duplicate of TBR SER - check with sam)
dat <- dat[-which(dat$cuid == 1023)[2], ]

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

cases <- list(
  all = as.numeric(which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 0)), # Has start, peak, and end
  startend = as.numeric(which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 1 & is.na(dat$rt_peak))), # Has start, end, but NO PEAK -> Assume normal with peak mid-way between start and end
  # startpeak = which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 1 & is.na(dat$rt_end)), # None
  # endpeak None
  # peak = which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 2 & !is.na(dat$rt_peak)), # has just peak -> No CUS
  start = as.numeric(which(apply(is.na(dat[, c("rt_start", "rt_peak", "rt_end")]), 1, sum) == 2 & !is.na(dat$rt_start))) # Has just start -> Many CUs, but can we use this?
)

# Check all cases accounted for
length(cases[[1]]) + length(cases[[2]]) + length(cases[[3]]) == dim(dat)[1]

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
# Calculate daily proportions from start, peak, and end dates
###############################################################################

DOY <- c(1:365)
DOY2 <- c(1:730)

n.cuid <- length(unique(dat$cuid))
cuid <- unique(dat$cuid)

# generate double wide matrix to deal with run timings that span the year
dat.wide <- array(NA, 
                  dim = c(n.cuid, length(DOY2)),
                  dimnames = list(cuid, DOY2)
)

for(i in 1:n.cuid){
  
  dat.i <- dat[dat$cuid == cuid[i], c("rt_start", "rt_peak", "rt_end")]
  
  # Adjust peak and end to ensure they are > start
  if(!is.na(dat.i$rt_peak) & dat.i$rt_peak < dat.i$rt_start){
    dat.i$rt_peak <- dat.i$rt_peak + 365
  }
  
  if(!is.na(dat.i$rt_end) & dat.i$rt_end < dat.i$rt_start){
    dat.i$rt_end <- dat.i$rt_end + 365
  }
  
  # Case 1: All three datapoints
  if(i %in% cases[[1]]){ 
    
    # Left hand side
    sd_left <- (dat.i$rt_peak - dat.i$rt_start)/1.96
    left <- dnorm(x = c(1:round(dat.i$rt_peak)), mean = dat.i$rt_peak, sd = sd_left)
    
    # Right hand side
    sd_right <- (dat.i$rt_end - dat.i$rt_peak)/1.96
    right <- dnorm(x = c(round(dat.i$rt_peak):730), mean = dat.i$rt_peak, sd = sd_right)
    
    together <- c(left/max(left), (right/max(right))[2:length(right)])
    # plot(c(1:365), together, "l")
  
    dat.wide[i, ] <- together
  
    rm(together, sd_left, sd_right, left, right)
  
  # Case 2: Has start and end but no peak
  # Solution: Assume peak is mid-point
  } else if(i %in% cases[[2]]){ 
    
    together0 <- dnorm(
      x = DOY2, 
      mean = mean(dat.i$rt_start, dat.i$rt_end), 
      sd = abs(dat.i$rt_start - dat.i$rt_end)/(2 * 1.96))
    
    together <- together0/max(together0)
    
    dat.wide[i, ] <- together
    
    rm(together0, together)
    
  # Case 3: Has just start
  } else if(i %in% cases[[3]]){ 
    # Assume sd equal to sd for other CU of the same species in the region
    species.i <- dat$species[dat$cuid == cuid[i]]
    if(species.i %in% c("PKE", "PKO")){
      species.i <- "PK"
    }
    sd <- timing.sd$sd[timing.sd$species == species.i & timing.sd$region == dat$region[dat$cuid == cuid[i]]]
    if(is.na(sd)){
      if(dat$region[dat$cuid == cuid[i]] == "haida gwaii"){
        sd <- timing.sd$sd[timing.sd$species == species.i & timing.sd$region == "nass"]
      } else if(dat$region[dat$cuid == cuid[i]] == "central coast"){
        sd <- timing.sd$sd[timing.sd$species == species.i & timing.sd$region == "skeena"]
      } else {
        stop("sd is NA")
      }
    }
    
    together0 <- dnorm(
      x = DOY2, 
      mean = dat.i$rt_start + 1.96 * sd, 
      sd = sd)
    
    together <- together0/max(together0)
    
    dat.wide[i, ] <- together
    
    rm(together0, together, sd)
    
  } # end case 3
  
} # end cuid i

###############################################################################
# Format for database (long format)
###############################################################################
dat.wide2 <- dat.wide[, 1:365] + dat.wide[, 366:730]

dat.out <- expand.grid(DOY, dat$cuid) %>%
  dplyr::rename(DOY = "Var1", cuid = "Var2")
head(dat.out)
str(dat.out)
dat.out$run_timing_ppn <- NA
dat.out$run_timing_quality <- NA

for(i in 1:n.cuid){
  dat.out$run_timing_quality[dat.out$cuid == cuid[i]] <- dat$rt_dat_qual[dat$cuid == cuid[i]]
  dum <- dat.wide2[which(as.numeric(rownames(dat.wide2)) == cuid[i]), ]
  dat.out$run_timing_ppn[dat.out$cuid == cuid[i]] <- dum/max(dum)
  rm(dum)
}

dat.out$run_timing_ppn <- round(dat.out$run_timing_ppn, digits = 3)


write.csv(dat[, c("cuid", "rt_dat_qual")], file = paste0(Dropbox_directory, "/timing/output/run-timing-data-quality_", Sys.Date(), ".csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Second option: Normalize to sum to 1 rather than peak = 1
#------------------------------------------------------------------------------

dat.out2 <- expand.grid(DOY, dat$cuid) %>%
  dplyr::rename(DOY = "Var1", cuid = "Var2")
dat.out2$run_timing_ppn <- NA

for(i in 1:n.cuid){
  dum <- dat.wide2[which(as.numeric(rownames(dat.wide2)) == cuid[i]), ]
  dat.out2$run_timing_ppn[dat.out2$cuid == cuid[i]] <- dum/sum(dum)
  rm(dum)
}

dat.out2$run_timing_ppn <- round(dat.out2$run_timing_ppn, digits = 4)

write.csv(dat.out2[!is.na(dat.out2$run_timing_ppn), c("cuid", "DOY", "run_timing_ppn")], file = paste0(Dropbox_directory, "/timing/output/run-timing_", Sys.Date(), ".csv"), row.names = FALSE)

# # compare
# cuid2compare <- cu_decoder$pooledcuid[cu_decoder$region == "Fraser" & cu_decoder$species_abbr == "SEL" & cu_decoder$cu_type == 'Current']
# n <- length(cuid2compare)
# xDate <- as.Date(paste(1999, DOY, sep = "-"), format = "%Y-%j")
# xDate2 <- as.Date(paste(c(rep(1999, 365), 2000), c(DOY, 1), sep = "-"), format = "%Y-%j")
# quartz(width = 5, height =  n/6+1.5, pointsize = 10)
# par(mar = c(3,2,2,3))
# plot(range(xDate2), c(1, n + 1), "n", bty = "l", xlab = "Day", ylab = "", main = "Fraser SEL", yaxt = "n", yaxs = "i", xaxs = "i")
# mtext(side = 2, line = 1, "Run timing by CU")
# for(i in 1:n){
#   lines(xDate, i + dat.out$run_timing_ppn[dat.out$cuid == cuid2compare[i]], col = species_cols_dark["Sockeye"], lwd = 2, xpd = NA)
#   # lines(xDate, i + 30*dat.out2$run_timing_ppn[dat.out2$cuid == cuid2compare[i]], col = species_cols_light["Sockeye"], lwd = 2, xpd = NA)
#   text(par('usr')[2], i+0.2, dat$culabel[which(dat$cuid == cuid2compare[i])], col = species_cols_light["Sockeye"], lwd = 2, xpd = NA, cex = 0.7, xpd = NA)
# }


###############################################################################
# Plot
###############################################################################

# species_cols <- c(
#   CK = SWP_cols['soil1'],
#   CM = SWP_cols['soil2'],
#   CO = SWP_cols['stone1'],
#   PKE = SWP_cols['clay'],
#   PKO = SWP_cols['clay'],
#   SEL = SWP_cols['stone2'],
#   SER = SWP_cols['stone2'],
#   SH = SWP_cols['tidal']
# )

source("code/colours.R")
species_cols <- species_cols_light[c(1, 2, 3, 4, 4, 5, 5, 6)]
names(species_cols) <- c("CK", "CM", "CO", "PKE", "PKO", "SEL", "SER", "SH")

for(r in 1:9){
  n <- length(which(dat$region == regions[r]))
# pdf(file = paste0(Dropbox_directory, "/timing/output/run-timing_", regions[r], ".pdf"), width = 5, height =  n/7.5+1.5, pointsize = 10)
quartz(width = 5, height =  n/6+1.5, pointsize = 10)
  plot(range(DOY2), c(1, n + 1), "n", bty = "l", xlab = "Day", ylab = "Run timing by CU", main = str_to_title(regions[r]), yaxt = "n", yaxs = "i", xaxs = "i")
for(i in 1:n){
  lines(DOY2, i + dat.wide[which(dat$region == regions[r])[i], ], col = species_cols[dat$species[which(dat$region == regions[r])[i]]], lwd = 2, xpd = NA)
  text(600, i+0.2, dat$culabel[which(dat$region == regions[r])[i]], col = species_cols[dat$species[which(dat$region == regions[r])[i]]], cex = 0.8)
}
  # dev.off()
}

plot(DOY2, i + dat.wide[i, ], col = species_cols[dat$species[i]], "l", lwd = 1.2)

xDate <- as.Date(paste(1999, DOY, sep = "-"), format = "%Y-%j")
xDate2 <- as.Date(paste(c(rep(1999, 365), 2000), c(DOY, 1), sep = "-"), format = "%Y-%j")
for(r in 1:9){
  n <- length(which(dat$region == regions[r]))
  # pdf(file = paste0(Dropbox_directory, "/timing/output/run-timing_", regions[r], ".pdf"), width = 5, height =  n/7.5+1.5, pointsize = 10)
  quartz(width = 5, height =  n/6+1.5, pointsize = 10)
  par(mar = c(3,2,2,3))
  plot(range(xDate2), c(1, n + 1), "n", bty = "l", xlab = "Day", ylab = "", main = str_to_title(regions[r]), yaxt = "n", yaxs = "i", xaxs = "i", xlim = xDate2[c(100, 320)])
  mtext(side = 2, line = 1, "Run timing by CU")
  for(i in 1:n){
    lines(xDate, i + dat.wide2[which(dat$region == regions[r])[i], ], col = species_cols[dat$species[which(dat$region == regions[r])[i]]], lwd = 2, xpd = NA)
    text(xDate[350], i+0.2, dat$culabel[which(dat$region == regions[r])[i]], col = species_cols[dat$species[which(dat$region == regions[r])[i]]], cex = 0.7, xpd = NA)
  }
  # dev.off()
}

# Compare dat.out and dat.out2
cu_decoder$pooledcuid[cu_decoder$region == "Fraser" & cu_decoder$species_abbr == "SEL" & cu_decoder$cu_type == 'Current']


