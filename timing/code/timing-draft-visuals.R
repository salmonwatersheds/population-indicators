###############################################################################
# Code to produce draft visuals of life-history timing to inform development
# of PSE 2.0
#'
#' Imported datasets:
#' - conservationunits.csv
#' - spawner_abundance.csv
#' - fry_and_smolt_mig_20231003.csv
#' - timing_CUs.csv
#' - migration_timing.csv
#' 
#' Exported datasets: none
#' - 
#'
#'
# Cct 5, 2023
# Steph Peacock
###############################################################################


# Weighting by abundance - avg CU spawner abundance and use regional average where
# Rolling up - no roll up when all regions/multiple regions
# SMU scale data - hmm...

library(dplyr)

###############################################################################
# Read in data
###############################################################################

#------------------------------------------------------------------------------
# Conservation Unit lookup file
#------------------------------------------------------------------------------

cu <- read.csv("timing/data/conservationunits.csv")

#------------------------------------------------------------------------------
# Spawner abundance (to weight timing in full species view)
#------------------------------------------------------------------------------

spawner_abundance <- read.csv("timing/data/spawner_abundance.csv") %>%
  subset(region == "Fraser" & species_name == "Lake sockeye")

#------------------------------------------------------------------------------
# Fry migration and smolt migration (trap) data
#------------------------------------------------------------------------------

# This is a compilation of Sam's location-specific data to the CU-level
# See data_cleaning_20231003.Rmd in Sam's repo
# https://www.dropbox.com/scl/fi/f1ovp1t44l7a6jrgv1e06/data_cleaning_20231003.Rmd?rlkey=7fkarzz1rec6pihxrstsrh4qa&dl=0

trap <- read.csv("timing/data/fry_and_smolt_mig_20231003.csv") %>%
  subset(region == "fraser" & species == "SEL")

#------------------------------------------------------------------------------
# Spawn timing data
#------------------------------------------------------------------------------

# These data come from Nuseds, analysis in https://www.dropbox.com/sh/eg32hwjn9iktmoo/AAB44SrinCtOyRDl1xWdVR1Na?dl=0

spawn <- read.csv("timing/data/spawn-timing_CUs.csv") %>%
  subset(region == "Fraser" & SPECIES_QUALIFIED == "SEL")

spawn$culabel <- cu$culabel[match(spawn$cuid, cu$cuid)]
spawn <- spawn[!is.na(spawn$cuid), ]

# Create unique list of CUs 
# Just subsetted to Fraser sockeye for now
CUs <- unique(spawn$culabel)

#------------------------------------------------------------------------------
# Migration timing
#------------------------------------------------------------------------------

# This is directly taken from Data Library for current timing
mt <- read.csv("timing/data/migration_timing.csv") %>%
  subset(location %in% CUs)

# Provided as difference from Babine enhanced; need to add 206 days
mt$datavalue[mt$parameter == "run_time"] <- mt$datavalue[mt$parameter == "run_time"] + 206

CUs %in% mt$location

###############################################################################
# Test visuals with Fraser sockeye example
###############################################################################

#------------------------------------------------------------------------------
# Compile matrices of daily timing probabilities
#------------------------------------------------------------------------------
timing <- list(
  fry_mig = matrix(0, nrow = length(CUs), ncol = 365),
  ocean_entry = matrix(0, nrow = length(CUs), ncol = 365),
  run_time = matrix(0, nrow = length(CUs), ncol = 365),
  spawn_time = matrix(0, nrow = length(CUs), ncol = 365))

# Look through each CUs
for(i in 1:length(CUs)){
  
  trap.i <- trap[which(trap$culabel == CUs[i]),]
  
  if(dim(trap.i)[1] > 0){ # If there are trap data
    
    # Fry migration
    if(!is.na(trap.i$fm_mid)){
      # Left hand side
      sd_left <- (trap.i$fm_mid - trap.i$fm_start)/1.96
      left <- dnorm(x = c(1:trap.i$fm_mid), mean = trap.i$fm_mid, sd = sd_left)
      sd_right <- (trap.i$fm_end - trap.i$fm_mid)/1.96
      right <- dnorm(x = c(trap.i$fm_mid:365), mean = trap.i$fm_mid, sd = sd_left)
      
      together <- c(left/max(left), (right/max(right))[2:length(right)])
      # plot(c(1:365), together, "l")
      timing$fry_mig[i, ] <- together
    }
    
    # Ocean entry
    if(!is.na(trap.i$oe_mid)){
      # Left hand side
      sd_left <- (trap.i$oe_mid - trap.i$oe_start)/1.96
      left <- dnorm(x = c(1:trap.i$oe_mid), mean = trap.i$oe_mid, sd = sd_left)
      sd_right <- (trap.i$oe_end - trap.i$oe_mid)/1.96
      right <- dnorm(x = c(trap.i$oe_mid:365), mean = trap.i$oe_mid, sd = sd_left)
      
      together <- c(left/max(left), (right/max(right))[2:length(right)])
      # plot(c(1:365), together, "l")
      
      timing$ocean_entry[i, ] <- together
    }
  } # end if there's trap data
  
  # Run timing
  rt <- mt[which(mt$location == CUs[i]),]
  timing$run_time[i, ] <- dnorm(c(1:365), mean = rt$datavalue[rt$parameter == "run_time"], sd = rt$datavalue[rt$parameter == "run_time_sd"])
  
  # plot(c(1:365),timing$run_time[i, ], "l")
  
  # Spawn timing
  st <- spawn[which(spawn$culabel == CUs[i]), ]
  timing$spawn_time[i, ] <- dnorm(c(1:365), mean = st$mu_spawn_50, sd = st$sd_spawn_50)
  
}

#------------------------------------------------------------------------------
# Plotting
#------------------------------------------------------------------------------

# Define colour palette based on PSE 2.0
cols <- c("#8DBB9B", "#769EBA", "#D1C76E", "#BD6461")
colDum <- colorRampPalette(c(cols, cols[1]))(n = 10)
stageCols <- c(incubation = )

DOY <- c(1:365)
xDate <- as.Date(paste0("1999-", DOY), format = "%Y-%j")


#--------------
# CU-level plot
#--------------
i <- 8

par(mar = c(3, 6, 1, 1))
plot(as.Date(paste0(c("1999-", "2000-"), 1), format = "%Y-%j"), c(1, 4), "n", ylim = c(1, 5.5), yaxs = "i", xaxs = "i", xlab = "", bty = "n", yaxt = "n", ylab = "")
for(j in 4:1){
  y <- j + timing[[j]][i, ]/max(timing[[j]][i, ])*1.25
  lines(xDate, y, col = cols[j], lwd = 2)
  polygon(x = c(xDate, rev(xDate)), y = c(y, rep(j, length(xDate))), col = paste0(cols[j], 30), border = cols[j])
}
axis(side = 2, at = c(1:4), labels = NA, las = 1)
text(rep(xDate[1] - 30, 4), 1:4 + 0.5, c("Fry\nmigration", "Ocean\nentry", "Run\ntiming", "Spawn\ntiming"), col = cols, xpd = NA)
mtext(side = 3, paste0("Fraser SEL: ", CUs[i]))

#--------------
# All Cus
#--------------
# ***Note from Eric: Need to weight by abundance***
par(mar = c(3, 6, 1, 1))
plot(as.Date(paste0(c("1999-", "2000-"), 1), format = "%Y-%j"), c(1, 4), "n", ylim = c(1, 5.5), yaxs = "i", xaxs = "i", xlab = "", bty = "n", yaxt = "n", ylab = "")
for(i in 1:length(CUs)){
  for(j in 4:1){
    y <- j + timing[[j]][i, ]/max(timing[[j]][i, ])*1.25
    lines(xDate, y, col = cols[j], lwd = 2)
    polygon(x = c(xDate, rev(xDate)), y = c(y, rep(j, length(xDate))), col = paste0(cols[j], 30), border = cols[j])
  }
}
axis(side = 2, at = c(1:4), labels = NA, las = 1)
text(rep(xDate[1] - 30, 4), 1:4 + 0.5, c("Fry\nmigration", "Ocean\nentry", "Run\ntiming", "Spawn\ntiming"), col = cols, xpd = NA)
mtext(side = 3, paste0("Fraser SEL: ", CUs[i]))

# All CUs - alternate
par(mar = c(3, 6, 1, 1))
plot(as.Date(paste0(c("1999-", "2000-"), 1), format = "%Y-%j"), c(1, 4), "n", ylim = c(1, 5.5), yaxs = "i", xaxs = "i", xlab = "", bty = "n", yaxt = "n", ylab = "")
for(j in 4:1){
  y0 <- apply(timing[[j]], 2, sum, na.rm = TRUE)
  y <- j + y0/max(y0)*1.25
  lines(xDate, y, col = cols[j], lwd = 2)
  polygon(x = c(xDate, rev(xDate)), y = c(y, rep(j, length(xDate))), col = paste0(cols[j], 30), border = cols[j])
}

#--------------
# Life stage tables
#--------------
i <- 10
CUs[i]

par(mar = c(3, 8, 1, 1))
plot(as.Date(paste0(c("1999-", "2000-"), 1), format = "%Y-%j"), c(1, 15), "n", ylim = c(1, 15), yaxs = "i", xaxs = "i", xlab = "", bty = "n", yaxt = "n", ylab = "")
abline(v = as.Date(paste(1999, c(1:12), 1, sep = "-")), col = grey(0.8))
axis(side = 1, at = as.Date(paste(1999, c(1:12), 1, sep = "-")), labels = FALSE, tck = -0.01)
# axis(side = 3, at = as.Date(paste(c(rep(1999, 12), 2000), c(1:12, 1), 1, sep = "-")), labels = FALSE, tck = -0.01)

for(j in 4:1){
  y <- 7 + timing[[j]][i, ]/max(timing[[j]][i, ])*8
  lines(xDate, y, col = cols[j], lwd = 2)
  polygon(x = c(xDate, rev(xDate)), y = c(y, rep(7, length(xDate))), col = paste0(cols[j], 30), border = cols[j])
}
# text(rep(xDate[1] - 30, 4), 9:12 + 0.5, c("Fry migration", "Ocean entry", "Run timing", "Spawn timing"), col = cols, xpd = NA, cex = 0.8, font = 2)

text(x = 10592 + c(4.5, 4, 7, 8.8)*30, 
     y= c(14.8, 14.6, 13.1, 14.8), 
     c("Fry\nmigration", "Ocean\nentry", "Run\ntiming", "Spawn\ntiming"), 
     col = cols, xpd = NA, cex = 0.8, font = 2, pos = c(4, 2, 2, 4))


# Life stages
stageCols <- c(paste0(cols[4], 80),
               paste0(cols[1], 60),
               paste0(cols[2], 60),
               cols[c(2,3,4)])
lwdStage <- 15
axis(side = 2, at = c(0:7), labels = NA)
axis(side = 4, at = c(0:7), labels = NA)
abline(h = c(2:6), col = grey(0.8))
text(x = xDate[1] - 10, y = c(1:6)+0.5, c("Spawning", "Adult FW migration", "Marine rearing", "Early marine", "FW juvenile", "Incubation"), xpd = NA, adj = 1, cex = 0.8, font = 2, col = rev(stageCols))

# Incubation
segments(x0 = as.Date(paste0("1999-", spawn$start_spawn_50[spawn$culabel == CUs[i]]), format = "%Y-%j"), 
         x1 = max(xDate), 
         y0 = 6.5, y1 = 6.5, col = paste0(cols[4], 80), lwd = lwdStage)

segments(x0 = xDate[1], 
         x1 = as.Date(paste0("1999-", trap$fm_end[trap$culabel == CUs[i]]), format = "%Y-%j"), 
         y0 = 6.5, y1 = 6.5, col = paste0(cols[4], 80), lwd = lwdStage)

# FW juvenile
segments(x0 = as.Date(paste0("1999-", trap$fm_start[trap$culabel == CUs[i]]), format = "%Y-%j"), 
         x1 = max(xDate), 
         y0 = 5.5, y1 = 5.5, col = paste0(cols[1], 60), lwd = lwdStage)

segments(x0 = xDate[1], 
         x1 = as.Date(paste0("1999-", trap$oe_end[trap$culabel == CUs[i]]), format = "%Y-%j"), 
         y0 = 5.5, y1 = 5.5, col = paste0(cols[1], 60), lwd = lwdStage)

# Early marine
segments(x0 = as.Date(paste0("1999-", trap$oe_end[trap$culabel == CUs[i]]), format = "%Y-%j"), 
         x1 = as.Date(paste0("1999-", trap$oe_end[trap$culabel == CUs[i]]), format = "%Y-%j") + 365/2,
         y0 = 4.5, y1 = 4.5, col = paste0(cols[2], 60), lwd = lwdStage)

# Marine rearing
segments(x0 = xDate[1], 
         x1 = max(xDate), 
         y0 = 3.5, y1 = 3.5, col = cols[2], lwd = lwdStage)

# FW adult migratoin
rt <- mt[which(mt$location == CUs[i]),]
segments(x0 = as.Date(paste0("1999-", rt$datavalue[rt$parameter == "run_time"] - 1.96*rt$datavalue[rt$parameter == "run_time_sd"]), format = "%Y-%j"), 
         x1 = as.Date(paste0("1999-", spawn$start_spawn_50[spawn$culabel == CUs[i]]), format = "%Y-%j"), 
         y0 = 2.5, y1 = 2.5, col = cols[3], lwd = lwdStage)

# Spawning
rt <- mt[which(mt$location == CUs[i]),]
segments(x0 = as.Date(paste0("1999-", spawn$start_spawn_50[spawn$culabel == CUs[i]]), format = "%Y-%j"), 
         x1 = as.Date(paste0("1999-", spawn$end_spawn_50[spawn$culabel == CUs[i]]), format = "%Y-%j"), 
         y0 = 1.5, y1 = 1.5, col = cols[4], lwd = lwdStage)

abline(h = 7)


#--------------
# Full species view
#--------------

# calculate mean abundance
cu_abund <- tapply(spawner_abundance$estimated_count, spawner_abundance$cu_name_pse, mean, na.rm = TRUE)

# Compute overall distribution
timing_all <- list(
  fry_mig = apply(timing$fry_mig, 2, sum, na.rm = TRUE),
  ocean_entry = apply(timing$ocean_entry, 2, sum, na.rm = TRUE),
  run_time = apply(timing$run_time, 2, sum, na.rm = TRUE),
  spawn_time = apply(timing$spawn_time, 2, sum, na.rm = TRUE)
)

par(mar = c(3, 8, 1, 1))
plot(as.Date(paste0(c("1999-", "2000-"), 1), format = "%Y-%j"), c(1, 15), "n", ylim = c(1, 15), yaxs = "i", xaxs = "i", xlab = "", bty = "n", yaxt = "n", ylab = "")
abline(v = as.Date(paste(1999, c(1:12), 1, sep = "-")), col = grey(0.8))
axis(side = 1, at = as.Date(paste(1999, c(1:12), 1, sep = "-")), labels = FALSE, tck = -0.01)
# axis(side = 3, at = as.Date(paste(c(rep(1999, 12), 2000), c(1:12, 1), 1, sep = "-")), labels = FALSE, tck = -0.01)

for(j in 4:1){
    y <- 7 + timing_all[[j]]/max(timing_all[[j]])*8
    lines(xDate, y, col = cols[j], lwd = 2)
    polygon(x = c(xDate, rev(xDate)), y = c(y, rep(7, length(xDate))), col = paste0(cols[j], 30), border = cols[j])
  }

# text(rep(xDate[1] - 30, 4), 9:12 + 0.5, c("Fry migration", "Ocean entry", "Run timing", "Spawn timing"), col = cols, xpd = NA, cex = 0.8, font = 2)

# text(x = 10592 + c(4.5, 4, 7, 8.8)*30, 
#      y= c(14.8, 14.6, 13.1, 14.8), 
#      c("Fry\nmigration", "Ocean\nentry", "Run\ntiming", "Spawn\ntiming"), 
#      col = cols, xpd = NA, cex = 0.8, font = 2, pos = c(4, 2, 2, 4))


# Life stages
stageCols <- c(paste0(cols[4], 80),
               paste0(cols[1], 60),
               paste0(cols[2], 60),
               cols[c(2,3,4)])
lwdStage <- 15
axis(side = 2, at = c(0:7), labels = NA)
axis(side = 4, at = c(0:7), labels = NA)
abline(h = c(2:6), col = grey(0.8))
text(x = xDate[1] - 10, y = c(1:6)+0.5, c("Spawning", "Adult FW migration", "Marine rearing", "Early marine", "FW juvenile", "Incubation"), xpd = NA, adj = 1, cex = 0.8, font = 2, col = rev(stageCols))

# Incubation
segments(x0 = as.Date(paste0("1999-", findInterval(0.025, cumsum(timing_all$spawn_time)/sum(timing_all$spawn_time))), format = "%Y-%j"), 
         x1 = max(xDate), 
         y0 = 6.5, y1 = 6.5, col = paste0(cols[4], 80), lwd = lwdStage)

segments(x0 = xDate[1], 
         x1 = as.Date(paste0("1999-", findInterval(0.975, cumsum(timing_all$fry_mig)/sum(timing_all$fry_mig))), format = "%Y-%j"), 
         y0 = 6.5, y1 = 6.5, col = paste0(cols[4], 80), lwd = lwdStage)

# FW juvenile
segments(x0 = as.Date(paste0("1999-", findInterval(0.025, cumsum(timing_all$fry_mig)/sum(timing_all$fry_mig))), format = "%Y-%j"), 
         x1 = max(xDate), 
         y0 = 5.5, y1 = 5.5, col = paste0(cols[1], 60), lwd = lwdStage)

segments(x0 = xDate[1], 
         x1 = as.Date(paste0("1999-", findInterval(0.975, cumsum(timing_all$ocean_entry)/sum(timing_all$ocean_entry))), format = "%Y-%j"), 
         y0 = 5.5, y1 = 5.5, col = paste0(cols[1], 60), lwd = lwdStage)

# Early marine
segments(x0 = as.Date(paste0("1999-", findInterval(0.975, cumsum(timing_all$ocean_entry)/sum(timing_all$ocean_entry))), format = "%Y-%j"), 
         x1 = as.Date(paste0("1999-", findInterval(0.975, cumsum(timing_all$ocean_entry)/sum(timing_all$ocean_entry))), format = "%Y-%j") + 365/2,
         y0 = 4.5, y1 = 4.5, col = paste0(cols[2], 60), lwd = lwdStage)

# Marine rearing
segments(x0 = xDate[1], 
         x1 = max(xDate), 
         y0 = 3.5, y1 = 3.5, col = cols[2], lwd = lwdStage)

# FW adult migratoin
segments(x0 = as.Date(paste0("1999-", findInterval(0.025, cumsum(timing_all$run_time)/sum(timing_all$run_time))), format = "%Y-%j"), 
         x1 = as.Date(paste0("1999-", findInterval(0.025, cumsum(timing_all$spawn_time)/sum(timing_all$spawn_time))), format = "%Y-%j"), 
         y0 = 2.5, y1 = 2.5, col = cols[3], lwd = lwdStage)

# Spawning
segments(x0 = as.Date(paste0("1999-", findInterval(0.025, cumsum(timing_all$spawn_time)/sum(timing_all$spawn_time))), format = "%Y-%j"), 
         x1 = as.Date(paste0("1999-", findInterval(0.975, cumsum(timing_all$spawn_time)/sum(timing_all$spawn_time))), format = "%Y-%j"), 
         y0 = 1.5, y1 = 1.5, col = cols[4], lwd = lwdStage)

abline(h = 7)


