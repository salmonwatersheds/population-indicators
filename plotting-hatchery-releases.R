###############################################################################
# Hatchery release score visualization
# Request for input from Rheanna
# https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1705539826321119
#
# January 17, 2024
###############################################################################

library(dplyr)

source("code/functions_general.R")

# Read in current hatchery releases data from database
# hr <- retrieve_data_from_PSF_databse_fun(name_dataset = "")
# Nevermind, don't know the table name (if it exists)

# Read in hatchery releases from latest output
# Dataset 384: Hatchery Releases for Salmon Conservation Units
# This dataset provides information on juvenile salmon releases from hatchery facilities in British Columbia. The following parameters are included: (1) program, (2) project, (3) release stage, (4) site name, (5) total releases, (6) broodstock CU, (7) release site CU, and (8) facility id. Source data was provided by Fisheries and Oceans Canada, Salmon Enhancement Program.
# Downloaded Jan 17, 2024 from https://data.salmonwatersheds.ca/result?datasetid=384

hr <- read.csv("hatchery-releases/data/dataset384_hatcheryreleases.csv")

hr_col <- "#499F9F" # Hatchery releases colour of plots from PSE

stages <- unique(hr$release_stage)

hr_pch = c("Smolt 0+" = 6,
           "Smolt 1+" =   2,
           "Fed Fry" =  15,
           "Unfed" = 0,
           "Eyed Egg" = 1,
           "Chan Fry" = 5,
           "Fed Fall" = 19,
           "Seapen 0+" = 3,
           "Seapen 1+" = 4,
           "Seapen" = 8)

hr_palette <- colorRampPalette(c("#FFFFFF", hr_col, "#000000"))(n = length(stages))

#------------------------------------------------------------------------------
# Choose an example site to plot: Big Qualicum River (EVI-GS Coho)
#------------------------------------------------------------------------------


hr1 <- hr[grep("Big Qualicum River", hr$release_site_name), ]

# Different point types
plot(hr1$year, hr1$total_release*10^-6, "n", bty = "l", xlab = "", ylab = "Number released (millions)", las = 1)
for(j in 1:length(stages)){
  hr2 <- hr1[hr1$release_stage == stages[j], ]
  if(dim(hr2)[1] > 5){
    type.j <- "o"
  } else {
   type.j <- "p"
  }
  points(hr2$year, hr2$total_release*10^-6, type = type.j, pch = hr_pch[stages[j]], col = hr_col)
}

tot <- tapply(hr1$total_release, hr1$year, sum)
lines(as.numeric(names(tot)), tot*10^-6, lwd = 4, col = paste0(hr_col, 30))

legend("topright", pch = hr_pch, col = hr_col, legend = names(hr_pch), title = "Release stage")

# Stacked bar
yrs <- as.numeric(names(tot))
yrs.label1 <- seq(1960, 2020, 10)
yrs.label2 <- seq(1960, 2020, 2)

hr1_bar <- matrix(0, nrow = length(tot), ncol = length(stages), dimnames = list(yrs, stages))

for(j in 1:length(stages)){
  hr2 <- hr1[hr1$release_stage == stages[j], ]
  hr1_bar[match(hr2$year, yrs), j] <- hr2$total_release
}

bp <- barplot(t(hr1_bar)*10^-6, col = hr_palette, bty = "l", xlab = "", ylab = "Number released (millions)", las = 1, xaxt = "n", yaxs= "i")
abline(h = 0)
axis(side = 1, at = bp[match(yrs.label1, yrs)], label = yrs.label1, line = 0)
axis(side = 1, at = bp[match(yrs.label2, yrs)], label = FALSE, tck = -0.01)

legend("topright", fill = hr_palette, legend = names(hr_pch), title = "Release stage")
