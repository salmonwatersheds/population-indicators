###############################################################################
# Code to estimate Fraser pink salmon mean and sd of run timing from daily passage
#
# September 27, 2023
# Steph Peacock
###############################################################################
library(dplyr)

# Read in data, provided by Hague, Merran <hague@psc.org> on Sept 7, 2023
dat <- read.csv("timing/data/MissionDailyPassage_PinkSalmon_2009_2023.csv")

# Create day-of-year (DOY) variable
dat$DOY <- as.Date(dat$Date) %>%
	strftime(format = "%j") %>%
	as.numeric()

# Sum passage across all years for each DOY
allPassage <- tapply(dat$Passage, dat$DOY, sum, na.rm = TRUE)

# Create dummy variable for summarizing quantiles, mean, and sd of timing
x <- rep.int(as.numeric(names(allPassage)), times = allPassage)

# Quantiles used in CCVA analysis
quantile(x, c(0.025, 0.5, 0.975))

# Mean and sd used in PSE run timing
mean(x)
sd(x)
