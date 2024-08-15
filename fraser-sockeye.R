###############################################################################
# Compare Fraser sockeye salmon daily passage to run timing in PSE
#
# April 23, 2024
# Steph Peacock
###############################################################################
library(dplyr)
library(stringr)
source("code/functions_general.R")
source("code/colours.R")

###############################################################################
# Read in data
###############################################################################

# Read in data, downloaded from PSC website Apr 23, 2024
# https://www.psc.org/publications/fraser-panel-in-season-information/fish-passage-past-the-psc-hydroacoustic-counting-station-near-mission-bc/

Dropbox_directory <- "/Users/stephaniepeacock/Salmon\ Watersheds\ Dropbox/Stephanie\ Peacock/X\ Drive/1_PROJECTS/1_Active/Population\ Methods\ and\ Analysis/population-indicators"

dat <- readxl::read_xlsx(paste0(Dropbox_directory, "/timing/data/EscxStkGrp.xlsx"), skip = 4)
totals <- apply(dat[, 2:14], 2, sum)

dat_ppn <- data.frame(
  date = dat$Date)
for(i in 2:14){
  dat_ppn <- cbind(dat_ppn, dat[, i]/totals[i-1])
}
names(dat_ppn)[2:14] <- names(dat)[2:14]
sum(dat_ppn[, 10])

head(dat)

pse <- read.csv(paste0(Dropbox_directory, "/timing/output/run-timing_2024-04-17.csv"))
cu_list <- read.csv(paste0(Dropbox_directory, "/data-input/conservationunits_decoder.csv"))

pse <- pse %>% 
  left_join(cu_list %>% select(cuid, cu_name_pse, region, species_abbr)) %>%
  filter(region == "Fraser", species_abbr == "SEL")

write.csv(sort(unique(pse$cu_name_pse)), "~/Desktop/pse_CUs.csv")
write.csv(names(dat_ppn), "~/Desktop/psc_CUs.csv")

date <- as.Date(dat_ppn$date, format = "%Y-%m%-%d UTC")
date_pse <- as.Date(paste(2023, 1:365, sep = "-"), format = "%Y-%j")

# Early Stuart
pse1 <- pse %>% filter(cu_name_pse == "Takla-Trembleur-Early Stuart (cyclic)")
dat_ppn1 <- dat_ppn$`Early Stuart`

plot(date, dat_ppn1, "l", main = "Takla-Trembleur-Early Stuart (cyclic) / PSC: Early Stuart", ylab = "Proportion of run")
lines(date_pse, pse1$run_timing_ppn, col = 2, lwd = 1.5)
legend("topright", col = c(1,2), lty = 1, c("PSC 2023 Mission", "PSE - Wilson"))

# Chilliwack
pse1 <- pse %>% filter(cu_name_pse == "Chilliwack-Early Summer (cyclic)")
dat_ppn1 <- dat_ppn$Chilliwack

plot(date, dat_ppn1, "l", main = "Chilliwack-Early Summer (cyclic) / PSC: Chilliwack", ylab = "Proportion of run")
lines(date_pse, pse1$run_timing_ppn, col = 2, lwd = 1.5)
legend("topright", col = c(1,2), lty = 1, c("PSC 2023 Mission", "PSE - Wilson"))

       