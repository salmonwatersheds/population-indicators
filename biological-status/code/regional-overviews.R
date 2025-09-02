###############################################################################
#
# Code to merge trends of regional salmon populations and Conservation Units
# and transform abundances to per cent anomalies for Regional Overviews.
#
###############################################################################


library(dplyr)

# Create Dropbox paths
source("code/ignore/set_dropbox.R") 
# Dropbox_dir = path to population-indicators in Dropbox
# sos_dir = path to state-of-salmon in Dropbox

# Function to import data from database
source("code/functions_general.R")

###############################################################################
# Read in data
###############################################################################


cu_abund <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset1cu_output")
cu_smoothed <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset103_output")
cu_smoothed$year <- as.integer(cu_smoothed$year)

regional_abund <- read.csv(paste0(sos_dir, "/output/sps-trends_plotting.csv"))

regions <- unique(regional_abund$region)

###############################################################################
# Transform cu_abund
###############################################################################

# Calculate long-term average spawner abundance
cu_avg <- cu_abund %>%
  group_by(cuid) %>%
  summarise(
    cuid = unique(cuid),
    avg = exp(mean(log(estimated_count + 1), na.rm = TRUE)))

# Combine datasets
cu_combined <- cu_smoothed %>%
  mutate(smoothed_abund = exp(avg_escape_log)) %>%
  left_join(cu_avg) %>%
  mutate(perc_anom = round((smoothed_abund - avg)/avg*100, 1)) %>%
  rename("species" = species_name)

###############################################################################
# Create Regional Overview dataset
###############################################################################

ro_dat <- cu_combined %>%
  mutate(scale = "ConservationUnit") %>%
  select(scale, region, species, cuid, cu_name_pse, year, perc_anom) %>%
  bind_rows(regional_abund %>%
              mutate(scale = "Regional",
                     cuid = NA,
                     cu_name_pse = NA) %>%
              rename("perc_anom" = spawners) %>%
              select(scale, region, species, cuid, cu_name_pse, year, perc_anom)
  ) %>%
  arrange(match(region, regions), species)

write.csv(ro_dat, paste(Dropbox_dir, "spawner-abundance/output/ro-trends.csv", sep = "/"), row.names = FALSE)
write.csv(ro_dat, paste0(Dropbox_dir, "/spawner-abundance/output/archive/spawner-abundance/output/ro-trends", Sys.Date(), ".csv"), row.names = FALSE)            

###############################################################################
# Plot
###############################################################################

r <- 5 # Select region
s <- 8 # Select species

pdf(file = paste(Dropbox_dir, "spawner-abundance/output/ro-trends.pdf", sep = "/"), width = 7, height = 5, pointsize = 12)
for(r in 1:10){
  for(s in 1:6){

    region.r <- regions[r]
species.s <- species[s]
# ro_dat.rs <- ro_dat %>% filter(region == region[r], species == species[s]) # DOESN'T WORK!!!

ro_dat.rs <- ro_dat %>% filter(region == region.r, species == species.s) # DOESN'T WORK!!!

if(nrow(ro_dat.rs) > 0){

cuids <- unique(ro_dat.rs$cuid)
cuids <- cuids[!is.na(cuids)]

plot(range(ro_dat.rs$year), range(ro_dat.rs$perc_anom, na.rm = TRUE), "n", bty = "l", las = 1, xlab = "", ylab = "Spawner abundance (% anomaly)", ylim = c(-100, 200))
abline(h = 0, lty = 2, col = grey(0.8), lwd = 0.8)

lines(ro_dat.rs$year[ro_dat.rs$scale == "Regional"], ro_dat.rs$perc_anom[ro_dat.rs$scale == "Regional"], lwd = 2)

for(i in 1:length(cuids)){
  lines(ro_dat.rs$year[ro_dat.rs$cuid == cuids[i]], ro_dat.rs$perc_anom[ro_dat.rs$cuid == cuids[i]], col = "#00000030")
  
}
mtext(side = 3, paste(regions[r], species[s]))

}}}
dev.off()