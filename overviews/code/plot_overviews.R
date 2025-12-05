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