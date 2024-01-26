###############################################################################
# How can we sort spawner surveys across species so that the most abundant salmon 
# rivers float to the top in the All Species view of PSE 2.0?
# January 25, 2024
# Steph Peacock
###############################################################################


library(dplyr)

#------------------------------------------------------------------------------
# Load spawner survey data
#------------------------------------------------------------------------------

# Load functions to pull data from database
source("code/functions_general.R") 

# Pull current spawner survey data from the database and filter VIMI
spawners <- retrieve_data_from_PSF_databse_fun(name_dataset = "Appdata.vwdl_streamspawnersurveys_output") %>%
  filter(region == "Vancouver Island & Mainland Inlets")

# Remove NAs
spawners <- filter(spawners, stream_observed_count != -989898 & year != -989898)

# Number of unique streams 
n.streams <- length(unique(spawners$stream_name_pse))
stream_names <- unique(spawners$stream_name_pse)

# Unique species codes
unique(spawners$species_name)
spawners$species_pooled <- spawners$species_name
spawners$species_pooled[spawners$species_name %in% c("Lake sockeye", "River sockeye")] <- "Sockeye"
spawners$species_pooled[spawners$species_name %in% c("Pink (even)", "Pink (odd)")] <- "Pink"
species_names <- unique(spawners$species_pooled)

#------------------------------------------------------------------------------
# Geometric mean function
#------------------------------------------------------------------------------
gmean = function(
    x, # Vector of data to calculate gmean of (e.g., spawner abundances) 
    na.rm = TRUE, # Remove NAs?
    zero.bump = TRUE # Add a small number to avoid any zeros leading to gmean = 0?
    ){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.bump){
    x <- x + 0.001
    
  } 
  
  return(exp(mean(log(x), na.rm = na.rm)))
}
#------------------------------------------------------------------------------
# Calculate average abundance by species-stream
#------------------------------------------------------------------------------

# First Q: Do we ignore not present species or count those as zero??
avg_spawners <- array(0, dim = c(n.streams, 6), dimnames = list(stream_names, species_names))
for(s in 1:6){
  spawners.s <- filter(spawners, species_pooled == species_names[s])
  avg_spawners.s <- tapply(spawners.s$stream_observed_count, spawners.s$stream_name_pse, gmean, na.rm = TRUE)
  
  avg_spawners[match(names(avg_spawners.s), stream_names), s] <- as.numeric(avg_spawners.s)
}

# Sort by total abundance
tot_spawners <- apply(avg_spawners, 1, sum)
length(tot_spawners)
sum(is.na(tot_spawners))

avg_spawners <- avg_spawners[order(tot_spawners, decreasing = TRUE), ]

sp_cols <- c(wesanderson::wes_palette("Darjeeling1"), grey(0.7))
names(sp_cols) <- species_names

par(mar = c(4, 10, 1, 2))
bp <- barplot(t(avg_spawners[c(30:1),]*10^-3), col = sp_cols, horiz = TRUE, border = NA, xlab = "Spawners (thousands)", las = 1, cex.names = 0.8)

text(sort(tot_spawners, decreasing = TRUE)[1:30]*10^-3, rev(bp), round(sort(tot_spawners, decreasing = TRUE)[1:30]*10^-3), pos = 4, cex = 0.8, xpd = NA, font = 2)
legend("bottomright", fill = sp_cols, legend = species_names, border = NA, bty = "n")


#------------------------------------------------------------------------------
# Abundance in most recent year
#------------------------------------------------------------------------------

# First Q: Do we ignore not present species or count those as zero??
recent_spawners <- array(0, dim = c(n.streams, 6), dimnames = list(stream_names, species_names))
recent_spawners_yr <- array(NA, dim = c(n.streams, 6), dimnames = list(stream_names, species_names))
for(s in 1:6){
  spawners.s <- filter(spawners, species_pooled == species_names[s])
  
  # Order by decreasing year
  spawners.s <- spawners.s[order(spawners.s$year, decreasing = TRUE), ]
  recent_spawners.s <- tapply(spawners.s$stream_observed_count, spawners.s$stream_name_pse, head, n = 1)
  
  
  recent_spawners[match(names(recent_spawners.s), stream_names), s] <- as.numeric(recent_spawners.s)
  recent_spawners_yr[match(names(recent_spawners.s), stream_names), s] <- tapply(spawners.s$year, spawners.s$stream_name_pse, head, n = 1)
}


# Sort by total abundance
tot_recent_spawners <- apply(recent_spawners, 1, sum)
length(tot_recent_spawners)
sum(is.na(tot_recent_spawners))

recent_spawners <- recent_spawners[order(tot_recent_spawners, decreasing = TRUE), ]
recent_spawners_yr <- recent_spawners_yr[order(tot_recent_spawners, decreasing = TRUE), ]
  
par(mar = c(4, 10, 1, 2))
bp <- barplot(t(recent_spawners[c(30:1),]*10^-3), col = sp_cols, horiz = TRUE, border = NA, xlab = "Spawners (thousands)", las = 1, cex.names = 0.8)

text(sort(tot_recent_spawners, decreasing = TRUE)[1:30]*10^-3, rev(bp), round(sort(tot_recent_spawners, decreasing = TRUE)[1:30]*10^-3), pos = 4, cex = 0.8, xpd = NA, font = 2)
legend("bottomright", fill = sp_cols, legend = species_names, border = NA, bty = "n")

# How current are the data?
plot(range(recent_spawners_yr[1:30,], na.rm = TRUE), y = c(1, 30), "n", bty = "n", xlab = "Year of most recent spawner estimate", ylab = "", yaxt = "n")
axis(side = 2, las = 1, at = c(30:1), dimnames(recent_spawners)[[1]][1:30], cex.axis = 0.8)
abline(h = 1:30, lty = 3)
abline(v = seq(1960, 2022, 5), col = grey(0.8))
for(s in 1:6){
  points(recent_spawners_yr[1:30, s], jitter(1:30), pch = 19, col = sp_cols[s], cex = 1.2)
}

#------------------------------------------------------------------------------
# Within species ranking
#------------------------------------------------------------------------------

rank_spawners <- array(0, dim = c(n.streams, 6), dimnames = list(stream_names, species_names))

for(s in 1:6){
  ordered_streams.s <- dimnames(avg_spawners)[[1]][order(avg_spawners[, s], decreasing = TRUE)]
  rank_spawners[match(ordered_streams.s, stream_names), s] <- c(1:length(ordered_streams.s))
}

tot_rank <- apply(rank_spawners, 1, sum)
ordered_rank <- names(tot_rank)[order(tot_rank, decreasing = FALSE)]
rank_spawners <- rank_spawners[order(tot_rank, decreasing = FALSE), ]

# Plot
plot(c(1,6), c(1,30), "n", xlim = c(0.5, 6.5), xaxs = "i", ylab = "", yaxt = "n", xlab = "Species rank", xaxt = "n", bty = "l")
axis(side = 2, las = 1, at = c(1:30), ordered_rank[30:1], cex.axis = 0.8)
axis(side = 1, at = c(1:6), species_names)
abline(h = seq(1.5, 30.5, 1), col = grey(0.8))
abline(v = seq(1.5, 6.5, 1), col = grey(0.8))
for(s in 1:6){
  text(s, 30:1, rank_spawners[1:30, s], col = sp_cols[s], font = 2)
}
    

#------------------------------------------------------------------------------
# Comparing top 30 streams for each approach
#------------------------------------------------------------------------------

write.csv(cbind(avg = dimnames(avg_spawners[c(1:30),])[[1]],
      recent = dimnames(recent_spawners[c(1:30),])[[1]],
      rank = dimnames(rank_spawners[c(1:30),])[[1]]),
      file = "~/Desktop/sortspanwers.csv")

plot(spawners$year[spawners$stream_name_pse == "SQUAMISH RIVER"], log(spawners$stream_observed_count[spawners$stream_name_pse == "SQUAMISH RIVER"]), col = sp_cols[spawners$species_pooled[spawners$stream_name_pse == "SQUAMISH RIVER"]], pch = 19, cex = 1.2, bty = "l", xlab = "", ylab = "logged spawner abundance", las = 1)
legend("bottomleft", pch = 19, col = sp_cols, legend = species_names)
