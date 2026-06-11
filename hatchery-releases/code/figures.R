

rm(list = ls())
graphics.off()

source("code/functions_set_wd.R")
source("code/functions_general.R")
source("code/colours.R")

# Find person path to /X Drive/1_PROJECTS
wd_X_Drive1_PROJECTS <- paste0(get_XDrive(),"1_PROJECTS")

wd_hatchery_releases_dropbox <- paste0(wd_X_Drive1_PROJECTS,
                                       "/1_Active/Population Methods and Analysis/population-indicators/hatchery-releases")

wd_spawner_surveys_data <- paste(wd_X_Drive1_PROJECTS,
                                 "1_Active/Population Methods and Analysis/population-indicators/spawner-surveys",
                                 "data",sep="/")

wd_output_dropbox <- paste0(wd_hatchery_releases_dropbox,"/output")

wd_figures_dropbox <- paste0(wd_hatchery_releases_dropbox,"/figures")


# Import the data:
DFO_df_figure <- import_mostRecent_file_fun(wd = paste0(wd_output_dropbox,"/archive"),
                                            pattern = "hatchery_data_all")

head(DFO_df_figure)

#
# Figure Species ------
#

unique(DFO_df_figure$SPECIES_NAME)

colours <- species_cols_light
# colours <- species_cols_dark

years <- min(DFO_df_figure$RELEASE_YEAR):max(DFO_df_figure$RELEASE_YEAR)

data <- tidyversedata <- matrix(NA,nrow = length(colours),
                                ncol = length(years))

rownames(data) <- names(colours)
colnames(data) <- years

for(yr in years){
  # yr <- 1956
  cond_yr <- DFO_df_figure$RELEASE_YEAR == yr
  for(s in names(species_cols_light)){
    # s <- "Chinook
    cond_s <- DFO_df_figure$SPECIES_NAME == s
    if(any(cond_yr & cond_s)){
      data[s,as.character(yr)] <- sum(DFO_df_figure$TotalRelease[cond_yr & cond_s],na.rm = T)
    }else{
      data[s,as.character(yr)] <- 0
    }
  }
}

data <- data / 1000000


svg(filename = paste0(wd_figures_dropbox,"/hatchery-releases-species.svg"),
    width = 14, height = 5)

pdf(file = paste0(wd_figures_dropbox,"/hatchery-releases-species.pdf"),
    width = 14, height = 5)

par(mar = c(5,5,1,.5))

bp <- barplot(data, 
              col = colours, 
              xlab = "Year of release", 
              ylab = "Number of individuals released (in millions)", 
              main = NA, 
              xaxt = "n",
              border = 'black', 
              space = 0)
box(bty = "l")

# 4. Add a legend to identify the segments
legend("topleft", bty = "n",
       legend = rev(names(colours)), 
       fill = rev(colours))

ticks <- which(years %% 10 == 0)
axis(side = 1, at = bp[ticks],labels = years[ticks])

dev.off()

#
# Figure release stages -------
#

unique(DFO_df_figure$RELEASE_STAGE_NAME)
sum(is.na(DFO_df_figure$RELEASE_STAGE_NAME))

unique(DFO_df_figure$release_type_pse)

table(DFO_df_figure$release_type_pse)

nrow(DFO_df_figure) # 37070

years <- min(DFO_df_figure$RELEASE_YEAR):max(DFO_df_figure$RELEASE_YEAR)
# years <- years[105:115]

stages <- c("Egg","Fry","Smolt","Seapen")

all(DFO_df_figure$release_type_pse %in% stages)
all(stages %in% DFO_df_figure$release_type_pse)


data <- tidyversedata <- matrix(NA,nrow = length(stages),
               ncol = length(years))

rownames(data) <- stages
colnames(data) <- years

for(yr in years){
  # yr <- 1956
  cond_yr <- DFO_df_figure$RELEASE_YEAR == yr
  for(s in stages){
    # s <- "Fry"
    cond_s <- DFO_df_figure$release_type_pse == s
    if(any(cond_yr & cond_s)){
      data[s,as.character(yr)] <- sum(DFO_df_figure$TotalRelease[cond_yr & cond_s],na.rm = T)
    }else{
      data[s,as.character(yr)] <- 0
    }
  }
}

colours <- c("#7CE3D8","gainsboro","tomato3","tan2")

graphics.off()

par(mar = c(5,5,1,.5))

bp <- barplot(data, 
              col = colours, 
              xlab = "Year of release", 
              ylab = "Number of individuals released", 
              main = NA, 
              xaxt = "n", 
              space = 0)

box(bty = "l")

# 4. Add a legend to identify the segments
legend("topleft", bty = "n",
       legend = rev(stages), 
       fill = rev(colours))

ticks <- which(years %% 10 == 0)
axis(side = 1, at = bp[ticks],labels = years[ticks])


