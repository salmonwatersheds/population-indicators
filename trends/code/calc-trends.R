###############################################################################
# Calculation of long-term and 3-generation trends in CU-level spawner abundance
# 
#' Inputs: 
#' - cuspawnerabundance.csv                          # CU-level spawner abundance data
#' - conservationunits_decoder.csv                   # 
#' 
#'  
#' Outputs: 
#' - dataset103_log_smoothed_spawners_YYY-MM-DD.csv  # the smoothed log-transformed spawner abundance data
#' - dataset202_allgen_trends_YYY-MM-DD.csv          # the trend over the entire time series
#' - dataset391_threegen_trends_YYY-MM-DD.csv        # the trend over the last three generations
#
# Related Tech-Report documentation:
# https://bookdown.org/salmonwatersheds/state-of-salmon/methods-results.html#25_Quantifying_change
#
###############################################################################


# Steph's notes
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1713593152783499
# https://www.dropbox.com/scl/fo/w0wpfhaoa5hd0tb3lcbnb/ANKzPkbsunvofXav50moBHc?rlkey=aub20ix3s83j5unc6bnj4p0kc&st=qenwuo4m&dl=0
# https://salmonwatersheds.slack.com/archives/C01D2S4PRC2/p1713592075622849

# reset the wd to head using the location of the current script
path <- rstudioapi::getActiveDocumentContext()$path
dirhead <- "population-indicators"
path_ahead <- sub(pattern = paste0("\\",dirhead,".*"),replacement = "", x = path)
wd_head <- paste0(path_ahead,dirhead)
setwd(wd_head)

# Now import functions related to directories.
# Note that the script cannot be called again once the directory is set to the 
# subdirectory of the project (unless setwd() is called again).
source("code/functions_set_wd.R")
source("code/functions_general.R")

# return the name of the directories for the different projects:
subDir_projects <- subDir_projects_fun()

wds_l <- set_working_directories_fun(subDir = subDir_projects$trends,
                                     Export_locally = F)
wd_head <- wds_l$wd_head
wd_project <- wds_l$wd_project
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS

wd_references_dropbox <- paste(wd_X_Drive1_PROJECTS,
                               wds_l$wd_project_dropbox,
                               "references",sep="/")

wd_data_dropbox <- paste(wd_X_Drive1_PROJECTS,
                         wds_l$wd_project_dropbox,
                         "data",sep="/")

wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# Loading packages & functions
library(dplyr)
library(zoo) # for rollmean function

# source("code/functions.R") # note used

figure_print <- T

#
# Import datasets --------
#

# Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- F

#'* Import cuspawnerabundance:  CU-level spawner abundance data *
spawners <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[2],
                                  fromDatabase = fromDatabase,
                                  update_file_csv = update_file_csv,
                                  wd = wd_pop_indic_data_input_dropbox)

spawners$estimated_count[which(spawners$estimated_count == -989898)] <- NA

# Filter out no estimated spawner abundance
spawners <- spawners %>% filter(!is.na(estimated_count))


#'* Import conservationunits_decoder.csv *
# CU list (includes gen length info for running avg) (conservationunits_decoder.csv)
# cu_decoder <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_conservationunits_decoder") %>%
#   select(region, species_abbr, pooledcuid, cuid, cu_name_pse, gen_length)
cu_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                 fromDatabase = fromDatabase,
                                 update_file_csv = update_file_csv,
                                 wd = wd_pop_indic_data_input_dropbox)

cu_decoder <- cu_decoder  %>%
  select(region, species_abbr, pooledcuid, cuid, cu_name_pse, gen_length)


#
# Fill datasets and produce figures ------------
#

cuid <- unique(spawners$cuid)

species_name <- spawners$species_name[match(cuid, spawners$cuid)]
# TEMPORARY
# simplify species_name as in PSE data meeting December 11 2024
# To remove eventually when conservationunits_decoder$species_name 
# changed as well.
species_name <- sapply(species_name, FUN = function(sp){
  out <- sp
  if(grepl("[s|S]ockeye",sp)){
    out <- "Sockeye"
  }else if(grepl("Pink",sp)){
    out <- "Pink"
  }
  return(out)
})

cu_list <- data.frame(
  region = cu_decoder$region[match(cuid, cu_decoder$pooledcuid)],
  species_name = species_name,
  species_qualified = cu_decoder$species_abbr[match(cuid, cu_decoder$pooledcuid)],
  cuid = cuid,
  cu_name_pse = cu_decoder$cu_name_pse[match(cuid, cu_decoder$pooledcuid)]
)

# Add year range
cu_list <- cu_list %>% 
  left_join(
    spawners %>% 
    select(cuid, year, estimated_count) %>%
    filter(!is.na(estimated_count)) %>%
    group_by(cuid) %>%
    summarise(first_year = min(year), last_year = max(year))
)

dataset103_output_new <- dataset202_output_new <- dataset391_output_new <- NULL

scale_log <- T # for the figures

for(i in 1:nrow(cu_list)){
  # i <- 8
  # i <- 112
  # i <- which(cu_list$species %in% c("PKE","SER"))[1]
  # i <- which(cu_list$species_abbr == "CO" & cu_list$cu_name_pse == "North Thompson")
  # i <- which(cu_list$cuid == 734)
  # i <- which(grepl("Pink",cu_list$species_name))[1]
  
  region <- cu_list$region[i]
  species_name <- cu_list$species_name[i]
  species_qualified <- cu_list$species_qualified[i]
  cuid <- cu_list$cuid[i]
  cu_name_pse <- cu_list$cu_name_pse[i]
  
  spawners.i <- subset(spawners, cuid == cu_list$cuid[i])
  
  # Ensure dataframe is in increasing year with no missing year
  x <- cu_list$first_year[i]:cu_list$last_year[i]
  y <- rep(NA, length(x))
  y[match(spawners.i$year, x)] <- spawners.i$estimated_count
  
  #'* Remove NAs that are the head of the time series * # this is just in case
  while(is.na(y[1])){
    y <- y[-1]
    x <- x[-1]
  }
  
  #'* Calculate long-term trends *
  
  # log transform (and deal with 0s)
  y[y == 0 & !is.na(y)] <- 0.01
  # y_log <- log(y + 0.01)
  y_log <- log(y)
  
  # get generation length
  g <- cu_decoder$gen_length[cu_decoder$cuid == cuid]
  
  # Smooth spawner abundance using running mean
  y_log_smooth <- rollapply(
    data = y_log, FUN = mean, width = g,
    # na.pad = TRUE, # deprecated. Use fill = NA instead of na.pad = TRUE
    na.rm = T, 
    fill = NA,
    align = "right") #
  
  #' For pink salmon only, prevent the 1st value in the time series to be NA when 
  #' the 1st year is odd for PKO and even for PKE to avoid a bug with the PSE (it
  #'is alway NA in the 1st year of the series, which is and odd year for PKE and 
  #' even for PKO)
  #' cf. PSE Data Check-In Meeting Notes - August 15 2024
  #' https://docs.google.com/document/d/12viWlyqX1FfJewUgbPAZOGrwoUT15cIN0WMt2pBOyx0/edit?usp=sharing
  #' https://salmonwatersheds.slack.com/archives/C01D2S4PRC2/p1723671105718289
  #' https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1723830003333579
  # For pink we do not use smooth line
  cond_pink <- grepl("Pink",species_name)
  if(cond_pink){
    y_log_smooth <- y_log
  }
  
  # Fill  dataset103_output (the smoothed log time series)
  dataset103_output_here <- data.frame(region = rep(region,length(x)))
  dataset103_output_here$species_name <- species_name
  dataset103_output_here$species_qualified <- species_qualified
  dataset103_output_here$cuid <- cuid
  dataset103_output_here$cu_name_pse <- cu_name_pse
  dataset103_output_here$year <- x
  dataset103_output_here$avg_escape_log <- y_log_smooth
  
  if(is.null(dataset103_output_new)){
    dataset103_output_new <- dataset103_output_here
  }else{
    dataset103_output_new <- rbind(dataset103_output_new,dataset103_output_here)
  }
  
  # Fit linear model
  y_LT <- y_log_smooth[!is.na(y_log_smooth)] # to trouble shoot lm_LT
  x_LT <- x[!is.na(y_log_smooth)]
  lm_LT <- lm(y_LT ~ x_LT)
  
  # percent_change
  year.span <- c(min(x[!is.na(y_log_smooth)]):max(x[!is.na(y_log_smooth)]))
  percent_change_total <- exp(lm_LT$coefficients[2] * (length(year.span) - 1)) - 1  # total % change in time period
  percent_change_total <- round(percent_change_total * 100,1)
  percent_change <- exp(lm_LT$coefficients[2]) - 1                              # % change / yr
  percent_change_2dec <- round(percent_change * 100,2)   # for the figure
  percent_change <- round(percent_change * 100,1)
  
  # Fill dataset202_output (LT trend dataset)
  dataset202_output_here <- data.frame(region = region)
  dataset202_output_here$species_name <- species_name
  dataset202_output_here$species_qualified <- species_qualified
  dataset202_output_here$cuid <- cuid
  dataset202_output_here$cu_name_pse <- cu_name_pse
  dataset202_output_here$percent_change <- percent_change              # CHANGE TO rate_change ?
  dataset202_output_here$percent_change_total <- percent_change_total
  dataset202_output_here$slope <- round(lm_LT$coefficients[2],6)
  dataset202_output_here$intercept <- round(lm_LT$coefficients["(Intercept)"],3)
  dataset202_output_here$intercept_start_yr <- predict(lm_LT,newdata = data.frame(x_LT = x_LT[1])) |> round(3) # intercept 1st year of the smoothed data
  dataset202_output_here$start_year <- min(x_LT)
  dataset202_output_here$end_year <- max(x_LT)
  
  if(is.null(dataset202_output_new)){
    dataset202_output_new <- dataset202_output_here
  }else{
    dataset202_output_new <- rbind(dataset202_output_new,dataset202_output_here)
  }
  
  #'* Calculate last 3 generations trends *
  
  x3g <- tail(x, g*3)
  # x3g <- tail(x[!is.na(y_log_smooth)], g*3)
  # x3g <- x3g[!is.na(y_log_smooth[x %in% x3g])]
  # lm_3g <- lm(tail(y_log_smooth[!is.na(y_log_smooth)], g*3) ~ x3g, na.action = "na.exclude") WRONG
  lm_3g <- lm(tail(y_log_smooth, g*3) ~ x3g, na.action = "na.exclude")
  
  if(cond_pink){
    if(species_qualified == "PKO"){
      cond_keep <- x3g %% 2 == 1
    }else{
      cond_keep <- x3g %% 2 == 0
    }
    x3g <- x3g[cond_keep]
    lm_3g <- lm(tail(y_log_smooth, g*3)[cond_keep] ~ x3g, na.action = "na.exclude")
  }
  
  #' Exclusion rule:
  #' If the number of NAs in x3g is > g --> we do not calculate the trend
  #' (i.e. we need at least 66.66% of data points to calculate the trend)
  cond_noEnoughData <- sum(is.na(tail(y_log, g*3))) > g
  if(grepl("Pink",species_name)){ # for pink salmon
    cond_noEnoughData <- sum(is.na(tail(y_log, g*3))) > 3 + 1  # because g = 2 and there is 3 NAs in a complete series
  }
  if(cond_noEnoughData){
    threegen_percent_change <- NA
    threegen_percent_change_total <- NA
    threegen_slope <- NA
    threegen_intercept <- NA
    threegen_intercept_start_yr <- NA
  }else{
    threegen_percent_change_total <- exp(lm_3g$coefficients[2] * (3 * g - 1)) - 1 # total change
    threegen_percent_change_total <- round(threegen_percent_change_total * 100,1)
    threegen_percent_change <- exp(lm_3g$coefficients[2]) - 1     # % change / yr
    threegen_percent_change_2dec <- round(threegen_percent_change * 100, 2)    # for the figure
    threegen_percent_change <- round(threegen_percent_change * 100,1)
    threegen_slope <- round(lm_3g$coefficients[2],6)
    threegen_intercept <- round(lm_3g$coefficients["(Intercept)"],3)
    threegen_intercept_start_yr <- predict(lm_3g, newdata = data.frame(x3g = x3g[1])) |> round(3)
  }
  
  # Fill dataset391_output
  dataset391_output_here <- data.frame(region = region)
  dataset391_output_here$region <- region
  dataset391_output_here$species_name <- species_name
  dataset391_output_here$species_qualified <- species_qualified
  dataset391_output_here$cuid <- cuid
  dataset391_output_here$cu_name_pse <- cu_name_pse
  dataset391_output_here$threegen_percent_change <- threegen_percent_change             # CHANGE TO threegen_rate_change ?
  dataset391_output_here$threegen_percent_change_total <- threegen_percent_change_total
  dataset391_output_here$threegen_slope <- threegen_slope
  dataset391_output_here$threegen_intercept <- threegen_intercept
  dataset391_output_here$threegen_intercept_start_yr <- threegen_intercept_start_yr
  dataset391_output_here$threegen_start_year <- min(x3g)
  dataset391_output_here$threegen_end_year <- max(x3g)
  dataset391_output_here$uploadid <- NA   # QUESTION: to remove?
  
  if(is.null(dataset391_output_new)){
    dataset391_output_new <- dataset391_output_here
  }else{
    dataset391_output_new <- rbind(dataset391_output_new, dataset391_output_here)
  }
  
  #'* Figure *
  
  region_here <- region
  if(region_here ==  "Vancouver Island & Mainland Inlets"){
    region_here <- "VIMI"
  }
  main <- paste(region_here,species_qualified,cuid,cu_name_pse,sep = "_")
  main <- gsub("/",".",main)
  main <- gsub(" ","_",main)
  
  if(figure_print){
    jpeg(filename = paste0(wd_figures,"/",main,".jpeg"),units = "cm", res = 300, 
         width = 20, height = 15)
  }
  
  main <- paste(region_here,species_name,cuid,cu_name_pse,sep = " - ")
  
  y_here <- y_log
  ylab <- "Spawner abundance (log)"
  if(!scale_log){
    y_here <- exp(y_here)
    ylab <- "Spawner abundance"
  }
  
  y_min <- min(y_here, na.rm = T)
  y_max <- max(y_here, na.rm = T)
  y_max <- y_max + (y_max - y_min) / 10 
  
  plot(x = x, y = y_here, col = grey(0.5), lwd = 2, pch = 16, main = main, las = 1,
       xlab = "Year", ylab = ylab, ylim = c(y_min,y_max))
  lines(x[!is.na(y_here)], y_here[!is.na(y_here)],lwd = 2, col = grey(0.5))
  
  # plot smoothed line
  y_here <- y_log_smooth
  if(!scale_log){
    y_here <- exp(y_here)
  }
  if(!cond_pink){
    lines(x, y_here, lwd = 2, col = "black")
  }
  
  # points(x[!is.na(y)], y_log_smooth[!is.na(y)], lwd = 2, col = "black", pch = 1)
  
  # plot regression line for the 3 generation:
  # - if not enough data points:
  if(cond_noEnoughData){
    legend("top","NOT ENOUGH DATA",text.col = "blue",bty = "n")
  }else{ # if enough datapoints
    y_here <- predict(lm_3g, newdata = data.frame(x3g))
    if(!scale_log){
      y_here <- exp(y_here)
    }
    lines(x3g, y_here, lwd = 2, col = "blue")
  }
  
  # plot regression line for LT
  y_here <- predict(lm_LT, newdata = data.frame(x[!is.na(y_log_smooth)]))
  if(!scale_log){
    y_here <- exp(y_here)
  }
  lines(x[!is.na(y_log_smooth)], y_here, lwd = 2, col = "red", lty = 2)
  # legend("bottomright",paste0("i = ",i),bty = "n")
  
  legend("topright",paste0(c(percent_change_2dec,threegen_percent_change_2dec),"% / year"), 
         bty = "n", text.col = c("red","blue"))
  #
  if(figure_print){
    dev.off()
  }
}

head(cu_list)

head(dataset103_output_new)
head(dataset202_output_new)
head(dataset391_output_new)

# Check that there is no duplicated rows:
cond <- dataset391_output_new$region == "Transboundary"
dataset391_output_new[cond,]

#
# Remove VIMI SH South Coast Winter 980 TEMPORARY --------
#' cf. PSE Data Check-In Meeting Notes - 19-09-2024
#' These CUs should be removed because the only population for which there is data
#' is not representative of the whole CU
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1726265990287849?thread_ts=1726158954.361189&cid=CJ5RVHVCG
#' Eric will edit the code so the estimated spawner abundance will not be calculated
#' and so the filter will happen automatically. But for now we do it manually.

cond <- dataset103_output_new$cuid == 980
sum(cond) # 25
dataset103_output_new <- dataset103_output_new[!cond,]

cond <- dataset202_output_new$cuid == 980
sum(cond) # 1
dataset202_output_new <- dataset202_output_new[!cond,]

cond <- dataset391_output_new$cuid == 980
sum(cond) # 1
dataset391_output_new <- dataset391_output_new[!cond,]

#
# Export the files ------
#
date <- as.character(Sys.Date())

# Export in /output/archive folder on dropbox
write.csv(dataset103_output_new,paste0(wd_output,"/archive/dataset103_log_smoothed_spawners_",date,".csv"),
          row.names = F)
write.csv(dataset202_output_new,paste0(wd_output,"/archive/dataset202_allgen_trends_",date,".csv"),
          row.names = F)
write.csv(dataset391_output_new,paste0(wd_output,"/archive/dataset391_threegen_trends_",date,".csv"),
          row.names = F)

# Export in /output folder on dropbox NOT NEEDED
# write.csv(dataset103_output_new,paste0(wd_output,"/dataset103_output.csv"),
#           row.names = F)
# write.csv(dataset202_output_new,paste0(wd_output,"/dataset202_output.csv"),
#           row.names = F)
# write.csv(dataset391_output_new,paste0(wd_output,"/dataset391_output.csv"),
#           row.names = F)

# Export in /output locally to push to github 
write.csv(dataset103_output_new,
          paste0(getwd(),"/output/dataset103_log_smoothed_spawners.csv"),
          row.names = F)
write.csv(dataset202_output_new,
          paste0(getwd(),"/output/dataset202_allgen_trends.csv"),
          row.names = F)
write.csv(dataset391_output_new,
          paste0(getwd(),"/output/dataset391_threegen_trends.csv"),
          row.names = F)

# QUESTION:
# cases with the 3 gen length regression line > total length: i = 233

# Old notes:

# QUESTION: the smooth function does not calculate value if there are NAs in the 
# width --> loss of a lot of information. see ex. i = 2. Can we fill the NAs?
# Or can we not use the smooth function in this case then?
# Other potential solution: fill NAs by predicting the values with a smoothing spline.
# https://stackoverflow.com/questions/18695335/replacing-all-nas-with-smoothing-spline
# If not, may be just use it for Pink only? I am not sure what is done originally
# (the code in the Rmd script is unclear (731 and after)).

# Checks related to this thread:
# https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1722278192424409?thread_ts=1719268108.787009&cid=C03LB7KM6JK


threegen_slope
threegen_intercept

threegen_slope <- round(lm_3g$coefficients[2],3)
threegen_intercept <- round(lm_3g$coefficients["(Intercept)"],1)

slope <- round(lm_LT$coefficients[2],3)
intercept <- round(lm_LT$coefficients["(Intercept)"],1)

predict(lm_3g, newdata = data.frame(x3g))

lines(x = x, y = exp(intercept + slope * x), col = "green")
lines(x = x, y = exp(intercept + slope * x), col = "green")

lines(x = x, y = exp(predict(lm_3g, newdata = data.frame(x3g = x))), 
      col = "red")
lines(x = x, y = exp(predict(lm_LT, newdata = data.frame(x_LT = x))), 
      col = "red")


lines(x = x, y = (predict(lm_3g, newdata = data.frame(x3g = x))), 
      col = "purple")
lines(x = x, y = (predict(lm_LT, newdata = data.frame(x_LT = x))), 
      col = "purple")

predict(lm_LT, newdata = data.frame(x))

# Check issue with PSE displaying trend lines too low ------
# https://salmonwatersheds.slack.com/archives/C03LB7KM6JK/p1722908866770829?thread_ts=1719268108.787009&cid=C03LB7KM6JK

cuid <- 709

pattern <- "dataset103_output"
dataset103_output <- import_mostRecent_file_fun(wd = wd_output, pattern = pattern)

pattern <- "dataset202_output"
dataset202_output <- import_mostRecent_file_fun(wd = wd_output, pattern = pattern)

pattern <- "dataset391_output"
dataset391_output <- import_mostRecent_file_fun(wd = wd_output, pattern = pattern)

# plot spawner data on log scale
cond_cuid <- spawners$cuid == cuid
y <- spawners$estimated_count[cond_cuid]
x <- spawners$year[cond_cuid]

plot(x = x, y = log(y), pch = 16, col = "grey50")
lines(x = x, y = log(y), lwd = 1.5, col = "grey50")

# plot smoothed trend
cond_cuid <- dataset103_output$cuid == cuid
y <- dataset103_output$avg_escape_log[cond_cuid]
x <- dataset103_output$year[cond_cuid]
lines(x = x, y = y, lwd = 2, col = "black")

# plot LT trend
years <- dataset103_output$year[cond_cuid]
years <- years[!is.na(dataset103_output$avg_escape_log[cond_cuid])]
cond_cuid <- dataset202_output$cuid == cuid
x <- min(years):max(years)
y <- dataset202_output$slope[cond_cuid] * x + dataset202_output$intercept[cond_cuid]
lines(x = x, y = y, lwd = 2, col = "red", lty = 2)

# Plot 3 gen trend
cond_cuid <- dataset391_output$cuid == cuid
years <- dataset391_output$threegen_start_year[cond_cuid]:max(years)
x <- min(years):max(years)
y <- dataset391_output$threegen_slope[cond_cuid] * x + dataset391_output$threegen_intercept[cond_cuid] 
lines(x = x, y = y, lwd = 2, col = "blue", lty = 2)

lines(x3g, lm_3g$coefficients[2] * x3g + (lm_3g$coefficients["(Intercept)"] |> round(1)),
      lwd = 2, col = "purple", lty = 2)


lm_3g$coefficients[2] * x3g + (lm_3g$coefficients["(Intercept)"] |> round(1))
lm_3g$coefficients[2] * x3g + lm_3g$coefficients["(Intercept)"]
0.1462081 * x + -285.6122


dataset391_output$threegen_slope[cond_cuid] * x + dataset391_output$threegen_intercept[cond_cuid] 


0.1462081 * x -285.6122
0.1462081 * x -285.6
0.146 * x -285.6122
0.146 * x + -285.6


0.1462081 * x -285.6
0.146 * x -285.6

0.1462081 * x -285.6
0.146208 * x -285.6

#
# Check previous vs. new trends ------
#

wd <- paste0(wd_output,"/archive")
pattern <- "dataset202"

# Most recent dataset:
trend_allgen <- import_mostRecent_file_fun(wd = wd, pattern = pattern)
nrow(trend_allgen) # 234

# Second most recent dataset:
files_c <- list.files(wd)
files_c <- files_c[grepl(x = files_c, pattern = pattern)]
file.mtime <- file.mtime(paste(wd,files_c,sep="/"))
files_c <- files_c[order(file.mtime)] |> rev()
file <- files_c[2]
print(paste0("File imported: ",file," ; Date modified: ", max(file.mtime)))


trend_allgen_old <- read.csv(paste0(wd_output,"/archive/",file),header = T)
head(trend_allgen_old)
nrow(trend_allgen_old) # 232

#
identical(trend_allgen,trend_allgen_old)

data_compare <- NULL
for(r in 1:nrow(trend_allgen)){
  cuid <- trend_allgen$cuid[r]
  cond_old <- trend_allgen_old$cuid == cuid
  
  toAdd <- F
  
  if(!any(cond_old)){
    trend_allgen_old_here <- trend_allgen[r,]
    trend_allgen_old_here$percent_change <- NA
    trend_allgen_old_here$percent_change_total <- NA
    trend_allgen_old_here$slope <- NA
    trend_allgen_old_here$intercept <- NA
    trend_allgen_old_here$intercept_start_yr <- NA
    trend_allgen_old_here$start_year <- NA
    trend_allgen_old_here$end_year <- NA
    data_here <- rbind(trend_allgen[r,],trend_allgen_old_here)
    data_here$dataset <- c("new","old")
    toAdd <- T
    
  }else{
    cond_diff <- round(trend_allgen$percent_change[r],1) != round(trend_allgen_old$percent_change[cond_old],1) |
      round(trend_allgen$percent_change_total[r],1) != round(trend_allgen_old$percent_change_total[cond_old],1) |
      round(trend_allgen$slope[r],6) != round(trend_allgen_old$slope[cond_old],6) |
      round(trend_allgen$intercept[r],3) != round(trend_allgen_old$intercept[cond_old],3) |
      trend_allgen$end_year[r] != trend_allgen_old$end_year[cond_old]
    
    if(cond_diff){
      data_here <- rbind(trend_allgen[r,],trend_allgen_old[cond_old,])
      data_here$dataset <- c("new","old")
      toAdd <- T
    }
  }
  
  if(toAdd){
   
    if(is.null(data_compare)){
      data_compare <- data_here
    }else{
      data_compare <- rbind(data_compare,data_here)
    }
  }
}
data_compare
data_compare$cuid |> unique() |> length() # 47
data_compare$region |> unique()
data_compare$species_name |> unique()

cond <- is.na(data_compare$slope)
data_compare[cond,]








