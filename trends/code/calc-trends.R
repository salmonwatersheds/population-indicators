###############################################################################
# Calculation of long-term and 3-generation trends in CU-level spawner abundance
# 
#' Inputs: 
#' - dataset1cu_output.csv    # = dataset_1part1_DATE.csv
#' - dataset103_output.csv    # just to access the format
#' - dataset202_output.csv    # just to access the format
#' - dataset391_output.csv    # just to access the format
#' 
#' Outputs: 
#' - dataset103_output_DATE.csv    
#' - dataset202_output_DATE.csv    
#' - dataset391_output_DATE.csv    
#
# Related Tech-Report documentation:
# https://bookdown.org/salmonwatersheds/state-of-salmon/methods-results.html#25_Quantifying_change
#
# Stephanie Peacock
# January 17, 2024
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

#
# Import datasets -------- 
#

# Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- update_file_csv <- F


#'* Import dataset1cu_output.csv:  CU-level spawner abundance data *
#' = dataset_1part2_DATE.csv
# spawners <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset1cu_output")
spawners <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[15],
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


#'* Import dataset103_output.csv *
#' To access structure
#' Average Spawners per Generation for Salmon and Steelhead Conservation Units
dataset103_output <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[16],
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)
head(dataset103_output)


#'* Import dataset202_output.csv *
#' To access structure 
#' Trends in Spawner Abundance (All Generations) for Salmon and Steelhead Conservation Units
dataset202_output <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[17],
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)
head(dataset202_output)


#'* Import dataset391_output.csv *
#' To access structure 
#' Trends in Spawner Abundance (Three Generations) for Salmon and Steelhead Conservation Units
dataset391_output <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[10],
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)
head(dataset391_output)


#
# Fill datasets and produce figures ------------
#

cuid <- unique(spawners$cuid)

cu_list <- data.frame(
  region = cu_decoder$region[match(cuid, cu_decoder$pooledcuid)],
  cuid = cuid,
  species_abbr = cu_decoder$species_abbr[match(cuid, cu_decoder$pooledcuid)],
  species_name = spawners$species_name[match(cuid, spawners$cuid)],
  cu_name_pse = cu_decoder$cu_name_pse[match(cuid, cu_decoder$pooledcuid)]
)

# Add year range
cu_list <- cu_list %>% left_join(
  spawners %>% 
    select(cuid, year, estimated_count) %>%
    filter(!is.na(estimated_count)) %>%
    group_by(cuid) %>%
    summarise(first_year = min(year), last_year = max(year))
)


dataset103_output_new <- dataset103_output[NULL,]
dataset202_output_new <- dataset202_output[NULL,]
dataset391_output_new <- dataset391_output[NULL,]

figure_print <- F
scale_log <- T
for(i in 1:nrow(cu_list)){
  # i <- 8
  # i <- 112
  # i <- which(cu_list$species %in% c("PKE","SER"))[1]
  region <- cu_list$region[i]
  species_name <- cu_list$species_name[i]
  cuid <- cu_list$cuid[i]
  cu_name_pse <- cu_list$cu_name_pse[i]
  
  spawners.i <- subset(spawners, cuid == cu_list$cuid[i])
  
  # Ensure dataframe is in increasing year with no missing year
  x <- cu_list$first_year[i]:cu_list$last_year[i]
  y <- rep(NA, length(x))
  y[match(spawners.i$year, x)] <- spawners.i$estimated_count
  
  #** Calculate long-term trends *
  
  # log transform (and deal with 0s)
  y[y == 0 & !is.na(y)] <- 0.01
  # log.y <- log(y + 0.01)
  log.y <- log(y)
  
  # get generation length
  g <- cu_decoder$gen_length[cu_decoder$cuid == cuid]
  
  # Smooth spawner abundance using running mean
  smooth.y <- rollapply(
    data = log.y, FUN = mean, width = g,
    # na.pad = TRUE, # deprecated. Use fill = NA instead of na.pad = TRUE
    na.rm = T, 
    fill = NA,
    align = "right") #
  
  # Fill  dataset103_output 
  dataset103_output_here <- dataset103_output[1:length(x),]
  dataset103_output_here$region <- region
  dataset103_output_here$species_name <- species_name
  dataset103_output_here$cuid <- cuid
  dataset103_output_here$cu_name_pse <- cu_name_pse
  dataset103_output_here$year <- x
  dataset103_output_here$avg_escape_log <- smooth.y
  dataset103_output_new <- rbind(dataset103_output_new,dataset103_output_here)
  
  # Fit linear model
  lm_LT <- lm(smooth.y[!is.na(smooth.y)] ~ x[!is.na(smooth.y)])
  
  # percent_change
  year.span <- c(min(x[!is.na(smooth.y)]):max(x[!is.na(smooth.y)]))
  # percent_change <- exp(lm_LT$coefficients[2] * (length(year.span) - 1)) - 1
  percent_change <- exp(lm_LT$coefficients[2]) - 1                              # to convert to % change / yr
  percent_change_2dec <- round(percent_change * 100,2)
  percent_change <- round(percent_change * 100,1)
  
  # Fill dataset202_output
  dataset202_output_here <- dataset202_output[1,]
  dataset202_output_here$region <- region
  dataset202_output_here$species_name <- species_name
  dataset202_output_here$cuid <- cuid
  dataset202_output_here$cu_name_pse <- cu_name_pse
  dataset202_output_here$percent_change <- percent_change
  dataset202_output_here$slope <- round(lm_LT$coefficients[2],3)
  dataset202_output_here$intercept <- round(lm_LT$coefficients["(Intercept)"],1)
  dataset202_output_new <- rbind(dataset202_output_new,dataset202_output_here)
  
  #'* Calculate last 3 generations trends *
  
  x3g <- tail(x, g*3)
  x3g <- tail(x[!is.na(smooth.y)], g*3)
  lm_3g <- lm(tail(smooth.y[!is.na(smooth.y)], g*3) ~ x3g, na.action = "na.exclude")
  
  #' Exclusion rule:
  #' If the number of NAs in x3g is > g --> we do not calculate the trend
  #' (i.e. we need at least 66.66% of data points to calculate the trend)
  cond_noEnoughData <- sum(is.na(tail(log.y, g*3))) > g
  if(grepl("Pink",species_name)){ # for pink salmon
    cond_noEnoughData <- sum(is.na(tail(log.y, g*3))) > 3 + 1  # because g = 2 and there is 3 NAs in a complete series
  }
  if(cond_noEnoughData){
    threegen_percent_change <- NA
    threegen_slope <- NA
    threegen_intercept <- NA
  }else{
    # threegen_percent_change <- exp(lm_3g$coefficients[2] * (3 * g - 1)) - 1
    threegen_percent_change <- exp(lm_3g$coefficients[2]) - 1     # % change / yr
    threegen_percent_change_2dec <- round(threegen_percent_change * 100, 2)
    threegen_percent_change <- round(threegen_percent_change * 100,1)
    threegen_slope <- round(lm_3g$coefficients[2],3)
    threegen_intercept <- round(lm_3g$coefficients["(Intercept)"],1)
  }
  
  # Fill dataset391_output
  dataset391_output_here <-  dataset391_output[1,]
  dataset391_output_here$region <- region
  dataset391_output_here$species_name <- species_name
  dataset391_output_here$cuid <- cuid
  dataset391_output_here$cu_name_pse <- cu_name_pse
  dataset391_output_here$threegen_percent_change <- threegen_percent_change
  dataset391_output_here$threegen_slope <- threegen_slope
  dataset391_output_here$threegen_intercept <- threegen_intercept
  dataset391_output_here$threegen_start_year <- x3g[1]
  dataset391_output_here$uploadid <- NA   # QUESTION: to remove?
  dataset391_output_new <- rbind( dataset391_output_new, dataset391_output_here)
  
  #'* Figure *
  
  region_here <- region
  if(region_here ==  "Vancouver Island & Mainland Inlets"){
    region_here <- "VIMI"
  }
  main <- paste(region_here,species_name,cuid,cu_name_pse,sep = "_")
  main <- gsub("/",".",main)
  main <- gsub(" ","_",main)
  
  if(figure_print){
    jpeg(filename = paste0(wd_figures,"/",main,".jpeg"),units = "cm", res = 300, 
         width = 20, height = 15)
  }
  
  main <- paste(region_here,species_name,cuid,cu_name_pse,sep = " - ")
  
  y_here <- log.y
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
  y_here <- smooth.y
  if(!scale_log){
    y_here <- exp(y_here)
  }
  lines(x, y_here, lwd = 2, col = "black")
  # points(x[!is.na(y)], smooth.y[!is.na(y)], lwd = 2, col = "black", pch = 1)
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
  y_here <- predict(lm_LT, newdata = data.frame(x[!is.na(smooth.y)]))
  if(!scale_log){
    y_here <- exp(y_here)
  }
  lines(x[!is.na(smooth.y)], y_here, lwd = 2, col = "red", lty = 2)
  legend("bottomright",paste0("i = ",i),bty = "n")
  
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


date <- as.character(Sys.time())
date <- strsplit(x = date, split = " ")[[1]][1]
date <- gsub("-","",date)

write.csv(dataset103_output_new,paste0(wd_output,"/dataset103_output_",date,".csv"),
          row.names = F)
write.csv(dataset202_output_new,paste0(wd_output,"/dataset202_output_",date,".csv"),
          row.names = F)
write.csv(dataset391_output_new,paste0(wd_output,"/dataset391_output_",date,".csv"),
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
