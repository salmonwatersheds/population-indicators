
#'******************************************************************************
#' The goal of the script is to analyse the biological status probabilities 
#' obtained from the HBSRM analysis.
#' 
#' Files imported (from ):
#' - region_species_biological_status.csv (created in benchmarks_HBSRM.R)
#' 
#' Files produced: 
#' - Biological_status_HBSRM_all.csv
#' 
#' Notes:
#' - 
#' 
#'******************************************************************************

# 
rm(list = ls())
graphics.off()

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

wds_l <- set_working_directories_fun(subDir = subDir_projects$biological_status,
                                     Export_locally = F)
wd_head <- wds_l$wd_head
wd_code <- wds_l$wd_code
wd_data <- wds_l$wd_data
wd_figures <- wds_l$wd_figures
wd_output <- wds_l$wd_output
wd_X_Drive1_PROJECTS <- wds_l$wd_X_Drive1_PROJECTS
wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# The datasets to input were outputted by other scripts 
wd_data_input <- wd_output

# Import functions for this specific project
source("Code/functions.R")

# Import species names and acronyms
species_acronym_df <- species_acronym_fun()

# Import region names
regions_df <- regions_fun()

#------------------------------------------------------------------------------#
# Analyses
#------------------------------------------------------------------------------#

# select all the regions
region <- as.character(regions_df[1,])

# select certain species
species <- c(species_acronym_df$species_name[species_acronym_df$species_acro == "CK"],    
             species_acronym_df$species_name[species_acronym_df$species_acro == "SX"])

# note that species_all take precedence over species in SRdata_path_species_fun()
species_all <- TRUE

# Import biological status based on HBSRM ------
# Import all the CSV files for each combination of region - species and rbind them
pattern <- "biological_status"
biological_status_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)
#
# write.csv(biological_status_df,paste0(wd_output,"/Biological_status_HBSRM_all.csv"),
#           row.names = F)

head(biological_status_df)
colnames(biological_status_df)
nrow(biological_status_df) # 142
unique(biological_status_df$comment)

printFig <- T

biological_status_compare_fun(biological_status_df = biological_status_df,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "region")

biological_status_compare_fun(biological_status_df = biological_status_df,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "species")

# Import the benchmark values associated with the HBSRM ------
pattern <- "benchmarks_summary"

benchmarks_summary_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                       wd_output = wd_output,
                                                       region = region,
                                                       species_all = F)

head(benchmarks_summary_df)
nrow(unique(benchmarks_summary_df[,c("region","species","CU")])) # 130

benchmarks_summary_df
benchmarks_summary_df_Smsy_HPD <- benchmarks_summary_df[benchmarks_summary_df$benchmark == "Smsy" & benchmarks_summary_df$method == "HPD",]


final_HBSRM <- merge(x = biological_status_df, 
                     y = benchmarks_summary_df_Smsy_HPD[,c("region","species","CU","m")],
                     by = c("region","species","CU"),
                     all.x = T)

View(final_HBSRM)

#
# Import the biological status based on historical spawner abundance -----
pattern <- "biological_status_SH_percentiles"

biological_status_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)

nrow(biological_status_df) # 428

# remove CUs with less than 20 data points
biological_status_df <- biological_status_df[biological_status_df$dataPointNb >= 20,]
nrow(biological_status_df) # 206

printFig <- T

biological_status_compare_fun(biological_status_df = biological_status_df,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "region")

biological_status_compare_fun(biological_status_df = biological_status_df,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "species")

#
# Compare biological status between HBSR and HS percentiles approach -----

# Import biostatus obtained from HS percentiles:
pattern <- "biological_status_SH_percentiles"
bioStatus_HSPercent <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)

# remove NAs
bioStatus_HSPercent <- bioStatus_HSPercent[!is.na(bioStatus_HSPercent$status_HSPercent_05_red),]

# remove Cus with less than 20 data points
bioStatus_HSPercent <- bioStatus_HSPercent[bioStatus_HSPercent$dataPointNb >= 20,]
nrow(bioStatus_HSPercent) # 158

# add column bioStatus for 0.25 - 0.75 benchmarks:
colProba <- colnames(bioStatus_HSPercent)[grepl("_075_",colnames(bioStatus_HSPercent))]
bioStatus_HSPercent$bioStatus_HSPercent <- sapply(X = 1:nrow(bioStatus_HSPercent), 
                                                  FUN = function(r){
                                                    # r <- 1
                                                    slice <- bioStatus_HSPercent[r,colProba]
                                                    out <- c("red","amber","green")[slice == max(slice)]
                                                    return(out)
                                                  })

colToKeep_HSPercent <- c("region","species","CU_pse","bioStatus_HSPercent")

# Import biostatus obtained from HBSR:
pattern <- "biological_status"
bioStatus_HBSR <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                wd_output = wd_output,
                                                region = region,
                                                species_all = species_all)
# remove CUs with NAs
bioStatus_HBSR <- bioStatus_HBSR[!is.na(bioStatus_HBSR$status_Smsy_red),]

# add column bioStatus for Sgen - Smsy benchmarks:
colProba <- colnames(bioStatus_HBSR)[grepl("_Smsy_",colnames(bioStatus_HBSR))]
bioStatus_HBSR$bioStatus_HBSR <- sapply(X = 1:nrow(bioStatus_HBSR), 
                                                  FUN = function(r){
                                                    # r <- 1
                                                    slice <- bioStatus_HBSR[r,colProba]
                                                    out <- c("red","amber","green")[slice == max(slice)]
                                                    return(out)
                                                  })

colToKeep_HBSR <- c("region","species","CU_pse","bioStatus_HBSR")

# merge the two datasets:

bioStatus_merged <- merge(x = bioStatus_HBSR[,colToKeep_HBSR],
                          y = bioStatus_HSPercent[,colToKeep_HSPercent], 
                          by = c("region","species","CU_pse"),
                          all = T)

nrow(bioStatus_merged) # 221

# count how many CUs have the same biostatus with both approaches
bioStatus_merged_same <- bioStatus_merged[bioStatus_merged$bioStatus_HBSR == bioStatus_merged$bioStatus_HSPercent,]
bioStatus_merged_same <- bioStatus_merged_same[!is.na(bioStatus_merged_same$region),]

countHere <- table(bioStatus_merged_same$bioStatus_HBSR)[c("red","amber","green")]
sum(countHere) # 19

table_n <- data.frame(bioStatus = c("red","amber","green"),
                      n_same = as.numeric(countHere))

# counts for how many CUs have only values for HBSR
bioStatus_merged_HBSR_only <- bioStatus_merged[!is.na(bioStatus_merged$bioStatus_HBSR) & 
                                                 is.na(bioStatus_merged$bioStatus_HSPercent),]

countHere <- table(bioStatus_merged_HBSR_only$bioStatus_HBSR)[c("red","amber","green")]
sum(countHere) # 63

table_n$HBSR_only <- as.numeric(countHere)

# counts for how many CUs have only values for HS benchmarks
bioStatus_merged_HSBench_only <- bioStatus_merged[is.na(bioStatus_merged$bioStatus_HBSR) & 
                                                    !is.na(bioStatus_merged$bioStatus_HSPercent),]

countHere <- table(bioStatus_merged_HSBench_only$bioStatus_HSPercent)[c("red","amber","green")]
sum(countHere) # 108

table_n$HSBench_only <- as.numeric(countHere)

# counts for how many CUs have different biostatus
bioStatus_merged_diff <- bioStatus_merged[bioStatus_merged$bioStatus_HBSR != bioStatus_merged$bioStatus_HSPercent,]
bioStatus_merged_diff <- bioStatus_merged_diff[!is.na(bioStatus_merged_diff$region),]
nrow(bioStatus_merged_diff) # 31

# make all column for counts for HBSR 
countHere <- table(bioStatus_merged_diff$bioStatus_HBSR)[c("red","amber","green")]
sum(countHere) # 31

table_n$diff <- as.numeric(countHere)

table_m <- as.matrix(table_n[,c("n_same","HBSR_only","HSBench_only","diff")])
rownames(table_m) <- table_n$bioStatus

coloursStatus <- rev(c(g = "#8EB687", a = "#DFD98D", r = "#9A3F3F"))

if(printFig){
  jpeg(paste0(wd_figures,"/comparison_bioStatus_HBSR_HSPercent.jpg"), 
       width = 20,height = 15, units = "cm", res = 300)
}
barplot(height = table_m, col = coloursStatus, 
        ylab = "Number of CUs",xlab = "Biological status difference",
        main = "Bio-status differences with HBSR vs. HS percentiles approaches",
        names.arg = c('Same','HBSR only','HS percent only','Different'))
#polygon(x = c(1,2,2,1),y = c(20,20,60,60))
offset <- .2
barLarger <- 1
x0 <- offset*4 + (ncol(table_m)-1)*barLarger + barLarger/2
x1 <- x0 + barLarger/2
# create a polygons for each possible combination of biostatus
height <- 0
for(bs in c("red","amber","green")){
  # bs <- c("red","amber","green")[2]
  bioStatus_merged_diff_cut <- bioStatus_merged_diff[bioStatus_merged_diff$bioStatus_HBSR == bs,]
  if(nrow(bioStatus_merged_diff) > 0){
    bioStatusHSPercentHere <- unique(bioStatus_merged_diff_cut$bioStatus_HSPercent)
    for(bshsp in bioStatusHSPercentHere){
      # bshsp <- bioStatusHSPercentHere[]
      colourHere <- coloursStatus[bshsp == c("red","amber","green")]
      bioStatus_merged_diff_cut2 <- bioStatus_merged_diff_cut[bioStatus_merged_diff_cut$bioStatus_HSPercent == bshsp,]
      heightUP <- nrow(bioStatus_merged_diff_cut2) + height
      polygon(x = c(x0,x1,x1,x0),y = c(height,height,heightUP,heightUP),col = colourHere)
      height <- heightUP
    }
  }
}
# add the correspondging method for the lab bar
text(labels = "HBSR",x = x0 - barLarger/4, y = height, pos = 3, cex = .8)
text(labels = "HS percent",x = x0 + barLarger/4, y = height, pos = 3, cex = .8)
#
if(printFig){
  dev.off()
}

# big contrasts:
bioStatus_merged_diff[bioStatus_merged_diff$bioStatus_HBSR == "green" & bioStatus_merged_diff$bioStatus_HSPercent == "red",]
# Skeena      SX Stephens

bioStatus_merged_diff[bioStatus_merged_diff$bioStatus_HBSR == "red" & bioStatus_merged_diff$bioStatus_HSPercent == "green",]




# Import the historical spawner abundance benchmark values ------
pattern <- "HS_percentiles_summary"

benchmarks_summary_HSPercent_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                                 wd_output = wd_output,
                                                                 region = region,
                                                                 species_all = F)

head(benchmarks_summary_HSPercent_df)



