
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


#' Import the conservationunits_decoder.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
datasetsNames_database <- datasetsNames_database_fun()
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = F,
                                                   update_file_csv = F,
                                                   wd = wd_pop_indic_data_input_dropbox)

nrow(unique(conservationunits_decoder[,c("region","species_name","cu_name_pse")]))

printFig <- F

#
# 1) Import benchmarks values and biological status files for both methods -----
# Import all the CSV files for each combination of region - species and rbind them

#'* Import biostatus obtained with HBSR Sgen - Smsy: *
pattern <- "biological_status"
biological_status_HBSR_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)

# write.csv(biological_status_HBSR_df,paste0(wd_output,"/Biological_status_HBSRM_all.csv"),
#           row.names = F)
head(biological_status_HBSR_df)
colnames(biological_status_HBSR_df)
nrow(biological_status_HBSR_df) # 142
unique(biological_status_HBSR_df$comment)

# add final biostatus for both threshold
colProb <- colnames(biological_status_HBSR_df)[grepl("Smsy_",colnames(biological_status_HBSR_df))]
biological_status_HBSR_df$status_Smsy <- sapply(X = 1:nrow(biological_status_HBSR_df), 
                                             FUN = function(r){
                                               # r <- 1
                                               slice <- biological_status_HBSR_df[r,colProb]
                                               out <- c("red","amber","green")[slice == max(slice)][1]
                                               return(out)
                                             })

colProb <- colnames(biological_status_HBSR_df)[grepl("Smsy80_",colnames(biological_status_HBSR_df))]
biological_status_HBSR_df$status_Smsy80 <- sapply(X = 1:nrow(biological_status_HBSR_df), 
                                               FUN = function(r){
                                                 # r <- 1
                                                 slice <- biological_status_HBSR_df[r,colProb]
                                                 out <- c("red","amber","green")[slice == max(slice)][1]
                                                 return(out)
                                               })

#'* Import the benchmark values associated with the HBSRM *
pattern <- "benchmarks_summary"
benchmarks_summary_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                       wd_output = wd_output,
                                                       region = region,
                                                       species_all = F)

head(benchmarks_summary_df)
nrow(unique(benchmarks_summary_df[,c("region","species","CU")])) # 

# merge biological_status_HBSR_df with benchmarks_summary_df with the highest posterior density estimate
benchmarks_summary_df
benchmarks_summary_df_Smsy_HPD <- benchmarks_summary_df[benchmarks_summary_df$benchmark == "Smsy" & benchmarks_summary_df$method == "HPD",]

final_HBSRM <- merge(x = biological_status_HBSR_df, 
                     y = benchmarks_summary_df_Smsy_HPD[,c("region","species","CU","m")],
                     by = c("region","species","CU"),
                     all.x = T)


#'* Import the biological status obtained with HS percentile method: *
pattern <- "biological_status_SH_percentiles"
biological_status_HSPercent_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                      wd_output = wd_output,
                                                      region = region,
                                                      species_all = species_all)

nrow(biological_status_HSPercent_df) # 428

# add final biostatus for both threshold
colProb <- colnames(biological_status_HSPercent_df)[grepl("_075_",colnames(biological_status_HSPercent_df))]
biological_status_HSPercent_df$status_percent075 <- sapply(X = 1:nrow(biological_status_HSPercent_df), 
                                           FUN = function(r){
                                             # r <- 1
                                             slice <- biological_status_HSPercent_df[r,colProb]
                                             out <- c("red","amber","green")[slice == max(slice)][1]
                                             return(out)
                                           })

colProb <- colnames(biological_status_HSPercent_df)[grepl("_05_",colnames(biological_status_HSPercent_df))]
biological_status_HSPercent_df$status_percent05 <- sapply(X = 1:nrow(biological_status_HSPercent_df), 
                                                     FUN = function(r){
                                                       # r <- 1
                                                       slice <- biological_status_HSPercent_df[r,colProb]
                                                       out <- c("red","amber","green")[slice == max(slice)][1]
                                                       return(out)
                                                     })

#'* Import the historical spawner abundance benchmark values *
pattern <- "HS_percentiles_summary"
benchmarks_summary_HSPercent_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                                 wd_output = wd_output,
                                                                 region = region,
                                                                 species_all = F)

head(benchmarks_summary_HSPercent_df)

#
# 2) Decision rules for HBSR and percentile benchmarks -------
#' The 5 rules to exclude data:
#' - cyclic dominance --> for both methods
#' - low productivity or high exploitation rate (EXCEPT IF ALREADY RED) --> for percentile
#' - data deficient  --> for both methods
#' - no current abundance --> for both methods
#' - insufficient time series length (< 20 data points) --> for percentile
#' New rule from Claire: low productivity or high exploitation rate (EXCEPT IF ALREADY RED)
#' 

# Steph diagram:
# https://www.dropbox.com/s/p0gf5aswd2kbd2p/Status_assessment_flowchart_SP.pptx?dl=0

# Clare's slack thread about the 8th decision rule:
# https://salmonwatersheds.slack.com/archives/CPD76USTU/p1700508091399359
# 

# List CUs with high exploitation/low productivity & cyclic dominance
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1700673066049189?thread_ts=1700604709.505309&cid=CJ5RVHVCG

highExploit_lowProd <- data.frame(species = c("Coho","Coho","Coho","Coho","Coho",
                                              "Chinook","Chinook",NA,NA),
                                  CU_name = c("Fraser Canyon","Interior Fraser",
                                              "Lower Thompson","North Thompson",
                                              "South Thompson","Lower Fraser River (Fall 4-1)",
                                              "Shuswap River (Summer 4-1)",
                                              "East Vancouver Island-Cowichan and Koksilah (Fall x-1)",
                                              "East Vancouver Island-Goldstream (Fall x-1)"))

highExploit_lowProd$toRemove <- T
for(i in 1:nrow(highExploit_lowProd)){
  # i <- 1
  sp <- highExploit_lowProd$species[i]
  cu <- highExploit_lowProd$CU_name[i]
  biological_status_HSPercent_dfHere <- biological_status_HSPercent_df[biological_status_HSPercent_df$CU_pse == cu,]
  if(nrow(biological_status_HSPercent_dfHere) > 1){
    print(biological_status_HSPercent_dfHere)
    biological_status_HSPercent_dfHere <- biological_status_HSPercent_dfHere[biological_status_HSPercent_dfHere$species == sp,]
  }
  status <- biological_status_HSPercent_dfHere$status_percent075
  if(!is.na(status)){
    if(status == "red"){ #' New rule from Claire:
      highExploit_lowProd$toRemove[i] <- F
    }
  }
  print(biological_status_HSPercent_dfHere$status_percent075)
}

# Are all these CUs in --> yes
highExploit_lowProd$CU_name[! highExploit_lowProd$CU_name %in% biological_status_HSPercent_df$CU_pse]

# Remove Cus with < 20 data points in biological_status_HSPercent_df:
biological_status_HSPercent_cut <- biological_status_HSPercent_df[biological_status_HSPercent_df$dataPointNb >= 20,]
nrow(biological_status_HSPercent_cut) # 206

# Remove the cyclic ones in both biological_status_HSPercent_df and biological_status_HBSR_df
# the cyclic have it written in name (e.g. Chilliwack-Early Summer (cyclic))
biological_status_HSPercent_cut <- biological_status_HSPercent_cut[!grepl("(cyclic)",biological_status_HSPercent_cut$CU_pse),]
nrow(biological_status_HSPercent_cut) # 200
biological_status_HBSR_cut <- biological_status_HBSR_df[!grepl("(cyclic)",biological_status_HBSR_df$CU_pse),]
nrow(biological_status_HBSR_cut) # 136

# Remove the CUs with high exploitation/low productivity in biological_status_HSPercent_cut:
# BUT only if status_percent075 != "red".
# If status_percent075 is NA then remove.
CUsToRemove <- highExploit_lowProd$CU_name[highExploit_lowProd$toRemove]
biological_status_HSPercent_cut <- biological_status_HSPercent_cut[!biological_status_HSPercent_cut$CU_pse %in% CUsToRemove,]
nrow(biological_status_HSPercent_cut) # 193

#
# 3) Figures biological status based on HBSRM comparison Smsy vs. 80% Smsy ------
#

biological_status_compare_fun(biological_status_df = biological_status_HBSR_cut,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "region")

biological_status_compare_fun(biological_status_df = biological_status_HBSR_df,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "species")

#
# 3) Figures comparison biological status HS abundance percentiles 0.75 vs. 0.50 -----

biological_status_compare_fun(biological_status_df = biological_status_HSPercent_cut,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "region")

biological_status_compare_fun(biological_status_df = biological_status_HSPercent_cut,
                              wd = wd_figures, 
                              printFig = printFig, 
                              group_var = "species")

#
# 4) Figure that compares biological status between HBSR and HS percentiles approach -----
#

bioStatus_HBSR <- biological_status_HBSR_cut
nrow(bioStatus_HBSR) # 136

bioStatus_HSPercent <- biological_status_HSPercent_cut
nrow(bioStatus_HSPercent) # 193

# merge the two datasets:
colToKeep_HSPercent <- c("region","species","CU_pse","status_percent075")
colToKeep_HBSR <- c("region","species","CU_pse","status_Smsy")

bioStatus_merged <- merge(x = bioStatus_HBSR[,colToKeep_HBSR],
                          y = bioStatus_HSPercent[,colToKeep_HSPercent], 
                          by = c("region","species","CU_pse"),
                          all = T)

nrow(bioStatus_merged) # 195
nrow(unique(bioStatus_merged[,c("region","species","CU_pse")]))

# count how many CUs have the same biostatus with both approaches
bioStatus_merged_noNA <- bioStatus_merged[!is.na(bioStatus_merged$status_Smsy) &
                                            !is.na(bioStatus_merged$status_percent075),]
nrow(bioStatus_merged_noNA) # 106

# Same status:
bioStatus_merged_same <- bioStatus_merged_noNA[bioStatus_merged_noNA$status_Smsy == bioStatus_merged_noNA$status_percent075,]
nrow(bioStatus_merged_same) # 49
countHere <- table(factor(bioStatus_merged_same$status_Smsy,levels = c("red","amber","green")))
table_n <- data.frame(bioStatus = c("red","amber","green"),
                      n_same = as.numeric(countHere))

# Status only values for HBSR:
bioStatus_merged_HBSR_only <- bioStatus_merged[!is.na(bioStatus_merged$status_Smsy) & 
                                                 is.na(bioStatus_merged$status_percent075),]
nrow(bioStatus_merged_HBSR_only) # 1
countHere <- table(factor(bioStatus_merged_HBSR_only$status_Smsy,levels = c("red","amber","green")))
table_n$HBSR_only <- as.numeric(countHere)

# Status only values for HS benchmarks:
bioStatus_merged_HSBench_only <- bioStatus_merged[is.na(bioStatus_merged$status_Smsy) & 
                                                    !is.na(bioStatus_merged$status_percent075),]
nrow(bioStatus_merged_HSBench_only) # 44
countHere <- table(factor(bioStatus_merged_HSBench_only$status_percent075,levels = c("red","amber","green")))
table_n$HSBench_only <- as.numeric(countHere)

# Different status:
bioStatus_merged_diff <- bioStatus_merged_noNA[bioStatus_merged_noNA$status_Smsy != bioStatus_merged_noNA$status_percent075,]
nrow(bioStatus_merged_diff) # 57
countHere <- table(factor(bioStatus_merged_diff$status_Smsy,levels = c("red","amber","green")))
table_n$diff <- as.numeric(countHere)

table_m <- as.matrix(table_n[,c("n_same","HBSR_only","Percentiles_only","diff")])
rownames(table_m) <- table_n$bioStatus

coloursStatus <- rev(c(g = "#8EB687", a = "#DFD98D", r = "#9A3F3F"))

if(printFig){
  jpeg(paste0(wd_figures,"/comparison_bioStatus_HBSR_Percentiles.jpg"), 
       width = 20,height = 15, units = "cm", res = 300)
}
barplot(height = table_m, col = coloursStatus, 
        ylim = c(0,max(apply(table_m,2,sum)) + 5), las = 1,
        ylab = "Number of CUs",xlab = "Biological status difference",
        main = "Bio-status differences with HBSR vs. percentiles methods",
        names.arg = c('Same','HBSR only','percentiles only','Different'))
#polygon(x = c(1,2,2,1),y = c(20,20,60,60))
offset <- .2
barLarger <- 1
x0 <- offset*4 + (ncol(table_m)-1)*barLarger + barLarger/2
x1 <- x0 + barLarger/2
# create a polygons for each possible combination of biostatus
height <- 0
for(bs in c("red","amber","green")){
  # bs <- c("red","amber","green")[2]
  bioStatus_merged_diff_cut <- bioStatus_merged_diff[bioStatus_merged_diff$status_Smsy == bs,]
  if(nrow(bioStatus_merged_diff) > 0){
    bioStatusHSPercentHere <- unique(bioStatus_merged_diff_cut$status_percent075)
    for(bshsp in bioStatusHSPercentHere){
      # bshsp <- bioStatusHSPercentHere[]
      colourHere <- coloursStatus[bshsp == c("red","amber","green")]
      bioStatus_merged_diff_cut2 <- bioStatus_merged_diff_cut[bioStatus_merged_diff_cut$status_percent075 == bshsp,]
      heightUP <- nrow(bioStatus_merged_diff_cut2) + height
      polygon(x = c(x0,x1,x1,x0),y = c(height,height,heightUP,heightUP),col = colourHere)
      height <- heightUP
    }
  }
}
# add the correspondging method for the lab bar
text(labels = "HBSR",x = x0 - barLarger/4, y = height, pos = 3, cex = .8)
text(labels = "percentiles",x = x0 + barLarger/4, y = height, pos = 3, cex = .8)
#
if(printFig){
  dev.off()
}

# big contrasts:
bioStatus_merged_diff[bioStatus_merged_diff$status_Smsy == "green" & bioStatus_merged_diff$status_percent075 == "red",]
# Haida Gwaii      PK West Haida Gwaii (even)          green                 red
#      Skeena      SX                Stephens          green                 red

bioStatus_merged_diff[bioStatus_merged_diff$status_Smsy == "red" & bioStatus_merged_diff$status_percent075 == "green",]

bioStatus_merged_HBSR_only

#
# 5) Create summary dataset of CUs that had a change in status for each method ------
#' - with fields for cuid, CU name, species, region, and benchmark_type applied
#' - (SR, if no SR then percentile)

# Remove CUs with no benchmark estimations and retain only the ones with contrasting status
biological_status_HBSR_cut_noNA <- biological_status_HBSR_cut[!is.na(biological_status_HBSR_cut$status_Smsy),]
biological_status_HSPercent_cut_noNA <- biological_status_HSPercent_cut[!is.na(biological_status_HSPercent_cut$status_percent075),]

biological_status_HBSR_diff <- biological_status_HBSR_cut_noNA[biological_status_HBSR_cut_noNA$status_Smsy != biological_status_HBSR_cut_noNA$status_Smsy80,]
nrow(biological_status_HBSR_diff) # 10
biological_status_HSPercent_diff <- biological_status_HSPercent_cut_noNA[biological_status_HSPercent_cut_noNA$status_percent075 != biological_status_HSPercent_cut_noNA$status_percent05,]
nrow(biological_status_HSPercent_diff) # 28

# Remove the CUs in biological_status_HSPercent_diff that are in biological_status_HBSR_diff
toRemove <- sapply(X = 1:nrow(biological_status_HSPercent_diff),FUN = function(r){
  # r <- 1
  rg <- biological_status_HSPercent_diff$region[r]
  sp <- biological_status_HSPercent_diff$species[r]
  cu <- biological_status_HSPercent_diff$CU_pse[r]
  
  HBSRHere <- biological_status_HBSR_diff[biological_status_HBSR_diff$region == rg &
                                            biological_status_HBSR_diff$species == sp &
                                            biological_status_HBSR_diff$CU_pse == cu,]
  if(nrow(HBSRHere) == 0){
    out <- F
  }else{
    out <- T
    print(HBSRHere)
    print(biological_status_HSPercent_diff[r,])
    print("***")
  }
  return(out)
})
sum(toRemove) # 2

biological_status_HSPercent_diff <- biological_status_HSPercent_diff[!toRemove,]
nrow(biological_status_HSPercent_diff) # 26

# Merge biological_status_HBSR_diff and biological_status_HSPercent_diff
biological_status_HBSR_diff$benchmark_type <- "HBSR"
biological_status_HSPercent_diff$benchmark_type <- "percentiles"
biological_status_HBSR_diff$biostatus <- biological_status_HBSR_diff$status_Smsy
biological_status_HSPercent_diff$biostatus <- biological_status_HSPercent_diff$status_percent075

colInCommon <- c("region","species","cuid","CU_pse","biostatus","benchmark_type")

biological_status_merge_diff <- merge(x = biological_status_HBSR_diff[,colInCommon],
                                      y = biological_status_HSPercent_diff[,colInCommon],
                                      by = c("region","species","cuid","CU_pse"),
                                      all = T)

nrow(biological_status_merge_diff) # 36 : correct

biological_status_merge_diff$biostatus <- sapply(X = 1:nrow(biological_status_merge_diff),
                                                 FUN = function(r){
                                                   # r <- 1
                                                   out <- biological_status_merge_diff[r,c("biostatus.x","biostatus.y")]
                                                   out <- out[!is.na(out)]
                                                   return(out)
})
biological_status_merge_diff$benchmark_type <- sapply(X = 1:nrow(biological_status_merge_diff),
                                                 FUN = function(r){
                                                   # r <- 1
                                                   out <- biological_status_merge_diff[r,c("benchmark_type.x","benchmark_type.y")]
                                                   out <- out[!is.na(out)]
                                                   return(out)
                                                 })

colToRemove <- c("biostatus.x","biostatus.y","benchmark_type.x","benchmark_type.y")
colToKeep <- colnames(biological_status_merge_diff)[! colnames(biological_status_merge_diff) %in% colToRemove]
biological_status_merge_diff <- biological_status_merge_diff[,colToKeep]

# write CSV:
# write.csv(biological_status_merge_diff,
#           paste0(wd_output,"/Biological_status_diff_SMsySmsy80_percent05075.csv"),
#           row.names = F)

#

#
# Check the difference between normal percentile benchmarks and the simulated ones -----
nrow(benchmarks_summary_HSPercent_df)
benchmarks_summary_HSPercent_df_noNA <- benchmarks_summary_HSPercent_df[!is.na(benchmarks_summary_HSPercent_df$m),]
nrow(benchmarks_summary_HSPercent_df_noNA)
percent_diff <- (benchmarks_summary_HSPercent_df_noNA$m - benchmarks_summary_HSPercent_df_noNA$m_sim) / benchmarks_summary_HSPercent_df_noNA$m * 100
hist(percent_diff)

benchmarks_summary_HSPercent_df_noNA[percent_diff < -50,]
# do those have cyclic dynamics ?



