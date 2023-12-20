
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

# Return list of CUs that have high exploitation rate or low production rates,
# as well as a final call on keeping or removing the CUs depending of their
# biostatus: the one with already a red/poor status are kept (i.e. Clare's 8th rule).
highExploit_lowProd <- cu_highExploit_lowProd_fun(biological_status_HSPercent_df)

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

table_m <- as.matrix(table_n[,c("n_same","HBSR_only","HSBench_only","diff")])
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
# 6) Create complete dataset with all the CUs and their biostatus and psf_staus_code -----
# https://salmonwatersheds.slack.com/archives/CJG0SHWCW/p1701464111241899?thread_ts=1701199596.229739&cid=CJG0SHWCW

# Field to include:
# percentile_red_prob
# percentile_yellow_prob
# percentile_green_prob
# percentile_status values: good, fair, poor, NA
# sr_status values: good, fair, poor, NA
# psf_status values: good, fair, poor, extinct, data-deficient, not-assessed
# psf_status_code values: 1 to 9
# 1 = good
# 2 = fair
# 3 = poor
# 4 = extinct
# 5 = not-assessed (cyclic dominance)
# 6 = not-assessed (low productivity and high exploitation)
# 7 = data-deficient (insufficient time series length)
# 8 = data-deficient (no estimates of spawner abundance in the most recent generation)
# 9 = data-deficient (no spawner estimates available)

# Return list of CUs that have high exploitation rate or low production rates,
# as well as a final call on keeping or removing the CUs depending of their
# biostatus: the one with already a red/poor status are kept (i.e. Clare's 8th rule).
highExploit_lowProd <- cu_highExploit_lowProd_fun(biological_status_HSPercent_df)

# A. add column psf_status_code to each dataset:
biological_status_HBSR <- biological_status_HBSR_df
biological_status_HSPercent <- biological_status_HSPercent_df
biological_status_HBSR$psf_status_code <- NA
biological_status_HSPercent$psf_status_code <- NA

# 4 = extinct
cu_extinct <- cu_extinct_fun()

row_toUpdate <- biological_status_HBSR$cuid %in% cu_extinct$cuid
val_toUpdate <- biological_status_HBSR$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,4, sep = ", ")
biological_status_HBSR$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- biological_status_HSPercent$cuid %in% cu_extinct$cuid
val_toUpdate <- biological_status_HSPercent$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,4, sep = ", ")
biological_status_HSPercent$psf_status_code[row_toUpdate] <- val_new

# 5 = not-assessed (cyclic dominance)
row_toUpdate <- grepl("(cyclic)",biological_status_HBSR$CU_pse)
val_toUpdate <- biological_status_HBSR$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,5, sep = ", ")
biological_status_HBSR$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- grepl("(cyclic)",biological_status_HSPercent$CU_pse)
val_toUpdate <- biological_status_HSPercent$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,5, sep = ", ")
biological_status_HSPercent$psf_status_code[row_toUpdate] <- val_new

# 6 = not-assessed (low productivity and high exploitation) --> Percentile only
highExploit_lowProd_toRemove <- highExploit_lowProd[highExploit_lowProd$toRemove,]
for(r in 1:nrow(highExploit_lowProd_toRemove)){
  # r <- 1
  rg <- highExploit_lowProd_toRemove$region[r]
  sp <- highExploit_lowProd_toRemove$species_abbr[r]
  cu <- highExploit_lowProd_toRemove$CU_name[r]
  
  row_toUpdate <- biological_status_HSPercent$region == rg &
    biological_status_HSPercent$species == sp &
    biological_status_HSPercent$CU_pse == cu
  
  val_toUpdate <- biological_status_HSPercent$psf_status_code[row_toUpdate]
  val_new <- paste(val_toUpdate,6, sep = ", ")
  
  biological_status_HSPercent$psf_status_code[row_toUpdate] <- val_new
}

# 7 = data-deficient (insufficient time series length) --> Percentile only
row_toUpdate <- biological_status_HSPercent$dataPointNb < 20
val_toUpdate <- biological_status_HSPercent$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,7, sep = ", ")
biological_status_HSPercent$psf_status_code[row_toUpdate] <- val_new

# 8 = data-deficient (no estimates of spawner abundance in the most recent generation)
row_toUpdate <- biological_status_HBSR$genLength_dataPointNb == 0
val_toUpdate <- biological_status_HBSR$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,8, sep = ", ")
biological_status_HBSR$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- biological_status_HSPercent$genLength_dataPointNb == 0
val_toUpdate <- biological_status_HSPercent$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,8, sep = ", ")
biological_status_HSPercent$psf_status_code[row_toUpdate] <- val_new

# 9 = data-deficient (no spawner estimates available)
row_toUpdate <- biological_status_HBSR$comment == "Only NAs in cuspawnerabundance.csv for this CU" &
  !is.na(biological_status_HBSR$comment)
val_toUpdate <- biological_status_HBSR$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,9, sep = ", ")
biological_status_HBSR$psf_status_code[row_toUpdate] <- val_new

row_toUpdate <- biological_status_HSPercent$comment  == "Only NAs in cuspawnerabundance.csv for this CU" &
  !is.na(biological_status_HSPercent$comment)
val_toUpdate <- biological_status_HSPercent$psf_status_code[row_toUpdate]
val_new <- paste(val_toUpdate,9, sep = ", ")
biological_status_HSPercent$psf_status_code[row_toUpdate] <- val_new

# Remove "NA, "
biological_status_HBSR$psf_status_code <- gsub("NA, ","",biological_status_HBSR$psf_status_code)
biological_status_HSPercent$psf_status_code <- gsub("NA, ","",biological_status_HSPercent$psf_status_code)

unique(biological_status_HBSR$psf_status_code)
unique(biological_status_HSPercent$psf_status_code)

# B. Combine the two datasets:

colCommon <- c("region","species","cuid","CU_pse","current_spawner_abundance","psf_status_code")
              # "year_last","year_first","genLength")
colHBSR <- c("status_Smsy_red","status_Smsy_amber","status_Smsy_green","status_Smsy")
colPercent <- c("status_HSPercent_075_red","status_HSPercent_075_amber","status_HSPercent_075_green",
                "status_percent075")

biological_status_merged <- merge(x = biological_status_HBSR[,c(colCommon,colHBSR)],
                                  y = biological_status_HSPercent[,c(colCommon,colPercent)],
                                  by =  c("region","species","cuid","CU_pse","current_spawner_abundance"), 
                                  all = T)

# Check if number of CUs is correct:
CUs_comm <- biological_status_HBSR$cuid[biological_status_HBSR$cuid %in% 
                                          biological_status_HSPercent$cuid]
length(CUs_comm) # 142
CUs_HBSR_only <- biological_status_HBSR$cuid[!biological_status_HBSR$cuid %in% 
                                               biological_status_HSPercent$cuid]
length(CUs_HBSR_only) # 0
CUs_Percent_only <- biological_status_HSPercent$cuid[!biological_status_HSPercent$cuid %in% 
                                                       biological_status_HBSR$cuid]
length(CUs_Percent_only) # 286

# Expected number of rows in biological_status_merged:
length(CUs_comm) + length(CUs_HBSR_only) + length(CUs_Percent_only) # 428
nrow(biological_status_merged) # 428 --> ALL GOOD

# Renames columns
colnames(biological_status_merged) <- gsub("red","red_prob",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("amber","yellow_prob",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("green","green_prob",colnames(biological_status_merged))

colnames(biological_status_merged) <- gsub("status_Smsy_","sr_",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_HSPercent_075_","percentile_",colnames(biological_status_merged))

colnames(biological_status_merged) <- gsub("status_Smsy","sr_status",colnames(biological_status_merged))
colnames(biological_status_merged) <- gsub("status_percent075","percentile_status",colnames(biological_status_merged))

# Create psf_status field and Attribute 1 (good), 2 (fair) or 3 (poor) for 
biological_status_merged$psf_status <- NA #  values: good, fair, poor, extinct, data-deficient, not-assessed
biological_status_merged$psf_status_code <- NA # values: 1 to 9

col_prob <- colnames(biological_status_merged)[grepl("_prob",colnames(biological_status_merged))]
col_sr_prob <- col_prob[grepl("sr_",col_prob)]
col_percent_prob <- col_prob[grepl("percentile_",col_prob)]

for(r in 1:nrow(biological_status_merged)){
  # r <- 428
  bs_here <- biological_status_merged[r,]
  
  if(!is.na(bs_here$sr_red_prob) & is.na(bs_here$psf_status_code.x)){ # is.na(bs_here$psf_status_code.x) might not be necessary but does not hurt
    
    psf_status_here <- c("poor","fair","good")[bs_here[,col_sr_prob] == max(bs_here[,col_sr_prob])]
    psf_status_code_here <- c(3:1)[psf_status_here == c("poor","fair","good")]

  }else if(!is.na(bs_here$percentile_red_prob) & is.na(bs_here$psf_status_code.y)){
    
    psf_status_here <- c("poor","fair","good")[bs_here[,col_percent_prob] == max(bs_here[,col_percent_prob])]
    psf_status_code_here <- c(3:1)[psf_status_here == c("poor","fair","good")]
    
  }else{
    
    code_HBSR <- bs_here$psf_status_code.x
    code_Percentile <- bs_here$psf_status_code.y
    code_both <- c(code_HBSR, code_Percentile)
    code_both <- code_both[!is.na(code_both)]
    code_both <- sapply(X = code_both, FUN =  strsplit, split = ", ")
    code_both <- as.numeric(code_both[[1]])
    code_both <- sort(code_both)
    code_both <- unique(code_both)
    
    psf_status_here <- NULL
    if(sum(code_both %in% 5:6) > 0){
      psf_status_here <- c(psf_status_here,"not-assessed")
    }
    if(sum(code_both %in% 7:9) > 0){
      psf_status_here <- c(psf_status_here,"data-deficient")
    }
    
    code_both <- paste(code_both,collapse = ", ")
    psf_status_code_here <- code_both
    
    psf_status_here <- paste(psf_status_here,collapse = ", ")
    
    # print(code_both)
  }
  
  biological_status_merged$psf_status[r] <- psf_status_here
  biological_status_merged$psf_status_code[r] <- psf_status_code_here
}

# Checks that there is no missing psf_status_code (i.e. no NAs)
unique(biological_status_merged$psf_status_code)
table(biological_status_merged$psf_status_code)
unique(biological_status_merged$psf_status)
table(biological_status_merged$psf_status)

# 
unique(biological_status_merged[,c("current_spawner_abundance","psf_status_code")][is.na(biological_status_merged$current_spawner_abundance),])

# Drop unecessary columns:
colToDrop <- c("psf_status_code.x","psf_status_code.y","current_spawner_abundance")
biological_status_merged <- biological_status_merged[,!colnames(biological_status_merged) %in% colToDrop]

# write.csv(biological_status_merged,paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),
#           row.names = F)

biological_status_merged <- read.csv(paste0(wd_output,"/Biological_status_HBSR_Percentile_all.csv"),
                                     header = T)

# check if there psf code of cyclic communities:
biological_status_merged[grepl("cyclic",biological_status_merged$CU_pse),]

#
# Check the difference between normal percentile benchmarks and the simulated ones -----
nrow(benchmarks_summary_HSPercent_df)
benchmarks_summary_HSPercent_df_noNA <- benchmarks_summary_HSPercent_df[!is.na(benchmarks_summary_HSPercent_df$m),]
nrow(benchmarks_summary_HSPercent_df_noNA)
percent_diff <- (benchmarks_summary_HSPercent_df_noNA$m - benchmarks_summary_HSPercent_df_noNA$m_sim) / benchmarks_summary_HSPercent_df_noNA$m * 100
hist(percent_diff)

benchmarks_summary_HSPercent_df_noNA[percent_diff < -50,]
# do those have cyclic dynamics ?







