
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
printFig <- F

#
# write.csv(biological_status_df,paste0(wd_output,"/Biological_status_HBSRM_all.csv"),
#           row.names = F)

head(biological_status_df)
colnames(biological_status_df)
nrow(biological_status_df) # 142
unique(biological_status_df$comment)

# discrepancies in CU names
sum(gsub("_"," ",biological_status_df$CU) != biological_status_df$CU_pse)/nrow(biological_status_df)
sum(gsub("_"," ",biological_status_df$CU) != biological_status_df$CU_dfo)/nrow(biological_status_df)
sum(biological_status_df$CU_pse != biological_status_df$CU_dfo)/nrow(biological_status_df)

# CUs with "Only NAs in cuspawnerabundance.csv for this CU" 
biological_status_df[!is.na(biological_status_df$comment) & 
                       biological_status_df$comment == "Only NAs in cuspawnerabundance.csv for this CU",]

# CUs with "Not recent enough data"
biological_status_df[!is.na(biological_status_df$comment) &
                       grepl("Not recent enough data",biological_status_df$comment),]

# rest of the CUs with data:
biological_status_df <- biological_status_df[is.na(biological_status_df$comment) |
                                               biological_status_df$comment == "",] # ?! there should not be NAs...
nrow(biological_status_df) # 113

colToRemove_biostatus <- c("CU_pse","CU_dfo","genLength_available","comment")

biological_status_df <- biological_status_df[,!colnames(biological_status_df) %in% colToRemove_biostatus]

# CUs that have contrasting biological status between Smsy80 and Smsy:
colnamesSelect <- c("region","species","CU",
                    colnames(biological_status_df)[grepl("Smsy_",colnames(biological_status_df))])

biological_status_df$status_Smsy <- sapply(X = 1:nrow(biological_status_df), 
                                          FUN = function(r){
                                            # r <- 1
                                            slice <- biological_status_df[r,colnames(biological_status_df)[grepl("Smsy_",colnames(biological_status_df))]]
                                            out <- c("red","amber","green")[slice == max(slice)]
                                            return(out)
                                          })

biological_status_df$status_Smsy80 <- sapply(X = 1:nrow(biological_status_df), 
                                           FUN = function(r){
                                             # r <- 1
                                             slice <- biological_status_df[r,colnames(biological_status_df)[grepl("Smsy80_",colnames(biological_status_df))]]
                                             out <- c("red","amber","green")[slice == max(slice)]
                                             return(out)
                                           })

# 
biological_status_df[biological_status_df$status_Smsy != biological_status_df$status_Smsy80,]
# that's not a lot of CUs!

# Figure
rDiff <- nrow(biological_status_df[biological_status_df$status_Smsy != biological_status_df$status_Smsy80,])
rSame <- nrow(biological_status_df[biological_status_df$status_Smsy == biological_status_df$status_Smsy80,])

table_region <- NULL

regions <- unique(biological_status_df$region)
for(rg in regions){
  # rg <- regions[1]
  biological_status_df_cut <- biological_status_df[biological_status_df$region == rg,]
  
  tableHere <- data.frame(region = rg,
                          n_same = sum(biological_status_df_cut$status_Smsy == biological_status_df_cut$status_Smsy80),
                          n_diff = sum(biological_status_df_cut$status_Smsy != biological_status_df_cut$status_Smsy80))
  if(is.null(table_region)){
    table_region <- tableHere
  }else{
    table_region <- rbind(table_region,tableHere)
  }
}

table_region_m <- as.matrix(table_region[,c(2:3)])
rownames(table_region_m) <- table_region$region
colours <- rainbow(n = nrow(table_region_m))
if(printFig){
  jpeg(paste0(wd_figures,"/comparison_bioStatus_Smsy_Smsy80_regions.jpg"), 
       width = 20,height = 15, units = "cm", res = 300)
}
par(mar=c(5,4.5,3,0.5))
barplot(height = table_region_m,
        main = "Comparison by regions",
        ylab = "Number of CUs", xlab = "Biological status difference",
        col = colours, 
        names.arg = c("Same","Different"))
legend("topright",rev(rownames(table_region_m)),fill = rev(colours), bty = "n")
if(printFig){
  dev.off()
}

# figure for species
table_species <- NULL
species <- unique(biological_status_df$species)
for(sp in species){
  # sp <- species[1]
  biological_status_df_cut <- biological_status_df[biological_status_df$species == sp,]
  
  tableHere <- data.frame(species = sp,
                          n_same = sum(biological_status_df_cut$status_Smsy == biological_status_df_cut$status_Smsy80),
                          n_diff = sum(biological_status_df_cut$status_Smsy != biological_status_df_cut$status_Smsy80))
  if(is.null(table_species)){
    table_species <- tableHere
  }else{
    table_species <- rbind(table_species,tableHere)
  }
}

table_species_m <- as.matrix(table_species[,c(2:3)])

rownamesSp <- sapply(X = table_species$species, FUN = function(sp){
  return(species_acronym_df$species_name[species_acronym_df$species_acro == sp][1])
})
rownames(table_species_m) <- rownamesSp
colours <- rainbow(n = nrow(table_species_m))
if(printFig){
  jpeg(paste0(wd_figures,"/comparison_bioStatus_Smsy_Smsy80_species.jpg"), 
       width = 20,height = 15, units = "cm", res = 300)
}
par(mar=c(5,4.5,3,0.5))
barplot(height = table_species_m,
        main = "Comparison by species",
        ylab = "Number of CUs", xlab = "Biological status difference",
        col = colours, 
        names.arg = c("Same","Different"))
sp_legend <- paste(rev(rownames(table_species_m)),"                 ")
legend("topright",sp_legend,fill = rev(colours), bty = "n")
if(printFig){
  dev.off()
}


#' Import the benchmark values associated with the HBSRM ------
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
#' Import the biological status based on historical spawner abundance -----
pattern <- "biological_status_SH_percentiles"

biological_status_HSPercent_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                                wd_output = wd_output,
                                                                region = region,
                                                                species_all = F)

biological_status_HSPercent_df <- biological_status_HSPercent_df[,! colnames(biological_status_HSPercent_df) %in% colToRemove_biostatus]

biological_status_HSPercent_df <- biological_status_HSPercent_df[!is.na(biological_status_HSPercent_df$status_HSPercent_red),]

biological_status_HSPercent_df$status_HS <- sapply(X = 1:nrow(biological_status_HSPercent_df), 
                                                   FUN = function(r){
                                                     # r <- 1
                                                     slice <- biological_status_HSPercent_df[r,colnames(biological_status_HSPercent_df)[grepl("status_HSPercent_",colnames(biological_status_HSPercent_df))]]
                                                     out <- c("red","amber","green")[slice == max(slice)]
                                                     return(out)
                                                   })

biostat_HBSR_SH <- merge(x = biological_status_df[,c("region","species","CU","status_Smsy","status_Smsy80")], 
                         y = biological_status_HSPercent_df[,c("region","species","CU","status_HS")], 
                         by = c("region","species","CU"),
                         all = T)
biostat_HBSR_SH

biostat_HBSR_SH_noNA <- biostat_HBSR_SH[!is.na(biostat_HBSR_SH$status_Smsy80) & !is.na(biostat_HBSR_SH$status_HS),]

biostat_HBSR_SH_noNA[biostat_HBSR_SH_noNA$status_Smsy == biostat_HBSR_SH_noNA$status_HS,]
biostat_HBSR_SH_noNA[biostat_HBSR_SH_noNA$status_Smsy80 == biostat_HBSR_SH_noNA$status_HS,]
biostat_HBSR_SH_noNA[biostat_HBSR_SH_noNA$status_Smsy != biostat_HBSR_SH_noNA$status_HS,]
biostat_HBSR_SH_noNA[biostat_HBSR_SH_noNA$status_Smsy80 != biostat_HBSR_SH_noNA$status_HS,]

#
#' Import the historical spawner abundance benchmark values ------
pattern <- "HS_percentiles_summary"

benchmarks_summary_HSPercent_df <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                                 wd_output = wd_output,
                                                                 region = region,
                                                                 species_all = F)

head(benchmarks_summary_HSPercent_df)



