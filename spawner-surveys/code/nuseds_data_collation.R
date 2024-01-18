

#'******************************************************************************
#' The goal of the script is to import, clean and format NuSEDS data.
#' Script based on Emma Atkinson's previous version (26-Mar-2019).
#' 
#' Files imported (from dropbox):
#' - 
#' 
#' Files produced: 
#' - 
#' 

#'******************************************************************************

# NOTE (to remove eventually): original script is:
# 1_nuseds_data_collationJun72023.R in:
# \X Drive\1_PROJECTS\1_Active\Fraser_VIMI\analysis\Compilation\Code

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

wds_l <- set_working_directories_fun(subDir = subDir_projects$spawner_surveys,
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

wd_documents <- paste(wd_project,"documents",sep="/")

wd_pop_indic_data_input_dropbox <- paste(wd_X_Drive1_PROJECTS,
                                         wds_l$wd_population_indicator_data_input_dropbox,
                                         sep = "/")

# Loading packages 
library(plyr)
library(dplyr)
library(tibble)
library(scales)
library(ggplot2)
library(readxl)
library(reshape2)
library(stringr)
library(viridis)

#
# Functions ------
source("code/functions.R")

#
# Import datasets -----

#' Import the dataframe of the NuSEDS datasets of interest:
NuSEDS_datasets_names <- NuSEDS_datasets_names_fun()

#' ** Import the NuSEDS data (all_areas_nuseds) **

# Import the all_areas_nuseds data:
all_areas_nuseds <- datasets_NuSEDS_fun(name_dataSet = NuSEDS_datasets_names$all_areas_nuseds, 
                                        from_NuSEDS_website = F, 
                                        wd = wd_data_dropbox)

colnames(all_areas_nuseds)
# View(all_areas_nuseds)

#' ** Import the NuSEDS list of CUs (conservation_unit_system_sites): **
# DFO provided files matching streams and Nuseds to full CU index 
conservation_unit_system_sites <- datasets_NuSEDS_fun(name_dataSet = NuSEDS_datasets_names$conservation_unit_system_sites, 
                                                      from_NuSEDS_website = F, 
                                                      wd = wd_data_dropbox)

#' ** Import the definition of the different fields of these two datasets **
fields_def <- nuseds_fields_definitions_fun(wd_references = wd_references_dropbox)
fields_def$all_areas_nuseds$AREA
fields_def$cu_system_sites$`Waterbody Name`


#' ** Import PSF list of CUs **
#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = F,
                                                   update_file_csv = F,
                                                   wd = wd_pop_indic_data_input_dropbox)

# Remove rows for Atlantic, Steelhead, and Kokanee #
sp_salmon_detail <- c("Chum", "Chinook", "Coho", "Pink even","Pink odd","Sockeye lake","Sockeye river")
sp_salmon <-        c("Chum", "Chinook", "Coho", "Pink","Pink","Sockeye","Sockeye")
sp_salmon_acro <-     c("CM", "CK", "CO", "PKE", "PKO", "SEL", "SER")
sp_salmon_acro_ncc <- c("CM", "CN", "CO", "PKE", "PKO",  "SX",  "SX")  # SpeciesId ; NCC Salmon Database (NCCSDB) Designation 

sp_salmon_names_acro_df <- data.frame(name = sp_salmon,
                                      name_detail = sp_salmon_detail,
                                      acronym = sp_salmon_acro,
                                      acronym_ncc = sp_salmon_acro_ncc)

all_areas_nuseds <- filter(all_areas_nuseds, !SPECIES %in% c("Steelhead","Atlantic","Kokanee"))

conservation_unit_system_sites <- filter(conservation_unit_system_sites, 
                                         SPECIES_QUALIFIED %in% sp_salmon_names_acro_df$acronym)

# unique(all_areas_nuseds$SPECIES)
# unique(conservation_unit_system_sites$SPECIES_QUALIFIED)

# Add or edit certain fields in all_areas_nuseds & conservation_unit_system_sites -----

# rename the year column
colnames(all_areas_nuseds)[colnames(all_areas_nuseds) == "ANALYSIS_YR"] <- "Year"

# add the field SPECIES to conservation_unit_system_sites
conservation_unit_system_sites$SPECIES <- NA
species <- c("Coho","Chinook","Pink","Pink","Chum","Sockeye","Sockeye")
sp_acronym_q <- c("CO","CK","PKE","PKO","CM","SEL","SER")
for(spq in sp_acronym_q){
  # spq <- sp_acronym_q[1]
  condition <- conservation_unit_system_sites$SPECIES_QUALIFIED == spq
  sp_Here <- unique(species[spq == sp_acronym_q])
  conservation_unit_system_sites$SPECIES[condition] <- sp_Here
}
unique(conservation_unit_system_sites$SPECIES)

#' Create the field "species_acronym_ncc" (previously "speciesId")
#' i.e., CN, SX instead of CK and SER or SEL in the SPECIES_QUALIFIED.
#' There is no information about the spawning habitat in all_areas_nuseds, 
#' contrary to in conservation_unit_system_sites. We consequently have to create
#' the field species_acronym_ncc in both dataset to be able to merge them after.
#' (Note that there is information
#' about the rearing locations the fieldsWATERBODY, GAZETTED_NAME, LOCAL_NAME_1,
#' LOCAL_NAME_2, POPULATION).
all_areas_nuseds$species_acronym_ncc <- conservation_unit_system_sites$species_acronym_ncc <- NA
species <- c("Coho","Chinook","Pink","Chum","Sockeye")
species_acronym_ncc <- c("CO","CN","PK","CM","SX")
for(sp in species){
  # sp <- species[3]
  sp_acroHere <- species_acronym_ncc[sp == species]
  condition <- all_areas_nuseds$SPECIES == sp
  all_areas_nuseds$species_acronym_ncc[condition] <- sp_acroHere
  condition <- conservation_unit_system_sites$SPECIES == sp
  conservation_unit_system_sites$species_acronym_ncc[condition] <- sp_acroHere
  
  # For Pink, add E and O for Even and Odd, respectively
  if(sp == "Pink"){
    all_areas_nuseds$species_acronym_ncc[all_areas_nuseds$SPECIES == sp & 
                                           all_areas_nuseds$Year %% 2 == 0] <- "PKE"
    all_areas_nuseds$species_acronym_ncc[all_areas_nuseds$SPECIES == sp & 
                                           all_areas_nuseds$Year %% 2 != 0] <- "PKO"
    
    condition <- conservation_unit_system_sites$SPECIES_QUALIFIED == "PKE"
    conservation_unit_system_sites$species_acronym_ncc[condition] <- "PKE"
    
    condition <- conservation_unit_system_sites$SPECIES_QUALIFIED == "PKO"
    conservation_unit_system_sites$species_acronym_ncc[condition] <- "PKO"
  }
}
unique(all_areas_nuseds$species_acronym_ncc)
unique(conservation_unit_system_sites$species_acronym_ncc)

#' Add the field IndexId = species_acronym_ncc + POP_ID
all_areas_nuseds$IndexId <- paste(all_areas_nuseds$species_acronym_ncc,
                                  all_areas_nuseds$POP_ID,sep="_")

conservation_unit_system_sites$IndexId <- paste(conservation_unit_system_sites$species_acronym_ncc,
                                                conservation_unit_system_sites$POP_ID,sep="_")

#
# Determine "returns" (i.e. number fish) in all_areas_nuseds -----
#' "Return" will be the column that contains the final fish count. Priority of the
#' fields to population Returns:
#'1) NATURAL_ADULT_SPAWNERS, if not available:
#'2) sum of 
#'  - NATURAL_SPAWNERS_TOTAL
#'  - ADULT_BROODSTOCK_REMOVALS (or TOTAL_BROODSTOCK_REMOVALS if not available)
#'  - OTHER_REMOVALS
#'3) TOTAL_RETURN_TO_RIVER

# Add "Return" which will contain the final number of fish
all_areas_nuseds$Returns <- all_areas_nuseds$NATURAL_ADULT_SPAWNERS         # All salmon that have reached maturity, excluding jacks (jacks are salmon that have matured at an early age).
all_areas_nuseds$Source <- "NATURAL_ADULT_SPAWNERS"                        # this field will change below depending on data availability and origin
NATURAL_ADULT_SPAWNERS_any <- !is.na(all_areas_nuseds$NATURAL_ADULT_SPAWNERS)
all_areas_nuseds$Source[!NATURAL_ADULT_SPAWNERS_any] <- NA

#' Define the sum of:
#' - "Spawner" = NATURAL_SPAWNERS_TOTAL +
#' - "Broodstock" = ADULT_BROODSTOCK_REMOVALS (or TOTAL_BROODSTOCK_REMOVALS if not available) + 
#' - "Removals" = OTHER_REMOVALS

# Spawners:
all_areas_nuseds$Spawners <- all_areas_nuseds$NATURAL_SPAWNERS_TOTAL
spawners_any <- !is.na(all_areas_nuseds$Spawners)
all_areas_nuseds$SpawnersSource[!NATURAL_ADULT_SPAWNERS_any & spawners_any] <- "NATURAL_SPAWNERS_TOTAL"

# Broodstock:
all_areas_nuseds$Broodstock <- all_areas_nuseds$ADULT_BROODSTOCK_REMOVALS
ADULT_BROODSTOCK_REMOVALS_any <- !is.na(all_areas_nuseds$ADULT_BROODSTOCK_REMOVALS)
TOTAL_BROODSTOCK_REMOVALS_any <- !is.na(all_areas_nuseds$TOTAL_BROODSTOCK_REMOVALS)
toReplace <- !ADULT_BROODSTOCK_REMOVALS_any & TOTAL_BROODSTOCK_REMOVALS_any
all_areas_nuseds$Broodstock[toReplace] <- all_areas_nuseds$TOTAL_BROODSTOCK_REMOVALS[toReplace]
all_areas_nuseds$BroodstockSource[!NATURAL_ADULT_SPAWNERS_any & ADULT_BROODSTOCK_REMOVALS_any] <- 'ADULT_BROODSTOCK_REMOVALS'
all_areas_nuseds$BroodstockSource[toReplace] <- 'TOTAL_BROODSTOCK_REMOVALS'

# Removals:
all_areas_nuseds$Removals <- all_areas_nuseds$OTHER_REMOVALS
OTHER_REMOVALS_any <- !is.na(all_areas_nuseds$OTHER_REMOVALS)
all_areas_nuseds$RemovalsSource[!NATURAL_ADULT_SPAWNERS_any & OTHER_REMOVALS_any] <- "OTHER_REMOVALS"

# Calculate Returns when !NATURAL_ADULT_SPAWNERS_any as the sum of what other 
# sources of data is available:
all_areas_nuseds$Returns[!NATURAL_ADULT_SPAWNERS_any] <- apply(
  X = all_areas_nuseds[!NATURAL_ADULT_SPAWNERS_any, c("Spawners","Broodstock","Removals")],
  MARGIN = 1, 
  FUN = sum, 
  na.rm = TRUE
)

all_areas_nuseds$Source[!NATURAL_ADULT_SPAWNERS_any] <- apply(
  X = all_areas_nuseds[!NATURAL_ADULT_SPAWNERS_any,c("SpawnersSource","BroodstockSource","RemovalsSource")], 
  MARGIN = 1, 
  FUN = function(x){
    paste(na.omit(x),collapse=" + ")}
)

# Set as NA rather than zero if no info in any column (the na.rm = T above produced)
# 0s when only NAs were available).
allNAs <- apply(
  X = all_areas_nuseds[!NATURAL_ADULT_SPAWNERS_any, c("Spawners","Broodstock","Removals")],
  MARGIN = 1, 
  FUN = function(x){all(is.na(x))}
) 
all_areas_nuseds$Returns[!NATURAL_ADULT_SPAWNERS_any][allNAs] <- NA

# Use TOTAL_RETURN_TO_RIVER if we still don't have a value
returns_any <- !is.na(all_areas_nuseds$Returns)
TOTAL_RETURN_TO_RIVER_any <- !is.na(all_areas_nuseds$TOTAL_RETURN_TO_RIVER)
all_areas_nuseds$Returns[!returns_any & TOTAL_RETURN_TO_RIVER_any] <- all_areas_nuseds$TOTAL_RETURN_TO_RIVER[!returns_any & TOTAL_RETURN_TO_RIVER_any]
all_areas_nuseds$Source[!returns_any & TOTAL_RETURN_TO_RIVER_any] <- "TOTAL_RETURN_TO_RIVER"

#
# CHECKS on all_areas_nuseds and conservation_unit_system_sites ---------

#' ** CHECK: association between POPULATION and IndexId in all_areas_nuseds **
#' It should be a ONE TO ONE relationship, except for Pink were a same 
#' POPULATION can be assocation to two Pink one, one Even, one Odd.
IndexId_POPULATION <- unique(all_areas_nuseds[,c("IndexId","POPULATION")])
sum(duplicated(IndexId_POPULATION$IndexId))    # 0
sum(duplicated(IndexId_POPULATION$POPULATION)) # 1764
duplicated_Pop <- IndexId_POPULATION$POPULATION[duplicated(IndexId_POPULATION$POPULATION)]
#' Many of these correspond to Pink Odd vs. Even population, which is normal. 

#' Check if for the Pink, it is systematically a one Even and one Odd, flag if not.
duplicated_Pop_Pink <- duplicated_Pop[grepl("[P|p]ink",duplicated_Pop)]
for(pop in duplicated_Pop_Pink){
  dataHere <- IndexId_POPULATION_Pink[IndexId_POPULATION_Pink$POPULATION == pop,]
  
  PKO <- any(sapply(X = 1:nrow(dataHere),
                    FUN = function(r){grepl("PKO",dataHere[r,]$IndexId)}))
  PKE <- any(sapply(X = 1:nrow(dataHere),
                    FUN = function(r){grepl("PKE",dataHere[r,]$IndexId)}))
  
  if(!(PKO & PKE)){
    print(dataHere)
  }
}
# All good.

#' Check if there are non-pink populations:
duplicated_Pop <- duplicated_Pop[!grepl("[P|p]ink",duplicated_Pop)]
IndexId_POP_dupli <- IndexId_POPULATION[IndexId_POPULATION$POPULATION %in% duplicated_Pop,]
IndexId_POP_dupli <- IndexId_POP_dupli[order(IndexId_POP_dupli$POPULATION),]
IndexId_POP_dupli

#' For each POPULATION, plot the population dynamics of the different IndexId.
#' In each case, retain the population that has the longest time series and 
#' remove the other one (we can see that there is barely any data for them).
IndexIdToRremove <- c()
layout(matrix(1:length(unique(IndexId_POP_dupli$POPULATION)),ncol = 2))
par(mar = c(4.5,4.5,.5,.5))
for(pop in unique(IndexId_POP_dupli$POPULATION)){
  # pop <- unique(IndexId_POP_dupli$POPULATION)[1]
  nusedsHere <- all_areas_nuseds[all_areas_nuseds$POPULATION == pop,]
  IndexIdHere <- unique(nusedsHere$IndexId)
  yr_min <- min(nusedsHere$Year)
  yr_max <- max(nusedsHere$Year)
  yrs <- yr_min:yr_max
  pop_max <- max(nusedsHere$Returns, na.rm = T)
  
  plot(NA, xlim = c(yr_min,yr_max), ylim = c(0,pop_max), 
       ylab = "Returns", xlab = "Years", main = "")
  series <- list()
  count <- 1
  for(iid in IndexIdHere){
    s_yr <- nusedsHere[nusedsHere$IndexId == iid, c("Year","Returns")]
    if(length(yrs[!yrs %in% s_yr$Year]) > 0){
      s_yr_add <- data.frame(Year = yrs[!yrs %in% s_yr$Year],
                             Returns = NA)
      s_yr <- rbind(s_yr,s_yr_add)
    }
    s_yr <- s_yr[order(s_yr$Year),]
    series[[count]] <- s_yr
    count <- count + 1
  }
  cols <- viridis(n = length(IndexIdHere))
  for(i in 1:length(series)){
    lines(x = yrs, y = series[[i]]$Returns, lty = 1, lwd = 3, col = cols[i])
  }
  legend("topright",IndexIdHere, col = cols, bty = "n", lwd = 3)
  legend("topleft",pop, bty = "n")
  
  # select the population to remove as a function of the length of the data
  dataPtsNb <- sapply(X = series, function(s){sum(!is.na(s$Returns))})
  toKeep <- dataPtsNb == max(dataPtsNb)
  IndexIdToRremove <- c(IndexIdToRremove,
                        IndexIdHere[!toKeep])
}

# Add CM_40836 because the data is insufficient
IndexIdToRremove <- c(IndexIdToRremove,"CM_40836")

# Remove them from all_areas_nuseds
all_areas_nuseds <- all_areas_nuseds[! all_areas_nuseds$IndexId %in% IndexIdToRremove,]

# Check if these populations are in conservation_unit_system_sites and remove them too
conservation_unit_system_sites$IndexId[conservation_unit_system_sites$IndexId %in%
                                         IndexIdToRremove]

conservation_unit_system_sites <- conservation_unit_system_sites[! conservation_unit_system_sites %in%
                                                                   IndexIdToRremove,]

#'** CHECK: association between GFE_ID and IndexId in both datasets **
#' It should be a ONE TO MANY relationship.
#' - For all_areas_nuseds: 
#'    - 1) check that for each IndexId there is an unique GFE_ID, if not, trouble
#'    shot.
#'    - 2) check that there is not duplicated multiple data points for a same year,
#'    if that's the case, trouble shoot.
#' - 

IndexId_GFE_ID <- unique(all_areas_nuseds[,c("IndexId","GFE_ID")])
sum(duplicated(IndexId_GFE_ID$IndexId)) # 79 --> not normal
sum(duplicated(IndexId_GFE_ID$GFE_ID)) # 9043 --> normal


duplicated_pop_nused <- c()
count <- 0
for(iid_i in 1:length(unique(all_areas_nuseds$IndexId))){
  # iid_i <- 74
  iid <- unique(all_areas_nuseds$IndexId)[iid_i]
  # print(count <- count + 1)
  all_areas_nuseds_cut <- all_areas_nuseds[all_areas_nuseds$IndexId == iid,]

  # 1) check if there is a unique GFE_ID
  gfe_id <- unique(all_areas_nuseds_cut$GFE_ID)
  if(length(gfe_id) > 1){
    print(paste0("Multiple GFE_ID at iid_i = ",iid_i))
    
    
  }
  
  # 2) check that there is not duplicated multiple data points for a same year
  yr <- all_areas_nuseds_cut$Year
  yr_duplicated <- yr[duplicated(yr)]
  if(length(yr_duplicated) > 0){
    duplicated_pop_nused <- c(duplicated_pop_nused,iid)
    print(paste("Population duplicated in all_areas_nuseds:",iid))
  }
} 
duplicated_pop_nused

duplicated_pop_nused_df <- NULL
for(iid in duplicated_pop_nused){
  # iid <- duplicated_pop_nused[3]
  all_areas_nuseds_cut <- all_areas_nuseds[all_areas_nuseds$IndexId == iid,]
  colSelect <- c("IndexId","WATERBODY","GFE_ID")
  data_unique <- unique(all_areas_nuseds_cut[,colSelect])
  
  # case where is is duplicated dates but one unique location:
  if(nrow(data_unique) == 1){
    
    yr_duplicated <- all_areas_nuseds_cut$Year[duplicated(all_areas_nuseds_cut$Year)]
    
    print("Only location but duplicated dates:")
    print(all_areas_nuseds_cut[all_areas_nuseds_cut$Year %in% yr_duplicated,])
    
    data_unique$year_start <- min(all_areas_nuseds_cut$Year)
    data_unique$year_end <- max(all_areas_nuseds_cut$Year)
    
  }else{ # case where there are multiple locations for a single IndexId
    
    year_range_m <- sapply(X = data_unique$GFE_ID, FUN = function(gfe){
      # gfe <- data_unique$GFE_ID[1]
      dataHere <- all_areas_nuseds_cut[all_areas_nuseds_cut$GFE_ID == gfe,]
      years <- dataHere$Year
      yr_range <- range(years,na.rm = T)
      if(sum(duplicated(years)) > 0){
        print(paste("Duplicated years for pop:",iid,", GFE_ID:",gfe))
      }
      return(yr_range)
    })
    data_unique$year_start <- year_range_m[1,]
    data_unique$year_end <- year_range_m[2,]
  }
  
  if(is.null(duplicated_pop_nused_df)){
    duplicated_pop_nused_df <- data_unique
  }else{
    duplicated_pop_nused_df <- rbind(duplicated_pop_nused_df,data_unique)
  }
  
  #print(unique(all_areas_nuseds_cut[condition,colSelect]))
}


duplicated_pop_sites <- c()
for(iid in unique(conservation_unit_system_sites$IndexId)){
  # iid <- unique(conservation_unit_system_sites$IndexId)[1]
  # print(count <- count + 1)
  cons_unit_syst_sites_cut <- conservation_unit_system_sites[conservation_unit_system_sites$IndexId == iid,]
  if(nrow(cons_unit_syst_sites_cut) > 1){
    duplicated_pop_sites <- c(duplicated_pop_sites,iid)
    print(paste("Population duplicated in conservation_unit_system_sites:",iid))
  }
}
duplicated_pop_sites
# "CN_7479"
# "SX_45525"
for(iid in duplicated_pop_sites){
  colSelect <- c("IndexId","SYSTEM_SITE","GFE_ID","SPECIES_QUALIFIED","CU_NAME",
                 "Y_LAT","X_LONGT","CU_LAT","CU_LONGT","CU_TYPE","CU_INDEX")
  condition <- conservation_unit_system_sites$IndexId == iid
  print(conservation_unit_system_sites[condition,colSelect])
}
conservation_unit_system_sites
all_areas_nuseds$GFE_ID
all_areas_nuseds$s

#' Look if these populations are present in all_areas_nuseds and, if that's the case,
#' compare the location using WATERBODY.
for(iid in duplicated_pop_sites){
  all_areas_nuseds_cut <- all_areas_nuseds[all_areas_nuseds$IndexId == iid,]
  if(nrow(all_areas_nuseds_cut) > 0){
    WATERBODY_here <- unique(all_areas_nuseds_cut$)
  }
  
}


# about SYSTEM_SITE, 
# https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1705361308291699?thread_ts=1705344122.088409&cid=CJ5RVHVCG


# Check 



sum(is.na(all_areas_nuseds$WATERBODY))

# looks like all_areas_nuseds$WATERBODY == conservation_unit_system_sites$SYSTEM_SITE
sites_SYSTEM_SITE <- unique(conservation_unit_system_sites$SYSTEM_SITE)
nused_WATERBODY <- unique(all_areas_nuseds$WATERBODY)
sum(nused_WATERBODY %in% sites_SYSTEM_SITE) / length(nused_WATERBODY)
sum(sites_SYSTEM_SITE %in% nused_WATERBODY) / length(sites_SYSTEM_SITE)

# For those site/waterbody that don't match, is it because of populations are not
# present in either dataset?
SYSTEM_SITE_notIn_WATERBODY <- sites_SYSTEM_SITE[! sites_SYSTEM_SITE %in% nused_WATERBODY]
WATERBODY_notIn_SYSTEM_SITE <- nused_WATERBODY[! nused_WATERBODY %in% sites_SYSTEM_SITE]
sort(SYSTEM_SITE_notIn_WATERBODY)
sort(WATERBODY_notIn_SYSTEM_SITE)

SYSTEM_SITE_notIn_WATERBODY_IndexId <- sapply(X = SYSTEM_SITE_notIn_WATERBODY, 
                                              FUN = function(ss){
                                                dataset <- conservation_unit_system_sites[conservation_unit_system_sites$SYSTEM_SITE == ss,]
                                                out <- unique(dataset$IndexId)
                                              })

WATERBODY_notIn_SYSTEM_SITE_IndexId <- sapply(X = WATERBODY_notIn_SYSTEM_SITE, 
                                              FUN = function(wb){
                                                dataset <- escapement[all_areas_nuseds$WATERBODY == wb,] # rows match in escapement and all_areas_nuseds 
                                                out <- unique(dataset$IndexId)
                                              })

#' Look for each of the populations associated with these miss-matched site
#' if (1) the it is present in the other dataset and if that's the case (2) find 
#' the name of the siwze to see if there is an typo error or not.

unlist(SYSTEM_SITE_notIn_WATERBODY_IndexId)

sites_missmatches <- data.frame(IndexId = )

for(pops_i in 1:length(SYSTEM_SITE_notIn_WATERBODY_IndexId)){
  #' pops_i <- 1
  pops <- SYSTEM_SITE_notIn_WATERBODY_IndexId[[pops_i]]
  syst_site_here <- names(SYSTEM_SITE_notIn_WATERBODY_IndexId)[pops_i]
  for(pop in pops){
    # pop <- pops[1]
    sapply(X = , FUN = function(){
      
    })
    
    
    
  }
  
  print(pops)
  
}




conservation_unit_system_sites$SPECIES

Check NuSEDS All Areas to see what the unique POP_ID is for the SYSTEM_SITE and Species combo. Seems likely that this is an error in the conservation_unit_system_sites.



# Check which population match or not between the two datasets:
IndexId_nused <- unique(escapement$IndexId)
length(IndexId_nused) # 11474
IndexId_sites <- unique(conservation_unit_system_sites$IndexId)
length(IndexId_sites) # 7143





View(all_areas_nuseds[escapement$SPECIES_QUALIFIED == "SE",])

conservation_unit_system_sites$SPECIES_QUALIFIED
conservation_unit_system_sites$POP_ID
conservation_unit_system_sites$SPECIES_QUALIFIED


escapement$IndexId <- paste(escapement$sp, PopId, sep="_")
StatArea <- Convert2StatArea(AREA)                      # AREA is the subdistrict; # QUESTION: why doing this?

# Add NCC Salmon Database Fields: SpeciesName and SpeciesId---------------------
# @TODO - could use general ConvertSpeciesCode 

# Reference table for species codes #
# spp.tab <- data.frame(
#   nccsdb   = c( "CM",       "CN",   "CO",     NA, "PKE", "PKO",      "SX",      "SX"),   # NCC Salmon Database (NCCSDB) Designation 
#   nuseds   = c("Chum", "Chinook", "Coho", "Pink",    NA,    NA, "Sockeye", "Sockeye"),     # NuSEDS
#   cu_sites = c(  "CM",      "CK",   "CO",     NA,  "PKE", "PKO",    "SEL",     "SER"), # Conservation Unit Sites
#   stringsAsFactors = FALSE
# )

sp_salmon_names_acro_df$acronym_ncc # = spp.tab$nccsdb
sp_salmon_names_acro_df$name # = spp.tab$nuseds

unique(escapement$SPECIES)
unique(all_areas_nuseds$SPECIES)

# escapement <- merge(x = escapement,
#                     y = data.frame(SpeciesName = sp_salmon_names_acro_df$name, 
#                                    SpeciesId = sp_salmon_names_acro_df$acronym_ncc),
#                     by.x = "SPECIES",
#                     by.y = "SpeciesName",
#                     all.x = TRUE)
# 
# escapement$SpeciesId[escapement$SPECIES == "Pink" & escapement$Year %% 2 == 0] <- "PKE"  # Even pink years
# escapement$SpeciesId[escapement$SPECIES == "Pink" & escapement$Year %% 2 != 0] <- "PKO"  # Odd pink years
# escapement$IndexId <- paste(escapement$SpeciesId, escapement$PopId, sep = "_")
# escapement$StatArea <- Convert2StatArea(escapement$AREA)
  
escapement <- within(
  data = merge(
    x = escapement,
    y = data.frame(SpeciesName = sp_salmon_names_acro_df$name, 
                   SpeciesId = sp_salmon_names_acro_df$acronym_ncc),
    by.x ="SPECIES",
    by.y = "SpeciesName",
    all.x = TRUE   # LEFT JOIN
  ),
  {
    SpeciesId[SPECIES == "Pink" & Year %% 2 == 0] <- "PKE"  # Even pink years
    SpeciesId[SPECIES == "Pink" & Year %% 2 != 0] <- "PKO"  # Odd pink years
    IndexId <- paste(SpeciesId, PopId, sep="_")
    StatArea <- Convert2StatArea(AREA)                      # AREA is the subdistrict
  }
)

head(escapement)
colnames(escapement)
unique(escapement$SpeciesId)

# Remove duplicated rows
nrow(escapement) # 561548
escapement <- distinct(escapement)
nrow(escapement) # 411886

#
# Subset and Sort -------------------------------------------------------

# Define the field for the meta data (???)
fields <- c("Id", "SpeciesId", "IndexId", "PopId", "Year", "StatArea", "Returns")
if(meta.data){
  # BSC: concatenate fields with column names in escapement that are not in fields
  fields <- c(fields, setdiff(colnames(escapement), fields)) # setdiff(x,y) is the same as: x[! x %in% y]
}

# Re-order columns
escapement <- escapement[,fields]

# Sort results, 1st by IndexId, then Year:
escapement <- escapement[order(escapement$IndexId, escapement$Year), ]

# Return ??? ------------------------------------------------------------------
if(na.rm){
  escapement <- subset(escapement, !is.na(Returns))
}

# Summarize escapement statistics for each stream

# First, make corrections for populations with discrepancies in area assignments #
# These errors become apparent when merging data frames later on #
escapement[escapement$IndexId == "CO_46240",]$StatArea <- "29"
escapement[escapement$IndexId == "PKO_51094",]$StatArea <- "12"  # BSC: there is one ""
escapement[escapement$IndexId == "SX_45495",]$StatArea <- "120"  # BSC: already "120"

escapement_wide <- dcast(melt(escapement[,c("IndexId","Year","Returns")], 
                              id.vars = c("IndexId", "Year")),  # wide to long format
                         IndexId + variable ~ Year,
                         value.var = "value",
                         fun.aggregate=sum, # Come back to this, currently won't let me widen data without aggregation function
                         fill = 0.12345) # Filler to make sure can distinguish true zeros

duplicates <- which(escapement_wide[,3:ncol(escapement_wide)][1] > 1) # BSC: ?!
escapement_wide$IndexId[duplicated(escapement_wide$IndexId)] # BSC
# zz <- zz[,-2] # remove column variable, bad practice
escapement_wide <- escapement_wide[,colnames(escapement_wide) != "variable"]

# Putting together extra info for final data frame #
names(escapement_wide)
names(stream.list)
names(sa.cu.lk) # BSC: not used

# Merging dataframes - THIS IS WHEN TO REMOVE BINNED CUs #
escapement_streams <- merge(stream.list, 
                            escapement_wide, 
                            by = "IndexId")





# BSC: calculate different statistics per group of unique combination of the .variables
# IndexId is = to paste(SpeciesId, PopId, sep="_")
# SpeciesId is = to sp_salmon_names_acro_df$acronym_ncc = the NCC Salmon Database (NCCSDB) Designation
# PopId is = to the all_areas_nuseds$POP_ID
# StatArea is = to Convert2StatArea(AREA), with AREA the subdistrict
esc.summary <- ddply(.data = escapement,
                     .variables = c("IndexId", "SpeciesId", "PopId", "StatArea"), 
                     .fun = summarise,
                     nrecs  = sum(!is.na(AREA)),   # Total number of NuSEDS records
                     nnumest  = sum(!is.na(Returns)), # number of observations
                     nins  = sum(ADULT_PRESENCE %in% c("NONE OBSERVED", "PRESENT")),  # nb of observations "inspected" (?)
                     npres = sum(ADULT_PRESENCE %in% c("PRESENT")),                   # nb observations with presence
                     pinsrec = (sum(ADULT_PRESENCE %in% c("NONE OBSERVED", "PRESENT")))/(sum(!is.na(AREA))), # proportion of observation inspected overall
                     ppres_ins = sum(ADULT_PRESENCE %in% c("PRESENT"))/sum(ADULT_PRESENCE %in% c("NONE OBSERVED", "PRESENT")), # proportion observations presence in the inspected ones
                     pest_pres = sum(!is.na(Returns))/sum(ADULT_PRESENCE %in% c("PRESENT")), # average returns per observation (?)
                     Escapement.total = sum(Returns, na.rm=TRUE),
                     Escapement.avg = mean(Returns, na.rm=TRUE),
                     Escapement.sd   = sd(Returns, na.rm=TRUE),
                     Escapement.se   = sd(Returns, na.rm=TRUE) / sqrt(nnumest),
                     Year.start = min(Year, na.rm=TRUE),
                     Year.end = max(Year, na.rm=TRUE)
)

nrow(esc.summary) # 11475

# BSC: check for duplicated rows
nrow(unique(esc.summary[,c("PopId","SpeciesId")])) # 11474 vs. 11475
rowDupli <- which(duplicated(esc.summary[,c("PopId","SpeciesId")]))
PopId_SpeciesId <- esc.summary[rowDupli,c("PopId","SpeciesId")]
esc.summary[esc.summary$PopId == PopId_SpeciesId$PopId & 
              esc.summary$SpeciesId == PopId_SpeciesId$SpeciesId,]

all_areas_nuseds$AREA[all_areas_nuseds$SPECIES == "Chinook" & 
                        all_areas_nuseds$POP_ID == 47367]


#' BSC: how can those be the same population?
#' Any way, it looks like there is no data for StatArea = 110:

all_areas_nuseds[all_areas_nuseds$SPECIES == "Chinook" & 
                   all_areas_nuseds$POP_ID == 47367 &
                   all_areas_nuseds$AREA == 110,]

all_areas_nuseds[all_areas_nuseds$SPECIES == "Chinook" & 
                   all_areas_nuseds$POP_ID == 47367 &
                   all_areas_nuseds$AREA == "29I",]

escapement[escapement$IndexId == "CN_47367",]

#' BSC: remove data with AREA == "29I" because it contains not data:
toRemove <- escapement$IndexId == "CN_47367" & escapement$AREA == "29I"
escapement <- escapement[!toRemove,]
toRemove <- esc.summary$IndexId == "CN_47367" & esc.summary$StatArea == "29I"
esc.summary <- esc.summary[!toRemove,]

# append available meta data
# BSC: that does not work...
stream.list <- within(
  data = merge(
    x = esc.summary,
    y = conservation_unit_system_sites,
    by.x = c("PopId","SpeciesId"),
    by.y = c("POP_ID","SpeciesId"),
    all.x = TRUE
  ),
  {
    # List of streams with confirmed escapement, used for calculations
    Active <- Escapement.total > 0   
    # List of st
    NoEscapement <- !is.na(Escapement.total) & Escapement.total == 0
    Surveyed <- !is.na(Escapement.avg)
    CU <- paste0(SpeciesId, "_", CU_INDEX)
    SITE_ID <- NA # ID  # QUESTION: this is conservation_unit_system_sites_old$X.ID. which contains only NAs and is not present in conservation_unit_system_sites ?! What's the point of it?
  }
)

nrow(stream.list) # 11477 --> there are two extra rows --> find them

# check for duplicates
nrow(unique(stream.list[,c("PopId","SpeciesId")])) # 11474 vs. 11477
rowDupli <- which(duplicated(stream.list[,c("PopId","SpeciesId")]))
PopId_SpeciesId <- stream.list[rowDupli,c("PopId","SpeciesId")]
for(i in 1:length(rowDupli)){
  out <- stream.list[stream.list$PopId == PopId_SpeciesId$PopId[i] & 
                       stream.list$SpeciesId == PopId_SpeciesId$SpeciesId[i],]
  colNo <- c("FWA_WATERSHED_CDE","WATERSHED_CDE","EFFECTIVE_DT","CMNTS","PopId","SpeciesId",
             "nrecs","nnumest","nins","npres","pinsrec","ppres_ins","pest_pres",
             "Escapement.avg","Escapement.sd","Escapement.se","Year.start","Year.end",
             "CU_TYPE","CU_INDEX","FULL_CU_IN","SBJ_ID","IS_INDICATOR","SITE_ID",
             "CU","Surveyed","NoEscapement","Active","SPECIES_QUALIFIED",
             "FAZ_ACRO","MAZ_ACRO","JAZ_ACRO","CU_NAME","CU_ACRO")
  colYes <- colnames(stream.list)[! colnames(stream.list) %in% colNo]
  print(out[,colYes])
  print("***")
}

conservation_unit_system_sites[conservation_unit_system_sites$POP_ID == 7479,]$SYSTEM_SITE

all_areas_nuseds
c("SPECIES","POP_ID")

conservation_unit_system_sites$SpeciesId

#' SP: Check NuSEDS All Areas to see what the unique POP_ID is for the SYSTEM_SITE
#' and Species combo. Seems likely that this is an error in the conservation_unit_system_sites.

conservation_unit_system_sites[conservation_unit_system_sites$POP_ID == 45525,]$SYSTEM_SITE



#' BSC: it look like for these populations (except CN_47367 above), these are 
#' duplicates because they have the same counts but location differ slightly.
#' - CN_7479: SYSTEM_SITE = BRIDGE RIVER vs. HIUIHILL CREEK
#' - SX_45525: SYSTEM_SITE = NADINA CHANNEL Artificial vs. NADINA RIVER
#' - CN_47367: same as above --> UPDATE: it was removed in esc.summary so does not appear here anymore

#' TODO: decide which duplicate row to remove --> see if matters in the end.

#
# Stream List Filtering and Fixes  ---------------------------------------------
# Drop streams without CU_INDEX values
# drop.cuindex <- subset(stream.list, is.na(CU_INDEX))
# message(nrow(drop.cuindex), " potential NCC streams were dropped from the NCC Salmon database master stream listing because no CU_INDEX value was available.")
# stream.list <- subset(stream.list, !is.na(CU_INDEX))

# Fix CU_NAME field that may have "<<VREQ[Bin]>>"
# EA: I think we want to filter out binned CUs?
# stream.list$CU_NAME <- str_replace(stream.list$CU_NAME, "<<VREQ\\[Bin\\]>>", "")

# browser()
if(legacy){
  colnames(stream.list) <- str_replace(colnames(stream.list), "^TYPE$", "CU_TYPE")
}

# Select particular fields #
# fields <- c('SpeciesId', "IndexId", "PopId",  "Records", "Surveys", "Active", "SYSTEM_SITE", "StatArea",
#             "CU", "CU_NAME",  "CU_ACRO", "CU_LATITUDE", "CU_LONGITUDE",
#             "SPECIES_QUALIFIED", "YLAT", "XLONG", "FAZ_ACRO", "MAZ_ACRO", "JAZ_ACRO",
#             "SITE_ID",  "GFE_ID", "NUMBER_OF_SITES", "CU_TYPE",
#             "SBJ_ID", "IS_INDICATOR", "OL_GRP_NM", "OL_GRP_N", "AREA", "ISENH", "COMMENTS", "GFE_ID_IN_NUSEDS", "POP_ID_IN_NUSEDS","CMNT", "EFFECTIVE_DT")
# 
# if (!all(check <- fields %in% colnames(stream.list))) {
#   stop("Final streams listing missing fields:", paste(fields[!check], collapse=", "))
# } 
# # View(stream.list[c(fields)])
# 
# stream.list <- stream.list[fields]

# CHECK Record Count -----------------------------------------------------
# Ensure our record count is accurate
# check <- merge(
#   x = stream.list[c("IndexId", "nrecs")],
#   y = as.data.frame(table(escapement$IndexId)), #  IndexId = paste(SpeciesId, PopId, sep="_")
#   by.x = "IndexId",
#   by.y = "Var1"       # i.e., IndexId
# )

#' BSC: same as above but easier to track the origin of the variables.
#' Also, should this not be done before creating stream.list ???
check <- merge(
  x = esc.summary[c("IndexId", "nrecs")],       #  nrecs  = sum(!is.na(AREA)) per unique combination of c("IndexId", "SpeciesId", "PopId", "StatArea")
  y = as.data.frame(table(escapement$IndexId)), #  IndexId = paste(SpeciesId, PopId, sep="_")
  by.x = "IndexId",
  by.y = "Var1"       # i.e., IndexId
)

if(!all(check$nrecs == check$Freq)){  # BSC:  nrecs  = sum(!is.na(AREA))
  print(check[check$nrecs != check$Freq,])
  stop("Record count error")
} 
check[check$nrecs != check$Freq,]

# Check for duplications
piv <- table(stream.list$IndexId)
piv[piv > 1] # Print which IndexIds have duplicates
stream.list[duplicated(stream.list$IndexId),c("PopId","SpeciesId","IndexId")]

# remove <- c("CO_46240", "PKO_51094", "SX_45495") # Temporary fix for populations with duplicates (from earlier check)
# remove2 <- c("SX_43790", "SX_47590", "SX_49234") # Temporary fix for populations with duplicates caught during transposing

# streams <- filter(stream.list, !(IndexId %in% remove))
# streams <- filter(streams, !(IndexId %in% remove2))

# streams <- stream.list # BSC: no t doing this , creates too many object, makes the script not tractable 

# CHECK: CU indicator ER --------------------------------------------------
# BSC: indicate all the AREAs for each CU
sa.cu.lk <- ddply(
  .data = subset(stream.list, Active = TRUE),
  .variables =   c("SpeciesId", "CU", "CU_NAME"), 
  .fun = summarise, 
  StatArea = paste(sort(unique(StatArea)), collapse=", ")
)

sa.cu.lk$StatArea[1]

# QUESTION: What are we checking here? is not used after.

# --- Part 2: Formatting output data frame with CU info and escapement info --- #

# z <- filter(escapement, !(IndexId %in% remove))
# z <- filter(z, !(IndexId %in% remove2))

# z <- escapement      #  BSC: commented out

# id <- unique(z$IndexId) # BSC: commented out until I see what it is used for.

#z[which(z$Returns==0),]$Returns <- "ZERO"

# zz <- z[,-c(1,10:18)]
# zz <- z[,c(3,5,7)]

# Widen data frame #
# zz <- dcast(melt(z[,c(3,5,7)], id.vars = c("IndexId", "Year")), # BSC Bad practices
# zz <- dcast(melt(z[,c("IndexId","Year","Returns")], 
#                  id.vars = c("IndexId", "Year")),  # wide to long format
#             IndexId + variable ~ Year,
#             value.var = "value",
#             fun.aggregate=sum, # Come back to this, currently won't let me widen data without aggregation function
#             fill = 0.12345) # Filler to make sure can distinguish true zeros


# data <- merge(stream.list, escapement_wide, by = "IndexId")

# Updating "SpeciesId" for sockeye populations with river/lake specification #
# i.e., "SX" --> "SEL" or "SER"
escapement_streams$SpeciesId <- as.character(escapement_streams$SpeciesId)

escapement_streams$SpeciesId <- ifelse(!is.na(escapement_streams$SPECIES_QUALIFIED) &
                                      escapement_streams$SPECIES_QUALIFIED == "SEL", 
                                    "SEL", escapement_streams$SpeciesId)

escapement_streams$SpeciesId <- ifelse(!is.na(escapement_streams$SPECIES_QUALIFIED) & 
                                      escapement_streams$SPECIES_QUALIFIED == "SER", 
                                    "SER", escapement_streams$SpeciesId)

# escapement_streams[escapement_streams$SpeciesId == "SX",][,c("SpeciesId","SPECIES_QUALIFIED")]

# Cleaning up data #
# escapement_streams$IndexId <- paste(escapement_streams$SpeciesId, escapement_streams$PopId,sep="_") # BSC: already done above for escapement

# remove all non-digit characters in CU_INDEX
escapement_streams$CU_INDEX <- as.character(gsub("\\D","", escapement_streams$CU_INDEX))

escapement_streams$CU_fname <- paste(escapement_streams$SpeciesId,
                                     escapement_streams$CU_NAME, sep="::")
escapement_streams$CU_facro <- paste(escapement_streams$SpeciesId,
                                     escapement_streams$CU_ACRO, sep="::")
escapement_streams$CU_findex <- escapement_streams$FULL_CU_IN

# Set all zero escapement values to NA #
# BSC: bad practices TODO: correct --> it does not work with the new conservation_unit_system_sites because certain columns are missing
# escapement_streams[,56:157][escapement_streams[,56:157] == 0.12345] <- " "
# escapement_streams[,56:157][is.na(escapement_streams[,56:157])] <- " "

col_dates <- colnames(escapement_streams)[colnames(escapement_streams) %in% as.character(1500:3000)]
escapement_streams[,col_dates][escapement_streams[,col_dates] == 0.12345] <- " "
escapement_streams[,col_dates][is.na(escapement_streams[,col_dates])] <- " "

# Check out CUs that are no longer current # BSC: AND DO WHAT?! these bin, bin1 and bin2 are not used 
noLongerCurrent <- c("bin","Bin","Deleted","VREQ[Bin]","VREQ[Extirpated]","Extirpated")
unique(escapement_streams$CU_TYPE)
bin <- filter(escapement_streams, CU_TYPE %in% noLongerCurrent)
length(unique(bin$CU_findex))

bin1 <- unique(bin$CU_findex)
bin2 <- unique(conservation_unit_system_sites$FULL_CU_IN[conservation_unit_system_sites$CU_TYPE %in% noLongerCurrent])
# bin3 <- unique(bb.cu$CU_findex[bb.cu$CU_type %in% noLongerCurrent]) # BSC: ?!
# setdiff(bin2, bin3)
setdiff(bin1, bin2)

# Put together final data frame #
fields <- c("PopId","SpeciesId","GFE_ID","SYSTEM_SITE","Y_LAT","X_LONGT",
            "FAZ_ACRO","MAZ_ACRO","JAZ_ACRO","CU_fname","CU_facro","CU_findex",
            "CU_NAME","CU_ACRO","CU_INDEX",
            # "ISENH",
            "IS_INDICATOR","nrecs","nins",
            "npres","nnumest","pinsrec","ppres_ins","pest_pres",
            # "OL_GRP_NM", # Group of statistical areas (https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/359596.pdf)
            "StatArea","IndexId","NoEscapement")

# BSC: what are the fields "ISENH" and "OL_GRP_NM"?

fields[! fields %in% colnames(escapement_streams)] # "ISENH"     "OL_GRP_NM"
c("ISENH","OL_GRP_NM")[! c("ISENH","OL_GRP_NM") %in% colnames(all_areas_nuseds)]
c("ISENH","OL_GRP_NM")[! c("ISENH","OL_GRP_NM") %in% colnames(conservation_unit_system_sites)]
c("ISENH","OL_GRP_NM")[! c("ISENH","OL_GRP_NM") %in% colnames(template_df)]

colnames(all_areas_nuseds)[grepl(pattern = "ENH",colnames(all_areas_nuseds))]
colnames(all_areas_nuseds)[grepl(pattern = "enh",colnames(all_areas_nuseds))]
colnames(conservation_unit_system_sites)[grepl(pattern = "ENH",colnames(conservation_unit_system_sites))]
colnames(conservation_unit_system_sites)[grepl(pattern = "enh",colnames(conservation_unit_system_sites))]


# re-order columns
# escapement_streams <- cbind(escapement_streams, escapement_streams[,56:157])
escapement_streams <- cbind(escapement_streams[,fields], escapement_streams[,col_dates])

# Add extra columns #
length <- length(escapement_streams$IndexId)
extras <- data.frame(ID = rep(NA, length),
                     Indicator = rep(NA, length),
                     Source = rep("NuSEDS", length),
                     popMAP = rep(NA, length),
                     EXPN = rep(NA, length),
                     Area = escapement_streams$StatArea,
                     IsInFiltEsc = rep(NA, length),
                     fr_timing = rep(NA, length),
                     Fraser_mnemonic = rep(NA, length),
                     CD_findex.name = rep(NA, length),
                     Reviewer = rep(NA, length),
                     QA = rep(NA, length),
                     Method = rep(NA, length),
                     Race = rep(NA, length),
                     WildCode = rep(NA, length),
                     WildRigor = rep(NA, length),
                     PopId = rep(NA, length),
                     SpeciesId2 = rep(NA, length))  # BSC: ?! there is no SpeciesId2 field in template_df

# Put it all together #                     
escapement_streams <- cbind(escapement_streams, extras)

final.fields <- c("ID","PopId","Indicator","SpeciesId","GFE_ID",
                  "SYSTEM_SITE","Source","popMAP","EXPN","Y_LAT",
                  "X_LONGT","FAZ_ACRO","MAZ_ACRO","JAZ_ACRO","CU_fname",
                  "CU_facro","CU_findex","CU_NAME","CU_ACRO","CU_INDEX",
                  "Area",
                  # "ISENH",
                  "IS_INDICATOR","IsInFiltEsc","nrecs","nins",
                  "npres","nnumest","pinsrec","ppres_ins","pest_pres","fr_timing",
                  # "OL_GRP_NM",
                  "Fraser_mnemonic","CD_findex.name","StatArea","Reviewer",
                  "QA","Method","Race","WildCode","WildRigor","IndexId","PopId","SpeciesId2",
                  "NoEscapement")

# All the columns in the right order #
# final <- ZZ[,final.fields]
# final <- cbind(final,escapement_streams[,c(29:130)])
final <- cbind(escapement_streams[,final.fields],escapement_streams[,col_dates])
final.fields[!final.fields %in% colnames(escapement_streams)] # "ISENH"     "OL_GRP_NM"

# Remove populations with no escapement data at all #
final <- filter(final, !(NoEscapement))
final <- final[,colnames(final) != "NoEscapement"]
# remove ???
# final <- final[,-46] # BSC: SpeciesId2 ??? --> NoEscapement (?)
colnames(final)[!colnames(final) %in% colnames(template_df)]

# Rename the columns per reference data frame #
# colnames(final)[1:45] <- colnames(template_df)[1:45] # BSC: better edit the column names as it is more transparent
colnames(final[1:45])
colnames(template_df[1:46])

colnames(final)[colnames(final) %in% c("PopId")] <- "POP_ID"

# ???
unique(final$SpeciesId)
unique(template_df$SPP)
unique(final$SpeciesId2)
unique(template_df$SpeciesId)

colnames(final)[colnames(final) %in% c("SYSTEM_SITE")] <- "SYS_NM"
tolower(final$SYSTEM_SITE)[tolower(final$SYSTEM_SITE) %in% tolower(template_df$SYS_NM)]
tolower(final$SYSTEM_SITE)[!tolower(final$SYSTEM_SITE) %in% tolower(template_df$SYS_NM)]

colnames(final)[colnames(final) %in% c("Source")] <- "source"

colnames(final)[colnames(final) %in% c("Y_LAT")] <- "yLAT"
colnames(final)[colnames(final) %in% c("X_LONGT")] <- "xLONG"

colnames(final)[colnames(final) %in% c("FAZ_ACRO","MAZ_ACRO","JAZ_ACRO")] <- tolower(colnames(final)[colnames(final) %in% c("FAZ_ACRO","MAZ_ACRO","JAZ_ACRO")])

colnames(final)[colnames(final) %in% c("CU_NAME")] <- "CU_name"
colnames(final)[colnames(final) %in% c("CU_ACRO")] <- "CU_acro"
colnames(final)[colnames(final) %in% c("CU_INDEX")] <- "CU_index"

# ???
unique(template_df$SEP_ENH) # "N" "Y"

colnames(final)[colnames(final) %in% c("IS_INDICATOR")] <- "IsIndicator"

colnames(final)[colnames(final) %in% c("pinsrec")] <- "pins_rec"

colnames(final)[colnames(final) %in% c("IS_INDICATOR")] <- "IsIndicator"

# ???
unique(template_df$OL_GRP_NM)

colnames(final)[colnames(final) %in% c("PopId.1")] <- "PopId"


# Write to file #
# setwd(dir.out)

#hardcode fixes to nuseds

#babine/onerka
final$CU_findex[final$POP_ID==49379] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49384] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49354] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==45452] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49389] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49394] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49404] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49419] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49399] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49424] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49434] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==49439] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48599] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48674] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==45462] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48684] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48064] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48069] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48074] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48094] <- "SEL-21-02-EW"
final$CU_findex[final$POP_ID==48099] <- "SEL-21-02-EW"

#nilkitkwa
final$CU_findex[final$POP_ID==49369] <- "SEL-21-02-LW"
final$CU_findex[final$POP_ID==49374] <- "SEL-21-02-LW"
final$CU_findex[final$POP_ID==49359] <- "SEL-21-02-LW"
final$CU_findex[final$POP_ID==49364] <- "SEL-21-02-LW"
final$CU_findex[final$POP_ID==49457] <- "SEL-21-02-LW"

# tahlo/morrison
final$CU_findex[final$POP_ID==49409] <- "SEL-21-02-MW"
final$CU_findex[final$POP_ID==49414] <- "SEL-21-02-MW"

#babine enhanced
final$CU_findex[final$POP_ID==3237] <- "SEL-21-02-F"
final$CU_findex[final$POP_ID==45467] <- "SEL-21-02-F"
final$CU_findex[final$POP_ID==45472] <- "SEL-21-02-F"
final$CU_findex[final$POP_ID==3238] <- "SEL-21-02-F"
final$CU_findex[final$POP_ID==45482] <- "SEL-21-02-F"

### bella coola chum
final$CU_findex[final$POP_ID==3119] <- "CM-16"
final$CU_findex[final$POP_ID==51771] <- "CM-16"
final$CU_findex[final$POP_ID==51772] <- "CM-16"
final$CU_findex[final$POP_ID==3143] <- "CM-16"
final$CU_findex[final$POP_ID==3122] <- "CM-16"
final$CU_findex[final$POP_ID==3125] <- "CM-16"
final$CU_findex[final$POP_ID==3138] <- "CM-16"
final$CU_findex[final$POP_ID==3128] <- "CM-16"
final$CU_findex[final$POP_ID==51778] <- "CM-16"


d1<- filter(final, POP_ID=='3119')

d1<- filter(all_areas_nuseds, WATERBODY=='AIRPORT SIDE CHANNEL')

date <- Sys.time()
date <- substr(x = date,start = 1, stop = 10)
date <- gsub("-","",date)
# write.csv(final, "NuSEDS_escapement_data_collated_20230818.csv", row.names=FALSE)
write.csv(final,paste0(wd_output,"/NuSEDS_escapement_data_collated_",date,".csv"),
                       row.names = FALSE)

# Notes:
# Remove CU's which are "BIN" at this stage? 










