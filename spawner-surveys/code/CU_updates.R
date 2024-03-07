


#'******************************************************************************
#' The goal of the script is to 
#' 
#' Previous script: Fraser_salmon_CU_updates.Rmd
#' 
#' 
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
library(tidyr)

#
date <- "20240306"
nuseds <- read.csv(paste0(wd_output,"/NuSEDS_escapement_data_collated_",date,".csv"),
                   header = T)
head(nuseds)


#' Import the name of the different datasets in the PSF database and their 
#' corresponding CSV files.
datasetsNames_database <- datasetsNames_database_fun()

fromDatabase <- F
update_file_csv <- F

#' Import streamlocationids to obtain the streamID 
streamlocationids <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[8],
                                           fromDatabase = fromDatabase,
                                           update_file_csv = update_file_csv,
                                           wd = wd_pop_indic_data_input_dropbox)

head(streamlocationids)

#' Import the conservationunits_decoder.csv from population-indicators/data_input or 
#' download it from the PSF database.
#' # To obtain the generation length and calculate the the "current spawner abundance".
conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                   fromDatabase = fromDatabase,
                                                   update_file_csv = update_file_csv,
                                                   wd = wd_pop_indic_data_input_dropbox)

head(conservationunits_decoder)

#' step 1: provide a cuid to each row in nuseds using conservationunits_decoder
#' using CU_name and cu_name_dfo:

unique(nuseds$Species)
unique(conservationunits_decoder$species_abbr)

conservationunits_decoder$cu_name_dfo

nuseds$cuid <- apply(X = nuseds, 1, function(r){
  # r <- nuseds[1,]
  cu_name_here <- tolower(r["CU_name"])
  species_here <- r["Species"]
  cond <- tolower(conservationunits_decoder$cu_name_dfo) == cu_name_here & 
    conservationunits_decoder$species_abbr == species_here
  cuids_here <- conservationunits_decoder$cuid[cond]
  if(length(cuids_here) == 0){
    cuids_here <- NA
  }
  return(cuids_here)
}) %>% unlist()

sum(is.na(nuseds$cuid)) # 3551


#' nuseds$CU_name does not correspond exactly to conservationunits_decoder$cu_name_pse 
#' not to conservationunits_decoder$cu_name_dfo
#' Characters to replace by ""
#' - " ", "-"

nuseds$cuid <- NA

CU_name_species <- unique(nuseds[,c("CU_name","Species","CU_TYPE")])

chara <- c(" ","-")
conservationunits_decoder$cu_name_pse_modif <- tolower(conservationunits_decoder$cu_name_pse)
conservationunits_decoder$cu_name_dfo_modif <- tolower(conservationunits_decoder$cu_name_dfo)
CU_name_species$CU_name_modif <- tolower(CU_name_species$CU_name)
                                         
for(c in chara){
  conservationunits_decoder$cu_name_pse_modif <- gsub(c,"",conservationunits_decoder$cu_name_pse_modif)
  conservationunits_decoder$cu_name_dfo_modif <- gsub(c,"",conservationunits_decoder$cu_name_dfo_modif)
  CU_name_species$CU_name_modif <- gsub(c,"",CU_name_species$CU_name_modif)
}

conservationunits_decoder$taken <- F

count <- 1
for(r in 1:nrow(CU_name_species)){
  # r <- 158
  cu_name_here <- tolower(CU_name_species$CU_name[r])
  species_here <- CU_name_species$Species[r]         # "CM"  "CK"  "CO"  "PKE" "PKO" "SER" "SEL"
  cu_type_here <- CU_name_species$CU_TYPE[r]
  cu_name_here_modif <- CU_name_species$CU_name_modif[r]
  
  cond <- (grepl(cu_name_here_modif,conservationunits_decoder$cu_name_dfo_modif) | 
             grepl(cu_name_here_modif,conservationunits_decoder$cu_name_pse_modif)) & 
    conservationunits_decoder$species_abbr == species_here
  
  
  case with sum(cond) > 1
  
  if(sum(cond) == 0){
    
    # apply more tricks
    if(grepl("river",cu_name_here_modif)){
      cu_name_here_modif <- gsub("river","",cu_name_here_modif)
    }
    
    cond <- (grepl(cu_name_here_modif,conservationunits_decoder$cu_name_dfo_modif) | 
               grepl(cu_name_here_modif,tolower(conservationunits_decoder$cu_name_pse_modif))) & 
      conservationunits_decoder$species_abbr == species_here
    
  }
  
  if(sum(cond) == 0){ # if still not found after the tricks
    cuids_here <- NA
    print(paste(count,r,species_here,cu_name_here,cu_type_here,cu_name_here_modif, sep = " - "))
    count <- count + 1
    
  }else{
    cuids_here <- unique(conservationunits_decoder$cuid[cond])
    conservationunits_decoder$taken[cond] <- T
    
    if(length(cuids_here) != 1){
      print(paste("***",r,"***"))
    }
    
    cond <- nuseds$CU_name == CU_name_species$CU_name[r] &
      nuseds$Species == CU_name_species$Species[r]
    nuseds$cuid[cond] <- cuids_here
  }
}

unique(nuseds$cu_)


CU_name_variations_fun(CUname = "spiller-fitz hugh-burke")


"spiller-fitz hugh-burke"

cond <- grepl('[W|w]hite',nuseds$CU_name)
nuseds[cond,]



[1] "2 - 9 - CM - southwest vancouver island - Current - southwestvancouverisland"
[1] "3 - 46 - CK - fraser-cross-cu supplementation exclusion - Bin - frasercrosscusupplementationexclusion"
[1] "4 - 48 - CK - north and central coast-early timing - Current - northandcentralcoastearlytiming"
[1] "5 - 60 - CK - south-miscellaneous - Bin - southmiscellaneous"
[1] "6 - 64 - CK - north and central coast-late timing - Current - northandcentralcoastlatetiming"
[1] "7 - 65 - CK - (p)hatchery exclusion-pallant creek - Bin - (p)hatcheryexclusionpallantcreek"
[1] "8 - 84 - CK - fraser-miscellaneous - Bin - frasermiscellaneous"
[1] "9 - 103 - CK - southern bc-cross-cu supplementation exclusion - Bin - southernbccrosscusupplementationexclusion"
[1] "10 - 113 - CK - fraser-harrison fall transplant_fa_0.3 - Bin - fraserharrisonfalltransplant_fa_0.3"
[1] "11 - 125 - CO - hg-graham island lowlands - Current - hggrahamislandlowlands"
[1] "12 - 136 - CO - hg-west - Current - hgwest"
[1] "13 - 137 - CO - hg-east - Current - hgeast"

pattern <- "yukon"
cond <- grepl(tolower(pattern),tolower(conservationunits_decoder$cu_name_dfo_modif))
cond <- grepl(tolower(pattern),tolower(conservationunits_decoder$cu_name_pse_modif))
cond <- grepl(tolower(pattern),tolower(conservationunits_decoder$cu_name_dfo))
cond <- grepl(tolower(pattern),tolower(conservationunits_decoder$cu_name_pse))
conservationunits_decoder[cond,c("species_abbr","cu_name_dfo","cu_name_pse")]

cond <- grepl("river",conservationunits_decoder$cu_name_pse)
conservationunits_decoder[cond,c("species_abbr","cu_name_dfo","cu_name_pse")]

cond <- tolower(conservationunits_decoder$cu_name_dfo) == "interior fraser" & 
  conservationunits_decoder$species_abbr == species_here

cond <- grepl("interior fraser",tolower(conservationunits_decoder$cu_name_dfo))
conservationunits_decoder[cond,]

"quesnel-summer timing"
cond <- grepl("quesnel",tolower(conservationunits_decoder$cu_name_dfo))
conservationunits_decoder[cond,]

cond <- grepl(pattern = "vreq", x = conservationunits_decoder$cu_name_dfo)
conservationunits_decoder[cond,]





