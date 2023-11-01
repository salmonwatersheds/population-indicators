
# 
rm(list = ls())
graphics.off()

# Set directory to /biological-status
if(!grepl(pattern = "hatchery-releases", x = getwd())){
  setwd(dir = paste0(getwd(),"/hatchery-releases"))
}


# Define subdirectories:
wd_code <- paste0(getwd(),"/code")
wd_data <- paste0(getwd(),"/data")

library(readxl)
library(tidyverse)

source(paste(wd_code,"functions.R",sep = "/"))

#' *** Import the latest 

# NOTES:
# - Eric: the only trick will be translating the "STOCK_CU_INDEX" field into 
# "cuid_broodstock". Will have to use one of the tables in the decoder repo. 
# Let me know if you have any other questions.
# - decoder tables:
# C:\Users\bcarturan\Salmon Watersheds Dropbox\Bruno Carturan\X Drive\1_PROJECTS\1_Active\Population Methods and Analysis\decoders\tables

# decoder repo:
# C:\Users\bcarturan\Salmon Watersheds Dropbox\Bruno Carturan\X Drive\1_PROJECTS\1_Active\Population Methods and Analysis\decoders

# Data obtained from DFO:
dataDFO <- read_excel(paste0(wd_data,"/PSF_modified_SEP_releases_2023.xlsx"),
                      sheet = 1)

head(dataDFO)
colnames(dataDFO)
# apply(X = dataDFO, MARGIN = 2, FUN = unique)
dataDFO$STOCK_CU_INDEX

# Template:
templSheet1 <- read_excel(paste0(wd_data,"/SWP_hatchery_data_template.xlsx"),
                          sheet = "DataProvider")
templSheet2 <- read_excel(paste0(wd_data,"/SWP_hatchery_data_template.xlsx"),
                          sheet = "DataEntry_facilities")
templSheet3 <- read_excel(paste0(wd_data,"/SWP_hatchery_data_template.xlsx"),
                          sheet = "DataEntry_facilitiescuids")
templSheet4 <- read_excel(paste0(wd_data,"/SWP_hatchery_data_template.xlsx"),
                          sheet = "DataEntry_releases")
# Sheet 1 -----
# QUESTION: do I need to fill sheet one as well?
templSheet1
dim(templSheet1)
fields_s1 <- templSheet1$`Pacific Salmon Foundation Salmon Watersheds Program` 
fields_s1 <- fields_s1[!is.na(fields_s1)]
fields_s1 <- fields_s1[3:length(fields_s1)]

# "First Name"         --> ???
# "Last Name"          --> ???
# "Organization"       --> ??? 
# "Email"              --> ???
# "Metadata file name" --> ???
unique(dataDFO$PROJ_NAME)
unique(dataDFO$REL_CU_NAME)
unique(dataDFO$RELEASE_SITE_NAME)

# Sheet 2 ----
colnames(templSheet2)

# "facilityid" ??? --> need a look up file, 
unique(dataDFO$)

# "program"  ???
unique(dataDFO$PROGRAM_CODE) # --> no, "program" is for instance: "Public Involvement Unit"

# "project"
unique(dataDFO$PROJ_NAME)

# "facilityname"
unique(dataDFO$FACILITY_NAME)  # ???

# "CUID"  --> not in the more recent template
unique(dataDFO$STOCK_CU_ID)   # cuid_broodstock 

# "facility_latitude"
unique(dataDFO$FACILITY_LATITUDE)

# "facility_longitude
unique(dataDFO$FACILITY_LONGITUDE)

# startyear"
unique(dataDFO$START_DATE)

# "endyear" 
unique(dataDFO$END_DATE)

# Sheet 3 -----
colnames(templSheet3)

# "facilityID" ???

# "CUID" 
unique(dataDFO$STOCK_CU_ID)

# sheet 4 -------

colnames(templSheet4)

# "species"
unique(dataDFO$SPECIES_NAME)

# "release_site_latitude"
unique(dataDFO$REL_LATITUDE)

# "release_site_longitude"
unique(dataDFO$REL_LONGITUDE)

# "release_site_name"
unique(dataDFO$RELEASE_SITE_NAME)

# "release_stage"
unique(dataDFO$RELEASE_STAGE_NAME)

# "release_site_CUID"
unique(dataDFO$REL_CU_INDEX)

# "facilityID" ???
unique(dataDFO$ID)

# "cuid_broodstock"        
# "release_date"           
# "total_release" 





# The PSF/SWP data set ----

SWP_df <- read_excel(paste0(wd_data,"/PSF_modified_SEP_releases_2023.xlsx"))
head(SWP_df)
colnames(SWP_df)
sort(unique(SWP_df$BROOD_YEAR))
unique(SWP_df$SPECIES_NAME)
unique(SWP_df$REL_CU_NAME)
unique(SWP_df$STOCK_NAME)
unique(SWP_df$STOCK_CU_INDEX)
unique(SWP_df$STOCK_GFE_ID)
unique(SWP_df$STOCK_CU_ID)
unique(SWP_df$FACILITY_NAME)


matchCol_df <- matching_columns_fun(wd_data)








