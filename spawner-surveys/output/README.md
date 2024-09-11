
# Below are the files produced by each R script. The symbol * means that the file
# is used in a next script. "NuSEDS" and "CUSS" refer to all_areas_nuseds and 
# conservation_unit_system_sites, respectively. 
# Abbrevations: Tranboundary region (TBR), steelhead (SH)

# All these files (except dataset2_spawner_surveys_dummy.csv) are exported in the 
# PFS's dropbox repository. The file dataset2_spawner_surveys_dummy.csv is exported 
# in the local /output; it contains the first two rows of dataset2_spawner_surveys_DATE.csv.
# The goal is to push the file to github to record the details of the update.

# 1_nuseds_collation.R (in the dropbox output/archive folder)

* 1_all_areas_nuseds_cleaned_DATE.csv
  . Remove rows for Atlantic, Steelhead, and Kokanee
  . Remove the IndexId & GFE_ID time series in NUSEDS with only NAs and/or 0s
  . Fix populations (IndexId - GFE_ID series)
  . Remove unmatched populations (not present in either NuSEDS or CUSS) with <= 3 data points
  
* 1_conservation_unit_system_sites_cleaned_DATE.csv 
  . Remove rows for Atlantic, Steelhead, and Kokanee
  . Fix coordinates of certain locations in CUSS (X_LONGT, Y_LAT)
  . Fix populations (IndexId - GFE_ID series)

* 1_NuSEDS_escapement_data_collated_DATE.csv *
  . Merge of 1_all_areas_nuseds_cleaned and 1_conservation_unit_system_sites_cleaned
  . Replace 0s by NAs
  . Fix when multiple POP_ID associated to the same CU are in one stream
  
* 1_series_inNUSEDS_noInCUSS_DATE.csv
  . Series removed because no alternative series could be found in CUSS

* 1_series_removed_DATE.csv
  . Series removed from either datasets and why
  
* 1_series_added_DATE.csv
  . Series added to either dataset and why


# 2_nuseds_cuid_streamid.R (in the dropbox output/archive folder)

- 2_Nuseds_cuid_streamid_DATE.csv * 
 . Attribute PSE's "cuid" to each POP_ID in 1_NuSEDS_escapement_data_collated
 . Edit FULL_CU_IN for several POP_IDs
 . Fix the CU attribution for certain time series 
 . Add field "stream_survey_quality" and "survey_score"
 . Edit field "ESTIMATE_METHOD"
 . Add field "streamed" (= unique cuid & GFE_ID combination)
 . Edit location coordinates eventually to match PSE


# 3_data_extra_Reynolds_lab.R

- 3_data_extra_Reynolds_lab_DATE.csv *  (NOT INTEGRATED YET)
  . Fix certain data points of the original file
  . Structure their dataset like in 2_Nuseds_cuid_streamid. 

- SFU_Escapement_issues.csv
  . data points in their dataset originating from NUSEDS but with a different value


# 4_datasets_for_PSE.R

-  dataset2_spawner_surveys_DATE.csv (previously dataset_1part2_DATE.csv)
  . Remove TBR, Columbia and Yukon from 2_Nuseds_cuid_streamid_DATE
  . Add the data for SH, Columbia, TBR and Yukon to 2_Nuseds_cuid_streamid_DATE 
    from their respective alternative sources
  . Combine 2_Nuseds_cuid_streamid with 3_data_extra_Reynolds_lab_DATE.csv (NOT INTEGRATED YET)

-  dataset2_spawner_surveys_dummy.csv
  . The file contains the first two rows of dataset2_spawner_surveys_DATE.csv


