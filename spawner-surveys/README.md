
This sub-folder concerns the integration of stream-level spawner data for the PSE.
The goal of the scripts is to import, clean and format the spawner data from the
New Salmon Escapement Database System (NuSEDS; https://open.canada.ca/data). The 
data is contained in two datasets: **all_areas_nuseds** (NuSEDS) and 
**conservation_unit_system_sites** (CUSS).

The spawner data for steelhead (SH) and the regions Columbia, Transboundary (TBR)
and Yukon is processed outside of this github repository because the sources and
treatment of this data is more complex. These data are integrated to the rest of 
the dataset processed in this sub-directory in script **4_datasets_for_PSE.R**.

Complementary data was provided by the Reynolds' Lab (SFU). It is processed in its
own script **3_data_extra_Reynolds_lab.R** and it is integrated to the rest of the 
dataset in  script **4_datasets_for_PSE.R**.

Below is the list of files imported and exported in each R script with relevant 
information. The files exported in bold are the ones imported to a script in a 
subsequent step of the workflow.

All the files produced are exported in the PFS's dropbox repository, except 
dataset2_spawner_surveys_dummy.csv. The latter is exported in the local /output
and contains the first two rows of the final dataset2_spawner_surveys_DATE.csv.
The goal is to push the file to github to record the details of the updates made
to the dataset.


# 1_nuseds_collation.R

## Files imported:

* all_areas_nuseds_DATE.csv
  - File downloaded from https://open.canada.ca/data
  
* conservation_unit_system_sites_DATE.csv
  - File downloaded from https://open.canada.ca/data
  
* nuseds_report_definitions.csv
  - File downloaded from https://open.canada.ca/data
  - Definition of the fields in all_areas_nuseds

* conservation_unit_report_definitions.csv
  - File downloaded from https://open.canada.ca/data
  - Definition of the fields in conservation_unit_system_sites

* DFO_GFE_IDs_list_1.xlsx and DFO_GFE_IDs_list_2.xlsx
  - List of streams provided by Wu Zhipeng (DFO)
  - Used for the GFE_ID
  
* conservationunits_decoder.csv
  - List of CUs present in the PSE database 

* streamlocationids.csv
  - List of streams present in the PSE database with associated CUs
  - Only used for check ups


## File exported:

* 1_all_areas_nuseds_cleaned_DATE.csv
  - Remove rows for Atlantic, Steelhead, and Kokanee
  - Remove the IndexId & GFE_ID time series in NUSEDS with only NAs and/or 0s
  - Fix populations (IndexId - GFE_ID series)
  - Remove unmatched populations (not present in either NuSEDS or CUSS) with <= 3 data points
  
* 1_conservation_unit_system_sites_cleaned_DATE.csv 
  - Remove rows for Atlantic, Steelhead, and Kokanee
  - Fix coordinates of certain locations in CUSS (X_LONGT, Y_LAT)
  - Fix populations (IndexId - GFE_ID series)

* **1_NuSEDS_escapement_data_collated_DATE.csv**
  - Merge of 1_all_areas_nuseds_cleaned and 1_conservation_unit_system_sites_cleaned
  - Replace 0s by NAs
  - Fix when multiple POP_ID associated to the same CU are in one stream
  
* 1_series_inNUSEDS_noInCUSS_DATE.csv
  - Series removed because no alternative series could be found in CUSS

* 1_series_removed_DATE.csv
  - Series removed from either datasets and why
  
* 1_series_added_DATE.csv
  - Series added to either dataset and why


# 2_nuseds_cuid_streamid.R

## Files imported:

* **1_NuSEDS_escapement_data_collated_DATE.csv**
  - File produced in 1_nuseds_collation.R

* conservationunits_decoder.csv
  - List of CUs present in the PSE database 
  
* streamspawnersurveys_output.csv
  - List of streams present in the PSE database with associated CUs
  - Only used for check ups

* streamlocationids.csv
  - List of streams present in the PSE database with associated CUs
  - Only used for check ups


## File exported:

* **2_Nuseds_cuid_streamid_DATE.csv** 
  - Attribute PSE's "cuid" to each POP_ID in 1_NuSEDS_escapement_data_collated
  - Edit FULL_CU_IN for several POP_IDs
  - Fix the CU attribution for certain time series 
  - Add field "stream_survey_quality" and "survey_score"
  - Edit field "ESTIMATE_METHOD"
  - Add field "streamed" (= unique cuid & GFE_ID combination)
  - Edit location coordinates eventually to match PSE


# 3_data_extra_Reynolds_lab.R

## Files imported:

* SFU_Escapement_PSF.xlsx
  - The Reynolds's Lab main excel file

* SFU_stream_Coordinates.xlsx
  - The Reynolds's Lab stream coordinates used

* **2_nuseds_cuid_streamid_nuseds_DATE.csv**
  - File produced in 2_nuseds_cuid_streamid.R
  
* DFO_All_Streams_Segments_20240408.xlsx
  - List of streams provided by Wu Zhipeng (DFO)
  - Used for the GFE_ID
  - Same as DFO_GFE_IDs_list_1.xlsx and DFO_GFE_IDs_list_2.xlsx but with all the streams


## File exported:

* **3_data_extra_Reynolds_lab_DATE.csv**  (NOT INTEGRATED YET)
  - Fix certain data points of the original file
  - Structure their dataset like in 2_Nuseds_cuid_streamid. 

* **SFU_Escapement_issues.csv**
  - Data points in their dataset originating from NUSEDS but with a different value


# 4_datasets_for_PSE.R

## Files imported:

* **2_nuseds_cuid_streamid_nuseds_DATE.csv**
  - File produced in 2_nuseds_cuid_streamid.R
  
* **3_data_extra_Reynolds_lab_DATE.csv** and **SFU_Escapement_issues.csv** (NOT INTEGRATED YET)
  - File produced in 3_data_extra_Reynolds_lab.R

* /.../steelhead_dataset_1part2.DATE.csv                             
  - Dataset generated in its own repository
  
* /data-input/columbia_dataset_1part2.DATE.csv                      
  - Dataset generated in its own repository
  
* /data-input/yukon_dataset_1part2.DATEYukon.csv
  - Dataset generated in its own repository

* /data-input/transboundary-data/output/dataset2_spawner_surveys.csv 
  - Dataset generated in its own repository


## File exported:

*  **dataset2_spawner_surveys_DATE.csv** (previously dataset_1part2_DATE.csv)
  - Remove TBR, Columbia and Yukon from 2_Nuseds_cuid_streamid_DATE
  - Add the data for SH, Columbia, TBR and Yukon to 2_Nuseds_cuid_streamid_DATE 
    from their respective alternative sources
  - Combine 2_Nuseds_cuid_streamid with 3_data_extra_Reynolds_lab_DATE.csv (NOT INTEGRATED YET)

*  dataset2_spawner_surveys_dummy.csv
  - The file contains the first two rows of dataset2_spawner_surveys_DATE.csv


