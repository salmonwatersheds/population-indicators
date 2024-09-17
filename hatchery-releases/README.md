
# hatchery-release

## Overview

The goal of the script is to update the hatchery release data for all region 
and species except Transboundary (TBR) and steelhead (SH). The script consists
in formatting the dataset sent by DFO (**PSF_modified_SEP_releases.xlsx** file) for
the PSE.

The data for TBR and SH is created elsewhere and uploaded to the database 
separately. However, it is imported here to edit the field "location_name_pse", 
and exported again. This convoluted workflow will eventually be removed and 
the hatchery data will be exported in one unique (**PSF_modified_SEP_releases.xlsx**)
file.

See the [Tech Report: 4.1.1.3 Hatchery Releases](https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#benchmarks-biostatus) for more details.


## Scripts & files

### data_formatting.R

#### Files imported:

* conservationunits_decoder.csv
  - List of CUs present in the PSE database

* PSF_modified_SEP_releases_2023.xlsx         
  - Source file sent by DFO to Eric

* SWP_hatchery_data_template.xlsx             
  - Use for formatting the output dataset
  
* dataset384_hatchery_releases_YYYY-MM-DD.csv
  - The hatchery release dataset downloaded from the database


#### Files exported:

* SWP_hatchery_data_DATE.xlsx
  - The formatted and edited hatchery release data

* cuid_broodstock_multi.csv
  - List of instances where there are multiple cuid_broodstock for a same release_site_name-release_stage-release_site_CUID-release_date combination
  - For check up

* dataset384_hatchery_releases_SH_YYYY-MM-DD.csv
  - The edited dataset384_hatchery_releases_YYYY-MM-DD.csv for SH

* dataset384_hatchery_releases_TBR_YYYY-MM-DD.csv
  - The edited dataset384_hatchery_releases_YYYY-MM-DD.csv for TBR

