
# trends

## Overview

This sub-folder concerns the CU-level spawner abundance trend analysis. The 
analysis consists in fitting a linear model to the smoothed and log-transformed
estimated spawner abundance data to determine the total and average annual % change.
The analysis is conducted on (1) the entire time time series 
(**dataset202_allgen_trends_YYY-MM-DD.csv**) and (2) the last three generations 
(**dataset391_threegen_trends_YYY-MM-DD.csv**). 

See the [Tech Report: 4.1.1.8 Trends in Spawner Abundance](https://bookdown.org/salmonwatersheds/tech-report-staging/analytical-approach.html#overview-population-indicators) for the detailed methodology.


## Scripts & files

### 1_nuseds_collation.R


#### Files imported:

* conservation_unit_system_sites_DATE.csv
  - File downloaded from https://open.canada.ca/data
  
* cuspawnerabundance.csv
  - List of CUs with estimated spawner abundance data


#### Files exported:

* dataset103_log_smoothed_spawners_YYY-MM-DD.csv
  - The CU-level smoothed log-transformed spawner abundance data

* dataset202_allgen_trends_YYY-MM-DD.csv  
  - The CU-level total and average annual % change, the slope and intercepts of the regression line for the entire time series

* dataset391_threegen_trends_YYY-MM-DD.csv
  - The CU-level total and average annual % change, the slope and intercepts of the regression line for the last three generations

