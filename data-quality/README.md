
# data-quality

## Overview

This sub-folder concerns the CU-level compilation of the data quality for the 
different indicators and the definition of the overall data quality score 
(**dataset390_data_quality.csv**).

See the [Pacific Salmon Explorer Tech Report: Data Quality Scores](https://salmonexplorer.ca/methods/analytical-approach.html#data-quality) for the detailed methodology.


## Scripts & files

### data-quality.R

The goal of the script is to import data associated with population indicators and other supplemental information and compile data quality scores for seven criteria: `survey_quality`, `survey_coverage`, `survey_execution`, `catch_quality`, `stockid_quality`, `juvenile_quality`, and `runtiming_quality`. These data quality criteria inform the CU-level data quality scores for population indicators in the [Pacific Salmon Explorer](https://salmonexplorer.ca/).

#### Files imported:

* `conservationunits_decoder.csv`
  - List of CUs present in the PSE database

* `streamspawnersurveys_output`
  - Spawner survey data shown in the PSE and available from the [Data Library](https://data.salmonwatersheds.ca/result?datasetid=1)
  
* `dataset88_juvenile_surveys.csv`
- Juvenile survey data shown in the PSE and available from the [Data Library](https://data.salmonwatersheds.ca/result?datasetid=88)

* `catch-quality_2024-09-04.xlsx`
- Qualitative catch quality scores based on the approach to estimating CU-level catch 

* `dataset3_catch_and_run_size.csv`
- Catch and run size data shown in the PSE and available from the [Data Library](https://data.salmonwatersheds.ca/result?datasetid=3)

* `Fraser Catch and StockID quality.xlsx`  
- Stock ID quality scores provided by the Pacific Salmon Commission for **Fraser sockeye** only

* `run-timing-data-quality.csv`                  
- Run timing data quality scores based on [Wilson and Peacock (in review)](https://bookdown.org/salmonwatersheds/life-cycle-timing) and output by `timing/code/run-timing.R`


#### Files exported:

* dataset390_data_quality.csv
  - CU-level data quality score (0-5) for each criteria and CU-level data quality scores for each population indicator, averaged over component criteria.







