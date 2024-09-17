
# spawner-abundance

## Overview

The goal of the script is to sum the stream-level observed spawner counts 
(in **dataset2_spawner_surveys_YYYY-MM-DD.csv**, which is produced in
**spawner-surveys/code/4_datasets_for_PSE.R**) for each CU and to export the 
results as the field `observed_count` in 
**dataset1_spawner_abundance_YYYY-MM-DD.csv**.

See the [Tech Report: 4.1.1.4 Spawner Abundance](https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#benchmarks-biostatus) for more details.


## Scripts & files

### abundance_nuseds.R

#### Files imported:

* streamspawnersurveys_output.csv  
  - The file is downloaded from the database
  - It is the equivalent of dataset2_spawner_surveys_YYYY-MM-DD.csv created in spawner-surveys/code/4_datasets_for_PSE.R

* dataset1cu_output.csv
  - The file is downloaded from the database
  - It is the equivalent of dataset1_spawner_abundance_YYYY-MM-DD.csv created elsewhere


#### Files exported:

* dataset1_spawner_abundance_YYYY-MM-DD.csv
  - The dataset is exported with the field `observed_count` completed 
  - The field `estimated_count` is determined elsewhere
  
  
### update-QAQC.R

The goal of the script is to conduct internal checks.
  