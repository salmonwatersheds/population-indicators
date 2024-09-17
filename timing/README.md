
# timing

## Overview

This sub-folder concerns the CU-level run-timing... TODO: complete.

See the [Tech Report: 4.1.1.5 Run Timing](https://bookdown.org/salmonwatersheds/tech-report-staging/analytical-approach.html#overview-population-indicators) for the detailed methodology.


## Scripts & files

### fraser-pink.R

The goal of the script is estimate Fraser pink salmon mean and sd of run timing 
from daily passage.

#### Files imported:

* MissionDailyPassage_PinkSalmon_2009_2023.csv 
  - Daily passage of pink salmon... TODO: complete
  - provided by Hague, Merran <hague@psc.org> on Sept 7, 2023


#### Files exported:

NA


### run-timing.R

The goal of the script is to update the legacy PSE run-timing data to (1) include
more data compiled by Sam Wilson; (2) introduce a data quality score for run 
timing for each CU (3) allow for skewed distributions or other non-normal distributions.

#### Files imported:

* conservationunits_decoder.csv
  - List of CUs present in the PSE database 
  
* 3Life_cycle_timing_by_CU_SKCK_update.csv
  - TODO: complete


#### Files exported:

* dataset90_run_timing_YYY-MM-DD.csv
  - TODO: complete

* run-timing-data-quality_YYY-MM-DD.csv
  - TODO: complete


### timing-draft-visuals.R

TODO: to complete (?)

#### Files imported:

#### Files exported:









