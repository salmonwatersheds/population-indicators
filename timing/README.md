
# timing

## Overview

This sub-folder concerns the CU-level run timing. Data compiled by [Wilson and Peacock (in review)](https://bookdown.org/salmonwatersheds/life-cycle-timing) on the start, peak, and end day-of-year for river entry are used to construct the average percentages of the CU that enter freshwater on a given day. Output datasets are **dataset90_run_timing.csv** and **run-timing-data-quality.csv**. 

See the [Pacific Salmon Explorer Tech. Report: Run Timing](https://salmonexplorer.ca/methods/analytical-approach.html#run-timing) for further details.


## Scripts & files


### run-timing.R

The goal of the script is to trasnform start, peak, and end spawn dates from [Wilson and Peacock (in review)](https://bookdown.org/salmonwatersheds/life-cycle-timing) to daily proportions for visualization in the Pacific Salmon Explorer, and output the associated data quality scores.

#### Files imported:

* conservationunits_decoder.csv
  - List of CUs present in the PSE database 
  
* 3Life_cycle_timing_by_CU.csv
  - CU-level summary of the start, peak, and end day-of-year from [Wilson and Peacock (in review)](https://bookdown.org/salmonwatersheds/life-cycle-timing).


#### Files exported:

* dataset90_run_timing_YYY-MM-DD.csv
  - Proportions of the run that enter freshwater each day of the year by CU 

* run-timing-data-quality_YYY-MM-DD.csv
  - Data quality scores based on the number of years of data, the currency of the data, and whether sampling captured the entirety of the run


<!--
### fraser-pink.R

The goal of the script is to estimate Fraser pink salmon mean and sd of run timing 
from daily passage.

#### Files imported:

* MissionDailyPassage_PinkSalmon_2009_2023.csv 
  - Daily passage of pink salmon... TODO: complete
  - provided by Hague, Merran <hague@psc.org> on Sept 7, 2023

#### Files exported:

NA

  
### timing-draft-visuals.R

TODO: to complete (?)

#### Files imported:

#### Files exported:


-->








