
# overviews

## Overview


This sub-folder concerns the calculation of the CU-level abundance anomalies shown in the regional Overviews pages of the Pacific Salmon Explorer. 

All the files produced are exported in PSF's Dropbox. The final dataset is **`datasets559_cu-anomaly.csv`** and is also exported to GitHub in the `/output`. 

## Scripts & files

### regional-overviews.R

Imports CU-level spanwer abundance and smoothed spawner abundance to 
1. Calculate average spawner abundance (`avg`) as the geometric mean among all years of `estimated_count` from dataset1
2. Calculate smoothed spawner abundance as `smoothed_abund = exp(avg_escape_log)` from dataset103
3. Calculate each year's percent anomaly from the average as `round((smoothed_abund - avg)/avg*100, 1))`.

There is some code (commented out) to repeat this for total run size if we choose to add in total abundance to the overviews page, but this has been commented out for now and will need to be revisited if/when we add total abundance.

#### Files imported:

* `dataset1_spawner_abundance.csv`
* `dataset103_log_smoothed_spawners.csv`

#### Files exported:

* `datasets559_cu-anomaly.csv`


## More information

Stephanie Peacock <speacock@psf.ca>
