# population-indicators

July 7, 2023

## Overview

This is a draft example of how we might centralize the population indicator compilation across regions. Steph initiated this as a trial for discussion among the population group (Eric, Bruno, Steph). The idea would be to mirror the headings in the [tech-report](https://bookdown.org/salmonwatersheds/tech-report/).


Working directory setup:

Many of the files used as input are saved in the PSF Dropbox. In order to set up 
the working directories properly, each use must:

1. Create a text file `named wd_X_Drive1_PROJECTS.txt` in your local 
`population-indicator` github folder (along with the R project file).

2. Paste in the file the your personal path from the C drive to the PSF dropbox 
`1_PROJECTS` folder on dropbox (e.g. C:/Users/YOUR OWN PATH/X Drive/1_PROJECTS).
For Microsoft Windows OS, replace the `\` by `/`.


## Files

#### `biological-status`

Contains (or will contain...) code to
* fit HBM spawner-recruit model
* estimate spawner-recruit benchmarks
* estimate percentile benchmarks
* spit out datasets 101 and 102 (biological status outcomes)

#### `spawner-surveys`

Contains code to compile dataset 1-part2 (spawner surveys) including
* importing most recent NuSEDS data from [Open Data Canada](https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6)
* applying 'hard-coded' fixes where we know there are errors
* making changes to reflect additional knowledge where available (e.g., removing low-quality stream, unreliable data as recommended for SBC Chinook)

Update QA/QC code runs the following checks:
* Looks in `-status/Output` folder for dataset1_part2 files and extracts date (SP: Think of updating this to be the `spawner-surveys/output` folder?)
* Sources most recent data (`newData`) and next-most-recent data (`oldData`)
* Checks that headers are the same between the datasets
* Checks if the same stream names are represented in both datasets and flags either missing or added streams
* Checks if the same `streamid`s are represented in both datasets and flags either missing or added `streamid`s
* Performs a full join of new and old data and then flags any `streamid`s that have multiple rows for a single year. This would occur if any of the fields for a given `streamid` didn't match: `NuSEDS.counts.by.stream` for a given `year` (could be due to a correction in NuSEDS or some other error in how data are assigned to `streamid`), `CUID` (can happen if a stream is re-assigned to a new CU), or `streamname`. This could also occur if there were multiple methods in a year in which case this is not an error, per se.

**Note:** You will get the error `no lines available in input` if Dropbox hasn't synced the data files. Suggest choosing "Make available offline" for the `xxx-status` folders to avoid this.

#### `spawner-abundance`

Compiles CU-level spawner abundance reconstructions, and/or runs those reconstructions using spawner surveys and expansion factors (would like to do this ourselves eventually?).

Also contains `update-QAQC.R` code that runs equivalent checks to those for the spawner surveys.

#### `timing`

Could include the run timing data but also spawn timing analysis of NuSEDS data or other timing data analysis?

#### `hatchery-release-score`

#### `data-quality`


## Acknowledgements

Something acknowledging data contributors, PSAC, regional TWGs that contributed to methods and data...

## More information

See the [tech-report](https://bookdown.org/salmonwatersheds/tech-report/) that documents the methods to compile indicators that are shown here.
Stephanie Peacock <speacock@psf.ca>
