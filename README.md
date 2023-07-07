# population-indicators

July 7, 2023

## Overview

This is a draft example of how we might centralize the population indicator compilation across regions. Steph initiated this as a trial for discussion among the population group (Eric, Bruno, Steph). The idea would be to mirror the headings in the [tech-report](https://bookdown.org/salmonwatersheds/tech-report/).

## Files

#### `biological-status`

Contains code to
* fit HBM spawner-recruit model
* estimate spawner-recruit benchmarks
* estimate percentile benchmarks
* spit out datasets 101 and 102 (biological status outcomes)

#### `spawner-surveys`

Contains code to compile dataset 1-part2 (spawner surveys) including
* importing most recent NuSEDS data from [Open Data Canada](https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6)
* applying 'hard-coded' fixes where we know there are errors
* making changes to reflect additional knowledge where available (e.g., removing low-quality stream, unreliable data as recommended for SBC Chinook)

#### `timing`

Could include the run timing data but also spawn timing analysis of NuSEDS data or other timing data analysis?

#### `hatchery-release-score`

#### `data-quality`


## Acknowledgements

Something acknowledging data contributors, PSAC, regional TWGs that contributed to methods and data...

## More information

See the [tech-report](https://bookdown.org/salmonwatersheds/tech-report/) that documents the methods to compile indicators that are shown here.
Stephanie Peacock <speacock@psf.ca>
