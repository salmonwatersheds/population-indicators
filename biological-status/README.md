# biological-status

July 18, 2023

## Overview


See the [Tech Report: Analytical Approach](https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#benchmarks-biostatus) for detailed methodology for biological status assessments.

## Files

#### `code`

Contains code to
* fit HBM spawner-recruit model
* estimate spawner-recruit benchmarks
* estimate percentile benchmarks
* calculate probability of different status outcomes
* spit out datasets 101 and 102 (biological status outcomes)

#### `data`

Contains spawner-recruit datasets and priors used in HBM fitting. The data file was historically a .txt file with initial rows detailing `#MaxStocks` and the priors on `b`: `prSmax` and `prCV`. I suggest these priors get moved to a separate file. In that case the data files for each region and species would be, e.g., `SRdata_fraser-pink.csv` or `SRdata-cc-sockeye.csv` with fields for `CUID`, `brood_year`, `spawners`, `recruits`, and `exploitation_rate`. The priors would `SRpriors_fraser-pink.csv` or `SRpriors-cc-sockeye.csv` with fields for `prSmax` and `prCV`. 

#### `output`

Place for mcmc posterior output files (as `.rds`?) of SR parameters (a and b) and benchmarks (Smsy, Sgen), and percentile benchmarks/samples, and output datasets.

## Acknowledgements

Something acknowledging data contributors, PSAC, regional TWGs that contributed to methods and data...

## More information

See the [tech-report](https://bookdown.org/salmonwatersheds/tech-report/) that documents the methods to compile indicators that are shown here.
Stephanie Peacock <speacock@psf.ca>
