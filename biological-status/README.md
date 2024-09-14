# biological-status

July 18, 2023

## Overview

See the [Tech Report: Analytical Approach](https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#benchmarks-biostatus) for detailed methodology for biological status assessments.


Below is the list of files imported and exported in each R script with relevant 
information. The files exported in bold are the ones imported to a script in a 
subsequent step of the workflow. TO EDIT


## Files

#### `code`

Contains code to
* fit Hierarchical Bayesian spawner-recruit model (HBSRM): **1a_HBSRM.R**
* estimate spawner-recruit benchmarks (sr) and calculate probability of different status outcomes: **2a_benchmarks_HBSRM.R**
* estimate percentile benchmarks: **1b_benchmarks_percentiles.R**
* determine the final biological status for all the CUs and export the two final datasets (**datasets101_biological_status.csv**, **datasets102_benchmarks.csv**): **3_biological_status.R**

#### `input`

TO KEEP ???

Contains spawner-recruit datasets and priors used in HBM fitting. The data file was historically a .txt file with initial rows detailing `#MaxStocks` and the priors on `b`: `prSmax` and `prCV`. I suggest these priors get moved to a separate file. In that case the data files for each region and species would be, e.g., `SRdata_fraser-pink.csv` or `SRdata-cc-sockeye.csv` with fields for `CUID`, `brood_year`, `spawners`, `recruits`, and `exploitation_rate`. The priors would `SRpriors_fraser-pink.csv` or `SRpriors-cc-sockeye.csv` with fields for `prSmax` and `prCV`. 


#### **1a_HBSRM.R**:

* recruitsperspawner.csv
  - List of CUs with available estimated abundances of spawner and recruits (the data is processed elsewhere)
  
* conservationunits_decoder.csv
  - List of CUs present in the PSE database

* data/priors_HBSRmodel.csv 
  - Contains the prior values for the HBSRM parameters `prSmax` and `prCV` that are used in **1a_HBSRM.R**
  - The file is created in **checks_fixes.R**
  - The original values of these priors come from the **SRdata.txt** files located elsewhere (in the "HBM and status" sub-folders in each region-specific folders of the PSF dropbox).


#### **2a_benchmarks_HBSRM.R**:

* cuspawnerabundance.csv
  - List of CUs with estimated spawner abundance data
  - Used to calculated `current spawner abundance`

* conservationunits_decoder.csv
  - List of CUs present in the PSE database

* REGION_SPECIES_HBSRM_posteriors_priorShift.rds
  - Posterior distributions of the HBSRM `a_i`, `b_i`, `mu_a` and `sigma_b_i` obtained from fitting the model to data using Markov Chain Monte Carlo (MCMC) sampling procedure.
  - Created in **1a_HBSRM.R**

* REGION_SPECIES_SR_matrices.rds (created in 1a_HBSRM.R)
  - List of matrices containing the abundance of spawner and recruits for each CUs of each species in each region
  - Created in **1a_HBSRM.R**


#### **1b_benchmarks_percentiles.R**:

* cuspawnerabundance.csv
  - List of CUs with estimated spawner abundance data
  - Used to calculated `current spawner abundance`

* conservationunits_decoder.csv
  - List of CUs present in the PSE database


#### **3_biological_status.R**:




#' Files imported:

* conservationunits_decoder.csv
  - List of CUs present in the PSE database

* dataset390_data_quality.csv
  - The overall data quality for each CUs
  - Calculated elsewhere
  - Used for the decision rules


 cuspawnerabundance.csv


* REGION_SPECIES_biological_status_HBSRM.csv
  - Calculated probability of different status outcomes
  - Created in **2a_benchmarks_HBSRM.R**
  
* REGION_SPECIES_benchmarks_summary_HBSRM.csv
  - Estimated spawner-recruits benchmarks values and their confidence intervals 
  - Created in **2a_benchmarks_HBSRM.R**

* REGION_SPECIES_benchmarks_summary_percentiles.csv
  - Estimated percentile benchmarks values and their confidence intervals 
  - Created in **1b_benchmarks_percentiles.R**
  
* REGION_SPECIES_biological_status_percentiles.csv
  - Biological status outcomes
  - Created in **1b_benchmarks_percentiles.R**



#' 
#' Files produced: 
* Biological_status_HBSR_Percentile_all.csv    # should become dataset_101_output I think
* Benchmarks_HBSR_Percentile_all.csv           # 
* data/code_PSF_Status.csv
* population-indicators/data-input/CUs_highExploitation_lowProductivity.csv
* output/dataset101_biological_status.csv #
* output/dataset102_benchmarks.csv        # 
* output/archive/dataset101_biological_status_YYYY-MM-DD.csv #
* output/archive/dataset102_benchmarks_YYYY-MM-DD.csv        # 

CUs_highExploitation_lowProductivity.csv



#### `output`


#### **1a_HBSRM.R**:

* **REGION_SPECIES_SR_matrices.rds**
  - List of matrices containing the abundance of spawner and recruits for each CUs of each species in each region


* **REGION_SPECIES_HBSRM_posteriors_priorShift.rds**
  - Posterior distributions of the HBSRM `a_i`, `b_i`, `mu_a` and `sigma_b_i` obtained from fitting the model to data using Markov Chain Monte Carlo (MCMC) sampling procedure.


* REGION_SPECIES_HBSRM_convDiagnostic.csv
  - Gelman and Rubin (1992)'s convergence diagnostic of the MCMC output
  
  
#### **2a_benchmarks_HBSRM.R**:

* **REGION_SPECIES_benchmarks_summary_HBSRM.csv**
  - Estimated spawner-recruits benchmarks values and their confidence intervals 

* **REGION_SPECIES_biological_status_HBSRM.csv**
  - Calculated probability of different status outcomes


#### **1b_benchmarks_percentiles.R**:

* **REGION_SPECIES_benchmarks_summary_percentiles.csv**
  - Estimated percentile benchmarks values and their confidence intervals 

* **REGION_SPECIES_biological_status_percentiles.csv**
  - Biological status outcomes





## Acknowledgements

Something acknowledging data contributors, PSAC, regional TWGs that contributed to methods and data...

## More information

See the [tech-report](https://bookdown.org/salmonwatersheds/tech-report/) that documents the methods to compile indicators that are shown here.
Stephanie Peacock <speacock@psf.ca>
