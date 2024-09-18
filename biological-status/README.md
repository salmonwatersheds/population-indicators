
# biological-status

## Overview


This sub-folder concerns the calculation of the CU-level biological status. The goal of the scripts is to (1) fit the Ricker's model to spawner-recruit data using a Hierarchical Bayesian framework (HBSRM) (**1a_HBSRM.R**), estimate spawner-recruit benchmarks (sr), and calculate the probabilities of different biostatus outcomes (i.e. "poor", "fair" or "good"; **2a_benchmarks_HBSRM.R**); (2) estimate percentile benchmarks (**1b_benchmarks_percentiles.R**); (3) determine the final biological status for all the CUs and export the two final datasets **datasets101_biological_status.csv** and **datasets102_benchmarks.csv** (**3_biological_status.R**); compare the new *versus* old biostatus (**4_biostatus_comparison_old_new.R**).

Below is the list of files imported and exported in each R script with relevant information.

All the files produced are exported in the PFS's dropbox repository. The final datasets **datasets101_biological_status.csv** and **datasets102_benchmarks.csv** are also exported locally in the /output folder and are pushed to github when updated. 

See the [Tech Report: Analytical Approach](https://bookdown.org/salmonwatersheds/tech-report/analytical-approach.html#benchmarks-biostatus) for detailed methodology for biological status assessments.


## Scripts & files

### 1a_HBSRM.R

The goal of the script is to fit a HBSRM to the recruit-per-spawner data to all the CUs of a species in a given region.

#### Files imported:

* recruitsperspawner.csv
  - List of CUs with available estimated abundances of spawner and recruits (the data is processed elsewhere)
  
* conservationunits_decoder.csv
  - List of CUs present in the PSE database

* data/priors_HBSRmodel.csv 
  - Contains the prior values for the HBSRM parameters `prSmax` and `prCV` that are used in **1a_HBSRM.R**
  - The file is created in **checks_fixes.R**
  - The original values of these priors come from the **SRdata.txt** files located elsewhere (in the "HBM and status" sub-folders in each region-specific folders of the PSF dropbox).

#### Files exported:

* REGION_SPECIES_SR_matrices.rds
  - List of matrices containing the abundance of spawner and recruits for each CUs of each species in each region


* REGION_SPECIES_HBSRM_posteriors_priorShift.rds
  - Posterior distributions of the HBSRM parameters $a_i$, $b_i$, $mu_a$ and $sigma_{bi}$ obtained from fitting the model to data using the Markov Chain Monte Carlo (MCMC) sampling procedure.


* REGION_SPECIES_HBSRM_convDiagnostic.csv
  - Gelman and Rubin (1992)'s convergence diagnostic of the MCMC output


### 2a_benchmarks_HBSRM.R

The goal of the script is to determine the upper (80% of $S_{MSY}$) and lower ($S_{gen}$) benchmarks for each CU and to generate the corresponding probabilities of having a "poor", "fair" or "good" biostatus using current spawner abundance.

#### Files imported:

* cuspawnerabundance.csv
  - List of CUs with estimated spawner abundance data
  - Used to calculated `current spawner abundance`

* conservationunits_decoder.csv
  - List of CUs present in the PSE database

* REGION_SPECIES_HBSRM_posteriors_priorShift.rds
  - Posterior distributions of the HBSRM $a_i$, $b_i$, $mu_a$ and $sigma_{bi}$ obtained from fitting the model to data using Markov Chain Monte Carlo (MCMC) sampling procedure.
  - Created in **1a_HBSRM.R**

* REGION_SPECIES_SR_matrices.rds (created in 1a_HBSRM.R)
  - List of matrices containing the abundance of spawner and recruits for each CUs of each species in each region
  - Created in **1a_HBSRM.R**


#### Files exported:

* REGION_SPECIES_benchmarks_summary_HBSRM.csv
  - Estimated spawner-recruits benchmarks values and their confidence intervals 

* REGION_SPECIES_biological_status_HBSRM.csv
  - Calculated probability of different status outcomes



### 1b_benchmarks_percentiles.R

The goal of the script is to determine the upper (50%) and lower (25%) percentile benchmarks using the estimated spawner abundance data and to produce the corresponding probabilities of having a "poor", "fair" or "good" biostatus using current spawner abundance.


#### Files imported:

* cuspawnerabundance.csv
  - List of CUs with estimated spawner abundance data
  - Used to calculated `current spawner abundance`

* conservationunits_decoder.csv
  - List of CUs present in the PSE database


#### Files exported:

* REGION_SPECIES_benchmarks_summary_percentiles.csv
  - Estimated percentile benchmarks values and their confidence intervals 

* REGION_SPECIES_biological_status_percentiles.csv
  - Biological status outcomes


### 3_biological_status.R

The goal of the script is to determine the final biological status for all the CUs. The decision tree (cf.[Tech Report: 4.1.4.4. Decision Rules for Assessing Biological Status](https://bookdown.org/salmonwatersheds/tech-report-staging/analytical-approach.html#indicators-benchmarks-pop)) is used to determine the appropriate modelling method (HBSRM *versus* percentile) or if the if the data is sufficient.


#### Files imported:

* conservationunits_decoder.csv
  - List of CUs present in the PSE database

* dataset390_data_quality.csv
  - The overall data quality for each CUs
  - Calculated elsewhere
  - Used for the decision rules

* cuspawnerabundance.csv
  - List of CUs with estimated spawner abundance data
  - Used to calculated `current spawner abundance`

* code_PSF_Status.csv 
  - The different biological status values and associated code

* cu_highExploit_lowProd.csv
  - The lis of CUs with high exploitation or low productivity 
  - The dataframe is generated in **function.R** for now but will eventually be imported

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


#### Files exported:

* dataset101_biological_status_YYYY-MM-DD.csv
  - The file to import to the database 
  - Contains the biological status values for all the CUs

* dataset102_benchmarks_YYYY-MM-DD.csv
  - The file to import to the database 
  - Contains the benchmark values, confidence intervals and other relevant information for all the CUs
  

### 4_biostatus_comparison_old_new.R

The goal of the script is to compare the new *versus* previous biological status in order to conduct QA/QC.


#### Files imported:

* conservationunits_decoder.csv
  - List of CUs present in the PSE database

* dataset390_data_quality.csv
  - The overall data quality for each CUs
  - Calculated elsewhere
  - Used for the decision rules

* cuspawnerabundance.csv
  - List of CUs with estimated spawner abundance data
  - Used to calculated `current spawner abundance`

* dataset102_benchmarks_YYYY-MM-DD.csv
  - The previous biological status values, benchmarks are other relevant information

* dataset101_biological_status_YYYY-MM-DD.csv
  - The file to import to the database 
  - Contains the biological status values for all the CUs
  - Produced in **3_biological_status.R**

* dataset102_benchmarks_YYYY-MM-DD.csv
  - The file to import to the database 
  - Contains the benchmark values, confidence intervals and other relevant information for all the CUs
  - Produced in **3_biological_status.R**


#### Files exported:

* biostatus_new_old_differences_YYYY-MM-DD.csv
  - Contains the new and previous biological status values, benchmark values, other relevant information



## Acknowledgements

Something acknowledging data contributors, PSAC, regional TWGs that contributed to methods and data...

## More information

See the [tech-report](https://bookdown.org/salmonwatersheds/tech-report/) that documents the methods to compile indicators that are shown here.
Stephanie Peacock <speacock@psf.ca>
