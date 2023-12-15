

# Datasets in used:

* priors_HBSRmodel.csv: 
- contains the prior values for the HBSR model parameters prSmax and prCV that 
are used in HBSRM.R.
- the file is created in checks_fixes.R
- the orginal values of these priors come from the SRdata.txt files found in the 
"HBM and status" subfolders in each region-specific folders.

# Datasets NOT used anymore (to remove eventually):

* spawner_abundance.csv:
- dataset is the spawner abundance downloaded by Steph Tuesday September 5th 2023
from the SPS work.
- the dataset used instead is population-indicators/data_input/cuspawnerabundance.csv

* appendix1.csv:
- dataset is the Appendix 1 of the tech report. It contains the 
Region, Species, Conservation.Unit, CUID, Full.CU.Index of all the CUs.
- the dataset used instead is population-indicators/data_input/conservationunits_decoder.csv
