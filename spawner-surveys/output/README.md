Datasets produced in 1_nuseds_collation.R:

- all_areas_nuseds_cleaned.csv               # to keep ?
- conservation_unit_system_sites_cleaned.csv # to keep ? 
- NuSEDS_escapement_data_collated_DATE.csv # the cleaned merge of all_areas_nuseds and conservation_unit_system_sites
- series_inNUSEDS_noInCUSS_DATE.csv        # the time series in all_areas_nuseds for which no alternative POP_ID nor GFE_ID could be found
- series_added_DATE.csv                    # the reference of series (POP_ID - GDE_ID) added to conservation_unit_system_sites
- series_removed_DATE.csv                  # the series removed from NuSEDS either deleted, or combined to alternative time series



Datasets produced in 2_nuseds_cuid_streamid.R:

- nuseds_cuid_streamid_DATE.csv            # 
- dataset_1part2_DATE.csv                  # 



# NOTE: the followinf files have been produce for the Salmon Data Demise paper
The different from the previous version)(2024-04-19) is that the 0s were NOT converted to NAs.

- 1_NuSEDS_escapement_data_collated_2024-11-21.csv
- 1_conservation_unit_system_sites_cleaned_2024-11-21.csv
- 1_all_areas_nuseds_cleaned_2024-11-21
- 2_nuseds_cuid_streamid_2024-11-21.csv
