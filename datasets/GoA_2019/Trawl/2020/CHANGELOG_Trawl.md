# CHANGELOG

This document lists all of the data tidying and transformations applied to the raw file `2019_GoA_Fish_data_Hakai.xlsx`.

Manual changes are described here, any scripts used to aid in data tidying can be found in `data_wrangle_trawl.R`. 

- Split `2019_GoA_Fish_data_Hakai.xlsx` into x CSV files: `trawl_event`, `trawl_occ`, `trawl_eMoF` and `trawl_loc` that are organized in a tidy, relational format. 