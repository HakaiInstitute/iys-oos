# CHANGELOG
This document lists all of the data tidying and transformations applied to the raw file `IYS_GoA_2019_Hydro&Chemistry_20190326.xlsx`.

Manual changes are described here, any scripts used to aid in data tidying can be found in `data_wrangle_nuts.R`


- Split `IYS_GoA_2019_Hydro&Chemistry_20190326.xlsx` into 5 separate CSVs: `nut_location`, `chem_event`, `tempsal_event`, `chem_measurement`, and `tempsal_measurement` that are organized in a tidy, relational format. `nut_location` can be joined to either `chem_event` or `temp&sal_event` by the column `locationID`
