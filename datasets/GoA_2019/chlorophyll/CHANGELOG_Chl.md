# CHANGELOG
This document lists all of the data tidying and transformations applied to the raw file `IYSchl_Hunt&Pakhomov.xlsx`.

Manual changes are described here, any scripts used to aid in data tidying can be found in `data_wrangle_chl.R`


- Split `IYSchl_Hunt&Pakhomov.xlsx` into 3 separate CSVs: `location`, `event`, and `measurement` that are organized in a tidy, relational format
- Time converted to ISO 8601 Extended date & time format

- Added a column `Cruise` in the raw data file, and filled the column with `GoA2019`. 
                              - 4/15/2020 - Tim van der Stap