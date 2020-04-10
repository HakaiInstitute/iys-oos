# CHANGELOG
This document lists all of the data tidying and transformations applied to the raw file `IYSchl_Hunt&Pakhomov.xlsx`.

Manual changes are described here, any scripts used to aid in data tidying can be found in `data_wrangle_.R`


- In Sheet 2, data was split in two and placed side-by-side, resulting in replicated columns; I lengthened the data to remove the duplicate columns
- In Sheet 1, 2, and 3, removed the secondary (redundant) `Sample ID` column