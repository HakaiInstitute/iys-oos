# ---
#  title: CTD measurements 2019 Gulf of Alaska expedition
#  authors: Julian Gan & Tim van der Stap, Hakai Institute
#  date: April 29, 2020
#  objective: Data wrangling of CTD data collected on the GoA 2019 cruise, 
#  according to Darwin Core Archive (DwC-A) standards. 
# ---

# Load required packages:
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(parsedate)
library(googledrive)

drive_download("https://drive.google.com/open?id=1-onECiDW02DJRIX6g4dXywtzLEqCGyce", 
               path = here::here("CTD", "raw_data", "Data_IYS_conbined_Final.xlsx"))

sheet1 <- read_excel(here("CTD", "raw_data", "Data_IYS_conbined_Final.xlsx"), sheet = "Sheet1")

# Update the timezone for the Event Core:
sheet2 <- read_excel(here("CTD", "raw_data", "Data_IYS_conbined_Final.xlsx"), sheet = "Sheet2") %>%
  mutate(eventDate_text = str_c(YEAR, MONTH, DAY, `TIME(ship time)`, `MIN(ship time)`, sep="-"),
         eventDate = ymd_hm(eventDate_text, tz = "Asia/Kamchatka")) %>%
  mutate(eventDate = format_iso_8601(as.POSIXct(eventDate,
                                                format = "%Y-%m-%d %H:%M%:S",
                                                tz="Asia/Kamchatka")),
         eventDate = str_replace(eventDate, "\\+00:00", "Z")
         ) %>%
  mutate(cruise = "GoA2019",
         station = paste(cruise, `NO.Trawl`, sep="_Stn"),
         cast = paste(station, "cast1", sep = ":"),
         ndepth = paste(cast, DEPTH, sep=":ctd:"))

# In our case, the recorded longitude values should be negative (as determined through QC,
# and cross-referencing with other datasets from the same cruise): 
sheet2$LON <- -sheet2$LON

ctd_cruise <- sheet2 %>% 
  select(eventID = cruise) %>%
  distinct(eventID) %>%
  mutate(type = "cruise")

# Join date to station
ctd_station <- sheet2 %>%
  select(eventID = station,
         parentEventID = cruise,
         eventDate,
         decimalLatitude = LAT,
         decimalLongitude = LON) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "station")

ctd_cast <- sheet2 %>% 
  select(eventID = cast,
         parentEventID = station) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "cast")

ctd_ndepth <- sheet2 %>% 
  select(eventID = ndepth,
         parentEventID = cast,
         minimumDepthInMetres = DEPTH,
         maximumDepthInMetres = DEPTH) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "sample")

ctd_event <- bind_rows(ctd_cruise, ctd_station, ctd_cast, ctd_ndepth) %>% 
  select(eventID, parentEventID:maximumDepthInMetres, type) 

# Re-order the Event Core:
order <- stringr::str_sort(ctd_event$eventID, numeric=TRUE)
ctd_event <- ctd_event[match(order, ctd_event$eventID),]

# Save locally and in GoogleDrive
write_csv(ctd_event, here("CTD", "tidy_data", "CTD_event.csv"))
drive_upload(here("CTD", "tidy_data", "CTD_event.csv"),
             path = "https://drive.google.com/drive/u/0/folders/1-XXOPhMN4-BmhI3owM2hvaMKXEy6hEYL",
             name = "CTD_event.csv",
             overwrite = TRUE)

## MeasurementOrFact  ----------------------------------------------------

ctd_botdepth <- sheet2 %>% 
  select(eventID = cast, `Bot. Depth`) %>%
  distinct(eventID, .keep_all = TRUE) %>%
  pivot_longer(cols = `Bot. Depth`,
               names_to = "measurementType",
               values_to = "measurementValue") %>%
  mutate(measurementValue = as.character(measurementValue),
         measurementID = paste(eventID, "depth", sep = ":"),
         measurementType = recode(measurementType,
                                  `Bot. Depth` = "seafloor depth"),
         measurementTypeID = "http://vocab.nerc.ac.uk/collection/C00/current/BRIDGE/",
         measurementUnit = "m",
         measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/ULAA/" ) %>%
  select(measurementID, eventID, measurementType, measurementTypeID, measurementValue,
         measurementUnit, measurementUnitID)


# First, EC25 is measured in microSiemens/cm, which is not (yet) in NERCs controlled 
# vocabulary. Therefore, we convert this column to milliSiemens/cm:
ctd_measurement <- sheet2 %>% 
  select(eventID = ndepth, 
         `TEM_S`:`BOD5[ml/l]`) %>%
  mutate(`EC25 [mS/cm]_R` = as.numeric(`EC25 [uS/cm]_R`) / 1000) %>%
  mutate_all(as.character)
ctd_measurement <- ctd_measurement[, -which(names(ctd_measurement) == "EC25 [uS/cm]_R")] %>%
  pivot_longer(`TEM_S`:`EC25 [mS/cm]_R`,
               names_to = "measurementType",
               values_to = "measurementValue")

# To create a measurement table for only the measurement taken at specific depths,
# change sheet2 to sheet1 in the above chunk of code.

ctd_measurement <- ctd_measurement %>% # Should "NO.Trawl" here be "NO.(ST)"?
  mutate(measurementID = case_when(measurementType == "TEM_S" ~ paste(eventID,"TEM_S",sep="_"),
                                   measurementType == "TEM_R" ~ paste(eventID,"TEM_R",sep="_"),
                                   measurementType == "SAL_S" ~ paste(eventID,"SAL_S",sep="_"),
                                   measurementType == "SAL_R" ~ paste(eventID,"SAL_R",sep="_"),
                                   measurementType == "Cond. [mS/cm]_R" ~ paste(eventID,"Cond. [mS/cm]_R", sep="_"),
                                   measurementType == "Density [kg/m^3]_R" ~ paste(eventID,"Density", sep="_"),
                                   measurementType == "SigmaT [ ]_R" ~ paste(eventID,"Sigma",sep="_"),
                                   measurementType == "Chl-Flu. [ppb]_R" ~ paste(eventID,"Chl-Flu", sep="_"),
                                   measurementType == "Chl-a [ug/l]_R" ~ paste(eventID,"Chl-a", sep="_"),
                                   measurementType == "Turb-M [FTU]_R" ~ paste(eventID,"Turbidity",sep="_"),
                                   measurementType == "DO [mg/l]_R" ~ paste(eventID,"DO",sep="_"),
                                   measurementType == "Batt. [V]_R" ~ paste(eventID,"Batt", sep="_"),
                                   measurementType == "pH" ~ paste(eventID,"pH",sep="_"),
                                   measurementType == "O2 [ml/l]" ~ paste(eventID,"O2 [ml/l]",sep="_"),
                                   measurementType == "BOD5[ml/l]" ~ paste(eventID,"BOD5[ml/l]",sep="_"),
                                   measurementType == "DO [%]_R" ~ paste(eventID,"BOD5[ml/l]",sep="_"),
                                   measurementType == "EC25 [mS/cm]_R" ~ paste(eventID,"EC25 [mS/cm]_R",sep="_")),
         
         measurementType = recode(measurementType, 
                                  `TEM_S` = "Sea Water Temperature [Sea-Bird]",
                                  `TEM_R` = "Sea Water Temperature [Rinko]",
                                  `SAL_R` = "Sea Water Practical Salinity [Rinko]",
                                  `SAL_S` = "Sea Water Practical Salinity [Sea-Bird]",
                                  `pH` = "pH",
                                  `O2 [ml/l]` = "Dissolved oxygen [Lab]",
                                  `Cond. [mS/cm]_R` = "Sea Water Electrical Conductivity",
                                  `Density [kg/m^3]_R` = "Sea Water Density",
                                  `SigmaT [ ]_R` = "Sea Water Sigma T",
                                  `Chl-Flu. [ppb]_R` = "Chlorophyll - Flu",
                                  `Chl-a [ug/l]_R` = "Chlorophyll-a",
                                  `Turb-M [FTU]_R` = "Turbidity",
                                  `DO [mg/l]_R` = "Dissolved oxygen",
                                  `Batt. [V]_R` = "Battery voltage",
                                  `BOD5[ml/l]` = "BOD in 5m depth [Lab]",
                                  `DO [%]_R` = "Percent oxygen saturation",
                                  `EC25 [mS/cm]_R` = "EC25"),
         
         measurementTypeID = recode(measurementType, #URIs from NERC website
                                    "Sea Water Temperature [Sea-Bird]" = "http://vocab.nerc.ac.uk/collection/P02/current/TEMP/2/",
                                    "Sea Water Temperature [Rinko]" = "http://vocab.nerc.ac.uk/collection/P02/current/TEMP/2/",
                                    "Sea Water Practical Salinity [Rinko]" = "http://vocab.nerc.ac.uk/collection/P02/current/PSAL/",
                                    "Sea Water Practical Salinity [Sea-Bird]" = "http://vocab.nerc.ac.uk/collection/P02/current/PSAL/",
                                    "Sea Water Electrical Conductivity" = "http://vocab.nerc.ac.uk/collection/P02/current/CNDC/",
                                    "Sea Water Density" = "http://vocab.nerc.ac.uk/collection/P04/current/G990/",
                                    "Sea Water Sigma T" = "http://vocab.nerc.ac.uk/collection/OG1/current/SIGTHETA/",     
                                    "Chlorophyll - Flu" = "http://vocab.nerc.ac.uk/collection/P07/current/CFSN0705/",
                                    "Chlorophyll-a" = "http://vocab.nerc.ac.uk/collection/P07/current/CFSN0705/",
                                    "Turbidity" = "http://vocab.nerc.ac.uk/collection/P25/current/TURB/",
                                    "Dissolved oxygen" = "http://vocab.nerc.ac.uk/collection/P35/current/EPC00002/",
                                    "Battery voltage" = "http://vocab.nerc.ac.uk/collection/S06/current/S0600163/",
                                    "pH" = "http://vocab.nerc.ac.uk/collection/P01/current/PHXXZZXX/",
                                    "Dissolved oxygen [Lab]" = "http://vocab.nerc.ac.uk/collection/P01/current/DOXYZZ01/",
                                    "BOD in 5m depth [Lab]" = "http://vocab.nerc.ac.uk/collection/P01/current/BODZZZZZ/1/",
                                    "Percent oxygen saturation" = "http://vocab.nerc.ac.uk/collection/P02/current/DOXY/",
                                    "EC25" = "http://vocab.nerc.ac.uk/collection/P02/current/CNDC/"),
         
         measurementUnit = case_when(measurementType == "Sea Water Temperature [Sea-Bird]" ~ "deg C",
                                     measurementType == "Sea Water Temperature [Rinko]" ~ "deg C",
                                     measurementType == "Sea Water Practical Salinity [Rinko]" ~ " ",
                                     measurementType == "Sea Water Practical Salinity [Sea-Bird]" ~ " ",
                                     measurementType == "pH" ~ " ",
                                     measurementType == "Sea Water Electrical Conductivity" ~ "mS/cm",
                                     measurementType == "Sea Water Density" ~ "kg m^-3 ",
                                     measurementType == "Sea Water Sigma T" ~ "kg m^-3",
                                     measurementType == "Chlorophyll - Flu" ~ "ppb",
                                     measurementType == "Chlorophyll-a" ~ "ug/L",
                                     # Alternatively, chlorophyll-a concentration can be reported in mg.m-3. Conversion = 1:1.
                                     measurementType == "Turbidity" ~ "FTU",
                                     measurementType == "Battery voltage" ~ "V",
                                     measurementType == "Dissolved oxygen" ~ "mg/L",
                                     measurementType == "Dissolved oxygen [Lab]" ~ "ml/L ",
                                     measurementType == "BOD in 5m depth [Lab]" ~ "ml/L",
                                     measurementType == "Percent oxygen saturation" ~ "percent saturation",
                                     measurementType == "EC25" ~ "mS/cm"),
         
         measurementUnitID = case_when(measurementType == "Sea Water Temperature [Sea-Bird]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPAA/",
                                       measurementType == "Sea Water Temperature [Rinko]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPAA/",
                                       measurementType == "Sea Water Practical Salinity [Rinko]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/PPTR/",
                                       measurementType == "Sea Water Practical Salinity [Sea-Bird]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/PPTR/",
                                       measurementType == "Sea Water Electrical Conductivity" ~ "http://vocab.nerc.ac.uk/collection/P06/current/MSCM/",
                                       measurementType == "Sea Water Density" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UKMC/",
                                       measurementType == "Sea Water Sigma T" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UKMC/",
                                       measurementType == "Chlorophyll - Flu" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPPB/",
                                       measurementType == "Chlorophyll-a" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UGPL/",
                                       measurementType == "Turbidity" ~ "http://vocab.nerc.ac.uk/collection/P06/current/USTU/",
                                       measurementType == "Dissolved oxygen" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMGL/",
                                       measurementType == "Battery voltage" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UVLT/",
                                       measurementType == "pH" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UUPH/",
                                       measurementType == "Dissolved oxygen [Lab]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMLL/",
                                       measurementType == "BOD in 5m depth [Lab]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMLL/",
                                       measurementType == "Percent oxygen saturation" ~ "http://vocab.nerc.ac.uk/collection/P01/current/OXYSZZ01/",
                                       measurementType == "EC25" ~ "http://vocab.nerc.ac.uk/collection/P06/current/MSCM/")
  ) %>%
  select(measurementID, measurementType, measurementTypeID,
         measurementValue, measurementUnit, measurementUnitID)

# Join the dataframes: 
ctd_measurementOrFact <- bind_rows(ctd_botdepth, ctd_measurement) %>%
  select(measurementID, eventID, measurementType, measurementTypeID,
         measurementValue, measurementUnit, measurementUnitID)

# Save locally and on GoogleDrive: 
write_csv(ctd_measurementOrFact, here("CTD", "tidy_data", "CTD_measurement.csv"))
drive_upload(here("CTD", "tidy_data", "CTD_measurement.csv"),
             path = "https://drive.google.com/drive/u/0/folders/1-XXOPhMN4-BmhI3owM2hvaMKXEy6hEYL",
             name = "CTD_measurement.csv",
             overwrite = TRUE)
