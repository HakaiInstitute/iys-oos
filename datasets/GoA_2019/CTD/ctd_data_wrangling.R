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
# order <- stringr::str_sort(ctd_event$eventID, numeric=TRUE)
# ctd_event <- ctd_event[match(order, ctd_event$eventID),]

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
  mutate_all(as.character) %>%
  dplyr::select(-`EC25 [uS/cm]_R`) %>% 
  pivot_longer(`TEM_S`:`EC25 [mS/cm]_R`,
               names_to = "measurementType",
               values_to = "measurementValue")

# To create a measurement table for only the measurement taken at specific depths,
# change sheet2 to sheet1 in the above chunk of code.

ctd_measurement <- ctd_measurement %>% # Should "NO.Trawl" here be "NO.(ST)"?
  mutate(measurementID = case_when(measurementType == "TEM_S" ~ paste(eventID,"temp_s",sep=":"),
                                   measurementType == "TEM_R" ~ paste(eventID,"temp_r",sep=":"),
                                   measurementType == "SAL_S" ~ paste(eventID,"sal_s",sep=":"),
                                   measurementType == "SAL_R" ~ paste(eventID,"sal_r",sep=":"),
                                   measurementType == "Cond. [mS/cm]_R" ~ paste(eventID,"cond_r", sep=":"),
                                   measurementType == "Density [kg/m^3]_R" ~ paste(eventID,"density", sep=":"),
                                   measurementType == "SigmaT [ ]_R" ~ paste(eventID,"sigma_t",sep=":"),
                                   measurementType == "Chl-Flu. [ppb]_R" ~ paste(eventID,"chl_flu", sep=":"),
                                   measurementType == "Chl-a [ug/l]_R" ~ paste(eventID,"chl-a", sep=":"),
                                   measurementType == "Turb-M [FTU]_R" ~ paste(eventID,"turb",sep=":"),
                                   measurementType == "DO [mg/l]_R" ~ paste(eventID,"DO_conc",sep=":"),
                                   measurementType == "Batt. [V]_R" ~ paste(eventID,"battery", sep=":"),
                                   measurementType == "pH" ~ paste(eventID,"pH",sep=":"),
                                   measurementType == "O2 [ml/l]" ~ paste(eventID,"O2",sep=":"),
                                   measurementType == "BOD5[ml/l]" ~ paste(eventID,"BOD5",sep=":"),
                                   measurementType == "DO [%]_R" ~ paste(eventID,"DO_per",sep=":"),
                                   measurementType == "EC25 [mS/cm]_R" ~ paste(eventID,"ec25",sep=":")),
         measurementType = recode(measurementType, 
                                  `TEM_S` = "seabird_sea_water_temperature",
                                  `TEM_R` = "rinko_sea_water_temperature",
                                  `SAL_R` = "rinko_sea_water_practical_salinity",
                                  `SAL_S` = "seabird_sea_water_practical_salinity",
                                  `pH` = "pH",
                                  `O2 [ml/l]` = "dissolved_oxygen_lab",
                                  `Cond. [mS/cm]_R` = "sea_water_electrical_conductivity",
                                  `Density [kg/m^3]_R` = "sea_water_density",
                                  `SigmaT [ ]_R` = "sea_water_sigma_t",
                                  `Chl-Flu. [ppb]_R` = "chlorophyll_flu",
                                  `Chl-a [ug/l]_R` = "chlorophyll_a",
                                  `Turb-M [FTU]_R` = "turbidity",
                                  `DO [mg/l]_R` = "dissolved_oxygen",
                                  `Batt. [V]_R` = "battery_voltage",
                                  `BOD5[ml/l]` = "BOD_5m_depth",
                                  `DO [%]_R` = "percent_oxygen_saturation",
                                  `EC25 [mS/cm]_R` = "EC25"),
         
         measurementTypeID = recode(measurementType, #URIs from NERC website
                                    "seabird_sea_water_temperature" = "http://vocab.nerc.ac.uk/collection/P02/current/TEMP/2/",
                                    "rinko_sea_water_temperature" = "http://vocab.nerc.ac.uk/collection/P02/current/TEMP/2/",
                                    "rinko_sea_water_practical_salinity" = "http://vocab.nerc.ac.uk/collection/P02/current/PSAL/",
                                    "seabird_sea_water_practical_salinity" = "http://vocab.nerc.ac.uk/collection/P02/current/PSAL/",
                                    "sea_water_electrical_conductivity" = "http://vocab.nerc.ac.uk/collection/P02/current/CNDC/",
                                    "sea_water_density" = "http://vocab.nerc.ac.uk/collection/P04/current/G990/",
                                    "sea_water_sigma_t" = "http://vocab.nerc.ac.uk/collection/OG1/current/SIGTHETA/",     
                                    "chlorophyll_flu" = "http://vocab.nerc.ac.uk/collection/P07/current/CFSN0705/",
                                    "chlorophyll_a" = "http://vocab.nerc.ac.uk/collection/P07/current/CFSN0705/",
                                    "turbidity" = "http://vocab.nerc.ac.uk/collection/P25/current/TURB/",
                                    "dissolved_oxygen" = "http://vocab.nerc.ac.uk/collection/P35/current/EPC00002/",
                                    "battery_voltage" = "http://vocab.nerc.ac.uk/collection/S06/current/S0600163/",
                                    "pH" = "http://vocab.nerc.ac.uk/collection/P01/current/PHXXZZXX/",
                                    "dissolved_oxygen_lab" = "http://vocab.nerc.ac.uk/collection/P01/current/DOXYZZ01/",
                                    "BOD_5m_depth" = "http://vocab.nerc.ac.uk/collection/P01/current/BODZZZZZ/1/",
                                    "percent_oxygen_saturation" = "http://vocab.nerc.ac.uk/collection/P02/current/DOXY/",
                                    "EC25" = "http://vocab.nerc.ac.uk/collection/P02/current/CNDC/"),
         
         measurementUnit = case_when(measurementType == "seabird_sea_water_temperature" ~ "deg C",
                                     measurementType == "rinko_sea_water_temperature" ~ "deg C",
                                     measurementType == "rinko_sea_water_practical_salinity" ~ " ",
                                     measurementType == "seabird_sea_water_practical_salinity" ~ " ",
                                     measurementType == "pH" ~ " ",
                                     measurementType == "sea_water_electrical_conductivity" ~ "mS/cm",
                                     measurementType == "sea_water_density" ~ "kg/m^3 ",
                                     measurementType == "sea_water_sigma_t" ~ "kg/m^3",
                                     measurementType == "chlorophyll_flu" ~ "ppb",
                                     measurementType == "chlorophyll_a" ~ "ug/L",
                                     # Alternatively, chlorophyll-a concentration can be reported in mg.m-3. Conversion = 1:1.
                                     measurementType == "turbidity" ~ "FTU",
                                     measurementType == "battery_voltage" ~ "V",
                                     measurementType == "dissolved_oxygen" ~ "mg/L",
                                     measurementType == "dissolved_oxygen_lab" ~ "ml/L ",
                                     measurementType == "BOD_5m_depth" ~ "ml/L",
                                     measurementType == "percent_oxygen_saturation" ~ "percent saturation",
                                     measurementType == "EC25" ~ "mS/cm"),
         
         measurementUnitID = case_when(measurementType == "seabird_sea_water_temperature" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPAA/",
                                       measurementType == "rinko_sea_water_temperature" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPAA/",
                                       measurementType == "rinko_sea_water_practical_salinity" ~ "http://vocab.nerc.ac.uk/collection/P06/current/PPTR/",
                                       measurementType == "seabird_sea_water_practical_salinity" ~ "http://vocab.nerc.ac.uk/collection/P06/current/PPTR/",
                                       measurementType == "sea_water_electrical_conductivity" ~ "http://vocab.nerc.ac.uk/collection/P06/current/MSCM/",
                                       measurementType == "sea_water_density" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UKMC/",
                                       measurementType == "sea_water_sigma_t" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UKMC/",
                                       measurementType == "chlorophyll_flu" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPPB/",
                                       measurementType == "chlorophyll_a" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UGPL/",
                                       measurementType == "turbidity" ~ "http://vocab.nerc.ac.uk/collection/P06/current/USTU/",
                                       measurementType == "dissolved_oxygen" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMGL/",
                                       measurementType == "battery_voltage" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UVLT/",
                                       measurementType == "pH" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UUPH/",
                                       measurementType == "dissolved_oxygen_lab" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMLL/",
                                       measurementType == "BOD_5m_depth" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMLL/",
                                       measurementType == "percent_oxygen_saturation" ~ "http://vocab.nerc.ac.uk/collection/P01/current/OXYSZZ01/",
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
             name = "CTD_eMoF.csv",
             overwrite = TRUE)
