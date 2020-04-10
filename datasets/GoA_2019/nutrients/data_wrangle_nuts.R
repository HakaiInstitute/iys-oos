library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(parsedate)
library(googledrive)

drive_download("https://drive.google.com/open?id=1MQysK1BEp9xcZH64ilq63Sx3ri5SJYCx", path = here("nutrients", "raw_data", "IYS_GoA_2019_Hydro&Chemistry_20190326.xlsx"))

## Chemistry ---------------------------------------------------------------------

chemistry <- read_excel(here("nutrients", "raw_data", "IYS_GoA_2019_Hydro&Chemistry_20190326.xlsx"), sheet = "Chemistry_GoA") %>% 
  mutate(locationID = paste(Cruise, Station, sep="_"),
         eventID = paste(locationID, `Depth [m]`, sep="_"))

nut_loc <- chemistry %>% # Locations in both Chemistry and Hydro are the same
  select(locationID, 
         decimalLatitude = Latitude, 
         decimalLongitude = Longitude, 
         maximumDepthInMeters = `Bot. Depth [m]`) %>% 
  distinct(locationID, .keep_all = TRUE)

write_csv(nut_loc, here("nutrients", "raw_data", "nut_location.csv"))

chem_event <- chemistry %>% 
  select(eventID,
         locationID,
         depth = `Depth [m]`) %>% 
  distinct(eventID, .keep_all = TRUE)

write_csv(chem_event, here("nutrients", "raw_data", "chem_event.csv"))

chem_measurement <- chemistry %>% 
  select(-c(1:6), -locationID, -...18) %>% 
  pivot_longer(`Temperature [°C]`:`Oxygen by Rinko [ml/l]`, names_to = "measurementType", values_to = "measurementValue") %>% 
  # Need to confirm what some of the units are
  mutate(measurementUnit = case_when(str_detect(measurementType, "Temp") ~ "C",
                                     str_detect(measurementType, "psu") ~ "PSU",
                                     str_detect(measurementType, "ml/l") ~ "mL L-1",
                                     str_detect(measurementType, "%") ~ "percent",
                                     str_detect(measurementType, "mkM/l") ~ "mkM L-1"
                                     ),
         measurementType = recode(measurementType,
                                  "Temperature [°C]" = "temperature",
                                  "Salinity [psu]" = "salinity",
                                  "O2 [ml/l]" = "O2",
                                  "[%] oxygen saturation" = "oxygen_sat",
                                  "biochemichal oxygen consumption (for 5 days) [ml/l]" = "BOD_5",
                                  "Si [mkM/l]" = "silicate",
                                  "DIP [mkM/l]" = "dissolved inorganic P",
                                  "DIN [mkM/l]" = "dissolved inorganic N",
                                  "NO2  [mkM/l]" = "nitrogen dioxiode",
                                  "NO3  [mkM/l]" = "nitrate",
                                  "Oxygen by Rinko [ml/l]" = "oxygen_Rinko"),
         measurementID = case_when(measurementType == "temperature" ~ paste(eventID, "temp", sep="_"),
                                   measurementType == "salinity" ~ paste(eventID, "sal", sep="_"),
                                   measurementType == "pH" ~ paste(eventID, "pH", sep="_"),
                                   measurementType == "oxygen_sat" ~ paste(eventID, "oxsat"),
                                   measurementType == "BOD_5" ~ paste(eventID, "BOD", sep="_"),
                                   measurementType == "silicate" ~ paste(eventID, "si", sep="_"),
                                   measurementType == "dissolved inorganic P" ~ paste(eventID, "DIP", sep="_"),
                                   measurementType == "dissolved inorganic N" ~ paste(eventID, "DIN", sep="_"),
                                   measurementType == "nitrogen dioxiode" ~ paste(eventID, "NO2", sep="_"),
                                   measurementType == "nitrate" ~ paste(eventID, "NO3", sep="_"),
                                   measurementType == "oxygen_Rinko" ~ paste(eventID, "oxrinko", sep="_")
         )
  ) %>% 
  drop_na(measurementValue) %>% 
  select(measurementID, eventID, measurementType, measurementValue, measurementUnit)
  
write_csv(chem_measurement, here("nutrients", "raw_data", "chem_measurement.csv"))



## Temperature & Salinity ----------------------------------------------------
ts <- read_excel(here("nutrients", "raw_data", "IYS_GoA_2019_Hydro&Chemistry_20190326.xlsx"), sheet = "Hydro_GoA") %>% 
  mutate(locationID = paste(Cruise, Station, sep="_"),
         eventID = paste(locationID, `DEPTH`, sep="_"))

# ts_loc <- ts %>% 
#   select(locationID, 
#          decimalLatitude = Latitude, 
#          decimalLongitude = Longitude, 
#          maximumDepthInMeters = `Bot. Depth`) %>% 
#   distinct(locationID, .keep_all = TRUE)

# Test to see if locations are the same
# qc <- full_join(ts_loc, chem_loc, by = "locationID", ignore) %>% 
#   mutate(qclat = ifelse(round(decimalLatitude.x, digits=4) == round(decimalLatitude.y, digits=4),"ok","error"),
#          qclong = ifelse(round(decimalLongitude.x, digits=4) == round(decimalLongitude.y, digits=4),"ok","error"),
#          qcdepth = ifelse(maximumDepthInMeters.x == maximumDepthInMeters.y,"ok","error"))
# They are!

ts_event <- ts %>% 
  select(eventID,
         locationID,
         depth = `DEPTH`) %>% 
  distinct(eventID, .keep_all = TRUE)

write_csv(ts_event, here("nutrients", "raw_data", "tempsal_event.csv"))

ts_measurement <- ts %>% 
  select(eventID, `TEMP_SBE [degC]`, `SAL_SBE [psu]`) %>% 
  pivot_longer(c(`TEMP_SBE [degC]`, `SAL_SBE [psu]`), names_to = "measurementType", values_to = "measurementValue") %>% 
  mutate(measurementType = recode(measurementType,
                                  `TEMP_SBE [degC]` = "temperature",
                                  `SAL_SBE [psu]` = "salinity"),
         measurementUnit = case_when(measurementType == "temperature" ~ "C",
                                     measurementType == "salinity" ~ "PSU"))

write_csv(ts_measurement, here("nutrients", "raw_data", "tempsal_measurement.csv"))


ctd_qc <- chem_measurement %>% 
  filter(measurementType == "temperature" | measurementType == "salinity") %>% 
  left_join(ts_measurement, by = c("eventID", "measurementType")) %>% 
  mutate(match = ifelse(measurementValue.x == measurementValue.y, "yes", "no")) %>% 
  filter(is.na(match))
# With the exception of the 0m depths missing from "Hydro", all values are the same between the two tabs