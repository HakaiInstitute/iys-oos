library(tidyverse)
library(lubridate)
library(readxl)
library(parsedate)
library(googledrive)
library(here)

drive_download("https://drive.google.com/open?id=1MQysK1BEp9xcZH64ilq63Sx3ri5SJYCx", path = here("nutrients", "raw_data", "IYS_GoA_2019_Hydro&Chemistry_20190326.xlsx"))


## Event Core --------------
chemistry <- read_excel(here("nutrients", "raw_data", "IYS_GoA_2019_Hydro&Chemistry_20190326.xlsx"), sheet = "Chemistry_GoA") %>% 
  # Create columns to turn into eventIDs
  mutate(cruise = "GoA2019",
         station = paste(cruise, Station, sep="_Stn"),
         cast = paste(station, "cast1", sep = ":"),
         ndepth = paste(cast, `Depth [m]`, sep=":niskin:"))

nut_cruise <- chemistry %>% 
  select(eventID = cruise) %>% 
  distinct(eventID) %>% 
  mutate(type = "cruise")

# Join date to station
hydro <- read_excel(here("nutrients", "raw_data", "IYS_GoA_2019_Hydro&Chemistry_20190326.xlsx"), sheet = "Hydro_GoA") %>% 
  mutate(station = paste("GoA2019", Station, sep="_Stn"))

nut_station <- chemistry %>%
  left_join(
    select(hydro,
           station,
           date)
  ) %>% 
  select(eventID = station,
         parentEventID = cruise,
         eventDate = date,
         decimalLatitude = Latitude,
         decimalLongitude = Longitude) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "station")

nut_cast <- chemistry %>% 
  select(eventID = cast,
         parentEventID = station) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "cast")

nut_ndepth <- chemistry %>% 
  select(eventID = ndepth,
         parentEventID = cast,
         minimumDepthInMetres = `Depth [m]`,
         maximumDepthInMetres = `Depth [m]`) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "sample")

nut_event <- bind_rows(nut_cruise, nut_station, nut_cast, nut_ndepth) %>% 
  select(eventID, parentEventID:maximumDepthInMetres, type) %>% 
  mutate(basisOfRecord = "Event")

write_csv(nut_event, here("nutrients", "tidy_data", "nut_event.csv"))
drive_upload(here("nutrients", "tidy_data", "nut_event.csv"),
             path = "https://drive.google.com/drive/u/0/folders/1M0JRuzW_rxwz9jZPVV-oDv5AARUS2K2w",
             name = "nutrient_Event",
             overwrite = TRUE)

## MeasurementOrFact -------------------
nut_measurement <- chemistry %>% 
  select(ID = ndepth, 
         `Si [mkM/l]`:`NO3  [mkM/l]`) %>% 
  pivot_longer(cols = `Si [mkM/l]`:`NO3  [mkM/l]`, names_to = "measurementType", values_to = "measurementValue") %>% 
  mutate(measurementUnit = "mkM L-1",
         measurementType = recode(measurementType,
                                  "Si [mkM/l]" = "silicate",
                                  "DIP [mkM/l]" = "dissolved inorganic P",
                                  "DIN [mkM/l]" = "dissolved inorganic N",
                                  "NO2  [mkM/l]" = "nitrogen dioxiode",
                                  "NO3  [mkM/l]" = "nitrate",
                                  ),
         measurementID = case_when(measurementType == "silicate" ~ paste(ID, "si", sep=":"),
                                   measurementType == "dissolved inorganic P" ~ paste(ID, "DIP", sep=":"),
                                   measurementType == "dissolved inorganic N" ~ paste(ID, "DIN", sep=":"),
                                   measurementType == "nitrogen dioxiode" ~ paste(ID, "NO2", sep=":"),
                                   measurementType == "nitrate" ~ paste(ID, "NO3", sep=":"),
                                   )
         )

write_csv(nut_measurement, here("nutrients", "tidy_data", "nut_measurement.csv"))
drive_upload(here("nutrients", "tidy_data", "nut_measurement.csv"),
             path = "https://drive.google.com/drive/u/0/folders/1M0JRuzW_rxwz9jZPVV-oDv5AARUS2K2w",
             name = "nutrient_MeasurementOrFact",
             overwrite = TRUE)
