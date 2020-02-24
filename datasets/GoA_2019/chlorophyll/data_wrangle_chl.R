library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(parsedate)
library(googledrive)

drive_download("https://drive.google.com/open?id=1y0obUAsdWeYp2nFOB596B5rGQzNyQxwN", path = here("datasets", "chlorophyll", "raw_data", "IYSchl_Hunt&Pakhomov.xlsx"))

chl <- read_excel(here("datasets", "chlorophyll", "raw_data", "IYSchl_Hunt&Pakhomov.xlsx"), sheet = "Chl data") %>% 
  # Convert the standalone time value (UTC+12) into ISO8601 extended format
  mutate(Time = format(Time, format = "%H:%M:%S"),
         dateTime = format_iso_8601(as.POSIXct(paste(Date, Time), 
                                               format="%Y-%m-%d %H:%M:%S", 
                                               tz="Asia/Kamchatka")),
         dateTime = str_replace(dateTime, "\\+00:00", "Z"),
         locationID = paste("GoA_2019", Station, sep="_"),
         eventID = paste(locationID,Depth, sep="_")
         )

chl_loc <- chl %>% 
  distinct(locationID, Region, Latitude, Longitude)
write_csv(chl_loc, here("datasets", "chlorophyll", "raw_data", "chl_location.csv"))

chl_event <- chl %>% 
  select(eventID, locationID, eventDate = dateTime, Depth) %>% 
  distinct(eventID, .keep_all = TRUE)
write_csv(chl_event, here("datasets", "chlorophyll", "raw_data", "chl_event.csv"))

chl_measurement <- chl %>%
  # Gather Chl and Phe into measurement types
  pivot_longer(c(`Chl`, `Phe`), names_to = "measurementType", values_to = "measurementValue") %>% 
  mutate(measurementID = case_when(measurementType == "Chl" ~ paste("tube",Tube,"_chl", sep=""),
                                   measurementType == "Phe" ~ paste("tube",Tube,"_phe", sep="")),
         measurementType = recode(measurementType, 
                                  Chl = "chlorophyll_a",
                                  Phe = "phaetopigments"),
         measurementUnit = case_when(measurementType == "chlorophyll_a" ~ "mg m^-3",
                                     measurementType == "phaetopigments" ~ "mg m^-3")
         ) %>% 
  select(measurementID, eventID, measurementType, measurementValue, measurementUnit)

write_csv(chl_measurement, here("datasets", "chlorophyll", "raw_data", "chl_measurement.csv"))


