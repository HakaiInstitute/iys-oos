# ---
#  title: Chlorophyll-a and Phaeopigment measurements 2019 Gulf of Alaska expedition
#  authors: Julian Gan & Tim van der Stap, Hakai Institute
#  date: April 29, 2020
#  objective: Wrangling of chlorophyll and phaeopigment data collected on 
#  the GoA2019 cruise, according to Darwin Core Archive (DwC-A) standards
# ---

# Load required packages
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(parsedate)
library(googledrive)
library(uuid)

drive_download("https://drive.google.com/file/d/131MbPdsbAoggip5SVxkGbkyWXX4r5U-V/view", 
               path = here::here("chlorophyll", "raw_data", 
                                 "IYSchl_Hunt&Pakhomov.xlsx"),
               overwrite = TRUE)

chl <- read_excel(here("chlorophyll", "raw_data", 
                       "IYSchl_Hunt&Pakhomov.xlsx"), 
                  sheet = "Chl data") %>% 
  # Convert the standalone time value (UTC+12) into ISO8601 extended format
  mutate(Time = format(Time, format = "%H:%M:%S"),
         dateTime = format_iso_8601(as.POSIXct(paste(Date, Time), 
                                               format="%Y-%m-%d %H:%M:%S", 
                                               tz="Asia/Kamchatka")),
         dateTime = str_replace(dateTime, "\\+00:00", "Z"),
         cruise = "GoA2019",
         station = paste(cruise, Station, sep="_Stn"),
         ndepth = paste(station, Depth, sep=":"),
         sample = paste(ndepth, "T", sep=":"),
         sample = paste(sample, Tube, sep="")
  ) 

chl_cruise <- chl %>% 
  select(eventID = cruise) %>%
  distinct(eventID) %>%
  mutate(type = "cruise")

# Join date to station
chl_station <- chl %>%
  select(eventID = station,
         parentEventID = cruise,
         eventDate = dateTime,
         decimalLatitude = Latitude,
         decimalLongitude = Longitude) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "station")

chl_ndepth <- chl %>% 
  select(eventID = ndepth,
         parentEventID = station,
         minimumDepthInMetres = Depth,
         maximumDepthInMetres = Depth) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "sample")

chl_sample <- chl %>%
  select(eventID = sample,
         parentEventID = ndepth) %>%
  distinct(eventID, .keep_all = TRUE) %>%
  mutate(type = "subsample")

chl_event <- bind_rows(chl_cruise, chl_station, chl_ndepth, chl_sample) %>% 
  select(eventID, parentEventID:maximumDepthInMetres, type) 

# If you wish to re-order the Event Core, use the following code:
order <- stringr::str_sort(chl_event$eventID, numeric=TRUE)
chl_event <- chl_event[match(order, chl_event$eventID),]

# Output .csv file in local folder and upload to GoogleDrive. 
write_csv(chl_event, here("chlorophyll", "tidy_data", "chl_event_fnl.csv"))
drive_upload(here("chlorophyll", "tidy_data", "chl_event_fnl.csv"),
             path = "https://drive.google.com/drive/folders/1TqGK3ih2b7hPWUion5utDgxIEVLYRm5W",
             name = "Chlorophyll_a_event.csv",
             overwrite = TRUE)

## MeasurementOrFact -----------------------------------------------
chl_measurement <- chl %>%
  select(eventID = sample, 
         Chl:Phe) %>%
  # Gather Chl and Phe into measurement types
  pivot_longer(c(`Chl`, `Phe`), names_to = "measurementType", 
               values_to = "measurementValue") %>% 
  mutate(measurementID = case_when(measurementType == "Chl" ~ paste(eventID,"chl", sep="_"),
                                   measurementType == "Phe" ~ paste(eventID,"phe", sep="_")),
         measurementType = recode(measurementType, 
                                  Chl = "Concentration of Chlorophyll a",
                                  Phe = "Concentration of Phaeopigments"),
         measurementTypeID = recode(measurementType, #URIs from NERC website
                                    "Concentration of Chlorophyll a" = "http://vocab.nerc.ac.uk/collection/P02/current/CPWC/",
                                    "Concentration of Phaeopigments" = "http://vocab.nerc.ac.uk/collection/P02/current/PHWC/"),
         measurementUnit = case_when(measurementType == "Concentration of Chlorophyll a" ~ "mg/m^3",
                                     measurementType == "Concentration of Phaeopigments" ~ "mg/m^3"),
         measurementUnitID = case_when(measurementType == "Concentration of Chlorophyll a" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMMC/",
                                       measurementType == "Concentration of Phaeopigments" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMMC/")
         
  ) %>%
  select(measurementID, measurementType, measurementTypeID,
         measurementValue, measurementUnit, measurementUnitID)

# Write up csv file and upload to GoogleDrive folder
write_csv(chl_measurement, here("chlorophyll", "tidy_data", "chl_measurement_fnl.csv"))
drive_upload(here("chlorophyll", "tidy_data", "chl_measurement_fnl.csv"),
             path = "https://drive.google.com/drive/folders/1TqGK3ih2b7hPWUion5utDgxIEVLYRm5W",
             name = "Chlorophyll_a_measurement.csv",
             overwrite = TRUE)

# Location of sampling event
chl_loc <- chl %>% 
  distinct(Station, Region, Latitude, Longitude)
write_csv(chl_loc, here("chlorophyll", "tidy_data", "chl_location.csv"))
