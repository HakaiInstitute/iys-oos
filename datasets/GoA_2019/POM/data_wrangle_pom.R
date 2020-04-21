library(tidyverse)
library(lubridate)
library(readxl)
library(parsedate)
library(googledrive)
library(here)

drive_download("https://drive.google.com/open?id=1vecoPwVLsP5rpZ8pj-H4xjOld4otCD7O", path = here("POM", "raw_data", "POM_tidy.xlsx"))

sheet1 <- read_excel(here("POM", "raw_data", "POM_tidy.xlsx"), sheet = "Sheet1") %>%
  mutate(Time = format(Time, "%H:%M:%S"),
         eventDate = format_ISO8601(as.POSIXct(paste(Date, Time),
                                               # Based off Chlorophyll metadata which stated timezone was UTC+12, and values match
                                               tz = "Asia/Kamchatka"), 
                                    usetz = TRUE),
         cruise = "GoA2019",
         station = paste(cruise, station, sep = "_Stn"),
         cast = paste(station, "cast1", sep = ":"),
         ndepth = paste(cast, `Sample depth`, sep=":niskin:")
         )
  
pom_cruise <- sheet1 %>% 
  select(eventID = cruise) %>% 
  distinct(eventID) %>% 
  mutate(type = "cruise")

pom_station <- sheet1 %>% 
  select(eventID = station,
         parentEventID = cruise,
         eventDate,
         decimalLatitude = Latitude,
         decimalLongitude = Longitude) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "station")

pom_cast <- sheet1 %>% 
  select(eventID = cast,
         parentEventID = station) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "cast")

pom_ndepth <- sheet1 %>% 
  select(eventID = ndepth,
         parentEventID = cast,
         minimumDepthInMetres = `Sample depth`,
         maximumDepthInMetres = `Sample depth`) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "sample")

pom_event <- bind_rows(pom_cruise, pom_station, pom_cast, pom_ndepth) %>% 
  select(eventID, parentEventID:maximumDepthInMetres, type) %>% 
  mutate(basisOfRecord = "Event")
