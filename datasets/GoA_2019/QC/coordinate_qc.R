library(tidyverse)
library(googledrive)
library(here)
library(readxl)

drive_download("https://drive.google.com/open?id=1YlWB7cKcZ9HbeYuizjEYaUGuWJrCg0ZY", path = here("QC", "raw_data", "chl_event.csv"))
drive_download("https://drive.google.com/open?id=15uXYVGMnYO98qicnHaHZu1ak5f2H_MOM", path = here("QC", "raw_data", "nut_event.csv"))
drive_download("https://drive.google.com/open?id=1QDh4LZF3PMxx1ZsvEGfHsBhfOcPV3esE", path = here("QC", "raw_data", "pom_event.csv"))
drive_download("https://drive.google.com/open?id=1c_GQ00SjQqmLBcMc8md4WAJjLPZ9u2W1", path = here("QC", "raw_data", "ctd_event.csv"))
drive_download("https://drive.google.com/open?id=1rtT6L5WSi_UiiXfoS95wnDqA8loc959T", path = here("QC", "raw_data", "trawl_raw.xlsx"))

chl <- read_csv(here("QC", "chl_event.csv")) %>% 
  filter(type == "station") %>% 
  mutate(eventID = str_replace(eventID, "GoA2019_Stn","chl")) %>% 
  select(station = eventID, latitude = decimalLatitude, longitude = decimalLongitude)
write_csv(chl, here("QC", "chl_stn.csv"))


nut <- read_csv(here("QC", "nut_event.csv")) %>% 
  filter(type == "station") %>% 
  mutate(eventID = str_replace(eventID, "GoA2019_Stn","nut"),
         decimalLongitude = decimalLongitude - 360) %>% 
  select(station = eventID, latitude = decimalLatitude, longitude = decimalLongitude)
write_csv(nut, here("QC", "nut_stn.csv"))

pom <- read_csv(here("QC", "pom_event.csv")) %>% 
  filter(type == "station") %>% 
  mutate(eventID = str_replace(eventID, "GoA2019_Stn","pom")) %>% 
  select(station = eventID, latitude = decimalLatitude, longitude = decimalLongitude)
write_csv(pom, here("QC", "pom_stn.csv"))


ctd <- read_csv(here("QC", "ctd_event.csv")) %>% 
  filter(type == "station") %>% 
  mutate(eventID = str_replace(eventID, "GoA2019_Stn","ctd")) %>% 
  select(station = eventID, latitude = decimalLatitude, longitude = decimalLongitude)
write_csv(ctd, here("QC", "ctd_stn.csv"))

trawl <- read_xlsx(here("QC", "trawl_raw.xlsx")) %>% 
  distinct(NUMBER, .keep_all = TRUE) %>% 
  mutate(NUMBER = paste("trawl", NUMBER, sep = "")) %>% 
  select(station = NUMBER, latitude = Y, longitude = X)
write_csv(trawl, here("QC", "trawl_stn.csv"))                   
