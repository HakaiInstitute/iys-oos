library(tidyverse)
library(lubridate)
library(readxl)
library(parsedate)
library(googledrive)
library(here)

drive_download("https://drive.google.com/open?id=1vecoPwVLsP5rpZ8pj-H4xjOld4otCD7O", path = here("POM", "raw_data", "POM_tidy.xlsx"))


### Event ----------------
sheet1 <- read_excel(here("POM", "raw_data", "POM_tidy.xlsx"), sheet = "Sheet1") %>%
  mutate(Time = format(Time, "%H:%M:%S"),
         eventDate = format_iso_8601(as.POSIXct(paste(Date, Time),
                                                format="%Y-%m-%d %H:%M:%S",
                                                # Based off Chlorophyll metadata which stated timezone was UTC+12, and values match
                                                tz = "Asia/Kamchatka")), 
         eventDate = str_replace(eventDate, "\\+00:00", "Z"),
         cruise = "GoA2019",
         station = paste(cruise, station, sep = "_Stn"),
         cast = paste(station, "cast1", sep = ":"),
         ndepth = paste(cast, `Sample depth`, sep=":niskin:"),
         sample = paste(ndepth, `Sample ID`, sep=":")
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

pom_sample <- sheet1 %>% 
  select(eventID = sample,
         parentEventID = ndepth) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "subsample")

pom_event <- bind_rows(pom_cruise, pom_station, pom_cast, pom_ndepth, pom_sample) %>% 
  select(eventID, parentEventID:maximumDepthInMetres, type) %>% 
  mutate(basisOfRecord = "Event")

write_csv(pom_event, here("POM", "tidy_data", "pom_event.csv"))
drive_upload(here("POM", "tidy_data", "pom_event.csv"),
             path = "https://drive.google.com/drive/folders/1WMsDahUdt9dKXGcoi2gcrjNmpGBgHOSB",
             name = "POM_event.csv",
             overwrite = TRUE)

### Measurement ---------------------------
pom_botdepth <- sheet1 %>%
  select(eventID = cast, Depth) %>% 
  pivot_longer(cols = Depth, names_to = "measurementType", values_to = "measurementValue") %>%
  mutate(measurementValue = as.character((measurementValue)),
         measurementID = paste(eventID, "depth", sep = ":"),
         measurementType = recode(measurementType,
                                  Depth = "seafloor depth"),
         measurementTypeID = "http://vocab.nerc.ac.uk/collection/C00/current/BRIDGE/",  # This is temporary unless another term is more accurate
         measurementUnit = "m",
         measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/ULAA/"
         ) %>% 
  select(measurementID, eventID, measurementType, measurementTypeID, measurementValue, measurementUnit, measurementUnitID)

pom_measurement <- sheet1 %>% 
  select(eventID = sample,
         `Volume filtered`,
         acidified = Acidified,
         d13C_VPDB = `d13CVPDB (‰)`, 
         `Total C (µg)`,
         d15N_air = `d15NAir (‰)`,
         `Total N (µg)`) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = `Volume filtered`:`Total N (µg)`, names_to = "measurementType", values_to = "measurementValue") %>% 
  mutate(measurementType = recode(measurementType,
                                  `Volume filtered` = "volume filtered",
                                  `Total C (µg)` = "total C",
                                  `Total N (µg)` = "total N"),
         measurementID = case_when(measurementType == "volume filtered" ~ paste(eventID, "vol", sep = ":"),
                                   measurementType == "acidified" ~ paste(eventID, "acid", sep = ":"),
                                   measurementType == "d13C_VPDB" ~ paste(eventID, "d13C", sep = ":"),
                                   measurementType == "d15N_air" ~ paste(eventID, "d15N", sep = ":"),
                                   measurementType == "total C" ~ paste(eventID, "Ctot", sep = ":"),
                                   measurementType == "total N" ~ paste(eventID, "Ntot", sep = ":")
                                   ),
         measurementTypeID = case_when(measurementType == "volume filtered" ~ "http://vocab.nerc.ac.uk/collection/S03/current/S0393/",
                                       measurementType == "acidified" ~ "http://vocab.nerc.ac.uk/collection/S03/current/S0366",
                                       measurementType == "d13C_VPDB" ~ "", # TBD
                                       measurementType == "d15N_air" ~ "http://vocab.nerc.ac.uk/collection/P01/current/D15NMTP1/",
                                       measurementType == "total C" ~ "",
                                       measurementType == "total N" ~ ""),
         measurementUnit = case_when(measurementType == "volume filtered" ~ "L",
                                     measurementType %in% c("d13C_VPDB", "d15N_air") ~ "ppt",
                                     measurementType %in% c("total C", "total N") ~ "ug"),
         measurementUnitID = case_when(measurementUnit == "L" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULIT/",
                                       measurementUnit == "‰" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPPT/",
                                       measurementUnit == "ug" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UGUG/")
  )


pom_measurementRemarks <- sheet1 %>% 
  select(eventID = sample, ends_with("Comment")) %>% 
  pivot_longer(cols = ends_with("Comment"), values_to = "measurementRemarks") %>% 
  mutate(measurementID = case_when(name == "C Comment" ~ paste(eventID, "Ctot", sep = ":"),
                                   name == "N Comment" ~ paste(eventID, "Ntot", sep = ":")
                                   )
  ) %>% 
  select(measurementID, measurementRemarks)

pom_measurementOrFact <- bind_rows(pom_botdepth, pom_measurement) %>% 
  left_join(pom_measurementRemarks) %>% 
  select(measurementID, eventID, measurementType, measurementTypeID, measurementValue, measurementUnit, measurementUnitID, measurementRemarks)
  

write_csv(pom_measurementOrFact, here("POM", "tidy_data", "pom_measurementOrFact.csv"))
drive_upload(here("POM", "tidy_data", "pom_measurementOrFact.csv"),
             path = "https://drive.google.com/drive/folders/1WMsDahUdt9dKXGcoi2gcrjNmpGBgHOSB",
             name = "POM_measurementOrFact.csv",
             overwrite = TRUE)
