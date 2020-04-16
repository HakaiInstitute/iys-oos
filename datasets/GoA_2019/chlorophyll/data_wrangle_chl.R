# ---
#  title: Chlorophyll-a and Phaeopigment measurements
#  author: Julian Gan & Tim van der Stap, Hakai Institute
#  date: "`r format(Sys.Date())`"
#  output:
#    html_document:
#     keep_md: TRUE
# ---

# Load required packages
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(parsedate)
library(googledrive)
library(uuid)

drive_download("https://drive.google.com/open?id=1y0obUAsdWeYp2nFOB596B5rGQzNyQxwN", 
               path = here::here("chlorophyll", "raw_data", "IYSchl_Hunt&Pakhomov.xlsx"),
               overwrite = TRUE)

chl <- read_excel(here("chlorophyll", "raw_data", "IYSchl_Hunt&Pakhomov.xlsx"), 
                  sheet = "Chl data") %>% 
  # Convert the standalone time value (UTC+12) into ISO8601 extended format
  mutate(Time = format(Time, format = "%H:%M:%S"),
         dateTime = format_iso_8601(as.POSIXct(paste(Date, Time), 
                                               format="%Y-%m-%d %H:%M:%S", 
                                               tz="Asia/Kamchatka")),
         dateTime = str_replace(dateTime, "\\+00:00", "Z")
         ) 

# Rename columns to fit OBIS Darwin Core terminology (https://obis.org/manual/dataformat/)
names(chl)[names(chl) == "Latitude"] <- "decimalLatitude"
names(chl)[names(chl) == "Longitude"] <- "decimalLongitude"

# Create two distinct dataframes: Event and extended MeasurementOrFact (eMoF)
# In this dataset we are only dealing with a-biotic data, and measurements can be 
# linked directly to the event -- no need for an Occurrence Core/table. 

chl_event <- chl %>%
  mutate(parentEventID = paste(Cruise, "Station", Station, sep="_"),
         eventID = paste(parentEventID, "Depth", Depth, sep="_"),
         minimumDepthInMeters = Depth,
         maximumDepthInMeters = Depth)
chl_event <- chl_event %>% 
  select(eventID, parentEventID, eventDate = dateTime, 
         decimalLongitude, decimalLatitude,
         minimumDepthInMeters, maximumDepthInMeters)
chl_event$decimalLatitude <- " "
chl_event$decimalLongitude <- " "
chl_event$eventDate <- " "

# Second table is made with Station-specific lat and longs, and sampling dates.
# The data collected at specific depths at each station will be recorded as
# `nested` within that eventID.
chl_2 <- chl %>%
  select(Cruise, eventDate = dateTime, Station, decimalLatitude, decimalLongitude) %>%
  unique() %>% # selecting unique stations with associated coordinates
  mutate(eventID = paste(Cruise, "Station", Station, sep="_")) %>%
  select(eventID, eventDate, decimalLatitude, decimalLongitude)

# Add columns so that the chl_2 dataframe matches the chl_event dataframe.
# Then combine the two dataframes. 
chl_2$parentEventID <- " "
chl_2$minimumDepthInMeters <- " "
chl_2$maximumDepthInMeters <- " "

chl_total <- rbind(chl_event, chl_2) %>%
  mutate(basisOfRecord = "MachineObservation") # add missing required field

# Though dataframes are now joined, the rows with Station-specific data are
# listed at the bottom, so to restructure the dataframe:
neworder <- stringr::str_sort(chl_total$eventID, numeric=TRUE)
# str_sort() was needed given how eventIDs were a mixture of numbers and letters. 
# Match with original dataframe to obtain final Chlorophyll-a dataframe:
chl_event_fnl <- chl_total[match(neworder, chl_total$eventID),]

#  Need to add another `Event` layer (GoA2019) for each Station
chl_event_fnl$parentEventID <- ifelse(grepl(" ", chl_event_fnl$parentEventID),
                              "GoA2019", chl_event_fnl$parentEventID)

#  Add extra row to the top of the data frame, with eventID = GoA2019 (the 
#  highest level of eventID in our dataset)
chl_event_fnl <- chl_event_fnl %>% 
  do(add_row(., .before = 0))
chl_event_fnl$eventID[is.na(chl_event_fnl$eventID)] <- "GoA2019"
chl_event_fnl[is.na(chl_event_fnl)] <- " " 

# Add column `type` to Event Core
chl_event_fnl$type <- " "
chl_event_fnl <- chl_event_fnl %>%
  mutate(type = case_when(eventID == "GoA2019" ~ "Cruise",
                          parentEventID == "GoA2019" ~ "Station",
                          grepl("_Depth_", eventID) ~ "Sample"))         

# Output .csv file in local folder and upload to GoogleDrive. 
write_csv(chl_event_fnl, here("chlorophyll", "raw_data", "chl_event_fnl.csv"))
drive_upload("./chlorophyll/raw_data/chl_event_fnl.csv",
             path = "https://drive.google.com/drive/folders/1TqGK3ih2b7hPWUion5utDgxIEVLYRm5W",
             name = "Chlorophyll_a_event.csv",
             overwrite = TRUE)
             # overwrite function does not seem to work for me; always creates a duplicate
             # .csv file in GoogleDrive.

# Extended measurement or fact (eMoF) table:
chl_measurement <- chl_event_fnl %>%
   select(eventID, parentEventID) %>%
   as.data.frame()
 
chl_measurement$eventID <- ifelse(grepl("_Depth_", chl_measurement$eventID),
                             chl_measurement$eventID, NA) 
chl_measurement <- chl_measurement %>% 
   tidyr::drop_na(eventID)

# Duplicate each row so we can associate a measurementID for each measured parameter
# to each depth subsample. 
chl_measurement <- chl_measurement[rep(1:nrow(chl_measurement), each = 2),]

chl_measurement_2 <- chl %>%
  # Gather Chl and Phe into measurement types
  pivot_longer(c(`Chl`, `Phe`), names_to = "measurementType", 
               values_to = "measurementValue") %>% 
  mutate(measurementID = case_when(measurementType == "Chl" ~ paste("tube",Tube,"_chl", sep=""),
                                   measurementType == "Phe" ~ paste("tube",Tube,"_phe", sep="")),
         measurementType = recode(measurementType, 
                                  Chl = "Concentration of Chlorophyll a",
                                  Phe = "Concentration of Phaeopigments"),
         measurementTypeID = recode(measurementType, #URIs from NERC website
                                    "Concentration of Chlorophyll a" = "http://vocab.nerc.ac.uk/collection/P02/current/CPWC/",
                                    "Concentration of Phaeopigments" = "http://vocab.nerc.ac.uk/collection/P02/current/PHWC/"),
         measurementUnit = case_when(measurementType == "Concentration of Chlorophyll a" ~ "mg/m^3",
                                     measurementType == "Concentration of Phaeopigments" ~ "mg/m^3"),
         measurementUnitID = case_when(measurementType == "Concentration of Chlorophyll a" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMMC/",
                                     measurementType == "Concentration of Phaeopigments" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMMC/"),
         occurrenceID = " "
         ) 

# Combine these two dataframes, en select relevant columns for eMoF Core:
chl_measurement_fnl <- cbind(chl_measurement, chl_measurement_2) %>% 
  select(eventID, occurrenceID, measurementID, measurementType, measurementTypeID,
         measurementValue, measurementUnit, measurementUnitID)

# Write up .csv file and upload to GoogleDrive folder
write_csv(chl_measurement_fnl, here("chlorophyll", "raw_data", "chl_measurement_fnl.csv"))
drive_upload("./chlorophyll/raw_data/chl_measurement_fnl.csv",
             path = "https://drive.google.com/drive/folders/1TqGK3ih2b7hPWUion5utDgxIEVLYRm5W",
             name = "Chlorophyll_a_measurement.csv",
             overwrite = TRUE)

# Location of sampling event
chl_loc <- chl %>% 
  distinct(parentEventID, Region, decimalLatitude, decimalLongitude)
write_csv(chl_loc, here("chlorophyll", "raw_data", "chl_location.csv"))
