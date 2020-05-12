# Load required packages
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(parsedate)
library(googledrive)
library(uuid)

drive_download(file = "https://docs.google.com/spreadsheets/d/1rtT6L5WSi_UiiXfoS95wnDqA8loc959T/edit#gid=668595086", 
               path = here::here("Trawl", "raw_data", 
                                 "2019_GoA_Fish_data_Hakai.xlsx"),
               overwrite = TRUE)

trawl <- read_excel(here("Trawl", "raw_data", "2019_GoA_Fish_data_Hakai.xlsx")) %>%
  mutate(eventDate = format_iso_8601(as.POSIXct(UTC_start)),
         eventDate = str_replace(eventDate, "\\+00:00", "Z"),
         cruise = "GoA2019", 
         station = paste(cruise, NUMBER, sep="_Stn"),
         trawl = paste(station, "trawl1", sep=":"))

### Event ----------------
trawl_cruise <- trawl %>% 
  select(eventID = cruise) %>% 
  distinct(eventID) %>% 
  mutate(type = "cruise")

trawl_station <- trawl %>% 
  select(eventID = station,
         parentEventID = cruise,
         eventDate,
         decimalLatitude = Y,
         decimalLongitude = X) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "station")

trawl_trawl <- trawl %>% 
  select(eventID = trawl,
         parentEventID = station) %>% 
  distinct(eventID, .keep_all = TRUE) %>% 
  mutate(type = "trawl")

trawl_event <- bind_rows(trawl_cruise, trawl_station, trawl_trawl) %>% 
  select(eventID, parentEventID:decimalLongitude, type)

# Re-order the Event Core:
order <- stringr::str_sort(trawl_event$eventID, numeric=TRUE)
trawl_event <- trawl_event[match(order, trawl_event$eventID),] %>%
  mutate(basisOfRecord = "HumanObservation")

write_csv(trawl_event, here("Trawl", "tidy_data", "trawl_event.csv"))
drive_upload(here("POM", "tidy_data", "pom_event.csv"),
             path = "",
             name = "trawl_event.csv",
             overwrite = TRUE)

### Occurrence ----------------

# Create one column with species:
trawl <- trawl %>%
  mutate(Species = paste(FISH1, FISH2, sep = " "))

# Determine the unique species caught throughout the entire expedition, to 
# determine whether these are present or absent at each station. 
unique_spp <- unique(trawl$Species)

# So a total of 46 unique species have been caught throughout the expedition.
# For each trawl, note for each of these species whether it was present or absent:

trawl_occ <- trawl %>% tidyr::complete(NUMBER, Species, 
                                       fill = list(PIECES=0)) %>%
  as.data.frame()
trawl_occ <- trawl_occ %>% group_by(NUMBER) %>% fill(trawl) %>%
  fill(trawl, .direction = "up") %>% ungroup(NUMBER)

# Add present or absent depending on whether "Pieces > 0"
trawl_occ <- trawl_occ %>% 
  mutate(occurrenceStatus = ifelse(PIECES > 0, "present", "absent")) %>%
  dplyr::rename(scientificName = Species) %>%
  select(eventID = trawl, 
         scientificName, occurrenceStatus)

# Two columns have to be added and populated: occurrenceID and scientificNameID:
trawl_occ <- trawl_occ %>% 
  mutate(occurrenceID = paste("occ", row_number(), sep = "_"),
         scientificNameID = case_when(
           scientificName == "Abraliopsis felis" ~ "urn:lsid:marinespecies.org:taxname:341849",
           scientificName == "Aequorea sp." ~ "urn:lsid:marinespecies.org:taxname:116998",
           scientificName == "Anotopterus nikparini" ~ "urn:lsid:marinespecies.org:taxname:272040",
           scientificName == "Aptocyclus ventricosus" ~ "urn:lsid:marinespecies.org:taxname:254299",
           scientificName == "Aurelia labiata" ~ "urn:lsid:marinespecies.org:taxname:287213",
           scientificName == "Aurelia limbata" ~ "urn:lsid:marinespecies.org:taxname:158199",
           scientificName == "Belonella borealis" ~ "urn:lsid:marinespecies.org:taxname:410406",
           scientificName == "Boreoteuthis borealis" ~ "urn:lsid:marinespecies.org:taxname:342326",
           # Different genus name to what was provided: Gonatopsis vs. Boreoteuthis
           scientificName == "Calycopsis sp." ~ "urn:lsid:marinespecies.org:taxname:117028",
           scientificName == "Chiroteuthis calyx" ~ "urn:lsid:marinespecies.org:taxname:341796",
           scientificName == "Chrysaora melonaster" ~ "urn:lsid:marinespecies.org:taxname:287209",
           # Slightly different spelling of species name
           scientificName == "Corolla calceola" ~ "urn:lsid:marinespecies.org:taxname:532663",
           scientificName == "Diaphus theta" ~ "urn:lsid:marinespecies.org:taxname:272694",
           scientificName == "Gasterosteus aculeatus" ~ "urn:lsid:marinespecies.org:taxname:126505",
           scientificName == "Gonatidae sp." ~ "urn:lsid:marinespecies.org:taxname:11743",
           scientificName == "Gonatus madokai" ~ "urn:lsid:marinespecies.org:taxname:341858",
           scientificName == "Gonatus onyx" ~ "urn:lsid:marinespecies.org:taxname:341859",
           scientificName == "Gonatus sp." ~ "urn:lsid:marinespecies.org:taxname:138036",
           scientificName == "Hormiphora cucumis" ~ "urn:lsid:marinespecies.org:taxname:106382",
           scientificName == "Icichthys lockingtoni" ~ "urn:lsid:marinespecies.org:taxname:279354",
           scientificName == "Japetella diaphana" ~ "urn:lsid:marinespecies.org:taxname:138849",
           scientificName == "Lestidium ringens" ~ "urn:lsid:marinespecies.org:taxname:272096",
           # slightly different accepted name than what was recorded
           scientificName == "Lipolagus ochotensis" ~ "urn:lsid:marinespecies.org:taxname:281374",
           scientificName == "Microstomus pacificus" ~ "urn:lsid:marinespecies.org:taxname:274294",
           scientificName == "Moroteuthis robusta" ~ "urn:lsid:marinespecies.org:taxname:410385",
           # different genus name than what was recorded
           scientificName == "Oncorhynchus gorbuscha" ~ "urn:lsid:marinespecies.org:taxname:127182",
           scientificName == "Oncorhynchus keta" ~ "urn:lsid:marinespecies.org:taxname:127183",
           scientificName == "Oncorhynchus kisutch" ~ "urn:lsid:marinespecies.org:taxname:127184",
           scientificName == "Oncorhynchus nerka" ~ "urn:lsid:marinespecies.org:taxname:254569",
           scientificName == "Oncorhynchus tschawytscha" ~ "urn:lsid:marinespecies.org:taxname:158075",
           scientificName == "Onychoteuthis borealijaponica" ~ "urn:lsid:marinespecies.org:taxname:342069",
           scientificName == "Paralepididae gen. sp." ~ "urn:lsid:marinespecies.org:taxname:125447",
           scientificName == "Periphylla periphylla" ~ "urn:lsid:marinespecies.org:taxname:135294",
           scientificName == "Phacellophora camtshchatica" ~ "urn:lsid:marinespecies.org:taxname:135309",
           # Slightly different spelling
           scientificName == "Salpa sp." ~ "urn:lsid:marinespecies.org:taxname:137233",
           scientificName == "Sebastes melanops" ~ "urn:lsid:marinespecies.org:taxname:274817",
           scientificName == "Sergestes similis" ~ "urn:lsid:marinespecies.org:taxname:514127",
           # Different accepted genus name
           scientificName == "Siphonophora sp." ~ "urn:lsid:marinespecies.org:taxname:1371",
           scientificName == "Squalus acanthias" ~ "urn:lsid:marinespecies.org:taxname:105923",
           scientificName == "Stenobrachius leucopsarus" ~ "urn:lsid:marinespecies.org:taxname:254363",
           scientificName == "Symbolophorus californiense" ~ "urn:lsid:marinespecies.org:taxname:272733",
           # Slightly different species name spelling
           scientificName == "Tarletonbeania crenularis" ~ "urn:lsid:marinespecies.org:taxname:282927",
           scientificName == "Thalassenchelys coheni" ~ "urn:lsid:marinespecies.org:taxname:282952",
           scientificName == "Thetys vagina" ~ "urn:lsid:marinespecies.org:taxname:137281",
           scientificName == "Thysanoessa spinifera" ~ "urn:lsid:marinespecies.org:taxname:237874",
           scientificName == "Zaprora silenus" ~ "urn:lsid:marinespecies.org:taxname:254353"
# Please take into account whether or not the Status of the species on WoRMS is 
# accepted or not!
         )) %>%
  select(eventID, occurrenceID, scientificName, scientificNameID, occurrenceStatus)

# Confirm whether each station in trawl_occ contains all unique_spp:
check_spp <- trawl_occ %>% group_by(eventID) %>%
  summarise(count = n_distinct(scientificNameID)) 
for (i in 1:nrow(check_spp)) {
  if(check_spp$count[i] == length(unique_spp)) {
    print("TRUE") 
  } else {
    print("FALSE")
  }
}

# TRUE for all stations - 

write_csv(trawl_occ, here("Trawl", "tidy_data", "trawl_occ.csv"))
drive_upload(here("Trawl", "tidy_data", "trawl_occ.csv"),
             path = "",
             name = "trawl_occ.csv",
             overwrite = TRUE)

### measurementOrFact  ----------------
