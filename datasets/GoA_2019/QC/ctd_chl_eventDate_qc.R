library(tidyverse)
library(lubridate)
library(here)

ctd <- read_csv(here("QC", "raw_data", "ctd_event.csv")) %>% 
  filter(type == "station") %>% 
  select(eventID, eventDate)
chl <- read_csv(here("QC", "raw_data", "chl_event.csv")) %>% 
  filter(type == "station") %>% 
  select(eventID, eventDate)

test <- full_join(ctd, chl, by = "eventID") %>% 
  mutate(check = ifelse(eventDate.x == eventDate.y, "match", "nomatch"))

write.csv(test, here("QC","ctd_chl_eventDate_qc.csv"))
