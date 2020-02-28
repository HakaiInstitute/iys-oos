library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(parsedate)
library(googledrive)

## Determine the nature of the CTD Files -----------------------------------------
drive_download("https://drive.google.com/open?id=1-onECiDW02DJRIX6g4dXywtzLEqCGyce", path = here("ctd", "raw_data", "Data_IYS_conbined_Final.xlsx"))

sheet1 <- read_excel(here("ctd", "raw_data", "Data_IYS_conbined_Final.xlsx"), sheet = "Sheet1")
sheet2 <- read_excel(here("ctd", "raw_data", "Data_IYS_conbined_Final.xlsx"), sheet = "Sheet2")

ctd_subset <- sheet1 %>% 
  mutate(ID = paste(`NO.Trawl`,`NO.(CTD)`,`DEPTH`,sep="_")) %>% 
  select(-c(1:3)) %>% 
  mutate_at(vars(-ID), as.numeric)

ctd_all <- sheet2 %>% 
  mutate(ID = paste(`NO.Trawl`,`NO.(CTD)`,`DEPTH`,sep="_")) %>% 
  select(-c(1:12)) %>% 
  filter(ID %in% ctd_subset$ID) %>%
  mutate_at(vars(-ID), as.numeric)

# ctd_match <- ctd_subset %>% 
#   left_join(ctd_all, by = ) %>% 
#   mutate(tem_match = ifelse(TEM_S_sub == TEM_S, "yes", "no"),
#          sal_match = ifelse(SAL_S_sub == SAL_S, "yes", "no")) %>% 
#   filter(is.na(tem_match) | tem_match == "no" | is.na(sal_match) | sal_match == "no")

# Test to see if Sheet 1 is just a subset of Sheet 2 with the same values.
match <- dplyr::setdiff(ctd_subset, ctd_all)


## Determine whether the CTD data in the Nutrients dataset (sheet Hydro_GoA) is a subset of the main CTD data --------------------------------------
ctd_all_ts <- sheet2 %>% 
  mutate(date = as.Date(ymd(paste(YEAR, MONTH, DAY, sep = "-"))),
         ID = paste(date, NO.Trawl, DEPTH, sep ="_")) %>% 
  select(ID, date, Station = NO.Trawl, DEPTH, TEM_S, SAL_S) %>% 
  mutate_at(vars(-ID, -date), as.numeric)

ctd_nut <- read_excel(here("nutrients", "raw_data", "IYS_GoA_2019_Hydro&Chemistry_20190326.xlsx"), sheet = "Hydro_GoA") %>% 
  mutate(ID = paste(date, Station, DEPTH, sep="_"),
         date = as.Date(date)) %>% 
  select(ID, date, Station, DEPTH, TEM_S_nut = `TEMP_SBE [degC]`, SAL_S_nut = `SAL_SBE [psu]`)

match <- inner_join(ctd_all_ts, ctd_nut, by = c("ID", "Station", "date", "DEPTH")) %>% 
  mutate(temp_match = TEM_S - TEM_S_nut,
         sal_match= SAL_S - SAL_S_nut) %>% 
  group_by(Station) %>% 
  # mutate(TEM_qc = case_when(TEM_S > lag(TEM_S, 1) ~ "greater", 
  #                           TEM_S == lag(TEM_S, 1) ~ "equal",
  #                           TEM_S < lag(TEM_S, 1) ~ "less")
  # ) %>% 
  filter(temp_match != 0 | sal_match != 0)


ctd_plot <- ctd_all_ts %>% 
  mutate(Station = as.factor(Station)) %>% 
  filter(Station %in% match$Station)


plot <- ggplot(ctd_plot) +
  geom_line(aes(x = DEPTH, y = TEM_S)) +
  facet_grid(~ Station)
print(plot)
