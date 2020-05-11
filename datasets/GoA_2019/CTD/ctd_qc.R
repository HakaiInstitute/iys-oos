# ---
#  title: CTD measurements 2019 Gulf of Alaska expedition
#  author: Julian Gan & Tim van der Stap, Hakai Institute
#  date: "`r format(Sys.Date())`"
#  objective: Data wrangling of CTD data collected on the GoA 2019 cruise, according
#  to Darwin Core standards. 
# ---

# Load required packages:
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(parsedate)
library(googledrive)

## Determine the nature of the CTD Files -----------------------------------------
drive_download("https://drive.google.com/open?id=1-onECiDW02DJRIX6g4dXywtzLEqCGyce", 
               path = here("./datasets/GoA_2019/CTD/raw_data/", "Data_IYS_conbined_Final.xlsx"))

sheet1 <- read_excel(here("./datasets/GoA_2019/CTD/raw_data/", "Data_IYS_conbined_Final.xlsx"), sheet = "Sheet1")
sheet2 <- read_excel(here("./datasets/GoA_2019/CTD/raw_data/", "Data_IYS_conbined_Final.xlsx"), sheet = "Sheet2")

# Create Event Core table from the CTD:
ctd_event <- sheet2 %>%
  select(`NO.Trawl`, `NO.(CTD)`, `NO.(ST)`, DEPTH, 
         LON, LAT, YEAR, MONTH, DAY, `TIME(ship time)`, `MIN(ship time)`) %>%
  mutate(eventDate_text = str_c(YEAR, MONTH, DAY, `TIME(ship time)`, `MIN(ship time)`, sep="-"),
         eventDate = ymd_hm(eventDate_text, tz = "Asia/Kamchatka")) %>%
  mutate(eventDate = format_iso_8601(as.POSIXct(eventDate,
                                                format = "%Y-%m-%d %H:%M%:S",
                                                tz="Asia/Kamchatka",
                                                usetz = TRUE)))

# Convert the standalone time value (UTC+12) into ISO8601 extended format
# To do: make sure timezone is correct

ctd_event <- dplyr::rename(ctd_event, decimalLongitude = LON)
ctd_event <- dplyr::rename(ctd_event, decimalLatitude = LAT)
ctd_event$basisOfRecord <- "MachineObservation"
ctd_event$Cruise <- "GoA2019"
ctd_event$minimumDepthInMeters <- sheet2$DEPTH
ctd_event$maximumDepthInMeters <- sheet2$DEPTH
ctd_event$bottomDepthInMeters <- sheet2$`Bot. Depth`

## Assumption: -------------------------------------------------------------------
# The assumption that we make here for the eventID is that NO.Trawl is the Station 
# number(!!).However, if that's the case, what is the column NO.(ST) in reference to? 
# Will need confirmation/clarification.
## -------------------------------------------------------------------------------

ctd_event <- ctd_event %>%
  mutate(parentEventID = paste("Station",`NO.Trawl`,"CTD",`NO.(CTD)`, sep="_"),
         eventID = paste(parentEventID, "D", DEPTH, sep="_"))
ctd_event_2 <- ctd_event %>%
  select(eventID, parentEventID, eventDate, decimalLongitude, decimalLatitude,
         minimumDepthInMeters, maximumDepthInMeters, bottomDepthInMeters, basisOfRecord)

## Next, in our dataframe we're going to add layers of EventIDs, based on the unique
# parentEventIDs of the row below:
ctd_event_2 <- ctd_event_2 %>% group_by(parentEventID, bottomDepthInMeters) %>% 
  do(add_row(., .before = 0))

for (row in 1:length(ctd_event_2$eventID)) {
  if(is.na(ctd_event_2$eventID[row]) == TRUE) {
    ctd_event_2$eventID[row] = ctd_event_2$parentEventID[row+1]
    ctd_event_2$bottomDepthInMeters[row] = ctd_event_2$bottomDepthInMeters[row+1]
  }
}

# parentEventIDs in the new created row are currently labeled NA. Given that at a 
# later stage we are going to be adding another layer of EventIDs, we need to fill
# in these NAs right now as well:
for (row in 1:length(ctd_event_2$parentEventID)) {
  if(is.na(ctd_event_2$parentEventID[row]) == TRUE) {
    ctd_event_2$parentEventID[row] = ctd_event_2$eventID[row]
  }
}

# In the previous `block of code`, we made parentEventIDs similar to eventIDs
# but we have to change this, to add another layer of eventID based on unique
# values in the parentEventID column:
ctd_event_new <- ctd_event_2 %>% ungroup(parentEventID, bottomDepthInMeters) %>% 
  mutate(parentEventID = ifelse(parentEventID == eventID,
   (parentEventID <- str_replace(parentEventID, "\\_CTD_.*", "")),
   (parentEventID <- ctd_event_2$parentEventID)))

## Tip for future use -----------------------------------------------------------
# The "\\_CTD_.*" is referred to as a 'wildcard', using the .* at the end of the 
# string means that regardless of the number following the string (i.e. "_CTD_42")
# it'll be replaced by, in this case, ""). This method can be used when there are
# multiple/different strings that are a combination of words/numbers that you'd
# like to replace. 
## ------------------------------------------------------------------------------

# Finally, we need to add a new layer above all parentEventIDs that do not
# contain `CTD` (only the Station #), and then merge this back to the main 
# data frame. 
subset <- ctd_event_new[!grepl("CTD", ctd_event_new$parentEventID),]
subset <- subset %>% group_by(parentEventID, bottomDepthInMeters) %>% 
  do(add_row(., .before = 0))

for (row in 1:length(subset$eventID)) {
  if(is.na(subset$eventID[row]) == TRUE) {
    subset$eventID[row] = subset$parentEventID[row+1]
  }
}

# Merge the subset to the new event dataframe, sort by eventID, and make sure that 
# eventDate and decimal coordinates get associated with the correct eventID layer:
full_ctd <- full_join(ctd_event_new, subset)
order <- stringr::str_sort(full_ctd$eventID, numeric=TRUE)
final_ctd_event <- full_ctd[match(order, full_ctd$eventID),]

for (row in 1:length(final_ctd_event$parentEventID)) {
  if(is.na(final_ctd_event$parentEventID[row] == TRUE)) {
    final_ctd_event$eventDate[row] = final_ctd_event$eventDate[row+2]
    final_ctd_event$decimalLatitude[row] = final_ctd_event$decimalLatitude[row+2]
    final_ctd_event$decimalLongitude[row] = final_ctd_event$decimalLongitude[row+2]
  }
}

# As eventDate, decimalLatitude and decimalLongitude are duplicated throughout the eventIDs,
# these are removed: 
final_ctd_event[duplicated(final_ctd_event[, c('eventDate', 'decimalLatitude', 'decimalLongitude', 'bottomDepthInMeters')]), 
   c('eventDate', 'decimalLatitude', 'decimalLongitude', 'bottomDepthInMeters')] <- NA

# For reasons still unknown to me, one duplicate of these columns still remained throughout
# each Station, so one final tidying up of the dataframe: 
final_ctd_event$eventDate[final_ctd_event$minimumDepthInMeters==1] <- NA
final_ctd_event$decimalLatitude[final_ctd_event$minimumDepthInMeters==1] <- NA
final_ctd_event$decimalLongitude[final_ctd_event$minimumDepthInMeters==1] <- NA
final_ctd_event$bottomDepthInMeters[final_ctd_event$minimumDepthInMeters==1] <- NA

final_ctd_event[is.na(final_ctd_event)] <- ''

# Save locally and in GoogleDrive
write_csv(final_ctd_event, here("./datasets/GoA_2019/CTD/raw_data/", "CTD_event.csv"))
drive_upload("./datasets/GoA_2019/CTD/raw_data/CTD_event.csv",
             path = "https://drive.google.com/drive/u/0/folders/1-XXOPhMN4-BmhI3owM2hvaMKXEy6hEYL",
             name = "draft_CTD_event.csv",
             overwrite = TRUE)

## Measurement or Fact table ----------------------------------------------------
# Create eMoF table, first from sheet1 (data collected only at specific depths),
# as this computes faster (smaller dataset) 

# First, EC25 is measured in microSiemens/cm, which is not (yet) in NERCs controlled 
# vocabulary. Therefore, we convert this column to milliSiemens/cm:
sheet1 <- sheet1 %>% mutate(`EC25 [mS/cm]_R` = as.numeric(`EC25 [uS/cm]_R`) / 1000)

ctd_measurement <- sheet1 %>% 
  # Select all the relevant measured parameters for data wrangling: 
  select(c(NO.Trawl, `NO.(CTD)`, `DEPTH`, `TEM_S`, `SAL_S`, 
           `TEM_R`, `TEM_S`,`SAL_R`, `SAL_S`,`Cond. [mS/cm]_R`, `pH`, `O2 [ml/l]`, `EC25 [mS/cm]_R`, 
           `Density [kg/m^3]_R`, `SigmaT [ ]_R`,`Chl-Flu. [ppb]_R`, `Chl-a [ug/l]_R`, `Turb-M [FTU]_R`, 
           `DO [mg/l]_R`, `Batt. [V]_R`,`BOD5[ml/l]`, `DO [%]_R`)) 

ctd_measurement <- ctd_measurement %>%
  pivot_longer(c(`TEM_R`, `SAL_R`, `TEM_S`, `SAL_S`, `Cond. [mS/cm]_R`, `pH`, `O2 [ml/l]`, `EC25 [mS/cm]_R`, 
                 `Density [kg/m^3]_R`, `SigmaT [ ]_R`,`Chl-Flu. [ppb]_R`, `Chl-a [ug/l]_R`, `Turb-M [FTU]_R`, 
                 `DO [mg/l]_R`, `Batt. [V]_R`,`BOD5[ml/l]`, `DO [%]_R`), 
               names_to = "measurementType", 
               values_to = "measurementValue",
               values_ptypes = list(measurementValue = 'character'))

ctd_measurement <- ctd_measurement %>% # Should "NO.Trawl" here be "NO.(ST)"?
  mutate(measurementID = case_when(measurementType == "TEM_S" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "TEM_S",sep="_"),
                                   measurementType == "TEM_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "TEM_R",sep="_"),
                                   measurementType == "SAL_S" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "SAL_S",sep="_"),
                                   measurementType == "SAL_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "SAL_R",sep="_"),
                                   measurementType == "Cond. [mS/cm]_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "Cond. [mS/cm]_R", sep="_"),
                                   measurementType == "Density [kg/m^3]_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH,"Density", sep="_"),
                                   measurementType == "SigmaT [ ]_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "Sigma",sep="_"),
                                   measurementType == "Chl-Flu. [ppb]_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "Chl-Flu", sep="_"),
                                   measurementType == "Chl-a [ug/l]_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "Chl-a", sep="_"),
                                   measurementType == "Turb-M [FTU]_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "Turbidity",sep="_"),
                                   measurementType == "DO [mg/l]_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "DO",sep="_"),
                                   measurementType == "Batt. [V]_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "Batt", sep="_"),
                                   measurementType == "pH" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH, "pH",sep="_"),
                                   measurementType == "O2 [ml/l]" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH,"O2 [ml/l]",sep="_"),
                                   measurementType == "BOD5[ml/l]" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH,"BOD5[ml/l]",sep="_"),
                                   measurementType == "DO [%]_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH,"BOD5[ml/l]",sep="_"),
                                   measurementType == "EC25 [mS/cm]_R" ~ paste("NO.Trawl",NO.Trawl,`NO.(CTD)`,"D",DEPTH,"EC25 [mS/cm]_R",sep="_")),
         
         measurementType = recode(measurementType, 
                                  `TEM_S` = "Sea Water Temperature [Sea-Bird]",
                                  `TEM_R` = "Sea Water Temperature [Rinko]",
                                  `SAL_R` = "Sea Water Practical Salinity [Rinko]",
                                  `SAL_S` = "Sea Water Practical Salinity [Sea-Bird]",
                                  `pH` = "pH",
                                  `O2 [ml/l]` = "Dissolved oxygen [Lab]",
                                  `Cond. [mS/cm]_R` = "Sea Water Electrical Conductivity",
                                  `Density [kg/m^3]_R` = "Sea Water Density",
                                  `SigmaT [ ]_R` = "Sea Water Sigma T",
                                  `Chl-Flu. [ppb]_R` = "Chlorophyll - Flu",
                                  `Chl-a [ug/l]_R` = "Chlorophyll-a",
                                  `Turb-M [FTU]_R` = "Turbidity",
                                  `DO [mg/l]_R` = "Dissolved oxygen",
                                  `Batt. [V]_R` = "Battery voltage",
                                  `BOD5[ml/l]` = "BOD in 5m depth [Lab]",
                                  `DO [%]_R` = "Percent oxygen saturation",
                                  `EC25 [mS/cm]_R` = "EC25"),
         
         measurementTypeID = recode(measurementType, #URIs from NERC website
                                    "Sea Water Temperature [Sea-Bird]" = "http://vocab.nerc.ac.uk/collection/P02/current/TEMP/2/",
                                    "Sea Water Temperature [Rinko]" = "http://vocab.nerc.ac.uk/collection/P02/current/TEMP/2/",
                                    "Sea Water Practical Salinity [Rinko]" = "http://vocab.nerc.ac.uk/collection/P02/current/PSAL/",
                                    "Sea Water Practical Salinity [Sea-Bird]" = "http://vocab.nerc.ac.uk/collection/P02/current/PSAL/",
                                    "Sea Water Electrical Conductivity" = "http://vocab.nerc.ac.uk/collection/P02/current/CNDC/",
                                    "Sea Water Density" = "http://vocab.nerc.ac.uk/collection/P04/current/G990/",
                                    "Sea Water Sigma T" = "http://vocab.nerc.ac.uk/collection/OG1/current/SIGTHETA/",     
                                    "Chlorophyll - Flu" = " ",
                                    "Chlorophyll-a" = "http://vocab.nerc.ac.uk/collection/P02/current/CPWC/",
                                    "Turbidity" = "http://vocab.nerc.ac.uk/collection/P25/current/TURB/",
                                    "Dissolved oxygen" = "http://vocab.nerc.ac.uk/collection/P35/current/EPC00002/",
                                    "Battery voltage" = "http://vocab.nerc.ac.uk/collection/S06/current/S0600163/",
                                    "pH" = "http://vocab.nerc.ac.uk/collection/P01/current/PHXXZZXX/",
                                    "Dissolved oxygen [Lab]" = "http://vocab.nerc.ac.uk/collection/P01/current/DOXYZZ01/",
                                    "BOD in 5m depth [Lab]" = "http://vocab.nerc.ac.uk/collection/P01/current/BODZZZZZ/1/",
                                    "Percent oxygen saturation" = "http://vocab.nerc.ac.uk/collection/P02/current/DOXY/",
                                    "EC25" = "http://vocab.nerc.ac.uk/collection/P02/current/CNDC/"),
         
         measurementUnit = case_when(measurementType == "Sea Water Temperature [Sea-Bird]" ~ "deg C",
                                     measurementType == "Sea Water Temperature [Rinko]" ~ "deg C",
                                     measurementType == "Sea Water Practical Salinity [Rinko]" ~ " ",
                                     measurementType == "Sea Water Practical Salinity [Sea-Bird]" ~ " ",
                                     measurementType == "pH" ~ " ",
                                     measurementType == "Sea Water Electrical Conductivity" ~ "mS/cm",
                                     measurementType == "Sea Water Density" ~ "kg m^-3 ",
                                     measurementType == "Sea Water Sigma T" ~ "kg m^-3",
                                     measurementType == "Chlorophyll - Flu" ~ "ppb",
                                     measurementType == "Chlorophyll-a" ~ "ug/L",
                                     # Alternatively, chlorophyll-a concentration can be reported in mg.m-3. Conversion = 1:1.
                                     measurementType == "Turbidity" ~ "FTU",
                                     measurementType == "Battery voltage" ~ "V",
                                     measurementType == "Dissolved oxygen" ~ "mg/L",
                                     measurementType == "Dissolved oxygen [Lab]" ~ "ml/L ",
                                     measurementType == "BOD in 5m depth [Lab]" ~ "ml/L",
                                     measurementType == "Percent oxygen saturation" ~ "percent saturation",
                                     measurementType == "EC25" ~ "mS/cm"),
         
         measurementUnitID = case_when(measurementType == "Sea Water Temperature [Sea-Bird]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPAA/",
                                       measurementType == "Sea Water Temperature [Rinko]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPAA/",
                                       measurementType == "Sea Water Practical Salinity [Rinko]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/PPTR/",
                                       measurementType == "Sea Water Practical Salinity [Sea-Bird]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/PPTR/",
                                       measurementType == "Sea Water Electrical Conductivity" ~ "http://vocab.nerc.ac.uk/collection/P06/current/MSCM/",
                                       measurementType == "Sea Water Density" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UKMC/",
                                       measurementType == "Sea Water Sigma T" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UKMC/",
                                       measurementType == "Chlorophyll - Flu" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPPB/",
                                       measurementType == "Chlorophyll-a" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UGPL/",
                                       measurementType == "Turbidity" ~ "http://vocab.nerc.ac.uk/collection/P06/current/USTU/",
                                       measurementType == "Dissolved oxygen" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMGL/",
                                       measurementType == "Battery voltage" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UVLT/",
                                       measurementType == "pH" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UUPH/",
                                       measurementType == "Dissolved oxygen [Lab]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMLL/",
                                       measurementType == "BOD in 5m depth [Lab]" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMLL/",
                                       measurementType == "Percent oxygen saturation" ~ "http://vocab.nerc.ac.uk/collection/P01/current/OXYSZZ01/",
                                       measurementType == "EC25" ~ "http://vocab.nerc.ac.uk/collection/P06/current/MSCM/"),
         occurrenceID = " "
  ) %>%
  select(measurementID, measurementType, measurementTypeID,
         measurementValue, measurementUnit, measurementUnitID)

# Save locally and on GoogleDrive: 
write_csv(ctd_measurement, here("./datasets/GoA_2019/CTD/raw_data/", "CTD_measurement.csv"))
drive_upload("./datasets/GoA_2019/CTD/raw_data/CTD_measurement.csv",
             path = "https://drive.google.com/drive/u/0/folders/1-XXOPhMN4-BmhI3owM2hvaMKXEy6hEYL",
             name = "draft_CTD_measurement.csv",
             overwrite = TRUE)

# Comments/things to consider ----------------------------------------------------------------------- 
# 1. Salinity can - alternatively - be measured in ppt. "For all practical purposes, salinity in PSS
# or PSU (which is dimensionless) has the same numerical value as salinity in ppt." 
# 2. Have to check whether the measurementUnit standard for turbidity is NTU or FTU.  
# 3. Have to determine whether measurementUnit for Chl-a is mg m-3 or ug/L [technically the same].
# 4. Should BOD5 be measured in mg/L instead of ml/L?
# ---------------------------------------------------------------------------------------------------

#### Unsure if code below is still required? ### 

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
