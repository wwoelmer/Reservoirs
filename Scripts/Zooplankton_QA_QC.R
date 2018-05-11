# Preliminary QA/QC for zooplankton field data

pacman::p_load(tidyverse, hms, lubridate)

# load Jon's zoop data
raw_zoop <- read_csv('./Data/EDI_Zooplankton.csv')

# Set up for QA/QC
zoop <- raw_zoop %>% 
  
  # Eliminate rows with Density = 0
  filter(Density_IndPerL > 0) %>%
  
  # Parse columns to target format and number of digits
  mutate(DateTime = ymd_hms(DateTime),
         SampleTime = hms::as.hms(DateTime, tz = "UTC"),
         Density_IndPerL = round(Density_IndPerL, 2),
         MeanLength_mm = round(as.double(MeanLength_mm),3), 
         MeanWeight_ug = round(as.double(MeanWeight_ug),3),
         Biomass_ugL = round(as.double(Biomass_ugL), 3)) %>%
  
  # Add columns for Flags
  mutate(Flag_Density = ifelse(is.na(Density_IndPerL), 0,
                               ifelse(Density_IndPerL < 0, 1, 0)),
         Flag_Length = ifelse(is.na(MeanLength_mm), 0,
                              ifelse(MeanLength_mm < 0, 1, 0)),
         Flag_Weight = ifelse(is.na(MeanWeight_ug), 0, 
                              ifelse(MeanWeight_ug < 0, 1, 0)), 
         Flag_Biomass = ifelse(is.na(Biomass_ugL), 0,
                               ifelse(Biomass_ugL < 0, 1, 0))) %>%
  
  # Arrange column order for final sheet
  select(Reservoir:DateTime, SampleTime, StartDepth_m:Taxon, Density_IndPerL:Flag_Biomass)

# Write formatted data to csv
write.csv(zoop, './Formatted_Data/zooplankton.csv', row.names=F)

#### Zoop diagnostic plots ####
zoop_long <- zoop %>%
  select(-(Flag_Density:Flag_Biomass)) %>%
  gather(metric, value, Density_IndPerL:Biomass_ugL) %>% 
  mutate(year = as.factor(year(DateTime)), day = yday(DateTime))