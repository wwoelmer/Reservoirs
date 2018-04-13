# Script to pull in water chemistry data from multiple reservoirs and years ####

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

#### In-lake and inflow water chemistry data ####
# Load .csv files whose file names end in _Chemistry
raw_chem <- dir(path = "./Data", pattern = "*_Chemistry.csv") %>% 
  map_df(~ read_csv(file.path(path = "./Data", .), col_types = cols(.default = "c")))

chemistry <- raw_chem %>%
  select(-Year, -(DIC_mgL:CH4_ppm), -(`OI TOC Conc (ppm)`:`Vario TNb Mean of 3 reps`),
         -(`OI DOC Conc (ppm)`:`Vario DNb Mean of 3 reps`)) %>%
  rename(Depth_m = Depth, DateTime = Date) %>%
  mutate(DateTime = ymd_hms(DateTime)) %>%
  group_by(Reservoir, DateTime, Notes) %>% # columns not to parse to numeric
  mutate_if(is.character,funs(round(as.double(.), 2))) %>%  # parse all other columns to numeric
  mutate(NO3NO2_ugL = ifelse(is.na(NO3NO2_ugL), NO3_ugL, NO3NO2_ugL),  # move NO3_ugL values to NO3NO2 column
         SRP_ugL = ifelse(is.na(SRP_ugL), PO4_ugL, SRP_ugL)) %>% # move PO4 values to SRP column
  mutate(Site = ifelse(Depth_m != 999, 50, 100)) %>% # Add site ID; 50 = deep hole; 100 = inflow
  mutate(Depth_m = replace(Depth_m, Depth_m == 999, 100)) %>% ##!! What should Depth_m be for inflow??
  select(Reservoir, Site, DateTime, Depth_m, TN_ugL, TP_ugL, infTPloads_g, # reorder columns by name
         NH4_ugL, NO3NO2_ugL, SRP_ugL, DOC_mgL, Notes) %>%
  arrange(DateTime, Reservoir, Depth_m) 

# Write to CSV (using write.csv for now; want ISO format embedded?)
write.csv(chemistry, './Formatted_Data/chemistry.csv', row.names=F)

##!! Check reasonable precision of nutrient concentrations for reporting; KF rounded NO3 and PO4 to 2 decimal(?)
##!! Do we retain a "Notes" column in EDI version? If not, should hand-check these caveats before final version
##!! Some nutrient concentrations <0 but retained... should check! 

#### YSI Profiles ####
raw_profiles <- dir(path = "./Data", pattern = "*_Profiles.csv") %>% 
  map_df(~ read_csv(file.path(path = "./Data", .), col_types = cols(.default = "c")))
