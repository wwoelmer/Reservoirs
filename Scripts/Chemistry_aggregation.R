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
  mutate(Site = ifelse(Depth_m != "999", "50", "100")) %>%
 #mutate(Depth_m = recode(Depth_m, "999" = "100")) %>% ##!! recode inflow as "100"-- currently not working
  group_by(Reservoir, Site, DateTime, Notes) %>% # columns not to parse to double
  mutate_if(is.character,as.double) %>%  # parse all other columns to double
  select(Reservoir, Site, DateTime, Depth_m, TN_ugL, TP_ugL, infTPloads_g, # reorder columns by name
         NH4_ugL, NO3NO2_ugL, NO3_ugL, SRP_ugL, PO4_ugL, DOC_mgL, Notes) %>%
  arrange(DateTime, Reservoir) # %>%
#write_csv('./SUBFOLDER?/chemistry.csv')

##!! Flags from KF: are NO3_ugL values == NO3NO2_ugL or was SOP different? Ditto PO4_ugL == SRP_ugL? 
##!! Do we retain a "Notes" column in EDI version? If not, should hand-check these caveats before final version
##!! Some nutrient concentrations <0 but retained... should check! 

#### YSI Profiles ####
raw_profiles <- dir(path = "./Data", pattern = "*_Profiles.csv") %>% 
  map_df(~ read_csv(file.path(path = "./Data", .), col_types = cols(.default = "c")))
