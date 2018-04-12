# Script to pull in water chemistry data from multiple reservoirs and years ####

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

# Load .csv files whose file names end in _Chemistry
raw <- dir(path = "./Data", pattern = "*_Chemistry.csv") %>% 
  map_df(~ read_csv(file.path(path = "./Data", .), col_types = cols(.default = "c"))) %>%
  select(-Year, -(DIC_mgL:CH4_ppm), -(`OI TOC Conc (ppm)`:`Vario DNb Mean of 3 reps`)) %>%
  rename(Depth_m = Depth, DateTime = Date) %>%
  mutate(DateTime = ymd_hms(DateTime), Site = as.factor(50), Chla_mgL = "", 
         Turbidity_NTU = "", Conductivity_uScm = "", PAR_umolm2s = "",
                ORP = "", pH = "") %>% group_by(Reservoir, Site, DateTime, Notes) %>%
  mutate_if(is.character,as.double) 

chemistry <- raw %>% 
  select(Reservoir, Site, DateTime, Depth_m, TN_ugL, TP_ugL, 
         NH4_ugL, NO3NO2_ugL, NO3_ugL, SRP_ugL, PO4_ugL, DOC_mgL, Chla_mgL, Turbidity_NTU, 
         Conductivity_uScm, PAR_umolm2s, ORP, pH, Notes) %>%
  arrange(DateTime, Reservoir) # %>%
#write_csv('./SUBFOLDER?/chemistry.csv')

##!! Flags from KF: are NO3_ugL values == NO3NO2_ugL or was SOP different? Ditto PO4_ugL == SRP_ugL? 
##!! Do we retain a "notes" column in EDI version? If not, should hand-check these caveats before final version
##!! Some nutrient concentrations <0 but retained... should check! 
