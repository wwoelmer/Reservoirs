# Script to pull in YSI and PAR data from multiple reservoirs and years ####
 
#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate) ## Use pacman package to install/load other packages

#### YSI Profiles ####

# Load all data files ending in "_Profiles.csv" and merge into a dataframe
raw_profiles <- dir(path = "./Data", pattern = "*_Profiles.csv") %>% 
  map_df(~ read_csv(file.path(path = "./Data", .), col_types = cols(.default = "c")))

names(raw_profiles) ## Print names of all the columns

profiles <- raw_profiles %>%
  ## Rename columns if needed (TargetName = OriginalName)
  rename(Depth_m = Depth, DateTime = Date, PAR_umolm2s = PAR, Temp_C = YSI_TEMP_C, 
         DO_mgL = YSI_DO_mgL, Cond_uScm = YSI_COND_uScm, DOSat = YSI_PSAT,
         ORP = YSI_ORP_mV) %>%
  filter(Depth_m != "888" & Depth_m != "777") %>%
  mutate(DateTime = ymd_hms(DateTime)) %>% ## Force DateTime to be a yyyy-mm-dd hh:mm format
  group_by(Reservoir, DateTime, Notes) %>% # columns not to parse to numeric
  mutate_if(is.character,funs(round(as.double(.), 2))) %>%  # parse all other columns to numeric
  ## Move values from duplicated column names into target column
  mutate(Cond_uScm = ifelse(is.na(Cond_uScm), YSI_COND, Cond_uScm),  # move YSI_COND values to Cond_uScm column
         DO_mgL = ifelse(is.na(DO_mgL), YSI_DO, DO_mgL), # move YSI_DO values to DO_mgL column
         PAR_umolm2s = ifelse(is.na(PAR_umolm2s), YSI_PAR, PAR_umolm2s), # move YSI_PAR values to PAR_umolm2s column
         pH = ifelse(is.na(pH), YSI_pH, pH), # move YSI_pH values to pH
         DOSat = ifelse(is.na(DOSat), YSI_DOSAT, DOSat), # move YSI_DOSAT values to DOSat
         Temp_C = ifelse(is.na(Temp_C), YSI_TEMP, Temp_C)) %>% # move YSI_TEMP values to Temp_C
  mutate(Site = ifelse(Depth_m == 999, 100, 50)) %>% ## Add a Site column; 50 = deep hole/dam; 100 = inflow
 # mutate(Depth_m = replace(Depth_m, Depth_m == 999, 100)) %>% ##!! What should Depth_m be for inflow??
  select(Reservoir, Site, DateTime, Depth_m, Temp_C, DO_mgL, DOSat, ## Select order of columns for final data table
         Cond_uScm, PAR_umolm2s, ORP, pH, Notes) %>%
  arrange(DateTime, Reservoir, Depth_m) ## Sort data

# Write to CSV (using write.csv for now; want ISO format embedded?)
write.csv(profiles, './Formatted_Data/YSI_PAR_profiles.csv', row.names=F)
  
#### YSI diagnostic plots ####
profiles_long <- profiles %>%
  gather(metric, value, Temp_C:pH) %>% 
  mutate(year = year(DateTime))

# Plot range of values per year for each reservoir; 
# annual mean value indicated with large black dot
ggplot(subset(profiles_long, Site == 50), aes(x = year, y = value, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(metric ~ Reservoir, scales= 'free_y') +
  scale_x_continuous("Date", breaks=seq(2013,2017,1)) +
  scale_y_continuous("") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')
