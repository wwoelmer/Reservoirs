# Script to pull in Secchi data from multiple reservoirs and years ####
 
#install.packages('pacman') ## Run this line if you don't have "pacman" package installed
pacman::p_load(tidyverse, lubridate) ## Use pacman package to install/load other packages

#### Secchi depths ####

# Load all data files ending in "_Secchi.csv" and merge into a dataframe
raw_secchi <- dir(path = "./Data", pattern = "*_Secchi.csv") %>% 
  map_df(~ read_csv(file.path(path = "./Data", .)))

secchi <- raw_secchi %>%
  # Rename columns if needed (TargetName = OriginalName)
  rename(DateTime = Date) %>%
  
  # Parse columns to target format
  mutate(DateTime = ymd_hms(DateTime)) %>% ## Force DateTime to be a yyyy-mm-dd hh:mm:ss format
  
  # Omit rows where all Secchi values NA (e.g., rows from files with trailing ,'s)
  filter(!is.na(Secchi_m) | !is.na(Secchi)) %>%
  
  # Move values from duplicated column names into target column
  mutate(Secchi_m = round(ifelse(is.na(Secchi_m), Secchi, Secchi_m),2),  
         Notes = ifelse(is.na(Notes), Comments, Notes),
         Site = 50) %>%  # Add Site ID; 50 = deep hole/dam
  
  # Add 'flag' columns for each variable; 1 = flag 
  mutate(Flag_Secchi = ifelse((Secchi_m < 0.5), 1, 0)) %>%
  
  # Arrange order of columns for final data table
  select(Reservoir, Site, DateTime, Secchi_m, Flag_Secchi, Notes) %>%
  arrange(Reservoir, DateTime) 

# Write to CSV (using write.csv for now; want ISO format embedded?)
write.csv(secchi, './Formatted_Data/Secchi_depth.csv', row.names=F)
  
#### YSI diagnostic plots ####
secchi_long <- secchi %>%
  select(-(Flag_Secchi:Notes)) %>%
  mutate(year = as.factor(year(DateTime)), day = yday(DateTime))

# Plot range of values per year for each reservoir; 
# annual mean value indicated with large black dot
ggplot(secchi_long, aes(x = year, y = Secchi_m, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(. ~ Reservoir, scales= 'free_y') +
  scale_x_discrete("Date", breaks=seq(2013,2017,1)) +
  scale_y_continuous("Secchi depth (m)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')

# Time series for each reservoir by julian day
ggplot(secchi_long, aes(x = day, y = Secchi_m, col=year)) +
  geom_point(size=2) + 
  facet_grid(Reservoir ~ ., scales= 'free_y') +
  scale_x_continuous("Julian day", limits=c(10,315), breaks=seq(50,300,50))+
  scale_y_continuous("Secchi depth (m)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='bottom')
