# Script to pull in water chemistry data from multiple reservoirs and years ####

#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

#### In-lake and inflow water chemistry data ####
# Load .csv files whose file names end in _Chemistry
raw_chem <- dir(path = "./Data", pattern = "*_Chemistry.csv") %>% 
  map_df(~ read_csv(file.path(path = "./Data", .), col_types = cols(.default = "c")))

chemistry <- raw_chem %>%
  select(-(DIC_mgL:CH4_ppm), -(`OI TOC Conc (ppm)`:`Vario TNb Mean of 3 reps`),
         -(`Used OI DIC Conc (ppm)`:`Vario DC Mean of 3 reps`), -(`Vario DNb Mean of 3 reps`)) %>%
  rename(Depth_m = Depth, DateTime = Date, DOC_OIAnalytical_mgL = DOC_mgL) %>%
  mutate(DateTime = ymd_hms(DateTime)) %>%
  group_by(Reservoir, DateTime, Notes) %>% # columns not to parse to numeric
  mutate_if(is.character,funs(round(as.double(.), 2))) %>%  # parse all other columns to numeric
  mutate(NO3NO2_ugL = ifelse(is.na(NO3NO2_ugL), NO3_ugL, NO3NO2_ugL),  # move NO3_ugL values to NO3NO2 column
         SRP_ugL = ifelse(is.na(SRP_ugL), PO4_ugL, SRP_ugL), # move PO4 values to SRP column
         TP_ugL = ifelse((Reservoir == "FCR" & Year == 2013), NA, TP_ugL),
         DOC_OIAnalytical_mgL = ifelse((is.na(DOC_OIAnalytical_mgL) & Year <= 2016), `OI DOC Conc (ppm)`, DOC_OIAnalytical_mgL),
         DOC_VarioDOC_mgL = ifelse(Year >= 2016, `Vario DOC Mean of 3 reps`, NA)) %>% 
  mutate(Site = ifelse(Depth_m != 999, 50, 100)) %>% # Add site ID; 50 = deep hole; 100 = inflow
  mutate(Depth_m = replace(Depth_m, Depth_m == 999, 100)) %>% ##!! What should Depth_m be for inflow??
  select(Reservoir, Site, DateTime, Depth_m, TN_ugL, TP_ugL, infTPloads_g, # reorder columns by name
         NH4_ugL, NO3NO2_ugL, SRP_ugL, DOC_OIAnalytical_mgL, DOC_VarioDOC_mgL, Notes) %>%
  arrange(DateTime, Reservoir, Depth_m) 

# Write to CSV (using write.csv for now; want ISO format embedded?)
write.csv(chemistry, './Formatted_Data/chemistry.csv', row.names=F)

#### Chemistry diagnostic plots ####
chemistry_long <- chemistry %>% 
  select(-infTPloads_g) %>%
  gather(metric, value, TN_ugL:DOC_VarioDOC_mgL) %>% 
  mutate(year = year(DateTime))

# Plot range of values per constituent per year for each reservoir; 
  # annual mean value indicated with large black dot
ggplot(subset(chemistry_long, Site == 50), aes(x = year, y = value, col=Reservoir)) +
  geom_point(size=1) +
  stat_summary(fun.y="mean", geom="point",pch=21,  size=3, fill='black') +
  facet_grid(metric ~ Reservoir, scales= 'free_y') +
  scale_x_continuous("Date", breaks=seq(2013,2017,1)) +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position='none')

# FCR deep hole data time series
jet.colors <- c("#00007F", "#00007F", "blue", "blue", "#007FFF", "cyan", "#7FFF7F", "#7FFF7F",
                                 "yellow","yellow", "#FF7F00", "#FF7F00", "red", "#7F0000")

my_cols <- c('#a50026', '#d73027', '#f46d43', '#fdae61', '#fee090', '#ffffbf', '#ffffbf',
             '#e0f3f8','#e0f3f8', '#abd9e9', '#abd9e9', '#74add1','#4575b4', '#313695')

ggplot(subset(chemistry_long, Reservoir=='FCR' & Site=="50"), aes(x = DateTime, y = value, col=as.factor(Depth_m))) +
  geom_point(cex=2) +
  facet_grid(metric ~ ., scales='free') +
  scale_x_datetime("Date", date_breaks="2 months", date_labels = "%b %Y") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_manual("Depth (m)", values = jet.colors)

# Non-FCR reservoir deep hole data time series
ggplot(subset(chemistry_long, Reservoir!='FCR' & Site=="50"), aes(x = DateTime, y = value, fill=as.factor(Depth_m))) +
  geom_point(pch=21, col='black') +
  facet_grid(metric ~ Reservoir, scales='free') +
  scale_x_datetime("Date", date_breaks="4 months", date_labels = "%b %Y") +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_discrete(name="Depth (m)")

##!! Check reasonable precision of nutrient concentrations for reporting; KF rounded NO3 and PO4 to 2 decimal(?)
##!! Do we retain a "Notes" column in EDI version? If not, should hand-check these caveats before final version
##!! Some nutrient concentrations <0 but retained... should check! 