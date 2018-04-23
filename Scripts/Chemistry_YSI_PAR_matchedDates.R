# Check for missing dates between Chemistry and YSI files
pacman::p_load(tidyverse, lubridate)

chemistry <- read_csv('./Formatted_Data/chemistry.csv') %>%
  mutate(Chemistry = "Yes", 
         DateTime = as.Date(DateTime)) %>%
  select(Reservoir:DateTime, Chemistry)

YSI <- read_csv('./Formatted_Data/YSI_PAR_profiles.csv') %>%
  mutate(DateTime = as.Date(DateTime)) %>%
  select(-Depth_m)

all <- right_join(YSI, chemistry) %>% 
  group_by(Reservoir, DateTime, Chemistry) %>%
  distinct(Reservoir, DateTime, .keep_all= T) %>%
  select(Chemistry, Reservoir, DateTime:Notes) %>%
  arrange(Reservoir, DateTime) %>%
  write_csv('./Formatted_Data/Chemistry_YSI_PAR_matchedDates.csv')
