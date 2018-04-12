# Script to pull in water chemistry data from multiple reservoirs and years ####
# Collate into one file for EDI 

#### Load packages, read in data files ####
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

# Load .csv files whose file names end in _Chemistry
## Need to mutate columns to correct parsing; differences between files prevented
## correct auto-parsing
raw <- dir(path = "./Data", pattern = "*_Chemistry.csv") %>% #
  map_df(~ read_csv(file.path(path = "./Data", .), col_types = cols(.default = "c"))) %>%
  mutate(Year = as.integer(Year))
