#Title: Script for aggregating FP txt files for publication to EDI
#Author: Mary Lofton
#Date: 13DEC18

rm(list=ls())

# load packages
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

# Load in column names for .txt files 
col_names <- names(read_tsv("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/20180410_FCR_50.txt", n_max = 0))

# Load in txt files
## NEED TO FIGURE OUT HOW TO ADD FILENAME AS A COLUMN SO CAN SPLIT UP BY CAST
raw_fp <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/FP_txt", pattern = paste0("*.txt")) %>% 
  map_df(~ read_tsv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/FP_txt", .), 
                    col_types = cols(.default = "c"), col_names = col_names, skip = 2))

# Rename and select useful columns; drop metrics we don't like such as cell count;
# eliminate shallow depths because of quenching
fp <- raw_fp %>%
  mutate(DateTime = `Date/Time`, GreenAlgae_ugL = as.numeric(`Green Algae`), Bluegreens_ugL = as.numeric(`Bluegreen`),
         Browns_ugL = as.numeric(`Diatoms`), Mixed_ugL = as.numeric(`Cryptophyta`), YellowSubstances_ugL = as.numeric(`Yellow substances`),
         TotalConc_ugL = as.numeric(`Total conc.`), Transmission_perc = as.numeric(`Transmission`), Depth_m = `Depth`, Temp_C = as.numeric(`Temp. Sample`),
         RFU_525nm = as.numeric(`LED 3 [525 nm]`), RFU_570nm = as.numeric(`LED 4 [570 nm]`), RFU_610nm = as.numeric(`LED 5 [610 nm]`),
         RFU_370nm = as.numeric(`LED 6 [370 nm]`), RFU_590nm = as.numeric(`LED 7 [590 nm]`), RFU_470nm = as.numeric(`LED 8 [470 nm]`),
         Pressure_unit = as.numeric(`Pressure`)) %>%
  select(DateTime, GreenAlgae_ugL, Bluegreens_ugL, Browns_ugL, Mixed_ugL, YellowSubstances_ugL,
         TotalConc_ugL, Transmission_perc, Depth_m, Temp_C, RFU_525nm, RFU_570nm, RFU_610nm,
         RFU_370nm, RFU_590nm, RFU_470nm) %>%
  mutate(DateTime = as.POSIXct(as_datetime(DateTime, tz = "", format = "%m/%d/%Y %I:%M:%S %p"))) %>%
  filter(Depth_m >= 0.2, #pretty sure this is a reasonable cut-off for quenching
         Transmission_perc >= 70) # is this the right cut-off?
## NEED TO ADD RESERVOIR AND SITE COLUMNS ONCE HAVE FILENAMES READ IN AS COLUMN - EMAIL NICOLE?

# Add in 2017 data (different format)

# Plot by cast to catch irregularities/problems with casts (is upcast included? and so on)
  


  