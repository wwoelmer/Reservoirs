# Script to pull in FCR Inflow data from multiple years ####

#### QAQC Inflow
##Tasks
#1. Fix dates, aggregate all data
#2. Plot to check for outliers
#3. Identify missing data; end of 2017, before 2014
#metadata tasks:
#4. Figure out what the units are..
#5. Identify start and end times
#6. Should the raw data be included? as in inflow as pressure? Should we include stream dimensions/equations separately?
#7. Should reservoir column/site number still be in it, even though it is not relevant in this set?

#install.packages('pacman') #installs pacman package, making it easier to load in packages
pacman::p_load(tidyverse, lubridate, magrittr, ggplot2) #installs and loads in necessary packages for script
setwd("~/Reservoirs") #just in case your working directory is wonky

##Data from pressure transducer
# Load in files with names starting with FCR_inf_15min, should only be .csv files
raw_inflow <- dir(path = "./Data", pattern = "FCR_inf_15min*") %>% 
  map_df(~ read_csv(file.path(path = "./Data", .), col_types = cols(.default = "c"), skip = 37)) %>% 
raw_inf= raw_inflow[,c(6,3,4)] #limits data to necessary columns

###Inflow data must be paired with barometric data, this is an attempt to convert that.. maybe disregard
#Data from 2013, has differing format from other data;
#raw_inflow2 <- dir(path = "./Data", pattern = "FCR_inf2_15min*") %>% 
#  map_df(~ read_csv(file.path(path = "./Data", .), col_types = cols(.default = "c"), skip = 28)) %>% 
#  raw_inf2= raw_inflow2[,c(2:4)] #limits data to necessary columns

pressure <- raw_inf %>%
  # Rename columns if needed (TargetName = OriginalName)
  rename(Pressure_psia = "Pressure(in H2O)", DateTime = "Barometric Date/Time", Temp_C = "Temperature(degC)") %>%
  
#  pressure2 <- raw_inf2 %>%
  # Rename columns if needed (TargetName = OriginalName)
#  rename(Pressure_psia = "Pressure(in H2O)", DateTime = "Date/Time", Temp_C = "Temperature(degC)") %>%  

#pressure=bind_rows(pressure1, pressure2)
    
#consolidates 2 date formats into 1 format  
ymd_hms <- ymd_hms(pressure$DateTime) 
mdy_hm <- mdy_hm(pressure$DateTime) 
ymd_hms[is.na(ymd_hms)] <- mdy_hm[is.na(ymd_hms)] # some dates are ambiguous, here we give 
pressure$DateTime <- ymd_hms  
  
   # Parse columns to target format
  pressure <- pressure %>%
  mutate(DateTime = ymd_hms(DateTime)) %>%
  mutate_if(is.character,funs(as.double(.))) %>%   # parse all other columns to numeric
  
  
inflow <- pressure
  flow2 <- pressure$Pressure_psia
  
  ### CALCULATE THE FLOW RATES AT INFLOW ### #Taken from RPM's 'old school' script
  #################################################################################################
  flow3 <- (flow2 - 6.3125 + 1.2) * 0.0254                     # Response: Height above the weir (m), rating curve equation
  flow4 <- (0.62 * (2/3) * (1.1) * 4.43 * (flow3 ^ 1.5) * 35.3147) # Flow CFS
  flow5 <- flow4 * 60                                              # Flow CFM
  flow6 <- (flow2 - 6.3125 + 1.2) / 12                         # Height above weir (ft)
  flow7 <- 3.33 * (3.60892 - (0.2 * flow6)) * flow6 ^ 1.5          # Q eqn (cfs)
  flow8 <- flow7 * 28.317                                          #Flows in (Liters/Second); converting cubic ft to liters
  flow9 <- flow8 * (1 / 16.67)                                     #Flows in (Meters Cubed/Minute); converting liters to 
  flow10 <- flow9 / 60                                             #Flows in (Meters Cubed/Second)
#################################################################################################


inflow$Reservoir <- "FCR" #creates reservoir column to match other data sets
inflow$Site <- 100  #creates site column to match other data sets
  FLOWS <- cbind(inflow,flow10, deparse.level = 1) #binds relevant columns, copied from RPM's code, so idk what "deparse" is
  
  FLOWS <- FLOWS %>%
    # Rename columns if needed (TargetName = OriginalName)
    rename(Flow_cms = flow10)
  
Inflow_Final <- FLOWS[,c(4,5,1,3,2,6)]
Inflow_Final <- Inflow_Final[order(Inflow_Final$DateTime),] #orders file by date

# Write to CSV
write.csv(Inflow_Final, './Formatted_Data/inflow.csv', row.names=F) #this would not push to github?? did manual upload

#### MISCELLANEOUS TEST CODE #####
###Files that are broken
#101416

## Identifies Missing dates
testtime=Inflow_Final[,c(3,4)] #pulls out datetime and temp, bc I hate dealing with vectors rn sry

testtime2=testtime
testtime2$Date <- as.Date(testtime2$DateTime) #changes DateTime to Date only

#creates data frame of start and end of missing data
Inflow_stopdates=data_frame(start = testtime2$Date[ diff(testtime2$Date)>1],
                            end = testtime2$Date[-1] [ diff(testtime2$Date)>1]) 
#note: only checks within parameters of existing data, so if your missing head and tail data, you are out of luck

##Trying to compare to FCR hourly inflow data
#some of this code is garbage and doesn't work yet
#need 2013-06 to 2014-06; 2017-08 to 2017-12

daily_inflow <- dir(path = "./Data", pattern = "*Inflow.csv") %>% 
  map_df(~ read_csv(file.path(path = "./Data", .), col_types = cols(.default = "c"))) %>% 
   #limits data to necessary columns

daily_inflow <- daily_inflow %>%
  mutate(Date = ymd_hms(Date)) %>%
  mutate_if(is.character,funs(as.double(.))) %>%   # parse all other columns to numeric

  Date_daily= as.Date(daily_inflow$Date)   

Date_15min=unique(testtime2$Date)

# Get the dates in DateRange that are not in DF$V2 
Date_daily[!Date_daily %in% Date_15min]

daily_inf <- daily_inf[order(daily_inf$Date),] 
