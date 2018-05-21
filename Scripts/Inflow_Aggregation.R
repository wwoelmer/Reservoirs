# Script to pull in FCR Inflow data from multiple years ####

####QAQC Inflow
##Tasks
#1. Compare to chemistry samples to see if dates line up, fix dates
#2. Plot to check for outliers
#3. Identify missing data; end of 2017, before 2014
#metadata tasks:
#4. Figure out what the units are..
#5. Identify start and end times
#6. Should the raw data be included? as in inflow as pressure? Should we include stream dimensions/equations separately?
#7. Should reservoir column still be in it, even though it is not relevant in this set?

#install.packages('pacman') #installs pacman package, making it easier to load in packages
pacman::p_load(tidyverse, lubridate, magrittr, ggplot2) #installs and loads in necessary packages for script
setwd("~/Reservoirs") #just in case your working directory is wonky

##Data from pressure transducer
# Load in files with names starting with FCR_inf_15min, should only be .csv files
raw_inflow <- dir(path = "./Data", pattern = "FCR_inf_15min*") %>% 
  map_df(~ read_csv(file.path(path = "./Data", .), col_types = cols(.default = "c"), skip = 37)) %>% 
raw_inf= raw_inflow[,c(6,3,4)] #limits data to necessary columns

pressure <- raw_inf %>%
  # Rename columns if needed (TargetName = OriginalName)
  rename(Pressure_unit = "Pressure(in H2O)", DateTime = "Barometric Date/Time", Temp_C = "Temperature(degC)") %>%

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
  flow2 <- pressure$Pressure_unit
  
  ### CALCULATE THE FLOW RATES AT INFLOW ### #Taken from RPM's 'old school' script
  #################################################################################################
  flow3 <- (flow2 - 6.3125 + 1.2) * 0.0254                     # Height above the weir (m)
  flow4 <- (0.62 * (2/3) * (1.1) * 4.43 * (flow3 ^ 1.5) * 35.3147) # Flow CFS
  flow5 <- flow4 * 60                                              # Flow CFM
  flow6 <- (flow2 - 6.3125 + 1.2) / 12                         # Height above weir (ft)
  flow7 <- 3.33 * (3.60892 - (0.2 * flow6)) * flow6 ^ 1.5          # Q eqn (cfs)
  flow8 <- flow7 * 28.317                                          #Flows in (Liters/Second)
  flow9 <- flow8 * (1 / 16.67)                                     #Flows in (Meters Cubed/Minute)
  flow10 <- flow9 / 60                                             #Flows in (Meters Cubed/Second)
#################################################################################################


#inflow$Reservoir <- "FCR" #creates reservoir column to match other data sets
#inflow$Site <- 100  
  FLOWS <- cbind(inflow,flow8,flow9,flow10, deparse.level = 1)
  
  FLOWS <- FLOWS %>%
    # Rename columns if needed (TargetName = OriginalName)
    rename(Flow_Ls = flow8, Flow_cmm = flow9, Flow_cms = flow10)
  
Inflow_Final <- FLOWS[,c(4,5,3,2,1,6:8)]

# Write to CSV
write.csv(Inflow_Final, './Formatted_Data/inflow.csv', row.names=F)


###Files that are broken
#101416


## Trying to check on stuff

for (i is 1 in nrow())
  for

x11()
ggplot(data = pressure)+
  geom_line(aes(DateTime,Pressure_unit))

testtime=inflow[c(1:30),c(3,4)]
testtime$diff=NA
for (i in 1:nrow(testtime)) {
diff[i]=difftime(testtime$DateTime[i],testtime$DateTime[i+1])
}