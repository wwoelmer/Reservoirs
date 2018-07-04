# Script to pull in FCR Inflow data from multiple years ####

#### QAQC Inflow
##Tasks
#1. Plot to check for outliers
#2. Fine tune QA/QC

#install.packages('pacman') #installs pacman package, making it easier to load in packages
pacman::p_load(tidyverse, lubridate, magrittr, ggplot2) #installs and loads in necessary packages for script
setwd("~/Reservoirs") #just in case your working directory is wonky
  #MEL: can you do this in RProjects? mine won't let me.....

##Data from pressure transducer
# Load in files with names starting with FCR_inf_15min, should only be .csv files
inflow_pressure <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV", pattern = "FCR_15min_Inf*") %>% 
  map_df(~ read_csv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV", .), col_types = cols(.default = "c"), skip = 28))
inflow_pressure = inflow_pressure[,-1] #limits data to necessary columns

##A wee bit o' data wrangling to get column names and formats in good shape
inflow_pressure <- inflow_pressure %>%
  rename(Date = `Date/Time`) %>%
  mutate(DateTime = parse_date_time(Date, 'dmy HMS',tz = "EST")) %>%
  select(-Date) %>%
  rename(Pressure_psi = `Pressure(psi)`,
         Temp_C = `Temperature(degC)`) %>%
  mutate(Pressure_psi = as.double(Pressure_psi),
         Temp_C = as.double(Temp_C))

##Preliminary visualization of raw pressure data from inflow transducer
plot_inflow <- inflow_pressure %>%
  mutate(Date = date(DateTime))

daily_flow <- group_by(plot_inflow, Date) %>% summarize(daily_pressure_avg = mean(Pressure_psi)) %>% mutate(Year = as.factor(year(Date)))

rawplot = ggplot(data = daily_flow, aes(x = Date, y = daily_pressure_avg))+
  geom_point()+
  ylab("Daily avg. inflow pressure (psi)")+
  theme_bw()
rawplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_inflow_pressure.png", rawplot, device = "png")

##based on this anything below 13.95 in 2017 for daily average looks like an outlier to me (when weir was leaking)
daily_flow[which(daily_flow$daily_pressure_avg <= 13.95),]
##10-24 to 10-30 but I thought it was down for longer than that?? may need to re-evaluate

pressure_hist = ggplot(data = daily_flow, aes(x = daily_pressure_avg, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. inflow pressure (psi)")+
  theme_bw()
pressure_hist
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_inflow_pressure_histogram.png", pressure_hist, device = "png")


pressure_boxplot = ggplot(data = daily_flow, aes(x = Year, y = daily_pressure_avg, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. inflow pressure (psi)")+
  theme_bw()
pressure_boxplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_inflow_pressure_boxplot.png", pressure_boxplot, device = "png")


##Read in catwalk pressure data
pressure <- read_csv("./Data/DataNotYetUploadedToEDI/FCR_DOsonde_2012to2017.csv")

##Data wrangling to get columns in correct format
pressure = pressure %>%
  select(Date, `Barometric Pressure Pressure (PSI)`) %>%
  mutate(DateTime = parse_date_time(Date, 'ymd HMS',tz = "EST")) %>%
  rename(Baro_pressure_psi = `Barometric Pressure Pressure (PSI)`) %>%
  select(-Date) %>%
  mutate(Baro_pressure_psi = as.double(Baro_pressure_psi))

##Combine inflow and catwalk barometric pressure
diff = left_join(inflow_pressure, pressure, by = "DateTime") %>%
  filter(!is.na(Baro_pressure_psi)) %>%
  mutate(Pressure_psia = Pressure_psi - Baro_pressure_psi)

plot_both <- diff %>%
  mutate(Date = date(DateTime)) 

daily_pressure <- group_by(plot_both, Date) %>% 
  summarize(daily_pressure_avg = mean(Pressure_psi),
            daily_baro_pressure_avg = mean(Baro_pressure_psi),
            daily_psia = mean(Pressure_psia)) %>%
  mutate(Year = as.factor(year(Date))) %>%
  gather('daily_pressure_avg','daily_baro_pressure_avg', 'daily_psia',
         key = 'pressure_type',value = 'psi') 

daily_pressure <- daily_pressure %>%
  mutate(pressure_type = ifelse(pressure_type == "daily_pressure_avg","inflow",ifelse(pressure_type == "daily_baro_pressure_avg","barometric","corrected")))

both_pressures = ggplot(data = daily_pressure, aes(x = Date, y = psi, group = pressure_type, colour = pressure_type))+
  geom_point()+
  theme_bw()
both_pressures

ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/all_pressure_types.png", both_pressures, device = "png")


# ###Inflow data must be paired with barometric data, this is an attempt to convert that.. maybe disregard
# 
# pressure <- raw_inf %>%
#   # Rename columns if needed (TargetName = OriginalName)
#   rename(Pressure_psia = "Pressure(in H2O)", DateTime = "Barometric Date/Time", Temp_C = "Temperature(degC)")
# 
# pressure=distinct(pressure) #removes duplicates
# 
#    # Parse columns to target format
#   pressure <- pressure %>%
#   mutate(DateTime = ymd_hms(DateTime)) %>%
#   mutate_if(is.character,funs(as.double(.)))   # parse all other columns to numeric
#   
#   
# inflow <- pressure

#create vector of corrected pressure to match nomenclature from RPM's script
flow2 <- diff$Pressure_psia 
  
  ### CALCULATE THE FLOW RATES AT INFLOW ### #Taken from RPM's 'old school' script, verified 05-23-18
  #################################################################################################
  flow3 <- (flow2 - 6.3125 + 1.2) * 0.0254  # Response: Height above the weir (m), rating curve equation
  flow4 <- (0.62 * (2/3) * (1.1) * 4.43 * (flow3 ^ 1.5) * 35.3147) # Flow CFS
  flow5 <- flow4 * 60                                              # Flow CFM
  flow6 <- (flow2 - 6.3125 + 1.2) / 12                         # Height above weir (ft)
  flow7 <- 3.33 * (3.60892 - (0.2 * flow6)) * flow6 ^ 1.5          # Q eqn (cfs)
  flow8 <- flow7 * 28.317                                          #Flows in (Liters/Second); converting cubic ft to liters
  flow9 <- flow8 * (1 / 16.67)                                     #Flows in (Meters Cubed/Minute); converting liters to 
  flow10 <- flow9 / 60                                             #Flows in (Meters Cubed/Second)
#################################################################################################


diff$Reservoir <- "FCR" #creates reservoir column to match other data sets
diff$Site <- 100  #creates site column to match other data sets
diff$Flow_cms <- flow10

##visualization of inflow
inflow = ggplot(diff, aes(x = DateTime, y = Flow_cms))

  # FLOWS <- bind_cols(diff,flow10) #binds relevant columns
  # FLOWS <- FLOWS %>%
  #   # Rename columns if needed (TargetName = OriginalName)
  #   rename(Flow_cms = flow10)
  
Inflow_Final <- FLOWS[,c(4,5,1,3,2,6)] #orders columns
Inflow_Final <- Inflow_Final[order(Inflow_Final$DateTime),] #orders file by date
#Inflow_Final <- Inflow_Final[-c(1,which(Inflow_Final$DateTime>"2017-12-31 23:45:00")),] #limits data to before 2017 and takes out first row erroneous value
Inflow_Final <- Inflow_Final[-which(Inflow_Final$DateTime>"2017-12-31 23:45:00"),]

# Write to CSV
write.csv(Inflow_Final, './Formatted_Data/MakeEMLInflow/inflow.csv', row.names=F) #this would not push to github?? did manual upload


####### MISCELLANEOUS TEST CODE #########

## Identifies Missing dates
testtime=Inflow_Final[,c(2,3)] #pulls out datetime and temp, bc I hate dealing with vectors rn sry

testtime2=testtime
testtime2$Date=testtime2$DateTime
testtime2$Date <- as_date(testtime2$DateTime) #changes DateTime to Date only
testtime2=testtime2[order(testtime2$DateTime),]
#creates data frame of start and end of missing data by day
Test_stopdates=data_frame(start = testtime2$Date[ diff(testtime2$Date)>1],
                            end = testtime2$Date[-1] [ diff(testtime2$Date)>1])
#creates data frame of start and end of missing data, 15 min interval
Test_stop15=data_frame(start15 = testtime2$DateTime[ diff(testtime2$DateTime)>900], end = testtime2$DateTime[-1] [ diff(testtime2$DateTime)>900])
#note: only checks within parameters of existing data, so if your missing head and tail data, you are out of luck

#write.csv(Test_stop15[c(73:88),], './Formatted_Data/MakeEMLInflow/Missing15minIntervals_Inflow.csv', row.names = F) #created data set of collection dates

head(Inflow_Final)
tail(Inflow_Final)
