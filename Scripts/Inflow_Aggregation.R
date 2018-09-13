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

##based on this anything below 13.95 in 2017 for daily average looks like an outlier to me (when weir was leaking)
daily_flow[which(daily_flow$daily_pressure_avg <= 13.95),]
##10-24 to 10-30 but I thought it was down for longer than that?? may need to re-evaluate

##Read in catwalk pressure data
pressure <- read_csv("./Data/DataNotYetUploadedToEDI/FCR_DOsonde_2012to2017.csv")
pressure_a4d <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV", pattern = "FCR_BV*") %>% 
  map_df(~ read_csv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV", .), col_types = cols(.default = "c"), skip = 28))
pressure_a4d = pressure_a4d[,-c(1,3)]

##Data wrangling to get columns in correct format and combine data from senvu.net and Aqua4Plus
pressure = pressure %>%
  select(Date, `Barometric Pressure Pressure (PSI)`) %>%
  mutate(DateTime = parse_date_time(Date, 'ymd HMS',tz = "EST")) %>%
  rename(Baro_pressure_psi = `Barometric Pressure Pressure (PSI)`) %>%
  select(-Date) %>%
  mutate(Baro_pressure_psi = as.double(Baro_pressure_psi))

pressure_a4d <- pressure_a4d %>%
  rename(Date = `Date/Time`) %>%
  mutate(DateTime = parse_date_time(Date, 'dmy HMS',tz = "EST")) %>%
  select(-Date) %>%
  rename(Baro_pressure_psi = `Pressure(psi)`) %>%
  mutate(Baro_pressure_psi = as.double(Baro_pressure_psi))

baro_pressure <- bind_rows(pressure, pressure_a4d)
baro_pressure = baro_pressure %>%
  filter(!is.na(Baro_pressure_psi)) %>%
  arrange(DateTime) %>%
  mutate(DateTime = parse_date_time(DateTime, 'ymd HMS',tz = "EST"))

baro_pressure <- distinct(baro_pressure)


##Preliminary visualization of raw pressure data from catwalk transducer
plot_catwalk <- baro_pressure %>%
  mutate(Date = date(DateTime))

daily_catwalk <- group_by(plot_catwalk, Date) %>% summarize(daily_pressure_avg = mean(Baro_pressure_psi)) %>% mutate(Year = as.factor(year(Date)))

rawplot = ggplot(data = daily_catwalk[which(daily_catwalk$Year == 2016 & month(daily_catwalk$Date) == 12),])+
  geom_point(aes(x = Date, y = daily_pressure_avg))+
  ylab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
rawplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_baro_pressure.png", rawplot, device = "png")


pressure_hist = ggplot(data = daily_catwalk, aes(x = daily_pressure_avg, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
pressure_hist
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_catwalk_pressure_histogram.png", pressure_hist, device = "png")


pressure_boxplot = ggplot(data = daily_catwalk, aes(x = Year, y = daily_pressure_avg, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
pressure_boxplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_catwalk_pressure_boxplot.png", pressure_boxplot, device = "png")

##based on this anything above 14.2 or below 13.5 in 2014 for daily average looks like an outlier to me (when weir was leaking)
daily_catwalk[which(daily_catwalk$daily_pressure_avg <= 13.5 | daily_catwalk$daily_pressure_avg >= 14.2),]
##03-22 to 04-18 looks wonky

##ARGH!! DATETIMES ARE NOT PLAYING NICELY!! cannot figure it out so just wrote to .csv and read in again
inflow_pressure$DateTime <- as.POSIXct(inflow_pressure$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
baro_pressure$DateTime <- as.POSIXct(baro_pressure$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

write.csv(inflow_pressure, "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow.csv")
write.csv(baro_pressure, "./Data/DataNotYetUploadedToEDI/Raw_inflow/baro.csv")

##OK - round 2. let's see how the datetimes play together
baro <- read_csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/baro.csv")
inflow <- read_csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow.csv")

#correct datetime wonkiness from 2013-09-04 10:30 AM to 2014-02-05 11:00 AM
inflow$DateTime[24304:39090] = inflow$DateTime[24304:39090] - (6*60+43)

#merge inflow and barometric pressures to do differencing
diff = left_join(baro, inflow, by = "DateTime") %>%
  mutate(Pressure_psia = Pressure_psi - Baro_pressure_psi) %>%
  select(-c(1,4))
diff <- distinct(diff)

#writing to csv just so I can read it back in if needed
#write.csv(diff, ("./Data/DataNotYetUploadedToEDI/Raw_inflow/psia_working.csv"))

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

# ##Test to develop regression between nonsense units and psi
# nonsense <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/inflow.csv")
# 
# test <- left_join(nonsense, diff, by = "DateTime") %>%
#   filter(!is.na(Pressure_psia.y))
# 
# corr_units = ggplot(data = test, aes(x = Pressure_psia.x, y = Pressure_psia.y))+
#   geom_point()+
#   geom_smooth(method=lm)+
#   geom_abline(slope = 1, intercept = 0)+
#   stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)+
#   theme_bw()
# corr_units
#   
# ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/correlation_nonsense_units_w_psi.png", corr_units, device = "png")
# #Yeah....this is not a good idea


#create vector of corrected pressure to match nomenclature from RPM's script
diff <- diff %>%
  filter(!is.na(Pressure_psia))
flow2 <- diff$Pressure_psia 
  
#   ### CALCULATE THE FLOW RATES AT INFLOW ### #Taken from RPM's 'old school' script, verified 05-23-18
#   #################################################################################################
#   flow3 <- (flow2 - 6.3125 + 1.2) * 0.0254  # Response: Height above the weir (m), rating curve equation
#   flow4 <- (0.62 * (2/3) * (1.1) * 4.43 * (flow3 ^ 1.5) * 35.3147) # Flow CFS
#   flow5 <- flow4 * 60                                              # Flow CFM
#   flow6 <- (flow2 - 6.3125 + 1.2) / 12                         # Height above weir (ft)
#   flow7 <- 3.33 * (3.60892 - (0.2 * flow6)) * flow6 ^ 1.5          # Q eqn (cfs)
#   flow8 <- flow7 * 28.317                                          #Flows in (Liters/Second); converting cubic ft to liters
#   flow9 <- flow8 * (1 / 16.67)                                     #Flows in (Meters Cubed/Minute); converting liters to 
#   flow10 <- flow9 / 60                                             #Flows in (Meters Cubed/Second)
# #################################################################################################
  
  ### CALCULATE THE FLOW RATES AT INFLOW ### #(MEL 2018-07-06)
  #################################################################################################
  flow3 <- flow2*0.70324961490205 - 0.1603375 + 0.03048  # Response: Height above the weir (m)
#pressure*conversion factor for head in m - distance from tip of transducer to lip of weir + distance from tip of transducer to pressure sensor (eq converted to meters)
  flow4 <- (0.62 * (2/3) * (1.1) * 4.43 * (flow3 ^ 1.5) * 35.3147) # Flow CFS - MEL: I have not changed this; should be rating curve with area of weir
  flow_final <- flow4*0.028316847                                  # Flow CMS - just a conversion factor from cfs to cms
  #################################################################################################


diff$Reservoir <- "FCR" #creates reservoir column to match other data sets
diff$Site <- 100  #creates site column to match other data sets
diff$Flow_cms <- flow_final #creates column for flow

##visualization of inflow
plot_inflow <- diff %>%
  mutate(Date = date(DateTime))

daily_inflow <- group_by(plot_inflow, Date) %>% summarize(daily_flow_avg = mean(Flow_cms)) %>% mutate(Year = as.factor(year(Date)))


inflow = ggplot(daily_inflow, aes(x = Date, y = daily_flow_avg))+
  geom_point()+
  ylab("Avg. daily flow (cms)")+
  theme_bw()
inflow
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow.png", inflow, device = "png")


inflow2 = ggplot(daily_inflow, aes(x = Date, y = daily_flow_avg))+
  geom_line()+
  ylim(0,0.3)+
  ylab("Avg. daily flow (cms)")+
  theme_bw()
inflow2
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_no_outliers.png", inflow2, device = "png")

inflow_hist = ggplot(data = daily_inflow, aes(x = daily_flow_avg, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. inflow (cms)")+
  xlim(0,0.5)+
  theme_bw()
inflow_hist
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_histogram_wo_20140405.png", inflow_hist, device = "png")


inflow_boxplot = ggplot(data = daily_inflow, aes(x = Year, y = daily_flow_avg, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. inflow (cms)")+
  ylim(0,0.3)+
  theme_bw()
inflow_boxplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_boxplot_wo_20140405.png", inflow_boxplot, device = "png")

  # FLOWS <- bind_cols(diff,flow10) #binds relevant columns
  # FLOWS <- FLOWS %>%
  #   # Rename columns if needed (TargetName = OriginalName)
  #   rename(Flow_cms = flow10)
  
Inflow_Final <- diff[,c(6,7,2,4,1,5,8,3)] #orders columns
Inflow_Final <- Inflow_Final[order(Inflow_Final$DateTime),] #orders file by date
#Inflow_Final <- Inflow_Final[-c(1,which(Inflow_Final$DateTime>"2017-12-31 23:45:00")),] #limits data to before 2017 and takes out first row erroneous value
Inflow_Final <- Inflow_Final %>%
  filter(DateTime < "2017-12-31 23:45:00")
# Write to CSV
write.csv(Inflow_Final, './Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/inflow_working_07SEP18.csv', row.names=F) 

#####check newly calculated inflow values against CCC's old ones and EDI

new_inflow <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/inflow_working_07SEP18.csv") %>%
  mutate(time = date(DateTime),
         month = month(DateTime),
         year = year(DateTime)) %>%
  group_by(month,year) %>% 
  summarize(daily_flow_avg_new = mean(Flow_cms, na.rm = TRUE)) 

old_inflow <- read_csv("C:/Users/Mary Lofton/Documents/RProjects/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_inflow/FCR_weir_inflow_2013_2017_20180716.csv") %>%
  mutate(month = month(time),
         year = year(time))%>%
  group_by(month,year)%>%
  summarize(daily_flow_avg_old = mean(FLOW, na.rm = TRUE)) 

edi <- read_csv("C:/Users/Mary Lofton/Downloads/inflow (1).csv") %>%
  mutate(time = date(DateTime),
         month = month(DateTime),
         year = year(DateTime)) %>%
  group_by(month,year) %>% 
  summarize(daily_flow_avg_edi = mean(Flow_cms, na.rm = TRUE)) 

compare <- left_join(old_inflow, edi, by = c("month","year"))

compare <- left_join(compare, new_inflow, by = c("month","year")) %>%
  mutate(date = paste0(year,"-",month),
         #rpd = (daily_flow_avg_old - daily_flow_avg_new)/((daily_flow_avg_old + daily_flow_avg_new)/2),
         real_date = as.Date(paste(date,"-01",sep="")))
         #discrepancy_months = ifelse(abs(rpd)>=0.05,date,NA))

  
compare_plot <- ggplot(data = compare, aes(x = daily_flow_avg_new, y = daily_flow_avg_old, label = date))+
  geom_point()+
  geom_text(aes(label = date),hjust = 0.2, vjust = 1.2)+
  xlim(0,0.5)+
  #ylim(0,0.15)+
  geom_smooth(method=lm)+
  xlab("new_flow_cms")+
  ylab("old_flow_cms")+
  geom_abline(slope = 1, intercept = 0)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, vjust = -3)+
  theme_bw()
compare_plot
ggsave(plot = compare_plot, filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/old_vs_new_compare.png")

# compare_rpd <- ggplot(data = compare, aes(x = real_date, y = rpd*100, label = discrepancy_months))+
#   geom_point()+
#   xlab("")+
#   ylab("% difference in avg. monthly flow")+
#   ggtitle("Note: negative % differences mean old flow value was lower than new value")+
#   geom_text(aes(label = discrepancy_months),vjust = 1.2)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   theme_bw()
# compare_rpd
# ggsave(plot = compare_rpd, filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/old_vs_new_compare_percent_diff_monthly.png")

plotdata <- compare %>% gather(daily_flow_avg_old:daily_flow_avg_new, key = "dataset",value = "flow_cms")

timeseries <- ggplot(data = plotdata, aes(x = real_date, y = flow_cms, colour = dataset))+
  geom_line(size = 1)+
  geom_point(data = subset(plotdata, dataset == "daily_flow_avg_edi"), aes(x = real_date, y = flow_cms),size = 2)+
  #scale_size_manual(breaks = c("daily_flow_avg_edi","daily_flow_avg_new","daily_flow_avg_old"), values = c(2,1.5,1))+
  scale_colour_hue(labels = c("EDI dataset", "New dataset","Old dataset"))+
  xlab("")+
  theme_bw()
timeseries
ggsave(timeseries, filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/old_vs_new_vs_edi_compare_timeseries.png")
#times that seem to have problems:
#late March/early April 2014 (extreme outliers where new flow is much higher)
#mid-May to mid_June 2016 (separate line altogether)
#Jan-Feb 2016 ("crosspiece" on 1:1 line)

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
