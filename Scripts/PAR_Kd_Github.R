#PAR data clean-up
#Author: Mary Lofton
#Date: 14JUL17

#Beer's law
#A = Ebc
setwd("~/PAR/2016") 

#Get data
#This should be a csv with the following format:
#Column 1: Date (I wrote this script using dates in numeric form, FYI!)
#Column 2: Depth (in meters)
#Column 3: PAR (in umol photons m^-2 s^-1)
light <-read.csv("./BVR_PAR_2016.csv")

#Limit to only observations with data (in case data is not entered for some days/depths)
library(stats)
light <- na.omit(light)

#Get rid of row with zero (can't take log later)
##Go through each row and determine if a value is zero
row_sub = apply(light, 1, function(row) all(row !=0 ))
##Subset as usual
light <- light[row_sub,]

#Truncate to epilimnion - only do this if your specific task requires it
#Most of the time, you will skip this step
# epi<-light[(light$Depth <= 6),]
# light = epi


#For-loop to plot PAR values by day
#Just to make sure everything looks relatively normal
dates <- unique(light$Date)
d <- c(0:11)
par(mfrow=c(2,2))

for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(light, light$Date == j)
  plot(q$PAR,q$Depth,
       ylim = rev(range(d)),
       main = j,
       xlab = "Light",
       ylab = "Depth")
                  
  
} 

#For-loop to plot ln of PAR
#Again, just to lay eyes on it

for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(light, light$Date == j)
  plot(log(q$PAR),q$Depth,
       ylim = rev(range(d)),
       main = j,
       xlab = "Light",
       ylab = "Depth",
       abline(lm(q$Depth ~ log(q$PAR))))
  
  
} 


#Get percent surface light
dates <- unique(light$Date)
final <- matrix(data=NA, ncol=3,nrow=1)

for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(light, light$Date == j)
  surf <- q$PAR[1]
  temp <- matrix(data=NA, ncol = 3, nrow = length(q$Depth))
  for (i in 1:length(q$Depth)){
    percent <- (q$PAR[i]/surf)*100
    rowz <- c(j,q$Depth[i],percent)
    temp[i,] <- rowz
  }
  final <- rbind(final, temp)
}

final <- final[-1,]
final <- data.frame(final)
colnames(final) <- c("datetime", "depth", "%surface")

#I like to save a copy of this
write.csv(final, "BVR_percent_trans.csv")


#For-loop to plot %surface by day
percent_surf <- read.csv("./BVR_percent_trans.csv")
dates <- unique(percent_surf$datetime)
d <- c(0:9)
par(mfrow=c(2,2))

for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(percent_surf, percent_surf$datetime == j)
  plot(q$X.surface,q$depth,
       ylim = rev(range(d)),
       main = j,
       xlab = "Light",
       ylab = "Depth")
} 

#For-loop to plot ln of % surface by day

for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(percent_surf, percent_surf$datetime == j)
  plot(log(q$X.surface),q$depth,
       ylim = rev(range(d)),
       main = j,
       xlab = "Light",
       ylab = "Depth",
       abline(lm(q$depth ~ log(q$X.surface))))
  
  
} 

#Get slope for Kd
final <- matrix(data=NA, ncol=2, nrow=24)


for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(percent_surf, percent_surf$datetime == j)
  mod <- lm(q$depth ~ log(q$X.surface))
  slope <- mod$coefficients[2]
  temp <- c(j,slope)
  final[i,] <- temp
  
}

final <- data.frame(final)

#Yay! :)
write.csv(final, "BVR_Kd_2016.csv")

#Plot FCR light attenuation
#Looks like I manually converted the csv of Kd values to a txt file 
#with datetime format of yyyy-mm-dd hh:mm:ss cause I don't have code for it here...?

library(rLakeAnalyzer)
data <- load.ts("./FCR_Kd_2016.txt")
plot(data$datetime, data$kd, type = "b", main = "FCR Kd 2016")
#The next three lines create vertical lines on particular days to denote special events of 
#interest, such as EM experiments
abline(v=as.numeric(data$datetime[5]), lwd=1, col='blue')
# abline(v=as.numeric(data$datetime[11]), lwd=1, col='blue')
# abline(v=as.numeric(data$datetime[17]), lwd=1, col='blue')


#Plot BVR light attenuation

data2 <- load.ts("./BVR_Kd_2016.txt")
plot(data2$datetime[3:22], data2$kd[3:22], type = "b", main = "BVR Kd 2016")
abline(v=as.numeric(data2$datetime[8]), lwd=1, col='blue')
abline(v=as.numeric(data2$datetime[12]), lwd=1, col='blue')
abline(v=as.numeric(data2$datetime[17]), lwd=1, col='blue')


#Compare the two
plot(data2$datetime[3:16], data2$kd[3:16], type = "l", main = "",
     xlab = "",
     ylab = expression("k  " ~ (m^{-1})),
     ylim = c(0.75,2.55),
     lty = 3,
     lwd = 2,
     xaxt = 'n')
axis.POSIXct(1,at = c(data2$datetime[4],data2$datetime[7],data2$datetime[11],data2$datetime[12],
                      data2$datetime[15]),
             labels = c("12 May", "29 May", "16 Jun", "28 Jun", "14 Jul"))
points(data$datetime[1:16], data$kd[1:16], type = "l", lty = 1, lwd = 2)
abline(v=as.numeric(data2$datetime[8]), lwd=2, col='darkgray')
abline(v=as.numeric(data2$datetime[12]), lwd=2, col='darkgray')
#abline(v=as.numeric(data2$datetime[17]), lwd=1, col='blue')
legend("topright", legend = c("FCR", "BVR"), 
       lty=c(1,3), lwd=c(2,2), bty = "n")



  
 
