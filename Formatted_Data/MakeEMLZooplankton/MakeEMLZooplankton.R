#Make Zooplankton EML
#Cayelan Carey, based on EDI workshop 24 May 2018

library(EMLassemblyline)
data<-read.csv('zooplankton.csv', header=TRUE)
View(data)
import_templates(path = "~/Dropbox/ComputerFiles/Virginia_Tech/Falling Creek/DataForWebsite/Github/ReservoirData/Formatted_Data/MakeEMLZooplankton",
                 license = "CCBY",
                 data.files = c("zooplankton"))
