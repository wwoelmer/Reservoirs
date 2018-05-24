#Make Zooplankton EML
#Cayelan Carey, based on EDI workshop 24 May 2018

# Install devtools
install.packages("devtools")

# Load devtools
library(devtools)

# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")

library(EMLassemblyline)
data<-read.csv('zooplankton.csv', header=TRUE)
View(data)

import_templates(path = "~/Dropbox/ComputerFiles/Virginia_Tech/Falling Creek/DataForWebsite/Github/ReservoirData/Formatted_Data/MakeEMLZooplankton",
                 license = "CCBY",
                 data.files = c("zooplankton"))

define_catvars(path = "~/Dropbox/ComputerFiles/Virginia_Tech/Falling Creek/DataForWebsite/Github/ReservoirData/Formatted_Data/MakeEMLZooplankton")

make_eml(path = "~/Dropbox/ComputerFiles/Virginia_Tech/Falling Creek/DataForWebsite/Github/ReservoirData/Formatted_Data/MakeEMLZooplankton",
         dataset.title = "Crustacean zooplankton density and biomass and rotifer density for Beaverdam Reservoir, Carvins Cove Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2014-2016",
         data.files = c("zooplankton"),
         data.files.description = c("Reservoir zooplankton dataset"),
         data.files.quote.character = c("\""),
         temporal.coverage = c("2014-04-04", "2016-10-25"),
         geographic.description = "Southwestern Virginia, USA, North America",
         maintenance.description = "completed", 
         user.id = "carylab0",
         package.id = "edi.198.1")
