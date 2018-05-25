#### FCR Inflow EML ####
pacman::p_load(devtools) #installs and loads to use install_github function

# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)


### Step 5: load templates ###
# View documentation for this function
#?import_templates

# Import templates for an example dataset licensed under CC0, with 2 tables.
import_templates(path = "~/Reservoirs/Formatted_Data/MakeEMLInflow/",
                 license = "CCBY",
                 data.files = "inflow")

# Import templates for an example dataset licensed under CCBY, with 2 tables.
import_templates(path = "",
                 license = "CCBY",
                 data.files = c("inflow"))

data<-read.csv('inflow.csv', header=TRUE)
View(data)

define_catvars(path = "")

make_eml(path = ".",
         dataset.title = "Discharge time series for the primary inflow tributary entering Falling Creek Reservoir, Vinton, Virginia, USA 2013-2017",
         data.files = c("inflow"),
         data.files.description = c("FCR inflow dataset"),
         data.files.quote.character = c("\""),
         temporal.coverage = c("2013-05-15", "2017-12-31"),
         geographic.description = "Southwestern Virginia, USA, North America",
         maintenance.description = "ongoing", 
         user.id = "carylab0",
         package.id = "edi.200.1")
