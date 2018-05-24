#### FCR Inflow EML ####
pacman::p_load(devtools) #installs and loads to use install_github function

# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

### Step 1: set working directory ###
setwd("~/Reservoirs/Formatted_Data/MakeEMLInflow/")

### Step 5: load templates ###
# View documentation for this function
#?import_templates

# Import templates for an example dataset licensed under CC0, with 2 tables.
import_templates(path = "",
                 license = "CC0",
                 data.files = c("lake_chloride_concentrations",
                                "lake_characteristics"))