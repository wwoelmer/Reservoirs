##MakeEMLFluoroProbe
##Author: Mary Lofton
##Date: 19DEC18

# # Install devtools
# install.packages("devtools")
# 
# # Load devtools
# library(devtools)
# 
# # Install and load EMLassemblyline
# install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

#Step 1: Create a directory for your dataset
#in this case, our directory is Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe

#Step 2: Move your dataset to the directory - duh.

#Step 3: Create an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset
#right now the only supported option is "table"