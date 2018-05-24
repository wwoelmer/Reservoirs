##MakeEMLChemistry
##Author: Mary Lofton
##Date: 24MAY18

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
#in this case, our directory is Reservoirs/Formatted_Data/MakeEMLChemistry

#Step 2: Move your dataset to the directory

#Step 3: Create an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset
#right now the only supported option is "table"

#Step 5: Import the core metadata templates

# View documentation for this function
?import_templates

# Import templates for an example dataset licensed under CC0, with 2 tables.
import_templates(path = "C:/Users/Mary Lofton/Documents/RProjects/Reservoirs/Formatted_Data/MakeEMLChemistry",
                 license = "CCBY",
                 data.files = c("chemistry"))

#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information
#nothing mandatory for Carey Lab in this section

#Step 10: Keywords
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#see the LabKeywords.txt file for keywords that are mandatory for all Carey Lab data products

#Step 11: Personnel
#copy-paste this information in from your metadata document
#Cayelan needs to be listed several times; she has to be listed separately for her roles as
#PI, creator, and contact, and also separately for each separate funding source (!!)

#Step 12: Attributes
#grab attribute names and definitions from your metadata word document
#for units....
# View and search the standard units dictionary
view_unit_dictionary()
#put flag codes and site codes in the definitions cell
#force reservoir to categorical

#Step 13: Close files
#if all your files aren't closed, sometimes functions don't work

#Step 14: Categorical variables
# View documentation for this function
?define_catvars

# Run this function for your dataset
define_catvars(path = "C:/Users/Mary Lofton/Documents/RProjects/Reservoirs/Formatted_Data/MakeEMLChemistry")

#open the created value IN A SPREADSHEET EDITOR and add a definition for each category


