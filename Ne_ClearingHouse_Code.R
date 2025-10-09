
#------------------------------------------------------------------------------#
#-----                      Download & Load Packages                      -----#
#------------------------------------------------------------------------------#

if (!is.element("dplyr", installed.packages())) install.packages("dplyr", dep = T)
library(dplyr)
if (!is.element("httr",  installed.packages())) install.packages("httr",  dep = T)
library(httr)
if (!is.element("readr", installed.packages())) install.packages("readr", dep = T)
library(readr)
if (!is.element("readxl", installed.packages())) install.packages("readxl", dep = T)
library(readxl)
if (!is.element("dataRetrieval", installed.packages())) install.packages("dataRetrieval", dep = T)
library(dataRetrieval)


       #### Make sure working directory is assigned before proceeding ####    

#------------------------------------------------------------------------------#
#-----       Nebraska Clearing House Nitrate Data Extraction & Prep       -----#
#------------------------------------------------------------------------------#

# GET request -> download excel file
response <- GET("https://clearinghouse.nebraska.gov/api/api/export/download")
writeBin(content(response, "raw"), "ProcessingStuff/Raw_Data.xlsx") # Save excel file in working directory folder
files <- "ProcessingStuff/Raw_Data.xlsx" # Read the Excel file into R


#------------------------- Load in Individual Sheets --------------------------#

# Load required sheets from downloaded excel doc.
XYcord      <- read_excel(files, sheet = "Facility")
SampleDates <- read_excel(files, sheet = "Sample")
Results     <- read_excel(files, sheet = "Result")

#------------------------------  Combine Sheets  ------------------------------#

#Join "SampleDates" to Results by the "SampleID" column
Nitrogen <- merge(Results, SampleDates, by = "SampleID")

# Remove all numbers besides 22 (nitrates designation) from "ParamID" column
Nitrogen <- Nitrogen[Nitrogen$ParamID == 22, ]

#Join x/y coordinates to the nitrate data
Nitrogen <- Nitrogen %>% rename(FacilityID = FacilityID.x)
Nitrogen <- Nitrogen %>% left_join(XYcord, by = "FacilityID")

#--------------------- Rename and Select Columns to Keep ----------------------#
Nitrogen <- Nitrogen %>% 
  rename( Latitude = `Latitude (Decimal Degrees)`, # rename columns to more manageable names...
          Longitude = `Longitude (Decimal Degrees)`,
          WellDepth = `Well Depth (Feet)`,
          Clearinghouse = `Clearinghouse Number`,
          DNR_Well_ID = `DNR Well ID`,
          DNR_Reg_Num = `DNR Registration Number`) %>%
  dplyr::select(FacilityID, SampleID, Concentration, # need to specify the dplyr package here to avoid conflicts with other packages
                Units, SampleDate, SampleName, 
                Latitude, Longitude, WellDepth, 
                Clearinghouse, DNR_Well_ID, DNR_Reg_Num)

#----------------------- Remove Duplicate Well Entries ------------------------#
#There are alot of wells with multiple samples so I want to keep only the most recent ones
#Step 1: Convert SampleDate to a simpler date/time format
#Step 2: Arrange by FacilityID and SampleDate (most recent samples first)
#Step 3: Keep only the first (most recent) row for every unique FacilityID

Nitrogen <- Nitrogen %>% 
  mutate(SampleDate = mdy_hms(SampleDate)) %>%
  arrange(FacilityID, desc(SampleDate)) %>% 
  distinct(FacilityID, .keep_all = TRUE)

#--------------------- Save the cleaned up data to a CSV ----------------------#

# Write to new CSV  ## Will save to the working directory
write.csv(Nitrogen, "Ne_Nitrogen_data.csv", row.names = FALSE)










#------------------------------------------------------------------------------#
#-----                                End                                 -----#
#------------------------------------------------------------------------------#







