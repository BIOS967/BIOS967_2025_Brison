

#------------------------------------------------------------------------------#
#-----                      Download & Load Packages                      -----#
#------------------------------------------------------------------------------#

if (!is.element("tidyverse", installed.packages())) install.packages("tidyverse", dep = T)
library(tidyverse)
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

# Make sure working directory is assigned before proceeding #   




################################################################################
#------------------------------------------------------------------------------#
#-----                      USGS Discrete Sample Data                     -----#
#------------------------------------------------------------------------------#
################################################################################

#--- Select Analytes of Interest & Get their Pcodes from the official List  ---#

# Download All USGS parameter codes 
list2_raw <- read.delim("https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&group_cd=%", 
                        comment.char = "#", # Removes comments
                        skip = 2, # Skips the first two lines since they are header rows we dont want :) 
                        stringsAsFactors = FALSE) # Prevents R from converting text to factors

# Filter for only analytes of interest & select their USGS parameter codes
list2_sub <- list2_raw %>%
  filter(grepl("nitrate|uranium|sulfate|sulfide|hydrogen|iron|dissolved oxygen", # Analytes of interest names 
               parm_nm,                   # The column name we look through to find the analytes 
               ignore.case = TRUE))  %>%  # Ignore capitalization when searching so it gets more results
  
  filter(!is.na(CASRN) & str_trim(CASRN) != "") %>% # This removes any rows without a CASRN Number becasue I dont trust it if it dont have it
 
  select(parm_cd) # Selects ONLY the parameter code column & puts it into the new dataframe

# List of states & FIPS codes to get data for
states <- c("US:19", # Iowa
            "US:20", # Kansas
            "US:31") # Nebraska


#------------------------------------------------------------------------------#
#--  --  --         Download Data From the USGS API R Package        --  --  --#

                # lapply loops through each state
analyte_list <- lapply(states, function(s) {   
  read_waterdata_samples(usgsPCode = list2_sub$parm_cd, # Lists all Pcodes to grab
                         stateFips = s)    })   # Lists each state individually

#------------------------------------------------------------------------------#
#--  --  --  --  --  --     Combine / Clean Results      --  --  --  --  --  --#

analyte_list_df <- bind_rows(analyte_list) %>% # Combine results into one data frame
  
  dplyr::select("Location_Identifier", "Location_Type", "Location_State",       # Columns to keep from the API results
                "Location_LatitudeStandardized", "Location_LongitudeStandardized", 
                "Location_HorzCoordStandardizedDatum", "Activity_MediaSubdivision", 
                "Activity_StartDate", "Activity_DepthHeightMeasure", "Activity_DepthHeightMeasureUnit", 
                "SampleCollectionMethod_Description", "Result_Characteristic", 
                "Result_CharacteristicUserSupplied", "Result_CASNumber", 
                "Result_Measure", "Result_MeasureUnit", "USGSpcode")

# Save combined data as CSV
write.csv(analyte_list_df, "Data/USGS_MidCont_Water_Sample_Data.csv", row.names = FALSE)


#------------------------------------------------------------------------------#
#-----                  Individual Contaminants Processing                -----#

# Load In Data
analytes <- read.csv("Data/USGS_MidCont_Water_Sample_Data.csv")


# Nitrate ---------------------------------------------------------------------#

Nitrate <- analytes %>%
  filter(grepl("nitrate", Result_Characteristic, ignore.case = TRUE)) %>%
  filter(Activity_StartDate >= as.Date("2000-01-01")) 

# convert ug/L units to mg/L, remove "mg/kg" and NA values
Nitrate <- Nitrate %>%
  mutate(Result_Measure = case_when(
    Result_MeasureUnit == "ug/L" ~ Result_Measure / 1000,
    Result_MeasureUnit == "mg/L" ~ Result_Measure,
    TRUE ~ NA_real_)) %>%
  filter(!is.na(Result_Measure)) %>%
  mutate(Result_MeasureUnit = "mg/L")

# Save as CSV
write.csv(Nitrate, "Data/USGS_Discrete_Outputs/USGS_Nitrate_data.csv", row.names = FALSE)


# Uranium ---------------------------------------------------------------------#

Uranium <- analytes %>%
  filter(grepl("Uranium", Result_Characteristic)) %>%
  filter(Activity_StartDate >= as.Date("2000-01-01")) %>%
  filter(!is.na(Result_Measure))

# check units
unique(Uranium$Result_MeasureUnit)

# Save as CSV
write.csv(Uranium, "Data/USGS_Discrete_Outputs/USGS_Uranium_data.csv", row.names = FALSE)







################################################################################
#------------------------------------------------------------------------------#
#-----       Nebraska Clearing House Nitrate Data Extraction & Prep       -----#
#------------------------------------------------------------------------------#
################################################################################


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
Nitrogen <- Nitrogen %>% 
  mutate(SampleDate = mdy_hms(SampleDate)) %>% #Step 1: Convert SampleDate to a simpler date/time format
  arrange(FacilityID, desc(SampleDate)) %>%    #Step 2: Arrange by FacilityID and SampleDate (most recent samples first)
  distinct(FacilityID, .keep_all = TRUE)       #Step 3: Keep only the first (most recent) row for every unique FacilityID


#--------------------- Save the cleaned up data to a CSV ----------------------#
# Create & save a new CSV file
write.csv(Nitrogen, "Ne_Nitrogen_data.csv", row.names = FALSE)






################################################################################
#------------------------------------------------------------------------------#
#-----                  USGS Public Gas Data - All States                 -----#
#------------------------------------------------------------------------------#
################################################################################

# USGS_Gas_Data (ScienceBase - Open Access)
USGS_Gas_Data <- GET("https://www.sciencebase.gov/catalog/file/get/60f19169d34e93b36670519b?name=Natural%20Gas%20Dataset.csv")
writeBin(content(USGS_Gas_Data, "raw"), "USGS_Gas_Data.csv") # Save excel file in working directory

USGS_Gas_Data <- read.csv("USGS_Gas_Data.csv") %>%
  dplyr::select(ID, LAT, LONG, STATE, HE, CO2, H2, N2, H2S, AR, O2, C1, C2, C3, 
                N.C4, I.C4, N.C5, I.C5, C6., BTU, DEPTH, FINAL_SAMPLING_DATE)

write.csv(USGS_Gas_Data, "USGS_Gas_Data.csv", row.names = FALSE)









