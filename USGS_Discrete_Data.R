

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


#------------------------------------------------------------------------------#
#-----                      USGS Discrete Sample Data                     -----#
#------------------------------------------------------------------------------#

#-----          Select Analytes of Interest & Get their Pcodes            -----#

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
write.csv(analyte_list_df, "USGS_MidCont_Water_Sample_Data.csv", row.names = FALSE)


#------------------------------------------------------------------------------#
#-----                  Individual Contaminants Processing                -----#
#------------------------------------------------------------------------------#

# Load In Data
analytes <- read.csv("USGS_MidCont_Water_Sample_Data.csv")


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
write.csv(Nitrate, "USGS_Discrete_Outputs/USGS_Nitrate_data.csv", row.names = FALSE)


# Uranium ---------------------------------------------------------------------#

Uranium <- analytes %>%
  filter(grepl("Uranium", Result_Characteristic)) %>%
  filter(Activity_StartDate >= as.Date("2000-01-01")) %>%
  filter(!is.na(Result_Measure))

# check units
unique(Uranium$Result_MeasureUnit)

# Save as CSV
write.csv(Uranium, "USGS_Discrete_Outputs/USGS_Uranium_data.csv", row.names = FALSE)


# Iron ------------------------------------------------------------------------#

Iron <- analytes %>%
  filter(grepl("Iron", Result_Characteristic)) %>%
  filter(Activity_StartDate >= as.Date("2000-01-01"))

# check units
unique(Iron$Result_MeasureUnit)

# convert ug/L units to mg/L, remove "mg/kg" and NA values
Iron <- Iron %>%
  mutate(Result_Measure = case_when(
    Result_MeasureUnit == "ug/L" ~ Result_Measure / 1000,
    Result_MeasureUnit == "mg/L" ~ Result_Measure,
    TRUE ~ NA_real_)) %>%
  filter(!is.na(Result_Measure)) %>%
  mutate(Result_MeasureUnit = "mg/L")

# Save as CSV
write.csv(Iron, "USGS_Discrete_Outputs/USGS_Iron_data.csv", row.names = FALSE)


# Sulfate ---------------------------------------------------------------------#

Sulfate <- analytes %>%
  filter(grepl("Sulfate", Result_Characteristic)) %>%
  filter(Activity_StartDate >= as.Date("2000-01-01"))

# check units
unique(Sulfate$Result_MeasureUnit)

# remove NA values
Sulfate <- Sulfate %>% filter(!is.na(Result_Measure))

# Save as CSV
write.csv(Sulfate, "USGS_Discrete_Outputs/USGS_Sulfate_data.csv", row.names = FALSE)


# Acidity, (H+) ---------------------------------------------------------------#

Acidity <- analytes %>%
  filter(grepl("Acidity", Result_Characteristic, ignore.case = TRUE)) %>%
  filter(Activity_StartDate >= as.Date("2000-01-01"))

# check units
unique(Acidity$Result_MeasureUnit)

# Save as CSV
write.csv(Acidity, "USGS_Discrete_Outputs/USGS_Acidity_data.csv", row.names = FALSE)


# Dissolved oxygen (DO) -------------------------------------------------------#

Dissolved_Oxygen <- analytes %>%
  filter(grepl("Dissolved oxygen", Result_Characteristic, ignore.case = TRUE)) %>%
  filter(Activity_StartDate >= as.Date("2000-01-01"))

# check units
unique(Dissolved_Oxygen$Result_MeasureUnit)

# select mg/L
Dissolved_Oxygen <- Dissolved_Oxygen %>%
  filter(Result_MeasureUnit == "mg/L") %>% filter(!is.na(Result_Measure))

# Save as CSV
write.csv(Dissolved_Oxygen, "USGS_Discrete_Outputs/USGS_Dissolved_Oxygen_data.csv", row.names = FALSE)


















