

################################################################################
#\\\\\                                                                    /////#
#\\\\\                        Analysis of Datasets                        /////#
#\\\\\                                                                    /////#
################################################################################


library(lubridate)


#------------------------------------------------------------------------------#
#-----                  USGS Public Gas Data - All States                 -----#
#------------------------------------------------------------------------------#


# USGS_Gas_Data (ScienceBase - Open Access)
USGS_Gas_Data <- GET("https://www.sciencebase.gov/catalog/file/get/60f19169d34e93b36670519b?name=Natural%20Gas%20Dataset.csv")
writeBin(content(USGS_Gas_Data, "raw"), "USGS_Gas_Data.csv") # Save excel file in working directory

USGS_Gas_Data <- read.csv("USGS_Gas_Data.csv") %>%
  dplyr::select(ID, LAT, LONG, STATE, HE, CO2, H2, N2, H2S, AR, O2, C1, C2, C3, 
                N.C4, I.C4, N.C5, I.C5, C6., BTU, DEPTH, FINAL_SAMPLING_DATE)

write.csv(USGS_Gas_Data, "USGS_Gas_Data.csv", row.names = FALSE)

#------------------------------------------------------------------------------#

# Load in Data
USGS_Gas_Data <- read.csv("USGS_Gas_Data.csv")


#------------------------- Sample Distribution by Year ------------------------#

# Convert to Date and extract year
USGS_Gas_Data$FINAL_SAMPLING_DATE <- as.Date(USGS_Gas_Data$FINAL_SAMPLING_DATE, format="%m/%d/%Y")
USGS_Gas_Data$YEAR <- year(USGS_Gas_Data$FINAL_SAMPLING_DATE)

# ensure YEAR numeric and subset for H2 samples
USGS_Gas_Data$YEAR <- as.numeric(USGS_Gas_Data$YEAR)
USGS_Gas_H2_Data <- USGS_Gas_Data %>% filter(!is.na(H2) & H2 != "")
# H2 data thats greater than or equal to 1
USGS_Gas_H2conc_Data <- USGS_Gas_H2_Data %>% filter(H2 >= .1)

# define common breaks (5-year bins here; tweak binwidth as desired)
minY <- floor(min(USGS_Gas_Data$YEAR, na.rm=TRUE) / 5) * 5
maxY <- ceiling(max(USGS_Gas_Data$YEAR, na.rm=TRUE) / 5) * 5
breaks <- seq(minY, maxY, by = 5)

# plot all-samples histogram
hist(USGS_Gas_Data$YEAR,
                 breaks = breaks,
                 col = rgb(0.2, 0.6, 0.9, 0.6), # semi-transparent blue
                 border = "darkblue",
                 main = "Sample Years",
                 xlab = "Year",
                 ylab = "Count")

# overlay H2 histogram using same breaks
hist(USGS_Gas_H2_Data$YEAR, breaks = breaks,
                col = rgb(0, 0.8, 0.2, 0.8), # slightly more opaque green
                border = "darkgreen", add = TRUE)

# overlay h2 data greater than or equal to .1
hist(USGS_Gas_H2conc_Data$YEAR, breaks = breaks,
                     col = rgb(0, 0.5, .1, 0.9),
                     border = "black", add = TRUE)

# add legend
legend("topright", legend = c("All Samples", "H2 Only", "H2 ≥ 0.1%"),
       fill = c(rgb(0.2, 0.6, 0.9, 0.6), rgb(0, 0.8, 0.2, 0.8), rgb(0, 0.5, .1, 0.9)),
       border = c("darkblue", "darkgreen", "black"), bty = "n") # no box around legend



d#------------------------- Map of H2 Concentrations --------------------------#

# Load required libraries
library(dplyr)
library(sf)
library(tmap)

tmap_mode("plot")

USGS_Gas_H2_Data <- USGS_Gas_Data %>%
  filter(!is.na(LAT) & !is.na(LONG)) %>% # Remove rows with missing coordinates
  #select rows where h2 has values, remove rows with NA or blank in H2 column
  filter(!is.na(H2) & H2 != "") %>%
  dplyr::mutate(H2 = as.numeric(H2)) %>%
  dplyr::select(ID, LAT, LONG, STATE, H2, FINAL_SAMPLING_DATE) %>%
  filter(H2 > 0) %>% # Keep only rows where H2 is greater than 0
  # keep only dates after the year 2000
  # date format is currently day month year (e.g. 2/27/1920)
  filter(as.Date(FINAL_SAMPLING_DATE, format="%m/%d/%Y") >= as.Date("2000-01-01"))

# convert df to sf object
USGS_Gas_H2_Data <- st_as_sf(USGS_Gas_H2_Data, coords = c("LONG", "LAT"), crs = 4326)

# Define a border for the US
us_bbox <- c(xmin = -115, ymin = 24, xmax = -80, ymax = 50)


tm_shape(USGS_Gas_H2_Data, bbox = us_bbox) +
  tm_basemap("Esri.WorldGrayCanvas", zoom = 7) + #Esri.WorldGrayCanvas
  tm_dots(size = 0.1, col = "red") +
  tm_title("USGS Natural Gas Data - H2 Concentrations", position = tm_pos_out("center", "top")) +
  #Add north arrow to map
  tm_compass(type = "4star", size = 2, position = c("right", "bottom")) +
  #Add scale to map
  tm_scalebar(position = c("left", "bottom"))
  




?tm_compass






