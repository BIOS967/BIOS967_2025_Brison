

#------------------------------------------------------------------------------#
#-----                      Download & Load Packages                      -----#
#------------------------------------------------------------------------------#

list <- c("tigris", "readr", "dplyr", "sf", "ggplot2", "units", "spdep")
for (packages in list) {
  if (!packages %in% rownames(installed.packages())) { install.packages(packages, dependencies = TRUE) }
  library(packages, character.only = TRUE) }
rm(list) # Frees up memory


################################################################################
#------------------------------------------------------------------------------#
#-----                        IDW of Nitrate Data                         -----#
#------------------------------------------------------------------------------#
################################################################################

#------------------------------- Load in Files --------------------------------#
  # 1. Shapefile of Nebraska
ne_state <- states(cb = TRUE) |> filter(STUSPS == "NE") |> st_as_sf()
ne_state_utm <- st_transform(ne_state, 26914)   # UTM for NE

  # 2. Make points from the CSV Files
nitrate_pts <- read_csv("Data/Ne_Nitrogen_data.csv")
nitrate_utm <- nitrate_pts |> st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |> 
  st_transform(26914)

# Also Fix Error with the file
# Makes Concentration a numeric & remove NAs added by the coercion or something like that idk...
nitrate_utm$Concentration <- as.numeric(nitrate_utm$Concentration)
nitrate_utm$Concentration[is.na(nitrate_utm$Concentration)] <- 0

#-------------------------------   Set up IDW  --------------------------------#

# Make a blank grid over the state
ne_grid_utm <- ne_state_utm |> 
  st_make_grid(cellsize = 5000) |>
  st_as_sf() |>
  st_intersection(ne_state_utm) |>
  mutate(grid_id = dplyr::row_number())

# sample locations (grid centroids)
ne_samp_utm <- ne_grid_utm |> st_centroid()

# distance matrix (gridpoints x nitrate points)
dmat <- st_distance(ne_samp_utm, nitrate_utm) |> units::drop_units()

# weight matrix: inverse distance^3, row-normalized
rownorm <- \(x) sweep(x, 1, rowSums(x), "/")
W <- rownorm(dmat^-3)

#-----------------------------  Run IDW Analysis ------------------------------#

# IDW interpolation: result is numeric
ne_grid_utm <- ne_grid_utm |>
  mutate(nitrate_idw = as.numeric(W %*% nitrate_utm$Concentration))

#-----------------------------     Graph Map     ------------------------------#

breaks <- c(-Inf, 1, 3.9, 6.9, 10, Inf)
labels <- c("< 1", "1 – 3.9", "4 – 6.9", "7 – 10", "> 10")

ne_grid_utm <- ne_grid_utm |> mutate(nitrate_class = cut(nitrate_idw, 
                                                         breaks = breaks, 
                                                         labels = labels, 
                                                         right = TRUE))

ggplot(ne_grid_utm, aes(fill = nitrate_class)) +
  geom_sf(alpha = 1, col = NA) +
  geom_sf(data = ne_state_utm, fill = NA, col = "darkred", lwd = 1) +
  scale_fill_manual(name = "Nitrate (mg/L)",
                    values = c("< 1"     = "#ffffcc",
                               "1 – 3.9" = "#a1dab4",
                               "4 – 6.9" = "#41b6c4",
                               "7 – 10"  = "#2c7fb8", 
                               "> 10"    = "#253494"))


################################################################################
#------------------------------------------------------------------------------#
#-----                             Moran's I                              -----#
#------------------------------------------------------------------------------#
################################################################################


#--------------------------    Remove Duplicates    ---------------------------#

nitrate_pts <- readr::read_csv("Data/Ne_Nitrogen_data.csv") |>
  mutate(SampleDate = as.Date(SampleDate)) |>   
  arrange(desc(SampleDate)) |>                        # newest first
  distinct(Longitude, Latitude, .keep_all = TRUE)     # keep newest per oordinate

#-------------------------------- Setup Stuff ---------------------------------#
nitrate_utm <- nitrate_pts |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  st_transform(26914) |>
  mutate(Concentration = as.numeric(Concentration)) |>
  filter(!is.na(Concentration))                       # don't turn NA -> 0

coords <- st_coordinates(nitrate_utm)
k <- 6
pt_nb <- knearneigh(coords, k = k) |> knn2nb()
pt_lw <- nb2listw(pt_nb, style = "W")

#--------------------------        Moran’s I         --------------------------#

moran.test(nitrate_utm$Concentration, pt_lw)
moran.plot(nitrate_utm$Concentration, pt_lw)








