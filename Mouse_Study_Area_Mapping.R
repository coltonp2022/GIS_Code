# Mouse Study Area Mapping
# Colton Padilla
# 12/7/2022

#==============================================================================#
#                                  Housekeeping                             ####
#==============================================================================#

# Packages 
library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(smoothr)

# Set working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/2022 Field Season/Vegetation Data/Vegetation Data")

# Pull in the vegetation data with waypoints
# The name of this file will change with the change in date
veg <- read_csv("veg_8_9_2022.csv") %>%
  dplyr::select(site_id, x, y)

# Change errant points
veg[8,3] <- 35.88441 # Thinking wrong recording prior to geopoint
veg[105,3] <- 35.97025 # Errant point

# Select the first point by site_id
veg <- veg %>% group_by(site_id) %>% filter(row_number() == 1)

# Add Burn column
veg$burn <- ifelse(grepl("TR", veg$site_id), "Thompson Ridge",
                   ifelse(grepl("LC", veg$site_id), "Las Conchas", "Unburned"))

#==============================================================================#
#                                Spatial Work                               ####
#==============================================================================#

# Make this data base into a SF  
points_sf <- st_as_sf(
  veg, # Data to make into sf
  coords = c("x", "y"), # Columns signifying coordinates
  crs = 4326 # Coordinate reference system, make sure this is correct
)

# Project these to NAD83 13N
points_sf <- st_transform(
  points_sf, # SF object
  crs = 26913 # CRS to project to 
)

# View this data to make sure nothing is wrong
mapview(points_sf)

# Save this shapefile to map in ArcGIS
st_write(points_sf,
         dsn = ".",
         layer = "Mouse_Trapline_Locations_2022",
         driver = "ESRI Shapefile",
         append = F)
