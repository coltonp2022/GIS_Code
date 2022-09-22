# Creating traplines and extracting data
# Colton Padilla
# 5/26/2022

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# This code was built to create line features from gps waypoints taken along 
# each trapline. Here we bring in the gps locations, connect them all by a line,
# buffer that line, then extract a mean value for each macrohabitat variable. 
# This will then be linked into the rodent ectoparasite data base for glms.

# Values were extracted to the buffer. Both means and standard deviations were
# created for the variables.

# Code last updated: 8/22/2022
# Created variables with standard deviations.
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

#---------------------#
# Data preparation ####
#---------------------#

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

# Change errant point in TR15
veg[8,3] <- 35.88441 # Thinking wrong recording prior to geopoint
veg[105,3] <- 35.97025

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

#------------------------------------------#
# Add lines between points at each site ####
#------------------------------------------#

# Create lines
lines <- points_sf %>%
  group_by(site_id) %>% # Group the sites together
  dplyr::summarize(do_union = F) %>% # Use the summarise function
  st_cast("LINESTRING") # Cast a line along the trapline

# Simplify the line
#lines <- smooth(lines, method = "ksmooth") 

# Simplify the lines
lines <- st_simplify(lines, dTolerance = 10) 
  
# View these lines to make sure nothing is wrong
mapview(lines) + mapview(points_sf)

# Calculate lengths of the lines
lines$length <- st_length(lines)

#----------------------------------#
# Create a buffer for the lines ####
#----------------------------------#

# Buffer
lines_buff <- st_buffer(
  lines, # Lines to buffer
  dist = 100 # Buffer distance in meters. Units depend on your projection
)

# Calculate area for the buffer
lines_buff$area <- st_area(lines_buff)

# View
mapview(lines) + mapview(lines_buff)

#-------------------------------------#
# Bring in the Mixed Conifer layer ####
#-------------------------------------#

# WD
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Data_and_Initial_Maps/Land_Cover_Data")

# MixCon 
mixcon <- st_read("Mixed_Conifer.shp")

# View the lines on the Mixed Conifer
mapview(mixcon, zcol = "fire_hist") + mapview(lines) + mapview(lines_buff)

#---------------------------#
# Bring in GIS Variables ####
#---------------------------#

# --- Tree Canopy Cover --- #
# Set working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Data_and_Initial_Maps/Tree_Canopy_Cover")

# Load in the raster
tcc <- rast("TCC_SA_26913.tif")

# --- Topographic Variables --- #
# Set working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles")

# Aspect
asp <- rast("sa_aspect_NAD83_13N.tif")
# DEM
elev <- rast("sa_dem_NAD83_13N.tif")
# Hillshade
hill <- rast("sa_hillshade_NAD83_13N.tif")
# HLI
hli <- rast("sa_hli_NAD83_13N.tif")
# Slope
slp <- rast("sa_slope_NAD83_13N.tif")
# Trasp
trasp <- rast("sa_trasp_NAD83_13N.tif")
# Topographic Wetness Index
twi <- rast("sa_TWI_R_NAD83_13N.tif")
# Vector Ruggedness Measure 3
vrm <- rast("sa_vrm_NAD83_13N.tif")
# Topographic Position Index
tpi <- rast("sa_TPI_NAD83_13N.tif")

# Create a raster stack
stack <- rast(
  list(asp, elev, hill, hli, slp, trasp, twi, vrm, tpi)
)

# Create names for the stack
names(stack) <- c("asp", "elev", "hill", "hli", "slp", "trasp", "twi", "vrm",
                  "tpi")

# --- Burn Severity --- #
## Set working directory
#setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop")
#
## Load the TR Severity file
#TR_sev <- st_read("ThompsonRidge_BurnSeverity.shp")
#
## Load the LC Severity file and manipulate 
#LC_sev <- st_read("./Final_Burn_Severity/LasConchas_FinalBurnSeverity.shp") 
#
## Now add GRIDCODE to the LC Severity file
#LC_sev$GRIDCODE <- ifelse(LC_sev$Severity == "Unchanged", 1,
#                          ifelse(LC_sev$Severity == "Low", 2,
#                                 ifelse(LC_sev$Severity == "Moderate", 3,
#                                        ifelse(LC_sev$Severity == "High", 4, NA))))
#
## Now remove any places with no data from LC Severity
#LC_sev <- LC_sev %>% 
#  filter(!Severity == "No data") %>%
#  relocate(GRIDCODE, Severity)
#
## Now bind the two files together
#sev <- rbind(TR_sev %>% select(GRIDCODE, Severity), LC_sev)
#
## View the severity files
#mapview(sev)
#
## Save the file
#st_write(sev,
#         dsn = "C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles",
#         layer = "SA_BurnSeverity.shp",
#         driver = "ESRI Shapefile",
#         append = F)

## Set working directory
#setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles")
#
## Bring in the file
#sev <- st_read("SA_BurnSeverity.shp")
#
## Create a blank raster
#r <- rast(crs = "epsg:26913", resolution = c(10,10), extent = ext(sev))
#
## Now rasterize the severity file
#sev_rast <- rasterize(vect(sev), r, field = "GRIDCODE")
#
## Now save that raster
#writeRaster(sev_rast,
#            "SA_BurnSeverity_NAD83_13N.tif",
#            gdal = "COMPRESS=DEFLATE",
#            overwrite = T)

# Set working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles")

# Bring in the file
sev <- rast("SA_BurnSeverity_NAD83_13N.tif")

#---------------------------------------------------#
# Extract mean values to the buffer and the line ####
#---------------------------------------------------#

# --- Tree Canopy --- #
# Extract tcc data to buffer
lines_buff$tcc <- extract(tcc,
                          vect(lines_buff),
                          fun = mean) %>% dplyr::select(-ID) %>% pull()

## Extract tcc data to the line
#lines$tcc <- extract(tcc,
#                     vect(lines), 
#                     fun = mean) %>% dplyr::select(-ID) %>% pull()

# --- Burn Severity --- #
# Extract severity to buffer
lines_buff$sev <- extract(sev,
                          vect(lines_buff),
                          fun = mean,
                          na.rm = T) %>% dplyr::select(-ID) %>% pull()

## Extract severity to buffer
#lines$sev <- extract(sev,
#                     vect(lines),
#                     fun = mean,
#                     na.rm = T) %>% dplyr::select(-ID) %>% pull()

# --- Stack --- #
# Extract mean for the buffer
vars_buff <- extract(
  stack, # Raster layer to extract from
  vect(lines_buff), # SpatVector of the buffer
  fun = mean # Make a mean value
)
head(vars_buff)

# Join those back to the buffer layer
lines_buff <- cbind(
  lines_buff, # Original buffer
  vars_buff # The variables that were extracted
) %>%
  dplyr::select(-ID) # Remove the ID column
head(lines_buff)

# Change column names to buffer
colnames(lines_buff)[2:(ncol(lines_buff) - 1)] <-
  paste0(colnames(lines_buff)[2:(ncol(lines_buff) - 1)], "_mean")

#--------------------------------#
# Extract standard deviations ####
#--------------------------------#

# --- Tree Canopy --- #
# Extract tcc data to buffer
lines_buff$tcc <- extract(tcc,
                          vect(lines_buff),
                          fun = sd) %>% dplyr::select(-ID) %>% pull()

# --- Burn Severity --- #
# Extract severity to buffer
lines_buff$sev <- extract(sev,
                          vect(lines_buff),
                          fun = sd,
                          na.rm = T) %>% dplyr::select(-ID) %>% pull()

# --- Stack --- #
# Extract mean for the buffer
vars_buff <- extract(
  stack, # Raster layer to extract from
  vect(lines_buff), # SpatVector of the buffer
  fun = sd # Make a sd value
)
head(vars_buff)

# Join those back to the buffer layer
lines_buff <- cbind(
  lines_buff, # Original buffer
  vars_buff # The variables that were extracted
) %>%
  dplyr::select(-ID) # Remove the ID column
head(lines_buff)

# Change column names to buffer
colnames(lines_buff)[15:(ncol(lines_buff) - 1)] <-
  paste0(colnames(lines_buff)[15:(ncol(lines_buff) - 1)], "_sd")

## Extract mean for the line
#vars_line <- extract(
#  stack, # Raster layer to extract from
#  vect(lines), # SpatVector of the lines
#  fun = mean # Create a mean for the line
#)
#
## Join that to the lines
#lines <- cbind(
#  lines, # Original lines
#  vars_line # Variables extracted
#) %>%
#  dplyr::select(-ID) # Remove the ID column
#head(lines)

## Change column names to lines
#colnames(lines)[2:(ncol(lines) - 1)] <-
#  paste0(colnames(lines)[2:(ncol(lines) - 1)], "_line")


# Remove their geometry
#lines <- st_drop_geometry(lines)
lines_buff <- st_drop_geometry(lines_buff)

# Join the two files together
vars <- lines_buff
head(vars)

# Set working directory to write the file
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/2022 Field Season/Macrohabitat_Data")

# Write out the final file
write_csv(
  vars,
  file = paste0("Macro_Covariates_", Sys.Date(), ".csv"),
)

