# Calculation of Macrohabitat Terrain Variables
# Colton Padilla
# 4/14/2022

# Load packages
library(terra)
library(sf)
library(tidyverse)
library(mapview)
library(spatialEco)
library(raster)
library(beepr)

# Create NAD83 projection

## Load study area for plotting
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Proposal_Maps/Study_Area_Boundary")
SA <- st_read("possible_study_boundary.shp")
SA <- st_transform(SA, crs = "EPSG:26913")
st_crs(SA) # Check
SA <- st_zm(SA) # Drop Z dimension
SA <- st_buffer(SA, 500)

# Final working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Data_and_Initial_Maps/ElevationData")

#---------------------------- Load in the DEM  --------------------------------#
## These DEMs come from the USGS website and are 10 meter resolution.
#
#dem_n <- rast("USGS_13_n37w107_20211208 (1).tif") # Northern
#dem_s <- rast("USGS_13_n36w107_20210630 (1).tif") # Southern
#
## Merge the two DEMs together
#dem <- merge(dem_n, dem_s)
#dem <- project(dem, "EPSG:26913") # Project to NAD83 Zone 13N
#beep(sound = 5)
#
#plot(dem) # View
#plot(st_geometry(SA), add = T)
#
## Crop and mask the dem to the study area
#sa_dem <- crop(dem, vect(SA))
##sa_dem <- mask(sa_dem, vect(SA))
#
## NOTE:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
## Think about resampling this raster to different resolution.
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#
#plot(sa_dem) # View
#res(sa_dem) # Check the resolution
#
# Set new working directory to write everything
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles")

## Write the dem to a file for later
#writeRaster(sa_dem, filename = "sa_dem_NAD83_13N.tif", overwrite = T)

sa_dem_t <- rast("sa_dem_NAD83_13N.tif")
sa_dem <- raster(sa_dem_t)

#------------------------------- Calculations ---------------------------------#
#--- Calculate slope raster
sa_slope_t <- terra::terrain(sa_dem_t, v = "slope", unit = "degrees")
plot(sa_slope_t) # View

# Write
writeRaster(sa_slope_t, filename = "sa_slope_NAD83_13N.tif",
            overwrite = T)

#--- Calculate aspect
sa_asp_t <- terra::terrain(sa_dem_t, v = "aspect", unit = "degrees")
plot(sa_asp_t) # View

# Write
writeRaster(sa_asp_t, filename = "sa_aspect_NAD83_13N.tif",
            overwrite = T)

#--- Trasp
trasp_fun <- function(x){ (1 - cos( (3.142/180) * (x - 30))) / 2}
sa_trasp_t <- terra::app(sa_asp_t, fun = trasp_fun)
plot(sa_trasp_t)

# Write
terra::writeRaster(sa_trasp_t, filename = "sa_trasp_NAD83_13N.tif",
            overwrite = T)

#--- VRM 
sa_vrm <- vrm(sa_dem, s = 3)
mapview(sa_vrm)

# Write
writeRaster(sa_vrm, filename = "sa_vrm_NAD83_13N.tif",
            overwrite = T)

#--- Hillshade
sa_hill_t <- terra::shade(sa_slope_t, sa_asp_t, angle = 45)
plot(sa_hill_t) # View

#Write
writeRaster(sa_hill_t, filename = "sa_hillshade_NAD83_13N.tif",
            overwrite = T)

#--- Heat load index
sa_HLI <- hli(sa_dem, check = T)
plot(sa_HLI)

# Write
raster::writeRaster(sa_HLI, filename = "sa_hli_NAD83_13N.tif",
                    overwrite = T)

#--- Terrain Position Index
sa_tpi <- terra::terrain(sa_dem_t, "TPI")
mapview(raster(sa_tpi))
plot(sa_tpi)

sa_tpi <- sa_tpi * 100

sa_tpi <- focal(sa_dem_t, fun = function(x, ...) x[5] - mean(x[-5]))

writeRaster(sa_tpi, filename = "sa_tpi_NAD83_13N.tif",
            overwrite = T)

#-- TWI (USE THE R TWI RASTER)

rm(sa_HLI, sa_slope_t, sa_vrm, sa_asp_t, sa_hill_t, sa_trasp_t)

# Create upslope area function
                # This function was lifted from stack exchange and is not my own work.
upslope <- function (dem, log = TRUE, atb = FALSE, deg = 0.12, fill.sinks = TRUE) 
{
  if (!all.equal(xres(dem), yres(dem))) {
    stop("Raster has differing x and y cell resolutions. Check that it is in a projected coordinate system (e.g. UTM) and use raster::projectRaster to reproject to one if not. Otherwise consider using raster::resample")
  }
  if (fill.sinks) {
    capture.output(dem <- invisible(raster::setValues(dem, topmodel::sinkfill(raster::as.matrix(dem), res = xres(dem), degree = deg))))
  }
  topidx <- topmodel::topidx(raster::as.matrix(dem), res = xres(dem))
  a <- raster::setValues(dem, topidx$area)
  if (log) {
    a <- log(a)
  }
  if (atb) {
    atb <- raster::setValues(dem, topidx$atb)
    a <- addLayer(a, atb)
    names(a) <- c("a", "atb")
  }
  return(a)
}

# Use the create layers function to create a stacked DEM for TWI
create_layers <- function (dem, fill.sinks = TRUE, deg = 0.1) 
{
  layers <- stack(dem)
  message("Building upslope areas...")
  a.atb <- upslope(dem, atb = TRUE, fill.sinks = fill.sinks, deg = deg)
  layers <- addLayer(layers, a.atb)
  names(layers) <- c("filled.elevations", "upslope.area", "twi")
  return(layers)
}

# Use the create layers function on your DEM.
twi_stack <- create_layers(sa_dem)

# Pull out the 3rd object 
twi_comp <- twi_stack[[3]] # Topographic Wetness Index

# Write the raster
writeRaster(twi_comp,
            "sa_TWI_R_NAD83_13N.tif",
            overwrite = T)
beep(sound = 5)

#----------------------#
# Tree Canopy Cover ####
#----------------------#

# Set the working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Data_and_Initial_Maps/Tree_Canopy_Cover")

# Load in the raster
tcc <- rast("TCC_National.tif")

# Set the SA to that raster crs
SA_tcc <- st_transform(SA, crs(tcc))

# Now crop tcc
tcc <- crop(tcc, SA_tcc)

# Now reproject and crop again
tcc <- project(tcc, "epsg:26913")
tcc <- crop(tcc, SA)
#tcc <- mask(tcc, vect(SA))

# View to check
mapview(raster(tcc)) + mapview(SA)

# Reset the raster values to the SA
tcc <- tcc / max(values(tcc, na.rm = T)) * 100
range(values(tcc, na.rm = T))
  
# Save the output Raster
writeRaster(tcc,
            "TCC_SA_26913.tif",
            gdal = "COMPRESS=DEFLATE",
            overwrite = T)

#------------------#
# Burn Severity ####
#------------------#

# Setwd
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Data_and_Initial_Maps/Burn_Severity_Data/Burn_Severity")

# Bring in the burn severity files
sev <- st_read("BurnSeverity.shp")
st_crs(sev) # Check the crs

# Project
sev <- st_transform(sev, st_crs(SA))

# Now crop it 
sev <- st_crop(sev, SA)

# Now view it
mapview(sev)

# Now write it to the working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles")

# write
st_write(sev,
         dsn = ".",
         layer = "SA_BurnSeverity.shp",
         driver = "ESRI Shapefile")
