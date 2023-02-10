# SNODAS Processing 
# Colton Padilla 
# 10/12/2022

#-----------------#
# Housekeeping ####
#-----------------#

# Set working directory 
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/SNODAS/LiquidPrecip/SNODAS/SNODAS_LiquidPrecipitation")

# Load packages
library(tidyverse)
library(terra)

# Now list all the files 
files <- list.files(".", pattern = ".tif$")

# Now create the raster
snodas <- rast(files)

# Get only numbers in the names
names(snodas) <- gsub("[^[:digit:]]", "", names(snodas)) %>%
  as.Date(., format = "%Y%m%d") %>% format.Date(., "%Y%m%d")

# Get unique dates
dates <- unique(names(snodas))

#------------------------#
# Weekly Precip Prior ####
#------------------------#

# Set the writing directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/Data_For_Callie")

# Create an empty raster
week <- rast(extent = ext(snodas), 
             resolution = res(snodas), 
             nlyrs = nlyr(snodas))

# Now loop through and calculate the previous weeks precip
for(i in 1:length(dates)){
  
  # Get set of dates for raster subset
  date <- as.Date(dates[i], format = "%Y%m%d")
  date_sub <- seq(date - 6, date, "days") %>% format.Date(., "%Y%m%d")
  
  # Subset the raster
  rast <- subset(snodas, which(names(snodas) %in% date_sub, arr.ind = T))
  
  # Calculate a sum of precip
  week[[i]] <- sum(rast)
  
  # Set the names at the end and save the raster
  if(i == length(dates)){
    # Names
    names(week) <- dates
    
    # Write the raster
    writeRaster(week,
                filename = "Weekly_Precip.tif",
                gdal = "COMPRESS=DEFLATE",
                overwrite = T)
  }
}


#---------------------------#
# Bi-Weekly Precip Prior ####
#---------------------------#

# Create an empty raster
biweek <- rast(extent = ext(snodas), 
               resolution = res(snodas), 
               nlyrs = nlyr(snodas))

# Now loop through and calculate the previous weeks precip
for(i in 1:length(dates)){
  
  # Get set of dates for raster subset
  date <- as.Date(dates[i], format = "%Y%m%d")
  date_sub <- seq(date - 13, date, "days") %>% format.Date(., "%Y%m%d")
  
  # Subset the raster
  rast <- subset(snodas, which(names(snodas) %in% date_sub, arr.ind = T))
  
  # Calculate a sum of precip
  biweek[[i]] <- sum(rast)
  
  # Set the names at the end and save the raster
  if(i == length(dates)){
    # Names
    names(biweek) <- dates
    
    # Write the raster
    writeRaster(biweek,
                filename = "Bi-Weekly_Precip.tif",
                gdal = "COMPRESS=DEFLATE",
                overwrite = T)
  }
}

#-------------------------#
# Monthly Precip Prior ####
#-------------------------#

# Create an empty raster
month <- rast(extent = ext(snodas), 
              resolution = res(snodas), 
              nlyrs = nlyr(snodas))

# Now loop through and calculate the previous weeks precip
for(i in 1:length(dates)){
  
  # Get set of dates for raster subset
  date <- as.Date(dates[i], format = "%Y%m%d")
  date_sub <- seq(date - 27, date, "days") %>% format.Date(., "%Y%m%d")
  
  # Subset the raster
  rast <- subset(snodas, which(names(snodas) %in% date_sub, arr.ind = T))
  
  # Calculate a sum of precip
  month[[i]] <- sum(rast)
  
  # Set the names at the end and save the raster
  if(i == length(dates)){
    # Names
    names(month) <- dates
    
    # Write the raster
    writeRaster(month,
                filename = "Monthly_Precip.tif",
                gdal = "COMPRESS=DEFLATE",
                overwrite = T)
  }
}

#--------------------#
# Cumulative Sums ####
#--------------------#

# Create cumulative sums
cumsum <- app(snodas, cumsum)

# Write Rasters
writeRaster(cumsum,
            filename = "CumSum_Precip.tif",
            gdal = "COMPRESS=DEFLATE",
            overwrite = T)

