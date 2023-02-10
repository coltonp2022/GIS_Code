# Calculating PRISM and SNODAS metrics
# Colton Padilla
# 10/5/2022

# Packages
library(tidyverse)
library(terra)
library(lubridate)

#--------------------------------#
# SNODAS Liquid Precipitation ####
#--------------------------------#

# --- CumSum --- #

# Set working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/SNODAS/LiquidPrecip/SNODAS/SNODAS_LiquidPrecipitation")

# List all the files
files <- list.files(path = ".",
                    pattern = ".tif")

# Remove the xml files
files <- files[!grepl(".xml", files)]

# Now read these files into a raster stack
snodas <- rast(files)

# Now calculate cumulative sums
cumsums <- app(snodas, cumsum)

# Now get only the numbers from the names 
names(cumsums) <- gsub("[^[:digit:]]", "", names(cumsums))

# Now add Cumsum to that
names(cumsums) <- paste0("Liquid_", names(cumsums), "_CumSum")

# Create a cumsum folder
if(!dir.exists("./Metrics")){
  dir.create("./Metrics")
}

# Now save the cumulative sums 
writeRaster(cumsums,
            filename = "./Metrics/Liquid_CumSum.tif",
            gdal = "COMPRESS = DEFLATE",
            overwrite = T)

# --- Weekly --- #

# Make a new raster stack
weekly <- snodas 
names(weekly) <- gsub("[^[:digit:]]", "", names(weekly)) %>%
  as.Date(., tryFormats = "%Y%m%d") %>%
  week()

# Get a vector of unique weeks
weeks <- unique(names(weekly))
weeks <- paste0("^", weeks, "$")
  
# Run the loop
for(i in 1:length(weeks)){
  # Subset the raster by each week
  rast <- subset(weekly, grep(weeks[i],
                              names(weekly),
                              value = T))
  
  # Now fill a layer with the weekly sum
  if(i == 1) weekly_sum <- sum(rast)
  else weekly_sum <- c(weekly_sum, sum(rast))
  
  # Set the names at the end
  if(i == length(weeks)){
    # Set names
    names(weekly_sum) <- unique(names(weekly))
    
    # Write the raster
    writeRaster(weekly_sum,
                filename = "./Metrics/Liquid_Weekly.tif",
                gdal = "COMPRESS = DEFLATE",
                overwrite = T)
  } 
}

# --- Monthly --- #

# Make a new raster stack
monthly <- snodas 
names(monthly) <- gsub("[^[:digit:]]", "", names(monthly)) %>%
  as.Date(., tryFormats = "%Y%m%d") %>%
  month()

# Get a vector of unique weeks
months <- unique(names(monthly))
months <- paste0("^", months, "$")

# Run the loop
for(i in 1:length(months)){
  # Subset the raster by each month
  rast <- subset(monthly, grep(months[i],
                               names(monthly),
                               value = T))
  
  # Now fill a layer with the monthly sum
  if(i == 1) monthly_sum <- sum(rast)
  else monthly_sum <- c(monthly_sum, sum(rast))
  
  # Set the names at the end
  if(i == length(months)) {
    # Set names
    names(monthly_sum) <- unique(names(monthly))
    
    # Save the raster
    writeRaster(monthly_sum,
                filename = "./Metrics/Liquid_Monthly.tif",
                gdal = "COMPRESS = DEFLATE",
                overwrite = T)
  } 
}

#---------------#
# SNODAS SWE ####
#---------------#

# --- CumSum --- #

# Set working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/SNODAS/SWE/SNODAS/SNODAS_SWE")

# List all the files
files <- list.files(path = ".",
                    pattern = ".tif")

# Remove the xml files
files <- files[!grepl(".xml", files)]

# Now read these files into a raster stack
snodas <- rast(files)

# Now calculate cumulative sums
cumsums <- app(snodas, cumsum)

# Now get only the numbers from the names 
names(cumsums) <- gsub("[^[:digit:]]", "", names(cumsums))

# Now add Cumsum to that
names(cumsums) <- paste0("Liquid_", names(cumsums), "_CumSum")

# Create a cumsum folder
if(!dir.exists("./Metrics")){
  dir.create("./Metrics")
}

# Now save the cumulative sums 
writeRaster(cumsums,
            filename = "./Metrics/SWE_CumSum.tif",
            gdal = "COMPRESS = DEFLATE",
            overwrite = T)

# --- Weekly --- #

# Make a new raster stack
weekly <- snodas 
names(weekly) <- gsub("[^[:digit:]]", "", names(weekly)) %>%
  as.Date(., tryFormats = "%Y%m%d") %>%
  week()

# Get a vector of unique weeks
weeks <- unique(names(weekly))
weeks <- paste0("^", weeks, "$")

# Create an empty raster
weekly_sum <- rast(extent = ext(weekly), resolution = res(weekly))

# Run the loop
for(i in 1:length(weeks)){
  # Subset the raster by each week
  rast <- subset(weekly, grep(weeks[i],
                              names(weekly),
                              value = T))
  
  # Now fill a layer with the weekly sum
  weekly_sum <- c(weekly_sum, sum(rast))
  
  # Set the names at the end
  if(i == length(weeks)){
    # Set names
    names(weekly_sum) <- unique(names(weekly))
    
    # Write the raster
    writeRaster(weekly_sum,
                filename = "./Metrics/SWE_Weekly.tif",
                gdal = "COMPRESS = DEFLATE",
                overwrite = T)
  } 
}

# --- Monthly --- #

# Make a new raster stack
monthly <- snodas 
names(monthly) <- gsub("[^[:digit:]]", "", names(monthly)) %>%
  as.Date(., tryFormats = "%Y%m%d") %>%
  month()

# Get a vector of unique weeks
months <- unique(names(monthly))
months <- paste0("^", months, "$")

# Create an empty raster
monthly_sum <- rast(extent = ext(monthly), resolution = res(monthly))

# Run the loop
for(i in 1:length(months)){
  # Subset the raster by each month
  rast <- subset(monthly, grep(months[i],
                               names(monthly),
                               value = T))
  
  # Now fill a layer with the monthly sum
  if(i == 1) monthly_sum <- sum(rast)
  else monthly_sum <- c(monthly_sum, sum(rast))
  
  # Set the names at the end
  if(i == length(months)) {
    # Set names
    names(monthly_sum) <- unique(names(monthly))
    
    # Save the raster
    writeRaster(weekly_sum,
                filename = "./Metrics/SWE_Monthly.tif",
                gdal = "COMPRESS = DEFLATE",
                overwrite = T)
  } 
}

#--------------------------#
# PRISM Max Temperature ####
#--------------------------#

# Set working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/PRISM/PRISM/PRISM_tmax")

# Load in Max Temperature data
files <- list.files(path = ".",
                    pattern = ".tif$")

# Now create a raster
tmax <- rast(files)

# Now change the names 
names(tmax) <- gsub("[^_[:digit:]]", "", names(tmax)) %>%
  gsub("___42_", "", .) %>%
  as.Date(., tryFormats = "%Y%m%d")
names(tmax) # Check

# Now sort the raster by date
tmax <- tmax[[order(names(tmax))]]

# --- Weekly Averages --- #

# Create a week raster and change the names
weekly_avg <- tmax
names(weekly_avg) <- week(names(weekly_avg))

# Create a weeks vector
weeks <- unique(names(weekly_avg)) %>%
  paste0("^", ., "$")

# Now for loop to select and create weekly average
for(i in 1:length(weeks)){
  
  # Get each week
  rast <- subset(weekly_avg, grep(weeks[i],
                            names(weekly_avg),
                            value = T))
  
  # Now calculate a mean
  if(i == 1) weekly <- mean(rast)
  else weekly <- c(weekly, mean(rast))
  
  # Now write the raster
  if(i == length(weeks)){
    # Set the names
    names(weekly) <- unique(names(weekly_avg))
    
    # Set a new directory
    setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/PRISM/PRISM")
    if(!dir.exists("./Metrics")){
      dir.create("./Metrics")
    }
    
    # Write the raster
    writeRaster(weekly,
                filename = "./Metrics/Weekly_Tmax.tif",
                gdal = "COMPRESS=DEFLATE",
                overwrite = T)
  }
}

# --- Monthly Average --- #

# Create a new raster
month <- tmax

# Reset the names
names(month) <- month(names(tmax))

# Get a month vector
months <- unique(names(month)) %>%
  paste0("^", ., "$")

# Now for loop to select and create monthly average
for(i in 1:length(months)){
  
  # Get each week
  rast <- subset(month, grep(months[i],
                                  names(month),
                                  value = T))
  
  # Now calculate a mean
  if(i == 1) monthly_avg <- mean(rast)
  else monthly_avg <- c(monthly_avg, mean(rast))
  
  # Now write the raster
  if(i == length(months)){
    # Set the names
    names(monthly_avg) <- unique(names(month))
    
    # Write the raster
    writeRaster(monthly_avg,
                filename = "./Metrics/Monthly_Tmax.tif",
                gdal = "COMPRESS=DEFLATE",
                overwrite = T)
  }
}

#------------------------------#
# PRISM Minimum Temperature ####
#------------------------------#

# Set working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/PRISM/PRISM/PRISM_tmin")

# Load in Max Temperature data
files <- list.files(path = ".",
                    pattern = ".tif$")

# Now create a raster
tmin <- rast(files)

# Now change the names 
names(tmin) <- gsub("[^_[:digit:]]", "", names(tmin)) %>%
  gsub("___42_", "", .) %>%
  as.Date(., tryFormats = "%Y%m%d")
names(tmin) # Check

# Now sort the raster by date
tmin <- tmin[[order(names(tmin))]]

# --- Weekly Averages --- #

# Create a week raster and change the names
weekly_avg <- tmin
names(weekly_avg) <- week(names(weekly_avg))

# Create a weeks vector
weeks <- unique(names(weekly_avg)) %>%
  paste0("^", ., "$")

# Now for loop to select and create weekly average
for(i in 1:length(weeks)){
  
  # Get each week
  rast <- subset(weekly_avg, grep(weeks[i],
                                  names(weekly_avg),
                                  value = T))
  
  # Now calculate a mean
  if(i == 1) weekly <- mean(rast)
  else weekly <- c(weekly, mean(rast))
  
  # Now write the raster
  if(i == length(weeks)){
    # Set the names
    names(weekly) <- unique(names(weekly_avg))
    
    # Set a new directory
    setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/PRISM/PRISM")
    if(!dir.exists("./Metrics")){
      dir.create("./Metrics")
    }
    
    # Write the raster
    writeRaster(weekly,
                filename = "./Metrics/Weekly_Tmin.tif",
                gdal = "COMPRESS=DEFLATE",
                overwrite = T)
  }
}

# --- Monthly Average --- #

# Create a new raster
month <- tmin

# Reset the names
names(month) <- month(names(tmax))

# Get a month vector
months <- unique(names(month)) %>%
  paste0("^", ., "$")

# Now for loop to select and create monthly average
for(i in 1:length(months)){
  
  # Get each week
  rast <- subset(month, grep(months[i],
                             names(month),
                             value = T))
  
  # Now calculate a mean
  if(i == 1) monthly_avg <- mean(rast)
  else monthly_avg <- c(monthly_avg, mean(rast))
  
  # Now write the raster
  if(i == length(months)){
    # Set the names
    names(monthly_avg) <- unique(names(month))
    
    # Write the raster
    writeRaster(monthly_avg,
                filename = "./Metrics/Monthly_Tmin.tif",
                gdal = "COMPRESS=DEFLATE",
                overwrite = T)
  }
}

#---------------------------#
# PRISM Mean Temperature ####
#---------------------------#

# Set working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/PRISM/PRISM/PRISM_tmean")

# Load in Max Temperature data
files <- list.files(path = ".",
                    pattern = ".tif$")

# Now create a raster
tmean <- rast(files)

# Now change the names 
names(tmean) <- gsub("[^_[:digit:]]", "", names(tmean)) %>%
  gsub("___42_", "", .) %>%
  as.Date(., tryFormats = "%Y%m%d")
names(tmean) # Check

# Now sort the raster by date
tmean <- tmean[[order(names(tmean))]]

# --- Weekly Averages --- #

# Create a week raster and change the names
weekly_avg <- tmean
names(weekly_avg) <- week(names(weekly_avg))

# Create a weeks vector
weeks <- unique(names(weekly_avg)) %>%
  paste0("^", ., "$")

# Now for loop to select and create weekly average
for(i in 1:length(weeks)){
  
  # Get each week
  rast <- subset(weekly_avg, grep(weeks[i],
                                  names(weekly_avg),
                                  value = T))
  
  # Now calculate a mean
  if(i == 1) weekly <- mean(rast)
  else weekly <- c(weekly, mean(rast))
  
  # Now write the raster
  if(i == length(weeks)){
    # Set the names
    names(weekly) <- unique(names(weekly_avg))
    
    # Set a new directory
    setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/PRISM/PRISM")
    if(!dir.exists("./Metrics")){
      dir.create("./Metrics")
    }
    
    # Write the raster
    writeRaster(weekly,
                filename = "./Metrics/Weekly_Tmean.tif",
                gdal = "COMPRESS=DEFLATE",
                overwrite = T)
  }
}

# --- Monthly Average --- #

# Create a new raster
month <- tmean

# Reset the names
names(month) <- month(names(tmean))

# Get a month vector
months <- unique(names(month)) %>%
  paste0("^", ., "$")

# Now for loop to select and create monthly average
for(i in 1:length(months)){
  
  # Get each week
  rast <- subset(month, grep(months[i],
                             names(month),
                             value = T))
  
  # Now calculate a mean
  if(i == 1) monthly_avg <- mean(rast)
  else monthly_avg <- c(monthly_avg, mean(rast))
  
  # Now write the raster
  if(i == length(months)){
    # Set the names
    names(monthly_avg) <- unique(names(month))
    
    # Write the raster
    writeRaster(monthly_avg,
                filename = "./Metrics/Monthly_Tmean.tif",
                gdal = "COMPRESS=DEFLATE",
                overwrite = T)
  }
}