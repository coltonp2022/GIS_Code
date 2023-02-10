# Coyote Species Distribution Model Using Fecal Sampling Data
# Colton Padilla
# 5/25/2022

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# All GIS data was moved to a 100 meter resolution from a 10 meter resolution
# within this code.
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

rm(list = ls())

#--------------------#
# Loading in Data ####
#--------------------#

# Set working directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/Transect_Data")

# Load Packages
library(sf)
library(tidyverse)
library(terra)
library(mapview)
library(spatialEco)
library(raster)
library(car)

# Load the data
coyote <- read_csv("Coyote_Sampling_Data_Clean.csv") %>%
  dplyr::select(x, y)

# Create a presence column
coyote$pres <- 1

# Now create a spatial data frame
coyote_sf <- st_as_sf(
  coyote, # DF to make sf
  coords = c("x", "y"), # Coordinate columns
  crs = 4326) %>% # Original crs
  st_transform(., crs = 26913) # Transformed CRS
st_crs(coyote_sf) # Check projection

# View the data
mapview(coyote_sf)

#--------------------------------------------#
# Create the bounding box and the fishnet ####
#--------------------------------------------#

# Buffer the points
coyote_buff <- st_union(
  st_buffer(
    coyote_sf, 
    dist = 5000)) # Buffer by 5 km

# View the bufer
mapview(coyote_sf) + mapview(coyote_buff)

# Buffer the points by 100 meters to remove that available area
coyote_buff_100 <- st_union(
  st_buffer(
    coyote_sf,
    dist = 100
  )
)


#-----------------------------------------------------#
# Sample Available Points based on a 10 to 1 ratio ####
#-----------------------------------------------------#

# Cut the 100 meter buffer out of the full buffer
# This mitigates spatial autocorrelation of available and used points
# This was my own idea and at the time, I had no paper backing saying to do 
# it
coyote_buff_final <- st_difference(coyote_buff, coyote_buff_100)
mapview(coyote_buff_final)

# Now sample available points within the final polygon
set.seed(12)
coyote_sample <- st_as_sf(st_sample(
  coyote_buff_final, 
  size = 10 * nrow(coyote_sf)
))

# Get the coordinates for the sf
coyote_avail_coord <- st_coordinates(coyote_sample)

# Add in the coordinates and presence column
coyote_sample <- cbind(coyote_sample, coyote_avail_coord)
coyote_sample$pres <- 0

# Change this to a data frame
coyote_sample <- st_drop_geometry(coyote_sample) %>%
  relocate(pres)

# Get original data frames coordinates
coyote_used_coord <- st_coordinates(coyote_sf)

# Add in coordinates and presence column
coyote_sf <- cbind(coyote_sf, coyote_used_coord)

# Change it to a data frame
coyote_sf <- st_drop_geometry(coyote_sf)

# Bind the two together and make them an sf
coyote_sf <- rbind(coyote_sf, coyote_sample) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 26913)

# View the output data frame
mapview(coyote_sf, zcol = "pres")

#---------------------------------------#
# Load in raster data for extraction ####
#---------------------------------------#

# Load the stack
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/Thesis")
stack <- rast("SPP_DIST_STACK.tif")

# Extract the values to the raster
x <- terra::extract(stack, vect(coyote_sf)) 

# Join the values to the dataframe
coyote_sf <- cbind(coyote_sf, x) %>% dplyr::select(-ID)

## Join the true landcover to the data
##LC_test <- read_csv("LC_legend.csv") %>%
##  rename(lc = num)
##
##legend <- LC_test %>% dplyr::select(lc, categorynew) %>%
##  group_by(lc) %>%
##  summarise(landcov = categorynew) %>%
##  unique()
##
##coyote_sf <- left_join(coyote_sf, legend, by = "lc")
##  
#coyote_sf <- coyote_sf %>% dplyr::select(-lc)

# Creating dummy variables
coyote_sf$lc <- as.character(coyote_sf$lc)
lc <- unique(coyote_sf$lc)
for(i in 1:length(lc)){
  coyote_sf[, ncol(coyote_sf) + 1] <- ifelse(coyote_sf$lc == lc[i], 1, 0)
  names(coyote_sf)[ncol(coyote_sf)] <- lc[i]
}

#----------------------------------#
# Now Exploratory Data analysis ####
#----------------------------------#
# Make presence a factor
coyote_sf$pres <- as.factor(coyote_sf$pres)

# Plotting quantitative variable distributions
vars_quant <- colnames(coyote_sf)[c(2:9,11)]
for(i in 1:length(vars_quant)){
  print(ggplot(coyote_sf, aes_string(x = vars_quant[i], fill = "pres")) +
          geom_density(alpha = 0.4))
}

# Plotting categorical variables
vars_cat <- colnames(coyote_sf)[13:ncol(coyote_sf)]
for(i in 1:length(vars_cat)){
  print(ggplot(coyote_sf, aes_string(x = vars_cat[i], fill = "pres")) +
          geom_histogram(stat = "count"))
}

#---------------#
# GLM Option ####
#---------------#

library(lme4)
library(glm2)
library(glmmTMB)
library(MASS)

# Create coyote_df
coyote_df <- st_drop_geometry(coyote_sf)
#coyote_df <- coyote_df %>% drop_na()
colnames(coyote_df)
str(coyote_df)

# Pull the rasters
for(i in 1:length(names(stack))){
  df <- stack[[i]]
  name <- names(stack)[i]
  assign(name, df)
}

# Full Model
full <- glm(
  pres ~ elev + slope + asp + trasp + vrm + tpi + tri + hli + tcc + I(elev ^ 2),
  family = binomial,
  data = coyote_df,
  na.action = "na.omit"
)
summary(full) # 1104.6

# Null Model
null <- glm(
  pres ~ 1,
  family = binomial,
  data = coyote_df,
  na.action = "na.omit")
summary(null) #1228.3


# Start stepwise regression
addterm(null, full, test = "Chisq", sorted = T) # VRM

m1 <- glm(
  pres ~ tri,
  family = binomial,
  data = coyote_df,
  na.action = "na.omit"
)
summary(m1) # 1165.7

# Keep going
addterm(m1, full, test = "Chisq", sorted = T) # VRM

m2 <- glm(
  pres ~ tri,
  family = binomial,
  data = coyote_df,
  na.action = "na.omit"
)
vif(glm( pres ~ vrm + trasp,
         family = binomial,
         data = coyote_df,
         na.action = "na.omit"))
summary(m2) # 1128.3


# Keep going
addterm(m2, full, test = "Chisq", sorted = T) # VRM

m3 <- glm(
  pres ~ vrm + trasp + tpi,
  family = binomial,
  data = coyote_df,
  na.action = "na.omit"
)
vif(glm( pres ~ vrm + trasp + lc,
         family = binomial,
         data = coyote_df,
         na.action = "na.omit"))
summary(m3) # 1107.3

# Keep going
addterm(m3, full, test = "Chisq", sorted = T) # VRM

m4 <- glm(
  pres ~ vrm + trasp + tpi + elev,
  family = binomial,
  data = coyote_df,
  na.action = "na.omit"
)
vif(glm( pres ~ vrm + trasp + lc + tpi,
         family = binomial,
         data = coyote_df,
         na.action = "na.omit"))
summary(m4) # 1095.6

# Keep going
addterm(m4, full, test = "Chisq", sorted = T) # VRM

m5 <- glm2(
  pres ~ vrm + trasp  + tpi + I(elev ^2) + tcc,
  family = binomial,
  data = coyote_df
)
vif(glm( pres ~ vrm + trasp  + tpi + elev + I(elev^2),
         family = binomial,
         data = coyote_df))
summary(m5) # 1092.4


# Top model here 

pred_stack <- rast(list(tri))

pred <- terra::predict(pred_stack, m1, type = "response")

# Accuracy of the model
library(PresenceAbsence)
library(DAAG)

full.pred <- predict(m1, type = "response"); head(full.pred)
modl <- "full"
dat <- data.frame(cbind(modl, coyote_df[,1], full.pred))
dat$full.pred <- as.numeric(dat$full.pred)
dat$V2 <- ifelse(dat$V2 == 2, 1, 0)
mod.cut <- optimal.thresholds(dat, opt.methods = c("ObsPrev"))

# Building confusion matrix
mod2.cfmat <- table(dat[[2]], factor(as.numeric(dat$full.pred >= mod.cut$full.pred)))
mod2.cfmat

# Model accuracy
mod2.acc <- presence.absence.accuracy(dat, threshold = mod.cut$full.pred)
mod2.acc$tss <- mod2.acc$sensitivity + mod2.acc$specificity - 1
mod2.acc[,c(1, 4:5, 7, 13)]

# AUC plot
auc.roc.plot(dat, color = T)
