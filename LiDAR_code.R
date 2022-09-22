# Testing out Lidar code with lasCatalog
# Colton Padilla
# 5/23/2022

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# This code takes some serious computational power. Even with these extremely
# small lidar tiles, there is Gigabytes worth of outputs. 7.3 GB for the 
# normalize_height function off an extremely small area. Setting the amount of
# area to process may be the way around it.
  # This website gives documentation for the LAScatalog function and options.
  # https://cran.r-project.org/web/packages/lidR/vignettes/lidR-LAScatalog-class.html
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

# Set working Directory
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/Lidar_Files")
# Load Packages
library(tidyverse)
library(lidR)
library(tictoc)
library(raster)
library(terra)
library(mapview)
library(EBImage)
library(sf)

# Bring all the test files in as a catalog
#ctg <- readLAScatalog("path/to/las/files/")

ctg <- readLAScatalog(".") # "." signifies the current working directory.

# Verify the LAS catalog
  # This LAS check function looks for signs that the data has been cleaned or
  # normalized. It gives you a large checklist and shows you what has been done.
  # It will give you a much larget check list for an individual LAS.
las_check(ctg)

# Plot the geometry
  # When you plot an LAScatalog, it just shows you the outline of the area that
  # the catalog overlaps.
plot(ctg, mapview = T, map.type = "Esri.WorldImagery")

# Bring in all sites to see them on the lidar tiles map
setwd("C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/Thesis")

sites <- st_read("2022_trapping_sites.shp")


# Plot the elevation values aka Z values
  # This gives you minimum elevation values for each LAS. However, this data is
  # not normalized so it is not really an informative plot.
plot(ctg["Min.Z"])


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# Here we get into the <opt_()> functions. If you go to the lidar package, you
# can look into all the different functions that allow you to change the 
# settings of an LAScatalog. There are less functions than it looks like there 
# are.

# There is good documentation on LAScatalogs online from the creator of the LidR
# package. An LAScatalog is basically an R object that is a folder. So if you
# have a folder full of LAS objects, an LAScatalog is an R object of that folder.
# It does not load any of the LAS files in until you run a function on them. 
# When you do run a function, it only loads a chunk (more on this later) or one
# LAS at a time. So if you have 3 LAS files in one folder and you run a function,
# LidR brings in one LAS, runs the function on it, retains the output, then 
# removes that LAS and repeats the process with the next one. From my 
# understanding, the outputs are stitched together one by one as the function 
# runs. This helps with the memory issues that can come with the amount of data
# in point cloud data. 
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

# Now to use functions on the catalog setting this writes the output files prior
# to a function. You'll reset this prior to every single function depending on
# where you want to write files and where you want them written.
  # Use this notation: {ORIGINALFILENAME}_<yournamingscheme> to write one for 
  # each file
  # Scroll to the right to see how I named the output files for this.
  # I could have also created a new folder for the DTM LAS files. That 
  # probably would have been a better idea than what I did here.

opt_output_files(ctg) = "C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/Lidar_Files/Lidar_Output/{ORIGINALFILENAME}_dtm"

  # Alternative code to show the new folder for DTMs                                                                                    *                
  #opt_output_files(ctg) = "C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/Lidar_Files/DTM/{ORIGINALFILENAME}_dtm"

# Here this is telling the catalog to overwrite any files that have the same 
# name that LidR is trying to write. Sometimes the driver is SpatRaster and 
# sometimes it is raster.
ctg@output_options$drivers$Raster$param$overwrite <- TRUE

# Now lets do a digital terrain model
  # When you do this, you get a "pattern of chunks" plot that shows you how the 
  # data is being processed. It will update you on how it is processing.
  # The output gives you a raster with the entire area you are processing.
  # These chunks are basically the LAS files themselves or the size of chunks
  # that you set in opt_chunk_size(). More on that later.
dtm_output <- grid_terrain(ctg, res = 1, knnidw())
plot(dtm_output)
  # Alternative viewing
  #mapview(dtm_output)

# Now you can write the entire raster
writeRaster(dtm_output,
            filename = "./Lidar_Output/lidar_test_DTM.tif",
            overwrite = T)

#----------------------------------------------#
# Now to normalize the points to the ground ####
#----------------------------------------------#

# Once again set your output
opt_output_files(ctg) = 
  "C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/Lidar_Files/Lidar_Output/{ORIGINALFILENAME}_normal"
  # Alternative code for writing new files
  #opt_output_files(ctg) =                                                                                            *
  #  "C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/Lidar_Files/LAS_normalized/{ORIGINALFILENAME}_normal"

# Once again set the overwrite
  # I am not 100% sure whether or not you need to do this everytime.
  # I am thinking that you do not once you set it once.
ctg@output_options$drivers$Raster$param$overwrite <- TRUE

# Now normalize the points
  # Normalizing heights is the lidar points being standardized to the DTM.
  # This means that the dtm is the ground and the height of the points is then
  # calculated based on the ground level.
normlas <- normalize_height(ctg, knnidw())

# LAS Check
las_check(normlas)

# Now since we have a normalized data set we can remove the original and the 
# DTM since we do not need them. 
rm(ctg, dtm_output)

#-------------------------------------------#
# Now let's create a canopy height model ####
#-------------------------------------------#

# Now lets try calculating this by chunk sizes to make computation easier
  # Here the opt_chunk_size() function is telling LidR to pull in 500x500 m
  # chunks at a time. This helps with keeping R from running out of memory
  # while doing a computation.
opt_chunk_size(normlas) <- 500 # 500x500m chunks

# Now because we are chunking {ORIGINALFILENAME} will not work for the naming.
# Here I set it to do the farthest west x-coord and farthest south y-coord.
  # {XLEFT}_{YBOTTOM}
# Once again set your output
opt_output_files(normlas) = 
  "C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/Lidar_Files/Lidar_Outputs/{XLEFT}_{YBOTTOM}_CHM"
  # Alternative code
  #opt_output_files(normlas) =   # See down this line for change in the directory                                *                                                *
  #  "C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/Lidar_Files/CHM/{XLEFT}_{YBOTTOM}_CHM"

# Once again I set this
normlas@output_options$drivers$Raster$param$overwrite <- TRUE

# Creating a canopy height model
  # I believe you can also use the grid_canopy function.
  # I am not 100% sure which is the best route. I think you can create your own
  # algorithms to put inside the grid_canopy function. There is some 
  # documentation on it online. Here p2r() is the algorithm. I believe it is the
  # default setting.
chm <- rasterize_canopy(normlas, res = 1, p2r())
plot(chm)


# Change to raster instead of terra/SpatRaster
  # The CHM function outputs the CHM as a SpatRaster not a raster so you cannot
  # mapview it without changing the SpatRaster into a normal Raster. I am sure
  # the creator of mapview is working on this since the raster package will be 
  # phased out in late 2023. To do this just use the raster() function on the
  # SpatRaster.
chm <- raster(chm)
mapview(chm)
writeRaster(chm,
            "./Lidar_Output/CHM.tif",
            overwrite = T)

#---------------------------------------#
# Now filtering points to understory ####
#---------------------------------------#

# Create duplicate las objects for the filter
understory <- normlas

# Now lets try calculating this by chunk sizes to make computation easier
opt_chunk_size(understory) <- 0 # 500x500m chunks

# Here we use the opt_chunk_buffer() function. This stops lidR from creating
# buffer zones between the LAS tiles. I believe that this should always be set
# to 0. However, I am not 100% sure.
opt_chunk_buffer(understory) <- 0

# Now to filter to our points
  # With a normal LAScatalog we would use the filter_poi() function (you'll see
  # this later on in the code). Here we use opt_filter() to drop points that are
  # below a height of 0 m and about 2 m. We also use the -drop_overlap to 
  # remove the overlapping flight areas. From what I have read, this will only
  # work if there is a overlap attribute in the original data set.
opt_filter(understory) <- "-drop_z_below 0 -drop_z_above 2 -drop_overlap"

# This function sets the new catalog up with the filter
  # This catalog_retile function basically takes the filter and applies that 
  # filter. The opt_filter function does not appear to work without using
  # catalog_retile.
understory <- catalog_retile(understory)

# Now because we are chunking {ORIGINALFILENAME} will not work for the naming.
# Here I set it to do the farthest west x-coord and farthest south y-coord.
# {XLEFT}_{YBOTTOM}
# Once again set your output
opt_output_files(understory) = 
  "C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/Lidar_Files/Lidar_Outputs/{XLEFT}_{YBOTTOM}_understory_density"
understory@output_options$drivers$SpatRaster$param$overwrite <- TRUE

# Now run the function
  # Since our LAS files has been thinned to points below two meters. This 
  # calculates the density of the point cloud below two meters. Once again
  # this function outputs as a SpatRaster. To mapview, use the raster() function
  # on the SpatRaster.
understory_dens <- rasterize_density(understory, res = 1)
plot(understory_dens)

writeRaster(understory_dens,
            "Understory_Density.tif",
            overwrite = T)


#-----------------------------------#
# Now filtering points to canopy ####
#-----------------------------------#

# Create duplicate las objects for the filter
overstory <- normlas

# Now lets try calculating this by chunk sizes to make computation easier
opt_chunk_size(overstory) <- 0 # 500x500m chunks
opt_chunk_buffer(overstory) <- 0
opt_filter(overstory) <- "-drop_z_below 2 -drop_overlap"

# This function sets the new catalog up with the filter
overstory <- catalog_retile(overstory)

# Now because we are chunking {ORIGINALFILENAME} will not work for the naming.
# Here I set it to do the farthest west x-coord and farthest south y-coord.
# {XLEFT}_{YBOTTOM}
# Once again set your output
opt_output_files(overstory) = 
  "C:/Users/coltonp/OneDrive - New Mexico State University/Desktop/GIS/Spatial_R_Code_Shapefiles/Lidar_Files/Lidar_Outputs/{XLEFT}_{YBOTTOM}_overstory_density"
overstory@output_options$drivers$SpatRaster$param$overwrite <- TRUE
# Now run the function
overstory_dens <- rasterize_density(overstory, res = 1)

plot(overstory_dens)

writeRaster(overstory_dens,
            "Canopy_Density.tif",
            overwrite = T)

rm(overstory, overstory_dens, 
   understory, understory_dens)

#----------------------------------------#
# Test the filter duplicates function ####
#----------------------------------------#

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# In this area of the code, we moved from using the LAS catalog to just a normal
# LAS files. These functions may not work on an LAScatalog object. I just have
# not attempted anything with them.
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

# Bring in a test file
test <- readLAS("USGS_LPC_NM_NorthCentral_B1_2016_13SCV570690.laz",
                filter = "-drop_overlap -keep_class 1 2")

# Check what has been done
las_check(test)

# Use filter duplicates
test <- filter_duplicates(test)

# Create a DTM
dtm <- grid_terrain(test, res = 1, knnidw())
plot(dtm)

# Normalize the las
norm_test <- normalize_height(test, dtm)
plot(norm_test)

# Filter points
  # This does to a normal LAS what opt_filter() and catalog_retile() does for an
  # LAS catalog. Z is the dimension for height.
# Understory
norm_test1 <- filter_poi(norm_test, 
                   Z > 0.25 & Z < 2)

# Canopy
norm_test2 <- filter_poi(norm_test, 
                         Z > 2)
plot(norm_test1)

# Density
norm_dens <- rasterize_density(norm_test1, res = 1)
plot(norm_dens)

norm_dens2 <- rasterize_density(norm_test2, res = 1)

norm_ras <- raster(norm_dens)
norm_ras2 <- raster(norm_dens2)
mapview(norm_ras2)


#------------------#
# Segment trees ####
#------------------#

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# Here we use some functions to get rid of noise within the data but also to 
# find trees in the LAS and create a polygon layer that outlines each individual
# tree. This polygon layer is not perfectly accurate for what I have seen on 
# mapview. See it for yourself to see what you think.
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

# Use norm_test as your data file here.

# Get rid of any noise in the data
  # This function gets rid of points by the 95% quantiles. Anything outside of
  # that (birds, random points, etc.) gets removed with this filter noise. 
  # I believe the earlier filter where I brought in the LAS (-keep_category 1 2)
  # removed most of the noise. USGS points clouds are classified into different
  # categories. 1 and 2 are ground and unclassified. 7 and 12 were something (7) 
  # and noise (12). You can find the definitions in the USGS LiDAR manual.
  # https://pubs.usgs.gov/tm/11b4/pdf/tm11-B4.pdf
filter_noise = function(las, sensitivity)
{
  p95 <- grid_metrics(las, ~quantile(Z, probs = 0.95), 10)
  las <- merge_spatial(las, p95, "p95")
  las <- filter_poi(las, Z < p95*sensitivity)
  las$p95 <- NULL
  return(las)
}
las_denoised <- filter_noise(norm_test, sensitivity = 1.2)

# Now create a canopy height model
  # Here we use the pitfree function to create an algorithm. I found this online
  # and do not know how it works.
chm <- grid_canopy(las_denoised, 0.5, pitfree(c(0,2,5,10,15), c(3,1.5), subcircle = 0.2))
plot_dtm3d(chm)

# Smooth the model
  # This smoothing has something to do with making sure the CHM does not have
  # random jumps.
ker <- matrix(1,5,5)
chm_s <- focal(chm, w = ker, fun = median)

# Create the algorithm and segment trees
  # Algo is the algorithm that is created using the watershed function. We then
  # input that into the segment trees function. I do not have it here, but
  # the locate_trees() function is what we would use to get coordinates for the
  # center of each tree. 
algo <- lidR::watershed(chm_s, th_tree = 3)
las_watershed  <- segment_trees(las_denoised, algo)

# remove points that are not assigned to a tree
trees <- filter_poi(las_watershed, !is.na(treeID))

# View the results
  # Viewing point clouds is kind of difficult and takes some getting used to.
  # It will bring up a new window in your computer to view it. My computer
  # is not very strong and has a lot of lag time. Yours may not.
plot(trees, color = "treeID", colorPalette = pastel.colors(100))

# Creating a canopy height model here ensures almost nothing is interfering
  # You can now create a canopy height model for trees only. After removing the
  # points that aren't associated with trees, everything else will be shown as
  # 0. Once again this outputs as a SpatRaster. Use the raster() function.
chm_tree <- rasterize_canopy(trees, res = 1)
plot(chm_tree)

chm_tree <- raster(chm_tree)
mapview(chm_tree)

chm_tree <- rast(chm_tree)

# Now create hulls for trees
  # This function is what will create the polygons for each individual tree.
  # I just used settings I found online for type, concavity, and func. There
  # is probably descriptions in the LidR package somewhere.
tree_hulls <- delineate_crowns(trees,
                               type = "convex",
                               concavity = 2,
                               func = .stdmetrics)

mapview(tree_hulls)
