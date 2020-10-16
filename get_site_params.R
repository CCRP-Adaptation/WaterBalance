#####################################################################
###   Pulling Site Parameters for D. THoma Water Balance Model ######
#####################################################################

rm(list=ls())

library(sf)
library(raster)
library(dplyr)
library(rgdal)
library(tmap)
#library(soilDB)

###########################   USER INPUTS ###################################################################################### 

setwd("C:/Users/adillon/Documents/ArcGIS")# Set working directory to where spatial files are located

# Set projection to be used for all spatial data: North America Albers Equal Area Conic
aea <- '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs' 


site <- "PETE"
OutDir <- "C:/Users/adillon/Documents/RSS/PETE/"

# load shapefiles for NPS park boundaries, US Counties, 

nps_boundary <- st_read('./nps_boundary/nps_boundary.shp')
nps_boundary <- st_transform(nps_boundary, crs = aea)
nps_centroids <- st_read('./nps_boundary_centroids/nps_boundary_centroids.shp')
nps_centroids <- st_transform(nps_centroids, crs = aea)
US_Counties <- st_read('./US_Counties/tl_2016_us_county.shp')
US_Counties <- st_transform(US_Counties, crs = aea)
US_States <- st_read('./State_Shapefile/Contig_US_Albers.shp')
US_States <- st_transform(US_States, crs = aea)
maca <- raster('Climate_grid/tdn_90d.nc') # MACA grid
maca <- projectRaster(maca, crs = nps_boundary)

# select park

park <- filter(nps_boundary, UNIT_CODE == "PETE")
centroid <- filter(nps_centroids, UNIT_CODE == "PETE")
state <- filter(US_States, STATE_NAME == "Virginia")
#########################     END USER INPUTS   ##################################################################################

# Check that spatial data looks OK so far. Precise projection doesn't matter at this point but should be close. 

state_and_park <- tm_shape(state) +
  tm_borders() + 
  tm_fill(col = "lightgrey") +
  tm_shape(park) +
  tm_borders() + 
  tm_fill(col = "green")

park_and_centroid <- tm_shape(park) + 
  tm_borders() +
  tm_fill(col = "lightgreen") + 
  tm_shape(centroid) + 
  tm_dots(size = 1, shape = 3)

tmap_arrange(state_and_park, park_and_centroid)

# Obtain MACA grid outline (not information within)

centroid <- as_Spatial(centroid) # objects must be Spatial (sp) to work with raster package (cannot be sf)
centroid <- SpatialPoints(centroid) # Converts to point object instead of df
cell <- cellFromXY(maca, centroid) # find grid cell park centroid lies within
maca_cell <- rasterFromCells(maca, cell) # create stand-alone raster for single MACA cell
maca.poly <- rasterToPolygons(maca_cell) # Create MACA polygon - original file in lat/long (note: datum differs from park shapefiles)

# Plot to check MACA location

tm_shape(park) +
  tm_borders() +
  tm_fill(col = "lightgreen") +
  tm_shape(maca.poly) +
  tm_borders()

# DEM and soils layers (sent by Mike Tercek)

dem <- raster('elevation_cropped.tif')
dem <- projectRaster(dem, maca) 

dem_crop <- crop(dem, state)
soil <- raster('water_storage.tif') # Mike Tercek's soil file - original projection Albert's Equal Area
soil <- projectRaster(soil, maca)

#####   SLOPE, ASPECT AND RANDOM POINTS   ##########################################################################################

# Create slope and aspect rasters

slope <- terrain(dem_crop, opt = "slope", unit = "degrees", neighbors = 4) # 4 is better for "smooth" surfaces; 8 is better for rough. See https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/terrain
plot(slope) # check slope looks OK
aspect <- terrain(dem_crop, opt = "aspect", unit = "degrees")
plot(aspect) # check aspect looks OK

# Crop to projected MACA cell 

soil_crop <- crop(soil_project_to_DEM, maca_project_to_DEM)
dem_crop <- crop(dem, maca_project_to_DEM)
slope_crop <- crop(slope, maca_project_to_DEM)
aspect_crop <- crop(aspect, maca_project_to_DEM)

# get 10 random points from soil raster and create SpatialPoints object
points <- spsample(maca_project_to_DEM, n = 10, type = "random")


####    PLOT TO CHECK SPATIAL ALIGNMENT   ############################################################################################

# Make sure points look identical against all rasters, and that rasters are similar in size and orientation

soil_plot <- tm_shape(soil_crop) + 
  tm_raster(legend.show = FALSE) +
  tm_shape(points) +             
  tm_dots(size = 1)

dem_plot <- tm_shape(dem_crop) + 
  tm_raster(legend.show = FALSE) + 
  tm_shape(points) + 
  tm_dots(size = 1)

slope_plot <- tm_shape(slope_crop) + 
  tm_raster(legend.show = FALSE) + 
  tm_shape(points) + 
  tm_dots(size = 1)

aspect_plot <- tm_shape(aspect_crop) + 
  tm_raster(legend.show = FALSE) + 
  tm_shape(points) + 
  tm_dots(size = 1)

tmap_arrange(soil_plot, dem_plot, slope_plot, aspect_plot) # make sure all points are within boundaries and plots look good. ignore warning message related to tmap_options

####    EXTRACT DATA FROM POINTS ######################################################################################################

# reproject points to lat/long so can eventually add to .csv

latlong <- st_as_sf(points) # convert to sf object 
latlong <- st_transform(latlong, crs = 4326) # project to lat/long

sites <- as.data.frame(st_coordinates(latlong)) # begin new dataframe for sites

sites[,3] <- extract(dem, points)
sites[,4] <- extract(aspect_crop, points)
sites[,5] <- extract(slope_crop, points)
sites[,6] <- extract(soil_crop, points)
sites[,7] <- seq.int(nrow(sites))
sites[,8] <- 5 # default value for wind
sites[,9] <- 0 # default value for snowpack
sites[,10] <- 0 # default value for Soil.Init
sites[,11] <- 1 # default value for shade coefficient
  
sites <- select(sites, 7,2,1,3:6, 8:11) # reorder columns
colnames(sites) <- c("SiteID", "Lat", "Lon", "Elev", "Aspect", "Slope", "SWC.Max", "Wind", "Snowpack", "Soil.Init", "Shade.Coeff")

sites$SWC.Max = sites$SWC.Max*10
sites # check 

write.csv(sites, file =  paste0(OutDir, site, "_site_characteristics.csv"), row.names = FALSE)

soil_crop@data



