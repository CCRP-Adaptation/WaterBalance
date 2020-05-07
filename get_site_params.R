#####################################################################
###   Pulling Site Parameters for D. THoma Water Balance Model ######
#####################################################################

rm(list=ls())

library(sf)
library(raster)
library(dplyr)
library(rgdal)
library(gdalUtils)
library(soilDB)

############################################################# USER INPUTS ##################################################################### 

setwd("C:/Users/adillon/Documents/ArcGIS") # Set working directory to where spatial files are located

# load shapefiles for NPS park boundaries, US Counties, 

nps_boundary <- st_read('./nps_boundary/nps_boundary.shp')
nps_boundary_centroids <- st_read('./nps_boundary_centroids/nps_boundary_centroids.shp')
US_Counties <- st_read('./US_Counties/tl_2016_us_county.shp')
State_Shapefile <- st_read('./State_Shapefile/Contig_US_Albers.shp')

# select park

park <- filter(nps_boundary, UNIT_CODE == "PETE")
centroid <- filter(nps_boundary_centroids, UNIT_CODE == "PETE")
state <- filter(State_Shapefile, STATE_NAME == "Virginia")

# MACA grid

centroid.coords <- cbind(centroid$Lon, centroid$Lat) # extract lat/long from centroid
centroid.sp <- SpatialPoints(centroid.coords) # convert centroid into SpatialPoints object
maca <- raster('tdn_90d.nc') # import MACA gridded data - this is to obtain MACA grid outline not information within

cell <- cellFromXY(maca, centroid.sp) # find grid cell park centroid lies within
maca_cell <- rasterFromCells(maca, cell) # create stand-alone raster for single MACA cell


# get county

county <- US_Counties %>%
  filter(st_intersects(.,centroid, sparse = FALSE))

county$NAME[, drop = TRUE] # See console for the name of the county where the park centroid resides. 
                           # Input this name into https://datagateway.nrcs.usda.gov/GDGOrder.aspx to get DEM and gridded soil data


######### STOP HERE AND DOWNLOAD DATA FROM https://datagateway.nrcs.usda.gov/GDGOrder.aspx #################################################

dem <- raster('./RSS/PETE/elevation/ned30m37077.tif') # DEM 30 m downloaded from USDA NRCS
soil <- raster('./RSS/PETE/MapunitRaster_10m2.tif') # raster file exported from ArcGIS (MapunitRaster_10m) with spatial join to valu1 table

# Project spatial data

dem_projection <- crs(dem) # raster 

park_proj <- st_transform(park, projection)
centroid_proj <- st_transform(centroid, projection)

# crop soil and MACA grid; reproject soil to DEM projection

maca.poly <- rasterToPolygons(maca_cell)
maca.poly <- spTransform(maca.poly, soil_projection) # project MACA cell to projection of soil layer
soil_crop <- crop(soil, maca.sp) # crop soil raster to maca cell

soil_proj <- projectRaster(soil_crop, crs = dem_projection) # project cropped soil layer to DEM projection
maca_proj <- spTransform(maca.poly, dem_projection)

###############################################################################################

# Create slope and aspect rasters

slope <- terrain(dem, opt = "slope", unit = "degrees", neighbors = 4) # 4 is better for "smooth" surfaces; 8 is better for rough. See https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/terrain
aspect <- terrain(dem, opt = "aspect", unit = "degrees")

# Create random points

points <- spsample(maca_proj, n = 10, type = "random")

# Create rasterstack to extract values to points

ext <- extent(maca_proj)

slope_crop <- crop(slope, ext)
aspect_crop <- crop(aspect, ext)

############
##  STUCK: CANNOT FIND WATER HOLDING CAPACITY DATA IN EXPORTED RASTER. START HERE MONDAY 5/7/2020

soil@data@attributes[[1]] # empty list





