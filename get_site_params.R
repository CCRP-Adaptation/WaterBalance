#####################################################################
###   Pulling Site Parameters for D. THoma Water Balance Model ######
#####################################################################

rm(list=ls())

library(sf)
library(raster)
library(dplyr)
library(soilDB)

############################################################# USER INPUTS ##################################################################### 

setwd("C:/Users/adillon/Documents/ArcGIS") # Set working directory to where spatial files are located

# load shapefiles for NPS park boundaries, US Counties, MACA grid, DEM

nps_boundary <- st_read('./nps_boundary/nps_boundary.shp')
nps_boundary_centroids <- st_read('./nps_boundary_centroids/nps_boundary_centroids.shp')
US_Counties <- st_read('./US_Counties/tl_2016_us_county.shp')
MACA_grid <- st_read('./Climate_grid/MACA_grid.shp')

# select park

park <- filter(nps_boundary, UNIT_CODE == "PETE")
centroid <- filter(nps_boundary_centroids, UNIT_CODE == "PETE")

# get county

county <- US_Counties %>%
  filter(st_intersects(.,centroid, sparse = FALSE))

county$NAME[, drop = TRUE] # See console for the name of the county where the park centroid resides. 
                           # Input this name into https://datagateway.nrcs.usda.gov/GDGOrder.aspx to get DEM and gridded soil data


######### STOP HERE AND DOWNLOAD DATA FROM https://datagateway.nrcs.usda.gov/GDGOrder.aspx #################################################

dem <- raster('./RSS/PETE/elevation/ned30m37077.tif') # DEM 30 m downloaded from USDA NRCS
soil <- raster('./RSS/PETE/MapunitRaster_10m1.tif') # raster file exported from ArcGIS (MapunitRaster_10m) with spatial join to valu1 table

