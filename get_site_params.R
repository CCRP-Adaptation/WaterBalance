#####################################################################
###   Pulling Site Parameters for D. THoma Water Balance Model ######
#####################################################################

rm(list=ls())

library(sf)
library(raster)
library(dplyr)
library(rgdal)
library(ggplot2)
#library(soilDB)

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
soil <- raster('./RSS/PETE/soil') # raster file exported from ArcGIS (MapunitRaster_10m) with spatial join to valu1 table

# Project spatial data

dem_projection <- crs(dem) # raster 
#soil_projection <- crs(soil) # This may come in handy later if we are able to import gridded soils directly into R

park_proj <- st_transform(park, dem_projection)
centroid_proj <- st_transform(centroid, dem_projection)

# crop soil and MACA grid; reproject soil to DEM projection

maca.poly <- rasterToPolygons(maca_cell)
#maca.poly <- spTransform(maca.poly, soil_projection) # project MACA cell to projection of soil layer - not necessary now but will need to crop soil layer later to cut processing time for reprojection
#soil_crop <- crop(soil, maca.poly) # crop soil raster to maca cell - same comment as above

soil_proj <- projectRaster(soil, crs = dem_projection) # project cropped soil layer to DEM projection
maca_proj <- spTransform(maca.poly, dem_projection)


#####   SLOPE, ASPECT AND RANDOM POINTS   ##########################################################################################

# Create slope and aspect rasters

slope <- terrain(dem, opt = "slope", unit = "degrees", neighbors = 4) # 4 is better for "smooth" surfaces; 8 is better for rough. See https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/terrain
aspect <- terrain(dem, opt = "aspect", unit = "degrees")

# Crop to projected MACA cell 

dem_crop <- crop(dem, maca_proj)
slope_crop <- crop(slope, maca_proj)
aspect_crop <- crop(aspect, maca_proj)

# Create random points

points <- spsample(maca_proj, n = 10, type = "random")


####    PLOT TO CHECK SPATIAL ALIGNMENT   ############################################################################################

soil_plot <- tm_shape(soil_proj) + 
  tm_raster(legend.show = FALSE) +
  tm_shape(points) +             
  tm_dots(size = 1)

dem_plot <- tm_shape(dem_crop) + # because slope and aspect plot were created
  tm_raster(legend.show = FALSE) + 
  tm_shape(points) + 
  tm_dots(size = 1)

tmap_arrange(soil_plot, dem_plot) # make sure all points are within 

# plot soil raster and DEM side-by-side to make sure points fall within boundaries of both. Because cropped soil layer was created in ArcGIS, 


############
set.seed(0)
r <- raster(nrow=10, ncol=10)
values(r) <- runif(ncell(r)) * 10
is.factor(r)
#is.factor(soil) # yes soil is a factor

r <- round(r)
f <- as.factor(r)
is.factor(f)

# add attribute
x <- levels(f)[[1]] # 
x
x$code <- letters[10:20]
levels(f) <- x
levels(f)
f

r <- raster(nrow=10, ncol=10)
values(r) = 1
r[51:100] = 2
r[3:6, 1:5] = 3
r <- ratify(r) # makes values into attributes

rat <- levels(r)[[1]] # list of attributes
atts <- levels(soil)[[1]]

rat$landcover <- c('Pine', 'Oak', 'Meadow')
rat$code <- c(12,25,30)
levels(r) <- rat
levels(soil) <- atts # assigns attributes to levels
r
soil

# extract values for some cells
i <- extract(r, c(1,2, 25,100))

# get 10 random points from soil raster

pts <- sampleRandom(soil, size = 10)

j <- extract(soil, pts)
# get the attribute values for these cells
factorValues(r, i)
factorValues(soil, pts)


pointCells <- cellFromXY(soil_proj, points)

levels(soil)

lev <- levels(soil_proj)[[1]]
aws <- as.factor(lev$AWS0_999)

i <- extract(soil_proj, pointCells)

factorValues(i, aws)
lev.factor
