########################################################################
###   Pulling Site Parameters for D. Thoma Water Balance Model #########
########################################################################

# Annie Kellner Dillon 
# 2020-12-10

rm(list=ls())

library(sf)
library(raster)
library(dplyr)
library(rgdal)
library(tmap)
library(tmaptools)
library(rJava) # required for mapping
library(OpenStreetMap) # required for mapping

# Enter the following code if you need to install Java and point R to the directory:
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_261') # for 64-bit version #make sure to check the program file for the jre1 numbers.

# Required data files for DEM and Soil WHC measurements are located on the Shared Drive:
# DOI\WB working group - General\Data_Files
# It is probably best to download the files to your local computer if you will use this script regularly

###########################   USER INPUTS ###################################################################################### 

setwd("C:/Users/adillon/Documents/ArcGIS")# Set working directory to where spatial files are located

# Park info

site <- "SAGU"
state <- "Arizona"

# MACA cell lat/long (if not using park centroid to determine MACA cell)

lat <- 40.4465
lon <- -108.7651

# Set projection to be used for all spatial data:

proj4 <-"+init=epsg:5070" #  North America Albers Equal Area Conic
epsg <- 5070 # North American Albers Equal Area Conic

OutDir <- "C:/Users/adillon/Documents/RSS/PETE/" # Output directory. Ouput created is a .csv file with site parameters and a series of maps for ppt presentation

###########################   END USER INPUTS   #################################################################################

###  Spatial Data  #####
# This data is available from NPS

# rasters - use proj4 to define projection

maca <- raster('./Climate_grid/tdn_90d.nc') 
maca <- projectRaster(maca, crs = proj4)
dem <- raster('elevation_cropped.tif') 
dem <- projectRaster(dem, crs = proj4)
soil <- raster('water_storage.tif') 
soil <- projectRaster(soil, crs= proj4)

# shapefiles - can use epsg code or existing object to define projection

nps_boundary <- st_read('./nps_boundary/nps_boundary.shp')
nps_boundary <- st_transform(nps_boundary, st_crs(maca))
nps_centroids <- st_read('./nps_boundary_centroids/nps_boundary_centroids.shp')
nps_centroids <- st_transform(nps_centroids, st_crs(maca))
US_Counties <- st_read('./US_Counties/tl_2016_us_county.shp')
US_Counties <- st_transform(US_Counties, st_crs(maca))
US_States <- st_read('./State_Shapefile/Contig_US_Albers.shp')
US_States <- st_transform(US_States, st_crs(maca))


# select park

park <- filter(nps_boundary, UNIT_CODE == site)

state <- filter(US_States, STATE_NAME == state)

# TWO DIFFERENT OPTIONS FOR CENTROID - use 1st option if running a general RSS and using park centroid. Second option if using specific lat long.

centroid <- filter(nps_centroids, UNIT_CODE == site) # use this line if using park centroid

#centroid <- data.frame(Lat = lat, Lon = lon) %>% # use lines 72, 73, 75, 76 if NOT using centroid
  #st_as_sf(coords=c("Lon", "Lat"))

#centroid <- st_set_crs(centroid, "+proj=longlat +datum=NAD83 +no_defs")
#centroid <- st_transform(centroid, st_crs(maca))


#########################     END USER INPUTS   ##################################################################################

# Check that spatial data looks OK so far. Precise projection doesn't matter at this point but should be close. 
# You should see the park outline by itself, and also where it lies within the state.

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

centroid<- as_Spatial(centroid) # objects must be Spatial (sp) to work with raster package (cannot be sf)
cell <- cellFromXY(maca, centroid) # find grid cell park centroid lies within
maca_cell <- rasterFromCells(maca, cell) # create stand-alone raster for single MACA cell
maca.poly <- rasterToPolygons(maca_cell) # Create MACA polygon - original file in lat/long (note: datum differs from park shapefiles)

# Plot to see that MACA cell is visible and appropriately located within park

tm_shape(park) + 
  tm_borders() + 
  tm_shape(centroid) + 
  tm_dots() + 
  tm_shape(maca.poly) + 
  tm_borders()


#####   SLOPE, ASPECT AND RANDOM POINTS   ##########################################################################################

# Create slope and aspect rasters

slope <- terrain(dem, opt = "slope", unit = "degrees", neighbors = 4) # 4 is better for "smooth" surfaces; 8 is better for rough. See https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/terrain
aspect <- terrain(dem, opt = "aspect", unit = "degrees")

# get 10 random points from soil raster and create SpatialPoints object
points <- spsample(maca.poly, n = 10, type = "random")

# plot to check points appear within borders of MACA cell. 

tm_shape(park) + 
  tm_borders() + 
  tm_shape(maca.poly) + 
  tm_borders() + 
  tm_shape(points) + 
  tm_dots()

####    EXTRACT DATA FROM POINTS  ######################################################################################################

# reproject points to lat/long so can eventually add to .csv

latlong <- st_as_sf(points) # convert to sf object 
latlong <- st_transform(latlong, crs = 4326) # project to lat/long

sites <- as.data.frame(st_coordinates(latlong)) # begin new dataframe for sites

sites[,3] <- extract(dem, points)
sites[,4] <- extract(aspect, points)
sites[,5] <- extract(slope, points)
sites[,6] <- extract(soil, points)
sites[,7] <- seq.int(nrow(sites))
sites[,8] <- 5 # default value for wind
sites[,9] <- 0 # default value for snowpack
sites[,10] <- 0 # default value for Soil.Init
sites[,11] <- 1 # default value for shade coefficient
  
sites <- select(sites, 7,2,1,3:6, 8:11) # reorder columns
colnames(sites) <- c("SiteID", "Lat", "Lon", "Elev", "Aspect", "Slope", "SWC.Max", "Wind", "Snowpack", "Soil.Init", "Shade.Coeff")

sites$SWC.Max = sites$SWC.Max*10 # convert units for Soil Water-holding capacity
sites # check to be sure values are populated correctly. There should not be NA values. 

write.csv(sites, file =  paste0(OutDir, site, "_site_characteristics.csv"), row.names = FALSE)

###########  AUTOMATE MAPS   ####################################################################################################

# This section creates a series of maps from which the project lead can choose a favorite

# NOTE: It does not seem to work to overlay the park layer onto the osm map and add transparency. You need to overlay the osm onto the park layer
# and make the osm layer transparent. 

tmap_mode('plot')

# Maps with 25 MACA cells
#"Directions" refers to the number of directions in which cells should be connected. 
# 16 = knight and one-cell queen moves. See adjacent function for more info. 

adjacent_cells <- adjacent(maca, cells = cell, directions = 16) # Find cells around centroid. 
adjacent_cells <- rasterFromCells(maca, adjacent_cells) # Create new raster from cells
adjacent_poly <- rasterToPolygons(adjacent_cells) # Convert raster to polygon so cell outlines are visible

adjacent_poly <- spTransform(adjacent_poly, CRSobj = "+init=epsg:4326")

map_types = c("bing", "osm", "stamen-terrain", "esri-topo", "apple-iphoto") # map types can be found here: https://www.rdocumentation.org/packages/OpenStreetMap/versions/0.3.4/topics/openmap

# Loop that creates series of maps with 25 MACA cells

for(i in 1:length(map_types)){
  osm <- tmaptools::read_osm(bb(adjacent_poly), type = map_types[i])
  map <- tm_shape(park) + tm_borders(col = "black", lwd = 3) + 
    tm_fill(col = "darkgreen") + 
    tm_shape(osm, is.master = TRUE) + tm_rgb(alpha = 0.8) +
    tm_shape(adjacent_poly) + tm_borders(lwd = 3, col = "orange") + 
    tm_shape(maca.poly, unit = "mi") + tm_borders(col = "#0000ff", lwd = 4) + 
    tm_add_legend(type = "line", label = "MACA grid", col = "orange", lwd = 3) + 
    tm_add_legend(type = "line", label = "Selected CMIP5 cell", col = "#0000ff", lwd = 4) +
    tm_add_legend(type = "fill", label = "National Park", col = "darkgreen") +
    tm_compass(type = "4star", position = c("right", "top"), size = 3, bg.color = "white") + tm_scale_bar(text.size = 0.75, breaks = c(0,1,2,3), bg.color = "white") +
    tm_layout(legend.position = c("left", "bottom"), legend.stack = "vertical", legend.frame = TRUE, legend.bg.color = "white", legend.text.size = 1)
  
  tmap_save(map, filename = paste0(OutDir, site, "-MACA25-", map_types[i], ".png"))
  
}

# Loop that creates maps with 9 MACA cells 

adjacent_cells <- adjacent(maca, cells = cell, directions = 8) # Find cells around centroid. 8 = "Queen's case"
adjacent_cells <- rasterFromCells(maca, adjacent_cells) # Create new raster from cells
adjacent_poly <- rasterToPolygons(adjacent_cells) # Convert raster to polygon so cell outlines are visible

adjacent_poly <- spTransform(adjacent_poly, CRSobj = "+init=epsg:4326")

map_types = c("bing", "osm", "stamen-terrain", "esri-topo", "apple-iphoto")

for(i in 1:length(map_types)){
  osm <- tmaptools::read_osm(bb(adjacent_poly), type = map_types[i])
  map <- tm_shape(park, name = "National Park") + 
    tm_borders() +
    tm_fill(col = "darkgreen") + 
    tm_shape(osm, is.master = TRUE) + tm_rgb(alpha = 0.5) +
    tm_shape(adjacent_poly) + tm_borders(lwd = 3, col = "orange") + 
    tm_shape(maca.poly, unit = "mi") + tm_borders(col = "#0000ff", lwd = 4) + 
    tm_add_legend(type = "line", label = "MACA grid", col = "orange", lwd = 3) + 
    tm_add_legend(type = "line", label = "Selected CMIP5 cell", col = "#0000ff", lwd = 4) +
    tm_add_legend(type = "fill", label = "National Park", col = "darkgreen", alpha = 0.5) +
    tm_compass(type = "4star", position = c("right", "top"), size = 3, bg.color = "white") + tm_scale_bar(text.size = 0.75, breaks = c(0,1,2,3), bg.color = "white") +
    tm_layout(legend.position = c("left", "bottom"), legend.stack = "vertical", legend.frame = TRUE, legend.bg.color = "white", legend.text.size = 1)
  
  tmap_save(map, filename = paste0(OutDir, site, "-MACA9-", map_types[i], ".png"))
  
}


