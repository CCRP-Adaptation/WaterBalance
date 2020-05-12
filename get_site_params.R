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

############################################################# USER INPUTS ##################################################################### 

setwd("C:/Users/adillon/Documents/ArcGIS")# Set working directory to where spatial files are located

site <- "PETE"
OutDir <- "C:/Users/adillon/Documents/RSS/PETE/"

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

# First, import DEM and soils layers into ArcGIS.
# Join gSSURGO soils raster (MapunitRaster_10m) with valu1 table
# Get UTM projection from DEM and reproject soils raster into appropriate projection
# Save new raster into park folder (NOT file geodatabase)
# NOTE: Soils raster cannot be reprojected in R because will lose data associated with RAT (Raster Attribute Table)

dem <- raster('./RSS/PETE/elevation/ned30m37077.tif') # DEM 30 m downloaded from USDA NRCS
soil <- raster('./RSS/PETE/soil_18N') # projected raster file exported from ArcGIS (MapunitRaster_10m) with spatial join to valu1 table
soil@data@attributes[[1]] # check that RAT looks OK

# Project spatial data

dem_projection <- crs(dem) # raster 
#soil_projection <- crs(soil) # This may come in handy later if we are able to import gridded soils directly into R

park_proj <- st_transform(park, dem_projection)
centroid_proj <- st_transform(centroid, dem_projection)

# MACA grid
# When we can get soils data directly, crop soil grid to MACA cell and reproject to DEM

maca.poly <- rasterToPolygons(maca_cell) # Create MACA polygon
#maca.poly <- spTransform(maca.poly, soil_projection) # project MACA cell to projection of soil layer - not necessary now but will need to crop soil layer later to cut processing time for reprojection
#soil_crop <- crop(soil, maca.poly) # crop soil raster to maca cell - same comment as above

#soil_proj <- projectRaster(soil, crs = dem_projection) # project cropped soil layer to DEM projection
maca_proj <- spTransform(maca.poly, dem_projection) # reproject MACA grid 


#####   SLOPE, ASPECT AND RANDOM POINTS   ##########################################################################################

# Create slope and aspect rasters

slope <- terrain(dem, opt = "slope", unit = "degrees", neighbors = 4) # 4 is better for "smooth" surfaces; 8 is better for rough. See https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/terrain
plot(slope) # check slope looks OK
aspect <- terrain(dem, opt = "aspect", unit = "degrees")
plot(aspect) # check aspect looks OK

# Crop to projected MACA cell 

soil_crop <- crop(soil, maca_proj)
dem_crop <- crop(dem, maca_proj)
slope_crop <- crop(slope, maca_proj)
aspect_crop <- crop(aspect, maca_proj)

# get 10 random points from soil raster and create SpatialPoints object

tail(soil_crop@data@attributes)
points <- spsample(maca_proj, n = 10, type = "random")


####    PLOT TO CHECK SPATIAL ALIGNMENT   ############################################################################################

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

tmap_arrange(soil_plot, dem_plot, slope_plot, aspect_plot) # make sure all points are within boundaries and plots look good

####    EXTRACT DATA FROM POINTS ######################################################################################################

is.factor(soil_crop) # should be TRUE
#atts <- levels(soil_crop)[[1]] # list of attributes
#levels(soil_crop) <- atts # assigns attributes to levels
#soil_crop
i <- extract(soil_crop, points) 
pointSoil <- factorValues(soil_crop, i)


latlong <- st_as_sf(points) # convert to sf object 
latlong <- st_transform(latlong, crs = 4326) # project to lat/long


sites <- as.data.frame(st_coordinates(latlong)) # begin new dataframe for sites

sites[,3] <- extract(dem, points)
sites[,4] <- extract(aspect_crop, points)
sites[,5] <- extract(slope_crop, points)
sites[,6] <- pointSoil$AWS0_999
sites[,7] <- seq.int(nrow(sites))
  
sites <- select(sites, 7,2,1,3:6) # reorder columns
colnames(sites) <- c("SiteID", "Lat", "Lon", "Elev", "Aspect", "Slope", "SWC.Max")

write.csv(sites, file =  paste0(OutDir, site, "_site_characteristics.csv"), row.names = FALSE)




