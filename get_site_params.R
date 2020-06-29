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

setwd("C:/Users/adillon/Documents/ArcGIS") # Set working directory to where spatial files are located

grid <- raster('./Climate_grid/tdn_90d.nc') # gridmet/MACA grid

latlon <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

State_Shapefile <- st_read('C:/Users/adillon/Documents/ArcGIS/State_Shapefile/Contig_US_Albers.shp')
state <- filter(State_Shapefile, STATE_NAME == "Colorado")
co <- st_transform(state, crs = latlon)

# Fort Collins 

lat <- 40.5853
lon <- -105.0844
M <- as.matrix(cbind(lon, lat))

foco <- SpatialPoints(M, proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
cell <- cellFromXY(grid, foco) # find gridmet cell Bailly is located in 

grid_cell <- rasterFromCells(grid, cell) # create stand-alone raster for single MACA cell

# plot

state_and_cell <- tm_shape(co) +
  tm_polygons() +
  tm_shape(grid_cell) +
  tm_raster(legend.show = FALSE) +
  tm_layout(main.title = "State and GridMET cell")

cell_and_city <- tm_shape(grid_cell) +
  tm_raster(legend.show = FALSE) +
  tm_shape(foco) +
  tm_symbols() + # ignore warning message
  tm_layout(main.title = "GridMET cell with FOCO", main.title.size = 1)

tmap_arrange(state_and_cell, cell_and_city)


######### STOP HERE AND DOWNLOAD DATA FROM https://datagateway.nrcs.usda.gov/GDGOrder.aspx #################################################

# First, import DEM and soils layers into ArcGIS.
# Join gSSURGO soils raster (MapunitRaster_10m) with valu1 table
# Get UTM projection from DEM and reproject soils raster into appropriate projection
# Save new raster into park folder (NOT file geodatabase)
# NOTE: Soils raster cannot be reprojected in R because will lose data associated with RAT (Raster Attribute Table)

dem <- raster('./Water_Balance_Model_Update/FortCollinsCO/elevation_NED30M_co069_3829101_01/elevation/ned30m40105.tif') # DEM 30 m downloaded from USDA NRCS
soil <- raster('./Water_Balance_Model_Update/FOrtCollinsCO/soils_fc') # projected raster file exported from ArcGIS (MapunitRaster_10m) with spatial join to valu1 table
soil@data@attributes[[1]] # check that RAT looks OK

# Project spatial data

dem_projection <- crs(dem) # raster 



foco_proj <- spTransform(foco, CRSobj = dem_projection)
ext <- projectExtent(grid_cell, crs = dem_projection) # create extent object to crop rasters

soil_crop <- crop(soil, ext) 


#####   SLOPE, ASPECT AND RANDOM POINTS   ##########################################################################################

# Create slope and aspect rasters

slope <- terrain(dem, opt = "slope", unit = "degrees", neighbors = 4) # 4 is better for "smooth" surfaces; 8 is better for rough. See https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/terrain
plot(slope) # check slope looks OK
aspect <- terrain(dem, opt = "aspect", unit = "degrees")
plot(aspect) # check aspect looks OK

# Crop to projected MACA cell 

dem_crop <- crop(dem, ext)
slope_crop <- crop(slope, ext)
aspect_crop <- crop(aspect, ext)

head(soil_crop@data@attributes) # check to see that RAT followed through processing




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

i <- extract(soil_crop, foco_proj) 
pointSoil <- factorValues(soil_crop, i)

# reproject points to lat/long so can eventually add to .csv

latlong <- st_as_sf(foco_proj) # convert to sf object 
latlong <- st_transform(latlong, crs = 4326) # project to lat/long


sites <- as.data.frame(st_coordinates(latlong)) # begin new dataframe for sites

sites[,3] <- extract(dem, foco_proj)
sites[,4] <- extract(aspect_crop, foco_proj)
sites[,5] <- extract(slope_crop, foco_proj)
sites[,6] <- pointSoil$AWS0_999
sites[,7] <- "Fort Collins"
sites[,8] <- 5 # default value for wind
sites[,9] <- 0 # default value for snowpack
sites[,10] <- 0 # default value for Soil.Init
sites[,11] <- 1 # default value for shade coefficient
  
sites <- select(sites, 7,2,1,3:6, 8:11) # reorder columns
colnames(sites) <- c("SiteID", "Lat", "Lon", "Elev", "Aspect", "Slope", "SWC.Max", "Wind", "Snowpack", "Soil.Init", "Shade.Coeff")

sites # check 

write.csv(sites, file =  paste0(OutDir, site, "_site_characteristics.csv"), row.names = FALSE)




