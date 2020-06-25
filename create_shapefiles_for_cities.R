########################################################################################
##          CONVERT POINTS TO SHAPEFILE      ###########################################
########################################################################################

rm(list = ls())

library(sf)

# import shapefiles created in ArcGIS
fc <-st_read("C:/Users/adillon/Documents/ArcGIS/Water_Balance_Model_Update/FortCollins.shp")
g <- st_read("C:/Users/adillon/Documents/ArcGIS/Water_Balance_Model_Update/Glencoe.shp")
i <- st_read("C:/Users/adillon/Documents/ArcGIS/Water_Balance_Model_Update/Ithaca.shp")

# set CRS

st_crs(fc) <- 4326
st_crs(g) <- 4326
st_crs(i) <- 4326

# Reproject to appropriate UTM zone

fc <- st_transform(fc, 2151) # UTM zone 13N
g <- st_transform(g, 3160) # UTM zone 16N
i <- st_transform(i, 2149) # UTM zone 18N

# Write shapefile

st_write(fc, "C:/Users/adillon/Documents/ArcGIS/Water_Balance_Model_Update/fc_13N.shp")
st_write(g, "C:/Users/adillon/Documents/ArcGIS/Water_Balance_Model_Update/Glencoe_16N.shp")       
st_write(i, "C:/Users/adillon/Documents/ArcGIS/Water_Balance_Model_Update/Ithaca_18N.shp")
