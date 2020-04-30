##############################################
###   Test - Jennings Coefficient   ##########
##############################################

rm(list = ls())

library(raster)

setwd('C:/Users/adillon/Documents/Repos/WaterBalance')

## Jennings Coefficient - Site 1 ## WORKS

j <- raster('./WaterBalance/merged_jennings2.tif') # Jennings coefficient layer
Sites = read.csv("C:/Users/adillon/Documents/RSS/WICA/WICA_site_parameters_jennings.csv")

Lat = Sites$Lat[1]
Lon = Sites$Lon[1]
projection <- CRS("+init=epsg:4326") # Lat/Long

coords <- cbind(Sites$Lon[1], Sites$Lat[1])
sp <- SpatialPoints(coords, proj4string = projection)

jennings.Coeff = extract(j, sp) # The Jennings coefficient = temperature where precip is half snow, half rain

