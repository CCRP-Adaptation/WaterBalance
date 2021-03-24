###################################################################################
## WB_functions.R
## Functions to calculate water balance variables from input climate data. Based on Dave Thoma's water balance Excel spreadsheet model.
## Created 10/30/2019 by ARC, Updated 3/24/21 by MGS
## v1.1.0
###################################################################################

#' Temperature threshold using Jennings et al. 2018 to partition rain and snow
#'
#' Extracts the rain-snow temperature threshold from a raster.
#' @param Lon Longitude of the site (degrees).
#' @param Lat Latitude of the site (degrees).
#' @export
#' get_jtemp()

get_jtemp = function(Lon, Lat){
  j.raster = raster::raster(system.file("extdata","merged_jennings2.tif", package="WaterBalance")) 
  projection = sp::CRS("+init=epsg:4326")
  coords = cbind(Lat, Lon)
  sp = sp::SpatialPoints(coords, proj4string = projection)
  j_temp = raster::extract(j.raster, sp)
  return(j_temp)
}

#' Freeze factor using Jennings et al. 2018 thresholds to partition rain and snow
#'
#' Calculates a freeze factor from 0-1 based on a temperature threshold from Jennings et al. 2018 and average temperature.
#' @param j_temp the Jennings temperature extracted from the raster based on latitude and longitude
#' @param tmean A vector of daily mean temperatures (deg C).
#' @export
#' get_freeze()

get_freeze = function(j_temp, tmean){
  low_thresh_temp = j_temp - 3
  high_thresh_temp = j_temp + 3
  freeze = ifelse(
    tmean <= low_thresh_temp, 0, 
    ifelse(tmean >= high_thresh_temp,
           1, (0.167*(tmean-low_thresh_temp))))
  return(freeze)
}

#' Rain
#'
#' Calculates rainfall totals based on precipitation and freeze factor
#' @param ppt A vector of precipitation values.
#' @param freeze A vector of freeze factor values, calculated from Tmean. Values are 0-1.
#' @export
#' get_rain()

get_rain = function(ppt, freeze){
  rain = ppt*freeze
  return(rain)
}

#' Snow
#'
#' Calculates snowfall totals based on precipitation and freeze factor
#' @param ppt A vector of precipitation values.
#' @param freeze A vector of freeze factor values, calculated from Tmean. Values are 0-1.
#' @export
#' get_snow()

get_snow = function(ppt, freeze){
  snow = (1 - freeze)*ppt
  return(snow)
}

#' Melt
#'
#' Calculates the amount of snowmelt at time steps from snowpack, temperature, and Hock melt factor
#' @param tmean A vector of daily mean temperatures (deg C).
#' @param j_temp the Jennings temperature extracted from the raster based on latitude and longitude
#' @param hock A melt factor of daily snowmelt when warm enough to melt.
#' @param snow A time series vector of snowfall values.
#' @param snowpack A time series vector of snowpack accumulation values. Initial snowpack default value is 0.
#' @export
#' get_melt()

get_melt = function(tmean,j_temp, hock, snow, sp.0=NULL){
  sp.0 = ifelse(!is.null(sp.0), sp.0, 0)
  low_thresh_temp = j_temp - 3
  melt <- vector()
  for (i in 1:1){
    melt[i] = ifelse(tmean[i]<low_thresh_temp||sp.0==0, 0, 
                     ifelse(((tmean[i]-low_thresh_temp)*hock[i])>sp.0, 
                            sp.0, ((tmean[i]-low_thresh_temp)*hock[i])))
  }
  snowpack = sp.0+snow+melt
  for(i in 2:length(tmean)){
    for (j in 2:(length(tmean))){
      melt[i] = ifelse(tmean[i]<low_thresh_temp||snowpack[i-1]==0, 0, 
                       ifelse(((tmean[i]-low_thresh_temp)*hock[i])>snowpack[i-1], 
                              snowpack[i-1], ((tmean[i]-low_thresh_temp)*hock[i])))
      snowpack[j] = snowpack[j-1]+snow[j]-melt[j]
    }
  }
  return(melt)
}

#' Snowpack
#'
#' Calculates snowpack accumulation at time steps, from a time series of precipitation values, freeze factor, and an initial snowpack value
#' @param ppt A vector of precipitation values.
#' @param freeze A vector of freeze factor values, calculated from Tmean. Values are 0-1.
#' @param p.0 (optional) Initial snowpack value. Default is 0.
#' @export
#' get_snowpack()

get_snowpack = function(ppt, freeze, p.0=NULL){
  p.i = ifelse(!is.null(p.0), p.0, 0)
  snowpack = c()
  for(i in 1:length(ppt)){
    snowpack[i] = ((1-freeze[i])^2)*ppt[i] + (1-freeze[i])*p.i
    p.i = snowpack[i]
  }
  return(snowpack)
}

#' Modify PET
#'
#' Modifies PET by heat load according to method by Lutz et al. (2010)
#' @param pet A time series vector of PET values.
#' @param slope Slope of the site (in degrees).
#' @param aspect Aspect of the site (in degrees).
#' @param lat Latitude of the site (in degrees).
#' @param freeze A vector of freeze factor values, calculated from Tmean. Values are 0-1.
#' @param shade.coeff (optional) A shade coefficient from 0-1. Default is 1.
#' @export
#' modify_PET()

modify_PET = function(pet, slope, aspect, lat, freeze, shade.coeff=NULL){
  f.aspect = abs(180 - abs(aspect - 225))
  lat.rad = ifelse(lat > 66.7, (66.7/180)*pi, (lat/180)*pi)
  slope.rad = (slope/180)*pi
  aspect.rad = (f.aspect/180)*pi
  heat.load = 0.339+0.808*cos(lat.rad)*cos(slope.rad) - 0.196*sin(lat.rad)*sin(slope.rad) - 0.482*cos(aspect.rad)*sin(slope.rad)
  sc = ifelse(!is.null(shade.coeff), shade.coeff, 1)
  freeze = ifelse(freeze == 0,0,1)
  PET.Lutz = pet*heat.load*sc*freeze
  return(PET.Lutz)
}

#' Soil Water Content (SWC)
#'
#' Calculates soil water content from available water (rain + snowmelt), PET, max. water-holding capacity, and initial SWC.
#' @param w  A time series vector of available water for soil charging (rain + snowmelt).
#' @param pet A time series vector of PET.
#' @param swc.max The maximum soil water-holding capacity of the soil layer being assessed.
#' @param swc.0 (optional) The initial soil water content value. Default is 0.
#' @export
#' get_soil()

get_soil = function(w, pet, swc.max, swc.0){
  swc.i = ifelse(!is.null(swc.0), swc.0, 0)
  soil = c()
  for(i in 1:length(w)){
    soil[i] = ifelse(w[i] > pet[i], min(w[i]-pet[i]+swc.i, swc.max), swc.i*exp(-(pet[i]-w[i])/swc.max))
    swc.i = soil[i]
  }
  return(soil)
}

#' Actual Evapotranspiration (AET)
#'
#' Calculates actual evapotranspiration (AET) from available water, PET, and soil water.
#' @param w  A time series vector of available water for soil charging (rain + snowmelt).
#' @param pet A time series vector of PET.
#' @param swc A time series vector of soil water content.
#' @param swc.0 (optional) The initial soil water content value. Default is 0.
#' @export
#' get_AET()

get_AET = function(w, pet, swc, swc.0){
  swc.i = ifelse(!is.null(swc.0), swc.0, 0)
  AET = c()
  for(i in 1:length(w)){
    AET[i] = ifelse(w[i] > pet[i], pet[i], w[i]+swc.i-swc[i])
    swc.i = swc[i]
  }
  return(AET)
}

#' Growing Degree-Days
#'
#' Calculates growing degree-days at daily time steps based on mean temperature and a threshold temperature
#' @param tmean A time series vector of daily mean temperatures (deg C)
#' @param tbase (optional) A threshold temperature, above which growing degree-days are calculated. Default is 0.
#' @export
#' get_GDD()

get_GDD = function(tmean, tbase){
  tb = ifelse(!is.null(tbase), tbase, 0)
  GDD = ifelse(tmean < tbase, 0, tmean - tb)
  return(GDD)
}
