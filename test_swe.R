 #############################################################################
######    TEST SNOW-WATER EQUIVALENT AGAINST D. THOMA'S EXCEL MODEL  ########
###############################################################################

# Annie Kellner 
# 11/13/2020

# PACK (D. Thoma) = init.value - melt + snowfall

rm(list = ls())

library(lubridate)

# ------ Load and prep data ----------------------------------------------------------------- #

frog <- read.csv("C:/Users/adillon/Documents/Water_Balance_Update/frog_rock_for_R.csv")
head(frog)
colnames(frog) <- c("year", "yday", "dayl", "prcp", "srad", "swe", "tmaxC", "tminC", "vp")

params <- read.csv("C:/Users/adillon/Documents/Water_Balance_Update/Site_params_Frog_Rock.csv")

frog$tmean_C <- (frog$tmaxC + frog$tminC)/2

# ----- Functions ------------------------------------------------------------------------------ #

#' Freeze factor with Jennings Coefficient
#'
#' Calculates a freeze factor from 0-1 based on mean temperature and Jennings coefficient
#' @param tmean A vector of daily mean temperatures (deg C).
#' @param high_thresh_temperatures Jennings coefficient + 3 
#' @param low_thresh_temperatures Jennings coefficient - 3
#' @export
#' get_freeze()

get_freeze_jennings = function(tmean, high_thresh_temperature, low_thresh_temperature){ # redefining get_freeze to incorporate Jennings Coefficient (* double check R will pull 'global' function and not package function)
  freeze = ifelse(tmean <= low_thresh_temperature, 0, ifelse(tmean >= high_thresh_temperature, 1, (1/(high_thresh_temperature - low_thresh_temperature))*(tmean - low_thresh_temperature)))
  return(freeze)
}

low_thresh_temperature = params$Jennings.Coeff - 3 # This sets up a 6 degree span, which corresponds to the 1/6 = 0.167 precip fraction.
high_thresh_temperature = params$Jennings.Coeff + 3

frog$freeze <- get_freeze_jennings(frog$tmeanC, high_thresh_temperature, low_thresh_temperature)

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

frog$snow <- get_snow(frog$prcp, frog$freeze)


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

frog$pack <- get_snowpack(frog$prcp, frog$freeze, p.0 = NULL)

#' Melt
#'
#' Calculates the amount of snowmelt at time steps from snowpack, snowfall, and freeze factor.
#' @param snowpack A time series vector of snowpack accumulation values.
#' @param snow A time series vector of snowfall values.
#' @param freeze A vector of freeze factor values, calculated from Tmean. Values are 0-1.
#' @param p.0 (optional) Initial snowpack value. Default is 0.
#' @export
#' get_snowpack()

get_melt = function(snowpack, snow, freeze, p.0=NULL){
  p.i = ifelse(!is.null(p.0), p.0, 0)
  melt = c()
  for(i in 1:length(snowpack)){
    melt[i] = freeze[i]*(p.i + snow[i])
    p.i = snowpack[i]
  }
  return(melt)
}

frog$melt <- get_melt(frog$pack, frog$snow, frog$freeze, p.0 = NULL)

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

frog$rain <- get_rain(frog$prcp, frog$freeze)


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

frog$w <- frog$melt + frog$rain

#' Potential Evapotranspiration (PET)
#' 
#' Calculates potential evapotranspiration from Hamon PET, heat load, and shade coefficient
#' @param pet A time series vector of PET
#' @param heat_load A constant (REVISE)
#' @param shade_coefficient Specified in params script (REVISE)

get_PET = function(ET_Hamon, heat_load, shade_coeff){
  pet = ET_Hamon * heat_load * shade_coeff
  return(pet)
}

#' Hamon Daily PET
#'
#' Calculates Hamon PET from a daily time series of Tmean and daylength.
#' @param x A daily time series data frame containing tmean_C (deg C), and daylength (seconds) * NOTE CHANGE FROM HOURS IN ORIGINAL SCRIPT
#' @export
#' ET_Hamon_daily()

ET_Hamon_daily = function(x){
  et.hamon = 0.1651*((x$dayl/3600)/12)*(216.7*(6.108*exp((17.26*x$tmean_C)/(x$tmean_C+273.3))))/(x$tmean_C+273.3)
  return(et.hamon)
}

frog$Hamon <- ET_Hamon_daily(frog)



write.csv(frog, "C:/Users/adillon/Documents/Water_Balance_Update/SWE/pack_R.csv")




