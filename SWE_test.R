#############################################################################
######    TEST SNOW-WATER EQUIVALENT AGAINST D. THOMA'S EXCEL MODEL  ########
#############################################################################

# Annie Kellner
# 11/13/2020
#Megan Sears picked up 1/13/2021

# PACK (D. Thoma) = init.value - melt + snowfall #
rm(list = ls())

library(lubridate)
library(dplyr)

# ------ Load and prep data ----------------------------------------------------------------- #

frog_input <- read.csv("C:/Users/msears/OneDrive - DOI/WB-cross check/Frogrock_data.csv")

colnames(frog_input) <- c("year", "yday", "dayl", "prcp", "srad", "swe", "tmaxC", "tminC", "vp", "soilWHC", "slope", "aspect", "jennings", "hock", "GDDbase", "shadecoeff","%runoff")

frog_input$tmean <- (frog_input$tmaxC + frog_input$tminC)/2

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

frog$freeze <- get_freeze_jennings(frog$tmean, high_thresh_temperature, low_thresh_temperature)

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

frog$snow <- get_snow(frog$prcp, freeze)


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

frog$pack <- get_snowpack(frog$prcp, freeze, p.0 = NULL)

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

frog$melt <- get_melt(pack, snow, freeze, p.0 = NULL)

write.csv(pack, "C:/Users/adillon/Documents/Water_Balance_Update/SWE/pack_R.csv")

