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

frog <- read.csv("C:/Users/msears/OneDrive - DOI/WB-cross check/FrogRock-inputforR.csv")

frog <- frog %>%
  mutate(tmean = (tmin_degC+tmax_degC)/2)

# ----- Functions ------------------------------------------------------------------------------ #

#' Freeze factor with Jennings Coefficient
#'
#' Calculates a freeze factor from 0-1 based on mean temperature and Jennings coefficient
#' @param tmean A vector of daily mean temperatures (deg C).
#' @param high_thresh_temperatures Jennings coefficient + 3 
#' @param low_thresh_temperatures Jennings coefficient - 3
#' @export
#' get_freeze()

get_freeze_jennings = function(tmean, high_thresh_temperature, low_thresh_temperature){ 
  freeze = ifelse(tmean <= low_thresh_temperature, 0, ifelse(tmean >= high_thresh_temperature, 1, (1/(high_thresh_temperature - low_thresh_temperature))*(tmean - low_thresh_temperature)))
  return(freeze)
}

low_thresh_temperature = (2.155578-3.0) # the first numbers in these lines come from D. Thoma's parameter/input Frog Rock xlsx
high_thresh_temperature = (2.15578+3.0) 

frog$freeze <- get_freeze_jennings(frog$tmean, high_thresh_temperature, low_thresh_temperature)

#' Snow
#'
#' Calculates snowfall totals based on precipitation and freeze factor
#' @param ppt A vector of precipitation values.
#' @param freeze A vector of freeze factor values, calculated from Tmean. Values are 0-1.
#' @export
#' get_snow()


frog_input$snow <- (1-frog_input$freeze)*frog_input$prcp


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

frog_input$pack <- get_snowpack(frog_input$prcp, frog_input$freeze, p.0 = NULL)

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

frog_input$melt <- get_melt(frog_input$pack, frog_input$snow, frog_input$freeze, p.0 = NULL)

write.csv(frog_input, "C:/Users/msears/OneDrive - DOI/WB-cross check/SWE_output")