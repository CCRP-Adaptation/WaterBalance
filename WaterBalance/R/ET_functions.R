###################################################################################
## ET_functions.R
## Functions to calculate potential evapotranspiration from input climate data. Based on Dave Thoma's water balance Excel spreadsheet model.
## Created 10/30/2019 by ARC
## v01 - Includes Hamon daily and Thornthwaite monthly calculations
## Future updates - adding more ET calculation methods
###################################################################################

#' Daylength
#'
#' Returns daylength in hours for a series of dates, based on latitude. Calls the 'geosphere' package.
#' @param dates A series of dates containing year, month, and day
#' @param lat Latitude (degrees)
#' get_daylength()
#' @export
#'
get_daylength = function(dates, lat){
  yday = as.numeric(strftime(dates, "%j"))
  dayl_h = geosphere::daylength(lat, yday)
  return(dayl_h)
}

#' Saturation Vapor Pressure
#'
#' Calculates mean saturation vapor pressure of air based on temperature.
#' @param temp A vector or single value of temperatures (deg C)
#' sat_vp()
#' @export
#'
sat_vp = function(temp){
  e.s = 0.6108*exp((17.27*temp)/(temp + 237.3))
  return(e.s)
}

#' Hamon Daily PET
#'
#' Calculates Hamon PET from a daily time series of Tmean and daylength.
#' @param x A daily time series data frame containing tmean_C (deg C), and daylength (hours)
#' ET_Hamon_daily()
#' @export
#'
ET_Hamon_daily = function(x){
  et.hamon = 0.1651*(x$daylength/12)*(216.7*(6.108*exp((17.26*x$tmean_C)/(x$tmean_C+273.3))))/(x$tmean_C+273.3)
  return(et.hamon)
}

#' Thornthwaite Monthly PET
#'
#' Calculates Thornthwaite PET from a monthly time series of Tmean and daylength.
#' @param x A monthly time series data frame containing Date, tmean_C (deg C), and daylength (hours)
#' ET_Thorn_monthly()
#' @export
#'
ET_Thorn_monthly = function(x){
  x$month = strftime(x$Date, "%m")
  N = lubridate::days_in_month(as.numeric(x$month))
  e.s = sat_vp(x$tmean_C)
  et.thorn = ifelse(x$tmean_C > 0, 29.8*N*x$daylength*(e.s/(x$tmean_C+273.2)), 0)
  return(et.thorn)
}
