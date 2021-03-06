###################################################################################
## ET_functions.R
## Functions to calculate potential evapotranspiration from input climate data. Based on Dave Thoma's water balance Excel spreadsheet model.
## Created 10/30/2019 by ARC
## v01 - Includes Hamon daily and Thornthwaite monthly calculations
## v01.01 - Added Penman-Monteith equations and dependent functions (ARC - 11/06/19)
## Future updates - adding more ET calculation methods
###################################################################################

#' Daylength
#'
#' Returns daylength in hours for a series of dates, based on latitude. Calls the 'geosphere' package.
#' @param dates A series of dates containing year, month, and day
#' @param lat Latitude (degrees)
#' @export
#' get_daylength()

get_daylength = function(dates, lat){
  yday = as.numeric(strftime(dates, "%j"))
  dayl_h = geosphere::daylength(lat, yday)
  return(dayl_h)
}

#' Saturation Vapor Pressure
#'
#' Calculates mean saturation vapor pressure (kPa) of air based on temperature (deg C).
#' @param temp Temperature (deg C)
#' @export
#' get_svp()

get_svp = function(temp){
  svp = 0.6108*exp((17.27*temp)/(temp + 237.3))
  return(svp)
}

#' Relative Humidity
#'
#' Calculates relative humidity (%) from atmospheric vapor pressure and temperature
#' @param vp Vapor pressure (kPa)
#' @param temp Temperature (deg C)
#' @export
#' get_rh()

get_rh = function(vp, temp){
  svp = get_svp(temp)
  rh = vp/svp
  return(rh)
}

#' Actual Vapor Pressure
#'
#' Calculates actual vapor pressure (kPa) of air based on maximum and minimum relative humidity and maximum and minimum temperature.
#' @param rhmax Daily maximum relative humidity (percent).
#' @param rhmin Daily minimum relative humidity (percent).
#' @param tmax Daily maximum temperature (deg C).
#' @param tmin Daily minimum temperature (deg C).
#' @export
#' actual_vp()

actual_vp = function(rhmax, rhmin, tmax, tmin){
  e.tmax = get_svp(tmax)
  e.tmin = get_svp(tmin)
  e.a = (e.tmin*(rhmax/100) + e.tmax*(rhmin/100))/2
  return(e.a)
}

#' Slope of Saturation Vapor Curve
#'
#' Calculates the slope of the saturation vapor curve for a given temperature.
#' @param temp A time series vector or single value of temperatures (deg C).
#' @export
#' vapor_curve()

vapor_curve = function(temp){
  vap.curve = 4098*(0.6108*exp((17.27*temp/(temp+237.3)))/(temp+273.3)^2)
  return(vap.curve)
}

#' Atmospheric Pressure
#'
#' Estimates atmospheric pressure (kPa) at a given elevation.
#' @param elev Elevation (m).
#' @export
#' atm_press()

atm_press = function(elev){
  atm.press = 101.3*((293 - 0.0065*elev)/293)^5.26
  return(atm.press)
}

#' Psychrometric Constant
#'
#' Calculates the psychrometric constant relating partial pressure of water in air to the air temperature, based on atmospheric pressure. Calls the atm_press() function to estimate atmospheric pressure from elevation.
#' @param elev Elevation (m)
#' @export
#' psyc_constant()

psyc_constant = function(elev){
  atm.press = atm_press(elev)
  psyc.const = 0.000665*atm.press
  return(psyc.const)
}

#' Clear Sky Radiation
#'
#' Calculates incoming clear-sky radiation (MJ m^-2 day^-1) based on day-of-year, latitude, and elevation
#' @param doy Day-of-year (Julian date)
#' @param lat Latitude (degrees)
#' @param elev Elevation (m)
#' @export
#' clear_sky_rad()

clear_sky_rad = function(doy, lat, elev){
  d.r = 1 + 0.033*cos(((2*pi)/365)*doy)
  declin = 0.409*sin((((2*pi)/365)*doy)-1.39)
  lat.rad = (pi/180)*lat
  sunset.ang = acos(-tan(lat.rad)*tan(declin))
  R.a = ((24*60)/pi)*0.0820*d.r*(sunset.ang*sin(lat.rad)*sin(declin) + cos(lat.rad)*cos(declin)*sin(sunset.ang))
  R.so = (0.75 + 2e-5*elev)*R.a
  return(R.so)
}

#' Outgoing Radiation
#'
#' Calculates outgoing radiation (MJ m^-2 day^-1) based on daily Tmax, Tmin, incoming radiation, actual vapor pressure, and clear-sky radiation.
#' @param tmax Daily maximum temperatures (deg C).
#' @param tmin Daily minimum temperatures (deg C).
#' @param R.s Incoming solar radiation (MJ m^-2 day^-1).
#' @param e.a Actual vapor pressure (kPa).
#' @param R.so Clear-sky radiation (MJ m^-2 day^-1).
#' @export
#' outgoing_rad()

outgoing_rad = function(tmax, tmin, R.s, e.a, R.so){
  R.nl = 4.903e-09*(((tmax + 273.16)^4 + (tmin + 273.16))/2)*(0.34-0.14*sqrt(e.a))*(1.35*(R.s/R.so) - 0.35)
  return(R.nl)
}

################################ ET Calculation Methods ##########################################

#' Hamon Daily PET
#'
#' Calculates Hamon PET from a daily time series of Tmean and daylength.
#' @param x A daily time series data frame containing tmean_C (deg C), and daylength (hours)
#' @export
#' ET_Hamon_daily()

ET_Hamon_daily = function(x){
  et.hamon = 0.1651*(x$daylength/12)*(216.7*(6.108*exp((17.26*x$tmean_C)/(x$tmean_C+273.3))))/(x$tmean_C+273.3)
  return(et.hamon)
}

#' Thornthwaite Monthly PET
#'
#' Calculates PET from monthly Tmean and daylength, according to the Thornthwaite method.
#' @param x A monthly time series data frame containing Date, tmean_C (deg C), and daylength (hours)
#' @export
#' ET_Thorn_monthly()

ET_Thorn_monthly = function(x){
  x$month = strftime(x$Date, "%m")
  N = lubridate::days_in_month(as.numeric(x$month))
  e.s = get_svp(x$tmean_C)
  et.thorn = ifelse(x$tmean_C > 0, 29.8*N*x$daylength*(e.s/(x$tmean_C+273.2)), 0)
  return(et.thorn)
}

#' Penman-Monteith Daily PET
#'
#' Calculates PET (mm) from daily Tmax, Tmin, solar radiation, elevation, and latitude, according to the Penman-Monteith method. May also use daily maximum and minimum relative humidity, atmospheric vapor pressure, and wind speeds.
#' @param x A daily time series data frame containing Date (date object), tmax_C (deg C), tmin_C (deg C), srad (MJ m^-2 day^-1). Optionally contains RHmax (percent), RHmin (percent), vp (kPa), and wind (m/s).
#' @param elev Elevation of the site (m).
#' @param lat Latitude of the site (degrees).
#' @param wind (optional) An estimated value for daily average wind speeds (m/s). Use if input data frame does not contain daily wind speed values.
#' @export
#' ET_PenmanMonteith_daily()

ET_PenmanMonteith_daily = function(x, elev, lat, wind=NULL){
  #Inputs
  tmax = x$tmax_C
  tmin = x$tmin_C
  tmean = (tmax + tmin)/2
  doy = as.numeric(strftime(x$Date, "%j"))
  rh.max = x$RHmax
  rh.min = x$RHmin
  vp = x$vp
  R.s = x$srad
  u = ifelse(is.null(wind) == TRUE, x$wind, wind)
  psyc.const = psyc_constant(elev)
  vap.curve = vapor_curve(tmean)

  #Auxilary calculations for wind terms
  DT = vap.curve/(vap.curve + psyc.const*(1+0.34*u))
  PT = psyc.const/(vap.curve + (psyc.const*(1+0.34*u)))
  TT = (900/(tmean + 273))*u

  #Saturation vapor pressure
  e.tmax = get_svp(tmax)
  e.tmin = get_svp(tmin)
  e.s = (e.tmax + e.tmin)/2

  #Actual vapor pressure
  if(is.null(vp) == TRUE){
    if(is.null(rh.max) == TRUE){
      e.a = e.tmin
    } else {
      e.a = actual_vp(rh.max, rh.min)
    }
  } else {
    e.a = vp
  }

  #Solar angle and radiation calculations
  R.ns = (1 - 0.23)*R.s
  R.so = clear_sky_rad(doy, lat, elev)
  R.nl = outgoing_rad(tmax, tmin, R.s, e.a, R.so)
  R.n = R.ns - R.nl
  R.ng = 0.408*R.n

  #ET from radiation
  ET.rad = DT*R.ng
  #ET from wind
  ET.wind = PT*TT*(e.s - e.a)
  #Total ET
  ET.o = ET.rad + ET.wind
  return(ET.o)
}

#' Oudin Daily PET
#'
#' Calculates PET (mm) based on temperature, latitude, and solar radiation 
#' @param doy Day-of-year (Julian date)
#' @param lat Latitude of the site (degrees).
#' @param snowpack A time series vector of snowpack accumulation values.
#' @param tmean A vector of daily mean temperatures (deg C).
#' @param slope Slope of the site (in degrees).
#' @param aspect Aspect of the site (in degrees).
#' @param shade.coeff (optional) A shade coefficient from 0-1. Default is 1.
#' @export
#' get_OudinPET()

get_OudinPET = function(doy, lat, snowpack, tmean, slope, aspect, shade.coeff=NULL){
  d.r = 1 + 0.033*cos((2*pi/365)*doy)
  declin = 0.409*sin((((2*pi)/365)*doy)-1.39)
  lat.rad = (pi/180)*lat
  sunset.ang = acos(-tan(lat.rad)*tan(declin))
  R.a = ((24*60)/pi)*0.082*d.r*((sunset.ang*sin(lat.rad)*sin(declin)) + (cos(lat.rad)*cos(declin)*sin(sunset.ang)))
  Oudin = ifelse(snowpack>2,0,ifelse(tmean>-5,(R.a*(tmean+5)*0.408)/100,0))
  Folded_aspect = abs(180-abs((aspect)-225))
  Heatload = (0.339+0.808*cos(lat*(pi/180))*cos(slope*(pi/180)))-(0.196*sin(lat.rad)*sin(slope*(pi/180)))-(0.482*cos(Folded_aspect*(pi/180))*sin(slope*(pi/180)))
  sc = ifelse(!is.null(shade.coeff), shade.coeff, 1)
  OudinPET = Oudin * Heatload * sc
  return(OudinPET)
}
