######################################
##    TESTS   ########################
######################################

rm(list=ls())

# Load data

ex_params <- read.csv("C:/Users/adillon/Documents/Water_Balance_Update/Example_sites.csv")
ex_daymet <- read.csv("C:/Users/adillon/Documents/Water_Balance_Update/FortCollinsCO_daymet_R.csv")


# ------  FUNCTIONS ------------------------------------------------------- #

#' Calculates extraterrestrial Solar Radiation (MJ m^-2 day^-1) based on day-of-year and latitude
#' @param doy Day-of-year (Julian date)
#' @param lat Latitude (degrees)
#' @export
#' extraterrestrial_solar_rad()

extraterrestrial_solar_rad = function(doy, lat){ #radRa - column AK
  d.r = 1 + 0.033*cos(((2*pi)/365)*doy) # cell AG9
  declin = 0.409*sin((((2*pi)/365)*doy)-1.39) # cell AH9
  lat.rad = (pi/180)*lat
  sunset.ang = acos(-tan(lat.rad)*tan(declin)) #cell AJ9
  R.a = ((24*60)/pi)*0.0820*d.r*(sunset.ang*sin(lat.rad)*sin(declin) + cos(lat.rad)*cos(declin)*sin(sunset.ang))
  return(R.a)
}

#' Oudin PET with radiation calculated from latitude and day-of-year
#' Equation 3 in Oudin et al. 2010
#' 
#' @param x A daily time series data frame containing tmean_C (deg C)
#' @param R.a. Extraterrestrial solar radiation
#' @param snowpack A time series vector of snowpack accumulation values. ### ACTUALLY THIS IS SNOW WATER EQUIVALENT, MAYBE NOT SNOWPACK. LOOK INTO SNOW WATER EQUIVALENT RECONCILIATION NEXT WEEK (WRITTEN 6/17/2020)

ET_Oudin = function(x, R.a., snowpack){ 
  et.oudin = c()
  for(i in 1:nrow(x)){
    top = R.a.[i] * (x$tmean_C[i] + 5) * 0.408
    bottom = 100
    PET = top/bottom
    et.oudin[i] = ifelse(snowpack[i] > 2,0, ifelse(x$tmean_C[i] > -5, PET, 0))} 
  return(et.oudin)
}

ET_Oudin_daymet_Ra = function(x, snowpack){
  et.oudin = c()
  for(i in 1:nrow(x)){
    top = x$srad[i] * x$daylength[i]/1000000 * (x$tmean_C[i] + 5) * 0.408
    bottom = 100
    PET = top/bottom
    et.oudin[i] = ifelse(snowpack[i] > 2, 0, ifelse(x$tmean_C[i] > -5, PET, 0))}
  return(et.oudin)
}

# --------------------------------------------------------------------------------------------------------------------------------------------- #
R.a. <- extraterrestrial_solar_rad(ex_daymet$yday[1], ex_params$Lat)
ex_daymet$tmean_C <- (ex_daymet$tmax..deg.c.+ex_daymet$tmin..deg.c.)/2
ex_daymet$snowpack <- 0
snowpack <- 0
ET_Oudin(ex_daymet,R.a.,snowpack)


# USE THIS CODE AS AN EXAMPLE!! 
get_snowpack = function(ppt, freeze, p.0=NULL){
  p.i = ifelse(!is.null(p.0), p.0, 0) 
  snowpack = c()
  for(i in 1:length(ppt)){
    snowpack[i] = ((1-freeze[i])^2)*ppt[i] + (1-freeze[i])*p.i
    p.i = snowpack[i]
  }
  return(snowpack)
}
