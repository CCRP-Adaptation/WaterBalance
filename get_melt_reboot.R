###################################################################
#####   IMPROVING EFFICIENCY OF GET_MELT  #########################
###################################################################

rm(list = ls())

#frog <- read.csv('./data/raw-data/frog_rock_for_R.csv')
#colnames(frog) <- c("year", "yday", "dayl", "prcp", "srad", "swe", "tmaxC", "tminC", "vp")

#params <- read.csv("C:/Users/adillon/Documents/Water_Balance_Update/Site_params_Frog_Rock.csv")

#frog$tmean_C <- (frog$tmaxC + frog$tminC)/2

# Test more data

j_temp = 1.381265 # MACA value


# Get freeze

get_freeze = function(j_temp, tmean){
  low_thresh_temp = j_temp - 3
  high_thresh_temp = j_temp + 3
  freeze = ifelse(
    tmean <= low_thresh_temp, 0, 
    ifelse(tmean >= high_thresh_temp,
           1, (0.167*(tmean-low_thresh_temp))))
  return(freeze)
}

frog$freeze <- get_freeze(j_temp = 2.155578, tmean = frog$tmean_C)

# Get snow

get_snow = function(ppt, freeze){
  snow = (1 - freeze)*ppt
  return(snow)
}

frog$snow <- get_snow(ppt = frog$prcp, freeze = frog$freeze)


#' Melt
#'
#' Calculates the amount of snowmelt at time steps from snowpack, temperature, and Hock melt factor
#' @param tmean A vector of daily mean temperatures (deg C).
#' @param j_temp the Jennings temperature extracted from the raster based on latitude and longitude.
#' @param hock A melt factor of daily snowmelt when warm enough to melt.
#' @param snow A time series vector of snowfall values.
#' @param sp.0 (optional) Initial snowpack value. Default is 0.
#' @export
#' get_melt()

get_melt = function(tmean,j_temp, hock, snow, sp.0=NULL){
  sp.0 = ifelse(!is.null(sp.0), sp.0, 0)
  low_thresh_temp = j_temp - 3
  melt_delta = (tmean-low_thresh_temp)*hock
  melt_delta = ifelse(melt_delta < 0, 0, melt_delta)
  melt <- vector()
  melt[1] = ifelse(tmean[1] < low_thresh_temp||sp.0==0, 0,
                        ifelse(melt_delta[1]>sp.0, 
                               sp.0, melt_delta[1]))
  snowpack <- vector()
  snowpack[1] = sp.0 + snow[1] - melt[1]
  for(i in 2:length(tmean)){
    melt[i] = ifelse(tmean[i]<low_thresh_temp | snowpack[i-1]==0, 0, 
                     ifelse(melt_delta[i]>snowpack[i-1], 
                            snowpack[i-1], melt_delta[i]))
    snowpack[i] = snowpack[i-1]+snow[i]-melt[i]
  }
  return(melt)
}

frog$melt <- get_melt(tmean = frog$tmean_C, j_temp = 2.155578, hock = 4, snow = frog$snow, sp.0= NULL)

system.time(
WBData$melt2 <- get_melt(tmean = WBData$tmean_C, j_temp = 1.381265, hock = 4, snow = WBData$SNOW, sp.0 = NULL)
)
