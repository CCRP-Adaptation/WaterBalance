######################################
##    TESTS   ########################
######################################

# ------  FUNCTIONS ------------------------------------------------------- #

extraterrestrial_solar_rad = function(doy, lat){
  d.r = 1 + 0.033*cos(((2*pi)/365)*doy) # cell AG9
  declin = 0.409*sin(((2*pi)/365)*doy) # cell AH9
  lat.rad = (pi/180)*lat
  sunset.ang = acos(-tan(lat.rad)*tan(declin)) #cell AJ9
  R.a = ((24*60)/pi)*0.0820*d.r*(sunset.ang*sin(lat.rad)*sin(declin) + cos(lat.rad)*cos(declin)*sin(sunset.ang))
  return(R.a)
}

ET_Oudin = function(x, R.a., snowpack){ 
  top = R.a. * (x$tmean_C + 5) * 0.408
  bottom = 100
  PET = top/bottom
  et.oudin = ifelse(snowpack > 2,0, ifelse(tmean_C > -5, PET, 0))
  return(et.oudin)
}

ET_Oudin_daymet = function(x){
  top = x$srad * x$daylength/1000000 * (tmean_C + 5) * 0.408
  bottom = 100
  PET = top/bottom
  et.oudin = ifelse(snowpack > 2, 0, ifelse(tmean_C > -5, PET, 0))
  return(et.oudin)
}