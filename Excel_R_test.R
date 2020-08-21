###########################################################################
##    CODE TEST   #########################################################
###########################################################################

rm(list = ls())


setwd("C:/Users/adillon/Documents/Water_Balance_Update")

# Load data

# ----  TEST: Oudin PET calculation: FAO Ra  ------------------------------------------------------------ #

snowpack <- read.csv("./PACK_time_series.csv") # created time series df from Thoma's calculations
snowpack <- as.numeric(snowpack$ï..PACK)

daymet <- read.csv("./FortCollinsCO_daymet_R.csv")
daymet$tmean_C <- (daymet$tmax + daymet$tmin)/2

# Run Functions from test_Oudin.R

R.a. <- extraterrestrial_solar_rad(daymet$yday, lat = 40.5853)

t <- ET_Oudin(daymet, R.a., snowpack)

write.csv(t, file = "ET_Oudin_results.csv")

# --- TEST: Oudin PET calculation: Daymet Ra  ----------------------------------------------------------- #

tt <- ET_Oudin_daymet_Ra(daymet, snowpack)

write.csv(tt, file = "ET_Oudin_daymet_Ra.csv")

# --- TEST: Jennings coefficient ------------------------------------------------------------------------ #



