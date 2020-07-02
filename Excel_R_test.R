###########################################################################
##    CODE TEST   #########################################################
###########################################################################

rm(list = ls())

setwd("C:/Users/adillon/Documents/Water_Balance_Update")

# Load data

pack <- read.csv("./PACK_time_series.csv") # created time series df from Thoma's calculations
pack <- pack[-c(14601:14615),]

daymet <- read.csv("./FortCollinsCO_daymet_R.csv")
daymet$tmean_C <- (daymet$tmax..deg.c. + daymet$tmin..deg.c.)/2

# Run Functions from test_Oudin.R

R.a. <- extraterrestrial_solar_rad(daymet$yday, lat = 40.5853)

t <- ET_Oudin(daymet, R.a., pack)

results <- t$yday

write.csv(results, file = "ET_Oudin_results.csv")
head(results)
results

pack
