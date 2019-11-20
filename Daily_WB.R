############################################# DAILY_WB ####################################################
#### Script to implement functions in 'WaterBalance' package, calculating water balance variables at a single site at daily time steps.
#### Created 11/5/2019 by ARC
#### v01 - Calculates water balance output table and aggregates results to monthly and annual averages/totals
###########################################################################################################

rm(list=ls())

library(devtools)

#Install WaterBalance package from Github repository
install_github("CCRP-Adaptation/WaterBalance", subdir="WaterBalance")

#Or, install from local folder
setwd("~/WaterBalance/WB R package") #Directory containing package folder
install("WaterBalance")

library("WaterBalance")

############################################################# USER INPUTS ##################################################################### 

#Formatted input data as a daily time series. See guidance for required columns, based on PET calculation method.
DailyClimData = read.csv("~/WaterBalance/WB R package/test_maca.csv")
DailyClimData$Date = as.Date(DailyClimData$Date, "%m/%d/%Y")

#Location
SiteID = "Test" #ID of location (used to label output files and plots)
Lat = 37.1747
Lon = -108.491

#Topographic characteristics 
Elev = 2007  #Elevation (meters)
Aspect = 0  #Aspect (degrees)
Slope = 0 #Slope (degrees)
  
#Maximum soil water-holding capacity (mm)
SWC.Max = 100

#Other inputs 
Wind = 0.5  #Default value if wind data is not included in data table. This value is ignored if input data contains a 'wind' column
Snowpack.Init = 0 #Initial snowpack depth for snow accumulation calculations. Default is 0. 
Soil.Init = 0 #Initial soil moisture value for soil moisture change calculations. Default is 0. 
T.Base = 0 #Threshold temperature (deg C) for growing degree-days calculation

#Method for PET calculation 
Method = "Hamon"  #Hamon is default method for daily PRISM and MACA data (containing only Tmax, Tmin, and Date). Penman-Monteith requires additional solar radiation and vapor pressure/relative humidity data. 

#Shade coefficient for heat load PET modification (method of Lutz et al., 2010)
Shade.Coeff = 1.0  #Default is 1.0 

#Output directory
OutDir = "~/WaterBalance/WB R package"

############################################################ END USER INPUTS ###################################################################


######################################################### CALCULATE WB VARIABLES ################################################################

#Calculate daily water balance variables 
DailyWB = DailyClimData
DailyWB$daylength = get_daylength(DailyWB$Date, Lat)
DailyWB$F = get_freeze(DailyWB$tmean_C)
DailyWB$RAIN = get_rain(DailyWB$ppt_mm, DailyWB$F)
DailyWB$SNOW = get_snow(DailyWB$ppt_mm, DailyWB$F)
DailyWB$PACK = get_snowpack(DailyWB$ppt_mm, DailyWB$F, Snowpack.Init)
DailyWB$MELT = get_melt(DailyWB$PACK, DailyWB$SNOW, DailyWB$F, Snowpack.Init)
DailyWB$W = DailyWB$MELT + DailyWB$RAIN
if(Method == "Hamon"){
  DailyWB$PET = ET_Hamon_daily(DailyWB)
  print("Calculating Hamon PET")
} else {
  if(Method == "Penman-Monteith"){
    print("Calculating Penman-Monteith PET")
    DailyWB$PET = ET_PenmanMonteith_daily(DailyWB, Elev, Lat, Wind)
  } else {
    print("Error - PET method not found")
  }
}
DailyWB$PET = modify_PET(DailyWB$PET, Slope, Aspect, Lat, Shade.Coeff)
DailyWB$W_PET = DailyWB$W - DailyWB$PET
DailyWB$SOIL = get_soil(DailyWB$W, DailyWB$PET, SWC.Max, Soil.Init)
DailyWB$DSOIL = diff(c(Soil.Init, DailyWB$SOIL))
DailyWB$AET = get_AET(DailyWB$W, DailyWB$PET, DailyWB$SOIL, Soil.Init)
DailyWB$W_ET_DSOIL = DailyWB$W - DailyWB$AET - DailyWB$DSOIL
DailyWB$D = DailyWB$PET - DailyWB$AET
DailyWB$GDD = get_GDD(DailyWB$tmean_C, T.Base)

######################################################### END WB VARIABLE CALCULATIONS ################################################################

######################################################### AGGREGATE OUTPUTS TO MONTLY/ANNUAL ################################################################

DailyWB$yrmon = strftime(DailyClimData$Date, "%Y%m")
DailyWB$year = strftime(DailyClimData$Date, "%Y")

#Monthly
MonthlyWB = data.frame(yrmon = unique(DailyWB$yrmon))
MonthlyWB$sum_p = aggregate(ppt_mm ~ yrmon, data=DailyWB, FUN=sum)[,2]
MonthlyWB$avg_t = aggregate(tmean_C ~ yrmon, data=DailyWB, FUN=mean)[,2]
MonthlyWB$sum_rain = aggregate(RAIN ~ yrmon, data=DailyWB, FUN=sum)[,2]
MonthlyWB$sum_snow = aggregate(SNOW ~ yrmon, data=DailyWB, FUN=sum)[,2]
MonthlyWB$max_pack = aggregate(PACK ~ yrmon, data=DailyWB, FUN=max)[,2]
MonthlyWB$sum_melt = aggregate(MELT ~ yrmon, data=DailyWB, FUN=sum)[,2]
MonthlyWB$sum_w = aggregate(W ~ yrmon, data=DailyWB, FUN=sum)[,2]
MonthlyWB$sum_pet = aggregate(PET ~ yrmon, data=DailyWB, FUN=sum)[,2]
MonthlyWB$sum_w_pet = aggregate(W_PET ~ yrmon, data=DailyWB, FUN=sum)[,2]
MonthlyWB$avg_soil = aggregate(SOIL ~ yrmon, data=DailyWB, FUN=mean)[,2]
MonthlyWB$sum_aet = aggregate(AET ~ yrmon, data=DailyWB, FUN=sum)[,2]
MonthlyWB$sum_w_et_dsoil = aggregate(W_ET_DSOIL ~ yrmon, data=DailyWB, FUN=sum)[,2]
MonthlyWB$sum_d = aggregate(D ~ yrmon, data=DailyWB, FUN=sum)[,2]
MonthlyWB$sum_gdd = aggregate(GDD ~ yrmon, data=DailyWB, FUN=sum)[,2]

#Annual
AnnualWB = data.frame(year = unique(DailyWB$year))
AnnualWB$sum_p = aggregate(ppt_mm ~ year, data=DailyWB, FUN=sum)[,2]
AnnualWB$avg_t = aggregate(tmean_C ~ year, data=DailyWB, FUN=mean)[,2]
AnnualWB$sum_rain = aggregate(RAIN ~ year, data=DailyWB, FUN=sum)[,2]
AnnualWB$sum_snow = aggregate(SNOW ~ year, data=DailyWB, FUN=sum)[,2]
AnnualWB$max_pack = aggregate(PACK ~ year, data=DailyWB, FUN=max)[,2]
AnnualWB$sum_melt = aggregate(MELT ~ year, data=DailyWB, FUN=sum)[,2]
AnnualWB$sum_w = aggregate(W ~ year, data=DailyWB, FUN=sum)[,2]
AnnualWB$sum_pet = aggregate(PET ~ year, data=DailyWB, FUN=sum)[,2]
AnnualWB$sum_w_pet = aggregate(W_PET ~ year, data=DailyWB, FUN=sum)[,2]
AnnualWB$avg_soil = aggregate(SOIL ~ year, data=DailyWB, FUN=mean)[,2]
AnnualWB$sum_aet = aggregate(AET ~ year, data=DailyWB, FUN=sum)[,2]
AnnualWB$sum_w_et_dsoil = aggregate(W_ET_DSOIL ~ year, data=DailyWB, FUN=sum)[,2]
AnnualWB$sum_d = aggregate(D ~ year, data=DailyWB, FUN=sum)[,2]
AnnualWB$sum_gdd = aggregate(GDD ~ year, data=DailyWB, FUN=sum)[,2]

#Save outputs
write.csv(MonthlyWB, paste(SiteID, Lat, Lon, "MonthlyWB.csv", sep="_"))
write.csv(AnnualWB, paste(SiteID, Lat, Lon, "AnnualWB.csv", sep="_"))
