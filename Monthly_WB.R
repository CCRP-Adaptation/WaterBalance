
rm(list=ls())

library(devtools)

#Install WaterBalance package from Github repository
install_github("CCRP-Adaptation/WaterBalance", subdir="WaterBalance")

#Or, install from local folder
setwd("~/WaterBalance/WB R package") #Directory containing package folder
install("WaterBalance")

library("WaterBalance")
############################################################# USER INPUTS ##################################################################### 

#Formatted input data as a daily time series. Needs to include the following columns: Date, ppt_mm, tmax_C, tmin_C, and tmean_C (temp.'s in deg. Celsius)
MonthlyClimData = read.csv("~/WaterBalance/WB R package/test_prism_monthly.csv", skip=10, header=TRUE)
MonthlyClimData$Date = as.Date(paste0(MonthlyClimData$Date, "-15"), "%Y-%m-%d")
names(MonthlyClimData) = c("Date", "ppt_mm", "tmin_C", "tmax_C") #Make sure columns are named correctly
MonthlyClimData$tmean_C = (MonthlyClimData$tmin_C + MonthlyClimData$tmax_C)/2

#Location
SiteID = "Test" #ID of location (used to label output files and plots)
Lat = 37.4516
Lon = -106.8676

#Topographic characteristics 
Elev = 2890  #Elevation (meters)
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
Method = "Thornthwaite"  #Thornthwaite is default method for monthly PRISM and MACA data (containing only Tmax, Tmin, and Date). 

#Shade coefficient for heat load PET modification (method of Lutz et al., 2010)
Shade.Coeff = 1.0  #Default is 1.0 

#Output directory
OutDir = "~/WaterBalance/WB R package"

############################################################ END USER INPUTS ###################################################################

######################################################### CALCULATE WB VARIABLES ################################################################

#Calculate monthly water balance variables 
MonthlyWB = MonthlyClimData
MonthlyWB$daylength = get_daylength(MonthlyWB$Date, Lat)
MonthlyWB$F = get_freeze(MonthlyWB$tmean_C)
MonthlyWB$RAIN = get_rain(MonthlyWB$ppt_mm, MonthlyWB$F)
MonthlyWB$SNOW = get_snow(MonthlyWB$ppt_mm, MonthlyWB$F)
MonthlyWB$PACK = get_snowpack(MonthlyWB$ppt_mm, MonthlyWB$F, Snowpack.Init)
MonthlyWB$MELT = get_melt(MonthlyWB$PACK, MonthlyWB$SNOW, MonthlyWB$F, Snowpack.Init)
MonthlyWB$W = MonthlyWB$MELT + MonthlyWB$RAIN
if(Method == "Thornthwaite"){
  MonthlyWB$PET = ET_Thorn_monthly(MonthlyWB)
}
MonthlyWB$PET = modify_PET(MonthlyWB$PET, Slope, Aspect, Lat, Shade.Coeff)
MonthlyWB$W_PET = MonthlyWB$W - MonthlyWB$PET
MonthlyWB$SOIL = get_soil(MonthlyWB$W, MonthlyWB$PET, SWC.Max, Soil.Init)
MonthlyWB$DSOIL = diff(c(Soil.Init, MonthlyWB$SOIL))
MonthlyWB$AET = get_AET(MonthlyWB$W, MonthlyWB$PET, MonthlyWB$SOIL, Soil.Init)
MonthlyWB$W_ET_DSOIL = MonthlyWB$W - MonthlyWB$AET - MonthlyWB$DSOIL
MonthlyWB$D = MonthlyWB$PET - MonthlyWB$AET

######################################################### END WB VARIABLE CALCULATIONS ################################################################

######################################################### AGGREGATE OUTPUTS TO ANNUAL ################################################################

MonthlyWB$year = as.numeric(strftime(MonthlyWB$Date, "%Y"))

#Annual
AnnualWB = data.frame(year = unique(MonthlyWB$year))
AnnualWB$sum_p = aggregate(ppt_mm ~ year, data=MonthlyWB, FUN=sum)[,2]
AnnualWB$avg_t = aggregate(tmean_C ~ year, data=MonthlyWB, FUN=mean)[,2]
AnnualWB$sum_rain = aggregate(RAIN ~ year, data=MonthlyWB, FUN=sum)[,2]
AnnualWB$sum_snow = aggregate(SNOW ~ year, data=MonthlyWB, FUN=sum)[,2]
AnnualWB$max_pack = aggregate(PACK ~ year, data=MonthlyWB, FUN=max)[,2]
AnnualWB$sum_melt = aggregate(MELT ~ year, data=MonthlyWB, FUN=sum)[,2]
AnnualWB$sum_w = aggregate(W ~ year, data=MonthlyWB, FUN=sum)[,2]
AnnualWB$sum_pet = aggregate(PET ~ year, data=MonthlyWB, FUN=sum)[,2]
AnnualWB$sum_w_pet = aggregate(W_PET ~ year, data=MonthlyWB, FUN=sum)[,2]
AnnualWB$avg_soil = aggregate(SOIL ~ year, data=MonthlyWB, FUN=mean)[,2]
AnnualWB$sum_aet = aggregate(AET ~ year, data=MonthlyWB, FUN=sum)[,2]
AnnualWB$sum_w_et_dsoil = aggregate(W_ET_DSOIL ~ year, data=MonthlyWB, FUN=sum)[,2]
AnnualWB$sum_d = aggregate(D ~ year, data=MonthlyWB, FUN=sum)[,2]

#Save outputs
write.csv(MonthlyWB, paste(SiteID, Lat, Lon, "MonthlyWB.csv", sep="_"))
write.csv(AnnualWB, paste(SiteID, Lat, Lon, "AnnualWB.csv", sep="_"))