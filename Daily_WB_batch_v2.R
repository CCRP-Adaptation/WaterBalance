####### Batch water balance code #######
# Update from Amanda batch code - to bring work from climate data 
rm(list=ls())
library("WaterBalance")
library(ggplot2)
library(plyr)
library(lubridate)
library(dplyr)

############################################################# USER INPUTS ##################################################################### 

#Formatted input data as a daily time series. Needs to include the following columns: Date, ppt_mm, tmax_C, tmin_C, and tmean_C (temp.'s in deg. Celsius)
setwd("C:/Users/achildress/Documents/RSS/Completed/PETE/MACA/Figs MACA/")
load("PETE_37.19106_-77.476_Final_Environment.RData") #Load final environment
PARK<-SiteID
rm(list=setdiff(ls(), c("ALL_HIST","ALL_FUTURE","PARK")))

#Site characteristics 
Sites = read.csv("C:/Users/achildress/Documents/RSS/Completed/PETE/MACA/Figs MACA/WB/PETE_site_parameters_R.csv") #CSV file containing properties for all sites
n<-nrow(Sites)
#Threshold temperature (deg C) for growing degree-days calculation
T.Base = 0 

#Method for PET calculation 
Method = "Hamon"  #Hamon is default method for daily PRISM and MACA data (containing only Tmax, Tmin, and Date). 

#Date format
DateFormat = "%m/%d/%Y"

#Output directory
OutDir = "~/Documents/RSS/Completed/PETE/MACA/Figs MACA/WB/"

#Select GCMs - Include RCP
unique(ALL_FUTURE$GCM)
GCMs = c("inmcm4.rcp45","MIROC5.rcp85")  

colors2<-c("light green","orange")
colors3 <- c("grey","light green","orange")
############################################################ END USER INPUTS ###################################################################

############################################################ CREATE CLIMATE INPUTS #############################################################
#### Historical
###** Use model average or specific GCMs for historical???
AH<-ALL_HIST
AH$GCM<-paste(AH$GCM,".rcp85",sep="")
ALL_HIST$GCM<-paste(ALL_HIST$GCM,".rcp45",sep="")
ALL_HIST<-rbind(ALL_HIST,AH);rm(AH)

# Convert pr.In to mm and F to C
ALL_HIST$ppt_mm <- (ALL_HIST$PrecipCustom*25.4)
ALL_HIST$tmax_C <- 5/9*(ALL_HIST$TmaxCustom - 32)
ALL_HIST$tmin_C <- 5/9*(ALL_HIST$TminCustom - 32)
ALL_HIST$tmean_C <- (ALL_HIST$tmax_C + ALL_HIST$tmin_C)/2

#### Projected
# Convert pr.In to mm
ALL_FUTURE$ppt_mm <- (ALL_FUTURE$PrecipCustom*25.4)
ALL_FUTURE$tmax_C <- 5/9*(ALL_FUTURE$TmaxCustom - 32)
ALL_FUTURE$tmin_C <- 5/9*(ALL_FUTURE$TminCustom - 32)
ALL_FUTURE$tmean_C <- (ALL_FUTURE$tmax_C + ALL_FUTURE$tmin_C)/2
#Add YrMon column


if(dir.exists(OutDir) == FALSE){
  dir.create(OutDir)
}
setwd(OutDir)
ClimData<-data.frame(Date=as.numeric(),ppt_mm=as.numeric(),tmean_C=as.numeric(),GCM=as.character())
# Loop through selected GCMs
for(i in 1:length(GCMs)){
  gcm <- GCMs[i]
    x<-subset(ALL_HIST,GCM == gcm, select=c("Date","ppt_mm","tmean_C","GCM"))
    y<-subset(ALL_FUTURE,GCM == gcm, select=c("Date","ppt_mm","tmean_C","GCM"))
    ClimData = rbind(ClimData,x,y)
}
ClimData$GCM<-factor(ClimData$GCM,levels=GCMs)
######################################################### END CLIMATE INPUTS ####################################################################


######################################################### CALCULATE WB VARIABLES ################################################################
AllDailyWB<-list()

for (j in 1:length(GCMs)){
  gcm = GCMs[j]
  DailyWB = subset(ClimData,GCM=gcm)
for(i in 1:nrow(Sites)){
  SiteID = Sites$SiteID[i]
  Lat = Sites$Lat[i]
  Lon = Sites$Lon[i]
  Elev = Sites$Elevation[i]
  Aspect = Sites$Aspect[i]
  Slope = Sites$Slope[i]
  SWC.Max = Sites$SWC.Max[i]
  Wind = Sites$Wind[i]
  Snowpack.Init = Sites$Snowpack.Init[i]
  Soil.Init = Sites$Soil.Init[i]
  Shade.Coeff = Sites$Shade.Coeff[i]
  
   #Calculate daily water balance variables 
  DailyWB$SiteID = SiteID
  DailyWB$daylength = get_daylength(DailyWB$Date, Lat)
  DailyWB$F = get_freeze(DailyWB$tmean_C)
  DailyWB$RAIN = get_rain(DailyWB$ppt_mm, DailyWB$F)
  DailyWB$SNOW = get_snow(DailyWB$ppt_mm, DailyWB$F)
  DailyWB$PACK = get_snowpack(DailyWB$ppt_mm, DailyWB$F, Snowpack.Init)
  DailyWB$MELT = get_melt(DailyWB$PACK, DailyWB$SNOW, DailyWB$F, Snowpack.Init)
  DailyWB$W = DailyWB$MELT + DailyWB$RAIN
  if(Method == "Hamon"){
    DailyWB$PET = ET_Hamon_daily(DailyWB)
  } else {
    if(Method == "Penman-Monteith"){
      DailyWB$PET = ET_PenmanMonteith_daily(DailyWB)
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
  AllDailyWB[[i]] = DailyWB
  }
}
WBData<-do.call(rbind,AllDailyWB)
######################################################### END WB VARIABLE CALCULATIONS ################################################################

######################################################### AGGREGATE OUTPUTS TO MONTLY/ANNUAL ################################################################

WBData$yrmon = strftime(WBData$Date, "%Y%m")
WBData$year = strftime(WBData$Date, "%Y")
  
  #Monthly
  MonthlyWB = aggregate(ppt_mm~yrmon+GCM,data=aggregate(ppt_mm~yrmon+GCM+SiteID,data=WBData,sum),mean)
  colnames(MonthlyWB)[3]<-"sum_p"
  
  MonthlyWB$avg_t = aggregate(tmean_C ~ yrmon+GCM, data=WBData, FUN=mean)[,3]
  MonthlyWB$sum_rain = aggregate(RAIN~yrmon+GCM,data=aggregate(RAIN~yrmon+GCM+SiteID,data=WBData,sum),mean)[,3]
  MonthlyWB$sum_snow = aggregate(SNOW~yrmon+GCM,data=aggregate(SNOW~yrmon+GCM+SiteID,data=WBData,sum),mean)[,3]
  MonthlyWB$max_pack = aggregate(PACK ~ yrmon+GCM, data=WBData, FUN=max)[,3]
  MonthlyWB$sum_melt = aggregate(MELT~yrmon+GCM,data=aggregate(MELT~yrmon+GCM+SiteID,data=WBData,sum),mean)[,3]
  MonthlyWB$sum_w = aggregate(W~yrmon+GCM,data=aggregate(W~yrmon+GCM+SiteID,data=WBData,sum),mean)[,3]
  MonthlyWB$sum_pet = aggregate(PET~yrmon+GCM,data=aggregate(PET~yrmon+GCM+SiteID,data=WBData,sum),mean)[,3]
  MonthlyWB$sum_w_pet = aggregate(W_PET~yrmon+GCM,data=aggregate(W_PET~yrmon+GCM+SiteID,data=WBData,sum),mean)[,3]
  MonthlyWB$avg_soil = aggregate(SOIL ~ yrmon+GCM, data=WBData, FUN=mean)[,3]
  MonthlyWB$sum_aet = aggregate(AET~yrmon+GCM,data=aggregate(AET~yrmon+GCM+SiteID,data=WBData,sum),mean)[,3]
  MonthlyWB$sum_w_et_dsoil = aggregate(W_ET_DSOIL~yrmon+GCM,data=aggregate(W_ET_DSOIL~yrmon+GCM+SiteID,data=WBData,sum),mean)[,3]
  MonthlyWB$sum_d = aggregate(D~yrmon+GCM,data=aggregate(D~yrmon+GCM+SiteID,data=WBData,sum),mean)[,3]
  MonthlyWB$sum_gdd = aggregate(GDD~yrmon+GCM,data=aggregate(GDD~yrmon+GCM+SiteID,data=WBData,sum),mean)[,3]
  
  #Annual
  AnnualWB = aggregate(ppt_mm ~ year+GCM, data=aggregate(ppt_mm~year+GCM+SiteID,data=WBData,sum), mean)
  colnames(AnnualWB)[3]<-"sum_p"
  AnnualWB$avg_t = aggregate(tmean_C ~ year+GCM, data=WBData, FUN=mean)[,3]
  AnnualWB$sum_rain = aggregate(RAIN ~ year+GCM, data=aggregate(RAIN~year+GCM+SiteID,data=WBData,sum), mean)[,3]
  AnnualWB$sum_snow = aggregate(SNOW ~ year+GCM, data=aggregate(SNOW~year+GCM+SiteID,data=WBData,sum), mean)[,3]
  AnnualWB$max_pack = aggregate(PACK ~ year+GCM, data=aggregate(PACK~year+GCM+SiteID,data=WBData,sum), mean)[,3]
  AnnualWB$sum_melt = aggregate(MELT ~ year+GCM, data=aggregate(MELT~year+GCM+SiteID,data=WBData,sum), mean)[,3]
  AnnualWB$sum_w = aggregate(W ~ year+GCM, data=aggregate(W~year+GCM+SiteID,data=WBData,sum), mean)[,3]
  AnnualWB$sum_pet = aggregate(PET ~ year+GCM, data=aggregate(PET~year+GCM+SiteID,data=WBData,sum), mean)[,3]
  AnnualWB$sum_w_pet = aggregate(W_PET ~ year+GCM, data=aggregate(W_PET~year+GCM+SiteID,data=WBData,sum), mean)[,3]
  AnnualWB$avg_soil = aggregate(SOIL ~ year+GCM, data=WBData, FUN=mean)[,3]
  AnnualWB$sum_aet = aggregate(AET ~ year+GCM, data=aggregate(AET~year+GCM+SiteID,data=WBData,sum), mean)[,3]
  AnnualWB$sum_w_et_dsoil = aggregate(W_ET_DSOIL ~ year+GCM, data=aggregate(W_ET_DSOIL~year+GCM+SiteID,data=WBData,sum), mean)[,3]
  AnnualWB$sum_d = aggregate(D ~ year+GCM, data=aggregate(D~year+GCM+SiteID,data=WBData,sum), mean)[,3]
  AnnualWB$sum_gdd = aggregate(GDD ~ year+GCM, data=aggregate(GDD~year+GCM+SiteID,data=WBData,sum), mean)[,3]

write.csv(MonthlyWB,"MonthlyWB.csv",row.names=F)
write.csv(AnnualWB,"AnnualWB.csv",row.names=F)


#######################################################################################################################
######################################### PLOTTING ####################################################################
# Inputs
F.start = 2025
F.end = 2055
CFs<-c("Warm Damp","Hot Wet")
AnnualWB$CF<-CFs[1];AnnualWB$CF[which(AnnualWB$GCM==GCMs[2])]<-CFs[2]
MonthlyWB$CF<-CFs[1];MonthlyWB$CF[which(MonthlyWB$GCM==GCMs[2])]<-CFs[2]

# Convert to Imperial units
AnnualWB$deficit<-AnnualWB$sum_d * 0.039
AnnualWB$AET<-AnnualWB$sum_aet * 0.039
AnnualWB$P<-AnnualWB$sum_p * 0.039
AnnualWB$Tmean<-(AnnualWB$avg_t * 9/5) + 32
AnnualWB$SOIL_in<-AnnualWB$avg_soil

# Subset to correct years for each period
H_annual<-subset(AnnualWB,year<2000,select=c("year","CF","deficit","AET","P","Tmean","SOIL_in","sum_d","sum_aet"))
F_annual<-subset(AnnualWB,year>=F.start & year<=F.end,select=c("year","CF","deficit","AET","P","Tmean","SOIL_in","sum_d","sum_aet"))
H_annual$CF<-"Historical"

# Average over sites and subset historical to be 30 years
Hist<-aggregate(cbind(deficit,AET,P,Tmean,SOIL_in,sum_d,sum_aet)~year+CF,mean,data=H_annual,na.rm=TRUE)
Fut<-aggregate(cbind(deficit,AET,P,Tmean,SOIL_in,sum_d,sum_aet)~year+CF,mean,data=F_annual,na.rm=TRUE)
set.seed(50) # set seed so same every time
Hist.subset<-sample_n(Hist,size=length(Fut$CF)/length(unique(Fut$CF)),replace=F)

Annual<-rbind(Hist.subset,Fut)
Annual$CF = factor(Annual$CF, levels = unique(Annual$CF))

ggplot(Annual, aes(x=deficit, y=AET, colour=CF)) + geom_point(size=3)+ geom_smooth(method="lm", se=FALSE, size=2)+
  
  scale_colour_manual("Scenario",values=colors3) +
  labs(
    y = "Annual Actual Evapotranspiration (in)",
    x = "Annual moisture deficit (in)",
    colour = "GCM",
    title = paste("Water Balance for ",PARK,sep="")  
  ) + theme(plot.title = element_text(hjust = 0.5)) + #+ geom_vline(xintercept=mean(Historical.wb$deficit), colour="black") +geom_vline(xintercept=mean(Future.wb$deficit), colour="blue")
  # size is pts
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22)) #+xlim(20,45)+ylim(2,16)

ggsave(paste("Water Balance-",PARK,".png",sep=""), width = 15, height = 9)

ggplot(Annual, aes(x=deficit, colour=CF,fill=CF,linetype=CF),show.legend=F) +geom_density(alpha=0.3,size=1.5) +
  scale_colour_manual(values=colors3) +
  scale_fill_manual(values=colors3) +  
  scale_linetype_manual(values=seq(1,length(unique(Annual$CF)),1)) +
  labs(y = "Density",
       x = "Annual moisture deficit (in)",
       title = paste(PARK,"  Water Deficit for GCMs (2025-2055) and Historical Period (1895-1999)",sep=" ")) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22, hjust=0),legend.position = c(.8,.8)) 

ggsave(paste(PARK,"-Deficit_density_panel.png",sep=""), width = 15, height = 9)

ggplot(Annual, aes(x=SOIL_in, colour=CF,fill=CF,linetype=CF),show.legend=F) +geom_density(alpha=0.3,size=1.5) +
  scale_colour_manual(values=colors3) +
  scale_fill_manual(values=colors3) +  
  scale_linetype_manual(values=seq(1,length(unique(Annual$CF)),1)) +
  labs(y = "Density",
       x = "Annual soil moisture (in)",
       title = paste(PARK,"  Soil Moisture for GCMs (2025-2055) and Historical Period (1895-1999)",sep=" ")) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22, hjust=0),legend.position = c(.8,.8)) 

ggsave(paste(PARK,"-SOIL_in_density_panel.png",sep=""), width = 15, height = 9)


########################
# biome plots
biome<-read.csv("C:/Users/achildress/Documents/RSS/Background/Biome_plots/D_AET_points.csv",header=T)
head(biome)
color<-as.character(unique(biome$color,ordered=T))

color<-setNames(as.character(unique(biome$color)),as.character(unique(biome$biome)))
color

PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=20),                                                                   #Text size of legend title
                  legend.position = "right")         

plot_1 <- ggplot() +
  # add biome polygons
  geom_polygon(data = biome,
               aes(x    = D_mm,
                   y    = AET_mm,
                   fill = biome),
               # adjust polygon borders
               colour = "black",
               size   = 1) + PlotTheme +
  scale_fill_manual(name   = "Veg biomes",
                    breaks = names(color),
                    labels = names(color),
                    values = color) + 
  labs(title=paste(PARK, " water balance effects on biome",sep=""),
       y = "Annual Evapotranspiration (mm)", x = "Annual moisture deficit (mm)") 
plot_1

plot_1 + geom_point(data=Annual, aes(x=sum_d, y=sum_aet, colour=CF), size=3) + 
  geom_smooth(data=Annual, aes(x=sum_d, y=sum_aet, colour=CF),method="lm", se=FALSE, size=2) + 
  scale_colour_manual("Scenario",values=colors3)
ggsave(paste(PARK,"-WB-biome effects.png",sep=""), width = 15, height = 9)

### Monthly
MonthlyWB$year<-as.numeric(substr(MonthlyWB$yrmon, 1, 4))
MonthlyWB$Month<-as.numeric(substr(MonthlyWB$yrmon, 5, 6))
MonthlyWB$SOIL_in<-MonthlyWB$avg_soil * 0.39
MonthlyWB$deficit<-MonthlyWB$sum_d * 0.39

H_monthly<-subset(MonthlyWB,year<2000)
F_monthly<-subset(MonthlyWB,year>=F.start & year<=F.end)
H_monthly$CF<-"Historical"

set.seed(50) # set seed so same every time
MHist.subset<-sample_n(H_monthly,size=length(F_monthly$CF)/length(unique(F_monthly$CF)),replace=F)

Mon<-aggregate(cbind(SOIL_in,deficit)~Month+CF,mean,data=MHist.subset,na.rm=TRUE)
Fut<-aggregate(cbind(SOIL_in,deficit)~Month+CF,mean,data=F_monthly,na.rm=TRUE)
Monthly<-rbind(Mon,Fut)
Monthly$CF = factor(Monthly$CF, levels = c("Warm Damp","Hot Wet"))

data<-Fut[,1:2]
data$SOIL_IN<-Fut$SOIL_in-Mon$SOIL_in
data$deficit<-Fut$deficit-Mon$deficit
data$mon<-month.abb[data$Month]

ggplot(data, aes(x=mon, y=SOIL_IN, group=CF, colour = CF)) +
  geom_line(colour = "black",size=2.5, stat = "identity") + # adds black outline
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour = "black", size = 4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20)) +
  scale_x_discrete(labels=month.abb) +
  labs(title = "Change in average monthly soil moisture \n 2040 (2025-2055) vs 1950-1999",
       x = "Month", y = "Change in soil moisture (inches)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22))

ggsave("MonthlySoil Moisture.png", width = 15, height = 9)

ggplot(data, aes(x=mon, y=deficit, group=CF, colour = CF)) +
  geom_line(colour = "black",size=2.5, stat = "identity") + # adds black outline
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour = "black", size = 4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20)) +
  scale_x_discrete(labels=month.abb) +
  labs(title = "Change in average monthly water deficit \n 2040 (2025-2055) vs 1950-1999",
       x = "Month", y = "Change in deficit (inches)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22))

ggsave("Monthly Deficit.png", width = 15, height = 9)




