### Daymet data download and cleaning


#### DOWNLOAD####
## Download daymet data for each plot
#https://khufkens.github.io/daymetr/

install.packages("daymetr")
library(daymetr)

setwd("C:\\David\\Water balance\\CHIS\\Round 2")
### Read in site file -- has column for site name, latitude, and longitude (in that order)
sites <- read.csv("sites.csv",head=T);head(sites)

#Download all sites at once
download_daymet_batch(file_location = 'sites.csv',
                      start = 1980, 
                      end = 2019,
                      internal = FALSE,
                      path="C:\\David\\Water balance\\CHIS\\Round 2\\daymet")


#daymetr adds unweildly extension names including the date range for every downloaded file.
#if needed use the following dos commands to rename files in a dos window
#open a dos window by clicking "Start" in Windows then start typing "command window"
#click the first program to pop up.
#navigate to your directory holding files you want to name-change  cd .. to back up and cd David, then cd water balance etc to go forward in a path name
#note: in the second step the *csv part chops off everything after "csv" and then replaces all that with .txt.
#https://www.robvanderwoude.com/battech_rename.php
# in the dos window type these two rename commands. 
#C:\David\Water balance\NCPN\Pivots\water balance\test>ren *.csv *.txt
#C:\David\Water balance\NCPN\Pivots\water balance\test>ren *.txt *csv  :NOTE!  there is no "." before csv in this ren line.


#below here is work in progress including
#attempt to reame files on drive in R
#compiling montly and annual daymet values from daily daymet files into a single data frame for analysis as long format in R

dfiles<-(list.files("C:\\David\\Water balance\\CHIS\\Round 2\\daymet", pattern=".csv"))

strsplit(dfiles, split=.csv_1980_2019.csv)



head(dfiles)
str(dfiles)
dfiles<-as.character(dfiles)
head(dfiles)
nrow(dfiles)
i=1

for (i in 1:nrow(dfiles)){
  y <- (unlist(strsplit(dfiles[i], "\\.csv_1980_2019.")))
}




split_files<-strsplit(dfiles,split=".csv_1980_2019")
  
  apply(unlist(strsplit(dfiles,split=".csv_1980_2019"));split_files
m<-matrix(split_files, ncol = 1))




img_yr_doy<-substr(dfiles, start=14, stop=21);img_yr_doy
yr<-as.numeric(substr(img_yr_doy, start=1, stop=4));yr
#yr_origin<-as.Date(paste0(yr, "-01-01"),tz = "MST") - days(1);yr_origin
doy<-as.numeric(substr(img_yr_doy, start=6, stop=9));doy


#### Combine Data ####
####Read in all the data and combine it into a single dataframe

library(plyr)
library(tidyverse)

#### MONTHLY ####
###Save file location
mydir <- "C:\\David\\Water Balance CONUS\\Cross check\\daymet"

### Get a list of all the files to read
myfiles <- list.files(path=mydir,pattern="*.csv",full.names = TRUE)

## Read in monthly data files
data.monthly <- ldply(myfiles,read_csv) ;head(data.monthly)

###Split the year/month into two columns
y <- strsplit(data.monthly$`Row Labels`,"_")
data.monthly$Year <- unlist(lapply(y, function(x) x[1]))
data.monthly$Month <- unlist(lapply(y, function(x) x[2]))
data.monthly$Year <-as.numeric(as.character(data.monthly$Year))
data.monthly$Month <-as.numeric(as.character(data.monthly$Month))



### Export data
write.csv(data.monthly,"MonthlyWaterBalance.csv")



#################################################
#### DAILY ####
###Save file location
mydir <- "C:/Users/A02261597/Documents/Chapter 2/Water Balance_2019_Aspen/daily"
myfiles <- list.files(path=mydir,pattern="*.csv",full.names = TRUE)
data.daily <- ldply(myfiles,read_csv) 


### Export data
write.csv(data.daily,"DailyWaterBalance.csv")

