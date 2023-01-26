#------------Tidying Time of Year Data------------#

####Packages####
library(tidyverse)
library(suncalc)
library(elevatr)
library(lubridate)

####Reading WT data####
TagData<-read.csv("RUGRdataset/ABMI_RUGR_-_BU_LAB_PROJECT_tag_details_report.csv",fileEncoding="UTF-8-BOM")
TaskData<-read.csv("RUGRdataset/ABMI_RUGR_-_BU_LAB_PROJECT_recording_task_report.csv",fileEncoding="UTF-8-BOM")

TaskData<-TaskData%>%filter(status%in%c("Transcribed","Bad Weather"))
TaskData$ID<-paste(TaskData$location, TaskData$recording_date)
TagData$ID<-paste(TagData$location,TagData$recording_date)

countData<-TagData%>%filter(vocalization=="Non-vocal")%>%count(ID)
TagData<-TagData%>%mutate(vocalization_rate=countData$n[match(TagData$ID,countData$ID)])
TagData$vocalization_rate<-replace_na(TagData$vocalization_rate,0)

####Combine Tag and Task Data####
Data<-TaskData%>%select(organization:latitude,daily_min_temp:daily_mean_temp,url,
                        daily_precipitation_mm,hourly_wind_speed,daily_snow_on_ground_cm)%>%
  mutate(species_code=TagData$species_code[match(TaskData$ID,TagData$ID)],
         vocalization=TagData$vocalization[match(TaskData$ID,TagData$ID)],
         drum_rate=TagData$vocalization_rate[match(TaskData$ID,TagData$ID)])
Data$drum_rate<-replace_na(Data$drum_rate,0)

####Extract RUGR Detection####
Data$RUGR<-0
Data$RUGR[grepl("RUGR",Data$species_code)]=1
Data<-Data%>%select(!species_code) #TIDY'D Pres/Absence RUGR Data

####Date Data Cleanup####
Data$julian<-yday(Data$recording_date)
Data$week<-as.integer(week(Data$recording_date))
Data$recording_date<-as.POSIXct(Data$recording_date)

Data$date<-as.Date(Data$recording_date)
Data$lat<-Data$latitude
Data$lon<-Data$longitude

####Sunrise####
sunrisetimes<-unique(getSunlightTimes(data=Data,tz="MST"))
sunrisetimes$ID<-paste(sunrisetimes$date,sunrisetimes$lat,sunrisetimes$lon)

azimuth<-unique(getSunlightPosition(data=Data,keep="azimuth"))
azimuth$ID<-paste(azimuth$date,azimuth$lat,azimuth$lon)
Data$ID<-paste(Data$date,Data$lat,Data$lon)

Data<-Data%>%select(organization:month)%>%mutate(azimuth=azimuth$azimuth[match(Data$ID,azimuth$ID)],
                                                     sunrise=sunrisetimes$sunrise[match(Data$ID,sunrisetimes$ID)])

Data$hour<-hour(Data$recording_date)
Data$TSSR<-TaskData$TSSR<-as.numeric(difftime(Data$recording_date,Data$sunrise,units="hours",tz="MST"))

####Elevation assign####
#Rearrange table for elevatr package#
col_order<- c("longitude","latitude","location")
elevation_locations<-Data[col_order]
elevation_locations<-elevation_locations%>%filter(!is.na(latitude))
#Extract elevations using elevatr#
projection<-'EPSG:4326'
spatial_data<-get_elev_point(locations=elevation_locations,prj=projection,src="aws",overwrite = T)
write.csv(spatial_data,"elevations.csv")
elevations<-read.csv("elevations.csv")
Data$elevation<-elevations$elevation[match(Data$location,elevations$location)]


####Day length####
Data$ID<-paste(Data$date,Data$lat,Data$lon)

Data<-Data%>%select(organization:elevation)%>%mutate(sunset=sunrisetimes$sunset[match(Data$ID,sunrisetimes$ID)],
                                                            sunrise=sunrisetimes$sunrise[match(Data$ID,sunrisetimes$ID)])
Data$daylength<-difftime(Data$sunset,Data$sunrise,units="hours")

####Remove the direction from location####
Data$location<-substr(Data$location,1,nchar(Data$location)-3)

####Tidy up data frame####
TidyData<-Data%>%select(location,latitude,longitude,recording_date,RUGR,drum_rate,julian,week,
                          elevation,daily_min_temp,daily_max_temp,daily_mean_temp, TSSR, azimuth,
                          daily_precipitation_mm, daylength)
TidyData<-na.omit(TidyData)
write.csv(TidyData,"Tidydata.csv")
####Aggregates for visualization####
agg<-aggregate(RUGR~julian,data=Data,mean)
plot(agg)
agg2<-aggregate(RUGR~round(TSSR),data=Data,mean)
plot(agg2)



