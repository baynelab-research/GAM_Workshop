#Temporal Behavior analysis
#libraries
library(tidyverse)
library(mgcv)
library(ggplot2)


Data<-read.csv("tidydata.csv")#Our Ruffed Grouse dataset
PIWOdata<-read.csv("DEMOpiwodata.csv")#forest types are random

#Let's explore the data and trends
#We will use PIWO data to start out
#make some graphs quickly to show general trends
#PIWOdrum binary detection/non-detection of a pileated woodpecker drum in a recording

ggplot(PIWOdata)+geom_col(aes(x=hour,y=PIWOdrum))
ggplot(PIWOdata)+geom_smooth(aes(x=hour,y=PIWOdrum))
#What might make you think from this graph we should use a GAM?
##Non-symmetrical
##Oscillating responses
##Circular

#Let's try the most basic approach
gam1<-gam(PIWOdrum~s(hour,bs="cp"),data=PIWOdata,family = "binomial")
#bs=cp due to the circular nature of time
summary(gam1)
gam.check(gam1)#check to see if k is at least 2 greater than edf

#Set location as factor so you can use as a random effect
Data$location<-as.factor(Data$location)
PIWOdata$location<-as.factor(PIWOdata$location)

#Adding a random effect of location
gam2<-gam(PIWOdrum~s(hour,bs="cp")+s(location,bs="re"),data=PIWOdata,family = "binomial")
summary(gam2)
gam.check(gam2)

#Adding more variables
gam3<-gam(PIWOdrum~s(hour,bs="cp")+s(location,bs="re")+s(latitude),
          data=PIWOdata,family = "binomial")
summary(gam3)
gam.check(gam3)

#Interaction with variables
gam4<-gam(PIWOdrum~s(hour,bs="cp")+s(location,bs="re")+s(latitude)+
          ti(hour,latitude,bs=c("cp","tp")),data=PIWOdata,family = "binomial")
summary(gam4)
gam.check(gam4)

#Interaction with catergorical variables
#Need to make Forest Type a factor
PIWOdata$Forest.Type<-as.factor(PIWOdata$Forest.Type)

gam5<-gam(PIWOdrum~Forest.Type+s(hour,bs="cp")+s(location,bs="re")+
          ti(hour,by=Forest.Type),data=PIWOdata,family = "binomial")
#Still have to figure out the bs for the interaction
summary(gam5)
gam.check(gam5)


############
#Try some stuff out with the RUGR data!
#Single Variable GAM Models
julian<-gam(RUGR~s(julian)+s(location,bs="re"), data=Data,family="binomial")
summary(julian)
gam.check(julian)

daylen<-gam(RUGR~s(daylength)+s(location,bs="re"), data=Data,family="binomial")
summary(daylen)
gam.check(daylen)

meantemp<-gam(RUGR~s(daily_mean_temp)+s(location,bs="re"),data=Data,family="binomial")
summary(meantemp)
gam.check(meantemp)

AIC(julian,daylen,meantemp)

#Plotting single variable models
ggplot(Data)+geom_smooth(aes(y=RUGR,x=julian),method="gam",formula=y~s(x,bs='tp'))

