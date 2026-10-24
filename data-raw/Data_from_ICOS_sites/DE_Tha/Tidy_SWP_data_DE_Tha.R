##########################################
#Tidy soil water potential(SWP) in CH-Dav
#data shared by Thomas
##########################################
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)

#-------------------
#(1) load the data and test
#-------------------
#
load.path<-"D:/data/Velux_shared_data/DE-THa/Soil_water_potential/"
#A. first load the SWP to check their creditablity (1997-2014):
#Thomas mentioned from 2005-2014-->the data might not be that accurate
df.SWP<-read_xlsx(paste0(load.path,"DE-Tha_soil water potential_1997-2014.xlsx"))
var_names<-c("rDate","SWP_20cm","SWP_30cm","SWP_50cm","SWP_70cm",
             "SWP_90cm")
names(df.SWP)<-var_names
#from checking the data-->I found the most of the time data are the postive, sometimes
#are the negative-->change the all values to the positive
df.SWP<-df.SWP %>%
  mutate(SWP_20cm=ifelse(!is.na(SWP_20cm) & SWP_20cm<0, SWP_20cm*-1,SWP_20cm),
         SWP_30cm=ifelse(!is.na(SWP_30cm) & SWP_30cm<0, SWP_30cm*-1,SWP_30cm),
         SWP_50cm=ifelse(!is.na(SWP_50cm) & SWP_50cm<0, SWP_50cm*-1,SWP_50cm),
         SWP_70cm=ifelse(!is.na(SWP_70cm) & SWP_70cm<0, SWP_70cm*-1,SWP_70cm),
         SWP_90cm=ifelse(!is.na(SWP_90cm) & SWP_90cm<0, SWP_90cm*-1,SWP_90cm)
         )
#convert the unit-->from pa to kPa (and convert to the minus value)
df.SWP<-df.SWP %>%
  mutate(across(SWP_20cm:SWP_90cm,function(x){-1*x/1000}))
unit_varnames<-c("","kPa","kPa","kPa","kPa","kPa")
df.SWP_19972014<-df.SWP

#
df.SWP_19972014_proc_daily<-df.SWP_19972014 %>%
  mutate(Date=rDate)%>%
  group_by(Date)%>%
  summarise(across(everything(),~mean(.x,na.rm = T)))


#-------------------
#(2) demonstration plot
#-------------------
df.SWP_daily<-df.SWP_19972014_proc_daily%>%
  mutate(rDate=NULL)
df.plot<-df.SWP_daily %>%
  pivot_longer(c(SWP_20cm:SWP_90cm),names_to = "Height",
               values_to = "SWP")%>%
  mutate(SWP=ifelse(SWP< -100,NA,SWP))%>%
  mutate(Height=factor(Height,
        levels=c("SWP_20cm","SWP_30cm","SWP_50cm",
                 "SWP_70cm","SWP_90cm")))
#
df.plot %>%
  ggplot(aes(x=Date,y=SWP,col=Height))+
  geom_point()
#
df.plot_agg<-df.plot %>%
  mutate(DoY=yday(Date))%>%
  group_by(DoY,Height)%>%
  summarise(SWP=mean(SWP,na.rm = T),
            SWP_sd=sd(SWP,na.rm=T))
df.plot_agg %>%
  ggplot(aes(x=DoY,y=SWP,col=Height))+
  geom_point()+
  geom_smooth()

#-------------------
#(4) save the data
#-------------------
df.SWP<-df.SWP_daily
save.path<-"./data/Soil_water_potential/"
saveRDS(df.SWP,paste0(save.path,"SWP_DE-Tha.RDS"))
