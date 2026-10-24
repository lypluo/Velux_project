##########################################
#Tidy soil water potential(SWP) in CH-Dav
#data shared by Iris
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
load.path<-"D:/data/Velux_shared_data/CH-Dav/Soil_water_potential/"
#A. first load the SWP to check their creditablity (2020-2024):
df.SWP<-read.csv2(paste0(load.path,"CH-Dav-WP-2020-2024.csv"),
                  header = F,sep=",")
var_names<-c("rDate","SWP_5cm","SWP_15cm",
             "SWP_45cm")
unit_varnames<-c("","kPa","kPa","kPa")
df.SWP_20202024<-df.SWP[-1,]
names(df.SWP_20202024)<-var_names

#
df.SWP_20202024<-df.SWP_20202024 %>%
  mutate(rDate=ymd_hms(rDate),
         across(SWP_5cm:SWP_45cm, as.numeric)
         )
#
df.SWP_20202024_proc<-df.SWP_20202024%>%
  mutate(agg_time=floor_date(rDate,"30 minutes"))%>%
  mutate(rDate=NULL)%>%
  group_by(agg_time)%>%
  summarise(across(everything(),~mean(.x,na.rm = T)))
#
df.SWP_20202024_proc_daily<-df.SWP_20202024_proc %>%
  mutate(Date=floor_date(agg_time,"day"))%>%
  mutate(agg_time=NULL)%>%
  group_by(Date)%>%
  summarise(across(everything(),~mean(.x,na.rm = T)))

#-------------------
#(2) demonstration plot
#-------------------
df.SWP_daily<-df.SWP_20202024_proc_daily
df.plot<-df.SWP_daily %>%
  pivot_longer(c(SWP_5cm:SWP_45cm),names_to = "Height",
               values_to = "SWP")%>%
  mutate(SWP=ifelse(SWP< -50,NA,SWP))%>%
  mutate(Height=factor(Height,
        levels=c("SWP_5cm","SWP_15cm","SWP_45cm")))
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
saveRDS(df.SWP,paste0(save.path,"SWP_CH-Dav.RDS"))
