##################################################
#Aim: tidy the data from sensors we deployed in the Tharandt
##################################################

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(dplyr)
library(tidyverse)
library(lubridate)

#----------------------
#(1)load and tidy the data
#---------------------
load.path<-"D:/EE_WSL/IMPACT_project/data_collection/THA_Data/tower_based_VIs/NDVI_PRI_tidy_files/"
file.names<-list.files(paste0(load.path))

#--for NDVI---
NDVI.files<-file.names[grep("NDVI",file.names)]
read_data<-function(df_path){
  # df_path<-paste0(load.path,NDVI.files[1])
  df_temp<-read.csv2(df_path,skip = 3,sep=",")
  table.names<-read.csv2(df_path,sep=",")[1,]
  names(df_temp)<-as.character(table.names[!is.na(table.names)])
  return(df_temp)
}
#
df.NDVI<-c()
for(i in 1:length(NDVI.files)){
  NDVI_temp<-read_data(paste0(load.path,file.names[i]))
  df.NDVI<-rbind(df.NDVI,NDVI_temp)
}
#
df.NDVI_temp<-df.NDVI %>%
  dplyr::select(TIMESTAMP,RECORD,NDVI_Avg)%>%
  mutate(TIMESTAMP=ymd_hms(TIMESTAMP))%>%
  mutate(Date=as.Date(TIMESTAMP),Hour=hour(TIMESTAMP),
         NDVI_Avg=as.numeric(NDVI_Avg))%>%
  filter(Hour>=10 & Hour<=14)%>%
  group_by(Date)%>%
  dplyr::summarise(NDVI=mean(NDVI_Avg,na.rm=T))%>%
  mutate(NDVI=ifelse(NDVI>=-1 & NDVI<=1, NDVI<-NDVI,NDVI<-NA))

#test plot:
t_NDVI<-df.NDVI_temp%>%
  ggplot()+
  geom_point(aes(x=Date,y=NDVI))


#--for PRI---
PRI.files<-file.names[grep("PRI",file.names)]
#
df.PRI<-c()
for(i in 1:length(PRI.files)){
  PRI_temp<-read_data(paste0(load.path,PRI.files[i]))
  df.PRI<-rbind(df.PRI,PRI_temp)
}
#original data recorded as every minute
#merge to daily(using the data in the midday)
df.PRI_temp<-df.PRI %>%
  dplyr::select(TIMESTAMP,RECORD,PRI_Avg)%>%
  mutate(TIMESTAMP=ymd_hms(TIMESTAMP))%>%
  mutate(Date=as.Date(TIMESTAMP),Hour=hour(TIMESTAMP),
         PRI_Avg=as.numeric(PRI_Avg))%>%
  filter(Hour>=10 & Hour<=14)%>%
  group_by(Date)%>%
  dplyr::summarise(PRI=mean(PRI_Avg,na.rm=T))

#test plot:
t_PRI<-df.PRI_temp%>%
  ggplot()+
  geom_point(aes(x=Date,y=PRI))
#
library(cowplot)
t_p_merge<-plot_grid(t_NDVI,t_PRI,nrow=2)

#----save the plot
ggsave(filename = paste0("./test/check_tower_based_VIs/Tharandt_VIs.png"),t_p_merge)
##save the data:
df.all.Tha<-left_join(df.NDVI_temp,df.PRI_temp)
####apply the filtering method
library(zoo)
source(file = "./R/max.filter.R")
#for NDVI, using 90% percentile to filter
df.filter_max1<-max.filter(df.all.Tha,c("NDVI"),act.opts = data.frame(w=7,qt=0.9))
#other variables, using 50% percentile to filter:
df.filter_max2<-max.filter(df.all.Tha,c("PRI"),act.opts = data.frame(w=7,qt=0.5))
#
df.all.Tha<-cbind(df.all.Tha,"NDVI.max.filtered"=df.filter_max1$NDVI.max.filtered,
                  "PRI.max.filtered"=df.filter_max2$PRI.max.filtered)%>%
  dplyr::filter(NDVI<=1&NDVI>=0)
save(df.all.Tha,file = paste0("./data/Tower_based_VIs/Tharandt_VIs.RDA"))
