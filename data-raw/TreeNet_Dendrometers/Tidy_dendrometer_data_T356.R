#########################################################
##Aim:Tidy the dendrometer data and aim to calculate 
#the belowground conductance (Kbg) and canopy conductance(gs)
#!!->need to use both dendrometer and sap flow data
#########################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

#---------------
#(1)load the data
#---------------
#Mikro sent me the 10mins raw data for Norway spruce
#In this file,I will only tidy the trees with tree Nr. 296, 356, and 358 as these three trees
# both have sap flow and dendrometer data--Their DBH are 40.6, 40.4, and 37.6 cm, respectively
# sap flow measurement: Tree_296:2010-present;Tree_356,Tree_358:2021-2023
# dendrometer measurements:Tree_296:1997-1998;Tree_356:2007-2024;Tree_358:1997-1999;
# In this sense-->only data from Tree_356 could be used to calculate the Kbg and gs(2021-2023)

load.path<-"D:/data/Velux_shared_data/CH-Dav/TreeNet_Davos/TreeNetData/"
filenames<-paste0("time_series_Davos-Seehornwald_",c(356),".csv")
#
df.dendro<-list()
for (i in 1:length(filenames)) {
  df.temp<-read.csv2(paste0(load.path,filenames[i]),sep = ",")
  df.dendro[[i]]<-df.temp
}
names(df.dendro)<-paste0("Tree_",c(356))


#---------------
#(2) aggregate the data to hourly:
#---------------
agg_hourly<-function(df){
  # df<-df.dendro$Tree_356
  #
  df.proc<-df %>%
    mutate(Tree_ID=series_id,series_id=NULL,
           rDate=ts,ts=NULL,
           X=NULL,
           dendro_ori=value,value=NULL
           )%>%
    mutate(Date=as.Date(rDate),Hour=hour(rDate))
  #
  # plot(df.proc$Date,df.proc$dendro_ori)
  #aggregate the data to hourly:
  df.proc_H<-df.proc %>%
    #format of data
    mutate(rDate=ymd_hms(rDate),dendro_ori=as.numeric(dendro_ori))%>%
    mutate(Year=year(rDate),rDate_H=floor_date(rDate,"hour"))%>%
    group_by(Tree_ID,rDate_H)%>%
    summarise(dendro_H=mean(dendro_ori,na.rm = T))
#
  return(df.proc_H)
  
}

#aggregate the data for Tree_356 to hourly:
df.dendro_T356<-agg_hourly(df.dendro$Tree_356)

#---------------
#(3) calculate the maximum and minimum daily xylem diamter
#---------------
df.dendro_T356_Daily<-df.dendro_T356 %>%
  mutate(Date=as_date(rDate_H))%>%
  group_by(Tree_ID,Date)%>%
  summarise(dendro_D_mean=mean(dendro_H),
            dendro_D_max=max(dendro_H),
            dendro_D_min=min(dendro_H),
            delta_dendro_D=dendro_D_max - dendro_D_min)
  summarise()
  
##save the data:
  save.path<-"./data/Tree_growth/"
  save(df.dendro_T356_Daily,file = paste0(save.path,"df.dendro_T356_Daily.RDA"))
  