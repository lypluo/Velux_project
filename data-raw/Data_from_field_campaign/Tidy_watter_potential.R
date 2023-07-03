###############################################
##Aim: tidy the data from the field campaigns 
###############################################
library(readxl)
library(ggplot2)
library(tidyverse)
library(plyr)

#----------------------
#(1)load the data 
#----------------------
base_path<-"D:/EE_WSL/IMPACT_project/data_collection/"
#for site paths
Dav.path<-paste0(base_path,"Dav_Data/")
Tha.path<-paste0(base_path,"Tha_Data/")
#---------load the Polypen data-----------

#For Davos:
df.Dav<-c()
table.list<-list.files(paste0(Dav.path,"WaterPotential/"))
for (i in 1:length(table.list)) {
  #at this stage, using the water potential without dark adapted:
  Dav_temp<-read_xlsx(paste0(Dav.path,"WaterPotential/",table.list[i]),sheet = "WP1",skip = 3)
  df.Dav<-rbind(df.Dav,Dav_temp)
}

#For Tharandt:
df.Tha<-c()
table.list<-list.files(paste0(Tha.path,"WaterPotential/"))
for (i in 1:length(table.list)) {
  Tha_temp<-read_xlsx(paste0(Tha.path,"WaterPotential/",table.list[i]),skip = 2)
  #
  if(i>=4){
    Tha_temp<-Tha_temp[,-ncol(Tha_temp)]
  }
  df.Tha<-rbind(df.Tha,Tha_temp)
}

#----------------------
#(2)merge the datasets and save data
#-----------------------
#
df.WaterP<-rbind(df.Dav,df.Tha)
df.WaterP<-as.data.frame(df.WaterP)

#save the data:
save.path<-"./data/"
save(df.WaterP,file = paste0(save.path,"WaterPotential.data.RDA"))






  