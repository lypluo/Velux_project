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
table.list<-list.files(paste0(Dav.path,"Polypen/"))
for (i in 1:length(table.list)) {
  Dav_temp<-read_xlsx(paste0(Dav.path,"Polypen/",table.list[i]))
  df.Dav<-rbind(df.Dav,Dav_temp)
}

#For Tharandt:
df.Tha<-c()
table.list<-list.files(paste0(Tha.path,"Polypen/"))
for (i in 1:length(table.list)) {
  Tha_temp<-read_xlsx(paste0(Tha.path,"Polypen/",table.list[i]))
  #convert the data format:
  Tha_temp<-Tha_temp %>%
    mutate(across(c(`Branch ID`:`PolyPen No.`,NDVI:RDVI)),as.numeric)
  df.Tha<-rbind(df.Tha,Tha_temp)
}

#----------------------
#(2)merge the datasets and save data
#-----------------------
#
df.Poly<-rbind(df.Dav,df.Tha)
df.Poly<-as.data.frame(df.Poly)

#save the data:
save.path<-"./data/"
save(df.Poly,file = paste0(save.path,"Polypen.data.RDA"))






  