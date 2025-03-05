###############################################
##Aim: tidy the water potential data from the field campaigns 
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
#select the tidied files-->with waterpotential.xlsx
table.list<-table.list[grep("Waterpotential",table.list)]
#for water potential at needle and twig
for (i in 1:length(table.list)) {
  #at this stage, using the water potential without dark adapted:
  Dav_temp<-read_xlsx(paste0(Dav.path,"WaterPotential/",table.list[i]),sheet = "WP1",skip = 3)
  if(i==6){
    #as we have several replicates in campaign 6, so we use the mean of these replicates for the analysis:
  Dav_temp<-Dav_temp %>%
    select(`Branch ID`,Height,Twig,Branch)
  }
  df.Dav<-rbind(df.Dav,Dav_temp)
}
#for water potential measured with dark adapted(represent trunk water potential)
df.Dav_trunk<-c()
for (i in 1:length(table.list)) {
  #at this stage, using the water potential without dark adapted:
  Dav_temp<-read_xlsx(paste0(Dav.path,"WaterPotential/",table.list[i]),sheet = "WP2",skip = 5)
  Dav_temp<-Dav_temp %>%
    select(`Branch ID`,Height,Twig)%>%
    mutate(Trunk=Twig, Twig=NULL)
  
  df.Dav_trunk<-rbind(df.Dav_trunk,Dav_temp)
}


#For Tharandt:
df.Tha<-c()
table.list<-list.files(paste0(Tha.path,"WaterPotential/"))
for (i in 1:length(table.list)) {
  Tha_temp<-read_xlsx(paste0(Tha.path,"WaterPotential/",table.list[i]),skip = 2)
  #
  if(i>=4){
    Tha_temp<-Tha_temp %>%
      select(`Branch ID`,Height,Twig,Branch)
  }
  df.Tha<-rbind(df.Tha,Tha_temp)
}

#----------------------
#(2)merge the datasets and save data
#-----------------------
#
df.WaterP<-rbind(df.Dav,df.Tha)
df.WaterP<-as.data.frame(df.WaterP)
#merge the Davos trunk water potential 
df.WaterP_trunk<-as.data.frame(df.Dav_trunk)
#save the data:
save.path<-"./data/Water_Potential/"
save(df.WaterP,file = paste0(save.path,"WaterPotential.data.RDA"))
save(df.WaterP_trunk,
     file = paste0(save.path,"WaterPotential_Trunk_Davos.data.RDA"))






  