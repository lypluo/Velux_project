########################################################
#Aim: tidy the sap flow density(SFD) sent by Ankit and calculate the stand sap flow
########################################################
library(tidyverse)
library(dplyr)
library(readxl)

#----------------
#(0)load the data
#----------------
#load the sap flow density (SFD) data:
load.path<-"./data-raw/Data_from_PIs/CH_Dav/sap_flow/"
files<-list.files(load.path)
sel_files<-files[grep(".csv",files)]
df.sap<-read.csv2(paste0(load.path,sel_files),sep = ",")
df.sap<-df.sap%>%
  mutate(sitename="CH-Dav")

#load the details for different trees:
df.DBH_H<-read_excel(paste0(load.path,"TreeNumber_details.xlsx"))
df.DBH_H<-df.DBH_H%>%
  mutate(sitename=Site,Site=NULL)

#
df.sap.agg<-left_join(df.sap,df.DBH_H)%>%
  mutate(Date=as.Date(Date),SFDm=as.numeric(SFDm))

#----------------
#(1)test plotting
#----------------
p.sap.test<-df.sap.agg %>%
  group_by(TreeNumber)%>%
  ggplot()+
  geom_point(aes(Date,SFDm))+
  facet_wrap(~TreeNumber)+
  theme_light()
##
save.path<-"./test/check_sapflow/"
ggsave(p.sap.test,filename = paste0(save.path,"Variation_of_SFDm.png"),
       width = 12,height = 10)
