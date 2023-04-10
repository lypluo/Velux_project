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
  df.Tha<-rbind(df.Tha,Tha_temp)
}

#----------------------
#(2)merge the datasets and data analysis
#-----------------------
#
df.Poly<-rbind(df.Dav,df.Tha)
df.Poly<-as.data.frame(df.Poly)
df.Poly.sel<-df.Poly %>%
  mutate(ID=`Branch ID`)%>%
  select(ID,NDVI,PRI)%>%
  mutate(sitename=substr(ID,4,6),
         CampaignNum=substr(ID,1,2),
         Position=substr(ID,11,11))
Poly.mean<-df.Poly.sel[,-1]%>%
  group_by(sitename,CampaignNum,Position)%>%
  dplyr::summarise(NDVI.mean=mean(NDVI,na.rm=T),
            NDVI.sd=sd(NDVI,na.rm = T),
            PRI.mean=mean(PRI,na.rm=T),
            PRI.sd=sd(PRI,na.rm = T)
            )

#----------------------
#(3)plotting
#----------------------
df.Poly.sel$NDVI<-as.numeric(df.Poly.sel$NDVI)
df.Poly.sel$PRI<-as.numeric(df.Poly.sel$PRI)

#NDVI
df.Poly.sel%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=NDVI,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=NDVI,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=0.8,width=0.2)+
  geom_point()+
  facet_wrap(~CampaignNum)+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))


#PRI
df.Poly.sel%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=PRI,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=PRI,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=0.8,width=0.2)+
  geom_point()+
  facet_wrap(~CampaignNum)+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))



  