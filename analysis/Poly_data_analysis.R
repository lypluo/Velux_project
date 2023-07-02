###############################################
##Aim: tidy the data and analyze the data 
###############################################
library(readxl)
library(ggplot2)
library(tidyverse)
library(plyr)

#----------------------
#(1)load the data
#----------------------
load.path<-"./data/"
load(paste0(load.path,"Polypen.data.RDA"))

##
df.Poly.sel<-df.Poly %>%
  mutate(ID=`Branch ID`)%>%
  select(ID,NDVI,PRI)%>%
  mutate(sitename=substr(ID,4,6),
         CampaignNum=substr(ID,1,2),
         Position=substr(ID,12,12))
Poly.mean<-df.Poly.sel[,-1]%>%
  group_by(sitename,CampaignNum,Position)%>%
  dplyr::summarise(NDVI.mean=mean(NDVI,na.rm=T),
                   NDVI.sd=sd(NDVI,na.rm = T),
                   PRI.mean=mean(PRI,na.rm=T),
                   PRI.sd=sd(PRI,na.rm = T)
  )

#----------------------
#(2)plotting
#----------------------
df.Poly.sel$NDVI<-as.numeric(df.Poly.sel$NDVI)
df.Poly.sel$PRI<-as.numeric(df.Poly.sel$PRI)

#NDVI
p_NDVI<-df.Poly.sel%>%
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
p_PRI<-df.Poly.sel%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=PRI,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=PRI,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=0.8,width=0.2)+
  geom_point()+
  facet_wrap(~CampaignNum)+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

###save the results:
ggsave(p_NDVI,filename = paste("./manuscript/NDVI_var_campaigns.png"),width = 9)
ggsave(p_PRI,filename = paste("./manuscript/PRI_var_campaigns.png"),width = 9)
