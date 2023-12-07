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
##adding information for the sampling dates:
#For Tharandt: Mar,02; Mar,22; Apr,13; Apr,28; May,17; July,14
#For Davos:Mar,08; Mar,27; Apr,21; May,03; May,22; July,17 
df.Poly.sel<-df.Poly.sel %>%
  mutate(Date=case_when(c(sitename=="THA" & CampaignNum=="C1") ~"2023-03-02",
                        c(sitename=="THA" & CampaignNum=="C2") ~"2023-03-22",
                        c(sitename=="THA" & CampaignNum=="C3") ~"2023-04-13",
                        c(sitename=="THA" & CampaignNum=="C4") ~"2023-04-28",
                        c(sitename=="THA" & CampaignNum=="C5") ~"2023-05-17",
                        c(sitename=="THA" & CampaignNum=="C6") ~"2023-07-14",
                        c(sitename=="DAV" & CampaignNum=="C1") ~"2023-03-08",
                        c(sitename=="DAV" & CampaignNum=="C2") ~"2023-03-27",
                        c(sitename=="DAV" & CampaignNum=="C3") ~"2023-04-21",
                        c(sitename=="DAV" & CampaignNum=="C4") ~"2023-05-03",
                        c(sitename=="DAV" & CampaignNum=="C5") ~"2023-05-22",
                        c(sitename=="DAV" & CampaignNum=="C6") ~"2023-07-17"
         ))
#
df.Poly.sel$NDVI<-as.numeric(df.Poly.sel$NDVI)
df.Poly.sel$PRI<-as.numeric(df.Poly.sel$PRI)
#save the data:
save.path<-"./data/"
save(df.Poly.sel,file=paste0(save.path,"Polypen.data.cleaned.RDA"))
#----------------------
#(2)plotting
#----------------------
#NDVI
p_NDVI<-df.Poly.sel%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=NDVI,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=NDVI,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=0.8,width=0.2)+
  geom_point()+
  facet_wrap(~CampaignNum)+
  ##adding the sampling dates:
  geom_text(x=1.5,y=0.6,aes(x=Position,y=NDVI,group=sitename,label=Date),
            data = df.Poly.sel[df.Poly.sel$sitename=="THA",])+
  geom_text(x=1.5,y=0.575,aes(x=Position,y=NDVI,group=sitename,label=Date),
            data = df.Poly.sel[df.Poly.sel$sitename=="DAV",])+
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
  ##adding the sampling dates:
  geom_text(x=1.5,y=-0.10,aes(x=Position,y=PRI,group=sitename,label=Date),
            data = df.Poly.sel[df.Poly.sel$sitename=="THA",])+
  geom_text(x=1.5,y=-0.125,aes(x=Position,y=PRI,group=sitename,label=Date),
            data = df.Poly.sel[df.Poly.sel$sitename=="DAV",])+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

###save the results:
ggsave(p_NDVI,filename = paste("./manuscript/NDVI_var_campaigns.png"),width = 9)
ggsave(p_PRI,filename = paste("./manuscript/PRI_var_campaigns.png"),width = 9)
