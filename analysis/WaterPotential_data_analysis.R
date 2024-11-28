###############################################
##Aim: tidy the data (water potential) and analyze the data 
###############################################
library(readxl)
library(ggplot2)
library(tidyverse)
library(plyr)
library(dplyr)

#----------------------
#(1)load the data
#----------------------
load.path<-"./data/Water_Potential/"
#Needle and Twig
load(paste0(load.path,"WaterPotential.data.RDA"))
#Trunk
load(paste0(load.path,"WaterPotential_Trunk_Davos.data.RDA"))

##converted to the water potential to negative values
#Needle and Twig
df.WaterP.sel<-df.WaterP %>%
  mutate(ID=`Branch ID`)%>%
  mutate(WP_Twig= -Twig,WP_Branch= -Branch,
         Twig=NULL,Branch=NULL)%>%
  select(ID,Height,WP_Branch,WP_Twig)%>%
  mutate(sitename=substr(ID,4,6),
         CampaignNum=substr(ID,1,2),
         Position=substr(ID,12,12))
#Trunk:
df.WaterP_trunk.sel<-df.WaterP_trunk%>%
  mutate(Trunk=as.numeric(Trunk))%>%
  mutate(ID=`Branch ID`)%>%
  mutate(WP_Trunk= -Trunk,
         Trunk=NULL)%>%
  select(ID,Height,WP_Trunk)%>%
  mutate(sitename=substr(ID,4,6),
         CampaignNum=substr(ID,1,2),
         Position=substr(ID,11,11))
#merge the data
df.WaterP_all<-dplyr::bind_rows(df.WaterP.sel,df.WaterP_trunk.sel)
#
WaterP.mean<-df.WaterP_all[,-1]%>%
  group_by(sitename,CampaignNum,Position)%>%
  dplyr::summarise(WP_Branch.mean=mean(WP_Branch,na.rm=T),
                   WP_Branch.sd=sd(WP_Branch,na.rm = T),
                   WP_Twig.mean=mean(WP_Twig,na.rm=T),
                   WP_Twig.sd=sd(WP_Twig,na.rm = T),
                   WP_Trunk.mean=mean(WP_Trunk,na.rm = T),
                   WP_Trunk.sd=sd(WP_Trunk,na.rm=T)
  )
##adding information for the sampling dates:
#For Tharandt: Mar,02; Mar,22; Apr,13; Apr,28; May,17; July,14
#For Davos:Mar,08; Mar,27; Apr,21; May,03; May,22; July,17 
df.WaterP.final<-df.WaterP_all %>%
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
#save the data
save.path<-"./data/Water_Potential/"
save(df.WaterP.final,file=paste0(save.path,"WaterPotential.data.cleaned.RDA"))
#----------------------
#(2)plotting
#----------------------
df.WaterP.final<-df.WaterP.final %>%
  pivot_longer(c(WP_Branch,WP_Twig,WP_Trunk),
               names_to = "WP_position",values_to = "WaterPotential")
#WP_Trunk 
p_WP_Trunk<-df.WaterP_all%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=WP_Trunk,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=WP_Trunk,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=0.8,width=0.2)+
  geom_point()+
  facet_wrap(~CampaignNum)+
  ##adding the sampling dates:
  # geom_text(x=1,y=-30,aes(x=Position,y=WP_Branch,group=sitename,label=Date),
  #           data = df.WaterP.sel[df.WaterP.sel$sitename=="THA",])+
  geom_text(x=1,y=-32,aes(x=Position,y=WP_Branch,group=sitename,label=Date),
            data = df.WaterP.sel[df.WaterP.sel$sitename=="DAV",])+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

#WP_Branch  
p_WP_Branch<-df.WaterP.sel%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=WP_Branch,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=WP_Branch,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=0.8,width=0.2)+
  geom_point()+
  facet_wrap(~CampaignNum)+
  ##adding the sampling dates:
  geom_text(x=1,y=-30,aes(x=Position,y=WP_Branch,group=sitename,label=Date),
            data = df.WaterP.sel[df.WaterP.sel$sitename=="THA",])+
  geom_text(x=1,y=-32,aes(x=Position,y=WP_Branch,group=sitename,label=Date),
            data = df.WaterP.sel[df.WaterP.sel$sitename=="DAV",])+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

#WP_Twig
p_WP_Twig<-df.WaterP.sel%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=WP_Twig,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=WP_Twig,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=0.8,width=0.2)+
  geom_point()+
  facet_wrap(~CampaignNum)+
  ##adding the sampling dates:
  geom_text(x=1,y=-30,aes(x=Position,y=WP_Twig,group=sitename,label=Date),
            data = df.WaterP.sel[df.WaterP.sel$sitename=="THA",])+
  geom_text(x=1,y=-32,aes(x=Position,y=WP_Twig,group=sitename,label=Date),
            data = df.WaterP.sel[df.WaterP.sel$sitename=="DAV",])+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

###save the results:
ggsave(p_WP_Branch,filename = paste("./manuscript/WP_Branch_var_campaigns.png"),width = 9)
ggsave(p_WP_Twig,filename = paste("./manuscript/WP_Twig_var_campaigns.png"),width = 9)
