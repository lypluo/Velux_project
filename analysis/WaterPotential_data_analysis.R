###############################################
##Aim: tidy the data (water potential) and analyze the data 
###############################################
library(readxl)
library(ggplot2)
library(tidyverse)
library(plyr)

#----------------------
#(1)load the data
#----------------------
load.path<-"./data/"
load(paste0(load.path,"WaterPotential.data.RDA"))

##converted to the water potential to negative values
df.WaterP.sel<-df.WaterP %>%
  mutate(ID=`Branch ID`)%>%
  mutate(WP_Twig= -Twig,WP_Branch= -Branch,
         Twig=NULL,Branch=NULL)%>%
  select(ID,Height,WP_Branch,WP_Twig)%>%
  mutate(sitename=substr(ID,4,6),
         CampaignNum=substr(ID,1,2),
         Position=substr(ID,12,12))
WaterP.mean<-df.WaterP.sel[,-1]%>%
  group_by(sitename,CampaignNum,Position)%>%
  dplyr::summarise(WP_Branch.mean=mean(WP_Branch,na.rm=T),
                   WP_Branch.sd=sd(WP_Branch,na.rm = T),
                   WP_Twig.mean=mean(WP_Twig,na.rm=T),
                   WP_Twig.sd=sd(WP_Twig,na.rm = T)
  )

#----------------------
#(2)plotting
#----------------------

df.WaterP.final<-df.WaterP.sel %>%
  pivot_longer(c(WP_Branch,WP_Twig),
               names_to = "WP_position",values_to = "WaterPotential")
#WP_Branch  
p_WP_Branch<-df.WaterP.sel%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=WP_Branch,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=WP_Branch,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=0.8,width=0.2)+
  geom_point()+
  facet_wrap(~CampaignNum)+
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
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

###save the results:
ggsave(p_WP_Branch,filename = paste("./manuscript/WP_Branch_var_campaigns.png"),width = 9)
ggsave(p_WP_Twig,filename = paste("./manuscript/WP_Twig_var_campaigns.png"),width = 9)
