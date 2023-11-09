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
load(paste0(load.path,"Leaf_traits.data.RDA"))

##
df.traits.sel<-df.traits %>%
  mutate(ID=`sample_ID`)%>%
  select(ID,Measurement,Object_number,Needle_dry_weight,
         ImageJ_leaf_area:ImageJ_average_width)%>%
  mutate(sitename=substr(ID,4,6),
         CampaignNum=substr(ID,1,2),
         Position=substr(ID,11,11))
#calculate some traits:
#convert the numeric variables to numeric:
df.traits.sel[,4:11]<-apply(df.traits.sel[,4:11],2,as.numeric)
#e.g. SLA=Area/Mass; LMA=Mass/Area
df.traits.sel<-df.traits.sel%>%
  mutate(SLA=ImageJ_total_area/Needle_dry_weight,
         LMA=Needle_dry_weight/ImageJ_leaf_area)

##aggregate
Traits.mean<-df.traits.sel%>%
  group_by(sitename,CampaignNum,Position)%>%
  dplyr::summarise(SLA.mean=mean(SLA,na.rm=T),
                   SLA.sd=sd(SLA,na.rm = T),
                   LMA.mean=mean(LMA,na.rm=T),
                   LMA.sd=sd(LMA,na.rm = T),
                   Leaf_length.mean=mean(ImageJ_average_length,na.rm = T),
                   Leaf_length.sd=sd(ImageJ_average_length,na.rm = T),
                   Leaf_width.mean=mean(ImageJ_average_width,na.rm = T),
                   Leaf_width.sd=sd(ImageJ_average_width,na.rm = T)
  )

#----------------------
#(2)plotting
#----------------------

#SLA
p_SLA<-df.traits.sel%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=SLA,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=SLA,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=2,linewidth=1.5)+
  geom_point(size=1.5)+
  scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.6),
                                "THA"=adjustcolor("cyan4",0.6)))+
  ylab(expression("SLA (cm"^2*"g"^-1*")"))+
  facet_wrap(~CampaignNum)+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

#LMA
p_LMA<-df.traits.sel%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=LMA,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=LMA,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=2,linewidth=1.5)+
  geom_point(size=1.5)+
  scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.6),
                                "THA"=adjustcolor("cyan4",0.6)))+
  ylab(expression("LMA (g"*"cm"^-2*")"))+
  facet_wrap(~CampaignNum)+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

#leaf length
p_Leaf_length<-df.traits.sel%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=ImageJ_average_length,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=ImageJ_average_length,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=2,linewidth=1.5)+
  geom_point(size=1.5)+
  scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.6),
                                "THA"=adjustcolor("cyan4",0.6)))+
  ylab(expression("Needle average length (cm)"))+
  facet_wrap(~CampaignNum)+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

#leaf width
p_Leaf_width<-df.traits.sel%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=ImageJ_average_width,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=ImageJ_average_width,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=2,linewidth=1.5)+
  geom_point(size=1.5)+
  scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.6),
                                "THA"=adjustcolor("cyan4",0.6)))+
  ylab(expression("Needle average width (cm)"))+
  facet_wrap(~CampaignNum)+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

###save the results:
ggsave(p_SLA,filename = paste("./manuscript/SLA_var_campaigns.png"),width = 9)
ggsave(p_LMA,filename = paste("./manuscript/LMA_var_campaigns.png"),width = 9)
ggsave(p_Leaf_width,filename = paste("./manuscript/Leaf_width_var_campaigns.png"),width = 9)

