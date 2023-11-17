###############################################
##Aim: tidy the pigment data and analyze the data 
###############################################
library(readxl)
library(ggplot2)
library(tidyverse)
library(plyr)

#----------------------
#(1)load the data
#----------------------
load.path<-"./data/"
load(paste0(load.path,"Pigment.data.RDA"))

#----------------------
#(2)plotting
#----------------------
#ratio of Car to Cab 
p_CartoCab_ratio<-df.Pigment%>%
  group_by(CampaignNum) %>%
  ggplot(aes(x=Position,y=CartoCab_ratio,col=sitename,group=sitename))+
  stat_summary(aes(x=Position,y=CartoCab_ratio,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange",size=2,linewidth=1.1)+
  geom_point()+
  scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.5),
                                "THA"=adjustcolor("cyan4",0.5)))+
  facet_wrap(~CampaignNum)+
  ylim(0.1,0.95)+
  ylab(expression("Car/Cab"))+
  ##adding the sampling dates:
  geom_text(x=1,y=0.2,aes(x=Position,y=CartoCab_ratio,group=sitename,label=Date),
            data = df.Pigment[df.Pigment$sitename=="THA",])+
  geom_text(x=1,y=0.14,aes(x=Position,y=CartoCab_ratio,group=sitename,label=Date),
            data = df.Pigment[df.Pigment$sitename=="DAV",])+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

###save the results:
ggsave(p_CartoCab_ratio,filename = paste("./manuscript/CartoCab_ratio_Campaigns.png"))

