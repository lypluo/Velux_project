#################################################
#Aim: structure the plot temporally
#e.g. x-->Date, y=Amax in different sites and height
##################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

#Date of 6 campaigns:
#For Tharandt: Mar,02; Mar,22; Apr,13; Apr,28; May,17; July,14
#For Davos:Mar,08; Mar,27; Apr,21; May,03; May,22; July,17 
#------------------
#(1)load the data
#------------------
#----------------------
#A.load the meteo data:
#----------------------
load.path<-"data/EC_MeteoandFlux/"
load(paste0(load.path,"df.Meteo.daily.RDA"))

#tidy the data:
df.Dav<-df.Meteo.daily$Dav
df.Tha<-df.Meteo.daily$Tha
names(df.Dav)
names(df.Tha)

#extract the Ta for Davos and Tharandt:
df.Dav.Meteo<-df.Dav %>%
  select(Date,TA,PPFD_IN)%>%
  filter(Date %in% as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                   "2023-05-03","2023-05-22","2023-07-17")))%>%
  mutate(sitename="DAV",
         CampaignNum=paste0("C",1:6))
df.Tha.Meteo<-df.Tha %>%
  select(Date,TA_F,PPFD_IN)%>%
  mutate(TA=TA_F,TA_F=NULL)%>%
  filter(Date %in% as.Date(c("2023-03-02","2023-03-22","2023-04-13",
                             "2023-04-28","2023-05-17","2023-07-14")))%>%
  mutate(sitename="THA",
         CampaignNum=paste0("C",1:6))
#
df.sites.Meteo<-rbind(df.Dav.Meteo,df.Tha.Meteo)

#----------------------
#B.load the LIcor measurements:
#----------------------
load.path<-"data/LIcor/"
load(paste0(load.path,"LRC.parameters.RDA"))
df.sites.LRC.paras<-df.LRC.paras %>%
  mutate(CampaignNum=substr(ID,1,2),
         Position=substr(ID,4,4))


#------------------
#(2)merge the data
#------------------
df.merge<-left_join(df.sites.LRC.paras,df.sites.Meteo)


#------------------
#(3)plotting
#------------------
p_Amax_Date<-df.merge%>%
  ggplot(aes(x=Date,y=k_sat,col=sitename,shape=Position))+
  geom_point(size=3)+
  geom_line(size=1.2)+
  # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
  #              geom="pointrange",size=3,linewidth=4)+
  scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.5),
                                "THA"=adjustcolor("cyan4",0.5)))+
  labs(
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  )+
  theme_bw()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.text = element_text(size=14),
        legend.position = c(0.8,0.25),
        legend.background = element_blank()
  )

#save the plot:
ggsave(p_Amax_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_Amax_time.png"),width = 9)



