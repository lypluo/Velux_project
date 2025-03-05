########################################################
#check the LUE in Davos and Tharandt
########################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

#----------------------
#(1)load the data:
#----------------------
load.path<-"./data/EC_MeteoandFlux/PAR/"
#1997-2023
df.PAR_Tha<-readRDS(paste0(load.path,"PAR_DE-Tha.RDS"))
#2021-2023
df.PAR_Dav<-readRDS(paste0(load.path,"PAR_CH-Dav.RDS"))

##only select the data after 2021 to compare between Tha and Dav:
#and only select the PAR above the canopy and below the canopy:
df.PAR_Tha_sel<-df.PAR_Tha %>%
  dplyr::select(Date,PAR_37m,PAR_2m)%>%
  mutate(Sitename="DE-Tha")%>%
  filter(Date>=as.POSIXct("2021-01-01"))%>%
  mutate(PAR_above=PAR_37m,PAR_below=PAR_2m,
         PAR_37m=NULL,PAR_2m=NULL)
df.PAR_Dav_sel<-df.PAR_Dav%>%
  mutate(Sitename="CH-Dav")%>%
  mutate(PAR_above=PAR_35m,PAR_below=PAR_2m,
         PAR_35m=NULL,PAR_2m=NULL)
##
df.PAR<-rbind(df.PAR_Dav_sel,df.PAR_Tha_sel)

#-------------------
#(2) plotting:
#-------------------
#A:PAR in different height
df.PAR_plot<-df.PAR %>%
  pivot_longer(c(PAR_above,PAR_below),names_to = "Height",
               values_to = "PAR")%>%
  mutate(PAR=ifelse(PAR< 0,NA,PAR))%>%
  mutate(Height=factor(Height,
                      levels=c("PAR_above","PAR_below")))
#color
p1<-df.PAR_plot %>%
  ggplot(aes(x=Date,y=PAR,col=Sitename,shape=Height))+
  geom_point()+
  geom_smooth(span=0.3)+
  # geom_smooth(span=0.5,se=FALSE)+
  scale_color_manual(values = c("CH-Dav"=adjustcolor("red",0.5),
                               "DE-Tha"=adjustcolor("orange",0.5)))+
  scale_shape_manual(values = c("PAR_above"=16,"PAR_below"=4))+
  # addding date: "2023-03-27"
  # geom_vline(xintercept = as.POSIXct("2023-03-27"),col="black")+
  # annotate(geom="text",x=as.POSIXct("2023-03-27")+50*3600*24,y=-50,
  #          label="Mar,27",col="black",size=6)+
  xlim(as.POSIXct(as.Date("2021-01-01")),as.POSIXct(as.Date("2023-12-31")))+
  ylab(expression("PAR"[IN]*" ("*mu*"mol m"^-2*s^-1*")"))+
  xlab("")+
  labs(color ="Sitename")+
  theme_light()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size=18),
        legend.background = element_blank(),
        axis.title = element_text(size=20),
        axis.text = element_text(size = 16)
        # legend.position =c(0.6,0.5)
  )+
  ylim(-10,800)

#B:calculate the absorbed PAR from tree canopy(above-below)
df.PAR_plot_absorbed<-df.PAR %>%
    mutate(PAR_absorbed=PAR_above - PAR_below)%>%
    mutate(PAR_absorbed=ifelse(PAR_absorbed< 0 | PAR_absorbed> 800,NA,PAR_absorbed))
p2<-df.PAR_plot_absorbed %>%
  ggplot(aes(x=Date,y=PAR_absorbed,col=Sitename))+
  geom_point()+
  geom_smooth(span=0.3)+
  scale_color_manual(values = c("CH-Dav"=adjustcolor("red",0.5),
                                "DE-Tha"=adjustcolor("orange",0.5)))+
  # addding date: "2023-03-27"
  # geom_vline(xintercept = 86,col="black")+
  # annotate(geom="text",x=110,y=-30,
  #          label="Mar,27",col="black",size=6)+
  xlim(as.POSIXct(as.Date("2021-01-01")),as.POSIXct(as.Date("2023-12-31")))+
  ylab(expression("PAR"[absorbed]*" ("*mu*"mol m"^-2*s^-1*")"))+
  xlab("")+
  # annotate(geom = "text",x=20,y=4,label="CH-Dav",size=6)+
  theme_light()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size=18),
        legend.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20)
  )+
  ylim(-10,800)

#C:Eco LUE:
#load the GPP data in two sites:
load.path<-"./data/EC_MeteoandFlux/"
load(paste0(load.path,"df_daily_from_ICOS.RDA"))

df.Dav<-df_DD$Dav
df.Tha<-df_DD$Tha

#select GPP and merge data from two sites
#period:2021-2023
df.Dav_GPP<-df.Dav %>%
  dplyr::select(Date,GPP)%>%
  filter(Date>=as.Date("2021-01-01") & Date<=as.Date("2023-12-31"))%>%
  mutate(Sitename="CH-Dav")
df.Tha_GPP<-df.Tha %>%
  dplyr::select(Date,GPP)%>%
  filter(Date>=as.Date("2021-01-01") & Date<=as.Date("2023-12-31"))%>%
  mutate(Sitename="DE-Tha")
df.GPP<-rbind(df.Dav_GPP,df.Tha_GPP)

#merge the data:
df.PAR_absorbed<-df.PAR_plot_absorbed %>%
  mutate(Date=as.Date(Date))
df.merge<-left_join(df.GPP,df.PAR_plot_absorbed)

#calculate the LUE
df.LUE_plot<-df.merge %>%
  #convert the unit-->GPP: gC m-2 d-1; PAR_absorbed: u mol m-2 s-1
  #convert the GPP unit-->umol C m-2 s-1-->
  #LUE unit: umol C m-2 umol-1
  mutate(LUE=c(GPP/12*10^6/c(24*60*60))/PAR_absorbed)%>%
  mutate(LUE=ifelse(LUE< 0 |LUE >0.1,NA,LUE))
#save the LUE data:
save.path<-"./data/Comprehensive_plot_data/Fig2/"
saveRDS(df.LUE_plot,paste0(save.path,"LUE_2sites.RDS"))

#
p3<-df.LUE_plot %>%
  ggplot(aes(x=Date,y=LUE,col=Sitename))+
  geom_point()+
  geom_smooth(span=0.3)+
  xlim(as.POSIXct(as.Date("2021-01-01")),as.POSIXct(as.Date("2023-12-31")))+
  scale_color_manual(values = c("CH-Dav"=adjustcolor("red",0.5),
                                "DE-Tha"=adjustcolor("orange",0.5)))+
  # addding date: "2023-03-27"
  # geom_vline(xintercept = 86,col="black")+
  # annotate(geom="text",x=110,y=-50,
  #          label="Mar,27",col="black",size=6)+
  ylab(expression("LUE"[Eco]*" ("*mu*"mol C m"^-2*mu*"mol"^-1*")"))+
  xlab("")+
  # annotate(geom = "text",x=20,y=4,label="CH-Tha",size=6)+
  theme_light()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size=18),
        legend.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20)
  )

#---------
#Merge the plots
#---------
p_merge<-plot_grid(p1,p2,p3,
          align = "hv",ncol = 1,nrow=3,
          labels = c("(a)","(b)","(c)")
          )
#
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig2_supp1_Eco_LUE_new.png"),
       p_merge,width = 16,height=13)

