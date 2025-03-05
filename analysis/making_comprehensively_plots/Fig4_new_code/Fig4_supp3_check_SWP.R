########################################################
#check the soil water potential in Davos and Tharandt
#especially for Davos in 2023
########################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

#----------------------
#(1)load the data:
#----------------------
load.path<-"./data/Soil_water_potential/"
#1997-2014
df.SWP_Tha<-readRDS(paste0(load.path,"SWP_DE-Tha.RDS"))
#2020-2024
df.SWP_Dav<-readRDS(paste0(load.path,"SWP_CH-Dav.RDS"))

#-------------------
#(2) plotting:
#-------------------

#A:Dav daily time series---
df.Dav_plot<-df.SWP_Dav %>%
  pivot_longer(c(SWP_5cm:SWP_45cm),names_to = "Depth",
               values_to = "SWP")%>%
  mutate(SWP=ifelse(SWP< -50,NA,SWP))%>%
  mutate(Depth=factor(Depth,
  levels=c("SWP_5cm","SWP_15cm","SWP_45cm")))

#color
Dav_cols<-viridis::viridis(3,option = "D",direction = -1)
p1<-df.Dav_plot %>%
  filter(Date<=as.POSIXct("2023-12-31"))%>%
  #only select the data in 15cm and 45cm as the data in 5cm varies too much
  # filter(Depth!="SWP_5cm")%>%
  ggplot(aes(x=Date,y=SWP,col=Depth))+
  geom_point()+
  # geom_smooth(span=0.5,se=FALSE)+
  #adding at Mar, 2025: SWP-->fiedl capacity
  geom_hline(yintercept = -33,lty=2,size=1.1)+
  scale_color_manual(values = c(
  "SWP_5cm"=adjustcolor(Dav_cols[1],0.5),
  "SWP_15cm"=adjustcolor(Dav_cols[2],0.5),
  "SWP_45cm"=adjustcolor(Dav_cols[3],0.5)))+
  # addding date: "2023-03-27"
  geom_vline(xintercept = as.POSIXct("2023-03-27"),col="black")+
  annotate(geom="text",x=as.POSIXct("2023-03-27")+50*3600*24,y=-50,
           label="Mar,27",col="black",size=6)+
  ylab("CH-Dav SWP (kPa)")+
  xlab("")+
  labs(color ="Depth")+
  theme_light()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size=18),
        legend.background = element_blank(),
        axis.title = element_text(size=20),
        axis.text = element_text(size = 16)
        # legend.position =c(0.6,0.5)
  )

#B:Davos multi-year mean
df.Dav_plot_agg<-df.Dav_plot %>%
  mutate(DoY=yday(Date))%>%
  group_by(DoY,Depth)%>%
  summarise(SWP=mean(SWP,na.rm = T),
            SWP_sd=sd(SWP,na.rm=T))
p2<-df.Dav_plot_agg %>%
  ggplot(aes(x=DoY,y=SWP,col=Depth))+
  geom_point()+
  geom_smooth(span=0.3)+
  #adding at Mar, 2025: SWP-->fiedl capacity
  geom_hline(yintercept = -33,lty=2,size=1.1)+
  scale_color_manual(values = c(
    "SWP_5cm"=adjustcolor(Dav_cols[1],0.8),
    "SWP_15cm"=adjustcolor(Dav_cols[2],0.8),
    "SWP_45cm"=adjustcolor(Dav_cols[3],0.8)))+
  # addding date: "2023-03-27"
  geom_vline(xintercept = 86,col="black")+
  annotate(geom="text",x=100,y=-30,
           label="Mar,27",col="black",size=6)+
  ylab("CH-Dav SWP(KPa)")+
  xlab("DOY (2020-2023)")+
  # annotate(geom = "text",x=20,y=4,label="CH-Dav",size=6)+
  theme_light()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size=18),
        legend.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20)
  )

#C:Tharandt multi-year mean
df.Tha_plot<-df.SWP_Tha %>%
  pivot_longer(c(SWP_20cm:SWP_90cm),names_to = "Depth",
               values_to = "SWP")%>%
  mutate(SWP=ifelse(SWP< -100,NA,SWP))%>%
  mutate(Depth=factor(Depth,
                       levels=c("SWP_20cm","SWP_30cm","SWP_50cm",
                                "SWP_70cm","SWP_90cm")))
#
df.Tha_plot_agg<-df.Tha_plot %>%
  mutate(DoY=yday(Date))%>%
  group_by(DoY,Depth)%>%
  summarise(SWP=mean(SWP,na.rm = T),
            SWP_sd=sd(SWP,na.rm=T))
#
Tha_cols<-viridis::viridis(5,option = "D",direction = -1)
p3<-df.Tha_plot_agg %>%
  ggplot(aes(x=DoY,y=SWP,col=Depth))+
  geom_point()+
  geom_smooth(span=0.3)+
  #adding at Mar, 2025: SWP-->fiedl capacity
  geom_hline(yintercept = -33,lty=2,size=1.1)+
  scale_color_manual(values = c(
    "SWP_20cm"=adjustcolor(Tha_cols[1],0.8),
    "SWP_30cm"=adjustcolor(Tha_cols[2],0.8),
    "SWP_50cm"=adjustcolor(Tha_cols[3],0.8),
    "SWP_70cm"=adjustcolor(Tha_cols[4],0.8),
    "SWP_90cm"=adjustcolor(Tha_cols[5],0.8)))+
  # addding date: "2023-03-27"
  # geom_vline(xintercept = 86,col="black")+
  # annotate(geom="text",x=110,y=-50,
  #          label="Mar,27",col="black",size=6)+
  ylab("DE-Tha SWP(KPa)")+
  xlab("DOY (1997-2014)")+
  # annotate(geom = "text",x=20,y=4,label="CH-Tha",size=6)+
  theme_light()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size=18),
        legend.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20)
  )

#-------------------------
#further plotting: adding lines and rectanges in plot
#-------------------------
#period:[sos-60,sos+60] is set to red
#adding the sos
#for CH-Dav
Dav_pheno<-data.frame("sos10"=61,"sos25"=65,"peak"=180,
                      "eos25"=296,"eos10"=322)
rect.coord_isevent=data.frame(x1=Dav_pheno[,1]-60,
                              x2=Dav_pheno[,1],
                              x3=Dav_pheno[,1]+60,
                              y1=-Inf, 
                              y2=Inf)
##. for DSPR period for CH-Dav:
p_2_new<-p2+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[,1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[,1])+10,y=-0.5,label="sos",col="forestgreen",size=6)

##. for DSPR period for DE-Tha:
p_3_new<-p3+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[,1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[,1])+10,y=-0.5,label="sos",col="forestgreen",size=6)

#---------
#Merge the plots
#---------
p_merge<-plot_grid(p1,p2,p3,
          aligh="hv",ncol = 1,nrow=3,
          labels = c("(a)","(b)","(c)")
          )
p_merge_new<-plot_grid(p_2_new,p_3_new,
                   aligh="hv",ncol = 1,nrow=2,
                   labels = c("(a)","(b)")
)

#
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig4_supp3_check_SWP.png"),
       p_merge,width = 15,height=12)
ggsave(paste0(save.path,"Fig4_supp3_check_SWP_new.png"),
       p_merge_new,width = 15,height=12)

