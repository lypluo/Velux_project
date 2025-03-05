########################################################
#check the sap flow data in Davos:
#especially for the data in 2023
########################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

#----------------------
#(1)load the data:
#----------------------
#------
#sap flow-->need to load the original data:
#------
#unit:cm/hour
#load the data from the Ankit-->as the data covers to 2023:
#load the original data--15 trees:
load.path<-"./data-raw/Data_from_PIs/CH_Dav/sap_flow/"
df.SFD_trees<-read.csv(file=paste0(load.path,"Dav_SFD_2010_2023_Daily_15trees.csv"))

#load the data for the stand scale(2021-2023):
#unit:mm d-1
load.path<-"./data/Sapflow/"
load(paste0(load.path,"df.Davos.sap_daily_fromAnkit.RDA"))
df.SF_stand<-df.sap.daily;rm(df.sap.daily)

#----------------------
#test:
#----------------------
#
df.SFD_trees<-df.SFD_trees %>%
  mutate(Tree_ID=TreeNumber,TreeNumber=NULL,
         Date=as.Date(Date))

#plotting test
df.SFD_trees %>%
  filter(Date>=as.Date("2021-01-01"))%>%
  group_by(Tree_ID)%>%
  ggplot(aes(x=Date,y=SFDm))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Tree_ID)
#to make the analsis more reliable
#-->only use the sap from trees after 2021:
df.SFD_trees_20212023<-df.SFD_trees%>%
  filter(Date>=as.Date("2021-01-01"))

#----------------------
#(2)plotting:
#----------------------
df.SF_stand<-df.SF_stand %>%
  mutate(doy=yday(Date),Year=year(Date))%>%
  mutate(Year=as.factor(Year))
df.SF_stand_mean<-df.SF_stand %>%
  group_by(doy)%>%
  summarise(sap_m_adj_mean=mean(sap_m_adj),
            sap_m_adj_sd=sd(sap_m_adj))
#
df.SFD_eachtree<-df.SFD_trees_20212023%>%
  mutate(doy=yday(Date),Year=year(Date))%>%
  #SFD unit: cm h-1
  mutate(SFD=SFDm,SFDm=NULL)
df.SFD_eachtree %>%
  group_by(Tree_ID)%>%
  ggplot(aes(x=Date,y=SFD))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Tree_ID)
#only targe for 2023
df.SFD_eachtree_mean2023<-df.SFD_eachtree %>%
  filter(Year==2023)%>%
  group_by(doy)%>%
  summarise(SFD_mean=mean(SFD,na.rm = T),
            SFD_sd=sd(SFD,na.rm=T))
#save the data:
df.SFD<-list("df.stand"=df.SF_stand,
             "df.eachtree"=df.SFD_eachtree)
saveRDS(df.SFD,file = paste0("./data/Comprehensive_plot_data/Fig4/df.SFD.RDS"))

#----------------------
#for the stand sapflow:
#-----------------------
p_stand<-df.SF_stand %>%
  ggplot(aes(x=doy,sap_m_adj,col=Year))+
  geom_point()+
  geom_point(aes(x=doy,y=sap_m_adj),
      col=adjustcolor("#440154",0.3),size=5,
      data = df.SF_stand %>% filter(Date==as.Date("2023-03-27")))+
  geom_smooth(span=0.2,se=FALSE)+
  scale_color_viridis_d(option = "D",direction = -1)+
  # addding date: "2023-03-27"
  geom_vline(xintercept = 86,col="#440154")+
  annotate(geom="text",x=110,y=0,
           label="Mar,27",col="#440154",size=6)+
  ylab(expression("SF"[Stand]*" (mm d"^-1*")"))+
  xlim(0,250)+
  ylim(-0.5,4)+
  xlab("DOY")+
  annotate(geom = "text",x=20,y=4,label="CH-Dav",size=6)+
  theme_light()+
  theme(legend.text = element_text(size = 14),
        legend.title=element_blank(),
        legend.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20),
        legend.position = c(0.1,0.85)
  )

#----------------------
#for different trees:
#-----------------------
p_eachtrees<-ggplot()+
  # geom_point(aes(x=doy,y=SFD),col=adjustcolor("grey50",1),
  #            data=df.SFD_eachtree%>%filter(Year==2023))+
  # geom_ribbon(aes(x=doy,ymin=SFD_mean-SFD_sd,ymax=SFD_mean+SFD_sd),
  #             data=df.SFD_eachtree_mean2023,
  #             fill="gray",size=1.5)+
  stat_smooth(aes(x=doy,y=SFD,
                  col=factor(Tree_ID)),span=0.2,
              data=df.SFD_eachtree%>%filter(Year==2023),
              se=FALSE)+
  scale_color_viridis_d()+
  stat_smooth(aes(x=doy,y=SFD_mean),
              data=df.SFD_eachtree_mean2023,span = 0.2,
              col=adjustcolor("black",0.8),size=2,se=TRUE)+
  # addding date: "2023-03-27"
  geom_vline(xintercept = 86,col="black")+
  annotate(geom="text",x=110,y=0,
           label="Mar,27",col="black",size=6)+
  xlim(0,250)+
  ylim(-0.5,5)+
  annotate(geom = "text",x=25,y=5,label="CH-Dav 2023",size=6)+
  # annotate(geom = "segment",x=180,xend=190,y = 2,yend = 2,col="red",size=1.5)+
  annotate(geom = "segment",x=180,xend=190,y = 0,yend = 0,col="black",size=1.5)+
  annotate(geom = "text",x=220,y=0,label=c("Mean"),size=5)+
  ylab(expression("SFD (cm h"^-1*")"))+
  xlab("DOY")+
  labs(color ="Trees")+
  theme_light()+
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size = 16)
        # legend.position =c(0.6,0.5)
  )

#---------
#Merge the plots
#---------
p_merge<-plot_grid(p_stand,p_eachtrees,
          aligh="hv",ncol = 2,nrow=1,
          labels = c("(a)","(b)")
          )
#
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig4_supp2_check_sapflow_resumption.png"),
       p_merge,width = 15,height=6)

