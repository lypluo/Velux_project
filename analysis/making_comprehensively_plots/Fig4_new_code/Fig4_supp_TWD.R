#############################
#check TWD in Davos:
##############################
library(dendRoAnalyst)  ## 
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

##1) load the data:
load.path<-"./data/Tree_growth/"
df.final_Dav<-readRDS(paste0(load.path,"Davos_daily_TWD_updated.RDS"))

#simply check the data:
unique(df.final_Dav$series_id)
# df.final_Dav%>%
#   group_by(series_id)%>%
#   ggplot(aes(x=Date,y=TWD))+
#   geom_point()+
#   facet_grid(.~series_id)
##delete the extreme data according to the visualization:
hist(df.final_Dav$TWD)
df.final_Dav<-df.final_Dav %>%
  mutate(year=year(Date),doy=yday(Date))%>%
  mutate(TWD=ifelse(TWD>=3000,NA,TWD))
#
df.final_Dav %>%
  filter(series_id==221)%>%
  ggplot(aes(x=Date,y=TWD))+
  geom_point()

##2) average TWD:
#check the data:
df.final_Dav%>%
  filter(year==2023)%>%
  group_by(series_id)%>%
  ggplot(aes(x=Date,y=TWD))+
  geom_point()+
  facet_grid(.~series_id)
df.final_Dav<-df.final_Dav %>%
  mutate(TWD=ifelse(TWD>=2000,NA,TWD))

##all years mean:
df.TWD_eachtree_mean<-df.final_Dav %>%
  group_by(tree_id,doy)%>%
  summarise(TWD_mean=mean(TWD,na.rm = T),
            TWD_sd=sd(TWD,na.rm=T))
df.TWD_eachtree_mean2023<-df.final_Dav %>%
  filter(year==2023)%>%
  group_by(tree_id,doy)%>%
  summarise(TWD_mean=mean(TWD,na.rm = T),
            TWD_sd=sd(TWD,na.rm=T))
#
df.TWD_Alltree_mean<-df.final_Dav %>%
  group_by(doy)%>%
  summarise(TWD_mean=mean(TWD,na.rm = T),
            TWD_sd=sd(TWD,na.rm=T))
df.TWD_Alltree_mean2023<-df.final_Dav %>%
  filter(year==2023)%>%
  group_by(doy)%>%
  summarise(TWD_mean=mean(TWD,na.rm = T),
            TWD_sd=sd(TWD,na.rm=T))
##plotting
p_TWD_eachtrees_AllYears<-ggplot()+
  stat_smooth(aes(x=doy,y=TWD_mean,
                  col=factor(tree_id)),span=0.15,
              data=df.TWD_eachtree_mean,
              se=FALSE)+
  scale_color_viridis_d()+
  stat_smooth(aes(x=doy,y=TWD_mean),
              data=df.TWD_Alltree_mean,span = 0.2,
              col=adjustcolor("black",0.8),size=2,se=TRUE)+
  # addding date: "2023-03-27"
  geom_vline(xintercept = 86,col="black")+
  annotate(geom="text",x=110,y=0,
           label="Mar,27",col="black",size=6)+
  xlim(0,250)+
  # ylim(-0.5,5)+
  annotate(geom = "text",x=25,y=800,label="CH-Dav",size=6)+
  # annotate(geom = "segment",x=180,xend=190,y = 2,yend = 2,col="red",size=1.5)+
  annotate(geom = "segment",x=180,xend=190,y = 0,yend = 0,col="black",size=1.5)+
  annotate(geom = "text",x=220,y=0,label=c("Mean"),size=5)+
  ylab(expression("TWD ("*mu*"m)"))+
  xlab("DOY (Mulit-Years)")+
  labs(color ="Trees")+
  theme_classic()+
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size = 16),
        legend.position ="none"
  )
p_TWD_eachtrees_2023<-ggplot()+
  stat_smooth(aes(x=doy,y=TWD_mean,
                  col=factor(tree_id)),span=0.15,
              data=df.TWD_eachtree_mean2023,
              se=FALSE)+
  scale_color_viridis_d()+
  stat_smooth(aes(x=doy,y=TWD_mean),
              data=df.TWD_Alltree_mean2023,span = 0.2,
              col=adjustcolor("black",0.8),size=2,se=TRUE)+
  # addding date: "2023-03-27"
  geom_vline(xintercept = 86,col="black")+
  annotate(geom="text",x=110,y=0,
           label="Mar, 27",col="black",size=6)+
  xlim(0,250)+
  ylim(0,1500)+
  annotate(geom = "text",x=25,y=1500,label="CH-Dav",size=6)+
  # annotate(geom = "segment",x=180,xend=190,y = 2,yend = 2,col="red",size=1.5)+
  annotate(geom = "segment",x=180,xend=190,y = 0,yend = 0,col="black",size=1.5)+
  annotate(geom = "text",x=220,y=0,label=c("Mean"),size=5)+
  ylab(expression("TWD ("*mu*"m)"))+
  xlab("DOY (2023)")+
  labs(color ="Trees")+
  theme_classic()+
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size = 16),
        legend.position ="none"
  )
#
plot_grid(p_TWD_eachtrees_AllYears,p_TWD_eachtrees_2023,ncol = 1)
##save the plot:
save.path<-"./data/Comprehensive_plot_data/Fig4/"
save(p_TWD_eachtrees_2023,file=paste0(save.path,"p_TWD_Trees2023.RDA"))
