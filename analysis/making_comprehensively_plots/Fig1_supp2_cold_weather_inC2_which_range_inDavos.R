##--------------------------------------------------------
#Aim: confirming if the temperature in C2 is cold among historial period
##--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(dplyr)

#-------------
#(1)load the data-using daily data
#-------------
##load the fluxes and Meteo:
load.path<-"./data/EC_MeteoandFlux/"
load(paste0(load.path,"df_daily_from_ICOS.RDA"))

df.Dav<-df_DD$Dav
#For this study, select the first layer TS and several layers of SWC:
#check the data gaps%
# visdat::vis_miss(df.Dav, warn_large_data = FALSE)
# visdat::vis_miss(df.Tha, warn_large_data = FALSE)
#

#-------------------
#(2) tidy the data 
#-------------------
#---merge the data according to doy
df<-df.Dav %>%
  mutate(DoY=yday(Date),
         Year=year(Date)
  )
#calculate the mean annual temperature and precipitation
df.spring<-df %>%
  #select spring period:
  filter(DoY>=60 | DoY<=151)

df.spring_mean<-df %>%
  group_by(Year)%>%
  filter(DoY>=60 | DoY<=151)%>%
  summarise(TA_mean=mean(TA,na.rm = T),
            P_sum=sum(P,na.rm = T))%>%
  mutate(Year=factor(Year))
df.spring_mean_MultiY<-df.spring_mean %>%
  summarise(TA_mean=mean(TA_mean))
#6 campaigns in Davos
# Date %in% as.Date(c("2023-03-08","2023-03-27","2023-04-21",
#                            "2023-05-03","2023-05-22","2023-07-17"))

C2_campaign_Date<-as.Date("2023-03-27")
df.spring_C2<-df.spring%>%
  filter(Date==C2_campaign_Date)

#---------------------------
#3) making the plots
#---------------------------
##--------------
#fast checking:
##--------------
#Davos for example
df %>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=TA))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

##-----------
#making the plots:
##-----------

# 设置随机种子以便复现
quantile(df.spring$TA,probs = c(0.1,0.9),na.rm = T)
set.seed(123)
p_C2_distribution<-df.spring %>%
  ggplot(aes(x=1,y=TA))+
  geom_violin(fill="grey90",color="grey90")+
  geom_boxplot(width=0.1, color="black",outlier.shape = TRUE)+
  stat_summary(fun = quantile, fun.args = list(probs = 0.10), 
               geom = "errorbar", width = 0.1, aes(ymin = ..y.., ymax = ..y..), 
               color = "black", size = 1) +  # 添加 10 分位数
  stat_summary(fun = quantile, fun.args = list(probs = 0.90), 
               geom = "errorbar", width = 0.1, aes(ymin = ..y.., ymax = ..y..), 
               color = "black", size = 1)+
  #adding labels for percentile 10 and 90
  annotate(geom = "text",x=1+0.2,
           y=c(-5.1,13.6),
           label=c("Perc10","Perc90"),size=3,
           col="black")+ 
  #only for 2023 mean:
  geom_point(aes(x=1-0.05,y=TA_mean,col=Year),
             data = df.spring_mean%>%filter(Year==2023),
             col=adjustcolor("#FDE725",0.6),size=4.5)+
  #add mean spring Ta for all years
  geom_point(aes(x=1,y=TA_mean),
             data=df.spring_mean_MultiY,col=adjustcolor("black",0.7),size=7)+
  #add mean spring Ta from different years
  geom_jitter(aes(x=1,y=TA_mean,col=Year),data = df.spring_mean)+
  scale_color_viridis_d(option = "D")+
  #adding labels
  annotate(geom = "text",x=1-0.02,
           y=df.spring_mean[df.spring_mean$Year==2023,]$TA_mean,
           label="2023",size=4,
           col="#FDE725")+ #color is for 2023
  xlim(0.4, 1.6)+
  #adding the C2 temperature
  geom_point(aes(x=1,y=TA),
             data=df.spring_C2,col=adjustcolor("red",0.8),size=4.5)+
  #adding labels
  annotate(geom = "text",x=1+0.1,
           y=df.spring_C2$TA,
           label="C2 (2023-03-27)",size=4,
           col="red")+ #color is for 2023
  theme_light()+
  ylab(expression("Ta ("* "°C)"))+
  xlab("")+
  theme(legend.text = element_text(size = 14),
        legend.title=element_text(size=22),
        legend.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20),
        #no ticks and label in x axis
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
        )

###save the ggplot plots:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig2_supp2_C2_weather_distribution.png"),
       p_C2_distribution,width = 15,height=10)

