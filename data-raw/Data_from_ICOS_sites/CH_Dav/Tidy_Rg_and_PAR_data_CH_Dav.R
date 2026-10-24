##########################################
#Tidy the radiation and PAR data in CH-Dav
#data shared by Iris
##########################################
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)

#-------------------
#(1) load the data and test
#-------------------
#
load.path<-"D:/data/Velux_shared_data/CH-Dav/radiation_data/"

#A. first load the Rg and PAR--from Iris, to check their realtionship (2021-2023):
df.Rg_PAR_20212023<-read.csv2(paste0(load.path,"CH-Dav_SW_data_2021-2023.csv"),
                             header = F,sep=",")
var_names<-c("rDate","PAR_35m","PAR_2m", "Rg_35m","Rg_2m")
unit_varnames<-c("","u mol m-2 s-1","u mol m-2 s-1","W m-2","W m-2")
df.Rg_PAR_20212023<-df.Rg_PAR_20212023[-1,]
names(df.Rg_PAR_20212023)<-var_names
#
df.Rg_PAR_20212023<-df.Rg_PAR_20212023 %>%
  mutate(rDate=dmy_hm(rDate),
         across(PAR_35m:Rg_2m, as.numeric)
  )

#B. load the Rg and PAR from 2.1m sent from Luana (2021-2024):
df.Rg_PAR_2m_20212024<-read.csv2(paste0(load.path,"CH-DAS_SW_PPFD_21-24.csv"),
                              header = F,sep=",")
var_names<-c("rDate","Rg_IN_2m","Rg_OUT_2m", "PAR_2m")
df.Rg_PAR_2m_20212024<-df.Rg_PAR_2m_20212024[-1,]
names(df.Rg_PAR_2m_20212024)<-var_names
#tempoarily do not use SW_OUT_2.1
df.Rg_PAR_2m_20212024<-df.Rg_PAR_2m_20212024 %>%
  mutate(Rg_OUT_2m=NULL)
df.Rg_PAR_2m_20212024<-df.Rg_PAR_2m_20212024 %>%
  mutate(rDate=dmy_hm(rDate),
         Rg_2m=Rg_IN_2m,Rg_IN_2m=NULL)


#C.load the Rg and PAR at 35m from the ICOS website:
load.path_new<-"D:/data/ICOS/ICOS_2023_Velux_sites/tidy_data_for_sites/CH-Dav/2019-2023/Meteo/"
df.Rg_PAR_35m_20192024<-read.csv2(paste0(load.path_new,"ICOSETC_CH-Dav_METEO_INTERIM_L2.csv"),
                                 header = F,sep=",")
var_names<-df.Rg_PAR_35m_20192024[1,]
names(df.Rg_PAR_35m_20192024)<-var_names
df.Rg_PAR_35m_20192024<-df.Rg_PAR_35m_20192024[-1,]
#only select the rDate, SW_IN
df.Rg_PAR_35m_20192024<-df.Rg_PAR_35m_20192024 %>%
  dplyr::select(TIMESTAMP_START,SW_IN,PPFD_IN)%>%
  mutate(rDate=ymd_hm(TIMESTAMP_START),TIMESTAMP_START=NULL,
         Rg_35m=SW_IN,PAR_35m=PPFD_IN,
         SW_IN=NULL,PPFD_IN=NULL)
df.Rg_PAR_35m_20192024$Rg_35m[df.Rg_PAR_35m_20192024$Rg_35m=="-9999"]<-NA
df.Rg_PAR_35m_20192024$PAR_35m[df.Rg_PAR_35m_20192024$PAR_35m=="-9999"]<-NA

##-----
#merge the Rg and PAR data in CH-Dav
##-----
df.Rg_PAR_20212024<-left_join(df.Rg_PAR_35m_20192024,df.Rg_PAR_2m_20212024)%>%
  filter(rDate>=as.POSIXct("2021-01-01"))

#
df.Rg_PAR_20212024_proc<-df.Rg_PAR_20212024%>%
  mutate(agg_time=floor_date(rDate,"30 minutes"))%>%
  mutate(rDate=NULL,
         across(Rg_35m:Rg_2m, as.numeric))%>%
  group_by(agg_time)%>%
  summarise(across(everything(),~mean(.x,na.rm = T)))
#
df.Rg_PAR_20212024_proc_daily<-df.Rg_PAR_20212024_proc %>%
  mutate(Date=floor_date(agg_time,"day"))%>%
  mutate(agg_time=NULL)%>%
  group_by(Date)%>%
  summarise(across(everything(),~mean(.x,na.rm = T)))
#
df.Rg_PAR_20212024_proc_daily[df.Rg_PAR_20212024_proc_daily<0]<-NA

#check the relationship between Rg and PAR (at 35m):
#Rg and PAR have very strong relationship:
# df.Rg_PAR_20212024_proc %>%
df.Rg_PAR_20212024_proc_daily %>%
  ggplot(aes(x=Rg_35m,y=PAR_35m))+
  geom_point()+
  geom_smooth(method = "lm",col="red")+
  geom_abline(slope = 1,intercept = 0,lty=2,col="blue")

lm_PAR_Rg_HH<-lm(PAR_35m ~ Rg_35m, 
                 data = df.Rg_PAR_20212024_proc)
lm_stats_HH<-summary(lm_PAR_Rg_HH)
#
lm_PAR_Rg_Daily<-lm(PAR_35m ~ Rg_35m, 
                    data = df.Rg_PAR_20212024_proc_daily)
lm_stats_Daily<-summary(lm_PAR_Rg_Daily)

#----
#obtain the PAR at 2m 
#by adopting the relationship between PAR and Rg at 37m
#----
#do not need to do this for CH-Dav
# df.Rg_PAR_20212023_proc<-df.Rg_PAR_20212023_proc %>%
#   mutate(PAR_2m = ifelse(Rg_2m==0,0,Rg_2m*round(coef(lm_stats_HH)[2,1],2)+
#            round(coef(lm_stats_HH)[1,1],2)))
  
#-------------------
#(2) demonstration plot
#-------------------
df.PAR_daily<-df.Rg_PAR_20212024_proc_daily %>%
  dplyr::select(Date,PAR_35m,PAR_2m)
df.plot<-df.PAR_daily %>%
  pivot_longer(c(PAR_35m:PAR_2m),names_to = "Height",
               values_to = "PAR")%>%
  mutate(PAR=ifelse(PAR<0,NA,PAR))%>%
  mutate(Height=factor(Height,
        levels=c("PAR_35m","PAR_2m")))
#
df.plot %>%
  ggplot(aes(x=Date,y=PAR,col=Height))+
  geom_point()
#
df.plot_agg<-df.plot %>%
  mutate(DoY=yday(Date))%>%
  group_by(DoY,Height)%>%
  summarise(PAR=mean(PAR,na.rm = T),
            PAR_sd=sd(PAR,na.rm=T))
df.plot_agg %>%
  ggplot(aes(x=DoY,y=PAR,col=Height))+
  geom_point()+
  geom_smooth()

#-------------------
#(4) save the data
#-------------------
df.PAR<-df.PAR_daily
save.path<-"./data/EC_MeteoandFlux/PAR/"
saveRDS(df.PAR,paste0(save.path,"PAR_CH-Dav.RDS"))
