##########################################
#Tidy the radiation and PAR data in DE-Tha
#data shared by Thomas
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
load.path<-"D:/data/Velux_shared_data/DE-Tha/Data_from_EC_systems/"

#A. first load the Rg and PAR to check their realtionship (2015-2022):
df.Rg_PAR_20152022<-read.csv2(paste0(load.path,"DE-Tha_Rg_2015-2023.csv"),
                             header = F,sep=";")
var_names<-c("rDate","Rg_2m","Rg_37m",
             "PAR_17m","PAR_23m","PAR_26m","PAR_37m")
unit_varnames<-c("","W m-2","W m-2",
            "u mol m-2 s-1","u mol m-2 s-1","u mol m-2 s-1","u mol m-2 s-1")
df.Rg_PAR_20152022<-df.Rg_PAR_20152022[-1,]
names(df.Rg_PAR_20152022)<-var_names
#
df.Rg_PAR_20152022<-df.Rg_PAR_20152022 %>%
  mutate(rDate=dmy_hm(rDate),
         across(Rg_2m:PAR_37m, as.numeric)
         )
#
df.Rg_PAR_20152022_proc<-df.Rg_PAR_20152022%>%
  mutate(agg_time=floor_date(rDate,"30 minutes"))%>%
  mutate(rDate=NULL)%>%
  group_by(agg_time)%>%
  summarise(across(everything(),~mean(.x,na.rm = T)))
#
df.Rg_PAR_20152022_proc_daily<-df.Rg_PAR_20152022_proc %>%
  mutate(Date=floor_date(agg_time,"day"))%>%
  mutate(agg_time=NULL)%>%
  group_by(Date)%>%
  summarise(across(everything(),~mean(.x,na.rm = T)))

#check the relationship between Rg and PAR (at 37m):
#Rg and PAR have very strong relationship:
# df.Rg_PAR_20152022_proc %>%
df.Rg_PAR_20152022_proc_daily %>%
  ggplot(aes(x=Rg_37m,y=PAR_37m))+
  geom_point()+
  geom_smooth(method = "lm",col="red")+
  geom_abline(slope = 1,intercept = 0,lty=2,col="blue")

lm_PAR_Rg_HH<-lm(PAR_37m ~ Rg_37m, 
                 data = df.Rg_PAR_20152022_proc)
lm_stats_HH<-summary(lm_PAR_Rg_HH)
#
lm_PAR_Rg_Daily<-lm(PAR_37m ~ Rg_37m, 
                    data = df.Rg_PAR_20152022_proc_daily)
lm_stats_Daily<-summary(lm_PAR_Rg_Daily)

#----
#obtain the PAR at 2m 
#by adopting the relationship between PAR and Rg at 37m
#----
df.Rg_PAR_20152022_proc<-df.Rg_PAR_20152022_proc %>%
  mutate(PAR_2m = ifelse(Rg_2m==0,0,Rg_2m*round(coef(lm_stats_HH)[2,1],2)+
           round(coef(lm_stats_HH)[1,1],2)))
  
#-------------------
#(2) start to tidy the data:
#-------------------

#A.load the PAR data in 1997-2022:
df.PAR_19972022<-read.csv2(paste0(load.path,
                                  "DE-Tha_PAR_1997-2022.csv"),
                              header = F,sep=";")
var_names<-c("rDate",
             "PAR_37m","PAR_26m","PAR_23m","PAR_17m")
unit_varnames<-c("",
                 "u mol m-2 s-1","u mol m-2 s-1","u mol m-2 s-1","u mol m-2 s-1")
df.PAR_19972022<-df.PAR_19972022[-c(1:2),]
names(df.PAR_19972022)<-var_names

#half-hourly data
df.PAR_19972022<-df.PAR_19972022 %>%
  mutate(rDate=dmy_hm(rDate),
         across(PAR_37m:PAR_17m, as.numeric)
  )

#B.load the data(Rg+PAR) in 2023:
df.Rg_PAR_2023<-read_xlsx(paste0(load.path,
                                 "DE-Tha_Rg2023_2.xlsx"))
df.Rg_PAR_2023<-df.Rg_PAR_2023[-1,]
var_names<-c("rDate","Rg_2m","Rg_37m",
             "PAR_17m","PAR_37m","PAR_OUT_37m")
unit_varnames<-c("","W m-2","W m-2",
                 "u mol m-2 s-1","u mol m-2 s-1","u mol m-2 s-1")
names(df.Rg_PAR_2023)<-var_names

df.Rg_PAR_2023_proc<-df.Rg_PAR_2023%>%
  mutate(across(Rg_2m:PAR_OUT_37m, as.numeric))%>%
  mutate(rDate=floor_date(rDate,"30 minutes"))%>%
  group_by(rDate)%>%
  summarise(across(everything(),~mean(.x,na.rm = T)))%>%
  #remove the PAR_OUT
  mutate(PAR_OUT_37m=NULL)

#----
#obtain the PAR at 2m 
#by adopting the relationship between PAR and Rg at 37m
#----
df.Rg_PAR_2023_proc <- df.Rg_PAR_2023_proc %>%
  mutate(PAR_2m=ifelse(Rg_2m==0,0,Rg_2m*round(coef(lm_stats_HH)[2,1],2)+
           round(coef(lm_stats_HH)[1,1],2)))

#C. merge the data together 1997-2023:
df.PAR_sel1<-df.PAR_19972022
df.PAR_sel2<-df.Rg_PAR_20152022_proc%>%
  select(agg_time,PAR_2m)%>%
  mutate(rDate=agg_time,agg_time=NULL)
df.PAR_sel3<-df.Rg_PAR_2023_proc %>%
  select(rDate,PAR_17m:PAR_2m)
#merging:
df.PAR_merge1<-left_join(df.PAR_sel1,df.PAR_sel2)
df.PAR_merge_final<-bind_rows(df.PAR_merge1,df.PAR_sel3)

#----
#HH data
#----
df.PAR_HH<-df.PAR_merge_final

#----
#Daily data
#----
df.PAR_daily<-df.PAR_HH %>%
  mutate(Date=floor_date(rDate,"day"))%>%
  group_by(Date)%>%
  summarise(across(everything(),~mean(.x,na.rm = T)))

#-------------------
#(3) demonstration plot
#-------------------
df.plot<-df.PAR_daily %>%
  pivot_longer(c(PAR_37m:PAR_2m),names_to = "Height",
               values_to = "PAR")%>%
  mutate(PAR=ifelse(PAR<0,NA,PAR))%>%
  mutate(Height=factor(Height,
        levels=c("PAR_37m","PAR_26m","PAR_23m","PAR_17m","PAR_2m")))
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
saveRDS(df.PAR,paste0(save.path,"PAR_DE-Tha.RDS"))
