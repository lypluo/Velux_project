#########################################################
##Aim:Tidy the long-term EC data from CH-Dav and DE-Tha
#########################################################
#data from the ICOS website:https://www.icos-cp.eu/data-products/YVR0-4898
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)

base.path<-"D:/data/ICOS/ICOS_2023_Velux_sites/tidy_data_for_sites/"
#For Davos:
Davos_files1<-list.files(paste0(base.path,"CH-Dav/1997-2018/"))
Davos_files2<-list.files(paste0(base.path,"CH-Dav/2019-2023/"))
#For Tharandt
Tharandt_files1<-list.files(paste0(base.path,"DE-Tha/1996-2018/"))
Tharandt_files2<-list.files(paste0(base.path,"DE-Tha/2020-2023/"))


##################
#I. For Daily data
##################

##--------------------------------
#1) load data and merge the data for each site
#selecting the related variables 
##--------------------------------
##A.For Davos:
df.Dav.daily_19972018<-read.csv(file=paste0(base.path,"CH-Dav/1997-2018/",Davos_files1[1]),header = T)
df.Dav.daily_20192023<-read.csv(file=paste0(base.path,"CH-Dav/2019-2023/",Davos_files2[1]),header = T)

#
df.Dav.daily_19972018_sel<-df.Dav.daily_19972018 %>%
  dplyr::select(TIMESTAMP,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
         LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F)
names(df.Dav.daily_19972018_sel)<-c("Date","NEE","RECO","GPP","LE",
      "SW_IN","PPFD_IN","TA","VPD","WS","P","Pressure")

df.Dav.daily_20192023_sel<-df.Dav.daily_20192023 %>%
  dplyr::select(TIMESTAMP,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
                LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F)
names(df.Dav.daily_20192023_sel)<-c("Date","NEE","RECO","GPP","LE",
                                    "SW_IN","PPFD_IN","TA","VPD","WS","P","Pressure")
df.Dav.daily_sel<-rbind(df.Dav.daily_19972018_sel,df.Dav.daily_20192023_sel)
df.Dav.daily_sel<-df.Dav.daily_sel%>%
  mutate(Date=ymd(Date))
#
df.Dav.daily_sel[df.Dav.daily_sel==-9999]<-NA

##B.For Tharandt:
df.Tha.daily_19962018<-read.csv(file=paste0(base.path,"DE-Tha/1996-2018/",Tharandt_files1[1]),header = T)
df.Tha.daily_20202023<-read.csv(file=paste0(base.path,"DE-Tha/2020-2023/",Tharandt_files2[1]),header = T)
#also load the data from PI(Thomas) to add the data for 2019:
df.Tha.daily_fromPI<-readxl::read_xlsx(paste0(base.path,"DE-Tha/","DE-Tha_2008-2021_data_fromPIs.xlsx"))

#
df.Tha.daily_19962018_sel<-df.Tha.daily_19962018 %>%
  dplyr::select(TIMESTAMP,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
                LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F)
names(df.Tha.daily_19962018_sel)<-c("Date","NEE","RECO","GPP","LE",
                                    "SW_IN","PPFD_IN","TA","VPD","WS","P","Pressure")
df.Tha.daily_19962018_sel<-df.Tha.daily_19962018_sel%>%
  mutate(Date=ymd(Date))


df.Tha.daily_20202023_sel<-df.Tha.daily_20202023 %>%
  dplyr::select(TIMESTAMP,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
                LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F)
names(df.Tha.daily_20202023_sel)<-c("Date","NEE","RECO","GPP","LE",
                                    "SW_IN","PPFD_IN","TA","VPD","WS","P","Pressure")
df.Tha.daily_20202023_sel<-df.Tha.daily_20202023_sel%>%
  mutate(Date=ymd(Date))


#for the year of 2019-->take the data from the PI's data:
df.Tha.daily_2019_sel<-df.Tha.daily_fromPI %>%
  mutate(Year=year(date))%>%
  filter(Year==2019)%>%
  select(date,`NEP (gC/m²)`,`TER (gC/m²)`,`GPP (gC/m²)`,`L.E (W/m²)`,
         `Rg (W/m²)`,`Tair (°C)`,`VPD (hPa)`,`WS (m/s)`,`P (mm)`)
names(df.Tha.daily_2019_sel)<-c("Date","NEE","RECO","GPP","LE",
                              "SW_IN","TA","VPD","WS","P")
df.Tha.daily_2019_sel<-df.Tha.daily_2019_sel %>%
  mutate(NEE= -NEE,Date=as.Date(Date))

df.Tha.daily_sel<-bind_rows(df.Tha.daily_19962018_sel,df.Tha.daily_2019_sel)
df.Tha.daily_sel<-bind_rows(df.Tha.daily_sel,df.Tha.daily_20202023_sel)
#
df.Tha.daily_sel[df.Tha.daily_sel==-9999]<-NA

##--------------------------------
#2)save the data:
##--------------------------------
df_DD<-list("Dav"=df.Dav.daily_sel,
            "Tha"=df.Tha.daily_sel)
save.path<-"./data/EC_MeteoandFlux/"
save(df_DD,file = paste0(save.path,"df_daily_from_ICOS.RDA"))


##################
#II. For half-hourly data-->using midday data to get the maximum values
##################

##--------------------------------
#1) load data and merge the data for each site
#selecting the related variables 
##--------------------------------
##A.For Davos:
df.Dav.HH_19972018<-read.csv(file=paste0(base.path,"CH-Dav/1997-2018/",Davos_files1[2]),header = T)
df.Dav.HH_20192023<-read.csv(file=paste0(base.path,"CH-Dav/2019-2023/",Davos_files2[2]),header = T)

#one additional check: when VPDmax happens
df_Dav_test<-df.Dav.HH_19972018 %>%
  mutate(Hour=hour(ymd_hm(TIMESTAMP_START)))
plot(df_Dav_test$Hour,df_Dav_test$VPD_F) #normally happens at 14-15

##for the analysis: only keep the data in the midday period(10-14):
df.Dav.HH_19972018<-df.Dav.HH_19972018%>%
  mutate(Hour=hour(ymd_hm(TIMESTAMP_START)))%>%
  filter(Hour>=10 & Hour<=14)
df.Dav.HH_20192023<-df.Dav.HH_20192023%>%
  mutate(Hour=hour(ymd_hm(TIMESTAMP_START)))%>%
  filter(Hour>=10 & Hour<=14)

#
df.Dav.HH_19972018_sel<-df.Dav.HH_19972018 %>%
  mutate(Date=date(ymd_hm(TIMESTAMP_START)))%>%
  dplyr::select(TIMESTAMP_START,Date,Hour,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
                LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F)
names(df.Dav.HH_19972018_sel)<-c("Date_Time","Date","Hour","NEE","RECO","GPP","LE",
                                    "SW_IN","PPFD_IN","TA","VPD","WS","P","Pressure")

df.Dav.HH_20192023_sel<-df.Dav.HH_20192023 %>%
  mutate(Date=date(ymd_hm(TIMESTAMP_START)))%>%
  dplyr::select(TIMESTAMP_START,Date,Hour,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
                LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F)
names(df.Dav.HH_20192023_sel)<-c("Date_Time","Date","Hour","NEE","RECO","GPP","LE",
                                    "SW_IN","PPFD_IN","TA","VPD","WS","P","Pressure")
df.Dav.HH_sel<-rbind(df.Dav.HH_19972018_sel,df.Dav.HH_20192023_sel)
#
df.Dav.HH_sel[df.Dav.HH_sel==-9999]<-NA

##B.For Tharandt:
df.Tha.HH_19962018<-read.csv(file=paste0(base.path,"DE-Tha/1996-2018/",Tharandt_files1[2]),header = T)
df.Tha.HH_20202023<-read.csv(file=paste0(base.path,"DE-Tha/2020-2023/",Tharandt_files2[2]),header = T)
#now do not have half-hourly data for Tharandt for 2019!
# df.Tha.daily_fromPI<-readxl::read_xlsx(paste0(base.path,"DE-Tha/","DE-Tha_2008-2021_data_fromPIs.xlsx"))

##for the analysis: only keep the data in the midday period(10-14):
df.Tha.HH_19962018<-df.Tha.HH_19962018%>%
  mutate(Hour=hour(ymd_hm(TIMESTAMP_START)))%>%
  filter(Hour>=10 & Hour<=14)
df.Tha.HH_20202023<-df.Tha.HH_20202023%>%
  mutate(Hour=hour(ymd_hm(TIMESTAMP_START)))%>%
  filter(Hour>=10 & Hour<=14)

#
df.Tha.HH_19962018_sel<-df.Tha.HH_19962018 %>%
  mutate(Date=date(ymd_hm(TIMESTAMP_START)))%>%
  dplyr::select(TIMESTAMP_START,Date,Hour,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
                LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F)
names(df.Tha.HH_19962018_sel)<-c("Date_Time","Date","Hour","NEE","RECO","GPP","LE",
                                 "SW_IN","PPFD_IN","TA","VPD","WS","P","Pressure")


df.Tha.HH_20202023_sel<-df.Tha.HH_20202023 %>%
  mutate(Date=date(ymd_hm(TIMESTAMP_START)))%>%
  dplyr::select(TIMESTAMP_START,Date,Hour,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
                LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F)
names(df.Tha.HH_20202023_sel)<-c("Date_Time","Date","Hour","NEE","RECO","GPP","LE",
                                 "SW_IN","PPFD_IN","TA","VPD","WS","P","Prssure")

#for the year of 2019-->take the data from the PI's data:
# df.Tha.HH_2019_sel<-df.Tha.HH_fromPI %>%
#   mutate(Year=year(date))%>%
#   filter(Year==2019)%>%
#   select(date,`NEP (gC/m²)`,`TER (gC/m²)`,`GPP (gC/m²)`,`L.E (W/m²)`,
#          `Rg (W/m²)`,`Tair (°C)`,`VPD (hPa)`,`WS (m/s)`,`P (mm)`)
# names(df.Tha.HH_2019_sel)<-c("Date","NEE","RECO","GPP","LE",
#                                 "SW_IN","TA","VPD","WS","P")
# df.Tha.HH_2019_sel<-df.Tha.HH_2019_sel %>%
#   mutate(NEE= -NEE,Date=as.Date(Date))

df.Tha.HH_sel<-bind_rows(df.Tha.HH_19962018_sel,df.Tha.HH_20202023_sel)
#
df.Tha.HH_sel[df.Tha.HH_sel==-9999]<-NA

##--------------------------------
#2)save the data:
##--------------------------------
df_midday<-list("Dav_midday"=df.Dav.HH_sel,
            "Tha_midday"=df.Tha.HH_sel)
save.path<-"./data/EC_MeteoandFlux/"
save(df_midday,file = paste0(save.path,"df_midday_from_ICOS.RDA"))

