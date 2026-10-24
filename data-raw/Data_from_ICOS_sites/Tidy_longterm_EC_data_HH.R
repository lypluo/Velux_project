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
#I. For half-hourly data
##################

##--------------------------------
#1) load data and merge the data for each site
#selecting the related variables 
##--------------------------------
##A.For Davos:
df.Dav.HH_19972018<-read.csv(file=paste0(base.path,"CH-Dav/1997-2018/",Davos_files1[2]),header = T)
df.Dav.HH_20192023<-read.csv(file=paste0(base.path,"CH-Dav/2019-2023/",Davos_files2[2]),header = T)

##for the analysis: only keep the all the HH data
df.Dav.HH_19972018<-df.Dav.HH_19972018%>%
  mutate(Hour=hour(ymd_hm(TIMESTAMP_START)))
df.Dav.HH_20192023<-df.Dav.HH_20192023%>%
  mutate(Hour=hour(ymd_hm(TIMESTAMP_START)))

#
df.Dav.HH_19972018_sel<-df.Dav.HH_19972018 %>%
  mutate(Date=date(ymd_hm(TIMESTAMP_START)))%>%
  dplyr::select(TIMESTAMP_START,Date,Hour,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
                LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F,LW_IN_F,LW_OUT,RH)
names(df.Dav.HH_19972018_sel)<-c("Date_Time","Date","Hour","NEE","RECO","GPP","LE",
                                    "SW_IN","PPFD_IN","TA","VPD","WS","P","Pressure","LW_IN","LW_OUT","RH")

df.Dav.HH_20192023_sel<-df.Dav.HH_20192023 %>%
  mutate(Date=date(ymd_hm(TIMESTAMP_START)))%>%
  dplyr::select(TIMESTAMP_START,Date,Hour,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
                LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F,LW_IN_F,LW_OUT,RH)
names(df.Dav.HH_20192023_sel)<-c("Date_Time","Date","Hour","NEE","RECO","GPP","LE",
                                    "SW_IN","PPFD_IN","TA","VPD","WS","P","Pressure","LW_IN","LW_OUT","RH")
df.Dav.HH_sel<-rbind(df.Dav.HH_19972018_sel,df.Dav.HH_20192023_sel)
#
df.Dav.HH_sel[df.Dav.HH_sel==-9999]<-NA

##B.For Tharandt:
df.Tha.HH_19962018<-read.csv(file=paste0(base.path,"DE-Tha/1996-2018/",Tharandt_files1[2]),header = T)
df.Tha.HH_20202023<-read.csv(file=paste0(base.path,"DE-Tha/2020-2023/",Tharandt_files2[2]),header = T)
#now do not have half-hourly data for Tharandt for 2019!
# df.Tha.daily_fromPI<-readxl::read_xlsx(paste0(base.path,"DE-Tha/","DE-Tha_2008-2021_data_fromPIs.xlsx"))

##for the analysis: keep all the HH data:
df.Tha.HH_19962018<-df.Tha.HH_19962018%>%
  mutate(Hour=hour(ymd_hm(TIMESTAMP_START)))
df.Tha.HH_20202023<-df.Tha.HH_20202023%>%
  mutate(Hour=hour(ymd_hm(TIMESTAMP_START)))

#
df.Tha.HH_19962018_sel<-df.Tha.HH_19962018 %>%
  mutate(Date=date(ymd_hm(TIMESTAMP_START)))%>%
  dplyr::select(TIMESTAMP_START,Date,Hour,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
      LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F,LW_IN_F,LW_OUT,RH)
names(df.Tha.HH_19962018_sel)<-c("Date_Time","Date","Hour","NEE","RECO","GPP","LE",
                                 "SW_IN","PPFD_IN","TA","VPD","WS","P","Pressure","LW_IN","LW_OUT","RH")


df.Tha.HH_20202023_sel<-df.Tha.HH_20202023 %>%
  mutate(Date=date(ymd_hm(TIMESTAMP_START)))%>%
  dplyr::select(TIMESTAMP_START,Date,Hour,NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF,
      LE_F_MDS,SW_IN_F_MDS,PPFD_IN,TA_F_MDS,VPD_F_MDS,WS_F,P_F,PA_F,LW_IN_F,LW_OUT,RH)
names(df.Tha.HH_20202023_sel)<-c("Date_Time","Date","Hour","NEE","RECO","GPP","LE",
                                 "SW_IN","PPFD_IN","TA","VPD","WS","P","Pressure","LW_IN","LW_OUT","RH")

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
df_HH<-list("Dav_HH"=df.Dav.HH_sel,
            "Tha_HH"=df.Tha.HH_sel)
save.path<-"./data/EC_MeteoandFlux/"
save(df_HH,file = paste0(save.path,"df_HH_from_ICOS.RDA"))

