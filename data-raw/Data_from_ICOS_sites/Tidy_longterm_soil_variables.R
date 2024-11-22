#########################################################
##Aim:Tidy the long-term data from EC system from CH-Dav and DE-Tha
#########################################################
#specifically, tidy the Soil T and SWC 
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)

base.path<-"D:/data/Velux_shared_data/ICOS_EC_system_data/Re_tidy_excels/"
#For Davos:
Davos_files1<-list.files(paste0(base.path,"CH-Dav/1997-2020/"))
Davos_files2<-list.files(paste0(base.path,"CH-Dav/2020-2024/"))
#For Tharandt
Tharandt_files1<-list.files(paste0(base.path,"DE-Tha/1996-2020/"))
Tharandt_files2<-list.files(paste0(base.path,"DE-Tha/2020-2023/"))

##################
#I. For Daily data
##################

##--------------------------------
#1) load data and merge the data for each site
#selecting the related variables 
##--------------------------------
##A.For Davos:
df.Dav.daily_19972020<-read.csv(file=paste0(base.path,"CH-Dav/1997-2020/",Davos_files1[1]),header = T)
df.Dav.daily_20202024<-read.csv(file=paste0(base.path,"CH-Dav/2020-2024/",Davos_files2[1]),header = T)

#focus on the soil variables
df.Dav.daily_19972020_sel<-df.Dav.daily_19972020 %>%
  dplyr::select(TIMESTAMP,TS_F_MDS_1:SWC_F_MDS_5_QC)
names(df.Dav.daily_19972020_sel)<-c("Date",
  paste0("TS_",1:6),paste0("TS_",1:6,"_QC"),
  paste0("SWC_",1:5),paste0("SWC_",1:5,"_QC")
  )

#more soil depth
df.Dav.daily_20202024_sel<-df.Dav.daily_20202024 %>%
  dplyr::select(TIMESTAMP,TS_F_MDS_1:SWC_F_MDS_7_QC)
names(df.Dav.daily_20202024_sel)<-c("Date",
  paste0("TS_",1:8),paste0("TS_",1:8,"_QC"),
  paste0("SWC_",1:7),paste0("SWC_",1:7,"_QC")
)
#only keep the first 6 layer TS and first 5 layer SWC as the data before 2020
df.Dav.daily_20202024_sel<-df.Dav.daily_20202024_sel %>%
  dplyr::select(Date,TS_1:TS_6,TS_1_QC:TS_6_QC,
                SWC_1:SWC_5,SWC_1_QC:SWC_5_QC)

#merge the data:
df.Dav.daily_sel<-rbind(df.Dav.daily_19972020_sel%>%
   mutate(Date=ymd(Date),Year=year(Date)),
   #select the year after 2020
   df.Dav.daily_20202024_sel%>%
   mutate(Date=ymd(Date),Year=year(Date))%>%
   filter(Year>2020))
#
df.Dav.daily_sel[df.Dav.daily_sel==-9999]<-NA

##B.For Tharandt:
df.Tha.daily_19962020<-read.csv(file=paste0(base.path,"DE-Tha/1996-2020/",Tharandt_files1[1]),header = T)
df.Tha.daily_20202023<-read.csv(file=paste0(base.path,"DE-Tha/2020-2023/",Tharandt_files2[1]),header = T)

#focus on the soil variables
df.Tha.daily_19962020_sel<-df.Tha.daily_19962020 %>%
  dplyr::select(TIMESTAMP,TS_F_MDS_1:SWC_F_MDS_4_QC)
names(df.Tha.daily_19962020_sel)<-c("Date",
      paste0("TS_",1:6),paste0("TS_",1:6,"_QC"),
      paste0("SWC_",1:4),paste0("SWC_",1:4,"_QC")
)
#only keep the first 5 layer TS and first 4 layer SWC as the data after 2020
df.Tha.daily_19962020_sel<-df.Tha.daily_19962020_sel %>%
  dplyr::select(Date,TS_1:TS_5,TS_1_QC:TS_5_QC,
                SWC_1:SWC_4,SWC_1_QC:SWC_4_QC)

#more soil depth
df.Tha.daily_20202023_sel<-df.Tha.daily_20202023 %>%
  dplyr::select(TIMESTAMP,TS_F_MDS_1:SWC_F_MDS_4_QC)
names(df.Tha.daily_20202023_sel)<-c("Date",
     paste0("TS_",1:5),paste0("TS_",1:5,"_QC"),
     paste0("SWC_",1:4),paste0("SWC_",1:4,"_QC")
)
#merge the data:
df.Tha.daily_sel<-rbind(df.Tha.daily_19962020_sel%>%
     mutate(Date=ymd(Date),Year=year(Date)),
     #select the year after 2020
     df.Tha.daily_20202023_sel%>%
     mutate(Date=ymd(Date),Year=year(Date))%>%
     filter(Year>2020))
#
df.Tha.daily_sel[df.Tha.daily_sel==-9999]<-NA

##--------------------------------
#2)save the data:
##--------------------------------
df_DD<-list("Dav"=df.Dav.daily_sel,
            "Tha"=df.Tha.daily_sel)
save.path<-"./data/EC_MeteoandFlux/"
save(df_DD,file = paste0(save.path,"df_soilvars_daily_from_ICOS.RDA"))


