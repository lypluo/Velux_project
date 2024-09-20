#------------------------------------------------------
#Aim: tidy the meteorological data from two sites: DAV and THA
#------------------------------------------------------
#original data download from the ICOS website:
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)

base.path<-"D:/EE_WSL/IMPACT_project/data_collection/MeteoFluxes_fromDav_andTha/"
###################
#1) For THA site
###################
##A. load the existing daily data(Meteo and Fluxes data):
#as the data from Tharandt processed to recently(Nov, 20)
#-->hence using the processed daily data(L2 data:including fluxes and Meteo)
df.Tha<-read.csv(paste0(base.path,"Meteo_and_flux_composite_download/ICOSETC_DE-Tha_ARCHIVE_INTERIM_L2/",
            "ICOSETC_DE-Tha_FLUXNET_DD_INTERIM_L2.csv"))
df.Tha.daily<-df.Tha %>%
  #P_F values in 2023 are -9999
  dplyr::select(TIMESTAMP,TA_F,SW_IN_F,VPD_F,P_F,PPFD_IN,
         PPFD_OUT,PPFD_DIF,TS_F_MDS_1:TS_F_MDS_5,
         SWC_F_MDS_1:SWC_F_MDS_4,
         NEE_VUT_REF,RECO_NT_VUT_REF,GPP_NT_VUT_REF)%>%
  mutate(Date=ymd(TIMESTAMP))
df.Tha.daily[df.Tha.daily==-9999]<-NA

##B.load the snow depth data-->unit: cm
# df.Tha.more<-read.csv(paste0(base.path,"Meteo_download/ICOSETC_DE-Tha_METEO_INTERIM_L2/",
#                              "ICOSETC_DE-Tha_METEO_INTERIM_L2.csv"))
# df.Tha.snow_and_P<-df.Tha.more %>%
#   select(TIMESTAMP_START,TIMESTAMP_END,D_SNOW,P)%>%
#   mutate(TIMESTAMP_START=ymd_hm(TIMESTAMP_START),
#          TIMESTAMP_END=ymd_hm(TIMESTAMP_END),
#          Date=as.Date(TIMESTAMP_END))
# #
# df.Tha.snow_and_P[df.Tha.snow_and_P==-9999]<-NA
# #if SNOW_depth<0, set SNOW_depth=NA
# df.Tha.snow_and_P$D_SNOW[df.Tha.snow_and_P$D_SNOW<0]=NA
# 
# df.Tha.snow_and_P.daily<-df.Tha.snow_and_P %>%
#   group_by(Date)%>%
#   dplyr::summarise(D_SNOW=max(D_SNOW),
#                    #keep the precipitation consistent with the names in processed Fluxnet dataset
#                    P_F=sum(P))
#############the snow depth is updated-->updated the results on Mar,2024
df.Tha.more<-read.csv(paste0(base.path,"Meteo_download/ICOSETC_DE-Tha_METEO_L2/",
                             "ICOSETC_DE-Tha_METEO_L2.csv"))
df.Tha.snow_and_P<-df.Tha.more %>%
  dplyr::select(TIMESTAMP_START,TIMESTAMP_END,D_SNOW,P)%>%
  mutate(TIMESTAMP_START=ymd_hm(TIMESTAMP_START),
         TIMESTAMP_END=ymd_hm(TIMESTAMP_END),
         Date=as.Date(TIMESTAMP_END))
#
df.Tha.snow_and_P[df.Tha.snow_and_P==-9999]<-NA
#if SNOW_depth<0, set SNOW_depth=NA
df.Tha.snow_and_P$D_SNOW[df.Tha.snow_and_P$D_SNOW<0]=NA

df.Tha.snow_and_P.daily<-df.Tha.snow_and_P %>%
  ##addtional filter:set snow depth data to NA if the D_SNOW>0 in summer(June-Sep)
  mutate(Month=month(Date),
         D_SNOW=ifelse(Month>=6 & Month<=9,NA,D_SNOW))%>%
  group_by(Date)%>%
  dplyr::summarise(D_SNOW=max(D_SNOW),
  #keep the precipitation consistent with the names in processed Fluxnet dataset
   P_F=sum(P))

##C.merge the data
df.Tha.daily<-left_join(df.Tha.daily,df.Tha.snow_and_P.daily[,c("Date","D_SNOW")])
#for Precipitation after 2023-->using P_F in df.Tha.snow_and_P
sub_df<-df.Tha.snow_and_P.daily %>%
  filter(Date>=as.Date("2023-01-01") & Date<=as.Date("2023-12-31"))
P_pos<-match(df.Tha.snow_and_P.daily$Date[df.Tha.snow_and_P.daily$Date>=as.Date("2023-01-01")],df.Tha.daily$Date)
P_pos<-P_pos[!is.na(P_pos)]
#subsitute the precipitation:
df.Tha.daily$P_F[P_pos]<-sub_df$P_F

###################
#2) For DAV site
###################
#as Davos does not have updated processed Meteo data-->processing the data from
#relatively raw data(L1 data)

##A.load the meterological data
df.Dav<-read.csv(paste0(base.path,"Meteo_download/ICOSETC_CH-Dav_METEO_NRT/",
                        "ICOSETC_CH-Dav_METEO_NRT.csv"))
#select variables:
df.Dav<-df.Dav %>%
  dplyr::select(TIMESTAMP_START,TIMESTAMP_END,SW_IN,P,PPFD_IN,PPFD_OUT,PPFD_DIF,
         SWC_1:SWC_7,TA:TA_6,TS_1:TS_8,VPD:VPD_6)%>%
  mutate(Date=ymd_hm(TIMESTAMP_END))%>%
  dplyr::select(!ends_with("_SD")&!starts_with("WD_")&!ends_with("_N")&!starts_with("WS_"))
#plotting variables:
plot_fun<-function(df,var_name){
  # df<-df.Dav
  # var_name<-"SWC"
  
  #
  df_sel<-df %>%
    select(Date,starts_with(var_name))
  df_sel[df_sel==-9999]<-NA
  #
  df_plot<-df_sel %>%
    pivot_longer(starts_with(var_name), names_to = "depth", values_to = "y") %>% 
    ggplot(aes(x=Date,y=y,col=depth))+
    geom_line()+
    ylab(var_name)
  return(df_plot)
}
# plot_fun(df.Dav,"TA")
# plot_fun(df.Dav,"P")
# plot_fun(df.Dav,"VPD")
# plot_fun(df.Dav,"SWC")
# plot_fun(df.Dav,"TS")

#finalize selecting variables:
df.Dav.daily<-df.Dav %>%
  #only selected the TS and SWC in the first layer
  dplyr::select(Date,SW_IN,P,PPFD_IN:PPFD_DIF,TA,VPD,starts_with("SWC"),starts_with("TS"))
df.Dav.daily[df.Dav.daily==-9999]<-NA
#summarize (mean) for the data:
df.Dav.daily<-df.Dav.daily%>%
  mutate(Date=as.Date(Date))%>%
  group_by(Date)%>%
  dplyr::summarise(SW_IN=mean(SW_IN,na.rm=T),PPFD_IN=mean(PPFD_IN,na.rm=T),
            P=sum(P),
            PPFD_OUT=mean(PPFD_OUT,na.rm=T),PPFD_DIF=mean(PPFD_DIF,na.rm=T),
            TA=mean(TA,na.rm=T),VPD=mean(VPD,na.rm=T),across(starts_with("SWC"),mean),
            across(starts_with("TS"),mean)
            )

##B.load the snow depth data-->unit: cm
# df.Dav.more<-read.csv(paste0(base.path,"Meteo_download/ICOSETC_CH-Dav_METEOSENS_NRT/",
#       "ICOSETC_CH-Dav_METEOSENS_NRT.csv"))
# #!!!original snow depth seems have some probelm-->set D_SNOW=D_SNOW*100(unit:cm)
# df.Dav.snow<-df.Dav.more %>%
#   select(TIMESTAMP_START,TIMESTAMP_END,D_SNOW_1_1_1)%>%
#   mutate(TIMESTAMP_START=ymd_hm(TIMESTAMP_START),
#          TIMESTAMP_END=ymd_hm(TIMESTAMP_END),
#          Date=as.Date(TIMESTAMP_END),
#          D_SNOW=D_SNOW_1_1_1*100,D_SNOW_1_1_1=NULL
#          )
# #
# df.Dav.snow[df.Dav.snow==-9999]<-NA
# #if SNOW_depth<0, set SNOW_depth=NA
# df.Dav.snow$D_SNOW[df.Dav.snow$D_SNOW<0]=NA
# 
# df.Dav.snow.daily<-df.Dav.snow %>%
#   group_by(Date)%>%
#   dplyr::summarise(D_SNOW=max(D_SNOW))

#############the snow depth is updated-->updated the results on Mar,2024
df.Dav.more<-read.csv(paste0(base.path,"Meteo_download/ICOSETC_CH-Dav_METEO_L2/",
                             "ICOSETC_CH-Dav_METEO_L2.csv"))
df.Dav.snow<-df.Dav.more %>%
  dplyr::select(TIMESTAMP_START,TIMESTAMP_END,D_SNOW)%>%
  mutate(TIMESTAMP_START=ymd_hm(TIMESTAMP_START),
         TIMESTAMP_END=ymd_hm(TIMESTAMP_END),
         Date=as.Date(TIMESTAMP_END))
#
df.Dav.snow[df.Dav.snow==-9999]<-NA
#if SNOW_depth<0, set SNOW_depth=NA
df.Dav.snow$D_SNOW[df.Dav.snow$D_SNOW<0]=NA

df.Dav.snow.daily<-df.Dav.snow %>%
  mutate(Month=month(TIMESTAMP_START))%>%
  ##addtional filter:set snow depth data to NA if the D_SNOW>0 in summer(June-Sep)
  mutate(D_SNOW=ifelse(Month>=6 & Month<=9,NA,D_SNOW))%>%
  group_by(Date)%>%
  dplyr::summarise(D_SNOW=max(D_SNOW))

##C.merge the data
df.Dav.daily<-left_join(df.Dav.daily,df.Dav.snow.daily)

###################
#3) save the data
###################
df.Meteo.daily<-list("Tha"=df.Tha.daily,
                     "Dav"=df.Dav.daily)
#save the data
save.path<-"./data/EC_MeteoandFlux/"
save(df.Meteo.daily,file = paste0(save.path,"df.Meteo.daily.RDA"))
