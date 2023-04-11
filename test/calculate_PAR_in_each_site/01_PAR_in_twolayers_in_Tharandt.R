###############################################
#To obtained the PAR values from the Tharandt site (different layers)
###############################################
library(readxl)
library(ggplot2)
library(dplyr)
library(plyr)
library(LakeMetabolizer) ##convert between PAR and SW
library(lubridate)
library(tidyverse)
#--------------------------------
#Part 1: The data from tidied EC data sent by Thomas at Oct, 2022
#-------------------------------
#only with one layer of PAR(top layer)
load.path<-"./data-raw/EC_data_from_PIs/DE_Tha/"
df<-readxl::read_xlsx(paste0(load.path,"DE-Tha_2008-2021.xlsx"),"DE-Tha")
#change to new variable names:
df_new<-df[,c("date","Rg (W/m²)")]
names(df_new)<-c("date","Rg")
#add PAR values-->unit:umol/m2/s(the relationship is according to sw.to.par function)
df_new<-df_new %>%
  mutate(PAR=Rg*2.114)
df_temp<-df_new %>%
  group_by(week=week(date)) %>%
  mutate(PAR.wk.average = mean(PAR)) %>%
  ungroup() %>%
  group_by(month = month(date)) %>%
  mutate(Price.mo.average = mean(PAR))

#test:
ggplot(df_new,aes(x=date,y=PAR))+
  geom_point()

#--------------------------------
#Part 2: The data from tidied Rg and PAR data sent by Thomas at Mar, 2023
#-------------------------------
#---------------
#half-hourly PAR from different layers:(37,26,23, and 17m-->the data in 37m is drifted calibrated)
#---------------
update.path<-"./data-raw/Data_from_ICOS_sites/DE_Tha/"
df.PAR<-read.csv2(paste0(update.path,"DE-Tha_PAR.csv"),sep = ";",header = T)
var_names<-df.PAR[1,] #names for variabels
df.PAR<-df.PAR[-1,];names(df.PAR)<-c("DateTime","PPFD_37m","PPFD_26m","PPFD_23m","PPFD_17m")
df.PAR$DateTime<-dmy_hm(df.PAR$DateTime); 
#convert to numeric
df.PAR[,2:ncol(df.PAR)]<-apply(df.PAR[,2:ncol(df.PAR)],2,as.numeric)
df.PAR_HH<-df.PAR

#---------------
#10mins Rg from different layers:(37 and 2m) 
#---------------
#add "check.names = F" to read the column names as they are otherwise we can load the data from this csv
df.Rg<-read.csv(paste0(update.path,"DE-Tha_Rg_2015_2023.csv"),sep = ";",header = T,check.names = F)
#only keep the incoming Rg data in df.Rg
df.Rg<-df.Rg[,1:3]
names(df.Rg)<-c("rDate","Rg_2m","Rg_37m")
df.Rg$rDate<-dmy_hm(df.Rg$rDate)
#average the 10mins to half-hourly data:
df.Rg_HH<-df.Rg %>%
  ##summarize the data to half-hourly (e.g. summarize the data from hh:09, hh:19,hh:29 to hh:09)
  group_by(DateTime=cut(rDate,breaks="30 min"))%>% 
  dplyr::summarize(Rg_2m=mean(Rg_2m,na.rm=T),
                   Rg_37m=mean(Rg_37m,na.rm=T))
#add the Datetime 21 mins(21*60 s) to make the calculated data to the right time:
df.Rg_HH$DateTime<-as.POSIXct(df.Rg_HH$DateTime,format="%Y-%m-%d %H:%M")+21*60

#----------------
#(3)using the mid-day data(10-14) and average the data to daily
#further average mutli-years doy
#----------------
#A.For PAR
df.PAR_midday<-df.PAR_HH %>%
  mutate(hour=hour(DateTime))%>%
  filter(hour>=10 & hour<=14)
df.PAR_midday_daily<-df.PAR_midday %>%
  mutate(Date=date(DateTime))%>%
  group_by(Date)%>%
  dplyr::summarise(across(PPFD_37m:PPFD_17m,~ mean(.x, na.rm = TRUE)))
df.PAR_multiY<-df.PAR_midday_daily %>%
  mutate(doy=yday(Date))%>%
  group_by(doy)%>%
  dplyr::summarise(across(PPFD_37m:PPFD_17m,~ mean(.x, na.rm = TRUE)))

#B. For Rg
df.Rg_midday<-df.Rg_HH %>%
  mutate(hour=hour(DateTime))%>%
  filter(hour>=10 & hour<=14)
df.Rg_midday_daily<-df.Rg_midday %>%
  mutate(Date=date(DateTime))%>%
  group_by(Date)%>%
  dplyr::summarise(across(Rg_2m:Rg_37m,~ mean(.x, na.rm = TRUE)))%>%
  #estimated PAR in 37m and 2m
  mutate(PAR_est_2m=Rg_2m*2.114,
         PAR_est_37m=Rg_37m*2.114)
df.Rg_multiY<-df.Rg_midday_daily %>%
  mutate(doy=yday(Date))%>%
  group_by(doy)%>%
  dplyr::summarise(across(Rg_2m:PAR_est_37m,~ mean(.x, na.rm = TRUE)))

#----------------
#(4)plotting check the PAR data and Rg(and calculated PAR from Rg)
#----------------
library(ggplot2)
#-------
#A.For PAR varation
#-------
#we can clearly see the sensor drfit for PAR observed from 17, 23, 26
df.PAR_midday_daily%>%
  pivot_longer(c(PPFD_37m:PPFD_17m), names_to = "Height", values_to = "PPFD") %>%
  ggplot(aes(x=Date,y=PPFD,col=Height))+
  geom_point()+
  theme_light()

#only PARin 37m is solid, the data quality of PAR in 26 and 23 m is relatively OK, but bad for 17m
#calculate a factor between PARin 26m(approximate the height of upper canopy we measured in the field)
f_convert<-mean(df.PAR_multiY$PPFD_37m,na.rm = T)/mean(df.PAR_multiY$PPFD_26m,na.rm = T)
df.PAR_multiY%>%
  pivot_longer(c(PPFD_37m:PPFD_17m), names_to = "Height", values_to = "PPFD") %>%
  ggplot(aes(x=doy,y=PPFD,col=Height))+
  geom_point()+
  theme_light()

#-------
#B.For variation of Rg/estimated PAR
#-------
#multi-year variation:
df.Rg_midday_daily%>%
  pivot_longer(c(PAR_est_2m:PAR_est_37m), names_to = "Height", values_to = "PPFD") %>%
  ggplot(aes(x=Date,y=PPFD,col=Height))+
  geom_point()+
  theme_light()

#multi-year variation(averaged with doy):
df.Rg_multiY%>%
  pivot_longer(c(PAR_est_2m:PAR_est_37m), names_to = "Height", values_to = "PPFD") %>%
  ggplot(aes(x=doy,y=PPFD,col=Height))+
  geom_point()+
  theme_light()

#----------------
#(5)report the PPFD of 2m and 37m as the reference value to LI-cor measurement in the field
#----------------
#average the daily to weekly data:
# doy_weekly=seq(min(df.Rg_multiY$doy),max(df.Rg_multiY$doy),7)
df.Rg_sum<-df.Rg_multiY%>%
  group_by(grp = as.integer(gl(n(),7, n()))) %>% 
  dplyr::summarise(
    doy=median(doy),
    PAR_est_2m=mean(PAR_est_2m,na.rm=T),
    PAR_est_26m_approx=mean(PAR_est_37m/f_convert,na.rm=T), #approx the PAR value at 26m using the data in 37m
    )%>%
  mutate(Date_2023=as.Date("2023-01-01")+doy)
#save the data:
write.csv(df.Rg_sum,file = paste0("./test/calculate_PAR_in_each_site/","estimated_PAR_weekly.csv"))
