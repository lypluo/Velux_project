###############################################
#To obtained the PAR values from the Davos site (different layers)
#code originally prepared by Jan and updated by Yunpeng
###############################################
library(readxl)
library(ggplot2)
library(dplyr)
library(plyr)
library(LakeMetabolizer) ##convert between PAR and SW
library(lubridate)
library(tidyverse)
library(magrittr)
library(phenopix)
library(zoo)
#--------------------------------
#Part 1: The data(2021-early spring, 2023) from tidied EC data sent by Iris at April, 2023
#-------------------------------
load.path<-"./data-raw/EC_data_from_PIs/CH_Dav/"
df<- read.csv(paste0(load.path,"CH-DAV_SW_data_2021-2023.csv"))
#change to new variable names:
df_new<-df
names(df_new)<-c("date","PAR_35", "PAR_2.1", "SW_35", "SW_2.1")
#add SW converted PAR values-->unit:umol/m2/s(the relationship is according to sw.to.par function)
df_new<-df_new %>%
  mutate(PAR_35_from_SW=SW_35*2.114,
         PAR_2.1_from_SW=SW_2.1*2.114)

df_temp<-df_new %>%
  mutate(date = dmy_hm(date),
         week = week(date)) 
###
df.PAR_midday<-df_temp %>%
  mutate(hour=hour(date))%>%
  filter(hour>=10 & hour<=14)
df.PAR_midday_daily<-df.PAR_midday %>%
  mutate(Date=date(date))%>%
  group_by(Date)%>%
  dplyr::summarise(across(PAR_35:PAR_2.1_from_SW,~ mean(.x, na.rm = TRUE)))

df.PAR_multiY<-df.PAR_midday_daily %>%
  mutate(doy=yday(Date))%>%
  group_by(doy)%>%
  dplyr::summarise(across(PAR_35:PAR_2.1_from_SW,~ mean(.x, na.rm = TRUE)))

##calculate the weekly PAR values
doy_weekly=seq(min(df.PAR_multiY$doy),max(df.PAR_multiY$doy),7)
#adopt the same convert factor from Tharandt(37 to 26m) to obtain the PAR in Davos(35 m -->17m height)
f_covert<-2.259463
df.PAR_sum<-df.PAR_multiY%>%
  group_by(grp = as.integer(gl(n(),7, n()))) %>% 
  dplyr::summarise(
    doy=median(doy),
    PAR_est_2.1=round(mean(PAR_2.1,na.rm=T),0),
    PAR_est_35=round(mean(PAR_35,na.rm=T),0),
    PAR_est_2.1_from_SW=round(mean(PAR_2.1_from_SW,na.rm=T),0),
    PAR_est_35_from_SW=round(mean(PAR_35_from_SW,na.rm=T),0),
    )%>%
  mutate(PAR_est_17=round(PAR_est_35/f_covert,0))%>%
  #convert the doy to 2023 date-->to inform the PAR values
  mutate(Date_2023=as.Date("2023-01-01")+doy)

##----remove the outliers in PAR_est_17 and refill the gap---
ts_ori<-df.PAR_sum$PAR_est_17
plot(ts_ori)
#moving window mean
ts_sm<-rollmean(ts_ori,5,align = "left")
points(ts_sm,col="red")
## if the values > 2 sd remove the data:
outlier_rm_and_gapfill<-function(ts_ori){
  # ts_ori<-df.PAR_sum$PAR_est_17
  
  #smoothed ts:
  ts_sm<-rollmean(ts_ori,5,align = "left")
  #
  ts_sd<-sd(ts_ori)
  #remove the outliers:
  ts_upper<-ts_sm+ts_sd*0.5
  ts_below<-ts_sm-ts_sd*0.5
  # points(ts_upper,col="blue")
  # points(ts_below,col="blue")
  for (i in 1:length(ts_sm)) {
    ifelse(ts_ori[i]>ts_upper[i] | ts_ori[i]<ts_below[i],ts_ori[i]<-NA,
           ts_ori[i]<-ts_ori[i])
  }
  #gapfilling the data:
  ts_new<-na.fill(ts_ori,"extend")
  return(ts_new)
}
#
ts_new<-outlier_rm_and_gapfill(ts_ori = ts_ori)
points(ts_new,col="orange")
#update the data for PAR_est_17:
df.PAR_sum$PAR_est_17<-ts_new

#--plotting---
#demonstrated plot
g_plot<-df.PAR_sum%>%
  pivot_longer(c(PAR_est_2.1:PAR_est_17), names_to = "Height", values_to = "PPFD") %>%
  ggplot(aes(x=doy,y=PPFD,col=Height))+
  geom_point()+
  theme_light()

#save the data and demonstrated plot:
write.csv(df.PAR_sum,file = paste0("./test/calculate_PAR_in_each_site/","estimated_Davos_PAR_weekly.csv"))
ggsave(g_plot,filename = paste0("./test/calculate_PAR_in_each_site/","estimated_Davos_weekly_PAR.png"))

