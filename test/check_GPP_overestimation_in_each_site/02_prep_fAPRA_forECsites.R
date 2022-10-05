#---------------------------
#prepare the fPAR data for EC sites
#---------------------------
library(tidyverse)
library(dplyr)

#--1. download the fPAR for each EC site
#download the fPAR for sites: NL-Loo; FI-Hyy; DE-Tha, and CH-Dav
#download link:https://modis.ornl.gov/globalsubset/

#--2. start to tidy the fAPAR data:
sitenames<-c("FI-Hyy","NL-Loo","DE-Tha","CH-Dav")
base.path<-"./data-raw/download_from_MODIS/fPAR/"

#
df_fPAR<-c()
for (i in 1:length(sitenames)) {
  df.temp<-read.csv(paste0(base.path,sitenames[i],"/statistics_Fpar_500m.csv"))
  fPAR.temp<-df.temp %>%
    select(date,mean) %>%
    mutate(sitename=sitenames[i])
  names(fPAR.temp)<-c("Date","fPAR","sitename")
  #
  df_fPAR<-rbind(df_fPAR,fPAR.temp)
}

#--3. test plot
#test:
library(ggplot2)
df_fPAR%>%
  # filter(sitename=="FI-Hyy")%>%
  ggplot(aes(x=as.Date(Date),y=fPAR,col=sitename))+
  # geom_line()
  geom_point()

#--4. interpolating the fAPAR
library(zoo)
df_fPAR<-df_fPAR %>%
  group_by(sitename)%>%
  mutate(fPAR_itpl=na.fill(fPAR,c(NA,"extend",NA)))
##save the data:
save(df_fPAR,file = paste0(base.path,"fPAR_itpl.RDA"))

