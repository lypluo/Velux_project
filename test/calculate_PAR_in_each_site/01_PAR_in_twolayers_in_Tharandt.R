###############################################
#To obtained the PAR values from the Tharandt site (different layers)
#top layer:
#low layer:
###############################################
library(readxl)
library(ggplot2)
library(dplyr)
library(plyr)
library(LakeMetabolizer) ##convert between PAR and SW
library(lubridate)
#--------------------------------
#1)load the data
#-------------------------------
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
