######################################################
#Calculate the minimum Ta in Davos and Tharandt:
######################################################
library(dplyr)
library(lubridate)
###
load.path<-"D:/EE_WSL/Data_for_use/Data_from_ICOS_sites/processed_data_from_ICOS/"
load(file=paste0(load.path,"Daily_data.RDA"))

#------------
#selecting the site names
#------------
df.sel<-df_all_sel_daily %>%
  filter(sitename %in% c("CH-Dav","DE-Tha"))

#-----------------------
#calculating the mean T in Winter:
#-----------------------
df.sel %>%
  mutate(Month=month(Date))%>%
  filter(Month>=12 | Month<=2)%>%
  group_by(sitename)%>%
  summarise(Ta_winter=mean(Ta_mean))
