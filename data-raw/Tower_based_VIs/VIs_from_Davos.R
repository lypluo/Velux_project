##################################################
#Aim: tidy the data from flox box shared from Michael and Alex
##################################################

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(dplyr)
library(tidyverse)
#----------------------
#(1)load the data  
#---------------------
load.path<-"D:/EE_WSL/IMPACT_project/data_collection/Dav_Data/Other_data_from_collabrators/tower_based_VIs/flox_dav_2021-2023/"
nc_data <- nc_open(paste0(load.path,"Dav_specdata_2021-01-01_2023-12-31_0930-1330.nc"))
##variables:
# - sza: [t], solar zenit angle, [°]
# - e_par: [t], PAR incoming [W /m2]
# - sif_a_sfm: [t], SIF A with SFM method [mW /m2 nm /sr]

###---spectral indices:

#data from Michael--aggregate original every 2 mins data to daily(use data between 9:30-13:30 local time)
#data range:May,11,2021- Dec,8,2023
NDVI<-ncvar_get(nc_data,"FULL/ndvi")
PRI<-ncvar_get(nc_data,"FULL/pri")
PAR_in<-ncvar_get(nc_data,"FULL/e_par")
FULL_sza<-ncvar_get(nc_data,"FULL/sza")
SIF_a_sfm<-ncvar_get(nc_data,"FLUO/sif_a_sfm")
FLUO_sza<-ncvar_get(nc_data,"FLUO/sza")
Date<-as.Date(ncvar_get(nc_data,"FLUO/time"),origin="2021-05-11")

df.VIs.Dav<-data.frame(Date=Date,NDVI=NDVI,PRI=PRI,PAR_in=PAR_in,FULL_sza=FULL_sza,
                       SIF_a_sfm=SIF_a_sfm,FLUO_sza=FLUO_sza)
#----------------------
#(2)exploring the data:  
#---------------------
p_t_NDVI<-ggplot(data=df.VIs.Dav)+
  geom_point(aes(x=Date,y=NDVI))
p_t_PRI<-ggplot(data=df.VIs.Dav)+
  geom_point(aes(x=Date,y=PRI))
p_t_PAR_in<-ggplot(data=df.VIs.Dav)+
  geom_point(aes(x=Date,y=PAR_in))
p_t_FULL_sza<-ggplot(data=df.VIs.Dav)+
  geom_point(aes(x=Date,y=FULL_sza))
p_t_FLUO_sza<-ggplot(data=df.VIs.Dav)+
  geom_point(aes(x=Date,y=FLUO_sza))
p_t_SIF_A<-ggplot(data=df.VIs.Dav)+
  geom_point(aes(x=Date,y=SIF_a_sfm))

#merge the plots:
library(cowplot)
p_t_merge<-plot_grid(p_t_PAR_in,p_t_FULL_sza,
                     p_t_NDVI,p_t_PRI,p_t_SIF_A,ncol = 2)
#
ggplot(data=df.VIs.Dav)+
  geom_point(aes(x=Date,y=NDVI))+
  ylim(0.6,1)

#----------------------
#(3)filter the data and construct the seasonal pattern of time seris(smoothing):  
#---------------------
library(phenopix)
library(zoo)
source(file = "./R/max.filter.R")
#for NDVI, using 90% percentile to filter
df.filter_max1<-max.filter(df.VIs.Dav,c("NDVI"),act.opts = data.frame(w=7,qt=0.9))
#other variables, using 50% percentile to filter:
df.filter_max2<-max.filter(df.VIs.Dav,c("PAR_in","PRI","SIF_a_sfm"),act.opts = data.frame(w=7,qt=0.5))

df.all.Dav<-left_join(df.VIs.Dav,df.filter_max1,by="Date")
df.all.Dav<-left_join(df.all.Dav,df.filter_max2,by="Date")%>%
  dplyr::filter(NDVI<=1&SIF_a_sfm<=1)
  
p_PAR_in<-ggplot(data=df.all.Dav)+
  geom_point(aes(x=Date,y=PAR_in))+
  geom_point(aes(x=Date,y=PAR_in.max.filtered),col="red")
p_NDVI<-ggplot(data=df.all.Dav)+
  geom_point(aes(x=Date,y=NDVI))+
  geom_point(aes(x=Date,y=NDVI.max.filtered),col="red")
p_PRI<-ggplot(data=df.all.Dav)+
  geom_point(aes(x=Date,y=PRI))+
  geom_point(aes(x=Date,y=PRI.max.filtered),col="red")
p_SIF_A<-ggplot(data=df.all.Dav)+
  geom_point(aes(x=Date,y=SIF_a_sfm))+
  geom_point(aes(x=Date,y=SIF_a_sfm.max.filtered),col="red")
##merge the plots
p_merge<-plot_grid(p_PAR_in,p_t_FULL_sza,
                     p_NDVI,p_PRI,p_SIF_A,ncol = 2)
#save the preliminary plot:
fig.save.path<-"./test/check_tower_based_VIs/"
ggsave(filename = paste0(fig.save.path,"VI_in_Davos.png"),p_merge,
       height = 12,width = 15)
##save the data:
save(df.all.Dav,file = paste0("./data/Tower_based_VIs/Davos_SIF_and_VIs.RDA"))
