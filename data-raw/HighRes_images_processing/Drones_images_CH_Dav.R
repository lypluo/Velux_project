#####################################################
#Aim:data processing for Davos
#####################################################
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(cowplot)

#----------------------------
#(0)load the drones' data 
#----------------------------
data.path<-"F:/WSL/EE_WSL/IMPACT_project/data_collection/Dav_Data/Drones_data/03_results/"
dir.files<-list.files(data.path)
sel.tiff<-dir.files[c(grep("_Ortho",dir.files),grep("_ortho",dir.files))]
##remove the not needed files
sel.tiff<-sel.tiff[c(1,2,4,5,6,8)]
## plotting test
f_test<-raster::raster(paste0(data.path,sel.tiff[2]))
raster::plot(f_test)

#----------------------------
#(2)start to processing the data 
#----------------------------
for(i in 1:length(sel.tiff)){
  df.proc<-paste0(data.path,sel.tiff[2])%>%
    raster::stack()%>%
    raster::as.data.frame(.,xy=TRUE)
  
  
}