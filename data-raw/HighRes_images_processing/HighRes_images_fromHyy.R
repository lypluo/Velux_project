##########################################################
##background:Jan sent us the HighRes images from drones and procesed data
#Try to access them and check how it looks
##########################################################
library("rgdal")
library("raster")
library("caTools")

#-------------------------
#(1)load the data:
#-------------------------
load.path<-"F:/data/Highres_images/FI_Hyy/2017-07-27_Hyytiala05_rela"
test<-raster(load.path)
