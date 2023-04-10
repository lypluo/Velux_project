###################################################
#Aim:to select the region of interests (ROIs) for NL-Loobos
###################################################
Sys.setenv(tz="UTC")
library(phenopix)
library(zoo)
library(lubridate)
library(tidyverse)
#create the strucuture folder:
folder.path<-"./data-raw/PhenoCam_Data/NL_Loo/"
# structureFolder(folder.path)  ##create the folders for analysis


##(1)Drow ROIs
  #-----set the path----
  ## set path of the reference image where to draw ROIs. Only one jpeg image is allowed in this folder.
  path.image.ref <- paste(folder.path,'REF/',sep='')  #here is the path for reference image
  ## set path where to store the ROI coordinates
  path.roi <- paste(folder.path,'ROI/',sep='')
  ## define path with all images to be processed
  # img.path <- paste0("D:/data/WSL_PhenoCam/Lorenz_Walthert/images_2021_2022/",years[i],"/") #change the path
  ## define in which folder VI data will be stored
  # vi.path <- paste0(folder.path,'VI/',years[i],"/")

  
  #---------------------
  #select the tree ROI:Tree1,Tree2 
  #---------------------

  roi.names <- c('EN_1000','EN_2000',"EN_2001","EN_2002","EN_2003","EN_2004")
  nroi=length(roi.names)
  drawROI<-TRUE
 
  if (drawROI == TRUE){ #Feb,2023-->need to press "ESC" when finishing the selection of ROI

    DrawMULTIROI(path.image.ref, path.roi, nroi=nroi,roi.names,file.type = ".jpg")

  }

 