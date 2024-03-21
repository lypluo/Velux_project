##################################################
#Aim: tidy the data from sensors we deployed in the Tharandt
##################################################

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(dplyr)
library(tidyverse)

#----------------------
#(1)load and tidy the data
#---------------------
