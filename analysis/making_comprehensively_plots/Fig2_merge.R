########################################################
#Aim: merge the small plots for Figure 2
########################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

#----load the data
load.path<-"./data/Comprehensive_plot_data/Fig2/"

load(paste0(load.path,"p_leaf_physio.RDA"))
load(paste0(load.path,"p_Eco_NEE.RDA"))

#--------
#merge the plot
#-------

#
p_merge<-plot_grid(p_leaf_physio,
                   p_Eco_NEE,align="hv",
                   ncol=2,rel_widths = c(0.6,0.4))
##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig2.png"),p_merge,width = 25,height=15)
