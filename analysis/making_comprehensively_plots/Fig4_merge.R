########################################################
#Aim: merge the small plots for Figure 4
########################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

#----load the data
load.path<-"./data/Comprehensive_plot_data/Fig4/"

load(paste0(load.path,"p_leaf_hydro.RDA"))
load(paste0(load.path,"p_samplingYear_VPD_SWC.RDA"))
load(paste0(load.path,"p_Eco_ET_Gs.RDA"))

#--------
#merge the plot
#-------

#
p_merge1<-plot_grid(p_leaf_hydro,p_samplingYear_VPD_SWC,
                   align="hv",
                   ncol=1,rel_widths = c(0.88,0.12))
p_merge2<-plot_grid(p_merge1,
                    p_Eco_ET_Gs,
                    align="hv",
                    ncol=2,rel_widths = c(0.6,0.4))
##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig4.png"),p_merge2,width = 33,height=16)

