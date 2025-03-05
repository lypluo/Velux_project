########################################################
#Aim: merge the small plots for Figure 3
########################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

#----load the data
load.path<-"./data/Comprehensive_plot_data/Fig3/"
load(paste0(load.path,"p_leaf_spectra_NDVI.RDA"))
load(paste0(load.path,"p_crown_NDVI.RDA"))
#load the drones' data
drones.path<-"D:/data/Velux_shared_data/CH-Dav/HighSpec/Github_data/"
load(paste0(drones.path,"p_Drones_NDVI_boxplot.RDA"))
load(paste0(drones.path,"p_Drones_NDVI_image.RDA"))
#Drone's CCI:
load(paste0(drones.path,"p_Drones_CCI_boxplot.RDA"))
load(paste0(drones.path,"p_Drones_CCI_image.RDA"))

#--------
#merge the plot
#-------
###A-->For NDVI
p_merge1<-plot_grid(p_merge_image_NDVI,p_NDVI_boxplot,
                    labels = c("(c)","(d)"),
                    label_x = 0.02,label_y = 0.98,
                    label_size = 24,
                    nrow=1,
                    greedy = FALSE,
                    rel_widths = c(3,2))
p_merge2<-plot_grid(p_leaf_spectra_NDVI,
                    p_crown_NDVI,
                    labels = c("(a)","(b)"),
                    label_x = -0.01,label_y = 1.05,
                    label_size = 24,
                    align="hv",
                    ncol=1)
p_merge3<-plot_grid(p_merge2,
                    p_merge1,
                    align="hv",
                    ncol=1,rel_heights = c(0.4,0.6))
##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
# ggsave(paste0(save.path,"Fig3_part.png"),
#        p_merge1,width = 29,height=20,limitsize = FALSE)

ggsave(paste0(save.path,"Fig3_supp1_NDVI.png"),
       p_merge3,width = 19,height=19.04)

###B-->For CCI:(no leaf and crown CCI)
p_merge1<-plot_grid(p_merge_image_CCI,p_CCI_boxplot,
                    labels = c("(a)","(b)"),
                    label_x = 0.02,label_y = 0.98,
                    label_size = 24,
                    nrow=1,
                    # align = "hv",
                    greedy = FALSE,
                    rel_widths = c(3,2))
ggsave(paste0(save.path,"Fig3_supp1_CCI.png"),
       p_merge1,width = 17.6,height=10.34)
