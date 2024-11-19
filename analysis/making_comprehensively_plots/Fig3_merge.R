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
load(paste0(load.path,"p_leaf_spectra_PRI.RDA"))
load(paste0(load.path,"p_Drones_PRI_boxplot.RDA"))
#load the drones' data
drones.path<-"D:/data/Velux_shared_data/CH-Dav/HighSpec/Github_data/"
load(paste0(drones.path,"p_Drones_PRI_boxplot.RDA"))
load(paste0(drones.path,"p_Drones_PRI_image.RDA"))

#--------
#merge the plot
#-------
p_merge1<-plot_grid(p_merge_image_PRI,p_PRI_boxplot,
                    labels = c("(c)","(d)"),
                    label_x = 0.05,label_y = 0.99,
                    label_size = 24,
                    nrow=1,
                    greedy = FALSE,
                    rel_widths = c(3,2))
p_merge2<-plot_grid(p_leaf_spectra_PRI,
                    p_crown_PRI,
                    labels = c("(a)","(b)"),
                    label_x = 0,label_y = 0.97,
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

ggsave(paste0(save.path,"Fig3.png"),
       p_merge3,width = 38,height=43.2)


