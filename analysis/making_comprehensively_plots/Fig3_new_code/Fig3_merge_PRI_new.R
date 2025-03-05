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
load(paste0(load.path,"p_leaf_spectra_PRI_new.RDA"))
load(paste0(load.path,"p_crown_PRI_new.RDA"))
#load the drones' data
drones.path<-"D:/data/Velux_shared_data/CH-Dav/HighSpec/Github_data/"
load(paste0(drones.path,"p_Drones_PRI_boxplot.RDA"))
load(paste0(drones.path,"p_Drones_PRI_image.RDA"))

#--------
#merge the plot
#-------
p_merge_image_PRI <- p_merge_image_PRI + theme(plot.margin = margin(5, 5, 5, 5))
p_PRI_boxplot <- p_PRI_boxplot + theme(plot.margin = margin(5, 5, 5, 5))
p_merge1<-plot_grid(p_merge_image_PRI,p_PRI_boxplot,
                    labels = c("(c)","(d)"),
                    # label_x = 0.02,label_y = 0.98,
                    label_size = 24,
                    nrow=1,
                    align="v"
                    # axis="tblr",
                    # greedy = FALSE
                    # rel_widths = c(3,2)
                    )
#
p_PRI_Meteo <- p_PRI_Meteo + theme(plot.margin = margin(5, 5, 5, 5))
p_crown_PRI <- p_crown_PRI + theme(plot.margin = margin(5, 5, 5, 5))
p_merge2<-plot_grid(p_PRI_Meteo,
                    p_crown_PRI,
                    labels = c("(a)","(b)"),
                    label_size = 24,
                    # label_x = -0.01,label_y = 1.05,
                    # axis="lr",
                    align="h",
                    ncol=1)
p_merge3<-plot_grid(p_merge2,
                    p_merge1,
                    align="v",
                    ncol=1,
                    rel_heights = c(0.48,0.52)
                    )
##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
# ggsave(paste0(save.path,"Fig3_part.png"),
#        p_merge1,width = 29,height=20,limitsize = FALSE)

ggsave(paste0(save.path,"Fig3_new.png"),
       p_merge3,width = 24,height=23)
       # p_merge3,width = 19.5,height=19.8)


