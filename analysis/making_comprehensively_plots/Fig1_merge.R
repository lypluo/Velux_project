########################################################
#Aim: merge the small plots for Figure 1
########################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

#----load the data
load.path<-"./data/Comprehensive_plot_data/Fig1/"
load(paste0(load.path,"p_check_overest.RDA"))
load(paste0(load.path,"p_MultiYear_Meteo.RDA"))
load(paste0(load.path,"p_samplingYear_Meteo.RDA"))

#--------
#merge the plot
#-------
# p1<-p_check_overest$p_over_CH_Dav
# p2<-p_check_overest$p_over_DE_Tha
# p3<-p_check_overest$p_overGPP_boxplot
# 
# p4<-p_MultiYears_Meteo$p_Ta
# p5<-p_MultiYears_Meteo$p_PPFD
# p6<-p_MultiYears_Meteo$p_boxplot
# 
# p7<-p_samplingYear_Meteo$p_Ta
# p8<-p_samplingYear_Meteo$p_PPFD
# p9<-p_samplingYear_Meteo$p_boxplot

#
# p_merge<-plot_grid(p1,p2,p3,
#           p4,p5,p6,
#           p7,p8,p9,
#           nrow=3,
#           align = "hv")

#
p_merge<-plot_grid(p_check_overest,
                   p_MultiYears_Meteo,
                   p_samplingYear_Meteo,align="hv",
                   nrow=3)
##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig1.png"),p_merge,width = 20,height=15)

