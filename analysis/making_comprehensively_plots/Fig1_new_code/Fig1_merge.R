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
load(paste0(load.path,"p_check_LUE.RDA"))
load(paste0(load.path,"p_MultiYear_PAR.RDA"))
load(paste0(load.path,"p_MultiYear_Ta.RDA"))

#--------
#merge the plot
#-------
p1<-p_check_overest$p_over_CH_Dav
p2<-p_check_overest$p_over_DE_Tha
p3<-p_check_overest$p_overGPP_boxplot

p4<-p_check_LUE$p_CH_Dav_LUE
p5<-p_check_LUE$p_DE_Tha_LUE
p6<-p_check_LUE$p_LUE_boxplot

p7<-p_PAR$p_CH_Dav_PAR
p8<-p_PAR$p_DE_Tha_PAR
p9<-p_PAR$p_PAR_boxplot

p10<-p_Ta$p_CH_Dav_Ta
p11<-p_Ta$p_DE_Tha_Ta
p12<-p_Ta$p_Ta_boxplot

#
p_merge<-plot_grid(p1,p2,p3,
          p4,p5,p6,
          p7,p8,p9,
          p10,p11,p12,
          nrow=4,
          labels = c("(a)","(b)","(c)",
                     "(d)","(e)","(f)",
                     "(g)","(h)","(i)",
                     "(j)","(k)","(m)"),
          label_size = 22,
          align = "hv")

#
# p_merge<-plot_grid(p_check_overest,
#                    p_check_LUE,
#                    p_PAR,
#                    p_Ta,align="hv",
#                    axis = "tblr",
#                    nrow=4)
##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig1.png"),p_merge,width = 21,height=18)

