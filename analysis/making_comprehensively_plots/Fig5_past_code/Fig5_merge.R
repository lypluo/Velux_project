########################################################
#Aim: merge the small plots for Figure 5
########################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

#----load the data
load.path<-"./data/Comprehensive_plot_data/Fig5/"

load(paste0(load.path,"p_GPP_PRI.RDA"))
load(paste0(load.path,"p_corr.RDA"))
load(paste0(load.path,"p_with_adjT_season_plot.RDA"))

#--------
#merge the plot
#-------
p_with_adjT_season_plot<-p_with_adjT_season_plot+
  xlim(0,250)+
  theme(legend.position = c(0.65,0.2))
#
p_merge<-plot_grid(p_with_adjT_season_plot,p_GPP_PRI,
                    p_corr,
                    align="hv",
                    ncol=3,nrow=1,
                    labels=c("(a)","(b)","(c)")
                    # rel_widths = c(0.4,0.3,0.3)
                    )
##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig5.png"),p_merge,
       width = 25,height=7)

