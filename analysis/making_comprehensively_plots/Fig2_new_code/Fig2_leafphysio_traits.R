########################################################
#Aim: to merge plots related to GPP estimation, LUE, and leaf physiology
########################################################
#
library(tidyverse)
library(dplyr)
library(lubridate)
library(phenopix)
library(cowplot)
#----------------------
#(1)load the data
#----------------------
#---------
#load the GPP data tidy before-->adopt from Fig1_01_check_GPP_estimation
#---------
load("./test/test_datasets/df.merge.RDA")

#---------
#load the leaf physio variables -->adopt from Fig2_01_leaf_physio
#---------
df.leaf_physio<-readRDS(paste0("./data/Comprehensive_plot_data/Fig2/","df.leaf_physio.RDS"))

#----------------------
#load the spectral meaurements 
#----------------------
load.path<-"data/"
load(paste0(load.path,"Polypen.data.cleaned.RDA"))
df.Poly.sepctra<-df.Poly.sel%>%
  mutate(Date=as.Date(Date))%>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="M") ~"Middle",
                            c(Position=="U") ~"Upper"))
#-------------------------
#(2)primary plotting: adding lines and rectanges in plot
#-------------------------

##adding overestimated periods of GPP in early spring:
##comparing EC GPP with GPP simulated by P-model
#source the functions:
source("./R/separate_norm_GPPmismatch_period_trs_diff0_3SD.R")

##
df_andPlot_DE_Tha<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="DE-Tha",],
                                                0.05,"Cfb","ENF","DE-Tha",c(1996:2018))
df_andPlot_CH_Dav<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="CH-Dav",],
                                                0.05,"Dfc","ENF","CH-Dav",c(1997:2018))
#tidy the timing of mean of phenology:
Dav_pheno<-round(colMeans(df_andPlot_CH_Dav$pos_agg,na.rm = T),0)
Tha_pheno<-round(colMeans(df_andPlot_DE_Tha$pos_agg,na.rm = T),0)

#-----------
#ploting making--1. making the original leaf physio for CH-Dav and DE-Tha
#-----------
plot_ori_meansd_fun<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.leaf_physio$df_FvFm_NPQ_qN
  # var_name<-"Fv.Fm"
  # legend.xy<-c(0.1,0.9)
  # arrow.xy<-data.frame(x=86,xend=86,y=0.75,yend=0.65)
  # arrow.flag<-FALSE
  
  df$y<-as.numeric(unlist(df[,var_name]))
  #
  df_THA<-df%>%
    filter(sitename=="THA")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)%>%
    mutate(doy=yday(Date))%>%
    ##summary the data
    group_by(sitename,doy,CampaignNum,Position)%>%
    summarise(y_mean=mean(y,na.rm = T),
              y_sd=sd(y,na.rm=T))
  df_DAV<-df%>%
    filter(sitename=="DAV")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)%>%
    mutate(doy=yday(Date))%>%
    ##summary the data
    group_by(sitename,doy,CampaignNum,Position)%>%
    summarise(y_mean=mean(y,na.rm = T),
              y_sd=sd(y,na.rm=T))
  ###
  if(var_name %in% c("Fv.Fm","NPQ","qN.qP")){
    p_Dav_Date<-df_DAV%>%
      ggplot(aes(x=doy,y=y_mean,col=Position))+
      geom_point(size=3)+
      geom_line(size=1.1,lty=2)+
      geom_errorbar(aes(ymin = y_mean - y_sd, 
                        ymax = y_mean + y_sd), 
                    width = 2) +
      # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
      #              geom="pointrange",size=3,linewidth=4)+
      scale_color_manual(values = c("Lower"=adjustcolor("red",1),
                                    "Upper"=adjustcolor("red",0.4)))+
      xlim(0,250)
    p_Tha_Date<-df_THA%>%
      ggplot(aes(x=doy,y=y_mean,col=Position))+
      geom_point(size=3)+
      geom_line(size=1.1,lty=2)+
      geom_errorbar(aes(ymin = y_mean - y_sd, 
                        ymax = y_mean + y_sd), 
                    width = 2) +
      # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
      #              geom="pointrange",size=3,linewidth=4)+
      scale_color_manual(values = c("Lower"=adjustcolor("orange",1),
                                    "Upper"=adjustcolor("orange",0.4)))+
      xlim(0,250)
    
  }
  if(!c(var_name %in% c("Fv.Fm","NPQ","qN.qP"))){
    p_Dav_Date<-df_DAV%>%
      ggplot(aes(x=doy,y=y_mean,col=Position))+
      geom_point(size=3)+
      geom_line(size=1.1,lty=2)+
      geom_errorbar(aes(ymin = y_mean - y_sd, 
                        ymax = y_mean + y_sd), 
                    width = 2) +
      # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
      #              geom="pointrange",size=3,linewidth=4)+
      scale_color_manual(values = c("Lower"=adjustcolor("red",1),
                                    "Middle"=adjustcolor("red",0.7),
                                    "Upper"=adjustcolor("red",0.4)))+
      xlim(0,250)
    p_Tha_Date<-df_THA%>%
      ggplot(aes(x=doy,y=y_mean,col=Position))+
      geom_point(size=3)+
      geom_line(size=1.1,lty=2)+
      geom_errorbar(aes(ymin = y_mean - y_sd, 
                        ymax = y_mean + y_sd), 
                    width = 2) +
      # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
      #              geom="pointrange",size=3,linewidth=4)+
      scale_color_manual(values = c("Lower"=adjustcolor("orange",1),
                                    "Middle"=adjustcolor("orange",0.7),
                                    "Upper"=adjustcolor("orange",0.4)))+
      xlim(0,250)
  }
  ##
  p_Dav_Date<-p_Dav_Date+
    # labs(fill = "Sitename")+
    xlab("2023 (DOY)")+
    xlim(0,250)+
    # #adding the 0:
    # geom_hline(yintercept = 0,lty=2)+
    ##separate the campaigns-adding in Jan, 2024
    # geom_vline(xintercept = c(2.5,4.5,6.5,8.5,10.5),lty=2)+
    theme_classic()+
    theme(axis.text = element_text(size=20),
          axis.title = element_text(size=22),
          # axis.text.x = element_text(angle = 35,hjust = 1),
          #increasing text size of facet grid labels
          strip.text.x = element_text(size=18),
          legend.text = element_text(size=16),
          legend.title = element_blank(),
          legend.box.background = element_blank(),
          legend.position = legend.xy,
          legend.background = element_blank()
    )

  p_Tha_Date<-p_Tha_Date+
    # labs(fill = "Sitename")+
    xlab("2023 (DOY)")+
    xlim(0,250)+
    #adding the 0:
    # geom_hline(yintercept = 0,lty=2)+
    ##separate the campaigns-adding in Jan, 2024
    # geom_vline(xintercept = c(2.5,4.5,6.5,8.5,10.5),lty=2)+
    theme_classic()+
    theme(axis.text = element_text(size=20),
          axis.title = element_text(size=22),
          # axis.text.x = element_text(angle = 35,hjust = 1),
          #increasing text size of facet grid labels
          strip.text.x = element_text(size=18),
          legend.text = element_text(size=16),
          legend.title = element_blank(),
          legend.box.background = element_blank(),
          legend.position = legend.xy,
          legend.background = element_blank()
    )
  if(arrow.flag==TRUE){
    p_Dav_Date<-p_Dav_Date+
      #adding the flag to indicate cold events in Davos
      geom_segment(
        aes(x = arrow.xy$x, y = arrow.xy$y,
            xend = arrow.xy$xend, yend = arrow.xy$yend),
        arrow = arrow(type = "closed", 
                      length = unit(0.2, "inches")),
        color = "red", size = 0.8)
  }
  #
  p_var_Date<-list("Dav"=p_Dav_Date,
                   "Tha"=p_Tha_Date)
  
  return(p_var_Date)
}
##
Tha_sample_date<-as.Date(c("2023-03-02","2023-03-22","2023-04-13",
                           "2023-04-28","2023-05-17","2023-07-14"))
Dav_sample_date<-as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                           "2023-05-03","2023-05-22","2023-07-17"))

##-------------
#LIcor data
##-------------
library(ggforce) #-->draw circle in the plot

#Fv/Fm,NPQ,qN:
#filter the data when NPQ=0...
df.merge_FvFm_NPQ_qN<-df.leaf_physio$df_FvFm_NPQ_qN %>%
  filter(Fv.Fm>0 & NPQ >0 & qN.qP>0 & qN.qP<10)
##
p_Fv.Fm_Date<-plot_ori_meansd_fun(df.merge_FvFm_NPQ_qN,"Fv.Fm",
                    c(0.1,0.9),
                    data.frame(x=86,xend=86,y=0.75,yend=0.65),
                    TRUE)
p_Fv.Fm_Date_Dav<-p_Fv.Fm_Date$Dav+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" Fv/Fm"),
    title = "CH-Dav"
  )+
  ylim(0.35,0.95)+
  # annotate(geom = "text",x=25,y=0.9,label="CH-Dav",size=6)+
  theme(
    legend.position = "none",
    panel.spacing = unit(0.05, "lines"))+
  # annotate(geom = "text",x=yday(Dav_sample_date[1:5]),y=rep(0.35,5),
  #          label=paste0("C",1:5),size=6)
  theme(
    plot.title = element_text(hjust = 0.5, size = 24,face = "bold")
  )

p_Fv.Fm_Date_Tha<-p_Fv.Fm_Date$Tha+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" Fv/Fm"),
    title = "DE-Tha"
  )+
  ylim(0.35,0.95)+
  # annotate(geom = "text",x=25,y=0.9,label="DE-Tha",size=6)+
  theme(
    legend.position = "none",
    panel.spacing = unit(0.05, "lines"))+
    # annotate(geom = "text",x=yday(Tha_sample_date),y=rep(0.35,6),
    #        label=paste0("C",1:6),size=6)
  theme(
    plot.title = element_text(hjust = 0.5, size = 24,face = "bold")
  )
# plot_grid(p_Fv.Fm_Date_Dav,p_Fv.Fm_Date_Tha)

#plot_grid(p_Fv.Fm_Date_Dav,p_Fv.Fm_Date_Tha)
p_NPQ_Date<-plot_ori_meansd_fun(df.merge_FvFm_NPQ_qN,
                            "NPQ",c(0.1,0.9),
                            data.frame(x=84,xend=84,y=1.5,yend=1.2),
                            TRUE)
p_NPQ_Date_Dav<-p_NPQ_Date$Dav+
  labs(
    x="",
    y = expression(" NPQ")
  )+
  ylim(0,3)+
  theme(
    legend.position = "none",
    # axis.text.x = element_blank(),
    # plot.margin = margin(-10,5,0,5),
    # panel.spacing = unit(0.1, "lines")
  )
p_NPQ_Date_Tha<-p_NPQ_Date$Tha+
  labs(
    x="",
    y = expression(" NPQ")
  )+
  ylim(0,3)+
  theme(
    legend.position = "none",
    # axis.text.x = element_blank(),
    # plot.margin = margin(-10,5,0,5),
    # panel.spacing = unit(0.1, "lines")
  )
# plot_grid(p_NPQ_Date_Dav,p_NPQ_Date_Tha)
###
p_qNtoqP_Date<-plot_ori_meansd_fun(df.merge_FvFm_NPQ_qN,
                                "qN.qP",c(0.8,0.2),
                                data.frame(x=86,xend=86,y=0.5,yend=1.5),
                                TRUE)
p_qNtoqP_Date_Dav<-p_qNtoqP_Date$Dav+
  labs(
    x="",
    y = expression(" qN/qP"))+
  ylim(0,7)+
  theme(
    # axis.text.x = element_blank(),
    legend.position = "none",
    # axis.title.x = element_blank(),
    # plot.margin = margin(0,5,0,5),
    # panel.spacing = unit(0.1, "lines")
  )
p_qNtoqP_Date_Tha<-p_qNtoqP_Date$Tha+
  labs(
    x="",
    y = expression(" qN/qP"))+
  ylim(0,7)+
  theme(
    # axis.text.x = element_blank(),
    legend.position = "none",
    # axis.title.x = element_blank(),
    # plot.margin = margin(0,5,0,5),
    # panel.spacing = unit(0.1, "lines")
  )
# plot_grid(p_qNtoqP_Date_Dav,p_qNtoqP_Date_Tha)
##-------------
#Pigments data
##-------------
#adding pigments ratios
df.merge_Pigments<-df.leaf_physio$df_Pigments 
# %>%
# mutate(ChatoChb=Cha/Chb)%>%
# #only select lower and upper panel:
# filter(Position=="Lower" | Position=="Upper")
# Car/Cab
p_CartoCab_Date<-plot_ori_meansd_fun(df.merge_Pigments,"CartoCab_ratio",
                                 c(0.1,0.9),
                                 data.frame(x=86,xend=86,y=0.75,yend=0.65),
                                 TRUE)
p_CartoCab_Date_Dav<-p_CartoCab_Date$Dav+
  ylim(0.2,0.95)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" Car/Cab")
  )+
  theme(
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank(),
    # legend.position = c(0.37,0.8),
    legend.position = "none",
    # legend.background = element_rect(),
    # plot.margin = margin(0,5,0,5),
    # panel.spacing.y = unit(0.05, "lines")
  )

p_CartoCab_Date_Tha<-p_CartoCab_Date$Tha+
  ylim(0.2,0.95)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" Car/Cab")
  )+
  theme(
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank(),
    # legend.position = c(0.37,0.8),
    legend.position = "none",
    # legend.background = element_rect(),
    # plot.margin = margin(0,5,0,5),
    # panel.spacing.y = unit(0.05, "lines")
  )
# plot_grid(p_CartoCab_Date_Dav,p_CartoCab_Date_Tha)
##-------------
#leaf traits
##-------------
df.merge_traits<-df.leaf_physio$df_traits
##
p_SLA_Date<-plot_ori_meansd_fun(df.merge_traits,
                            "SLA",c(0.9,0.9),
                            data.frame(x=86,xend=86,y=50,yend=40),
                            TRUE)
p_SLA_Date_Dav<-p_SLA_Date$Dav+
  ylim(15,60)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" SLA (cm"^2*g^-1*")"),
    title = "CH-Dav"
  )+
  # annotate(geom = "text",x=25,y=60,label="CH-Dav",size=6)+
  theme(legend.position = "none",
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        # plot.margin = margin(0,5,0,5)
  )+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"))
p_SLA_Date_Tha<-p_SLA_Date$Tha+
  ylim(15,60)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" SLA (cm"^2*g^-1*")"),
    title = "DE-Tha"
  )+
  annotate(geom = "text",x=25,y=60,label="DE-Tha",size=6)+
    theme(legend.position = "none",
          # axis.text.x = element_blank(),
          # axis.title.x = element_blank(),
          # plot.margin = margin(0,5,0,5)
    )+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"))
# plot_grid(p_SLA_Date_Dav,p_SLA_Date_Tha)
  
##
p_width_Date<-plot_ori_meansd_fun(df.merge_traits,
                              "ImageJ_average_width",c(0.9,0.9),
                              data.frame(x=86,xend=86,y=0.065,yend=0.07),
                              TRUE)
p_width_Date_Dav<-p_width_Date$Dav+
  ylim(0.06,0.11)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" Width"[needle]*" (cm"*")")
  )+
  annotate(geom = "text",x=yday(Tha_sample_date[1:6]),y=rep(0.055,6),
           label=paste0("C",1:6),size=6)+
  theme(legend.position = c(0.85,0.2),
        # plot.margin = margin(0,5,0,5),
        # panel.spacing = unit(0.05, "lines")
  )
p_width_Date_Tha<-p_width_Date$Tha+
  ylim(0.06,0.11)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" Width"[needle]*" (cm"*")")
  )+
  annotate(geom = "text",x=yday(Tha_sample_date[1:6]),y=rep(0.055,6),
           label=paste0("C",1:6),size=6)+
  theme(legend.position = c(0.85,0.2),
        # plot.margin = margin(0,5,0,5),
        # panel.spacing = unit(0.05, "lines")
  )
# plot_grid(p_width_Date_Dav,p_width_Date_Tha)

#-----------
#PRI:
#-----------
#PRI
df.Poly.sepctra<-df.Poly.sepctra
# %>%
# #only select lower and upper panel:
# filter(Position=="Lower" | Position=="Upper")
p_PRI_Date<-plot_ori_meansd_fun(df.Poly.sepctra,"PRI",c(0.1,0.1),
                            data.frame(x=86,xend=86,y=0.05,yend=0.02),
                            TRUE)
p_PRI_Date_Dav<-p_PRI_Date$Dav+
  ylim(-0.15,0.06)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" PRI"[Needle])
  )+
  annotate(geom = "text",x=yday(Dav_sample_date[1:6]),y=rep(-0.15,6),
           label=paste0("C",1:6),size=6)+
  theme(legend.position = c(0.8,0.3),
        # plot.margin = margin(0,5,0,5),
        # panel.spacing = unit(0.05, "lines")
  )
p_PRI_Date_Tha<-p_PRI_Date$Tha+
  ylim(-0.15,0.06)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" PRI"[Needle])
  )+
  annotate(geom = "text",x=yday(Tha_sample_date[1:6]),y=rep(-0.15,6),
           label=paste0("C",1:6),size=6)+
  theme(legend.position = c(0.8,0.3),
        # plot.margin = margin(0,5,0,5),
        # panel.spacing = unit(0.05, "lines")
  )
#
# plot_grid(p_PRI_Date_Dav,p_PRI_Date_Tha)

#-----------
#ploting making--2. making the delta leaf physio for CH-Dav and DE-Tha
#-----------
plot_meansd_fun<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.merge_FvFm_NPQ_qN
  # var_name<-"NPQ"
  # legend.xy<-c(0.1,0.9)
  # arrow.xy<-data.frame(x=84,xend=84,y=-0.1,yend=-0.15)
  # arrow.flag<-FALSE
  
  df$y<-as.numeric(unlist(df[,var_name]))
  #
  df_THA<-df%>%
    filter(sitename=="THA")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)%>%
    ##summary the data
    group_by(sitename,Date,CampaignNum,Position)%>%
    summarise(y_mean=mean(y,na.rm = T),
                y_sd=sd(y,na.rm=T))
  df_THA_stats<-df%>%
    filter(sitename=="THA")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)
  ##
  df_DAV<-df%>%
    filter(sitename=="DAV")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)%>%
    ##summary the data
    group_by(sitename,Date,CampaignNum,Position)%>%
    summarise(y_mean=mean(y,na.rm = T),
              y_sd=sd(y,na.rm=T))
  df_DAV_stats<-df%>%
    filter(sitename=="DAV")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)
  
  #using data from Dav - Tha
  df_retidy<-left_join(df_THA,df_DAV,by=c("CampaignNum","Position"))%>%
    mutate(diff_y_mean=y_mean.y - y_mean.x,
           diff_y_sd=sqrt(y_sd.x^2+y_sd.y^2),
           diff_Date=round(as.numeric(c(yday(Date.x) + yday(Date.y))/2)))%>%
    dplyr::select(sitename.x,diff_Date,CampaignNum,Position,diff_y_mean,diff_y_sd)%>%
    mutate(sitename.x=NULL)
  df_retidy_stats<-left_join(df_THA_stats,df_DAV_stats,by=c("CampaignNum","Position"))%>%
    mutate(diff_y=y.y - y.x,
           diff_Date=round(as.numeric(c(yday(Date.x) + yday(Date.y))/2)))%>%
    dplyr::select(sitename.x,diff_Date,CampaignNum,Position,diff_y)%>%
    mutate(sitename.x=NULL)
  sig_test<-c()
  if(var_name %in% c("Fv.Fm","NPQ","qN.qP","SLA","ImageJ_average_width")){
    p_var_Date<-df_retidy%>%
      ggplot(aes(x=diff_Date,y=diff_y_mean,col=Position))+
      geom_point(size=3)+
      geom_line(size=1.1,lty=2)+
      geom_errorbar(aes(ymin = diff_y_mean - diff_y_sd, 
                        ymax = diff_y_mean + diff_y_sd), 
                    width = 2) +
      # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
      #              geom="pointrange",size=3,linewidth=4)+
      scale_color_manual(values = c("Lower"=adjustcolor("blue",0.9),
                                    "Upper"=adjustcolor("steelblue2",0.9)))
    ##add sig.test:
    var_Lower<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
    #select DOY<140--C1-C4
    filter(Position=="Lower" & diff_Date<140)%>%dplyr::select(diff_y)))
    # filter(Position=="Lower" & CampaignNum=="C2")%>%dplyr::select(diff_y)))
    var_Upper<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
    filter(Position=="Upper"& diff_Date<140)%>%dplyr::select(diff_y)))
    # filter(Position=="Upper" & CampaignNum=="C2")%>%dplyr::select(diff_y)))

    norm_test_Lower<-ks.test(var_Lower,"pnorm")
    norm_test_Upper<-ks.test(var_Upper,"pnorm")
    #update in Nov,2025:
    mean(var_Lower)
    mean(var_Upper)
    
    ifelse(norm_test_Lower$p.value>0.05,
           sig.Lower<-t.test(var_Lower),sig.Lower<-wilcox.test(var_Lower))
    ifelse(norm_test_Upper$p.value>0.05,
           sig.Upper<-t.test(var_Upper),sig.Upper<-wilcox.test(var_Upper))
    sig_test<-data.frame("sig_Lower_pvalue"=round(sig.Lower$p.value,3),
                   "sig_Lower_n"=length(var_Lower),
                   "sig_Upper_pvalue"=round(sig.Upper$p.value,3),
                   "sig_Upper_n"=length(var_Upper)
                   )
  }
  if(!c(var_name %in% c("Fv.Fm","NPQ","qN.qP","SLA","ImageJ_average_width"))){
    p_var_Date<-df_retidy%>%
      ggplot(aes(x=diff_Date,y=diff_y_mean,col=Position))+
      geom_point(size=3)+
      geom_line(size=1.1,lty=2)+
      geom_errorbar(aes(ymin = diff_y_mean - diff_y_sd, 
                        ymax = diff_y_mean + diff_y_sd), 
                    width = 2) +
      # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
      #              geom="pointrange",size=3,linewidth=4)+
      scale_color_manual(values = c("Lower"=adjustcolor("blue",0.9),
                                    "Middle"=adjustcolor("purple1",0.9),
                                    "Upper"=adjustcolor("steelblue2",0.9)))
  
    ##add sig.test:
    var_Lower<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
                                   #select DOY<140--C1-C4
                                   filter(Position=="Lower" & diff_Date<140)%>%dplyr::select(diff_y)))
    var_Middle<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
                                   filter(Position=="Middle"& diff_Date<140)%>%dplyr::select(diff_y)))
    var_Upper<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
                                   filter(Position=="Upper"& diff_Date<140)%>%dplyr::select(diff_y)))
    norm_test_Lower<-ks.test(var_Lower,"pnorm")
    norm_test_Middle<-ks.test(var_Middle,"pnorm")
    norm_test_Upper<-ks.test(var_Upper,"pnorm")
    
    ifelse(norm_test_Lower$p.value>0.05,
           sig.Lower<-t.test(var_Lower),sig.Lower<-wilcox.test(var_Lower))
    ifelse(norm_test_Middle$p.value>0.05,
           sig.Middle<-t.test(var_Middle),sig.Middle<-wilcox.test(var_Middle))
    ifelse(norm_test_Upper$p.value>0.05,
           sig.Upper<-t.test(var_Upper),sig.Upper<-wilcox.test(var_Upper))
    sig_test<-list("sig_Lower_pvalue"=round(sig.Lower$p.value,2),
                   "sig_Lower_n"=length(var_Lower),
                   "sig_Middle_pvalue"=round(sig.Middle$p.value,2),
                   "sig_Middle_n"=length(var_Middle),
                   "sig_Upper_pvalue"=round(sig.Upper$p.value,2),
                   "sig_Upper_n"=length(var_Upper)
    )
  }
  print(sig_test)
  
  p_var_Date<-p_var_Date+
    # labs(fill = "Sitename")+
    xlab("2023-DOY")+
    xlim(0,250)+
    #adding the 0:
    geom_hline(yintercept = 0,lty=2)+
    ##separate the campaigns-adding in Jan, 2024
    # geom_vline(xintercept = c(2.5,4.5,6.5,8.5,10.5),lty=2)+
    theme_classic()+
    theme(axis.text = element_text(size=20),
          axis.title = element_text(size=22),
          # axis.text.x = element_text(angle = 35,hjust = 1),
          #increasing text size of facet grid labels
          strip.text.x = element_text(size=18),
          legend.text = element_text(size=16),
          legend.title = element_blank(),
          legend.box.background = element_blank(),
          legend.position = legend.xy,
          legend.background = element_blank()
    )
  if(arrow.flag==TRUE){
    p_var_Date<-p_var_Date+
      #adding the flag to indicate cold events in Davos
      geom_segment(
        aes(x = arrow.xy$x, y = arrow.xy$y,
            xend = arrow.xy$xend, yend = arrow.xy$yend),
        arrow = arrow(type = "closed", 
                      length = unit(0.2, "inches")),
        color = "red", size = 0.8)
  }
  return(p_var_Date)
}

##-------------
#LIcor data
##-------------
library(ggforce) #-->draw circle in the plot

#Fv/Fm,NPQ,qN:
#filter the data when NPQ=0...
df.merge_FvFm_NPQ_qN<-df.leaf_physio$df_FvFm_NPQ_qN %>%
  filter(Fv.Fm>0 & NPQ >0 & qN.qP>0 & qN.qP<10)
p_Fv.Fm_Date_diff<-plot_meansd_fun(df.merge_FvFm_NPQ_qN,"Fv.Fm",
                               c(0.1,0.9),
                               data.frame(x=84,xend=84,y=-0.05,yend=-0.15),
                               TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" Fv/Fm"),
    title = "CH-Dav — DE-Tha"
  )+
  # annotate(geom = "text",x=200,y=0.02,label="CH-Dav - DE-Tha",size=6)+
  theme(
        legend.position = "none",
        # legend.background = element_rect(),
        # plot.margin = margin(0,5,0,5),
        panel.spacing = unit(0.05, "lines"))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 24,face = "bold")
  )
  # annotate(geom = "text",x=c(64),y=rep(-0.4),label=c("C1"),size=6)+
  # annotate(geom = "text",x=c(84),y=rep(-0.4),label=c("C2"),size=6)+
  # annotate(geom = "text",x=c(107),y=rep(-0.4),label=c("C3"),size=6)+
  # annotate(geom = "text",x=c(120),y=rep(-0.4),label=c("C4"),size=6)+
  # annotate(geom = "text",x=c(140),y=rep(-0.4),label=c("C5"),size=6)
  # annotate(geom = "text",x=c(196),y=rep(-0.4),label=c("C6"),size=6)


# p_PhiPS2_Date<-plot_boxplot_fun(df.merge_FvFm_NPQ_qN,
#                                 "PhiPS2",c(0.8,0.2))+
#   labs(
#     x="",
#     y = (expression(phi[PSII]))
#   )+
#   theme(legend.position = "none",
#         axis.text.x = element_blank()
#         )
p_NPQ_Date_diff<-plot_meansd_fun(df.merge_FvFm_NPQ_qN,
                             "NPQ",c(0.1,0.9),
                             data.frame(x=84,xend=84,y=0,yend=-0.3),
                             TRUE)+
  labs(
    x="",
    y = expression(Delta*" NPQ")
  )+
  theme(
        legend.position = "none",
        # axis.text.x = element_blank(),
        # plot.margin = margin(-10,5,0,5),
        # panel.spacing = unit(0.1, "lines")
        )
p_qNtoqP_Date_diff<-plot_meansd_fun(df.merge_FvFm_NPQ_qN,
                                "qN.qP",c(0.8,0.2),
                                data.frame(x=84,xend=84,y=5,yend=3.5),
                                TRUE)+
  labs(
    x="",
    y = expression(Delta*" qN/qP"))+
  # ylim(0,9)+
  theme(
    # axis.text.x = element_blank(),
    legend.position = "none",
        # axis.title.x = element_blank(),
        # plot.margin = margin(0,5,0,5),
        # panel.spacing = unit(0.1, "lines")
    )

##-------------
#Pigments data
##-------------
#adding pigments ratios
df.merge_Pigments<-df.leaf_physio$df_Pigments 
  # %>%
  # mutate(ChatoChb=Cha/Chb)%>%
  # #only select lower and upper panel:
  # filter(Position=="Lower" | Position=="Upper")
# Car/Cab
p_CartoCab_Date_diff<-plot_meansd_fun(df.merge_Pigments,"CartoCab_ratio",
                                       c(0.1,0.9),
                                       data.frame(x=84,xend=84,y=0.5,yend=0.35),
                                       TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" Car/Cab")
  )+
  theme(
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        # legend.position = c(0.37,0.8),
        legend.position = "none",
        # legend.background = element_rect(),
        # plot.margin = margin(0,5,0,5),
        # panel.spacing.y = unit(0.05, "lines")
        )

#addtional plots:
#theorotically, if ratio of Cha/Chb high-->higher light photoprotection according to Liyao
#but the results did not reflect this 
p_ChatoChb_Date_diff<-plot_meansd_fun(df.merge_Pigments,"Car",c(0.9,0.9),
                                       data.frame(x=84,xend=84,y=0.15,yend=0.1),
                                       TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Chla/Chb")
  )+
  theme(
    # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        # legend.position = c(0.35,0.8),
        # legend.background = element_rect(),
        plot.margin = margin(0,5,0,5))

##-------------
#leaf traits
##-------------
df.merge_traits<-df.leaf_physio$df_traits
p_SLA_Date_diff<-plot_meansd_fun(df.merge_traits,
                             "SLA",c(0.9,0.9),
                             data.frame(x=84,xend=84,y=5,yend=1.5),
                             TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" SLA (cm"^2*g^-1*")"),
    title = "CH-Dav — DE-Tha"
  )+
  # annotate(geom = "text",x=200,y=5,label="CH-Dav - DE-Tha",size=6)+
  theme(legend.position = "none",
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        # plot.margin = margin(0,5,0,5)
        )+
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold")
  )

# p_LMA_Date<-plot_point_fun_meansd(df.merge_traits,"LMA",c(0.9,0.9))+
#   labs(
#     x="2023",
#     # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
#     y = expression(LMA*" (g"*"cm"^-2*")")
#   )
p_width_Date_diff<-plot_meansd_fun(df.merge_traits,
                               "ImageJ_average_width",c(0.9,0.9),
                               data.frame(x=84,xend=84,y=0.04,yend=0.02),
                               TRUE)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" Width"[needle]*" (cm"*")")
  )+
  theme(legend.position = c(0.85,0.2),
        # plot.margin = margin(0,5,0,5),
        # panel.spacing = unit(0.05, "lines")
        )+
  annotate(geom = "text",x=c(64),y=rep(-0.02),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.02),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.02),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.02),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.02),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.02),label=c("C6"),size=6)

#-----------
#PRI:making the delta leaf spectra for CH-Dav and DE-Tha
#-----------
#PRI
df.Poly.sepctra<-df.Poly.sepctra
  # %>%
  # #only select lower and upper panel:
  # filter(Position=="Lower" | Position=="Upper")
p_PRI_Date_diff<-plot_meansd_fun(df.Poly.sepctra,"PRI",c(0.1,0.1),
                                  data.frame(x=84,xend=84,y=0.05,yend=0.01),
                                  TRUE)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" PRI"[Needle])
  )+
  theme(legend.position = c(0.8,0.3),
        # plot.margin = margin(0,5,0,5),
        # panel.spacing = unit(0.05, "lines")
        )+
  annotate(geom = "text",x=c(64),y=rep(-0.13),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.13),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.13),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.13),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.13),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.13),label=c("C6"),size=6)

#-------------------------
#(3)further plotting: adding lines and rectanges in plot
#-------------------------
#period:[sos-60,sos+60] is set to red
#adding the sos
#for CH-Dav
rect.coord_isevent_Dav=data.frame("x1"=as.numeric(Dav_pheno[1])-60,
                              "x2"=as.numeric(Dav_pheno[1]),
                              "x3"=as.numeric(Dav_pheno[1]+60),
                              "y1"=-Inf,
                              "y2"=Inf)
rect.coord_isevent_Tha=data.frame("x1"=as.numeric(Tha_pheno[1])-60,
                                  "x2"=as.numeric(Tha_pheno[1]),
                                  "x3"=as.numeric(Tha_pheno[1]+60),
                                  "y1"=-Inf,
                                  "y2"=Inf)
plot_add_rectanges<-function(df.plot,rect.coord_isevent,x_text,y_text){
  # df.plot<-p_Fv.Fm_Date_Dav
  # rect.coord_isevent<-rect.coord_isevent_Dav
  # x_text<-Dav_pheno[1]
  # y_text<-0.4
  
  df_add<-df.plot+
    # annotate("rect",xmin = rect.coord_isevent$x1,
    #          xmax=rect.coord_isevent$x2,
    #          ymin = -Inf, ymax = Inf,
    #          fill="blue",alpha=0.3
    #          )+
    annotate("rect",xmin = rect.coord_isevent$x2,
             xmax=rect.coord_isevent$x3,
             ymin = -Inf, ymax = Inf,
             fill="blue",alpha=0.1
    )+
    geom_vline(xintercept=as.numeric(x_text),col="forestgreen",size=1.1)+
    annotate(geom = "text",x=as.numeric(x_text)+10,y=y_text,label="sos",col="forestgreen",size=6)
  
  return(df_add)
}

#1. for DSPR period for Fv.Fm:
p_Fv.Fm_Date_Dav<-plot_add_rectanges(p_Fv.Fm_Date_Dav,rect.coord_isevent_Dav,Dav_pheno[1],0.4)
p_Fv.Fm_Date_Tha<-plot_add_rectanges(p_Fv.Fm_Date_Tha,rect.coord_isevent_Tha,Tha_pheno[1],0.4)
p_Fv.Fm_Date_diff<-plot_add_rectanges(p_Fv.Fm_Date_diff,rect.coord_isevent_Tha,Tha_pheno[1],-0.4)
##add sig. symbol:
p_Fv.Fm_Date_diff<-p_Fv.Fm_Date_diff +
  annotate("text",x=c(100,100),y=c(0.04,0),label=c("***","***"),
           size=10,col=c(adjustcolor("blue",0.9),adjustcolor("steelblue2",0.9)))
plot_grid(p_Fv.Fm_Date_Dav,p_Fv.Fm_Date_Tha,p_Fv.Fm_Date_diff,nrow=1)

#2. for DSPR period for NPQ:
p_NPQ_Date_Dav<-plot_add_rectanges(p_NPQ_Date_Dav,rect.coord_isevent_Dav,Dav_pheno[1],0.4)
p_NPQ_Date_Tha<-plot_add_rectanges(p_NPQ_Date_Tha,rect.coord_isevent_Tha,Tha_pheno[1],0.4)
p_NPQ_Date_diff<-plot_add_rectanges(p_NPQ_Date_diff,rect.coord_isevent_Tha,Tha_pheno[1],-1.2)
##add sig. symbol:
p_NPQ_Date_diff<-p_NPQ_Date_diff +
  annotate("text",x=c(100,100),y=c(0.9,0.5),label=c("***","***"),
           size=10,col=c(adjustcolor("blue",0.9),adjustcolor("steelblue2",0.9)))
plot_grid(p_NPQ_Date_Dav,p_NPQ_Date_Tha,p_NPQ_Date_diff,nrow=1)

#3. for DSPR period for qN/qP:
p_qNtoqP_Date_Dav<-plot_add_rectanges(p_qNtoqP_Date_Dav,rect.coord_isevent_Dav,Dav_pheno[1],0.4)
p_qNtoqP_Date_Tha<-plot_add_rectanges(p_qNtoqP_Date_Tha,rect.coord_isevent_Tha,Tha_pheno[1],0.4)
p_qNtoqP_Date_diff<-plot_add_rectanges(p_qNtoqP_Date_diff,rect.coord_isevent_Tha,Tha_pheno[1],-4.5)
##add sig. symbol:
p_qNtoqP_Date_diff<-p_qNtoqP_Date_diff +
  annotate("text",x=c(100,100),y=c(4.5,3),label=c("***","***"),
           size=10,col=c(adjustcolor("blue",0.9),adjustcolor("steelblue2",0.9)))
plot_grid(p_qNtoqP_Date_Dav,p_qNtoqP_Date_Tha,p_qNtoqP_Date_diff,nrow=1)

#4. for DSPR period for CartoCarb:
p_CartoCab_Date_Dav<-plot_add_rectanges(p_CartoCab_Date_Dav,rect.coord_isevent_Dav,Dav_pheno[1],0.3)
p_CartoCab_Date_Tha<-plot_add_rectanges(p_CartoCab_Date_Tha,rect.coord_isevent_Tha,Tha_pheno[1],0.3)
p_CartoCab_Date_diff<-plot_add_rectanges(p_CartoCab_Date_diff,rect.coord_isevent_Tha,Tha_pheno[1],-0.2)
p_CartoCab_Date_diff<-p_CartoCab_Date_diff +
  annotate("text",x=c(100,100,100),y=c(0.45,0.35,0.25),label=c("***","***","***"),
           size=10,col=c(adjustcolor("blue",0.9),
                         adjustcolor("purple",0.9),adjustcolor("steelblue2",0.9)))
plot_grid(p_CartoCab_Date_Dav,p_CartoCab_Date_Tha,p_CartoCab_Date_diff,nrow=1)

#5. for DSPR period for PRI:
p_PRI_Date_Dav<-plot_add_rectanges(p_PRI_Date_Dav,rect.coord_isevent_Dav,Dav_pheno[1],-0.13)
p_PRI_Date_Tha<-plot_add_rectanges(p_PRI_Date_Tha,rect.coord_isevent_Tha,Tha_pheno[1],-0.13)
p_PRI_Date_diff<-plot_add_rectanges(p_PRI_Date_diff,rect.coord_isevent_Tha,Tha_pheno[1],-0.11)
##add sig. symbol:
p_PRI_Date_diff<-p_PRI_Date_diff +
  annotate("text",x=c(100,100,100),y=c(0.045,0.025,0.005),label=c("***","***","***"),
           size=10,col=c(adjustcolor("blue",0.9),adjustcolor("purple",0.9),
                         adjustcolor("steelblue2",0.9)))
plot_grid(p_PRI_Date_Dav,p_PRI_Date_Tha,p_PRI_Date_diff,nrow=1)

#5. for DSPR period for SLA:
p_SLA_Date_Dav<-plot_add_rectanges(p_SLA_Date_Dav,rect.coord_isevent_Dav,Dav_pheno[1],0.4)
p_SLA_Date_Tha<-plot_add_rectanges(p_SLA_Date_Tha,rect.coord_isevent_Tha,Tha_pheno[1],0.4)
p_SLA_Date_diff<-plot_add_rectanges(p_SLA_Date_diff,rect.coord_isevent_Tha,Tha_pheno[1],-25)
##add sig. symbol:
p_SLA_Date_diff<-p_SLA_Date_diff +
  annotate("text",x=c(100,100),y=c(4.5,0.5),label=c("***","**"),
           size=10,col=c(adjustcolor("blue",0.9),adjustcolor("steelblue2",0.9)))
plot_grid(p_SLA_Date_Dav,p_SLA_Date_Tha,p_SLA_Date_diff,nrow=1)

#6. for DSPR period for width:
p_width_Date_Dav<-plot_add_rectanges(p_width_Date_Dav,rect.coord_isevent_Dav,Dav_pheno[1],0.06)
p_width_Date_Tha<-plot_add_rectanges(p_width_Date_Tha,rect.coord_isevent_Tha,Tha_pheno[1],0.06)
p_width_Date_diff<-plot_add_rectanges(p_width_Date_diff,rect.coord_isevent_Tha,Tha_pheno[1],-0.015)
##add sig. symbol:
p_width_Date_diff<-p_width_Date_diff +
  annotate("text",x=c(100,100),y=c(0.035,0.025),label=c("***","***"),
           size=10,col=c(adjustcolor("blue",0.9),adjustcolor("steelblue2",0.9)))
plot_grid(p_width_Date_Dav,p_width_Date_Tha,p_width_Date_diff,nrow=1)

#-------------------------
#(4)merge the plots:
#-------------------------
p_merge_main<-plot_grid(
  p_Fv.Fm_Date_Dav,p_Fv.Fm_Date_Tha,p_Fv.Fm_Date_diff,
  p_NPQ_Date_Dav,p_NPQ_Date_Tha,p_NPQ_Date_diff,
  p_qNtoqP_Date_Dav,p_qNtoqP_Date_Tha,p_qNtoqP_Date_diff,
  p_CartoCab_Date_Dav,p_CartoCab_Date_Tha,p_CartoCab_Date_diff,
  p_PRI_Date_Dav,p_PRI_Date_Tha,p_PRI_Date_diff,
  ncol = 3, nrow=5,
  align = "hv",
  labels = c("(a)","(b)","(c)",
  "(d)","(e)","(f)",
  "(g)","(h)","(i)",
  "(j)","(k)","(l)",
  "(m)","(n)","(o)"
  ),
  axis="tblr",
  label_size = 22,
  # rel_heights = c(0.19,0.19,0.19,0.22,0.2)
  theme(plot.margin = margin(0,0,0,0),
        panel.spacing = unit(0,"lines"))
  )
p_merge_traits<-plot_grid(
  p_SLA_Date_Dav,p_SLA_Date_Tha,p_SLA_Date_diff,
  p_width_Date_Dav,p_width_Date_Tha,p_width_Date_diff,
  ncol = 3, nrow=2,
  align = "hv",
  labels = c("(a)","(b)","(c)",
             "(d)","(e)","(f)"
  ),
  axis="tblr",
  label_size = 22,
  # rel_heights = c(0.19,0.19,0.19,0.22,0.2)
  theme(plot.margin = margin(0,0,0,0),
        panel.spacing = unit(0,"lines"))
                          )

##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig2_main.png"),p_merge_main,width = 20,height=18)
ggsave(paste0(save.path,"Fig2_supp_traits.png"),p_merge_traits,width = 20,height=8)

