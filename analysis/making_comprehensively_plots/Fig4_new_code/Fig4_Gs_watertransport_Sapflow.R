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
#load calculated Gs tidy before-->adopt from Fig4_03_Eco_Gs
#---------
df_Gc<-readRDS(paste0("./data/Comprehensive_plot_data/Fig4/","df.Eco_ETGs.RDA"))

#---------
#load the leaf physio variables -->adopt from Fig2_01_leaf_physio
#---------
df.leaf_physio<-readRDS(paste0("./data/Comprehensive_plot_data/Fig2/","df.leaf_physio.RDS"))

#----------------------
#load the water potential -->adopt from Fig4_supp1_branch_stem_WaterPotential
#----------------------
load.path<-"data/Water_Potential/"
load(paste0(load.path,"WaterPotential.data.cleaned.RDA"))
df.WP<-df.WaterP.final%>%
  mutate(Date=as.Date(Date))%>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="U") ~"Upper",
                            c(Position=="D") ~"Trunk"))
#----------------------
#load the sap flow -->adopt from Fig4_supp2_check_sapflow_afterC2
#----------------------
df.SF<-readRDS(paste0("./data/Comprehensive_plot_data/Fig4/df.SFD.RDS"))

#----------------------
#load the soil water potential 
#----------------------
load.path<-"./data/Soil_water_potential/"
#1997-2014
df.SWP_Tha<-readRDS(paste0(load.path,"SWP_DE-Tha.RDS"))

#2020-2024
df.SWP_Dav<-readRDS(paste0(load.path,"SWP_CH-Dav.RDS"))
#
##-->aggregate the SWP in different depth-->shallow: <=30; Medium: <50
df.SWP_Tha_agg<-df.SWP_Tha %>%
  mutate(SWP_20cm=ifelse(SWP_20cm< -100,NA,SWP_20cm),
         SWP_30cm=ifelse(SWP_30cm< -100,NA,SWP_30cm),
         SWP_50cm=ifelse(SWP_50cm< -100,NA,SWP_50cm)
         )%>%
  group_by(Date)%>%
  mutate(SWP_shallow=rowMeans(across(SWP_20cm:SWP_30cm),na.rm = T),
         SWP_medium=SWP_50cm)%>%
  dplyr::select(-c(SWP_20cm:SWP_90cm))%>%
  mutate(sitename="DE-Tha",DoY=yday(Date))

df.SWP_Dav_agg<-df.SWP_Dav %>%
  mutate(SWP_5cm=ifelse(SWP_5cm< -50,NA,SWP_5cm),
         SWP_15cm=ifelse(SWP_15cm< -50,NA,SWP_15cm),
         SWP_45cm=ifelse(SWP_45cm< -50,NA,SWP_45cm)
  )%>%
  group_by(Date)%>%
  mutate(SWP_shallow=rowMeans(across(SWP_5cm:SWP_15cm),na.rm = T),
         SWP_medium=SWP_45cm)%>%
  dplyr::select(-c(SWP_5cm:SWP_45cm))%>%
  mutate(sitename="CH-Dav",DoY=yday(Date))
#
df.SWP<-rbind(df.SWP_Tha_agg,df.SWP_Dav_agg)
#-------------------------
#(2)primary plotting: adding lines and rectangles in plot
#-------------------------
#-----------
#ploting making--1. making the delta GPP(GPP_mod-GPP_obs) for CH-Dav and DE-Tha
#-----------
##.adding overestimated periods of GPP in early spring:
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
THA_pheno<-round(colMeans(df_andPlot_DE_Tha$pos_agg,na.rm = T),0)

#-----------
#ploting making--1. making the Gc and delta Gc for CH-Dav and DE-Tha
#-----------
df_Gc_proc<-df_Gc%>%
  filter(sitename=="CH-Dav"|sitename=="DE-Tha")%>%
  dplyr::select(sitename,Date,ET,Gs_mol)%>%
  mutate(doy=yday(Date))%>%
  #remove the outliers
  filter(Gs_mol<1 & Gs_mol>0)
#calculate the mean and sd:
df_Gc_agg<-df_Gc_proc %>%
  group_by(sitename,doy)%>%
  summarise(ET_mean=mean(ET,na.rm = T),
            Gs_mean=mean(Gs_mol,na.rm = T),
            ET_sd=sd(ET,na.rm=T),
            Gs_sd=sd(Gs_mol,na.rm=T))
##
p_Gc_Dav<-df_Gc_agg%>%
  filter(sitename=="CH-Dav")%>%
  ggplot(aes(x=doy,y=Gs_mean,col=sitename))+
  geom_point()+
  xlim(0,250)+
  ylim(0,0.08)+
  # scale_y_break(c(0.04,0.07))+ #对y轴进行截断
  geom_smooth(span=0.3)+
  scale_color_manual(values = c("CH-Dav"=adjustcolor("red",0.3),
                                "DE-Tha"=adjustcolor("orange",0.3)))+
  # addding date: "2023-03-27"
  # geom_vline(xintercept = 86,col="black")+
  # annotate(geom="text",x=110,y=-50,
  #          label="Mar,27",col="black",size=6)+
  ylab(expression(" Gc"*" (mol m"^-2*"s"^-1*")"))+
  # xlab("")+
  xlab("DOY (Mutli-Years)")+
  # annotate(geom = "text",x=20,y=4,label="CH-Tha",size=6)+
  theme_classic()+
  theme(
    # legend.position="none",
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = "none",
    # legend.position = c(0.8,0.88),
    axis.text = element_text(size=20),
    axis.title = element_text(size=22)
  )
  # theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"))

p_Gc_Tha<-df_Gc_agg%>%
  filter(sitename=="DE-Tha")%>%
  ggplot(aes(x=doy,y=Gs_mean,col=sitename))+
  geom_point()+
  xlim(0,250)+
  ylim(0,0.08)+
  # scale_y_break(c(0.04,0.07))+ #对y轴进行截断
  geom_smooth(span=0.3)+
  scale_color_manual(values = c("CH-Dav"=adjustcolor("red",0.3),
                                "DE-Tha"=adjustcolor("orange",0.3)))+
  # addding date: "2023-03-27"
  # geom_vline(xintercept = 86,col="black")+
  # annotate(geom="text",x=110,y=-50,
  #          label="Mar,27",col="black",size=6)+
  ylab(expression(" Gc"*" (mol m"^-2*"s"^-1*")"))+
  # xlab("")+
  # labs(
  #   title = "DE-Tha"
  # )+
  xlab("DOY (Mutli-Years)")+
  # annotate(geom = "text",x=20,y=4,label="CH-Tha",size=6)+
  theme_classic()+
  theme(
    axis.text = element_text(size=20),
    axis.title = element_text(size=22),
    legend.position="none",
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16)
    # legend.position = c(0.8,0.88)
    )
  # theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"))

##calculate the delta Gc between two sites--Apirl, 2025:
  df_Gc_agg_Dav<-df_Gc_agg %>%
    filter(sitename=="CH-Dav")%>%
    dplyr::select(doy,Gs_mean,Gs_sd)%>%
    mutate("Dav_Gc_mean"=Gs_mean,"Dav_Gc_sd"=Gs_sd,
           Gs_mean=NULL,Gs_sd=NULL)
  df_Gc_agg_Tha<-df_Gc_agg %>%
    filter(sitename=="DE-Tha")%>%
    dplyr::select(doy,Gs_mean,Gs_sd)%>%
    mutate("Tha_Gc_mean"=Gs_mean,"Tha_Gc_sd"=Gs_sd,
           Gs_mean=NULL,Gs_sd=NULL)
  df_Gc_diff<-left_join(as.data.frame(df_Gc_agg_Dav)[,-1],
                        as.data.frame(df_Gc_agg_Tha)[,-1])%>%
    mutate(Gc_diff_mean=Dav_Gc_mean - Tha_Gc_mean,
           Gc_diff_sd=sqrt(Dav_Gc_sd^2 + Tha_Gc_sd^2))
  
  p_Gc_diff<-df_Gc_diff%>%
    ggplot(aes(x=doy,y=Gc_diff_mean,col="CH-Dav - DE-Tha"))+
    geom_point()+
    xlim(0,250)+
    ylim(-0.05,0.05)+
    # scale_y_break(c(0.04,0.07))+ #对y轴进行截断
    geom_smooth(span=0.3)+
    scale_color_manual(values = c("CH-Dav - DE-Tha"=adjustcolor("steelblue2",1)))+
    geom_hline(yintercept = 0,lty=2,size=1.1)+
    # addding date: "2023-03-27"
    # geom_vline(xintercept = 86,col="black")+
    # annotate(geom="text",x=110,y=-50,
    #          label="Mar,27",col="black",size=6)+
    ylab(expression(Delta*" Gc"*" (mol m"^-2*"s"^-1*")"))+
    # xlab("")+
    # labs(
    #   title = "CH-Dav — DE-Tha"
    # )+
    xlab("DOY (Mutli-Years)")+
    # annotate(geom = "text",x=20,y=4,label="CH-Tha",size=6)+
    theme_classic()+
    theme(
      legend.position="none",
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text = element_text(size=16),
      # legend.position = c(0.8,0.88),
      axis.text = element_text(size=20),
      axis.title = element_text(size=22)
    )
    # theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"))
# plot_grid(p_Gc_Dav,p_Gc_Tha,p_Gc_diff,nrow=1)
##sig.test:
  var_Eco_Gc<-as.numeric(unlist(df_Gc_diff%>%
                                 filter(doy>=60 & doy<121)%>%
                           dplyr::select(Gc_diff_mean)))
  norm_test<-ks.test(var_Eco_Gc,"pnorm")
  ifelse(norm_test$p.value>0.05,
         sig.diff<-t.test(var_Eco_Gc),sig.diff<-wilcox.test(var_Eco_Gc))
  sig_test_Eco_Gc<-data.frame("sig_Gc_pvalue"=round(sig.diff$p.value,3),
                       "sig_Gc_n"=length(var_Eco_Gc))
#-----------
#ploting making--2. making the soil water potential for CH-Dav and DE-Tha
#-----------
plot_fun_soilvars<-function(df,var_name,legend_flag){
  # df<-df.SWP
  # var_name<-"SWP_shallow"
  # legend_flag<-TRUE

  df.proc<-df %>%
    dplyr::select(sitename,Date,var_name,DoY)%>%
    mutate(Date=as.POSIXct(Date))
  names(df.proc)<-c("sitename","Date","var","DoY")

  df.use<-df.proc %>%
    group_by(sitename,DoY)%>%
    dplyr::summarise(var_mean=mean(var,na.rm=T),
                     var_sd=sd(var,na.rm=T))
  #
  df.use_Dav<-df.use %>%
    filter(sitename=="CH-Dav")%>%
    mutate(var_mean_Dav=var_mean,
           var_sd_Dav=var_sd,
           var_mean=NULL,var_sd=NULL
           )
  df.use_Tha<-df.use %>%
    filter(sitename=="DE-Tha")%>%
    mutate(var_mean_Tha=var_mean,
           var_sd_Tha=var_sd,
           var_mean=NULL,var_sd=NULL
           )
  df.use_diff<-left_join(df.use_Dav[,-1],df.use_Tha[,-1])%>%
    mutate(var_mean = var_mean_Dav - var_mean_Tha,
           var_sd = sqrt(var_sd_Dav ^ 2+ var_sd_Tha^2))%>%
    mutate(sitename="CH-Dav - DE-Tha")
  
  #For Year 2023:
  df.use_2023<-df.proc %>%
    dplyr::filter(Date>=as.Date("2023-01-01") & Date<=as.Date("2023-12-31"))
  #
  p_plot_Dav<-df.use %>%
    filter(sitename=="CH-Dav")%>%
    ggplot()+
    geom_ribbon(
      aes(x = DoY, ymin = var_mean - var_sd,
          ymax = var_mean + var_sd, fill=sitename),
      alpha = 0.2
    ) +
    # xlab("DOY (Multi-Years)")+
    xlab("")+
    geom_line(aes(x=DoY,y=var_mean,col=sitename),size=1.1)+
    scale_color_manual(values = c("CH-Dav"="brown1",
                                  "DE-Tha"="orange"))+
    scale_fill_manual(values = c("CH-Dav"="brown1",
                                 "DE-Tha"="orange"))+
    theme(legend.text = element_text(size = 20),
          legend.background = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size=16),
          axis.title = element_text(size=20))
    # #add the lines for 2023:
    # geom_line(aes(x=DoY,y=var,col=sitename),data=df.use_2023%>%
    #             filter(sitename=="CH-Dav"))
  p_plot_Tha<-df.use %>%
    filter(sitename=="DE-Tha")%>%
    ggplot()+
    geom_ribbon(
      aes(x = DoY, ymin = var_mean - var_sd,
          ymax = var_mean + var_sd, fill=sitename),
      alpha = 0.2
    ) +
    # xlab("DOY (Multi-Years)")+
    xlab("")+
    geom_line(aes(x=DoY,y=var_mean,col=sitename),size=1.1)+
    scale_color_manual(values = c("CH-Dav"="brown1",
                                  "DE-Tha"="orange"))+
    scale_fill_manual(values = c("CH-Dav"="brown1",
                                 "DE-Tha"="orange"))+
    theme(legend.text = element_text(size = 20),
          legend.background = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size=20),
          axis.title = element_text(size=22))+
    #add the lines for 2023:
    geom_line(aes(x=DoY,y=var,col=sitename),data=df.use_2023%>%
                filter(sitename=="DE-Tha"))
  #For diff:
  p_plot_diff<-df.use_diff %>%
    ggplot()+
    geom_ribbon(
      aes(x = DoY, ymin = var_mean - var_sd,
          ymax = var_mean + var_sd, fill=sitename),
      alpha = 0.2
    ) +
    xlab("")+
    # xlab("DOY (Multi-Years)")+
    geom_line(aes(x=DoY,y=var_mean,col=sitename),size=1.1)+
    scale_color_manual(values = c("CH-Dav - DE-Tha"="steelblue2"))+
    scale_fill_manual(values = c("CH-Dav - DE-Tha"="steelblue2"))+
    geom_hline(yintercept = 0,lty=2,size=1.1)+
    theme(legend.text = element_text(size = 20),
          legend.background = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size=20),
          axis.title = element_text(size=22))
  ###
  sig_test_Eco_SWP<-c()
  if(var_name %in% c("SWP_shallow","SWP_medium")){
    p_plot_Tha<-p_plot_Tha+
      xlim(0,250)+
      ylim(-70,0)+
      ylab(expression("SWP (KPa)"))+
      theme_classic()+
      theme(
        legend.position = c(0.8,0.9),
        legend.text = element_text(size = 20),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=22))
    p_plot_Dav<-p_plot_Dav+
      xlim(0,250)+
      ylim(-70,0)+
      ylab(expression("SWP (KPa)"))+
      theme_classic()+
      theme(
        legend.position = c(0.8,0.9),
        legend.text = element_text(size = 20),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=22))
    p_plot_diff<-p_plot_diff+
      xlim(0,250)+
      # ylim(0,750)+
      ylab(expression("SWP (KPa)"))+
      theme_classic()+
      theme(
        legend.position = c(0.8,0.9),
        legend.text = element_text(size = 20),
            legend.background = element_blank(),
            legend.title = element_blank(),
            axis.text = element_text(size=20),
            axis.title = element_text(size=22))
    ##sig.test:
    var_Eco_SWP<-as.numeric(unlist(df.use_diff%>%
                                    filter(DoY>=60 & DoY<121)%>%
                                    dplyr::select(var_mean)))
    norm_test<-ks.test(var_Eco_SWP,"pnorm")
    ifelse(norm_test$p.value>0.05,
           sig.diff<-t.test(var_Eco_SWP),sig.diff<-wilcox.test(var_Eco_SWP))
    sig_test_Eco_SWP<-data.frame("sig_SWP_pvalue"=round(sig.diff$p.value,3),
                                "sig_SWP_n"=length(var_Eco_SWP))
    
  }
  print(sig_test_Eco_SWP)
  p_plot_Dav<-p_plot_Dav+
    #adding at Mar, 2025: SWP-->field capacity
    geom_hline(yintercept = -33,lty=2,size=1.1)+
    theme(legend.text = element_text(size = 20),
          legend.background = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size=20),
          axis.title = element_text(size=22))
  p_plot_Tha<-p_plot_Tha+
    #adding at Mar, 2025: SWP-->field capacity
    geom_hline(yintercept = -33,lty=2,size=1.1)+
    theme(legend.text = element_text(size = 20),
          legend.background = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size=20),
          axis.title = element_text(size=22))
  ##
  if(legend_flag==FALSE){
    p_plot_Dav<-p_plot_Dav+theme(legend.position = "none")
    p_plot_Tha<-p_plot_Dav+theme(legend.position = "none")
    p_plot_diff<-p_plot_diff+theme(legend.position = "none")
  }
    p_plot<-list("Dav"=p_plot_Dav,"Tha"=p_plot_Tha,
                 "Dav - Tha"=p_plot_diff)
  return(p_plot)
}
#SWP_shallow:
p_SWP_shallow<-plot_fun_soilvars(df.SWP,"SWP_shallow",FALSE)
#filling manually:
sig_test_Eco_SWP_shallow<-data.frame("sig_SWP_pvalue"=0,
                             "sig_SWP_n"=61)
p_SWP_shallow_Dav<-p_SWP_shallow$Dav+
  ylab(expression("SWP"[upper]*" (KPa)"))+
  labs(title = "CH-Dav")+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"),
        legend.position = "none")

p_SWP_shallow_Tha<-p_SWP_shallow$Tha+
  ylab(expression("SWP"[upper]*" (KPa)"))+
  labs(title = "DE-Tha")+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"),
        legend.position = "none")
p_SWP_shallow_diff<-p_SWP_shallow$`Dav - Tha`+
  ylab(expression("SWP"[upper]*" (KPa)"))+
  labs(title = "CH-Dav - DE-Tha")+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"),
        legend.position = "none")
plot_grid(p_SWP_shallow_Dav,p_SWP_shallow_Tha,p_SWP_shallow_diff,nrow=1)

#SWP_medium:
p_SWP_medium<-plot_fun_soilvars(df.SWP,"SWP_medium",TRUE)
#filling manually:
sig_test_Eco_SWP_medium<-data.frame("sig_SWP_pvalue"=0.008,
                                     "sig_SWP_n"=61)
#
p_SWP_medium_Dav<-p_SWP_medium$Dav+
  ylab(expression("SWP"[deeper]*" (KPa)"))+
  labs(title = "CH-Dav")+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"),
        legend.position = "none")
p_SWP_medium_Tha<-p_SWP_medium$Tha+
  ylab(expression("SWP"[deeper]*" (KPa)"))+
  labs(title = "DE-Tha")+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"),
        legend.position = "none")
p_SWP_medium_diff<-p_SWP_medium$`Dav - Tha`+
  ylab(expression("SWP"[deeper]*" (KPa)"))+
  labs(title = "CH-Dav - DE-Tha")+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"),
        legend.position = "none"
        )
# plot_grid(p_SWP_medium_Dav,p_SWP_medium_Tha,p_SWP_medium_diff,nrow=1)

#-----------
#ploting making--2. making the original leaf physio for CH-Dav and DE-Tha
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

# Gs and E:
df_hydro<-df.leaf_physio$df.gs_E
p_gs_Date<-plot_ori_meansd_fun(df_hydro,"Gs",
                           c(0.05,0.85),
                           data.frame(x=86,xend=86,y=1.5e-05,yend=1e-05),
                           TRUE)
p_gs_Date_Dav<-p_gs_Date$Dav+
  labs(
    # x="DOY (2023)",
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" g"[s] ~ "(" * mol ~ m^{-2} ~ s^{-1} * ")")
  )+
  ylim(-1e-05,4e-05)+
  annotate(geom = "text",x=25,y=0.9,label="CH-Dav",size=6)+
  theme(
    legend.position = c(0.85,0.3),
    panel.spacing = unit(0.05, "lines"))+
  annotate(geom = "text",x=yday(Dav_sample_date[1:6]),y=rep(-1e-05,6),
         label=paste0("C",1:6),size=6)
p_gs_Date_Tha<-p_gs_Date$Tha+
  labs(
    # x="DOY (2023)",
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(" g"[s] ~ "(" * mol ~ m^{-2} ~ s^{-1} * ")")
  )+
  ylim(-1e-05,4e-05)+
  annotate(geom = "text",x=25,y=0.9,label="CH-Dav",size=6)+
  theme(
    legend.position = c(0.85,0.3),
    panel.spacing = unit(0.05, "lines"))+
  annotate(geom = "text",x=yday(Tha_sample_date[1:6]),y=rep(-1e-05,6),
           label=paste0("C",1:6),size=6)
# plot_grid(p_gs_Date_Dav,p_gs_Date_Tha)

##-------------
#water potential
##-------------
#convert the unit: 
df.WP<-df.WP %>%
  mutate(WP_Branch=WP_Branch/10,
         WP_Twig=WP_Twig/10,
         WP_Trunk=WP_Trunk/10)
#Twig:
p_WP_Twig_Date<-plot_ori_meansd_fun(df.WP,"WP_Twig",c(0.1,0.85),
                                data.frame(x=86,xend=86,y=-2,yend=-2.5),
                                TRUE)

p_WP_Twig_Date_Dav<-p_WP_Twig_Date$Dav+
  labs(
    x="",
    # x="DOY (2023)",
    y = expression(psi[Twig]*" (MPa)")
  )+
  ylim(-4,0)+
  theme(
    legend.position = "none",
    panel.spacing = unit(0.05, "lines"))
  # annotate(geom = "text",x=yday(Dav_sample_date[1:6]),y=rep(-4,6),
  #          label=paste0("C",1:6),size=6)
  # annotate(geom = "text",x=25,y=-0.2,label="CH-Dav",size=6)
  
p_WP_Twig_Date_Tha<-p_WP_Twig_Date$Tha+
  labs(
    x="",
    # x="DOY (2023)",
    y = expression(psi[Twig]*" (MPa)")
  )+
  ylim(-4,0)+
  theme(
    legend.position = "none",
    panel.spacing = unit(0.05, "lines"))
  # annotate(geom = "text",x=yday(Tha_sample_date[1:6]),y=rep(-4,6),
  #          label=paste0("C",1:6),size=6)
  # annotate(geom = "text",x=25,y=-0.2,label="DE-Tha",size=6)
# plot_grid(p_WP_Twig_Date_Dav,p_WP_Twig_Date_Tha)

#Branch:
p_WP_Branch_Date<-plot_ori_meansd_fun(df.WP,"WP_Branch",c(0.1,0.85),
                                    data.frame(x=86,xend=86,y=-2,yend=-2.5),
                                    TRUE)
p_WP_Branch_Date_Dav<-p_WP_Branch_Date$Dav+
  labs(
    # x="",
    x="DOY (2023)",
    y = expression(psi[Branch]*" (MPa)")
  )+
  ylim(-4,0)+
  theme(
    legend.position = "none",
    panel.spacing = unit(0.05, "lines"))
  # annotate(geom = "text",x=yday(Dav_sample_date[1:6]),y=rep(-4,6),
  #          label=paste0("C",1:6),size=6)

p_WP_Branch_Date_Tha<-p_WP_Branch_Date$Tha+
  labs(
    # x="",
    x="DOY (2023)",
    y = expression(psi[Branch]*" (MPa)")
  )+
  ylim(-4,0)+
  theme(
    legend.position = "none",
    panel.spacing = unit(0.05, "lines"))
  # annotate(geom = "text",x=yday(Tha_sample_date[1:6]),y=rep(-4,6),
  #          label=paste0("C",1:6),size=6)
# plot_grid(p_WP_Branch_Date_Dav,p_WP_Branch_Date_Tha)

#-----------
#ploting making--3. making the delta leaf gs for CH-Dav and DE-Tha
#-----------
plot_meansd_fun<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.leaf_physio$df.gs_E
  # var_name<-"Gs"
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
  #
  sig_test<-c()
  sig_test_noC2<-c()
  if(var_name %in% c("Fv.Fm","NPQ","qN.qP","Gs","WP_Twig","WP_Branch")){
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
    #
    var_Lower<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
                                   #select DOY<140--C1-C4
                                   filter(Position=="Lower" & diff_Date<140)%>%dplyr::select(diff_y)))
    var_Upper<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
                                   filter(Position=="Upper"& diff_Date<140)%>%dplyr::select(diff_y)))
    var_Lower_noC2<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
                                   #select DOY<140--C1-C4 and C2
                                   filter(CampaignNum!="C2")%>%
                                   filter(Position=="Lower" & diff_Date<140)%>%dplyr::select(diff_y)))
    var_Upper_noC2<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
                                   filter(CampaignNum!="C2")%>%
                                   filter(Position=="Upper"& diff_Date<140)%>%dplyr::select(diff_y)))
    #
    norm_test_Lower<-ks.test(var_Lower,"pnorm")
    norm_test_Upper<-ks.test(var_Upper,"pnorm")
    norm_test_Lower_noC2<-ks.test(var_Lower_noC2,"pnorm")
    norm_test_Upper_noC2<-ks.test(var_Upper_noC2,"pnorm")
    
    ifelse(norm_test_Lower$p.value>0.05,
           sig.Lower<-t.test(var_Lower),sig.Lower<-wilcox.test(var_Lower))
    ifelse(norm_test_Upper$p.value>0.05,
           sig.Upper<-t.test(var_Upper),sig.Upper<-wilcox.test(var_Upper))
    ifelse(norm_test_Lower_noC2$p.value>0.05,
           sig.Lower_noC2<-t.test(var_Lower_noC2),sig.Lower_noC2<-wilcox.test(var_Lower_noC2))
    ifelse(norm_test_Upper_noC2$p.value>0.05,
           sig.Upper_noC2<-t.test(var_Upper_noC2),sig.Upper_noC2<-wilcox.test(var_Upper_noC2))
    #
    sig_test<-data.frame("sig_Lower_pvalue"=round(sig.Lower$p.value,3),"sig_Lower_n"=length(var_Lower),
      "sig_Upper_pvalue"=round(sig.Upper$p.value,3),"sig_Upper_n"=length(var_Upper),
      "sig_Lower_noC2_pvalue"=round(sig.Lower_noC2$p.value,3),"sig_Lower_noC2_n"=length(var_Lower_noC2),
      "sig_Upper_noC2_pvalue"=round(sig.Upper_noC2$p.value,3),"sig_Upper_noC2_n"=length(var_Upper_noC2)
    )
  }
  
  ##
  if(!c(var_name %in% c("Fv.Fm","NPQ","qN.qP","Gs","WP_Twig","WP_Branch"))){
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
    norm_test_Lower<-shapiro.test(var_Lower)
    norm_test_Middle<-shapiro.test(var_Middle)
    norm_test_Upper<-shapiro.test(var_Upper)
    
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
    xlab("DOY (2023)")+
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

# Gs and E:
df_hydro<-df.leaf_physio$df.gs_E
p_gs_Date_diff<-plot_meansd_fun(df_hydro,"Gs",
                                 c(0.85,0.3),
                                 data.frame(x=86,xend=86,y=1.5e-05,yend=0.5e-05),
                                 TRUE)+
  ylim(-2.5e-05,2e-05)+
  labs(
    x="",
    # x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" g"[s] ~ "(" * mol ~ m^{-2} ~ s^{-1} * ")")
  )+
  theme(legend.position = c(0.85,0.3))+
  annotate(geom = "text",x=c(64),y=rep(-2.5e-05),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-2.5e-05),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-2.5e-05),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-2.5e-05),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-2.5e-05),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-2.5e-05),label=c("C6"),size=6)
#
# plot_grid(p_gs_Date_Dav,p_gs_Date_Tha,p_gs_Date_diff,nrow=1)

#-#filling manually:
sig_test_gs<-data.frame("sig_Lower_pvalue"=0.032,"sig_Lower_n"=4458,
    "sig_Upper_pvalue"=0,"sig_Upper_n"=4981,
    "sig_Lower_noC2_pvalue"=0,"sig_Lower_noC2_n"=3093,
    "sig_Upper_noC2_pvalue"=0,"sig_Upper_noC2_n"=3616
                        )
#----------
#ploting making--4. making the delta water potential for CH-Dav and DE-Tha
#-----------

#Twig:
p_WP_Twig_Date_diff<-plot_meansd_fun(df.WP,"WP_Twig",c(0.1,0.85),
                                  data.frame(x=86,xend=86,y=0,yend=-1),
                                  TRUE)+
  labs(
    x="",
    # x="DOY (2023)",
    y = expression(Delta*psi[Twig]*" (MPa)")
  )+
  ylim(-2.5,1.5)+
  theme(legend.position = "none")
  # annotate(geom = "text",x=200,y=1.3,label="CH-Dav - DE-Tha",size=6)
  #adding Campaign Nr.
  # annotate(geom = "text",x=c(64),y=rep(-2.5),label=c("C1"),size=6)+
  # annotate(geom = "text",x=c(84),y=rep(-2.5),label=c("C2"),size=6)+
  # annotate(geom = "text",x=c(107),y=rep(-2.5),label=c("C3"),size=6)+
  # annotate(geom = "text",x=c(120),y=rep(-2.5),label=c("C4"),size=6)+
  # annotate(geom = "text",x=c(140),y=rep(-2.5),label=c("C5"),size=6)+
  # annotate(geom = "text",x=c(196),y=rep(-2.5),label=c("C6"),size=6)
#-#filling manually:
sig_test_WP_Twig<-data.frame("sig_Lower_pvalue"=0.012,"sig_Lower_n"=100,
     "sig_Upper_pvalue"=0.001,"sig_Upper_n"=100,
     "sig_Lower_noC2_pvalue"=0.037,"sig_Lower_noC2_n"=75,
     "sig_Upper_noC2_pvalue"=0.344,"sig_Upper_noC2_n"=75
     
)

#Branch
p_WP_Branch_Date_diff<-plot_meansd_fun(df.WP,"WP_Branch",c(0.1,0.85),
                                        data.frame(x=86,xend=86,y=0,yend=-1),
                                        TRUE)+
  labs(
    # x="",
    x="DOY (2023)",
    y = expression(Delta*psi[branch]*" (MPa)")
  )+
  ylim(-2,1.5)+
  theme(legend.position = "none")
  #adding Campaign Nr.
  # annotate(geom = "text",x=c(64),y=rep(-2),label=c("C1"),size=6)+
  # annotate(geom = "text",x=c(84),y=rep(-2),label=c("C2"),size=6)+
  # annotate(geom = "text",x=c(107),y=rep(-2),label=c("C3"),size=6)+
  # annotate(geom = "text",x=c(120),y=rep(-2),label=c("C4"),size=6)+
  # annotate(geom = "text",x=c(140),y=rep(-2),label=c("C5"),size=6)+
  # annotate(geom = "text",x=c(196),y=rep(-2),label=c("C6"),size=6)

#
# plot_grid(p_WP_Branch_Date_Dav,p_WP_Branch_Date_Tha,p_WP_Branch_Date_diff,nrow=1)
# plot_grid(p_WP_Twig_Date_Dav,p_WP_Twig_Date_Tha,p_WP_Twig_Date_diff,nrow=1)
#-#filling manually:
sig_test_WP_Branch<-data.frame("sig_Lower_pvalue"=0.044,"sig_Lower_n"=100,
                             "sig_Upper_pvalue"=0.028,"sig_Upper_n"=100,
    "sig_Lower_noC2_pvalue"=0.007,"sig_Lower_noC2_n"=75,
    "sig_Upper_noC2_pvalue"=0.011,"sig_Upper_noC2_n"=75
                             
)
#-------------------------
#(3)further plotting: adding lines and rectanges in plot
#-------------------------
#period:[sos-60,sos+60] is set 
#adding the sos
#for CH-Dav
rect.coord_isevent=data.frame(x1=Dav_pheno[1]-60,
                              x2=Dav_pheno[1],
                              x3=Dav_pheno[1]+60,
                              y1=-Inf, 
                              y2=Inf)
#1. for DSPR period for Gc:
p_Gc_Dav<-p_Gc_Dav+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=0,label="sos",col="forestgreen",size=6)
p_Gc_Tha<-p_Gc_Tha+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=0,label="sos",col="forestgreen",size=6)
p_Gc_diff<-p_Gc_diff+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=0,label="sos",col="forestgreen",size=6)
##add sig. symbol:
sig_test_Eco_Gc
p_Gc_diff<-p_Gc_diff +
  annotate("text",x=100,y=0.045,label=c("***"),
           size=10)

#2. for DSPR period for SWP:
p_SWP_shallow_Dav<-p_SWP_shallow_Dav+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-50,label="sos",col="forestgreen",size=6)
p_SWP_shallow_Tha<-p_SWP_shallow_Tha+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-50,label="sos",col="forestgreen",size=6)
p_SWP_shallow_diff<-p_SWP_shallow_diff+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-50,label="sos",col="forestgreen",size=6)
##add sig. symbol:
sig_test_Eco_SWP_shallow
p_SWP_shallow_diff<-p_SWP_shallow_diff +
  annotate("text",x=100,y=45,label=c("***"),
           size=10)

##############
p_SWP_medium_Dav<-p_SWP_medium_Dav+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-60,label="sos",col="forestgreen",size=6)
p_SWP_medium_Tha<-p_SWP_medium_Tha+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-60,label="sos",col="forestgreen",size=6)
p_SWP_medium_diff<-p_SWP_medium_diff+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-60,label="sos",col="forestgreen",size=6)

##add sig. symbol:
sig_test_Eco_SWP_medium
p_SWP_medium_diff<-p_SWP_medium_diff +
  annotate("text",x=100,y=45,label=c("**"),
           size=10)

#3. for DSPR period for gs:
p_gs_Date_Dav<-p_gs_Date_Dav+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-0.2e-05,label="sos",col="forestgreen",size=6)
p_gs_Date_Tha<-p_gs_Date_Tha+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-0.2e-05,label="sos",col="forestgreen",size=6)
p_gs_Date_diff<-p_gs_Date_diff+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-1.85e-05,label="sos",col="forestgreen",size=6)
##add sig. symbol:
sig_test_gs
p_gs_Date_diff<-p_gs_Date_diff +
  annotate("text",x=rep(100,2),y=c(1.8e-05,1.5e-05),label=c("*  ***","***  ***"),
           size=10,col=c(adjustcolor("blue",0.9),adjustcolor("steelblue2",0.9)))

#4. for DSPR period for Water potential:
p_WP_Twig_Date_Dav<-p_WP_Twig_Date_Dav+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y= -3.7,label="sos",col="forestgreen",size=6)
p_WP_Twig_Date_Tha<-p_WP_Twig_Date_Tha+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y= -3.7,label="sos",col="forestgreen",size=6)
p_WP_Twig_Date_diff<-p_WP_Twig_Date_diff+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y= -2,label="sos",col="forestgreen",size=6)
##add sig. symbol:
sig_test_WP_Twig
p_WP_Twig_Date_diff<-p_WP_Twig_Date_diff +
  annotate("text",x=c(100,100),y=c(1.4,1),label=c("*  *","***  ns"),
           size=10,col=c(adjustcolor("blue",0.9),adjustcolor("steelblue2",0.9)))

p_WP_Branch_Date_Dav<-p_WP_Branch_Date_Dav+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y= -3.7,label="sos",col="forestgreen",size=6)
p_WP_Branch_Date_Tha<-p_WP_Branch_Date_Tha+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y= -3.7,label="sos",col="forestgreen",size=6)
p_WP_Branch_Date_diff<-p_WP_Branch_Date_diff+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y= -1.7,label="sos",col="forestgreen",size=6)
##add sig. symbol:
sig_test_WP_Branch
p_WP_Branch_Date_diff<-p_WP_Branch_Date_diff +
  annotate("text",x=c(100,100),y=c(1.4,1),label=c("*  **","*  *"),
           size=10,col=c(adjustcolor("blue",0.9),adjustcolor("steelblue2",0.9)))
#-------------------------
#(4)merge the plots:
#-------------------------
p_merge<-plot_grid(
  p_SWP_shallow_Dav,p_SWP_shallow_Tha,p_SWP_shallow_diff,
  p_Gc_Dav,p_Gc_Tha,p_Gc_diff,
  p_gs_Date_Dav,p_gs_Date_Tha,p_gs_Date_diff,
  p_WP_Twig_Date_Dav,p_WP_Twig_Date_Tha,p_WP_Twig_Date_diff,
  p_WP_Branch_Date_Dav,p_WP_Branch_Date_Tha,p_WP_Branch_Date_diff,
  ncol = 3, nrow=5,
  align = "hv",
  labels = c("(a)","(b)","(c)",
             "(d)","(e)","(f)",
             "(g)","(h)","(i)",
             "(j)","(k)","(l)",
             "(m)","(n)","(o)"
             ),
  label_size = 20,
  axis="tblr", #very important-->按轴去对齐
  # rel_heights = c(0.19,0.19,0.19,0.22,0.2)
  theme(plot.margin = margin(0,0,0,0),
        panel.spacing = unit(0,"lines"))
)
#
p_merge_SWP_deeper<-plot_grid(
  p_SWP_medium_Dav,p_SWP_medium_Tha,p_SWP_medium_diff,
  align = "hv",
  labels = c("(a)","(b)","(c)"
  ),
  label_size = 20,
  nrow=1)

##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig4_main.png"),p_merge,width = 22,height=20)
#
ggsave(paste0(save.path,"Fig4_supp_SWP_deeper.png"),
       p_merge_SWP_deeper,width = 20,height=5)

#-----------
#(5) making the sap flow for CH-Dav 
#-----------
#----------------------
#for the stand sapflow:
#-----------------------
df.SF_stand<-df.SF$df.stand
p_SF_stand<-df.SF_stand%>%
  ggplot(aes(x=doy,sap_m_adj,col=Year))+
  geom_point()+
  geom_point(aes(x=doy,y=sap_m_adj),
             col=adjustcolor("#440154",0.3),size=5,
             data = df.SF_stand %>% filter(Date==as.Date("2023-03-27")))+
  geom_smooth(span=0.2,se=FALSE)+
  scale_color_viridis_d(option = "D",direction = -1)+
  # addding date: "2023-03-27"
  geom_vline(xintercept = 86,col="#440154")+
  annotate(geom="text",x=110,y=0,
           label="Mar,27",col="#440154",size=6)+
  ylab(expression("SF"[Stand]*" (mm d"^-1*")"))+
  xlim(0,250)+
  ylim(-0.5,4.5)+
  xlab("DOY")+
  annotate(geom = "text",x=20,y=4.5,label="CH-Dav",size=6)+
  theme_classic()+
  theme(legend.text = element_text(size = 14),
        legend.title=element_blank(),
        legend.background = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=22),
        legend.position = c(0.1,0.85)
  )

#----------------------
#for different trees:
#-----------------------
df.SFD_eachtree<-df.SF$df.eachtree
df.SFD_eachtree_mean2023<-df.SFD_eachtree %>%
  filter(Year==2023)%>%
  group_by(doy)%>%
  summarise(SFD_mean=mean(SFD,na.rm = T),
            SFD_sd=sd(SFD,na.rm=T))

p_SFD_eachtrees<-ggplot()+
  # geom_point(aes(x=doy,y=SFD),col=adjustcolor("grey50",1),
  #            data=df.SFD_eachtree%>%filter(Year==2023))+
  # geom_ribbon(aes(x=doy,ymin=SFD_mean-SFD_sd,ymax=SFD_mean+SFD_sd),
  #             data=df.SFD_eachtree_mean2023,
  #             fill="gray",size=1.5)+
  stat_smooth(aes(x=doy,y=SFD,
                  col=factor(Tree_ID)),span=0.2,
              data=df.SFD_eachtree%>%filter(Year==2023),
              se=FALSE)+
  scale_color_viridis_d()+
  stat_smooth(aes(x=doy,y=SFD_mean),
              data=df.SFD_eachtree_mean2023,span = 0.2,
              col=adjustcolor("black",0.8),size=2,se=TRUE)+
  # addding date: "2023-03-27"
  geom_vline(xintercept = 86,col="black")+
  annotate(geom="text",x=110,y=0,
           label="Mar,27",col="black",size=6)+
  xlim(0,250)+
  ylim(-0.5,4.5)+
  annotate(geom = "text",x=25,y=4.5,label="CH-Dav",size=6)+
  # annotate(geom = "segment",x=180,xend=190,y = 2,yend = 2,col="red",size=1.5)+
  annotate(geom = "segment",x=180,xend=190,y = 0,yend = 0,col="black",size=1.5)+
  annotate(geom = "text",x=220,y=0,label=c("Mean"),size=5)+
  ylab(expression("SFD (cm h"^-1*")"))+
  xlab("DOY (2023)")+
  labs(color ="Trees")+
  theme_classic()+
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size = 16),
        legend.position ="none"
  )
#load the TWD data:
p_TWD.path<-"./data/Comprehensive_plot_data/Fig4/"
load(file = paste0(p_TWD.path,"p_TWD_Trees2023.RDA"))
###update in Oct, 2025:adding the DSPR period:
p_SF_stand<-p_SF_stand+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)
p_SFD_eachtrees<-p_SFD_eachtrees+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)
p_TWD_eachtrees_2023<-p_TWD_eachtrees_2023+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)

p_merge_SF<-plot_grid(p_SF_stand,p_SFD_eachtrees,
                      p_TWD_eachtrees_2023,
                      align = "hv",
                      labels = c("(a)","(b)","(c)"),
                      label_size = 20,
                      nrow=1)
#
ggsave(paste0(save.path,"Fig4_supp_SF.png"),
       p_merge_SF,width = 18,height=7)

