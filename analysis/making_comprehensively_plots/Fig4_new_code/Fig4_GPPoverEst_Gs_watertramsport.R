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
###doy of mean and sd
df_meandoy <- df.merge %>%
  mutate(doy=yday(date),gpp_res=gpp_mod-gpp_obs)%>%
  group_by(sitename, doy) %>%
  dplyr::summarise(across(starts_with("gpp_"), mean, na.rm = TRUE))
df_sddoy<-df.merge%>%
  mutate(doy=yday(date),gpp_res=gpp_mod-gpp_obs)%>%
  group_by(sitename, doy) %>%
  summarise(gpp_obs_sd=sd(gpp_obs),
            gpp_mod_sd=sd(gpp_mod),
            gpp_res_sd=sd(gpp_res))
df_GPP<-left_join(df_meandoy,df_sddoy)

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
##1a.
df_plot_2sites<-df_GPP%>%
  filter(sitename=="CH-Dav"|sitename=="DE-Tha")

p_GPP_plot<-df_plot_2sites %>%
  ggplot()+
  geom_line(aes(x=doy,y=gpp_res,col=sitename),size=1.1)+
  xlim(0,250)+
  # ylim(-1,3)+
  labs(y = expression( paste(Delta*"GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "")+
  # geom_ribbon(
  #   aes(x = doy, ymin = gpp_res - gpp_res_sd, 
  #       ymax = gpp_res + gpp_res_sd, fill=sitename),
  #   alpha = 0.2
  # ) +
  scale_color_manual(values = c("CH-Dav"="brown1",
                                "DE-Tha"="orange"))+
  # scale_fill_manual(values = c("CH-Dav"="brown1",
  #                              "DE-Tha"="orange"))+
  geom_hline(yintercept = 0,lty=2,size=1.1)+
  theme_light()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = c(0.8,0.88),
    axis.text = element_text(size=16),
    axis.title = element_text(size=20))
##1b.adding overestimated periods of GPP in early spring:
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
#ploting making--2. making the delta Gc for CH-Dav and DE-Tha
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
p_Gc<-df_Gc_agg%>%
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
  ylab(expression(Delta*" Gc"*" (mol m"^-2*"s"^-1*")"))+
  xlab("DOY (Mutli-Years)")+
  # annotate(geom = "text",x=20,y=4,label="CH-Tha",size=6)+
  theme_light()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = c(0.8,0.88),
    # legend.text = element_text(size = 16),
    # legend.title = element_text(size=18),
    # legend.background = element_blank(),
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank(),
    axis.text = element_text(size=16),
    axis.title = element_text(size=20)
  )

#-----------
#ploting making--3. making the delta leaf gs for CH-Dav and DE-Tha
#-----------
plot_meansd_fun<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.leaf_physio$df_traits
  # var_name<-"SLA"
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
  df_DAV<-df%>%
    filter(sitename=="DAV")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)%>%
    ##summary the data
    group_by(sitename,Date,CampaignNum,Position)%>%
    summarise(y_mean=mean(y,na.rm = T),
              y_sd=sd(y,na.rm=T))
  #using data from Dav - Tha
  df_retidy<-left_join(df_THA,df_DAV,by=c("CampaignNum","Position"))%>%
    mutate(diff_y_mean=y_mean.y - y_mean.x,
           diff_y_sd=sqrt(y_sd.x^2+y_sd.y^2),
           diff_Date=round(as.numeric(c(yday(Date.x) + yday(Date.y))/2)))%>%
    dplyr::select(sitename.x,diff_Date,CampaignNum,Position,diff_y_mean,diff_y_sd)%>%
    mutate(sitename.x=NULL)
  
  if(var_name %in% c("Fv.Fm","NPQ","qN.qP")){
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
  }
  if(!c(var_name %in% c("Fv.Fm","NPQ","qN.qP"))){
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
  }
  
  p_var_Date<-p_var_Date+
    # labs(fill = "Sitename")+
    xlab("DOY (2023)")+
    xlim(0,250)+
    #adding the 0:
    geom_hline(yintercept = 0,lty=2)+
    ##separate the campaigns-adding in Jan, 2024
    # geom_vline(xintercept = c(2.5,4.5,6.5,8.5,10.5),lty=2)+
    theme_light()+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=20),
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
p_gs_Date<-plot_meansd_fun(df_hydro,"Gs",
                                 c(0.05,0.85),
                                 data.frame(x=84,xend=84,y=1.5e-05,yend=0.5e-05),
                                 TRUE)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" g"[s] ~ "(" * mol ~ m^{-2} ~ s^{-1} * ")")
  )+
  theme(legend.position = c(0.1,0.4))+
  annotate(geom = "text",x=c(64),y=rep(-2e-05),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-2e-05),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-2e-05),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-2e-05),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-2e-05),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-2e-05),label=c("C6"),size=6)

# p_E_Date<-plot_meansd_fun(df_hydro,"E",c(0.05,0.9),
#                                 data.frame(x=84,xend=84,y=3e-05,yend=1.5e-05),
#                                 FALSE)+
#   labs(
#     x="",
#     # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
#     y = expression("T (mol" ~ m^{-2} ~ s^{-1} * ")")
#   )
# #additionally:
# p_gws_Date<-plot_meansd_fun(df_hydro,"gsw",c(0.1,0.9),
#                                   data.frame(x=4,xend=4,y=3e-05,yend=1.5e-05),
#                                   FALSE)+
#   labs(
#     x="",
#     # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
#     y = expression(gsw ~ "(" * mol ~ m^{-2} ~ s^{-1} * ")")
#   )+
#   theme(legend.position = "none")

#-----------
#ploting making--4. making the delta water potential for CH-Dav and DE-Tha
#-----------
#Twig:
p_WP_Twig_Date<-plot_meansd_fun(df.WP,"WP_Twig",c(0.1,0.85),
                                  data.frame(x=84,xend=84,y=-5,yend=-10),
                                  TRUE)+
  labs(
    x="DOY (2023)",
    y = expression(Delta*psi[Twig]*" (bar)")
  )+
  theme(legend.position = c(0.1,0.4))

#Branch
p_WP_Branch_Date<-plot_meansd_fun(df.WP,"WP_Branch",c(0.1,0.85),
                                        data.frame(x=84,xend=84,y=-5,yend=-10),
                                        TRUE)+
  labs(
    x="DOY (2023)",
    y = expression(Delta*psi[branch]*" (bar)")
  )+
  theme(legend.position = c(0.1,0.4))+
  #adding Campaign Nr.
  annotate(geom = "text",x=c(64),y=rep(-20),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-20),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-20),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-20),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-20),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-20),label=c("C6"),size=6)

#-----------
#ploting making--5. making the sap flow for CH-Dav 
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
  ylim(-0.5,4)+
  xlab("DOY")+
  annotate(geom = "text",x=20,y=4,label="CH-Dav",size=6)+
  theme_light()+
  theme(legend.text = element_text(size = 14),
        legend.title=element_blank(),
        legend.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20),
        legend.position = c(0.1,0.8)
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
  ylim(-0.5,5)+
  annotate(geom = "text",x=25,y=5,label="CH-Dav",size=6)+
  # annotate(geom = "segment",x=180,xend=190,y = 2,yend = 2,col="red",size=1.5)+
  annotate(geom = "segment",x=180,xend=190,y = 0,yend = 0,col="black",size=1.5)+
  annotate(geom = "text",x=220,y=0,label=c("Mean"),size=5)+
  ylab(expression("SFD (cm h"^-1*")"))+
  xlab("DOY (2023)")+
  labs(color ="Trees")+
  theme_light()+
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size = 16),
        legend.position ="none"
  )

#-----------
#ploting making--6. making the soil moisture for CH-Dav 
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
  #For Year 2023:
  df.use_2023<-df.proc %>%
    dplyr::filter(Date>=as.Date("2023-01-01") & Date<=as.Date("2023-12-31"))
  #
  p_plot<-df.use %>%
    ggplot()+
    geom_ribbon(
      aes(x = DoY, ymin = var_mean - var_sd, 
          ymax = var_mean + var_sd, fill=sitename),
      alpha = 0.2
    ) +
    xlab("DOY (Multi-Years)")+
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
  #add the lines for 2023:
  p_plot<-p_plot+
    geom_line(aes(x=DoY,y=var,col=sitename),data=df.use_2023)
  
  if(var_name %in% c("SWP_shallow","SWP_medium")){
    p_plot<-p_plot+
      xlim(0,250)+
      # ylim(0,750)+
      ylab(expression("SWP (KPa)"))+
      theme_light()+
      theme(legend.position = c(0.8,0.9))
  }
  p_plot<-p_plot+
    #adding at Mar, 2025: SWP-->field capacity
    geom_hline(yintercept = -33,lty=2,size=1.1)+
    theme(legend.text = element_text(size = 20),
          legend.background = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size=16),
          axis.title = element_text(size=20))
  if(legend_flag==FALSE){
    p_plot<-p_plot+theme(legend.position = "none")
  }
  
  return(p_plot)
}
#
p_SWP_shallow<-plot_fun_soilvars(df.SWP,"SWP_shallow",TRUE)+
  ylab(expression("SWP"[shallow]*" (KPa)"))
p_SWP_medium<-plot_fun_soilvars(df.SWP,"SWP_medium",FALSE)+
  ylab(expression("SWP"[medium]*" (KPa)"))

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
#1. for DSPR period for GPP:
p_plot_deltaGPP_2sites<-p_GPP_plot+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-0.5,label="sos",col="forestgreen",size=6)

#2. for DSPR period for Gc:
p_plot_Gc_2sites<-p_Gc+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=0,label="sos",col="forestgreen",size=6)

#3. for DSPR period for SWP:
p_SWP_shallow<-p_SWP_shallow+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-60,label="sos",col="forestgreen",size=6)

p_SWP_medium<-p_SWP_medium+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-80,label="sos",col="forestgreen",size=6)

#-------------------------
#(4)merge the plots:
#-------------------------
p_merge<-plot_grid(
  p_plot_deltaGPP_2sites,p_WP_Twig_Date,p_WP_Branch_Date,
  p_plot_Gc_2sites,p_SFD_eachtrees,p_SF_stand,
  p_gs_Date,p_SWP_shallow,p_SWP_medium,
  ncol = 3, nrow=3,
  align = "hv",
  labels = c("(a)","(d)","(e)",
  "(b)","(f)","(g)",
  "(c)","(h)","(i)"),
  label_size = 22,
  axis="tblr", #very important-->按轴去对齐
  # rel_heights = c(0.19,0.19,0.19,0.22,0.2)
  theme(plot.margin = margin(0,0,0,0),
        panel.spacing = unit(0,"lines"))
  )


##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig4_new.png"),p_merge,width = 22,height=15)



