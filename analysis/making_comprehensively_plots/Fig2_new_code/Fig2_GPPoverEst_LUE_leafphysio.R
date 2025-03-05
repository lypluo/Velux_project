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
#load the LUE tidy before-->adopt from Fig2_supp1_04_Eco_LUE
#---------
df_LUE<-readRDS(paste0("./data/Comprehensive_plot_data/Fig2/","LUE_2sites.RDS"))

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
#ploting making--2. making the delta LUE for CH-Dav and DE-Tha
#-----------
df_LUE_Dav<-df_LUE%>%
  filter(Sitename=="CH-Dav")%>%
  dplyr::select(Date,LUE)%>%
  mutate(LUE_Dav=LUE,LUE=NULL)
df_LUE_Tha<-df_LUE%>%
  filter(Sitename=="DE-Tha")%>%
  dplyr::select(Date,LUE)%>%
  mutate(LUE_Tha=LUE,LUE=NULL)
df_delta_LUE<-left_join(df_LUE_Dav,df_LUE_Tha)%>%
  mutate(LUE_diff=LUE_Tha - LUE_Dav,
         doy=yday(Date))
#using the df_LUE at the end for better demonstration:
df_LUE<-df_LUE%>%
  mutate(doy=yday(Date))%>%
  group_by(Sitename,doy)%>%
  summarise(LUE_mean=mean(LUE,na.rm = T))
#
library(ggbreak)
p_LUE<-df_LUE%>%
  ggplot(aes(x=doy,y=LUE_mean,col=Sitename))+
  geom_point()+
  xlim(0,250)+
  ylim(0,0.05)+
  # scale_y_break(c(0.04,0.07))+ #对y轴进行截断
  geom_smooth(span=0.3)+
  scale_color_manual(values = c("CH-Dav"=adjustcolor("red",0.5),
                                "DE-Tha"=adjustcolor("orange",0.5)))+
  # addding date: "2023-03-27"
  # geom_vline(xintercept = 86,col="black")+
  # annotate(geom="text",x=110,y=-50,
  #          label="Mar,27",col="black",size=6)+
  ylab(expression("LUE"[Eco]*" ("*mu*"mol C m"^-2*mu*"mol"^-1*")"))+
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
#ploting making--3. making the delta leaf physio for CH-Dav and DE-Tha
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
    xlab("2023-DOY")+
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

#Fv/Fm,NPQ,qN:
#filter the data when NPQ=0...
df.merge_FvFm_NPQ_qN<-df.leaf_physio$df_FvFm_NPQ_qN %>%
  filter(Fv.Fm>0 & NPQ >0 & qN.qP>0 & qN.qP<10)
p_Fv.Fm_Date<-plot_meansd_fun(df.merge_FvFm_NPQ_qN,"Fv.Fm",
                               c(0.1,0.9),
                               data.frame(x=84,xend=84,y=-0.05,yend=-0.15),
                               TRUE)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" Fv/Fm")
  )+
  theme(
        legend.position = c(0.1,0.4),
        # legend.background = element_rect(),
        # plot.margin = margin(0,5,0,5),
        panel.spacing = unit(0.05, "lines"))+
  annotate(geom = "text",x=c(64),y=rep(-0.4),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.4),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.4),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.4),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.4),label=c("C5"),size=6)
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
p_NPQ_Date<-plot_meansd_fun(df.merge_FvFm_NPQ_qN,
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
p_qNtoqP_Date<-plot_meansd_fun(df.merge_FvFm_NPQ_qN,
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
p_CartoCab_Date<-plot_meansd_fun(df.merge_Pigments,"CartoCab_ratio",
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
p_ChatoChb_Date<-plot_meansd_fun(df.merge_Pigments,"Car",c(0.9,0.9),
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
p_SLA_Date<-plot_meansd_fun(df.merge_traits,
                             "SLA",c(0.9,0.9),
                             data.frame(x=84,xend=84,y=5,yend=1.5),
                             TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" SLA (cm"^2*g^-1*")")
  )+
  theme(legend.position = "none",
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        # plot.margin = margin(0,5,0,5)
        )
# p_LMA_Date<-plot_point_fun_meansd(df.merge_traits,"LMA",c(0.9,0.9))+
#   labs(
#     x="2023",
#     # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
#     y = expression(LMA*" (g"*"cm"^-2*")")
#   )
p_width_Date<-plot_meansd_fun(df.merge_traits,
                               "ImageJ_average_width",c(0.9,0.9),
                               data.frame(x=84,xend=84,y=0.025,yend=0.02),
                               TRUE)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" Width"[needle]*" (cm"*")")
  )+
  theme(legend.position = c(0.1,0.4),
        # plot.margin = margin(0,5,0,5),
        # panel.spacing = unit(0.05, "lines")
        )+
  annotate(geom = "text",x=c(64),y=rep(-0.01),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.01),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.01),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.01),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.01),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.01),label=c("C6"),size=6)

#-----------
#ploting making--4. making the delta leaf spectra for CH-Dav and DE-Tha
#-----------
#PRI
df.Poly.sepctra<-df.Poly.sepctra
  # %>%
  # #only select lower and upper panel:
  # filter(Position=="Lower" | Position=="Upper")
p_PRI_Date<-plot_meansd_fun(df.Poly.sepctra,"PRI",c(0.1,0.1),
                                  data.frame(x=84,xend=84,y=0.01,yend=-0.012),
                                  TRUE)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" PRI"[Needle])
  )+
  theme(legend.position = c(0.1,0.4),
        # plot.margin = margin(0,5,0,5),
        # panel.spacing = unit(0.05, "lines")
        )+
  annotate(geom = "text",x=c(64),y=rep(-0.125),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.125),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.125),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.125),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.125),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.125),label=c("C6"),size=6)

#-------------------------
#(3)further plotting: adding lines and rectanges in plot
#-------------------------
#period:[sos-60,sos+60] is set to red
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

#2. for DSPR period for LUE:
p_plot_LUE_2sites<-p_LUE+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=0,label="sos",col="forestgreen",size=6)


#-------------------------
#(4)merge the plots:
#-------------------------
p_merge<-plot_grid(
  p_plot_deltaGPP_2sites,p_NPQ_Date,p_CartoCab_Date,
  p_plot_LUE_2sites,p_qNtoqP_Date,p_SLA_Date,
  p_Fv.Fm_Date,p_PRI_Date,p_width_Date,
  ncol = 3, nrow=3,
  align = "hv",
  labels = c("(a)","(d)","(g)",
  "(b)","(e)","(h)",
  "(c)","(f)","(i)"),
  axis="tblr",
  label_size = 22,
  # rel_heights = c(0.19,0.19,0.19,0.22,0.2)
  theme(plot.margin = margin(0,0,0,0),
        panel.spacing = unit(0,"lines"))
  )


##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig2_new.png"),p_merge,width = 22,height=15)



