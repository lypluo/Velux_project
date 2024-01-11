#################################################
#Aim: structure the plot temporally-->for the leaf scale measurements
#e.g. x-->Date, y=Amax in different sites and height
##################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)
#Date of 6 campaigns:
#For Tharandt: Mar,02; Mar,22; Apr,13; Apr,28; May,17; July,14
#For Davos:Mar,08; Mar,27; Apr,21; May,03; May,22; July,17 
#------------------
#(1)load the data
#------------------
#----------------------
#A.load the meteo data:
#----------------------
load.path<-"data/EC_MeteoandFlux/"
load(paste0(load.path,"df.Meteo.daily.RDA"))

#tidy the data:
df.Dav<-df.Meteo.daily$Dav
df.Tha<-df.Meteo.daily$Tha
names(df.Dav)
names(df.Tha)

#extract the Ta for Davos and Tharandt:
df.Dav.Meteo<-df.Dav %>%
  select(Date,TA,PPFD_IN,P)%>%
  filter(Date %in% as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                   "2023-05-03","2023-05-22","2023-07-17")))%>%
  mutate(sitename="DAV",
         CampaignNum=paste0("C",1:6))
df.Tha.Meteo<-df.Tha %>%
  select(Date,TA_F,PPFD_IN,P_F)%>%
  mutate(TA=TA_F,TA_F=NULL,
         P=P_F,P_F=NULL)%>%
  filter(Date %in% as.Date(c("2023-03-02","2023-03-22","2023-04-13",
                             "2023-04-28","2023-05-17","2023-07-14")))%>%
  mutate(sitename="THA",
         CampaignNum=paste0("C",1:6))
#
df.sites.Meteo<-rbind(df.Dav.Meteo,df.Tha.Meteo)

#----------------------
#B.load the LIcor measurements:
#----------------------
#Amax
load.path<-"data/LIcor/"
load(paste0(load.path,"LRC.parameters.RDA"))
df.sites.LRC.paras<-df.LRC.paras %>%
  mutate(CampaignNum=substr(ID,1,2),
         Position=substr(ID,4,4))
#photosynthesis(A):
load(paste0(load.path,"df.Amax.cleaned.RDA"))
df.A<-dat
#Gs and E:
load(paste0(load.path,"Gs_E.cleaned.RDA"))
df.Gs_E<-df.physio

#Fv/Fm,NPQ, qN
load.path<-"./data/LIcor/"
load(paste0(load.path,"df.NPQ.cleaned.RDA"))
df.FvFm_NPQ_qN<-df.NPQ.merge

#----------------------
#C.load the pigments
#----------------------
load.path<-"data/"
load(paste0(load.path,"Pigment.data.RDA"))
df.Pigment<-df.Pigment %>%
  select(sitename,CampaignNum,Position,Cha,Chb,Car,
         CartoCab_ratio)%>%
  mutate(Cab=Cha+Chb)
#----------------------
#D.load the spectral meaurements 
#----------------------
load.path<-"data/"
load(paste0(load.path,"Polypen.data.cleaned.RDA"))
df.Poly.sepctra<-df.Poly.sel%>%
  mutate(Date=as.Date(Date))%>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="M") ~"Middle",
                            c(Position=="U") ~"Upper"))
#----------------------
#E.load the water potential data:
#----------------------
load.path<-"data/"
load(paste0(load.path,"WaterPotential.data.cleaned.RDA"))
df.WP<-df.WaterP.sel%>%
  mutate(Date=as.Date(Date))%>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="U") ~"Upper"))
#----------------------
#F.load the leaf traits:
#----------------------
load.path<-"data/"
load(paste0(load.path,"Leaf_traits.data.cleaned.RDA"))
df.traits<-df.traits.sel
#------------------
#(2)merge the data and change the Position names
#------------------
df.merge_Amax<-left_join(df.sites.LRC.paras,df.sites.Meteo)
df.merge_A<-left_join(df.A,df.sites.Meteo)
df.merge_Gs_E<-left_join(df.Gs_E,df.sites.Meteo)
df.merge_FvFm_NPQ_qN<-left_join(df.FvFm_NPQ_qN,df.sites.Meteo)
df.merge_Pigments<-left_join(df.Pigment,df.sites.Meteo)
df.merge_traits<-left_join(df.traits,df.sites.Meteo)
##
df.merge_Amax<-df.merge_Amax %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="U") ~"Upper"))
df.merge_A<-df.merge_A %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="U") ~"Upper"))
df.merge_Gs_E<-df.merge_Gs_E %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="U") ~"Upper"))
df.merge_FvFm_NPQ_qN<-df.merge_FvFm_NPQ_qN %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="U") ~"Upper"))
df.merge_Pigments<-df.merge_Pigments %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="M") ~"Middle",
                            c(Position=="U") ~"Upper"))
df.merge_traits<-df.merge_traits %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="M") ~"Middle",
                            c(Position=="U") ~"Upper"))
#------------------
#(3)plotting
#------------------
plot_point_fun<-function(df,var_name,legend.xy){
  # df<-df.merge_Amax
  # var_name<-"k_sat"
  # legend.xy<-c(0.8,0.25)
  
  df$y<-df[,var_name]
  p_var_Date<-df%>%
    ggplot(aes(x=Date,y=y,col=sitename,shape=Position))+
    geom_point(size=3)+
    geom_line(size=1.2,lty=2)+
    # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
    #              geom="pointrange",size=3,linewidth=4)+
    scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.8),
                                  "THA"=adjustcolor("cyan4",0.8)))+
    labs(color = "Sitename")+
    # labs(
    #   # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    #   y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
    # )
    theme_bw()+
    theme(axis.text = element_text(size=14),
          axis.title = element_text(size=16),
          legend.text = element_text(size=14),
          legend.position = legend.xy,
          legend.background = element_blank()
    )
  #
  return(p_var_Date)
}

#addding mean and sd
plot_point_fun_meansd<-function(df,var_name,legend.xy){
  # df<-df.merge_A
  # var_name<-"A"
  # legend.xy<-c(0.8,0.25)
  
  df$y<-as.numeric(unlist(df[,var_name]))
  p_var_Date<-df%>%
    group_by(Position)%>%
    ggplot(aes(x=Date,y=y,col=sitename))+
    geom_point(size=3)+
    # geom_line(size=1.2)+
    stat_summary(aes(x=Date,y=y,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
                 geom="pointrange",size=1.8,linewidth=1.2)+
    facet_wrap(~Position)+
    scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.5),
                                  "THA"=adjustcolor("cyan4",0.5)))+
    labs(color = "Sitename")+
    # labs(
    #   # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    #   y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
    # )
    theme_bw()+
    theme(axis.text = element_text(size=14),
          axis.title = element_text(size=16),
          legend.text = element_text(size=14),
          strip.text.x = element_text(size=18),
          legend.position = legend.xy,
          legend.background = element_blank()
    )
  #
  return(p_var_Date)
}

plot_boxplot_fun<-function(df,var_name,legend.xy){
  # df<-df.merge_Pigments
  # var_name<-"CartoCab_ratio"
  # legend.xy<-c(0.1,0.9)
  
  df$y<-as.numeric(unlist(df[,var_name]))
  p_var_Date<-df%>%
    group_by(Position)%>%
    ggplot(aes(x=as.factor(substr(Date,6,10)),y=y,fill=sitename))+
    geom_line(size=1.2)+
    geom_boxplot()+
    geom_point(size=1.2)+
    facet_wrap(~Position)+
    # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
    #              geom="pointrange",size=3,linewidth=4)+
    scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.5),
                                  "THA"=adjustcolor("cyan4",0.5)))+
    labs(fill = "Sitename")+
    ##separate the campaigns-adding in Jan, 2024
    geom_vline(xintercept = c(2.5,4.5,6.5,8.5,10.5),lty=2)+
    theme_bw()+
    theme(axis.text = element_text(size=14),
          axis.title = element_text(size=16),
          axis.text.x = element_text(angle = 45,hjust = 1),
          #increasing text size of facet grid labels
          strip.text.x = element_text(size=18),
          legend.text = element_text(size=14),
          legend.position = legend.xy,
          legend.background = element_blank()
    )
  return(p_var_Date)
}
##------------
#Basic Meteo
##------------
plot_Meteo_fun<-function(df,var_name,legend.xy,if.legend){
  # df<-df.sites.Meteo
  # var_name<-"P"
  # legend.xy<-c(0.8,0.25)
  # if.lgend<-FALSE
  
  df[,"y"]<-df[,var_name]
  p_var_Date<-df%>%
    ggplot(aes(x=Date,y=y,col=sitename))+
    geom_point(size=4)+
    geom_line(size=1.2,lty=2)+
    scale_color_manual(values = c("DAV"=adjustcolor("tomato"),
                                  "THA"=adjustcolor("cyan4")))+
    labs(color = "Sitename")+
    # labs(
    #   # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    #   y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
    # )
    theme_bw()+
    theme(axis.text = element_text(size=14),
          axis.title = element_text(size=16),
          legend.text = element_text(size=14),
          legend.position = legend.xy,
          legend.background = element_blank()
    )
  if(if.legend==FALSE){
  p_var_Date<-p_var_Date+
    theme(legend.position = "NA")
  }
  #
  return(p_var_Date)
}
#Ta:
p_T_Date<-plot_Meteo_fun(df.sites.Meteo,"TA",c(0.8,0.1),FALSE)+
  labs(
    x = "2023",
    y = expression("Ta ("* "°C)"))
#P:
p_P_Date<-plot_Meteo_fun(df.sites.Meteo,"P",c(0.8,0.1),FALSE)+
  labs(
    x = "2023",
    y = expression("P (mm)"))
#PAR:
p_PAR_Date<-plot_Meteo_fun(df.sites.Meteo,"PPFD_IN",c(0.8,0.1),FALSE)+
  labs(
    x = "2023",
    y = expression(PAR ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
    )
##
p_Meteo_Date<-plot_grid(p_T_Date,p_PAR_Date,p_P_Date,nrow = 1,align = 'hv')
#save the plot:
ggsave(p_Meteo_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_Meteo_time.png"),width = 10)

##-------------
#LIcor data
##-------------
library(ggforce) #-->draw circle in the plot
#1) Amax:
p_Amax_Date<-plot_point_fun(df.merge_Amax,"k_sat",c(0.8,0.25))+
  labs(
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  )+
  geom_circle(aes(x0=as.Date("2023-07-14"),y0=9,r=1.1),inherit.aes = FALSE,size=1.1,color="black")
#save the plot:
ggsave(p_Amax_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_Amax_time.png"))

#2) A:
p_A_Date<-plot_boxplot_fun(df.merge_A,"A",c(0.1,0.9))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(A ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  )
#save the plot:
ggsave(p_A_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_A_time.png"),width = 9)

#3) Gs and E:
p_Gs_Date<-plot_point_fun_meansd(df.merge_Gs_E,"Gs",c(0.1,0.9))+
  # plot_point_fun_meansd(df.merge_Gs_E[df.merge_Gs_E$CampaignNum!="C6",],"Gs",c(0.1,0.9))+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(G[s] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  )+
  theme(legend.position = "none")
p_E_Date<-plot_point_fun_meansd(df.merge_Gs_E,"E",c(0.05,0.9))+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("T (mol" ~ m^{-2} ~ s^{-1} * ")")
  )
#save the plot:
ggsave(p_Gs_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_Gs_time.png"),width = 9)
ggsave(p_E_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_E_time.png"),width = 9)

#4)Fv/Fm,NPQ,qN:
#filter the data when NPQ=0...
df.merge_FvFm_NPQ_qN<-df.merge_FvFm_NPQ_qN %>%
  filter(Fv.Fm>0 & NPQ >0)
p_Fv.Fm_Date<-plot_boxplot_fun(df.merge_FvFm_NPQ_qN,"Fv.Fm",c(0.1,0.9))+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Fv/Fm")
  )+
  theme(axis.text.x = element_blank(),legend.position = c(0.4,0.3))
p_PhiPS2_Date<-plot_boxplot_fun(df.merge_FvFm_NPQ_qN,"PhiPS2",c(0.8,0.2))+
  labs(
    x="",
    y = (expression(phi[PSII]))
  )+
  theme(legend.position = "none",
        axis.text.x = element_blank()
        )
p_NPQ_Date<-plot_boxplot_fun(df.merge_FvFm_NPQ_qN,"NPQ",c(0.1,0.9))+
  labs(
    x="",
    y = expression("NPQ")
  )+
  theme(legend.position = "none",
    axis.text.x = element_blank())
p_qN_Date<-plot_point_fun_meansd(df.merge_FvFm_NPQ_qN,"qN",c(0.8,0.2))+
  labs(
    x="2023",
    y = expression("qN")
  )
p_qNtoqP_Date<-plot_boxplot_fun(df.merge_FvFm_NPQ_qN,"qN.qP",c(0.8,0.2))+
  labs(
    x="2023",
    y = expression("qN/qP")
  )+ylim(0,9)+
  theme(legend.position = "none")

#save the plot:
# ggsave(p_Fv.Fm_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_Fv.Fm_time.png"),width = 9)
# ggsave(p_NPQ_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_NPQ_time.png"),width = 9)
# ggsave(p_qN_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_qN_time.png"),width = 9)

#merge the plots:
# p_physio_merge<-plot_grid(p_Fv.Fm_Date,p_PhiPS2_Date,p_NPQ_Date,p_qNtoqP_Date,
#           ncol = 1,align = "v",labels = c("A","B","C","D"))
p_physio_merge<-plot_grid(p_Fv.Fm_Date,p_NPQ_Date,p_qNtoqP_Date,
                          ncol = 1,align = "v",labels = c("A","B","C"))

ggsave(p_physio_merge,filename = paste("./manuscript/Summary_Vars_with_Time/P_physio_merge.png"),
       width = 8,height = 7.5)


##-------------
#Pigments data
##-------------
#adding pigments ratios
df.merge_Pigments<-df.merge_Pigments %>%
  mutate(ChatoChb=Cha/Chb)
#1) Car/Cab
p_CartoCab_Date<-plot_point_fun_meansd(df.merge_Pigments,"CartoCab_ratio",c(0.1,0.9))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Car/Cab")
  )+
  theme(legend.position = "none")
#2) Cha
p_Cha_Date<-plot_point_fun_meansd(df.merge_Pigments,"Cha",c(0.1,0.9))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Cha (mg g"^-1*")")
  )
#3) Chb
p_Chb_Date<-plot_point_fun_meansd(df.merge_Pigments,"Chb",c(0.9,0.9))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Chb (mg g"^-1*")")
  )
#4) Cab
p_Cab_Date<-plot_point_fun_meansd(df.merge_Pigments,"Cab",c(0.9,0.75))+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Cab (mg g"^-1*")")
  )
#5)Car
p_Car_Date<-plot_point_fun_meansd(df.merge_Pigments,"Car",c(0.9,0.9))+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Car (mg g"^-1*")")
  )+
  theme(legend.position = "none")
#addtional plots:
#theorotically, if ratio of Cha/Chb high-->higher light photoprotection according to Liyao
#but the results did not reflect this 
p_ChatoChb_Date<-plot_point_fun_meansd(df.merge_Pigments,"Car",c(0.9,0.9))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Chla/Chb")
  )
#save the plot:
# ggsave(p_CartoCab_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_CartoCab_time.png"),width = 9)
# ggsave(p_Cha_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_Cha_time.png"),width = 9)
# ggsave(p_Chb_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_Chb_time.png"),width = 9)
# ggsave(p_Cab_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_Cab_time.png"),width = 9)
# ggsave(p_Car_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_Car_time.png"),width = 9)
#merge the plots:
p_pigments_merge<-plot_grid(p_Cab_Date,p_Car_Date,p_CartoCab_Date,ncol=1,
                            align = "v",labels = c("A","B","C"))
ggsave(p_pigments_merge,filename = paste("./manuscript/Summary_Vars_with_Time/P_pigment_merge.png"),
       width = 8,height = 8)

##-------------
#leaf spectral data
##-------------
#1) NDVI
p_NDVI_Date<-plot_point_fun_meansd(df.Poly.sepctra,"NDVI",c(0.1,0.1))+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("NDVI")
  )+
  theme(legend.position =c(0.25,0.25))
#2) PRI
p_PRI_Date<-plot_point_fun_meansd(df.Poly.sepctra,"PRI",c(0.1,0.1))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("PRI")
  )+
  theme(legend.position = "none")
#save the plot:
# ggsave(p_NDVI_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_NDVI_time.png"),width = 9)
# ggsave(p_PRI_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_PRI_time.png"),width = 9)
#merge the plots:
p_VIs_merge<-plot_grid(p_NDVI_Date,p_PRI_Date,ncol=1,align = "v",labels = c("A","B"))
ggsave(p_VIs_merge,filename = paste("./manuscript/Summary_Vars_with_Time/P_VIs_merge.png"),
       width = 9,height = 6)

##-------------
#leaf water potential
##-------------
#small twig
p_WP_twig_Date<-plot_point_fun_meansd(df.WP,"WP_Twig",c(0.8,0.2))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(psi[twig]*" (bar)")
  )+theme(legend.position = "none")
#big branch
p_WP_branch_Date<-plot_point_fun_meansd(df.WP,"WP_Branch",c(0.8,0.2))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(psi[branch]*" (bar)")
  )
#save the plot:
# ggsave(p_WP_twig_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_WP_twig_time.png"),width = 9)
# ggsave(p_WP_branch_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_WP_branch_time.png"),width = 9)
#!merge the transpiration,Gs, and,leaf water potential
p_hydro_merge<-plot_grid(p_E_Date,p_Gs_Date,p_WP_twig_Date,ncol=1,
                         align = "v",labels = c("A","B","C"))
ggsave(p_hydro_merge,filename = paste("./manuscript/Summary_Vars_with_Time/P_hydro_merge.png"),
       width = 10,height = 8)

##-------------
#leaf traits
##-------------
p_SLA_Date<-plot_boxplot_fun(df.merge_traits,"SLA",c(0.9,0.9))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(SLA*" (cm"^2*g^-1*")")
  )+
  theme(legend.position = "none")
# p_LMA_Date<-plot_point_fun_meansd(df.merge_traits,"LMA",c(0.9,0.9))+
#   labs(
#     x="2023",
#     # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
#     y = expression(LMA*" (g"*"cm"^-2*")")
#   )
p_width_Date<-plot_boxplot_fun(df.merge_traits,"ImageJ_average_width",c(0.9,0.9))+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Needle width (cm"*")")
  )+
  theme(legend.position = c(0.05,0.8))

#save the plot:
# ggsave(p_SLA_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_SLA_time.png"),width = 9)
# ggsave(p_width_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_width_time.png"),width = 9)
p_traits_merge<-plot_grid(p_width_Date,p_SLA_Date,ncol=1,align = "v",labels = c("A","B"))
ggsave(p_traits_merge,filename = paste("./manuscript/Summary_Vars_with_Time/P_traits_merge.png"),
       width = 9,height = 6)


###additional Merge plots:
#merge the leaf pigments and VIs:
p_CartoCab_Date<-p_CartoCab_Date+
  xlab("")+
  theme(legend.position = c(0.05,0.8))
p_PRI_Date<-p_PRI_Date+
  ylim(-0.2,0.1)
p_pigment_PRI<-plot_grid(p_CartoCab_Date,p_PRI_Date,align = "h",ncol=1,nrow=2,
                         width=9,height=6,labels=c("A","B"))
ggsave(p_pigment_PRI,filename = paste("./manuscript/Summary_Vars_with_Time/P_pigment_PRI.png"),
       width = 9,height = 6)
