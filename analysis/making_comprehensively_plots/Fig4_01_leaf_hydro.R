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
load(paste0(load.path,"df.Meteo_andFluxes.daily.RDA"))

#tidy the data:
df.Dav<-df.Meteo.daily$Dav
df.Tha<-df.Meteo.daily$Tha
names(df.Dav)
names(df.Tha)

#extract the Ta for Davos and Tharandt:
df.Dav.Meteo<-df.Dav %>%
  dplyr::select(Date,TA_F,PPFD_IN,P_F)%>%
  mutate(TA=TA_F,TA_F=NULL,
         P=P_F,P_F=NULL)%>%
  filter(Date %in% as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                   "2023-05-03","2023-05-22","2023-07-17")))%>%
  mutate(sitename="DAV",
         CampaignNum=paste0("C",1:6))
df.Tha.Meteo<-df.Tha %>%
  dplyr::select(Date,TA_F,PPFD_IN,P_F)%>%
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
#addding mean and sd

#also adding arrows in C2:


# plot_point_fun_meansd(df.merge_Gs_E,"Gs",c(0.05,0.85))
plot_point_fun_meansd<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.merge_Gs_E
  # var_name<-"Gs"
  # legend.xy<-c(0.1,0.9)
  # arrow.xy<-data.frame(x=4,xend=4,y=3e-05,yend=1.5e-05)
  # arrow.flag<-TRUE
  
  df$y<-as.numeric(unlist(df[,var_name]))
  p_var_Date<-df%>%
    filter(Position!="Middle")%>%
    group_by(Position)%>%
    ggplot(aes(x=as.factor(substr(Date,6,10)),y=y))+
    geom_point(size=2,shape=4)+
    # geom_line(size=1.2)+
    stat_summary(aes(x=as.factor(substr(Date,6,10)),y=y,color=sitename),
                 fun.data=mean_sdl, fun.args = list(mult=1),
                 geom="pointrange",size=1.8,linewidth=1.2,shape=16)+
    facet_wrap(~Position)+
    scale_color_manual(values = c("DAV"=adjustcolor("red",0.5),
                                  "THA"=adjustcolor("orange",0.5)))+
    # labs(color = "Sitename")+
    # labs(
    #   # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    #   y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
    # )
    ##separate the campaigns-adding in 2024
    geom_vline(xintercept = c(2.5,4.5,6.5,8.5,10.5),lty=2)+
    theme_light()+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=20),
          axis.text.x = element_text(angle = 35,hjust = 1),
          legend.text = element_text(size=16),
          legend.title = element_blank(),
          strip.text.x = element_text(size=18),
          legend.position = legend.xy,
          legend.background = element_blank()
    )
  #
  if(arrow.flag==TRUE){
    p_var_Date<-p_var_Date+
      #adding the flag to indicate cold events in Davos
      geom_segment(
        aes(x = arrow.xy$x, y = arrow.xy$y,
            xend = arrow.xy$xend, yend = arrow.xy$yend),
        arrow = arrow(type = "closed", 
                      length = unit(0.2, "inches")),
        color = "blue", size = 0.8)
  }
  
  
  
  return(p_var_Date)
}

plot_boxplot_fun<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.merge_traits
  # var_name<-"SLA"
  # legend.xy<-c(0.1,0.9)
  # arrow.xy<-data.frame(x=4,xend=4,y=0.8,yend=0.65)
  # arrow.flag<-FALSE
  
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
    scale_fill_manual(values = c("DAV"=adjustcolor("red",0.6),
                                 "THA"=adjustcolor("orange",0.6)))+
    # labs(fill = "Sitename")+
    xlab("2023")+
    ##separate the campaigns-adding in Jan, 2024
    geom_vline(xintercept = c(2.5,4.5,6.5,8.5,10.5),lty=2)+
    theme_light()+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=20),
          axis.text.x = element_text(angle = 35,hjust = 1),
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
        color = "blue", size = 0.8)
  }
  return(p_var_Date)
}


##-------------
#LIcor data
##-------------
library(ggforce) #-->draw circle in the plot

# Gs and E:
p_Gs_Date<-plot_point_fun_meansd(df.merge_Gs_E,"Gs",
                                 c(0.05,0.85),
                                 data.frame(x=4,xend=4,y=3e-05,yend=1.5e-05),
                                 TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(G[s] ~ "(" * mol ~ m^{-2} ~ s^{-1} * ")")
  )+
  theme(legend.background = element_rect())
p_E_Date<-plot_point_fun_meansd(df.merge_Gs_E,"E",c(0.05,0.9),
                                data.frame(x=4,xend=4,y=3e-05,yend=1.5e-05),
                                FALSE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("T (mol" ~ m^{-2} ~ s^{-1} * ")")
  )
#additionally:
p_gws_Date<-plot_point_fun_meansd(df.merge_Gs_E,"gsw",c(0.1,0.9),
                                  data.frame(x=4,xend=4,y=3e-05,yend=1.5e-05),
                                  FALSE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(gsw ~ "(" * mol ~ m^{-2} ~ s^{-1} * ")")
  )+
  theme(legend.position = "none")

##-------------
#leaf water potential
##-------------
#small twig
p_WP_twig_Date<-plot_point_fun_meansd(df.WP,"WP_Twig",c(0.36,0.2),
               data.frame(x=4,xend=4,y=-20,yend=-30),
                                      TRUE)+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(psi[twig]*" (bar)")
  )+
  theme(legend.position = "none")
#big branch
p_WP_branch_Date<-plot_point_fun_meansd(df.WP,"WP_Branch",c(0.36,0.2),
                 data.frame(x=4,xend=4,y=-20,yend=-30),
                                        TRUE)+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(psi[branch]*" (bar)")
  )


#!merge the Gs, and,leaf water potential
p_leaf_hydro<-plot_grid(p_Gs_Date,p_WP_twig_Date,ncol=1,
                         align = "v",labels = c("(a)","(b)"))
# ggsave(p_hydro_merge,filename = paste("./manuscript/Summary_Vars_with_Time/P_hydro_merge.png"),
#        width = 10,height = 8)
##save the ggplot plots:
save.path<-"./data/Comprehensive_plot_data/Fig4/"
save(p_leaf_hydro,file=paste0(save.path,"p_leaf_hydro.RDA"))

