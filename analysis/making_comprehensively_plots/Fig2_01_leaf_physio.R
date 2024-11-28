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
load.path<-"./data/"
load(paste0(load.path,"Pigment.data.RDA"))
df.Pigment<-df.Pigment %>%
  dplyr::select(sitename,CampaignNum,Position,Cha,Chb,Car,
         CartoCab_ratio)%>%
  mutate(Cab=Cha+Chb)

#--other variables---
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
plot_point_fun_meansd<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.merge_Pigments
  # var_name<-"CartoCab_ratio"
  # legend.xy<-c(0.1,0.9)
  # arrow.xy<-data.frame(x=4,xend=4,y=0.8,yend=0.65)
  # arrow.flag<-FALSE
  
  df$y<-as.numeric(unlist(df[,var_name]))
  p_var_Date<-df%>%
    filter(Position!="Middle")%>%
    group_by(Position)%>%
    ggplot(aes(x=as.factor(substr(Date,6,10)),y=y))+
    geom_point(size=2,shape=4)+
    # geom_line(size=1.2)+
    stat_summary(aes(x=as.factor(substr(Date,6,10)),y=y,color=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
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
  #
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

#Fv/Fm,NPQ,qN:
#filter the data when NPQ=0...
df.merge_FvFm_NPQ_qN<-df.merge_FvFm_NPQ_qN %>%
  filter(Fv.Fm>0 & NPQ >0)
p_Fv.Fm_Date<-plot_boxplot_fun(df.merge_FvFm_NPQ_qN,"Fv.Fm",
                               c(0.1,0.9),
                               data.frame(x=4,xend=4,y=0.8,yend=0.65),
                               TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Fv/Fm")
  )+
  theme(legend.position = c(0.4,0.3),
        legend.background = element_rect(),
        axis.text.x = element_blank(),
        # plot.margin = margin(0,5,0,5),
        panel.spacing = unit(0.05, "lines"))
# p_PhiPS2_Date<-plot_boxplot_fun(df.merge_FvFm_NPQ_qN,
#                                 "PhiPS2",c(0.8,0.2))+
#   labs(
#     x="",
#     y = (expression(phi[PSII]))
#   )+
#   theme(legend.position = "none",
#         axis.text.x = element_blank()
#         )
p_NPQ_Date<-plot_boxplot_fun(df.merge_FvFm_NPQ_qN,
                             "NPQ",c(0.1,0.9),
                             data.frame(x=4,xend=4,y=2,yend=1.5),
                             TRUE)+
  labs(
    x="",
    y = expression("NPQ")
  )+
  theme(legend.position = "none",
    axis.text.x = element_blank(),
    plot.margin = margin(-10,5,0,5),
    panel.spacing = unit(0.1, "lines"))
p_qNtoqP_Date<-plot_boxplot_fun(df.merge_FvFm_NPQ_qN,
                                "qN.qP",c(0.8,0.2),
                                data.frame(x=4,xend=4,y=9,yend=7.5),
                                TRUE)+
  labs(
    x="",
    y = expression("qN/qP")
  )+ylim(0,9)+
  theme(legend.position = "none",
        plot.margin = margin(0,5,0,5),
        panel.spacing = unit(0.1, "lines"))


#merge the plots:
# plot_grid(p_Fv.Fm_Date,p_NPQ_Date,p_qNtoqP_Date,
#           ncol = 1,align = "v",labels = c("(a)","(b)","(c)"))

##-------------
#Pigments data
##-------------
#adding pigments ratios
df.merge_Pigments<-df.merge_Pigments %>%
  mutate(ChatoChb=Cha/Chb)
# Car/Cab
p_CartoCab_Date<-plot_point_fun_meansd(df.merge_Pigments,"CartoCab_ratio",
                                       c(0.1,0.9),
                                       data.frame(x=4,xend=4,y=0.8,yend=0.65),
                                       TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Car/Cab")
  )+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.37,0.8),
        legend.background = element_rect(),
        plot.margin = margin(0,5,0,5),
        panel.spacing.y = unit(0.05, "lines"))

#addtional plots:
#theorotically, if ratio of Cha/Chb high-->higher light photoprotection according to Liyao
#but the results did not reflect this 
p_ChatoChb_Date<-plot_point_fun_meansd(df.merge_Pigments,"Car",c(0.9,0.9),
                                       data.frame(x=4,xend=4,y=0.55,yend=0.45),
                                       TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Chla/Chb")
  )+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.35,0.8),
        legend.background = element_rect(),
        plot.margin = margin(0,5,0,5))

##-------------
#leaf traits
##-------------
p_SLA_Date<-plot_boxplot_fun(df.merge_traits,
                             "SLA",c(0.9,0.9),
                             data.frame(x=4,xend=4,y=0,yend=0),
                             FALSE
                             )+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(SLA*" (cm"^2*g^-1*")")
  )+
  theme(legend.position = "none",
        plot.margin = margin(0,5,0,5))
# p_LMA_Date<-plot_point_fun_meansd(df.merge_traits,"LMA",c(0.9,0.9))+
#   labs(
#     x="2023",
#     # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
#     y = expression(LMA*" (g"*"cm"^-2*")")
#   )
p_width_Date<-plot_boxplot_fun(df.merge_traits,
                               "ImageJ_average_width",c(0.9,0.9),
                               data.frame(x=4,xend=4,y=0,yend=0),
                               FALSE)+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Width"[needle]*" (cm"*")")
  )+
  theme(legend.position = "none",
        plot.margin = margin(0,5,0,5),
        panel.spacing = unit(0.05, "lines"))+
  annotate(geom = "text",x=c(1.5),y=rep(0.055),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(3.5),y=rep(0.055),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(5.5),y=rep(0.055),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(7.5),y=rep(0.055),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(9.5),y=rep(0.055),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(11.5),y=rep(0.055),label=c("C6"),size=6)

###Merge plots:
p_leaf_physio<-plot_grid(p_Fv.Fm_Date,p_NPQ_Date,p_qNtoqP_Date,
                         p_CartoCab_Date,p_width_Date,
                ncol = 1,align = "hv",
                labels = c("(a)","(b)","(c)","(d)","(e)"),
                rel_heights = c(0.19,0.19,0.19,0.22,0.2))+
  theme(plot.margin = margin(0,0,0,0),
    panel.spacing = unit(0,"lines"))
# ggsave(paste0(save.path,"Fig2_temp.png"),p_leaf_physio,width = 12,height=15)
##save the ggplot plots:
save.path<-"./data/Comprehensive_plot_data/Fig2/"
save(p_leaf_physio,file=paste0(save.path,"p_leaf_physio.RDA"))


