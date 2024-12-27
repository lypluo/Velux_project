#################################################
#Aim: structure the plot temporally-->for the leaf scale measurements
#e.g. x-->Date, y=Amax in different sites and height
#update in Nov, 2024-->specifically plot A and Amax
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

#------------------
#(2)merge the data and change the Position names
#------------------
df.merge_Amax<-left_join(df.sites.LRC.paras,df.sites.Meteo)
df.merge_A<-left_join(df.A,df.sites.Meteo)
##
df.merge_Amax<-df.merge_Amax %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="U") ~"Upper"))
df.merge_A<-df.merge_A %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
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
    scale_color_manual(values = c("DAV"=adjustcolor("red",0.6),
                                  "THA"=adjustcolor("orange",0.6)))+
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
    scale_color_manual(values = c("DAV"=adjustcolor("red",0.5),
                                  "THA"=adjustcolor("orange",0.5)))+
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
  # df<-df.merge_A
  # var_name<-"A"
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
    scale_fill_manual(values = c("DAV"=adjustcolor("red",0.5),
                                  "THA"=adjustcolor("orange",0.5)))+
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

##-------------
#LIcor data
##-------------
library(ggforce) #-->draw circle in the plot
#1) Amax:
p_Amax_Date<-plot_point_fun(df.merge_Amax,"k_sat",c(0.8,0.25))+
  labs(
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    x ="2023",
    y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  )+
  geom_circle(aes(x0=as.Date("2023-07-14"),y0=9,r=2),
              size=1.1,color="black")

#2) A:
p_A_Date<-plot_boxplot_fun(df.merge_A,"A",c(0.04,0.9))+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(A ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  )
#save the plot:
p_A_Amax<-plot_grid(p_A_Date,p_Amax_Date,align = "v",ncol=1,nrow=2,
                    labels=c("(a)","(b)"))
#
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig2_supp1_A_Amax.png"),
       p_A_Amax,
       width = 10,height=12)

