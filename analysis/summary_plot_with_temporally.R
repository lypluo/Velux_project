#################################################
#Aim: structure the plot temporally
#e.g. x-->Date, y=Amax in different sites and height
##################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

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
  select(Date,TA,PPFD_IN)%>%
  filter(Date %in% as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                   "2023-05-03","2023-05-22","2023-07-17")))%>%
  mutate(sitename="DAV",
         CampaignNum=paste0("C",1:6))
df.Tha.Meteo<-df.Tha %>%
  select(Date,TA_F,PPFD_IN)%>%
  mutate(TA=TA_F,TA_F=NULL)%>%
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


#------------------
#(2)merge the data
#------------------
df.merge_Amax<-left_join(df.sites.LRC.paras,df.sites.Meteo)
df.merge_A<-left_join(df.A,df.sites.Meteo)
df.merge_Gs_E<-left_join(df.Gs_E,df.sites.Meteo)
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
    geom_line(size=1.2)+
    # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
    #              geom="pointrange",size=3,linewidth=4)+
    scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.5),
                                  "THA"=adjustcolor("cyan4",0.5)))+
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

plot_boxplot_fun<-function(df,var_name,legend.xy){
  # df<-df.merge_A
  # var_name<-"A"
  # legend.xy<-c(0.1,0.9)
  
  df$y<-df[,var_name]
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
    theme_bw()+
    theme(axis.text = element_text(size=14),
          axis.title = element_text(size=16),
          axis.text.x = element_text(angle = 45,hjust = 1),
          legend.text = element_text(size=14),
          legend.position = legend.xy,
          legend.background = element_blank()
    )
  return(p_var_Date)
}



##-------------
#LIcor data
##-------------
#1) Amax:
p_Amax_Date<-plot_point_fun(df.merge_Amax,"k_sat",c(0.8,0.25))+
  labs(
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  )
#save the plot:
ggsave(p_Amax_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_Amax_time.png"),width = 9)

#2) A:
p_A_Date<-plot_boxplot_fun(df.merge_A,"A",c(0.1,0.9))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  )
#save the plot:
ggsave(p_A_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_A_time.png"),width = 9)

#3) Gs and E:
p_Gs_Date<-plot_boxplot_fun(df.merge_Gs_E,"Gs",c(0.1,0.9))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(G[s] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  )
p_E_Date<-plot_boxplot_fun(df.merge_Gs_E,"E",c(0.1,0.9))+
  labs(
    x="2023",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("Transpiration (mol" ~ m^{-2} ~ s^{-1} * ")")
  )
#save the plot:
ggsave(p_Gs_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_Gs_time.png"),width = 9)
ggsave(p_E_Date,filename = paste("./manuscript/Summary_Vars_with_Time/P_E_time.png"),width = 9)
