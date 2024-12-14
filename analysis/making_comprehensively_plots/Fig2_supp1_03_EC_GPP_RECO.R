##--------------------------------------------------------
#Aim: checking EC fluxes in Dav and Tha
##--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(dplyr)
library(phenopix)
#-------------
#(1)load the data-using daily data
#-------------
load.path<-"./data/EC_MeteoandFlux/"
load(paste0(load.path,"df_daily_from_ICOS.RDA"))

df.Dav<-df_DD$Dav
df.Tha<-df_DD$Tha

#mrege data from two sites
df.Dav<-df.Dav %>%
  mutate(sitename="CH-Dav")
df.Tha<-df.Tha %>%
  mutate(sitename="DE-Tha")
df<-rbind(df.Dav,df.Tha)

#-------------------
#(2) tidy the data 
#-------------------
#---merge the data according to doy
df<-df %>%
  mutate(DoY=yday(Date),
         Year=year(Date)
  )

#---------------------------
#3) making the demonstration plot
#---------------------------
##--------------
#fast checking
##--------------
#Davos
# df %>%
#   filter(sitename=="CH-Dav")%>%
#   group_by(Year)%>%
#   ggplot(aes(x=DoY,y=RECO))+
#   geom_point()+
#   geom_smooth()+
#   facet_wrap(~Year)
# 
# df %>%
#   filter(sitename=="CH-Dav")%>%
#   group_by(Year)%>%
#   ggplot(aes(x=DoY,y=NEE))+
#   geom_point()+
#   geom_smooth()+
#   facet_wrap(~Year)
# 
# df %>%
#   filter(sitename=="CH-Dav")%>%
#   group_by(Year)%>%
#   ggplot(aes(x=DoY,y=LE))+
#   geom_point()+
#   geom_smooth()+
#   facet_wrap(~Year)
# 
# #Tharandt
# df %>%
#   filter(sitename=="DE-Tha")%>%
#   group_by(Year)%>%
#   ggplot(aes(x=DoY,y=RECO))+
#   geom_point()+
#   geom_smooth()+
#   facet_wrap(~Year)
# 
# df %>%
#   filter(sitename=="DE-Tha")%>%
#   group_by(Year)%>%
#   ggplot(aes(x=DoY,y=NEE))+
#   geom_point()+
#   geom_smooth()+
#   facet_wrap(~Year)
# 
# df %>%
#   filter(sitename=="DE-Tha")%>%
#   group_by(Year)%>%
#   ggplot(aes(x=DoY,y=LE))+
#   geom_point()+
#   geom_smooth()+
#   facet_wrap(~Year)

##---------
#GPP 
##---------
plot_fun_GPP_mean<-function(df,sitename,legend_flag){
  # df<-df
  # sitename<-"DE-Tha"
  # legend_flag<-TRUE

  if(sitename=="CH-Dav"){
    df.GPP.use<-df%>%
      filter(sitename=="CH-Dav")%>%
      filter(Year!=2009 & Year!=2011 & Year!=2013 & Year!=2019)%>%
      mutate(Year=as.factor(Year))
  }
  if(sitename=="DE-Tha"){
    df.GPP.use<-df%>%
      filter(sitename=="DE-Tha")%>%
      filter(Year!=1996 & Year!=2020 )%>%
      mutate(Year=as.factor(Year))
  }
      
  df.GPP.use.mean<-df.GPP.use %>%
    group_by(DoY)%>%
    dplyr::summarise(GPP_mean=mean(GPP),
              GPP_sd=sd(GPP))
  # 
  p_plot<-ggplot()+
    geom_ribbon(aes(x=DoY,ymin=GPP_mean-GPP_sd,ymax=GPP_mean+GPP_sd),
                data=df.GPP.use.mean,fill="gray",size=1.5)+
    stat_smooth(aes(x=DoY,y=GPP,col=factor(Year)),
                data=df.GPP.use[df.GPP.use$Year!=2023,],se=FALSE,
                method = "loess",span = 0.6)+
    scale_color_viridis_d()+
    stat_smooth(aes(x=DoY,y=GPP_mean),
                data=df.GPP.use.mean,
                col=adjustcolor("black",0.8),size=2,
                method = "loess",span = 0.6)+
    stat_smooth(aes(x=DoY,y=GPP),data=df.GPP.use[df.GPP.use$Year==2023,],
                col="red",se=FALSE,size=2,
                method = "loess",span = 0.6)+
    xlim(0,250)+
    ylim(-2,12)+
    annotate(geom = "text",x=50,y=11.5,label=sitename,size=6)+
    annotate(geom = "segment",x=180,xend=190,y = 2,yend = 2,col="red",size=1.5)+
    annotate(geom = "segment",x=180,xend=190,y = 1,yend = 1,col="black",size=1.5)+
    annotate(geom = "text",x=220,y=c(2,1),label=c("2023","Mean"),size=5)+
    ylab(expression("GPP ("*mu*"mol m"^-2*"s"^-1*")"))+
    xlab("DOY")+
    labs(color ="Year")+
    theme_light()+
    theme(axis.title = element_text(size=20),
          axis.text = element_text(size = 16),
          legend.position =c(0.2,0.75),
          legend.title = element_text(size=18),
          legend.text = element_text(size=16),
          legend.background = element_blank()
    )+
    guides(color = guide_legend(ncol = 4)) #legend for 4 column

  #deterimines the timings for 10% GPP[mean] and 10% in GPP[2023]
  #For GPP mean
  df.GPP.use.mean_s<-df.GPP.use.mean %>%
    mutate(GPP_mean_smooth=loess(GPP_mean ~ DoY,span = 0.6)$fitted)
  GPP.stats_mean<-df.GPP.use.mean_s%>%
    summarise(max_GPP=quantile(GPP_mean_smooth,0.975,na.rm = T),
           min_GPP=quantile(GPP_mean_smooth,0.025,na.rm = T),
           amp_GPP=max_GPP-min_GPP,
           value_for_amp10=0.1*amp_GPP+min_GPP)
  GPPmean_sos10<-30+which.min(as.numeric(abs(GPP.stats_mean$value_for_amp10 - 
           df.GPP.use.mean_s$GPP_mean_smooth[30:250])))-1
  #For GPP 2023
  df.GPP.use.2023_s<-df.GPP.use%>%
    filter(Year==2023)%>%
    mutate(GPP_smooth=loess(GPP ~ DoY,span = 0.6)$fitted)
  GPP.stats_2023<-df.GPP.use.2023_s%>%
    summarise(max_GPP=quantile(GPP_smooth,0.95,na.rm = T),
              min_GPP=quantile(GPP_smooth,0.05,na.rm = T),
              amp_GPP=max_GPP-min_GPP,
              value_for_amp10=0.1*amp_GPP+min_GPP)
  GPP2023_sos10<-30+which.min(as.numeric(abs(GPP.stats_2023$value_for_amp10 - 
              df.GPP.use.2023_s$GPP_smooth[30:250])))-1
  
  #add lines:
  if(sitename=="CH-Dav"){

    p_plot<-p_plot+
      #for averaged years
      geom_segment(aes(x=GPPmean_sos10,y=0,
                  xend = GPPmean_sos10,
                  #-0.2-->is the adjustment for the height
                  yend = df.GPP.use.mean_s$GPP_mean_smooth[GPPmean_sos10]-0.2),size=1.1,
                   color = "black",lty=2)+
      #for year 2023:
      geom_segment(aes(x=GPP2023_sos10,y=0,
                  xend = GPP2023_sos10,
                  #-0.1-->is the adjustment for the height
                  yend = df.GPP.use.2023_s$GPP_smooth[GPP2023_sos10]-0.2),size=1.1,
                   color = "red",lty=2)
  }
  if(sitename=="DE-Tha"){
    p_plot<-p_plot+
      #for averaged years
      geom_segment(aes(x=GPPmean_sos10,y=0,
                       xend = GPPmean_sos10,
                       #-0.2-->is the adjustment for the height
                       yend = df.GPP.use.mean_s$GPP_mean_smooth[GPPmean_sos10]),size=1.1,
                   color = "black",lty=2)+
      #for year 2023:
      geom_segment(aes(x=GPP2023_sos10,y=0,
                       xend = GPP2023_sos10,
                       #-0.1-->is the adjustment for the height
                       yend = df.GPP.use.2023_s$GPP_smooth[GPP2023_sos10]-0.15),size=1.1,
                   color = "red",lty=2)
  }
  
  #
  if(legend_flag==FALSE){
    p_plot<-p_plot+
      theme(legend.position = "none")
  }

  #
  return(p_plot)
  
}
#
plot_Dav<-plot_fun_GPP_mean(df,"CH-Dav",TRUE)
plot_Tha<-plot_fun_GPP_mean(df,"DE-Tha",FALSE)
#
p_GPP<-plot_grid(plot_Dav,plot_Tha,
                 ncol=1,
                 align = "v",labels = c("(a)","(b)"))
#save the plots
# save.path<-"./manuscript/comprehensive_plot/"
# ggsave(paste0(save.path,"Fig2_supp1_EC_GPP.png"),
#        p_GPP,width = 12,height=15)


##---------
#RECO
##---------
plot_fun_RECO_mean<-function(df,sitename,legend_flag){
  # df<-df
  # sitename<-"DE-Tha"
  # legend_flag<-TRUE
  
  if(sitename=="CH-Dav"){
    df.RECO.use<-df%>%
      filter(sitename=="CH-Dav")%>%
      filter(Year!=2009 & Year!=2011 & Year!=2013 & Year!=2019)%>%
      # filter(Year!=2009 & Year!=2013)%>%
      mutate(Year=as.factor(Year))
  }
  if(sitename=="DE-Tha"){
    df.RECO.use<-df%>%
      filter(sitename=="DE-Tha")%>%
      # filter(Year!=1996)%>%
      filter(Year!=1996 & Year!=2020)%>%
      mutate(Year=as.factor(Year))
  }
  
  df.RECO.use.mean<-df.RECO.use %>%
    group_by(DoY)%>%
    dplyr::summarise(RECO_mean=mean(RECO),
                     RECO_sd=sd(RECO))
  # 
  p_plot<-ggplot()+
    geom_ribbon(aes(x=DoY,ymin=RECO_mean-RECO_sd,ymax=RECO_mean+RECO_sd),
                data=df.RECO.use.mean,fill="gray",size=1.5)+
    stat_smooth(aes(x=DoY,y=RECO,col=factor(Year)),
                data=df.RECO.use[df.RECO.use$Year!=2023,],se=FALSE,
                method = "loess",span = 0.6)+
    scale_color_viridis_d()+
    stat_smooth(aes(x=DoY,y=RECO_mean),
                data=df.RECO.use.mean,
                col=adjustcolor("black",0.8),size=2,
                method = "loess",span = 0.6)+
    stat_smooth(aes(x=DoY,y=RECO),data=df.RECO.use[df.RECO.use$Year==2023,],
                col="red",se=FALSE,size=2,
                method = "loess",span = 0.6)+
    xlim(0,250)+
    ylim(0,8.5)+
    annotate(geom = "text",x=50,y=8.5,label=sitename,size=6)+
    annotate(geom = "segment",x=180,xend=190,y = 2,yend = 2,col="red",size=1.5)+
    annotate(geom = "segment",x=180,xend=190,y = 1,yend = 1,col="black",size=1.5)+
    annotate(geom = "text",x=220,y=c(2,1),label=c("2023","Mean"),size=5)+
    ylab(expression("RECO ("*mu*"mol m"^-2*"s"^-1*")"))+
    xlab("DOY")+
    labs(color ="Year")+
    theme_light()+
    theme(axis.title = element_text(size=20),
          axis.text = element_text(size = 16),
          legend.position =c(0.2,0.75),
          legend.title = element_text(size=18),
          legend.text = element_text(size=16),
          legend.background = element_blank()
    )+
    guides(color = guide_legend(ncol = 4)) #legend for 4 column
  
  #deterimines the timings for 10% RECO[mean] and 10% in RECO[2023]
  #For RECO mean
  df.RECO.use.mean_s<-df.RECO.use.mean %>%
    mutate(RECO_mean_smooth=loess(RECO_mean ~ DoY,span = 0.6)$fitted)
  RECO.stats_mean<-df.RECO.use.mean_s%>%
    summarise(max_RECO=quantile(RECO_mean_smooth,1,na.rm = T),
              min_RECO=quantile(RECO_mean_smooth,0,na.rm = T),
              amp_RECO=max_RECO-min_RECO,
              value_for_amp10=0.1*amp_RECO+min_RECO)
  RECOmean_min<-30+which.min(as.numeric(abs(RECO.stats_mean$min_RECO - 
                                               df.RECO.use.mean_s$RECO_mean_smooth[30:250])))-1
  #For RECO 2023
  df.RECO.use.2023_s<-df.RECO.use%>%
    filter(Year==2023)%>%
    mutate(RECO_smooth=loess(RECO ~ DoY,span = 0.6)$fitted)
  RECO.stats_2023<-df.RECO.use.2023_s%>%
    summarise(max_RECO=quantile(RECO_smooth,1,na.rm = T),
              min_RECO=quantile(RECO_smooth,0,na.rm = T),
              amp_RECO=max_RECO-min_RECO,
              value_for_amp10=0.1*amp_RECO+min_RECO)
  RECO2023_min<-30+which.min(as.numeric(abs(RECO.stats_2023$min_RECO - 
                                               df.RECO.use.2023_s$RECO_smooth[30:250])))-1
  
  #add lines:
  if(sitename=="CH-Dav"){
    
    p_plot<-p_plot+
      #for averaged years
      geom_segment(aes(x=RECOmean_min,y=0,
                       xend = RECOmean_min,
                       #-0.2-->is the adjustment for the height
                       yend = df.RECO.use.mean_s$RECO_mean_smooth[RECOmean_min]+0.26),size=1.1,
                   color = "black",lty=2)+
      #for year 2023:
      geom_segment(aes(x=RECO2023_min,y=0,
                       xend = RECO2023_min,
                       #-0.1-->is the adjustment for the height
                       yend = df.RECO.use.2023_s$RECO_smooth[RECO2023_min]+0.28),size=1.1,
                   color = "red",lty=2)
  }
  if(sitename=="DE-Tha"){
    p_plot<-p_plot+
      #for year 2023:
      geom_segment(aes(x=RECO2023_min,y=0,
                       xend = RECO2023_min,
                       #-0.1-->is the adjustment for the height
                       yend = df.RECO.use.2023_s$RECO_smooth[RECO2023_min]+0.05),size=1.1,
                   color = "red",lty=2)+
      #for averaged years
      geom_segment(aes(x=RECOmean_min,y=0,
                       xend = RECOmean_min,
                       #-0.2-->is the adjustment for the height
                       yend = df.RECO.use.mean_s$RECO_mean_smooth[RECOmean_min]+0.05),size=1.1,
                   color = "black",lty=2)
  }
  
  #
  if(legend_flag==FALSE){
    p_plot<-p_plot+
      theme(legend.position = "none")
  }
  
  #
  return(p_plot)
}

#
plot_Dav_RECO<-plot_fun_RECO_mean(df,"CH-Dav",FALSE)
plot_Tha_RECO<-plot_fun_RECO_mean(df,"DE-Tha",FALSE)

#
p_RECO<-plot_grid(plot_Dav_RECO,plot_Tha_RECO,
                 ncol=1,
                 align = "v",labels = c("(c)","(d)"))
#merge GPP and RECO
p_merge<-plot_grid(p_GPP,p_RECO,
                   ncol=2,
                   align = "hv")
#save the plots
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig2_supp1_EC_GPP.png"),
       p_merge,width = 18,height=15)




