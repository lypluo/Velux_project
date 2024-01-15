##--------------------------------------------------------
#Aim: checking EC fluxes in Dav and Tha
##--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(dplyr)

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
df %>%
  filter(sitename=="CH-Dav")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=GPP))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

df %>%
  filter(sitename=="CH-Dav")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=NEE))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

df %>%
  filter(sitename=="CH-Dav")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=LE))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

#Tharandt
df %>%
  filter(sitename=="DE-Tha")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=GPP))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

df %>%
  filter(sitename=="DE-Tha")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=NEE))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

df %>%
  filter(sitename=="DE-Tha")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=LE))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

##---------
#GPP 
##---------
plot_fun_GPP_mean<-function(df,sitename){
  # df<-df
  # sitename<-"CH-Dav"
  
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
    # geom_point(aes(x=DoY,y=GPP, col=as.factor(Year)),data=df.GPP.use[df.GPP.use$Year!=2023,])+
    geom_ribbon(aes(x=DoY,ymin=GPP_mean-GPP_sd,ymax=GPP_mean+GPP_sd),
                data=df.GPP.use.mean,fill="gray",size=1.5)+
    stat_smooth(aes(x=DoY,y=GPP, col=Year),data=df.GPP.use[df.GPP.use$Year!=2023,],se=FALSE)+
    # geom_line(aes(x=DoY,y=GPP_mean),data=df.GPP.use.mean,col=adjustcolor("black",0.8),size=2)+
    stat_smooth(aes(x=DoY,y=GPP_mean),data=df.GPP.use.mean,col=adjustcolor("black",0.8),size=2)+
    stat_smooth(aes(x=DoY,y=GPP),data=df.GPP.use[df.GPP.use$Year==2023,],
                col="orange",se=FALSE,size=2)+
    xlim(0,242)+
    ylim(-2,12)+
    annotate(geom = "text",x=20,y=11.5,label=sitename,size=6)+
    annotate(geom = "segment",x=180,xend=190,y = 1.5,yend = 1.5,col="orange",size=1.5)+
    annotate(geom = "segment",x=180,xend=190,y = 1,yend = 1,col="black",size=1.5)+
    annotate(geom = "text",x=220,y=c(1.5,1),label=c("2023","Mean"),size=5)+
    ylab(expression("GPP ("*mu*"mol m"^-2*"s"^-1*")"))+
    theme(axis.title = element_text(size=24),
          axis.text = element_text(size = 22),
          legend.position =c(0.6,0.5),
          legend.background = "none"
          )+
    theme_bw()
  #
  return(p_plot)
  
}
#
plot_Dav<-plot_fun_GPP_mean(df,"CH-Dav")
plot_Tha<-plot_fun_GPP_mean(df,"DE-Tha")
#
GPP_multiY_merge<-plot_grid(plot_Tha,plot_Dav,align = "h",labels = c("A","B"),nrow=1)

#save the plots
save.path<-"./manuscript/"
ggsave(GPP_multiY_merge,filename = paste("./manuscript/GPP_multiY_merge.png"),
       width = 12,height = 6)

##---------
#NEE and LE
##---------
plot_fun_fluxes_mean<-function(df,sitename,flux_name){
  # df<-df
  # sitename<-"DE-Tha"
  # flux_name<-"NEE"
  
  if(sitename=="CH-Dav"){
    df.use<-df%>%
      filter(sitename=="CH-Dav")%>%
      filter(Year!=2019)%>%
      mutate(Year=as.factor(Year))%>%
      select(DoY,Year,flux_name)
    names(df.use)<-c("DoY","Year","y")
  }
  if(sitename=="DE-Tha"){
    df.use<-df%>%
      filter(sitename=="DE-Tha")%>%
      filter(Year!=1996 & Year!=2020 )%>%
      mutate(Year=as.factor(Year))%>%
      select(DoY,Year,flux_name)
    names(df.use)<-c("DoY","Year","y")
  }
  
  df.use.mean<-df.use %>%
    select(DoY,y)%>%
    group_by(DoY)%>%
    dplyr::summarise(y_mean=mean(y),
              y_sd=sd(y))
  # 
  p_plot<-ggplot()+
    # geom_point(aes(x=DoY,y=GPP, col=as.factor(Year)),data=df.y.use[df.y.use$Year!=2023,])+
    geom_ribbon(aes(x=DoY,ymin=y_mean-y_sd,ymax=y_mean+y_sd),
                data=df.use.mean,fill="gray",size=1.5)+
    stat_smooth(aes(x=DoY,y=y, col=Year),data=df.use[df.use$Year!=2023,],se=FALSE)+
    # geom_line(aes(x=DoY,y=y_mean),data=df.y.use.mean,col=adjustcolor("black",0.8),size=2)+
    stat_smooth(aes(x=DoY,y=y_mean),data=df.use.mean,col=adjustcolor("black",0.8),size=2)+
    stat_smooth(aes(x=DoY,y=y),data=df.use[df.use$Year==2023,],
                col="orange",se=FALSE,size=2)+
    xlim(0,242)+
        theme(axis.title = element_text(size=24),
          axis.text = element_text(size = 22),
          legend.position =c(0.6,0.5),
          legend.background = "none"
    )+
    theme_bw()
  if(flux_name=="LE"){
    p_plot<-p_plot+
      ylim(0,135)+
      annotate(geom = "text",x=20,y=125,label=sitename,size=6)+
      annotate(geom = "segment",x=180,xend=190,y = 15,yend = 15,col="orange",size=1.5)+
      annotate(geom = "segment",x=180,xend=190,y = 5,yend = 5,col="black",size=1.5)+
      annotate(geom = "text",x=220,y=c(15,5),label=c("2023","Mean"),size=5)+
      ylab(expression("LE ( W m"^-2*"s"^-1*")"))
  }
  if(flux_name=="NEE"){
    p_plot<-p_plot+
      ylim(-5.5,4)+
      geom_hline(yintercept = 0,lty=2,size=1.1)+
      annotate(geom = "text",x=20,y=3.9,label=sitename,size=6)+
      annotate(geom = "segment",x=180,xend=190,y = -4.7,yend = -4.7,col="orange",size=1.5)+
      annotate(geom = "segment",x=180,xend=190,y = -4.95,yend = -4.95,col="black",size=1.5)+
      annotate(geom = "text",x=220,y=c(-4.7,-4.95),label=c("2023","Mean"),size=5)+
      ylab(expression("NEE ("*mu*"mol m"^-2*"s"^-1*")"))
  }
  #
  return(p_plot)
}
#$######
#NEE
#$######
plot_Dav_NEE<-plot_fun_fluxes_mean(df,"CH-Dav","NEE")
plot_Tha_NEE<-plot_fun_fluxes_mean(df,"DE-Tha","NEE")
#
NEE_multiY_merge<-plot_grid(plot_Tha_NEE,plot_Dav_NEE,align = "h",labels = c("A","B"),nrow=1)
#save the plots
save.path<-"./manuscript/"
ggsave(NEE_multiY_merge,filename = paste("./manuscript/NEE_multiY_merge.png"),
       width = 12,height = 6)

#$######
#LE
#$######
plot_Dav_LE<-plot_fun_fluxes_mean(df,"CH-Dav","LE")
plot_Tha_LE<-plot_fun_fluxes_mean(df,"DE-Tha","LE")
#
LE_multiY_merge<-plot_grid(plot_Tha_LE,plot_Dav_LE,align = "h",labels = c("A","B"),nrow=1)
#save the plots
save.path<-"./manuscript/"
ggsave(LE_multiY_merge,filename = paste("./manuscript/LE_multiY_merge.png"),
       width = 12,height = 6)






