##--------------------------------------------------------
#Aim: checking maximum EC fluxes(potential) in Dav and Tha
##--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(dplyr)

#-------------
#(1)load the data-using half-houlry data
#-------------
load.path<-"./data/EC_MeteoandFlux/"
load(paste0(load.path,"df_midday_from_ICOS.RDA"))

df.Dav<-df_midday$Dav_midday
df.Tha<-df_midday$Tha_midday

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
#3) get the maximum GPP, NEE or LE
#---------------------------
##--------------
#fast checking
##--------------
#Davos
# df %>%
#   filter(sitename=="CH-Dav")%>%
#   group_by(Year)%>%
#   ggplot(aes(x=DoY,y=GPP))+
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

#Tharandt
# df %>%
#   filter(sitename=="DE-Tha")%>%
#   group_by(Year)%>%
#   ggplot(aes(x=DoY,y=GPP))+
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

#--------
#tidy the midday fluxes and start to get the maximum values
#--------
#load the R function:
source("./R/max.filter.R")
#-----
#for GPP
#-----
tidy_GPP<-function(df,sitename){
  # df<-df
  # sitename<-"CH-Dav"
  
  if(sitename=="CH-Dav"){
    df.GPP.use<-df%>%
      filter(sitename=="CH-Dav")%>%
      filter(Year!=2009 & Year!=2011 & Year!=2013 & Year!=2019)%>%
      mutate(Year=as.factor(Year),
             Date_Time=ymd_hm(Date_Time)
             )
  }
  if(sitename=="DE-Tha"){
    df.GPP.use<-df%>%
      filter(sitename=="DE-Tha")%>%
      filter(Year!=1996 & Year!=2020 )%>%
      mutate(Year=as.factor(Year),
             Date_Time=ymd_hm(Date_Time))
  }
  ##get the maximum GPP:
  #filter options-->set the filter window as 3 days--90% percentile as the filter threshold
  filter.options<-data.frame(w=3,qt=0.9)
  temp<- max.filter(df.GPP.use,"GPP",act.opts = filter.options)
  #merge the original GPP and filtered GPP
  df.out<-left_join(df.GPP.use,temp)
  #
  df.out<-df.out %>%
    mutate(sitename=sitename)
  return(df.out)
  
}
df.Dav_GPP<-tidy_GPP(df,"CH-Dav")
df.Tha_GPP<-tidy_GPP(df,"DE-Tha")
#
df.GPP.max<-rbind(df.Dav_GPP,df.Tha_GPP)
#-----
#for NEE and LE
#-----
tidy_fluxes<-function(df,sitename,flux_name){
  # df<-df
  # sitename<-"CH-Dav"
  # flux_name<-"NEE"
  
  if(sitename=="CH-Dav"){
    df.use<-df%>%
      filter(sitename=="CH-Dav")%>%
      filter(Year!=2019)%>%
      mutate(Year=as.factor(Year))%>%
      select(Date_Time,Date,DoY,Year,flux_name)
    names(df.use)<-c("Date_Time","Date","DoY","Year",flux_name)
  }
  if(sitename=="DE-Tha"){
    df.use<-df%>%
      filter(sitename=="DE-Tha")%>%
      filter(Year!=1996 & Year!=2020 )%>%
      mutate(Year=as.factor(Year))%>%
      select(Date_Time,Date,DoY,Year,flux_name)
    names(df.use)<-c("Date_Time","Date","DoY","Year",flux_name)
  }
  
  ##get the maximum NEE/LE:
  #filter options-->set the filter window as 3 days--90% percentile as the filter threshold
  if(flux_name=="NEE"){
    filter.options<-data.frame(w=3,qt=0.1) #as negative NEE indicate the carbon uptake
  }
  if(flux_name=="LE"){
    filter.options<-data.frame(w=3,qt=0.9)
  }
  temp<- max.filter(df.use,flux_name,act.opts = filter.options)
  #merge the original GPP and filtered GPP
  df.out<-left_join(df.use,temp)
  #
  df.out<-df.out %>%
    mutate(sitename=sitename)
  return(df.out)
  
}
#-for NEE--
df.Dav_NEE<-tidy_fluxes(df,"CH-Dav","NEE")
df.Tha_NEE<-tidy_fluxes(df,"DE-Tha","NEE")
#
df.NEE.max<-rbind(df.Dav_NEE,df.Tha_NEE)

#-for LE--
df.Dav_LE<-tidy_fluxes(df,"CH-Dav","LE")
df.Tha_LE<-tidy_fluxes(df,"DE-Tha","LE")
#
df.LE.max<-rbind(df.Dav_LE,df.Tha_LE)

#---------------------------
#4) making the demonstration plot
#---------------------------
##---------
#For different fluxes:GPP, NEE, and LE
##---------
plot_fun_fluxes_mean<-function(df,sitename,flux_name){
  # df<-df.LE.max
  # sitename<-"DE-Tha"
  # flux_name<-"LE"
  
  if(sitename=="CH-Dav"){
    df.use<-df%>%
      filter(sitename=="CH-Dav")%>%
      select(DoY,Year,flux_name,paste0(flux_name,".max.filtered"))
    names(df.use)<-c("DoY","Year","y","y.max.filtered")
  }
  if(sitename=="DE-Tha"){
    df.use<-df%>%
      filter(sitename=="DE-Tha")%>%
      select(DoY,Year,flux_name,paste0(flux_name,".max.filtered"))
    names(df.use)<-c("DoY","Year","y","y.max.filtered")
  }
  
  df.use.mean<-df.use %>%
    select(DoY,y,y.max.filtered)%>%
    group_by(DoY)%>%
    dplyr::summarise(y_mean=mean(y),y_sd=sd(y),
              y.max_mean=mean(y.max.filtered),y.max_sd=sd(y.max.filtered)
              )
  # 
  p_plot<-ggplot()+
    # geom_ribbon(aes(x=DoY,ymin=y_mean-y_sd,ymax=y_mean+y_sd),
    #             data=df.use.mean,fill="gray",size=1.5)+
    geom_ribbon(aes(x=DoY,ymin=y.max_mean-y.max_sd,ymax=y.max_mean+y.max_sd),
                data=df.use.mean,fill="grey",size=1.5)+
    stat_smooth(aes(x=DoY,y=y.max.filtered, col=Year),data=df.use[df.use$Year!=2023,],se=FALSE)+
    # geom_line(aes(x=DoY,y=y_mean),data=df.y.use.mean,col=adjustcolor("black",0.8),size=2)+
    stat_smooth(aes(x=DoY,y=y.max_mean),data=df.use.mean,col=adjustcolor("black",0.8),size=2)+
    stat_smooth(aes(x=DoY,y=y.max.filtered),data=df.use[df.use$Year==2023,],
                col="orange",se=FALSE,size=2)+
    xlim(0,242)+
        theme(axis.title = element_text(size=24),
          axis.text = element_text(size = 22),
          legend.position =c(0.6,0.5),
          legend.background = "none"
    )+
    theme_bw()
  if(flux_name=="GPP"){
    p_plot<-p_plot+
      ylim(-2,30)+
      annotate(geom = "text",x=20,y=28,label=sitename,size=6)+
      annotate(geom = "segment",x=180,xend=190,y = 3,yend = 3,col="orange",size=1.5)+
      annotate(geom = "segment",x=180,xend=190,y = 1,yend = 1,col="black",size=1.5)+
      annotate(geom = "text",x=220,y=c(3,1),label=c("2023","Mean"),size=5)+
      ylab(expression("GPP"[max]*" ("*mu*"mol m"^-2*"s"^-1*")"))
  }
  
  if(flux_name=="NEE"){
    p_plot<-p_plot+
      ylim(-22,4)+
      geom_hline(yintercept = 0,lty=2,size=1.1)+
      annotate(geom = "text",x=20,y=3.9,label=sitename,size=6)+
      annotate(geom = "segment",x=180,xend=190,y = -21,yend = -21,col="orange",size=1.5)+
      annotate(geom = "segment",x=180,xend=190,y = -22,yend = -22,col="black",size=1.5)+
      annotate(geom = "text",x=220,y=c(-21,-22),label=c("2023","Mean"),size=5)+
      ylab(expression("NEE"[max]*" ("*mu*"mol m"^-2*"s"^-1*")"))
  }
  
  if(flux_name=="LE"){
    p_plot<-p_plot+
      ylim(0,350)+
      annotate(geom = "text",x=20,y=350,label=sitename,size=6)+
      annotate(geom = "segment",x=180,xend=190,y = 25,yend = 25,col="orange",size=1.5)+
      annotate(geom = "segment",x=180,xend=190,y = 5,yend = 5,col="black",size=1.5)+
      annotate(geom = "text",x=220,y=c(25,5),label=c("2023","Mean"),size=5)+
      ylab(expression("LE"[max]*" ( W m"^-2*"s"^-1*")"))
  }
  #
  return(p_plot)
}
##---------
#GPP 
##---------
#
plot_Dav<-plot_fun_fluxes_mean(df.GPP.max,"CH-Dav",'GPP')
plot_Tha<-plot_fun_fluxes_mean(df.GPP.max,"DE-Tha","GPP")
#
GPP_max_multiY_merge<-plot_grid(plot_Tha,plot_Dav,align = "h",labels = c("A","B"),nrow=1)

#save the plots
save.path<-"./manuscript/"
ggsave(GPP_max_multiY_merge,filename = paste("./manuscript/GPP_max_multiY_merge.png"),
       width = 12,height = 6)

#$######
#NEE
#$######
plot_Dav_NEE<-plot_fun_fluxes_mean(df.NEE.max,"CH-Dav","NEE")
plot_Tha_NEE<-plot_fun_fluxes_mean(df.NEE.max,"DE-Tha","NEE")
#
NEE_max_multiY_merge<-plot_grid(plot_Tha_NEE,plot_Dav_NEE,align = "h",labels = c("A","B"),nrow=1)
#save the plots
save.path<-"./manuscript/"
ggsave(NEE_max_multiY_merge,filename = paste("./manuscript/NEE_max_multiY_merge.png"),
       width = 12,height = 6)

#$######
#LE
#$######
plot_Dav_LE<-plot_fun_fluxes_mean(df.LE.max,"CH-Dav","LE")
plot_Tha_LE<-plot_fun_fluxes_mean(df.LE.max,"DE-Tha","LE")
#
LE_max_multiY_merge<-plot_grid(plot_Tha_LE,plot_Dav_LE,align = "h",labels = c("A","B"),nrow=1)
#save the plots
save.path<-"./manuscript/"
ggsave(LE_max_multiY_merge,filename = paste("./manuscript/LE_max_multiY_merge.png"),
       width = 12,height = 6)



