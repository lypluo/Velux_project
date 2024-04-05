##--------------------------------------------------------
#Aim: checking the variation of partioned T in Dav and Tha
##--------------------------------------------------------
#Weigeng conducted ET partioned by following method from Zhou et al., 2016
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(dplyr)

#-------------
#(1)load the partitioned daily data:
#-------------
data.path<-"./data-raw/Data_from_ICOS_sites/Partioned_T_fromET/"
#uWUEa:observed underlying water use efficiency 
df.Dav<-read.csv2(file = paste0(data.path,"CH_Dav_1997_2023_S.csv"),sep = ",")
df.Tha<-read.csv2(file = paste0(data.path,"DE_Tha_1996_2023_S.csv"),sep = ",")
#
df.Dav<-df.Dav %>%
  mutate(Date=ymd(Date),
         T=as.numeric(T..mm.d.),T..mm.d.=NULL,
         ET=as.numeric(ET..mm.d.),ET..mm.d.=NULL,
         sitename="CH-Dav")
df.Tha<-df.Tha %>%
  mutate(Date=ymd(Date),
         T=as.numeric(T..mm.d.),T..mm.d.=NULL,
         ET=as.numeric(ET..mm.d.),ET..mm.d.=NULL,
         sitename="DE-Tha")
#merge the data from two sites:
df<-rbind(df.Dav,df.Tha)
df<-df %>%
  mutate(DoY=yday(Date),
         Year=year(Date)
  )

#---------------------------
#3) plotting
#---------------------------
##--------------
#fast checking
##--------------
#Davos
df %>%
  filter(sitename=="CH-Dav")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=T))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

df %>%
  filter(sitename=="CH-Dav")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=ET))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

#Tharandt
df %>%
  filter(sitename=="DE-Tha")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=T))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

df %>%
  filter(sitename=="DE-Tha")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=ET))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

##---------
#ET and partioned T
##---------
plot_fun_fluxes_mean<-function(df,sitename,flux_name){
  # df<-df
  # sitename<-"CH-Dav"
  # flux_name<-"ET"
  
  if(sitename=="CH-Dav"){
    df.use<-df%>%
      filter(sitename=="CH-Dav")%>%
      #data has issue in 2019
      filter(Year!=2019)%>%
      mutate(Year=as.factor(Year))%>%
      select(DoY,Year,flux_name)
    names(df.use)<-c("DoY","Year","y")
  }
  if(sitename=="DE-Tha"){
    df.use<-df%>%
      filter(sitename=="DE-Tha")%>%
      #Tharandt has data issue before 100 in 1996
      filter(Year!=1996)%>%
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
  if(flux_name=="T"){
    p_plot<-p_plot+
      ylim(-0.1,1.7)+
      annotate(geom = "text",x=20,y=1.5,label=sitename,size=6)+
      annotate(geom = "segment",x=180,xend=190,y = 0.25,yend = 0.25,col="orange",size=1.5)+
      annotate(geom = "segment",x=180,xend=190,y = 0,yend = 0,col="black",size=1.5)+
      annotate(geom = "text",x=220,y=c(0.25,0),label=c("2023","Mean"),size=5)+
      ylab(expression("T ( mm d"^-1*")"))
  }
  if(flux_name=="ET"){
    p_plot<-p_plot+
      ylim(-0.2,5)+
      # geom_hline(yintercept = 0,lty=2,size=1.1)+
      annotate(geom = "text",x=20,y=4.5,label=sitename,size=6)+
      annotate(geom = "segment",x=180,xend=190,y = 0.5,yend = 0.5,col="orange",size=1.5)+
      annotate(geom = "segment",x=180,xend=190,y = 0,yend = 0,col="black",size=1.5)+
      annotate(geom = "text",x=220,y=c(0.5,0),label=c("2023","Mean"),size=5)+
      ylab(expression("ET ( mm d"^-1*")"))
  }
  #
  return(p_plot)
}
########
#ET
#$######
plot_Dav_ET<-plot_fun_fluxes_mean(df,"CH-Dav","ET")
plot_Tha_ET<-plot_fun_fluxes_mean(df,"DE-Tha","ET")

########
#T
#$######
plot_Dav_T<-plot_fun_fluxes_mean(df,"CH-Dav","T")
plot_Tha_T<-plot_fun_fluxes_mean(df,"DE-Tha","T")

##merge the plots:
ETandT_multiY_merge<-plot_grid(plot_Tha_ET,plot_Dav_ET,
                               plot_Tha_T,plot_Dav_T,
                               align = "hv",
                               labels = c("A","B","C","D"),nrow=2)
#save the plots
save.path<-"./manuscript/"
ggsave(ETandT_multiY_merge,filename = paste("./manuscript/ETandT_multiY_merge.png"),
       width = 12,height = 10)
