##--------------------------------------------------------
#Aim: checking EC fluxes in Dav and Tha
##--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)

#-------------
#(1)load the data
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

##---------
#calculate the mean 
##---------
# summarise(GPP_mean=mean(GPP),NEE_mean=mean(NEE),LE_mean=mean(LE),
#           SW_IN_mean=mean(SW_IN),PPFD_IN_mean=mean(PPFD_IN),
#           TA_mean=mean(TA),VPD_mean=mean(VPD),P_mean=mean(P)
# )
plot_fun_mean<-function(df,sitename){
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
    summarise(GPP_mean=mean(GPP),
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
    ylab(expression("GPP ("*mu*"mol m"^-2*"s"^-1*")"))+
    theme(axis.title = element_text(size=22),
          axis.text = element_text(size = 18),
          legend.position =c(0.6,0.5),
          legend.background = "none"
          )+
    theme_bw()
  #
  return(p_plot)
  
}
#
plot_Dav<-plot_fun_mean(df,"CH-Dav")
plot_Tha<-plot_fun_mean(df,"DE-Tha")
#
plot_grid(plot_Tha,plot_Dav,align = "h",labels = c("A","B"),nrow=1)
