##--------------------------------------------------------
#Aim: checking EC fluxes and calculate Gs in Dav and Tha
##--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(dplyr)
#
library(bigleaf)

#-------------
#(1)load the data-using daily data
#-------------
load.path<-"./data/EC_MeteoandFlux/"
load(paste0(load.path,"df_HH_from_ICOS.RDA"))

df.Dav<-df_HH$Dav
df.Tha<-df_HH$Tha

#
df.Dav<-df.Dav %>%
  mutate(rDate=ymd_hm(Date_Time),Date_Time=NULL)%>%
  mutate(sitename="CH-Dav")
df.Tha<-df.Tha %>%
  mutate(rDate=ymd_hm(Date_Time),Date_Time=NULL)%>%
  mutate(sitename="DE-Tha")

#-------------------
#(2) tidy the data and calculate the Gc(canopy conductance)
#-------------------
#filter the data-->remove the 48 hour data after rainfall before calculate Gs:
#calculated Gs-->proxy for the Gc(canopy conductance)
df.Dav_f<-filter.data(df.Dav,quality.control = FALSE,
                      filter.precip = T,
                      precip = "P",tprecip = 0.2,
                      precip.hours = 24,records.per.hour = 2)
df.Tha_f<-filter.data(df.Tha,quality.control = FALSE,
                      filter.precip = T,
                      precip = "P",tprecip = 0.2,
                      precip.hours = 24,records.per.hour = 2)
#agg to daily:
df.Dav_f_daily<-df.Dav_f %>%
  group_by(Date)%>%
  summarize(P=sum(P),
    across(c(NEE:WS,Pressure),function(x){mean(x,na.rm = T)}))%>%
  mutate(sitename="CH-Dav")
#
df.Tha_f_daily<-df.Tha_f %>%
  group_by(Date)%>%
  summarize(P=sum(P),
            across(c(NEE:WS,Pressure),function(x){mean(x,na.rm = T)}))%>%
  mutate(sitename="DE-Tha")

df<-rbind(df.Dav_f_daily,df.Tha_f_daily)
#---merge the data according to doy
df<-df %>%
  mutate(DoY=yday(Date),
         Year=year(Date)
  )
#calculate the Gs-->using the surface.conductance function in bigleaf R package:
df<-df%>%
  #calculate Gs based on a simple gradient approach
  mutate(surface.conductance(LE=LE,Tair = TA,pressure = Pressure,
                             VPD = VPD,formulation = "Flux-Gradient"))%>%
  #calculate ET
  mutate(ET=LE.to.ET(LE,TA))

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
# 
# #Tharandt
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

##---------
#LE and Gs
##---------
plot_fun_fluxes_mean<-function(df,sitename,flux_name,legend_flag){
  # df<-df
  # sitename<-"DE-Tha"
  # flux_name<-"Gs_mol"
  # legend_flag<-F
  # 
  if(sitename=="CH-Dav"){
    df.use<-df%>%
      filter(sitename=="CH-Dav")%>%
      filter(Year!=2019)%>%
      mutate(Year=as.factor(Year))%>%
      dplyr::select(DoY,Year,flux_name)
    names(df.use)<-c("DoY","Year","y")
  }
  if(sitename=="DE-Tha"){
    df.use<-df%>%
      filter(sitename=="DE-Tha")%>%
      filter(Year!=1996 & Year!=2020 )%>%
      mutate(Year=as.factor(Year))%>%
      dplyr::select(DoY,Year,flux_name)
    names(df.use)<-c("DoY","Year","y")
  }
  
  df.use.mean<-df.use %>%
    dplyr::select(DoY,y)%>%
    filter(!is.na(y))%>%
    group_by(DoY)%>%
    dplyr::summarise(y_mean=mean(y,na.rm = T),
              y_sd=sd(y,na.rm=T))
  # 
  p_plot<-ggplot()+
    # geom_point(aes(x=DoY,y=GPP, col=as.factor(Year)),data=df.y.use[df.y.use$Year!=2023,])+
    geom_ribbon(aes(x=DoY,ymin=y_mean-y_sd,ymax=y_mean+y_sd),
                data=df.use.mean,fill="gray",size=1.5)+
    stat_smooth(aes(x=DoY,y=y, col=Year),data=df.use[df.use$Year!=2023,],se=FALSE)+
    # geom_line(aes(x=DoY,y=y_mean),data=df.y.use.mean,col=adjustcolor("black",0.8),size=2)+
    stat_smooth(aes(x=DoY,y=y_mean),data=df.use.mean,col=adjustcolor("black",0.8),size=2)+
    stat_smooth(aes(x=DoY,y=y),data=df.use[df.use$Year==2023,],
                col="red",se=FALSE,size=2)+
    xlab("DOY")+
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
      annotate(geom = "segment",x=180,xend=190,y = 15,yend = 15,col="red",size=1.5)+
      annotate(geom = "segment",x=180,xend=190,y = 5,yend = 5,col="black",size=1.5)+
      annotate(geom = "text",x=220,y=c(15,5),label=c("2023","Mean"),size=5)+
      ylab(expression("LE ( W m"^-2*"s"^-1*")"))
  }
  if(flux_name=="ET"){
    p_plot<-p_plot+
      ylim(-0.2e-05,5e-05)+
      # geom_hline(yintercept = 0,lty=2,size=1.1)+
      annotate(geom = "text",x=20,y=5e-05,label=sitename,size=6)+
      annotate(geom = "segment",x=180,xend=190,y = 1e-05,yend = 1e-05,col="red",size=1.5)+
      annotate(geom = "segment",x=180,xend=190,y = 0.5e-05,yend = 0.5e-05,col="black",size=1.5)+
      annotate(geom = "text",x=220,y=c(1e-05,0.5e-05),label=c("2023","Mean"),size=5)+
      ylab(expression("ET (kg m"^-2*"s"^-1*")"))+
      theme(axis.title = element_text(size=20),
            axis.text = element_text(size = 16),
            legend.position = c(0.3,0.75),
            legend.title = element_text(size=18),
            legend.text = element_text(size=16),
            legend.background = element_blank())+
      guides(color = guide_legend(ncol = 3))   # 设置图例为4列
  }
  if(flux_name=="Gs_mol"){
    p_plot<-p_plot+
      ylim(-0.1,0.2)+
      geom_hline(yintercept = 0,lty=2,size=1.1)+
      annotate(geom = "text",x=20,y=0.2,label=sitename,size=6)+
      annotate(geom = "segment",x=180,xend=190,y = -0.02,yend = -0.02,col="red",size=1.5)+
      annotate(geom = "segment",x=180,xend=190,y = -0.05,yend = -0.05,col="black",size=1.5)+
      annotate(geom = "text",x=220,y=c(-0.02,-0.05),label=c("2023","Mean"),size=5)+
      ylab(expression("Gc (mol m"^-2*"s"^-1*")"))+
      theme(axis.title = element_text(size=20),
            axis.text = element_text(size = 16),
            legend.position = c(0.3,0.75),
            legend.title = element_text(size=18),
            legend.text = element_text(size=16),
            legend.background = element_blank())+
      guides(color = guide_legend(ncol = 3))   # 设置图例为4列
  }
  #
  #add lines:
  # if(sitename=="DE-Tha" & flux_name=="ET"){
  #   p_plot<-p_plot+
  #     #for averaged years
  #     geom_segment(aes(x=25,y=0.3e-05,xend = 25,yend = 0),size=1.05,
  #                  color = "black",lty=2)+
  #     #for year 2023:
  #     geom_segment(aes(x=30,y=0.3e-05,xend = 30,yend = 0),size=1.05,
  #                  color = "red",lty=2)
  # }
  # if(sitename=="CH-Dav"& flux_name=="ET"){
  #   p_plot<-p_plot+
  #     #for averaged years
  #     geom_segment(aes(x=30,y=0.6e-05,xend = 30,yend = 0),size=1.1,
  #                  color = "black",lty=2)+
  #     #for year 2023:
  #     geom_segment(aes(x=38,y=0.3e-05,xend = 38,yend = 0),size=1.1,
  #                  color = "red",lty=2)
  # }
  #
  if(legend_flag==FALSE){
    p_plot<-p_plot+
      theme(legend.position = "none")
  }
  
  #
  return(p_plot)
}

#$######
#ET
#$######
plot_Dav_ET<-plot_fun_fluxes_mean(df,"CH-Dav","ET",TRUE)+
  xlab("")+
  theme(axis.title.x = element_blank())
plot_Tha_ET<-plot_fun_fluxes_mean(df,"DE-Tha","ET",FALSE)+
  # theme(axis.title.x = element_blank())+
  xlab("DOY")
#
plot_grid(plot_Dav_ET,plot_Tha_ET,align = "h",nrow=1)

#$######
#Gs-->Gc
#$######
plot_Dav_Gs<-plot_fun_fluxes_mean(df,"CH-Dav","Gs_mol",FALSE)+
  xlab("")
plot_Tha_Gs<-plot_fun_fluxes_mean(df,"DE-Tha","Gs_mol",FALSE)
#
plot_grid(plot_Dav_Gs,plot_Tha_Gs,align = "h",labels = c("(a)","(b)"),nrow=1)

p_Eco_ET_Gs<-plot_grid(plot_Dav_ET,plot_Dav_Gs,plot_Tha_ET,plot_Tha_Gs,
                       align = "hv",
                       labels = c("(f)","(g)","(h)","(i)"),nrow=2)

###save the ggplot plots:
save.path<-"./data/Comprehensive_plot_data/Fig4/"
save(p_Eco_ET_Gs,file=paste0(save.path,"p_Eco_ET_Gs.RDA"))

