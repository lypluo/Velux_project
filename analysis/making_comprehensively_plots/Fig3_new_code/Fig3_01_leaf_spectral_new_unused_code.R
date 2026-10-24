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
df.Dav.Meteo_all<-df.Dav %>%
  dplyr::select(Date,TA_F,PPFD_IN,P_F)%>%
  mutate(TA=TA_F,TA_F=NULL,
         P=P_F,P_F=NULL)
df.Dav.Meteo<-df.Dav.Meteo_all%>%
  filter(Date %in% as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                   "2023-05-03","2023-05-22","2023-07-17")))%>%
  mutate(sitename="DAV",
         CampaignNum=paste0("C",1:6))
df.Tha.Meteo_all<-df.Tha %>%
  dplyr::select(Date,TA_F,PPFD_IN,P_F)%>%
  mutate(TA=TA_F,TA_F=NULL,
         P=P_F,P_F=NULL)
df.Tha.Meteo<-df.Tha.Meteo_all%>%
  filter(Date %in% as.Date(c("2023-03-02","2023-03-22","2023-04-13",
                             "2023-04-28","2023-05-17","2023-07-14")))%>%
  mutate(sitename="THA",
         CampaignNum=paste0("C",1:6))
#
df.sites.Meteo<-rbind(df.Dav.Meteo,df.Tha.Meteo)

#----------------------
#B.load the spectral meaurements 
#----------------------
load.path<-"data/"
load(paste0(load.path,"Polypen.data.cleaned.RDA"))
df.Poly.sepctra<-df.Poly.sel%>%
  mutate(Date=as.Date(Date))%>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="M") ~"Middle",
                            c(Position=="U") ~"Upper"))

#------------------
#(2)merge the data and change the Position names
#------------------
#.....

#------------------
#(3)plotting
#------------------
#addding mean and sd
plot_meansd_fun<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.Poly.sepctra
  # var_name<-"PRI"
  # legend.xy<-c(0.1,0.9)
  # arrow.xy<-data.frame(x=84,xend=84,y=-0.1,yend=-0.15)
  # arrow.flag<-FALSE
  
  df$y<-as.numeric(unlist(df[,var_name]))
  #
  df_THA<-df%>%
    filter(sitename=="THA")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)%>%
    ##summary the data
    group_by(sitename,Date,CampaignNum,Position)%>%
    summarise(y_mean=mean(y,na.rm = T),
              y_sd=sd(y,na.rm=T))
  df_DAV<-df%>%
    filter(sitename=="DAV")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)%>%
    ##summary the data
    group_by(sitename,Date,CampaignNum,Position)%>%
    summarise(y_mean=mean(y,na.rm = T),
              y_sd=sd(y,na.rm=T))
  #using data from Dav - Tha
  df_retidy<-left_join(df_THA,df_DAV,by=c("CampaignNum","Position"))%>%
    mutate(diff_y_mean=y_mean.y - y_mean.x,
           diff_y_sd=sqrt(y_sd.x^2+y_sd.y^2),
           diff_Date=round(as.numeric(c(yday(Date.x) + yday(Date.y))/2)))%>%
    dplyr::select(sitename.x,diff_Date,CampaignNum,Position,diff_y_mean,diff_y_sd)%>%
    mutate(sitename.x=NULL)
    ##
  # df_retidy<-df_retidy 
  # %>%filter(Position=="Upper")
  
    p_var_Date<-df_retidy%>%
      ggplot(aes(x=diff_Date,y=diff_y_mean,col=Position))+
      geom_point(size=3)+
      geom_line(size=1.1,lty=2)+
      geom_errorbar(aes(ymin = diff_y_mean - diff_y_sd, 
                        ymax = diff_y_mean + diff_y_sd), 
                    width = 2) +
      # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
      #              geom="pointrange",size=3,linewidth=4)+
      scale_color_manual(values = c("Lower"=adjustcolor("blue",0.9),
                                    "Middle"=adjustcolor("purple1",0.9),
                                    "Upper"=adjustcolor("steelblue2",0.9)))
  
  p_var_Date<-p_var_Date+
    # labs(fill = "Sitename")+
    xlab("DOY (2023)")+
    xlim(0,250)+
    #adding the 0:
    geom_hline(yintercept = 0,lty=2)+
    ##separate the campaigns-adding in Jan, 2024
    # geom_vline(xintercept = c(2.5,4.5,6.5,8.5,10.5),lty=2)+
    theme_light()+
    theme(axis.text = element_text(size=24),
          axis.title = element_text(size=28),
          # axis.text.x = element_text(angle = 35,hjust = 1),
          #increasing text size of facet grid labels
          strip.text.x = element_text(size=18),
          legend.text = element_text(size=22),
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
        color = "red", size = 0.8)
  }
  return(p_var_Date)
}

##-------------
#leaf spectral data
##-------------
#1) NDVI-->only for the upper layer
p_NDVI_Date<-plot_meansd_fun(df.Poly.sepctra,
                                   "NDVI",c(0.1,0.85),
                                   data.frame(x=84,xend=84,y=0.12,yend=0.08),
                                   TRUE)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" NDVI"[Needle])
  )
#2) PRI
p_PRI_Date<-plot_meansd_fun(df.Poly.sepctra,"PRI",c(0.1,0.85),
                                  data.frame(x=84,xend=84,y=0.05,yend=0),
                                  TRUE)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" PRI"[Needle])
  )
##
##adding the lines to indicate the field sampling dates:
Date.Dav<-as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                    "2023-05-03","2023-05-22","2023-07-17"))
Date.Tha<-as.Date(c("2023-03-02","2023-03-22","2023-04-13",
                    "2023-04-28","2023-05-17","2023-07-14"))
#
Date.Dav_DOY<-yday(Date.Dav)
Date.Tha_DOY<-yday(Date.Tha)

#
p_PRI_Date<-p_PRI_Date+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  annotate(geom = "text",x=c(64),y=rep(-0.14),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.14),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.14),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.14),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.14),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.14),label=c("C6"),size=6)

p_NDVI_Date<-p_NDVI_Date+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  annotate(geom = "text",x=c(64),y=rep(-0.15),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.15),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.15),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.15),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.15),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.15),label=c("C6"),size=6)

##-------------
#Meteo data
##-------------
df.Meteo<-df.sites.Meteo %>%
  mutate(doy=yday(Date))%>%
  mutate(sitename=ifelse(sitename=="DAV","CH-Dav","DE-Tha"))
#for Ta in 2023:
df.Tha.Meteo2023<-df.Tha.Meteo_all%>%
    filter(Date>=as.Date("2023-01-01"))%>%
    mutate(doy=yday(Date),
           sitename="DE-Tha")
df.Dav.Meteo2023<-df.Dav.Meteo_all%>%
  filter(Date>=as.Date("2023-01-01"))%>%
  mutate(doy=yday(Date),
         sitename="CH-Dav")
#
p_Meteo<-df.Meteo %>%
  ggplot(aes(x=doy,y=TA,col=sitename))+
  ##adding the continous temperature:
  geom_line(aes(x=doy,TA),
            data = df.Tha.Meteo2023,
            size=0.8,col=adjustcolor("orange",0.5))+
  geom_line(aes(x=doy,TA),
            data = df.Dav.Meteo2023,size=0.8,col=adjustcolor("red",0.5))+
  geom_point(size=5)+
  scale_color_manual(values = c("CH-Dav"=adjustcolor("red",0.8),
       "DE-Tha"=adjustcolor("orange",0.8)))+
  xlim(0,250)+
  ylab(expression("Ta ("* "°C)"))+
  xlab("DOY (2023)")+
  #adding snowing period
  geom_bar(aes(x=doy,y=PPFD_IN/50,fill=sitename),stat="identity")+
  scale_fill_manual(values = c("CH-Dav"=adjustcolor("red",0.5),
                              "DE-Tha"=adjustcolor("orange",0.5)))+
  #add second axix
  scale_y_continuous(
    sec.axis = sec_axis(~ . *50, 
    name =expression("PAR ("*mu*"mol m"^-2*s^-1*")")))+# 反向缩放
  # annotate(geom = "text",x=20,y=4,label="CH-Tha",size=6)+
  theme_light()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=22),
    legend.position = c(0.1,0.85),
    # legend.text = element_text(size = 16),
    # legend.title = element_text(size=18),
    # legend.background = element_blank(),
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank(),
    axis.text = element_text(size=24),
    axis.title = element_text(size=28))
##
p_Meteo<-p_Meteo+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  annotate(geom = "text",x=c(64),y=rep(-10),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-10),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-10),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-10),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-10),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-10),label=c("C6"),size=6)

###Merge plots:
p_leaf_spectra_NDVI<-p_NDVI_Date
p_NDVI_Meteo<-plot_grid(p_NDVI_Date,p_Meteo,
                nrow = 1)
##
p_leaf_spectra_PRI<-p_PRI_Date
p_PRI_Meteo<-plot_grid(p_PRI_Date,p_Meteo,
          nrow = 1
          # labels = c("","")
          )
# ggsave(paste0(save.path,"Fig2_temp.png"),p_leaf_physio,width = 12,height=15)
##save the ggplot plots:
save.path<-"./data/Comprehensive_plot_data/Fig3/"
#
save(p_leaf_spectra_NDVI,file=paste0(save.path,"p_leaf_spectra_NDVI.RDA"))
save(p_leaf_spectra_PRI,file=paste0(save.path,"p_leaf_spectra_PRI.RDA"))
#
save(p_NDVI_Meteo,file=paste0(save.path,"p_leaf_spectra_NDVI_new.RDA"))
save(p_PRI_Meteo,file=paste0(save.path,"p_leaf_spectra_PRI_new.RDA"))

