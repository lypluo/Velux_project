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
#D.load the spectral meaurements 
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
plot_point_fun_meansd<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.Poly.sepctra
  # var_name<-"PRI"
  # legend.xy<-c(0.1,0.1)
  # arrow.xy<-data.frame(x=4,xend=4,y=0.6,yend=0.65)
  # arrow.flag<-TRUE

  df$y<-as.numeric(unlist(df[,var_name]))
  #
  p_var_Date<-df%>%
    # filter(Position!="Middle")%>%
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
    theme(axis.text = element_text(size=20),
          axis.title = element_text(size=24),
          axis.text.x = element_text(angle = 35,hjust = 1),
          legend.text = element_text(size=20),
          legend.title = element_blank(),
          strip.text.x = element_text(size=22),
          legend.position = legend.xy,
          legend.background = element_blank()
    )
  if(var_name=="NDVI"){
    p_var_Date<-p_var_Date+
      annotate(geom = "text",x=c(1.5),y=rep(0.56),label=c("C1"),size=6)+
      annotate(geom = "text",x=c(3.5),y=rep(0.56),label=c("C2"),size=6)+
      annotate(geom = "text",x=c(5.5),y=rep(0.56),label=c("C3"),size=6)+
      annotate(geom = "text",x=c(7.5),y=rep(0.56),label=c("C4"),size=6)+
      annotate(geom = "text",x=c(9.5),y=rep(0.56),label=c("C5"),size=6)+
      annotate(geom = "text",x=c(11.5),y=rep(0.56),label=c("C6"),size=6)
  }
  if(var_name=="PRI"){
    p_var_Date<-p_var_Date+
      annotate(geom = "text",x=c(1.5),y=rep(-0.13),label=c("C1"),size=6)+
      annotate(geom = "text",x=c(3.5),y=rep(-0.13),label=c("C2"),size=6)+
      annotate(geom = "text",x=c(5.5),y=rep(-0.13),label=c("C3"),size=6)+
      annotate(geom = "text",x=c(7.5),y=rep(-0.13),label=c("C4"),size=6)+
      annotate(geom = "text",x=c(9.5),y=rep(-0.13),label=c("C5"),size=6)+
      annotate(geom = "text",x=c(11.5),y=rep(-0.13),label=c("C6"),size=6)
  }
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

##-------------
#leaf spectral data
##-------------
#1) NDVI
p_NDVI_Date<-plot_point_fun_meansd(df.Poly.sepctra,"NDVI",c(0.1,0.1),
                                   data.frame(x=4,xend=4,y=0.6,yend=0.65),
                                   TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("NDVI")
  )+
  theme(legend.position =c(0.3,0.35))
#2) PRI
p_PRI_Date<-plot_point_fun_meansd(df.Poly.sepctra,"PRI",c(0.1,0.1),
                                  data.frame(x=4,xend=4,y=0.1,yend=0.05),
                                  TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression("PRI")
  )+
  theme(legend.position = c(0.3,0.35))

###Merge plots:
p_leaf_spectra_NDVI<-p_NDVI_Date
# plot_grid(p_NDVI_Date,
#                 nrow = 1,
#                 labels = c("(a)"))
p_leaf_spectra_PRI<-p_PRI_Date
# plot_grid(p_PRI_Date,
#           nrow = 1,
#           labels = c("(a)"))
# ggsave(paste0(save.path,"Fig2_temp.png"),p_leaf_physio,width = 12,height=15)
##save the ggplot plots:
save.path<-"./data/Comprehensive_plot_data/Fig3/"
save(p_leaf_spectra_NDVI,file=paste0(save.path,"p_leaf_spectra_NDVI.RDA"))
save(p_leaf_spectra_PRI,file=paste0(save.path,"p_leaf_spectra_PRI.RDA"))


