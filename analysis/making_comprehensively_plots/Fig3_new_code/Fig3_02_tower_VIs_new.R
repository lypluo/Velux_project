##################################
##Aim:tower-based NDVI and PRI variations in Tharandt and Davos
##################################
library(dplyr)
library(tidyverse)

#----------
#(1)load the data
#----------
##A.load the tower-based VIs
load.path<-"./data/Tower_based_VIs/"
load(paste0(load.path,"Davos_SIF_and_VIs.RDA"))
load(paste0(load.path,"Tharandt_VIs.RDA"))

#
df.Dav<-df.all.Dav%>%
  mutate(sitename="CH-Dav")
df.Tha<-df.all.Tha%>%
  mutate(sitename="DE-Tha")
#
df.all<-dplyr::bind_rows(df.Dav,df.Tha)

##B.load the Meteo data(especially snow data)
load(paste0("./data/EC_MeteoandFlux/","df.Meteo.daily.RDA"))
df.Dav.Meteo<-df.Meteo.daily$Dav%>%
  dplyr::select(Date,D_SNOW)%>%
  mutate(sitename="CH-Dav")
df.Tha.Meteo<-df.Meteo.daily$Tha%>%
  dplyr::select(Date,D_SNOW)%>%
  mutate(sitename="DE-Tha")
df.Meteo<-bind_rows(df.Dav.Meteo,df.Tha.Meteo)%>%
  #as the earliest VIs from Davos is from May,2021
  filter(Date>=as.Date("2021-05-01"))

#merge the VIs and Meteo:
df.all<-full_join(df.all,df.Meteo)
#test:
# df.all.t<-df.all[df.all$sitename=="DE-Tha",]
# ggplot()+
#   geom_point(aes(x=Date,y=PRI),data=df.all.t)+
#   geom_point(aes(x=Date,y=PRI.max.filtered),data = df.all.t,col="red")


#normlization the PRI with the 95% and 5% percentil value:
#the value ranges around 0-1
norm_01<-function(y){
  y_95<-quantile(y,0.95,na.rm=T)
  y_5<-quantile(y,0.05,na.rm=T)
  y_norm<-c(y-y_5)/c(y_95-y_5)
  return(y_norm)
}

####apply the filtering method
library(zoo)
source(file = "./R/max.filter.R")
df.all<-df.all%>%
  group_by(sitename)%>%
  mutate(PRI_norm=norm_01(PRI))
#test
# ggplot()+
#   geom_point(aes(x=Date,y=PRI_norm),data=df.all[df.all$sitename=="CH-Dav",])

df.filter_max_DE_Tha<-max.filter(df.all[df.all$sitename=="DE-Tha",],
                                 c("PRI_norm"),
                          act.opts = data.frame(w=7,qt=0.5))
df.filter_max_CH_Dav<-max.filter(df.all[df.all$sitename=="CH-Dav",],
                                 c("PRI_norm"),
                                 act.opts = data.frame(w=7,qt=0.5))
df.filter_max<-rbind(data.frame(df.filter_max_CH_Dav,"sitename"="CH-Dav"),
                     data.frame(df.filter_max_DE_Tha,"sitename"="DE-Tha"))
df.all<-left_join(df.all%>%mutate(Date=as.Date(Date)),
                  df.filter_max%>%mutate(Date=as.Date(Date)),by=c("sitename","Date"))
#test:
# df.all.tt<-df.all[df.all$sitename=="DE-Tha",]
# ggplot()+
#   geom_point(aes(x=Date,y=PRI_norm),data=df.all.tt)+
#   geom_point(aes(x=Date,y=PRI_norm.max.filtered),data = df.all.tt,col="red")
#----------
#(2)plotting
#----------
plot_fun<-function(df,var_name){
  # df<-df.all
  # var_name<-"PRI_norm"

  #
  df<-df%>%
    mutate(doy=yday(Date))
  df_sel<-df%>%
      dplyr::select(Date,var_name,paste0(var_name,".max.filtered"),sitename,doy)%>%
      filter(Date>=as.Date("2023-01-01") & doy<=300)
  names(df_sel)<-c("Date","y","y.max.filtered","sitename","doy")
  ## in Tharand as the VIs are only avaiable from April, 2023-->try to use the data
  ## in 2024 to illustrate the VI variation in Tharandt
  df_Tha_add<-df %>%
    dplyr::select(Date,var_name,paste0(var_name,".max.filtered"),sitename)%>%
    filter(sitename=="DE-Tha")%>%
    filter(Date>=as.Date("2024-01-01") & Date<=as.Date("2024-04-03"))%>%
    mutate(doy=yday(Date))
  names(df_Tha_add)<-c("Date","y","y.max.filtered","sitename","doy")
  df_Tha_add$pesudo_Date<-as.POSIXct(as.Date("2023-01-01")+yday(df_Tha_add$Date)-1)
  
  #plotting
  p_plot<-ggplot()+
    geom_point(aes(x=doy,y=y,col=sitename),data=df_sel)+
    scale_color_manual(values = c("CH-Dav"=adjustcolor("red",0.4),
                                  "DE-Tha"=adjustcolor("orange",0.4)))+
    geom_line(aes(doy,y.max.filtered),data=df_sel[df_sel$sitename=="DE-Tha",],
              col="orange",linewidth=1.1)+
    geom_line(aes(doy,y.max.filtered),data=df_sel[df_sel$sitename=="CH-Dav",],
              col="red",linewidth=1.1)+
    ##add additional data for Tharandt-->using 2024-01--2024-04 data-->using dashed line
    geom_point(aes(x=doy,y=y,col=sitename),data=df_Tha_add)+
    geom_line(aes(doy,y.max.filtered),data=df_Tha_add[df_Tha_add$sitename=="DE-Tha",],
              col="orange",linewidth=1.1,lty=2)+
    theme_light()+
    ylim(-0.2,1)+
    ylab(var_name)+
    xlab("")+
    theme(legend.position = c(0.8,0.25),
          legend.title = element_blank(),
          legend.text = element_text(size = 22),
          legend.background = element_blank(),
          axis.text = element_text(size=24),
          axis.title = element_text(size=28)
    )
  # if(var_name=="NDVI"){
  #
  
    p_plot<-p_plot+
      #adding snowing period
      geom_bar(aes(x=doy,y=D_SNOW/50),stat="identity",fill=adjustcolor("red",0.4),
               data = df %>% filter(sitename=="CH-Dav")%>%
                 filter(Date>=as.Date("2023-01-01") & doy<=300)%>%
                 filter(!is.na(D_SNOW) & D_SNOW>0))+
      #2023-DE-Tha
      geom_bar(aes(x=doy,y=D_SNOW/50),stat="identity",fill=adjustcolor("orange",0.6),
               data = df %>% filter(sitename=="DE-Tha")%>%
                 filter(Date>=as.Date("2023-01-01")& doy<=300)%>%
                 filter(!is.na(D_SNOW) & D_SNOW>0))+
      #add second axix
      scale_y_continuous(
        limits = c(-0.05,1.2),
        sec.axis = sec_axis(~ . *50, name = expression("D"[snow]*" (cm)" ))  # 反向缩放
      )
      #2024-DE-Tha
      # geom_bar(aes(x=Date,y=D_SNOW/50),stat="identity",fill=adjustcolor("orange",0.6),
      #          data = df %>% filter(sitename=="DE-Tha")%>%
      #            filter(Date>=as.Date("2024-01-01")&Date<=as.Date("2024-04-03"))%>%
      #            #pesudo Date
      #            mutate(Date=Date-365)%>%
      #            filter(!is.na(D_SNOW) & D_SNOW>0))
      # +
      # scale_fill_manual(values = c("CH-Dav"=adjustcolor("tomato",0.1),
      #                               "DE-Tha"=adjustcolor("cyan3",0.1)))
  # }
    #
    if(var_name=="PRI"){
    p_plot<-p_plot+
      ylim(-0.05,1.2)
    }
    if(var_name=="NDVI"){
      p_plot<-p_plot+
        ylim(-0.05,0.9)
    }
    return(p_plot)
}
##
p_NDVI<-plot_fun(df.all,"NDVI")+
  xlab("DOY (2023)")+
  ylab(expression("NDVI"[Crown]))
# plot_fun(df.all,"PRI") # the absolute PRI in two sites are different-->hence
# normalize it
p_PRI<-plot_fun(df.all,"PRI_norm")+
  ylab(expression("Norm PRI"[Crown]))+
  # theme(legend.position = "none")+
  xlab("DOY (2023)")
p_SIF<-plot_fun(df.all,"SIF_a_sfm")+
  theme(legend.position = "none")+
  ylab(expression("SIF (mW m"^-2*"nm "^-1*sr^-1*")"))
#further need to plot SIF/PAR-->the pattern does not look very clear
p_SIF_over_PAR<-plot_fun(df.all,"SIF_over_PAR")+
  theme(legend.position = "none")+
  ylab(expression("SIF/PAR (nm "^-1*")"))

#merge the plots:
library(cowplot)
p_crown_NDVI<-p_NDVI
# plot_grid(p_NDVI,nrow=1,
#                        labels = c("(b)"))
p_crown_PRI<-p_PRI
# plot_grid(p_PRI,nrow=1,
#                         labels = c("(b)"))
##adding the lines to indicate the field sampling dates:
Date.Dav<-as.Date(c("2023-03-08","2023-03-27","2023-04-21",
          "2023-05-03","2023-05-22","2023-07-17"))
Date.Tha<-as.Date(c("2023-03-02","2023-03-22","2023-04-13",
          "2023-04-28","2023-05-17","2023-07-14"))
#
Date.Dav_DOY<-yday(Date.Dav)
Date.Tha_DOY<-yday(Date.Tha)

#
p_crown_PRI<-p_crown_PRI+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  xlim(0,250)+
  annotate(geom = "text",x=c(64),y=rep(-0.05),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.05),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.05),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.05),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.05),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.05),label=c("C6"),size=6)

p_crown_NDVI<-p_crown_NDVI+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  xlim(0,250)+
  annotate(geom = "text",x=c(64),y=rep(-0.05),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.05),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.05),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.05),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.05),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.05),label=c("C6"),size=6)


#save the plot
save.path<-"./data/Comprehensive_plot_data/Fig3/"
save(p_crown_NDVI,file=paste0(save.path,"p_crown_NDVI_new.RDA"))
save(p_crown_PRI,file=paste0(save.path,"p_crown_PRI_new.RDA"))
