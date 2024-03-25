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
load(paste0(load.path,"Tharandt_SIF_and_VIs.RDA"))

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
df.all<-left_join(df.all,df.Meteo)
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

df.filter_max_DE_Tha<-max.filter(df.all[df.all$sitename=="DE-Tha",],c("PRI_norm"),
                          act.opts = data.frame(w=7,qt=0.5))
df.filter_max_CH_Dav<-max.filter(df.all[df.all$sitename=="CH-Dav",],c("PRI_norm"),
                                 act.opts = data.frame(w=7,qt=0.5))
df.filter_max<-rbind(data.frame(df.filter_max_CH_Dav,"sitename"="CH-Dav"),
                     data.frame(df.filter_max_DE_Tha,"sitename"="DE-Tha"))
df.all<-left_join(df.all,df.filter_max)
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
  # var_name<-"NDVI"

  #
  df_sel<-df%>%
      dplyr::select(Date,var_name,paste0(var_name,".max.filtered"),sitename)%>%
      filter(Date>=as.Date("2023-01-01") & Date<=as.Date("2023-10-31"))
  names(df_sel)<-c("Date","y","y.max.filtered","sitename")

  
  #plotting
  p_plot<-df_sel%>%
    ggplot()+
    geom_point(aes(x=Date,y=y,col=sitename))+
    scale_color_manual(values = c("CH-Dav"=adjustcolor("tomato",0.4),
                                  "DE-Tha"=adjustcolor("cyan3",0.4)))+
    geom_line(aes(Date,y.max.filtered),data=df_sel[df_sel$sitename=="DE-Tha",],
              col="cyan4",linewidth=1.1)+
    geom_line(aes(Date,y.max.filtered),data=df_sel[df_sel$sitename=="CH-Dav",],
              col="tomato",linewidth=1.1)+
    theme_light()+
    ylab(var_name)+
    xlab("")+
    theme(legend.position = c(0.9,0.25),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          legend.background = element_blank(),
          axis.text = element_text(size=20),
          axis.title = element_text(size=24)
    )
  if(var_name=="NDVI"){
    p_plot<-p_plot+
      #adding snowing period
      geom_bar(aes(x=Date,y=D_SNOW/50),stat="identity",fill=adjustcolor("tomato",0.4),
               data = df.all %>% filter(sitename=="CH-Dav")%>%
                 filter(Date<=as.Date("2023-10-31"))%>%
                 filter(!is.na(D_SNOW) & D_SNOW>0))+
      geom_bar(aes(x=Date,y=D_SNOW/50),stat="identity",fill=adjustcolor("cyan3",0.6),
               data = df.all %>% filter(sitename=="DE-Tha")%>%
                 filter(Date<=as.Date("2023-10-31"))%>%
                 filter(!is.na(D_SNOW) & D_SNOW>0))
      # +
      # scale_fill_manual(values = c("CH-Dav"=adjustcolor("tomato",0.1),
      #                               "DE-Tha"=adjustcolor("cyan3",0.1)))
  }
    return(p_plot)
}
##
p_NDVI<-plot_fun(df.all,"NDVI")
p_PRI<-plot_fun(df.all,"PRI_norm")+
  ylab("Norm PRI")+
  ylim(0,1.2)+
  theme(legend.position = "none")
p_SIF<-plot_fun(df.all,"SIF_a_sfm")+
  theme(legend.position = "none")+
  ylab(expression("SIF (mW m"^-2*"nm "^-1*sr^-1*")"))
#further need to plot SIF/PAR-->the pattern does not look very clear
p_SIF_over_PAR<-plot_fun(df.all,"SIF_over_PAR")+
  theme(legend.position = "none")+
  ylab(expression("SIF/PAR (nm "^-1*")"))

#merge the plots:
library(cowplot)
p_merge<-plot_grid(p_NDVI,p_PRI,p_SIF,nrow=3)

#save the plot
save.path<-"./manuscript/Ecosystem_observations/"
ggsave(p_merge,filename = paste0(save.path,"tower_VIs.png"),height = 11,width = 12)
