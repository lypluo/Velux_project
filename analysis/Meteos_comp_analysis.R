##############################################
#Aim: comparing the meterological condition in Dav and Tha
##############################################
#note: At present, the snow depth in Dav has some probelm
library(tidyverse)
library(dplyr)

#---------
#(1)load the data
#---------
load.path<-"data/EC_MeteoandFlux/"
load(paste0(load.path,"df.Meteo.daily.RDA"))

#tidy the data:
df.Dav<-df.Meteo.daily$Dav
df.Tha<-df.Meteo.daily$Tha

names(df.Dav)
names(df.Tha)

#----------
#(2)select the variables in two dataset and do the comparison:
#----------
##normalization the SWC:
SWC.Dav.q95<-quantile(df.Dav$SWC_1,0.95,na.rm = T)
SWC.Tha.q95<-quantile(df.Tha$SWC_F_MDS_1,0.95,na.rm = T)

df.Dav_sel<-df.Dav %>%
  select(Date,SW_IN:VPD,
         #only select the first layer SWC and TS
         SWC_1,TS_1,D_SNOW
         )%>%
  #normalization of SWC
  mutate(SWC_Norm=SWC_1/SWC.Dav.q95)%>%
  mutate(sitename="DAV")

df.Tha_sel<-df.Tha%>%
  select(Date,TA_F:PPFD_DIF,TS_F_MDS_1,SWC_F_MDS_1,D_SNOW,
        TA=TA_F,TA_F=NULL,
        SW_IN=SW_IN_F,SW_IN_F=NULL,
        VPD=VPD_F,VPD_F=NULL,
        TS_1=TS_F_MDS_1,TS_F_MDS_1=NULL,
        SWC_1=SWC_F_MDS_1,SWC_F_MDS_1=NULL,
        P_F=NULL
        )%>%
  #normalization of SWC
  mutate(SWC_Norm=SWC_1/SWC.Tha.q95)%>%
  mutate(sitename="THA")

#
df.all<-bind_rows(df.Tha_sel,df.Dav_sel)

#----------
#(3)plotting:
#----------
#function:
plot_fun<-function(df,var,x_f,legend_f){
  # df<-df.all
  # var<-"TA"
  # x_f=FALSE
  # legend_f=FALSE
  
  #
  df_sel<-df %>%
    filter(Date>=as.Date("2023-01-01")&Date<=as.Date("2023-09-01"))%>%
    select(Date,var,sitename)
  names(df_sel)<-c("Date","y","sitename")
  #plotting
  #
  df_sel_Tha_Samp<-df_sel %>%
    filter(sitename=="THA")%>%
    filter(Date %in% as.Date(c("2023-03-02","2023-03-22","2023-04-13",
                               "2023-04-28","2023-05-17","2023-07-14")))
  df_sel_Dav_Samp<-df_sel %>%
    filter(sitename=="DAV")%>%
    filter(Date %in% as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                               "2023-05-03","2023-05-22","2023-07-17")))
  
  
  df_plot<-ggplot(aes(Date,y,col=sitename),data=df_sel)+
    geom_point()+
    ylab("")+
    xlab("2023")+
    geom_point(aes(Date,y),data=df_sel_Tha_Samp,col="cyan4",size=5)+
    geom_point(aes(Date,y),data=df_sel_Dav_Samp,col="tomato",size=5)+
    scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.6),
                       "THA"=adjustcolor("cyan3",0.6)))+
    theme_light()+
    theme(legend.position = c(0.8,0.25),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          legend.background = element_blank(),
          axis.text = element_text(size=16)
          )
  ##########
  if(var=="TA"){
    df_plot<-df_plot+
      ylab(expression("Ta ("* "°C)"))+
      annotate(geom="text",x=df_sel_Tha_Samp$Date,y=df_sel_Tha_Samp$y+2,
               label=paste0("C",1:6),col="cyan4",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$Date,y=df_sel_Dav_Samp$y+2,
               label=paste0("C",1:6),col="tomato",size=5)
  }
  if(var=="TS_1"){
    df_plot<-df_plot+
      ylab(expression("Ts ("* "°C)"))+
      annotate(geom="text",x=df_sel_Tha_Samp$Date,y=df_sel_Tha_Samp$y+2,
               label=paste0("C",1:6),col="cyan4",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$Date,y=df_sel_Dav_Samp$y+2,
               label=paste0("C",1:6),col="tomato",size=5)
  }
  if(var=="SW_IN"){
    df_plot<-df_plot+
      ylab(expression("SW_IN (W m"^-2*")"))
  }
  if(var=="VPD"){
    df_plot<-df_plot+
      ylab(expression("VPD (hPa)"))
  }
  if(var=="PPFD_IN"|var=="PPFD_OUT"|var=="PPFD_DIF"){
    df_plot<-df_plot+
      ylab(expression("PPFD ("*mu*"mol m"^-2*s^-1))+
      annotate(geom="text",x=df_sel_Tha_Samp$Date,y=df_sel_Tha_Samp$y+50,
               label=paste0("C",1:6),col="cyan4",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$Date,y=df_sel_Dav_Samp$y+50,
               label=paste0("C",1:6),col="tomato",size=5)
  }
  if(var=="SWC_1"){
    df_plot<-df_plot+
      ylab(expression("SWC"))+
      annotate(geom="text",x=df_sel_Tha_Samp$Date,y=df_sel_Tha_Samp$y+2,
               label=paste0("C",1:6),col="cyan4",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$Date,y=df_sel_Dav_Samp$y+2,
               label=paste0("C",1:6),col="tomato",size=5)
  }
  if(var=="SWC_Norm"){
    df_plot<-df_plot+
      ylab(expression("Norm SWC"))+
      annotate(geom="text",x=df_sel_Tha_Samp$Date,y=df_sel_Tha_Samp$y+0.02,
               label=paste0("C",1:6),col="cyan4",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$Date,y=df_sel_Dav_Samp$y+0.02,
               label=paste0("C",1:6),col="tomato",size=5)
  }
  if(var=="D_SNOW"){
    df_plot<-df_plot+
      ylab(expression("Snow depth (cm)"))+
      annotate(geom="text",x=df_sel_Tha_Samp$Date,y=df_sel_Tha_Samp$y+2,
               label=paste0("C",1:6),col="cyan4",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$Date,y=df_sel_Dav_Samp$y+2,
               label=paste0("C",1:6),col="tomato",size=5)
  }
  #########
  if(x_f==FALSE){
    df_plot<-df_plot+
      xlab("")
  }
  if(legend_f==FALSE){
    df_plot<-df_plot+
      theme(legend.position = "NA")
    
  }
  return(df_plot)
}
#
p_TA<-plot_fun(df.all,"TA",FALSE,TRUE)
p_SW_IN<-plot_fun(df.all,"SW_IN",FALSE,FALSE)
p_VPD<-plot_fun(df.all,"VPD",FALSE,FALSE)
p_PAR<-plot_fun(df.all,"PPFD_IN",FALSE,FALSE)
p_TS<-plot_fun(df.all,"TS_1",FALSE,FALSE)
p_SWC<-plot_fun(df.all,"SWC_1",FALSE,FALSE)
p_SWC_Norm<-plot_fun(df.all,"SWC_Norm",FALSE,FALSE)
p_Snow<-plot_fun(df.all,"D_SNOW",TRUE,FALSE)
#merge the plots:
library(cowplot)
plot_merge<-plot_grid(p_TA,p_PAR,p_TS,p_SWC_Norm,p_Snow,ncol=1,align = "hv")
#save the plot
save.path<-"./manuscript/"
ggsave(plot_merge,filename = paste0(save.path,"Meteo.png"),height = 11,width = 12)

