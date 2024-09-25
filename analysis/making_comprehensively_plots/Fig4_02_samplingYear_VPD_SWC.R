##############################################
#Aim: comparing the meterological condition in Dav and Tha
##############################################
#note: At present, the snow depth in Dav has some probelm
library(tidyverse)
library(dplyr)
library(cowplot)
library(dplyr)
#---------
#(1)load the data
#---------
load.path<-"data/EC_MeteoandFlux/"
load(paste0(load.path,"df.Meteo_andFluxes.daily.RDA"))

#tidy the data:
df.Dav<-df.Meteo.daily$Dav
df.Tha<-df.Meteo.daily$Tha

names(df.Dav)
names(df.Tha)

#----------
#(2)select the variables in two dataset and do the comparison:
#----------
##normalization the SWC:
SWC.Dav.q95<-quantile(df.Dav[df.Dav$Date>=as.Date("2023-01-01"),]$SWC_F_MDS_1,0.95,na.rm = T)
SWC.Tha.q95<-quantile(df.Tha[df.Tha$Date>=as.Date("2023-01-01"),]$SWC_F_MDS_1,0.95,na.rm = T)

df.Dav_sel<-df.Dav %>%
  dplyr::select(Date,TA_F:PPFD_DIF,TS_F_MDS_1,SWC_F_MDS_1,D_SNOW,
         TA=TA_F,TA_F=NULL,
         SW_IN=SW_IN_F,SW_IN_F=NULL,
         VPD=VPD_F,VPD_F=NULL,
         TS_1=TS_F_MDS_1,TS_F_MDS_1=NULL,
         SWC_1=SWC_F_MDS_1,SWC_F_MDS_1=NULL,
         #change precipitation name
         P=P_F,P_F=NULL,
         #adding GPP,NEE,RECO, and LE->Jan,2024
         NEE_VUT_REF:LE_F_MDS,
         NEE=NEE_VUT_REF,NEE_VUT_REF=NULL,
         RECO=RECO_NT_VUT_REF,RECO_NT_VUT_REF=NULL,
         GPP=GPP_NT_VUT_REF,GPP_NT_VUT_REF=NULL,
         LE=LE_F_MDS,LE_F_MDS=NULL
  )%>%
  #normalization of SWC
  mutate(SWC_Norm=SWC_1/SWC.Dav.q95)%>%
  mutate(sitename="DAV")

df.Tha_sel<-df.Tha%>%
  dplyr::select(Date,TA_F:PPFD_DIF,TS_F_MDS_1,SWC_F_MDS_1,D_SNOW,
        TA=TA_F,TA_F=NULL,
        SW_IN=SW_IN_F,SW_IN_F=NULL,
        VPD=VPD_F,VPD_F=NULL,
        TS_1=TS_F_MDS_1,TS_F_MDS_1=NULL,
        SWC_1=SWC_F_MDS_1,SWC_F_MDS_1=NULL,
        #change precipitation name
        P=P_F,P_F=NULL,
        #adding GPP,NEE,RECO, and LE->Jan,2024
        NEE_VUT_REF:LE_F_MDS,
        NEE=NEE_VUT_REF,NEE_VUT_REF=NULL,
        RECO=RECO_NT_VUT_REF,RECO_NT_VUT_REF=NULL,
        GPP=GPP_NT_VUT_REF,GPP_NT_VUT_REF=NULL,
        LE=LE_F_MDS,LE_F_MDS=NULL
        )%>%
  #normalization of SWC
  mutate(SWC_Norm=SWC_1/SWC.Tha.q95)%>%
  mutate(sitename="THA")

#
df.all<-bind_rows(df.Tha_sel,df.Dav_sel)
#change sitename:
df.all<-df.all %>%
  mutate(sitename=case_when(c(sitename=="THA") ~"DE-Tha",
                                 c(sitename=="DAV") ~"CH-Dav"))
#----------
#(3)plotting:
#----------
##-----------
#adding the focused period
##-----------
#load the phenology data:
load.path<-"./data/Comprehensive_plot_data/Fig1/"
df_pheno<-readRDS(file=paste0(load.path,"df_pheno.RDS"))
#
Dav_pheno<-df_pheno[df_pheno$sitename=="CH-Dav",]
Tha_pheno<-df_pheno[df_pheno$sitename=="DE-Tha",]

rect.coord_isevent=data.frame(
  #SOS->sos10
  x1=as.Date("2023-01-01")-1+df_pheno[df_pheno$sitename=="CH-Dav",]$sos10[1],
  #end of spring
  x2=as.Date("2023-01-01")-1+df_pheno[df_pheno$sitename=="CH-Dav",]$peak,
  x3=as.Date("2023-01-01")-1+df_pheno[df_pheno$sitename=="DE-Tha",]$peak,
                              y1=-Inf, 
                              y2=Inf)

#function:
plot_fun<-function(df,var,x_f,legend_f,df_rect){
  # df<-df.all
  # var<-"SWC_1"
  # x_f=FALSE
  # legend_f=FALSE
  # df_rect=rect.coord_isevent
  
  #
  df_sel<-df %>%
    filter(Date>=as.Date("2023-01-01")&Date<=as.Date("2023-09-01"))%>%
    dplyr::select(Date,var,sitename)
  names(df_sel)<-c("Date","y","sitename")
  #calculate the rolling mean:
  df_sel<-df_sel %>%
    group_by(sitename)%>%
    mutate(y_15=zoo::rollmean(y,k=15,fill="extend"))
  #change the sitename-->shorten:
  df_sel<-df_sel%>%
    mutate(sitename=case_when(sitename=="DE-Tha"~"Tha",
                              sitename=="CH-Dav"~"Dav"))
  
  #plotting
  #
  df_sel_Tha_Samp<-df_sel %>%
    filter(sitename=="Tha")%>%
    filter(Date %in% as.Date(c("2023-03-02","2023-03-22","2023-04-13",
                               "2023-04-28","2023-05-17","2023-07-14")))
  df_sel_Dav_Samp<-df_sel %>%
    filter(sitename=="Dav")%>%
    filter(Date %in% as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                               "2023-05-03","2023-05-22","2023-07-17")))

  
  df_plot<-ggplot()+
    geom_point(aes(Date,y,col=sitename),data=df_sel)+
    ylab("")+
    xlab("2023")+
    geom_rect(data=df_rect,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill="blue",alpha=0.4)+
    geom_rect(data=df_rect,
              mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), 
              fill="blue",alpha=0.2)+
    geom_point(aes(Date,y),data=df_sel_Tha_Samp,col="orange",size=5)+
    geom_line(aes(Date,y_15),data=df_sel[df_sel$sitename=="Tha",],col="orange",size=1.2)+
    geom_point(aes(Date,y),data=df_sel_Dav_Samp,col="brown1",size=5)+
    geom_line(aes(Date,y_15),data=df_sel[df_sel$sitename=="Dav",],col="brown1",size=1.2)+
    scale_color_manual(values = c("Dav"=adjustcolor("brown1",0.2),
                       "Tha"=adjustcolor("orange",0.2)))+
    theme_light()+
    theme(legend.position = c(0.85,0.9),
          legend.text = element_text(size = 20),
          legend.title = element_blank(),
          legend.background = element_blank(),
          axis.text = element_text(size=16),
          axis.title = element_text(size=20)
          )
  ##########
  #SWC_1
  if(var=="SWC_1"){
    df_plot<-df_plot+
      ylab(expression("SWC (%)"))+
      annotate(geom="text",x=df_sel_Tha_Samp$Date,y=df_sel_Tha_Samp$y+1,
               label=paste0("C",1:6),col="orange",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$Date,y=df_sel_Dav_Samp$y-1,
               label=paste0("C",1:6),col="brown1",size=5)+
      #adding the sos and peaks of two sites:
      #sos
      geom_vline(xintercept=df_rect$x1,col="forestgreen",size=1.1)+
      annotate(geom = "text",x=as.Date(df_rect$x1)+10,
               y=1.5,label="sos",col="forestgreen",size=6)+
      #peaks
      annotate(geom = "text",x=as.Date(df_rect$x2)+10,
               y=1.5,label="peak",col="brown1",size=6)+
      geom_vline(xintercept=df_rect$x2,col="brown1",size=1.1)+
      annotate(geom = "text",x=as.Date(df_rect$x3)-10,
               y=1.5,label="peak",col="orange",size=6)+
      geom_vline(xintercept=df_rect$x3,col="orange",size=1.1)
    
  }
  #SWC_Norm:
  if(var=="SWC_Norm"){
    df_plot<-df_plot+
      ylab(expression("Norm_SWC"))+
      annotate(geom="text",x=df_sel_Tha_Samp$Date,y=df_sel_Tha_Samp$y+0.1,
               label=paste0("C",1:6),col="orange",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$Date,y=df_sel_Dav_Samp$y-0.1,
               label=paste0("C",1:6),col="brown1",size=5)+
      #adding the sos and peaks of two sites:
      #sos
      geom_vline(xintercept=df_rect$x1,col="forestgreen",size=1.1)+
      # annotate(geom = "text",x=as.Date(df_rect$x1)+10,
      #          y=1.5,label="sos",col="forestgreen",size=6)+
      #peaks
      # annotate(geom = "text",x=as.Date(df_rect$x2)+10,
      #          y=1.5,label="peak",col="brown1",size=6)+
      geom_vline(xintercept=df_rect$x2,col="brown1",size=1.1)+
      # annotate(geom = "text",x=as.Date(df_rect$x3)-10,
      #          y=1.5,label="peak",col="orange",size=6)+
      geom_vline(xintercept=df_rect$x3,col="orange",size=1.1)
    
  }

  if(var=="VPD"){
    df_plot<-df_plot+
      ylab(expression("VPD (bar)"))+
      annotate(geom="text",x=df_sel_Tha_Samp$Date,y=df_sel_Tha_Samp$y+1,
               label=paste0("C",1:6),col="orange",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$Date,y=df_sel_Dav_Samp$y-1,
               label=paste0("C",1:6),col="brown1",size=5)+
      #adding the sos and peaks of two sites:
      # annotate(geom = "text",x=as.Date(df_rect$x1)+10,
      #          y=1.5,label="sos",col="forestgreen",size=6)+
      geom_vline(xintercept=df_rect$x1,col="forestgreen",size=1.1)+
      # annotate(geom = "text",x=as.Date(df_rect$x2)-10,
      #          y=0,label="peak",col="brown1",size=6)+
      geom_vline(xintercept=df_rect$x2,col="brown1",size=1.1)+
      # annotate(geom = "text",x=as.Date(df_rect$x3)+10,
      #          y=0,label="peak",col="orange",size=6)+
      geom_vline(xintercept=df_rect$x3,col="orange",size=1.1)
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
p_SWC<-plot_fun(df.all,"SWC_1",TRUE,TRUE,rect.coord_isevent)
p_SWC_norm<-plot_fun(df.all,"SWC_Norm",TRUE,FALSE,rect.coord_isevent)
p_VPD<-plot_fun(df.all,"VPD",TRUE,FALSE,rect.coord_isevent)
#merge the plots:
p_samplingYear_VPD_SWC<-plot_grid(p_SWC,p_SWC_norm,p_VPD,nrow = 1,
                                  labels = c("(c)","(d)","(e)"))
###save the ggplot plots:
save.path<-"./data/Comprehensive_plot_data/Fig4/"
save(p_samplingYear_VPD_SWC,file=paste0(save.path,"p_samplingYear_VPD_SWC.RDA"))







