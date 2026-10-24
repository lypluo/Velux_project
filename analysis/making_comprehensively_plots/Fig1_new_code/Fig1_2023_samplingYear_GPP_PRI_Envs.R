##############################################
#Aim: comparing the meterological condition in Dav and Tha
#in 2023
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
SWC.Dav.q95<-quantile(df.Dav[df.Dav$Date>=as.Date("2023-01-01")&df.Dav$Date<=as.Date("2023-01-01")+250,]$SWC_F_MDS_1,0.95,na.rm = T)
SWC.Tha.q95<-quantile(df.Tha[df.Tha$Date>=as.Date("2023-01-01")&df.Tha$Date<=as.Date("2023-01-01")+250,]$SWC_F_MDS_1,0.95,na.rm = T)

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

#----------------------
#(3)load the PRI data from DE-Tha and CH-Dav:
#----------------------
load.path<-"./data/Tower_based_VIs/"
load(paste0(load.path,"Twosites_VIs.RDA"))

#
df.Dav_PRI<-df.all%>%
  filter(sitename=="CH-Dav")%>%
  dplyr::select(Date,NDVI,PRI,NDVI.max.filtered,PRI.max.filtered,
                PRI_norm,PRI_norm.max.filtered)
df.Tha_PRI<-df.all%>%
  filter(sitename=="DE-Tha")%>%
  dplyr::select(Date,NDVI,PRI,NDVI.max.filtered,PRI.max.filtered,
                PRI_norm,PRI_norm.max.filtered)
#
df.PRI<-bind_rows(df.Dav_PRI,df.Tha_PRI)
#########
df.all_temp<-bind_rows(df.Tha_sel,df.Dav_sel)
#change sitename:
df.all_temp<-df.all_temp %>%
  mutate(sitename=case_when(c(sitename=="THA") ~"DE-Tha",
                                 c(sitename=="DAV") ~"CH-Dav"))
df.all<-left_join(df.all_temp,df.PRI)
#adding PRI
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

# rect.coord_isevent=data.frame(x1=as.Date("2023-01-01")-1+
#                               df_pheno[df_pheno$sitename=="CH-Dav",]$sos10[1]-60,
#                               x2=as.Date("2023-01-01")-1+df_pheno[df_pheno$sitename=="CH-Dav",]$sos10[1],
#                               x3=as.Date("2023-01-01")-1+df_pheno[df_pheno$sitename=="CH-Dav",]$sos10[1]+60,
#                               y1=-Inf, 
#                               y2=Inf)
#period:[sos-60,sos+60] is set 
#adding the sos
#for CH-Dav
rect.coord_isevent=data.frame("x1"=Dav_pheno[1]-60,
                              "x2"=Dav_pheno[1],
                              "x3"=Dav_pheno[1]+60,
                              "y1"=-Inf, 
                              "y2"=Inf)
#
names(rect.coord_isevent)<-c("x1","x2","x3","y1","y2")

#function:
plot_fun<-function(df,var,x_f,legend_f,df_rect){
  # df<-df.all
  # var<-"PPFD_IN"
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
    mutate(y_15=zoo::rollmean(y,k=15,fill="extend"))%>%
    mutate(DoY=yday(Date))
  
  #plotting
  #
  df_sel_Tha_Samp<-df_sel %>%
    filter(sitename=="DE-Tha")%>%
    filter(Date %in% as.Date(c("2023-03-02","2023-03-22","2023-04-13",
                               "2023-04-28","2023-05-17","2023-07-14")))
  df_sel_Dav_Samp<-df_sel %>%
    filter(sitename=="CH-Dav")%>%
    filter(Date %in% as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                               "2023-05-03","2023-05-22","2023-07-17")))
  
  
  df_plot<-ggplot()+
    geom_point(aes(DoY,y,col=sitename),data=df_sel)+
    ylab("")+
    xlab("DOY (2023)")+
    # geom_rect(data=df_rect,
    #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
    #           fill="blue",alpha=0.4)+
    geom_rect(data=df_rect,
              mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), 
              fill="blue",alpha=0.2)+
    geom_point(aes(DoY,y),data=df_sel_Tha_Samp,col="orange",size=5)+
    geom_line(aes(DoY,y_15),data=df_sel[df_sel$sitename=="DE-Tha",],col="orange",size=1.2)+
    geom_point(aes(DoY,y),data=df_sel_Dav_Samp,col="brown1",size=5)+
    geom_line(aes(DoY,y_15),data=df_sel[df_sel$sitename=="CH-Dav",],col="brown1",size=1.2)+
    scale_color_manual(values = c("CH-Dav"=adjustcolor("brown1",0.3),
                       "DE-Tha"=adjustcolor("orange",0.3)))+
    # geom_vline(xintercept=df_rect$x2,col="forestgreen",size=1.1)+
    # annotate(geom="text",x=df_sel_Tha_Samp$DoY,y=df_sel_Tha_Samp$y+2,
    #          label=paste0("C",1:6),col="orange",size=5)+
    # annotate(geom="text",x=df_sel_Dav_Samp$DoY,y=df_sel_Dav_Samp$y-2,
    #          label=paste0("C",1:6),col="brown1",size=5)+
    theme_classic()+
    theme(legend.position = c(0.15,0.85),
          legend.text = element_text(size = 20),
          legend.title = element_blank(),
          legend.background = element_blank(),
          axis.text = element_text(size=16),
          axis.title = element_text(size=20)
          )
  ##########
  if(var=="TA"){
    df_plot<-df_plot+
      ylab(expression("Ta ("* "°C)"))
      # annotate(geom="text",x=df_sel_Tha_Samp$DoY,y=df_sel_Tha_Samp$y+2,
      #          label=paste0("C",1:6),col="orange",size=5)+
      # annotate(geom="text",x=df_sel_Dav_Samp$DoY,y=df_sel_Dav_Samp$y-2,
      #          label=paste0("C",1:6),col="brown1",size=5)
      # geom_vline(xintercept=df_rect$x2,col="forestgreen",size=1.1)+
      # annotate(geom = "text",x=df_rect$x2+10,y=-10,
      #          label="sos",col="forestgreen",size=6)
      #adding the flag to indicate cold events in Davos
      # geom_segment(
      #   aes(x = df_sel_Dav_Samp$DoY[2], y = -10,
      #       xend = df_sel_Dav_Samp$DoY[2], yend = -7),
      #   arrow = arrow(type = "closed", 
      #                 length = unit(0.2, "inches")),
      #   color = "red", size = 0.8)
  }
  if(var=="PPFD_IN"){
    df_plot<-df_plot+
      ylab(expression("PAR ("*mu*"mol m"^-2*s^-1*")"))+
      annotate(geom="text",x=df_sel_Tha_Samp$DoY,y=df_sel_Tha_Samp$y-50,
               label=paste0("C",1:6),col="orange",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$DoY,y=df_sel_Dav_Samp$y+50,
               label=paste0("C",1:6),col="brown1",size=5)+
      # geom_vline(xintercept=df_rect$x2,col="forestgreen",size=1.1)+
      # annotate(geom = "text",x=df_rect$x2+10,y=0,
      #          label="sos",col="forestgreen",size=6)+
      #adding the flag to indicate cold events in Davos
      geom_segment(
        aes(x = df_sel_Dav_Samp$DoY[2], y = 550,
            xend = df_sel_Dav_Samp$DoY[2], yend = 400),
        arrow = arrow(type = "closed", 
                      length = unit(0.2, "inches")),
        color = "red", size = 0.8)
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
p_GPP<-plot_fun(df.all,"GPP",FALSE,TRUE,rect.coord_isevent)+
  ylab(expression("GPP (g C m"^-2* " d"^-1* ")" ))
p_PRI_Norm<-plot_fun(df.all,"PRI_norm",FALSE,FALSE,rect.coord_isevent)+
  ylim(0,1.15)+
  ylab(expression(PRI[Norm]))
p_SWC_Norm<-plot_fun(df.all,"SWC_Norm",FALSE,FALSE,rect.coord_isevent)+
  ylab(expression(SWC[Norm]))
p_TA<-plot_fun(df.all,"TA",FALSE,FALSE,rect.coord_isevent)
p_PAR<-plot_fun(df.all,"PPFD_IN",TRUE,FALSE,rect.coord_isevent)
#merge the plots:
p_samplingYear_GPP_PRI_Envs<-plot_grid(p_GPP,p_PRI_Norm,p_SWC_Norm,p_TA,p_PAR,ncol = 1,
          labels = c("(a)","(b)","(c)",
                     "(d)","(e)"),
          align = "hv")
#save the plot
save.path<-"./manuscript/Comprehensive_plot/"
ggsave(p_samplingYear_GPP_PRI_Envs,
     height=18,width=13,
     file=paste0(save.path,"Fig.1_supp_samplingYear_GPP_PRI_Env.png"))







