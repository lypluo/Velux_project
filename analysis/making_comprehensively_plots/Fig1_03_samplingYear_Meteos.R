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
SWC.Dav.q95<-quantile(df.Dav$SWC_F_MDS_1,0.95,na.rm = T)
SWC.Tha.q95<-quantile(df.Tha$SWC_F_MDS_1,0.95,na.rm = T)

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

rect.coord_isevent=data.frame(x1=as.Date("2023-01-01")-1+df_pheno[df_pheno$sitename=="CH-Dav",]$sos10[1]-60,
                              x2=as.Date("2023-01-01")-1+df_pheno[df_pheno$sitename=="CH-Dav",]$sos10[1],
                              x3=as.Date("2023-01-01")-1+df_pheno[df_pheno$sitename=="CH-Dav",]$sos10[1]+60,
                              y1=-Inf, 
                              y2=Inf)

#function:
plot_fun<-function(df,var,x_f,legend_f,df_rect){
  # df<-df.all
  # var<-"TA"
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
    geom_line(aes(Date,y_15),data=df_sel[df_sel$sitename=="DE-Tha",],col="orange",size=1.2)+
    geom_point(aes(Date,y),data=df_sel_Dav_Samp,col="brown1",size=5)+
    geom_line(aes(Date,y_15),data=df_sel[df_sel$sitename=="CH-Dav",],col="brown1",size=1.2)+
    scale_color_manual(values = c("CH-Dav"=adjustcolor("brown1",0.2),
                       "DE-Tha"=adjustcolor("orange",0.2)))+
    theme_light()+
    theme(legend.position = c(0.8,0.15),
          legend.text = element_text(size = 20),
          legend.title = element_blank(),
          legend.background = element_blank(),
          axis.text = element_text(size=16),
          axis.title = element_text(size=20)
          )
  ##########
  if(var=="TA"){
    df_plot<-df_plot+
      ylab(expression("Ta ("* "°C)"))+
      annotate(geom="text",x=df_sel_Tha_Samp$Date,y=df_sel_Tha_Samp$y+2,
               label=paste0("C",1:6),col="orange",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$Date,y=df_sel_Dav_Samp$y-2,
               label=paste0("C",1:6),col="brown1",size=5)+
      geom_vline(xintercept=df_rect$x2,col="forestgreen",size=1.1)+
      annotate(geom = "text",x=as.Date(df_rect$x2)+10,y=-10,label="sos",col="forestgreen",size=6)
  }
  if(var=="PPFD_IN"){
    df_plot<-df_plot+
      ylab(expression("PAR ("*mu*"mol m"^-2*s^-1*")"))+
      annotate(geom="text",x=df_sel_Tha_Samp$Date,y=df_sel_Tha_Samp$y-50,
               label=paste0("C",1:6),col="orange",size=5)+
      annotate(geom="text",x=df_sel_Dav_Samp$Date,y=df_sel_Dav_Samp$y+50,
               label=paste0("C",1:6),col="brown1",size=5)+
      geom_vline(xintercept=df_rect$x2,col="forestgreen",size=1.1)+
      annotate(geom = "text",x=as.Date(df_rect$x2)+10,y=0,label="sos",col="forestgreen",size=6)
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
p_TA<-plot_fun(df.all,"TA",TRUE,TRUE,rect.coord_isevent)
p_PAR<-plot_fun(df.all,"PPFD_IN",TRUE,FALSE,rect.coord_isevent)
#merge the plots:
plot_grid(p_TA,p_PAR,nrow = 1)


#-------------------------
#(4)making the boxplots
#-------------------------
df_plot<-df.all %>%
  dplyr::filter(Date>=as.Date("2023-01-01"))%>%
  group_by(sitename,Date)%>%
  dplyr::summarise(Ta_mean=mean(TA,na.rm=T),
                   PAR_mean=mean(PPFD_IN,na.rm = T))%>%
  mutate(DoY=yday(Date))

df_add_1<-left_join(df_plot,df_pheno)%>%
  filter(sitename %in% c("DE-Tha","CH-Dav"))%>%
  #only keep the data between [sos10-60,sos10_60]
  filter(DoY>=sos10-60 & DoY<sos10)%>%
  mutate(period="pre-sos")
df_add_2<-left_join(df_plot,df_pheno)%>%
  filter(sitename %in% c("DE-Tha","CH-Dav"))%>%
  #only keep the data between [sos10-60,sos10_60]
  filter(DoY>sos10 & DoY<=sos10+60)%>%
  mutate(period="post-sos")
df_add<-rbind(df_add_1,df_add_2)
#
df_add_Dav<-df_add[df_add$sitename=="CH-Dav",]
df_add_Tha<-df_add[df_add$sitename=="DE-Tha",]
#向量相减
df_t<-df_add_Dav[,-c(1,length(df_add_Dav))] - df_add_Tha[,-c(1,length(df_add_Dav))]
df_add_final<-cbind(df_t,"period"=df_add_Dav$period)%>%
  dplyr::select(Ta_mean,PAR_mean,period)%>%
  pivot_longer(c("Ta_mean","PAR_mean"),names_to = "var",values_to = "var_value")%>%
  mutate(var=case_when(var=="Ta_mean" ~ "Ta",
                       var=="PAR_mean" ~ "PAR"))

#
p_Ta_boxplot<-df_add_final %>%
  filter(var=="Ta")%>%
  ggplot(aes(x=period,y=var_value,fill=period))+
  geom_violin()+
  geom_boxplot(width = 0.1,position = position_dodge(0.9),col="grey",size=1.05) +
  # stat_summary(fun = mean, geom = "point", size=3, color = "black")+
  ylab(expression(paste(Delta*"Ta (", "°C)")))+
  xlab("")+
  scale_fill_manual(values = c("pre-sos"=adjustcolor("blue",0.4),
                               "post-sos"=adjustcolor("blue",0.2)))+
  theme_light()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20)
  )
p_PAR_boxplot<-df_add_final %>%
  filter(var=="PAR")%>%
  ggplot(aes(x=period,y=var_value,fill=period))+
  geom_violin()+
  geom_boxplot(width = 0.1,position = position_dodge(0.9),col="grey",size=1.05) +
  # stat_summary(fun = mean, geom = "point", size=3, color = "black")+
  ylab(expression(paste(Delta*"PAR (",mu*"mol m"^-2,"s"^-1,")")))+
  xlab("CH-DAV - DE-Tha")+
  scale_fill_manual(values = c("pre-sos"=adjustcolor("blue",0.4),
                               "post-sos"=adjustcolor("blue",0.2)))+
  theme_light()+
  theme(legend.position = "none",
        legend.background = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_blank(),
        # axis.text.x = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20)
  )

##
p_boxplot<-plot_grid(p_Ta_boxplot,p_PAR_boxplot,nrow = 2,align = "hv")

p_samplingYear_Meteo<-plot_grid(p_TA,p_PAR,p_boxplot,nrow=1,
                              labels = c("(h)","(j)","(k)"))
###save the ggplot plots:
# p_samplingYear_Meteo<-list("p_Ta"=p_TA,
#                          "p_PPFD"=p_PAR,
#                          "p_boxplot"=p_boxplot)
save.path<-"./data/Comprehensive_plot_data/Fig1/"
save(p_samplingYear_Meteo,file=paste0(save.path,"p_samplingYear_Meteo.RDA"))







