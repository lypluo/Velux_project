#################################################
#Aim: structure the plot temporally-->for the leaf scale measurements
#e.g. x-->Date, y=Amax in different sites and height
#update in Nov, 2024-->specifically plot A and Amax
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
df.sites.Meteo<-rbind(df.Dav.Meteo,df.Tha.Meteo)%>%
  mutate(sitename=case_when(sitename=="THA" ~ "DE-Tha",
                            sitename=="DAV" ~ "CH-Dav"))

#----------------------
#B.load the LIcor measurements:
#----------------------
#Amax
load.path<-"data/LIcor/"
load(paste0(load.path,"LRC.parameters.RDA"))
df.sites.LRC.paras<-df.LRC.paras %>%
  mutate(CampaignNum=substr(ID,1,2),
         Position=substr(ID,4,4))%>%
  mutate(sitename=case_when(sitename=="THA" ~ "DE-Tha",
                            sitename=="DAV" ~ "CH-Dav"))

#photosynthesis(A):
load(paste0(load.path,"df.Amax.cleaned.RDA"))
df.A<-dat
#
df.A<-df.A %>%
  mutate(sitename=case_when(sitename=="THA" ~ "DE-Tha",
                             sitename=="DAV" ~ "CH-Dav"))

#------------------
#(2)merge the data and change the Position names
#------------------
df.merge_Amax<-left_join(df.sites.LRC.paras,df.sites.Meteo)
df.merge_A<-left_join(df.A,df.sites.Meteo)
##
df.merge_Amax<-df.merge_Amax %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="U") ~"Upper"))
df.merge_A<-df.merge_A %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="U") ~"Upper"))
#------------------
#(3)plotting
#------------------
#for A:
plot_meansd_fun<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.merge_Amax
  # var_name<-"k_sat"
  # legend.xy<-c(0.1,0.9)
  # arrow.xy<-data.frame(x=84,xend=84,y=-0.1,yend=-0.15)
  # arrow.flag<-FALSE
  
  df$y<-as.numeric(unlist(df[,var_name]))
  #
  df_THA<-df%>%
    filter(sitename=="DE-Tha")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)%>%
    ##summary the data
    group_by(sitename,Date,CampaignNum,Position)%>%
    summarise(y_mean=mean(y,na.rm = T),
              y_sd=sd(y,na.rm=T))
  df_DAV<-df%>%
    filter(sitename=="CH-Dav")%>%
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
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=20),
          # axis.text.x = element_text(angle = 35,hjust = 1),
          #increasing text size of facet grid labels
          strip.text.x = element_text(size=18),
          legend.text = element_text(size=16),
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
#For Amax:
plot_point_fun<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.merge_Amax
  # var_name<-"k_sat"
  # legend.xy<-c(0.8,0.25)
  #arrow.xy<-data.frame(x=84,xend=84,y=-0.1,yend=-0.15)
  #arrow.flag<-TRUE
  
  df$y<-df[,var_name]
  #
  df<-df %>%
    mutate(doy=yday(Date))
  p_var_Date<-df%>%
    ggplot(aes(x=doy,y=y,col=sitename,shape=Position))+
    geom_point(size=3)+
    geom_line(size=1.2,lty=2)+
    # stat_summary(aes(x=Date,y=k_sat,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
    #              geom="pointrange",size=3,linewidth=4)+
    scale_color_manual(values = c("CH-Dav"=adjustcolor("red",0.8),
                                  "DE-Tha"=adjustcolor("orange",0.8)))+
    xlab("DOY (2023)")+
    xlim(0,250)+
    labs(color = "")+
    # labs(
    #   # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    #   y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
    # )
    theme_bw()+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=20),
          legend.text = element_text(size=16),
          legend.position = legend.xy,
          legend.background = element_blank()
    )
  #
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
  #
  return(p_var_Date)
}
##-------------
#LIcor data
##-------------
library(ggforce) #-->draw circle in the plot
#1) A:
p_A_Date<-plot_meansd_fun(df.merge_A,"A",c(0.1,0.2),
                          data.frame(x=84,xend=84,y=0,yend=-2),
                          TRUE)+
  labs(
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta *A ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  )

#2) Amax:
p_Amax_Date<-plot_point_fun(df.merge_Amax,"k_sat",c(0.85,0.4),
                            data.frame(x=yday(as.Date("2023-03-27")),
                                       xend=yday(as.Date("2023-03-27")),y=3,yend=1),
                            TRUE)+
  labs(
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  )+
  geom_circle(aes(x0=yday(as.Date("2023-07-14")),y0=9,r=2),
              size=1.1,color="black")

#-------------------------
#(3)further plotting: adding lines and rectanges in plot
#-------------------------
##adding the lines to indicate the field sampling dates:
Date.Dav<-as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                    "2023-05-03","2023-05-22","2023-07-17"))
Date.Tha<-as.Date(c("2023-03-02","2023-03-22","2023-04-13",
                    "2023-04-28","2023-05-17","2023-07-14"))
#
Date.Dav_DOY<-yday(Date.Dav)
Date.Tha_DOY<-yday(Date.Tha)

#
p_A_Date<-p_A_Date+
  # geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  # geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  xlim(25,250)+
  annotate(geom = "text",x=c(64),y=rep(-10),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-10),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-10),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-10),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-10),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-10),label=c("C6"),size=6)
p_Amax_Date<-p_Amax_Date+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  xlim(25,250)+
  annotate(geom = "text",x=c(64),y=rep(-0.5),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.5),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.5),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.5),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.5),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.5),label=c("C6"),size=6)


#save the plot:
p_A_Amax<-plot_grid(p_A_Date,p_Amax_Date,align = "v",ncol=1,nrow=2,
                    labels=c("(a)","(b)"))
#
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig2_supp1_A_Amax_new.png"),
       p_A_Amax,
       width = 10,height=8)

