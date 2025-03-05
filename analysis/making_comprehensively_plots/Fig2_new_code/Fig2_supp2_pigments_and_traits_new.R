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
df.sites.Meteo<-rbind(df.Dav.Meteo,df.Tha.Meteo)%>%
  mutate(sitename=case_when(sitename=="THA" ~ "DE-Tha",
                            sitename=="DAV" ~ "CH-Dav"))

#----------------------
#B.load the pigments
#----------------------
load.path<-"./data/"
load(paste0(load.path,"Pigment.data.RDA"))
df.Pigment<-df.Pigment %>%
  dplyr::select(sitename,CampaignNum,Position,Cha,Chb,Car,
         CartoCab_ratio)%>%
  mutate(Cab=Cha+Chb)%>%
  mutate(sitename=case_when(sitename=="THA" ~ "DE-Tha",
                            sitename=="DAV" ~ "CH-Dav"))

#--other variables---
#----------------------
#C.load the leaf traits:
#----------------------
load.path<-"data/"
load(paste0(load.path,"Leaf_traits.data.cleaned.RDA"))
df.traits<-df.traits.sel%>%
  mutate(sitename=case_when(sitename=="THA" ~ "DE-Tha",
                            sitename=="DAV" ~ "CH-Dav"))

#------------------
#(2)merge the data and change the Position names
#------------------
df.merge_Pigments<-left_join(df.Pigment,df.sites.Meteo)
df.merge_traits<-left_join(df.traits,df.sites.Meteo)
##
df.merge_Pigments<-df.merge_Pigments %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="M") ~"Middle",
                            c(Position=="U") ~"Upper"))
df.merge_traits<-df.merge_traits %>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="M") ~"Middle",
                            c(Position=="U") ~"Upper"))
#------------------
#(3)plotting
#------------------
#addding mean and sd
plot_meansd_fun<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
  # df<-df.merge_Pigments
  # var_name<-"Car"
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
                                  "Middle"=adjustcolor("purple",0.9),
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


##-------------
#Pigments data
##-------------
#adding pigments ratios
df.merge_Pigments<-df.merge_Pigments %>%
  mutate(ChatoChb=Cha/Chb)
#Cab:
p_Cab_Date<-plot_meansd_fun(df.merge_Pigments,"Cab",
                      c(0.1,0.9),
                      data.frame(x=84,xend=84,y=0.5,yend=0.2),
                      TRUE)+
  labs(x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" Cab (mg g"^-1*")")
  )+
  theme(
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank(),
        legend.position = c(0.15,0.25),
        # legend.background = element_rect(),
        # plot.margin = margin(0,5,0,5),
        # panel.spacing.y = unit(0.05, "lines")
        )+
  annotate(geom = "text",x=c(64),y=rep(-1.25),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-1.25),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-1.25),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-1.25),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-1.25),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-1.25),label=c("C6"),size=6)
#Car:
p_Car_Date<-plot_meansd_fun(df.merge_Pigments,"Car",
                      c(0.1,0.9),
                      data.frame(x=84,xend=84,y=0.15,yend=0.08),
                      TRUE)+
  labs(x="DOY (2023)",
       # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
       y = expression(Delta*" Car (mg g"^-1*")")
  )+
  theme(
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank(),
        legend.position = "none",
        legend.background = element_rect(),
        plot.margin = margin(0,5,0,5),
        panel.spacing.y = unit(0.05, "lines"))+
  annotate(geom = "text",x=c(64),y=rep(-0.2),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.2),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.2),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.2),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.2),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.2),label=c("C6"),size=6)


# Car/Cab
p_CartoCab_Date<-plot_meansd_fun(df.merge_Pigments,"CartoCab_ratio",
                            c(0.1,0.9),
                            data.frame(x=84,xend=84,y=0.5,yend=0.4),
                            TRUE)+
  labs(
    x="",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" Car/Cab")
  )+
  theme(
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank(),
        legend.position = "none",
        legend.background = element_rect(),
        plot.margin = margin(0,5,0,5),
        panel.spacing.y = unit(0.05, "lines"))+
  annotate(geom = "text",x=c(64),y=rep(-0.3),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.3),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.3),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.3),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.3),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.3),label=c("C6"),size=6)

#addtional plots:
#theorotically, if ratio of Cha/Chb high-->higher light photoprotection according to Liyao
#but the results did not reflect this 
p_ChatoChb_Date<-plot_meansd_fun(df.merge_Pigments,"ChatoChb",c(0.9,0.9),
                                       data.frame(x=84,xend=84,y=7,yend=5),
                                       TRUE)+
  labs(
    x="DOY (2023)",
    # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(Delta*" Chla/Chb"))+
  theme(
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank(),
        legend.position = "none",
        legend.background = element_rect(),
        plot.margin = margin(0,5,0,5))+
  annotate(geom = "text",x=c(64),y=rep(-8),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-8),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-8),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-8),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-8),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-8),label=c("C6"),size=6)

##-------------
#leaf traits
##-------------


#----------------
#Merge plots:
#----------------
p_pigments<-plot_grid(p_Cab_Date,p_Car_Date,
                p_ChatoChb_Date,
                ncol = 3,nrow=1,
                align = "hv",
                labels = c("(a)","(b)","(c)"))
  # # theme(plot.margin = margin(0,0,0,0),
  # #       panel.spacing = unit(0,"lines")
  #       )

#only save the SLA data
# p_leaf_traits<-plot_grid(p_SLA_Date,p_width_Date,
#                 ncol = 2,
#                 align = "hv",
#                 labels = c("(A)","(B)")
#                 # rel_heights = c(0.19,0.19,0.19,0.22,0.2)
#                 )+
#   theme(plot.margin = margin(0,0,0,0),
#     panel.spacing = unit(0,"lines"))

##save the ggplot plots:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig2_supp2_pigments_new.png"),
       p_pigments,width = 20,height=5)





