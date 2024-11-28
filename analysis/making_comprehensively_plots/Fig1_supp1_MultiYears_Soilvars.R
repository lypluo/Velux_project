##--------------------------------------------------------
#Aim: checking Meteos(especially for Rg and Ta) in Dav and Tha
##--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(dplyr)

#-------------
#(1)load the data-using daily data
#-------------
##load the fluxes and Meteo:
load.path<-"./data/EC_MeteoandFlux/"
load(paste0(load.path,"df_daily_from_ICOS.RDA"))

df.Dav<-df_DD$Dav
df.Tha<-df_DD$Tha
##load the soil TS and SWC:
load(paste0(load.path,"df_soilvars_daily_from_ICOS.RDA"))
df.Dav_extend<-df_DD$Dav
df.Tha_extend<-df_DD$Tha
#For this study, select the first layer TS and several layers of SWC:
#check the data gaps%
# visdat::vis_miss(df.Dav_extend, warn_large_data = FALSE)
# visdat::vis_miss(df.Tha_extend, warn_large_data = FALSE)
#
df.Dav_extend_sel<-df.Dav_extend %>%
  dplyr::select(Date,TS_1,SWC_1:SWC_5)
  
df.Tha_extend_sel<-df.Tha_extend %>%
  dplyr::select(Date,TS_1,SWC_1:SWC_4)

##merge the data:
df.Dav<-left_join(df.Dav,df.Dav_extend_sel)
df.Tha<-left_join(df.Tha,df.Tha_extend_sel)

#update--Nov,23,2024:adding the Ts data from another data source for Davos
#as the data are not available for Davos in 2022-2023
load.path<-"data/EC_MeteoandFlux/"
load(paste0(load.path,"df.Meteo_andFluxes.daily.RDA"))

#tidy the data:
df.Dav_add<-df.Meteo.daily$Dav
names(df.Dav_add)
#Take out the variables for TS_F_MDS_1
df.Dav_add_sel<-df.Dav_add %>%
  select(TIMESTAMP,TS_F_MDS_1)%>%
  mutate(Date=ymd(TIMESTAMP),TS_1=TS_F_MDS_1)%>%
  select(-c(TIMESTAMP,TS_F_MDS_1))%>%
  filter(Date>=as.Date("2022-01-01"))
#update the TS_1 data for df.Dav
df.Dav[df.Dav$Date>=as.Date("2022-01-01") & 
         df.Dav$Date<=as.Date("2023-12-31"),]$TS_1<-df.Dav_add_sel$TS_1


#merge data from two sites
df.Dav<-df.Dav %>%
  mutate(sitename="CH-Dav")%>%
  #according to info:SWC layer 3-->20cm; 4-->50cm
  #calculated the mean for shallow layer(<=20 cm); and medium layer(>30)
  mutate(across(SWC_1:SWC_5, as.numeric))%>%
  group_by(Date)%>%
  mutate(SWC_shallow=rowMeans(across(SWC_1:SWC_4),na.rm = T),
         SWC_medium=SWC_5)%>%
  dplyr::select(-c(SWC_2:SWC_5))


df.Tha<-df.Tha %>%
  mutate(sitename="DE-Tha")%>%
  #according to info:SWC layer 4-->30 cm; 5-->40cm
  #calculated the mean for shallow layer(<=30 cm); and medium layer(>30)
  mutate(across(SWC_1:SWC_4, as.numeric))%>%
  group_by(Date)%>%
  mutate(SWC_shallow=rowMeans(across(SWC_1:SWC_3),na.rm = T),
         SWC_medium=SWC_4)%>%
  dplyr::select(-c(SWC_2:SWC_4))
#
df<-rbind(df.Dav,df.Tha)

#-------------------
#(2) tidy the data 
#-------------------
#---merge the data according to doy
df<-df %>%
  mutate(DoY=yday(Date),
         Year=year(Date)
  )

#---------------------------
#3) making the plots
#---------------------------
##--------------
#fast checking:
##--------------
#Davos for example
df %>%
  filter(sitename=="CH-Dav")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=SWC_medium))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

df %>%
  filter(sitename=="DE-Tha")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=SWC_1))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

##-----------
#making the plots:
##-----------
plot_fun_meteo<-function(df,var_name,legend_flag){
  # df<-df
  # var_name<-"SWC_1"
  # legend_flag<-TRUE

  df.proc<-df %>%
    dplyr::select(sitename,Date,var_name,DoY)
  names(df.proc)<-c("sitename","Date","var","DoY")
  
  df.use<-df.proc %>%
    group_by(sitename,DoY)%>%
    dplyr::summarise(var_mean=mean(var,na.rm=T),
                     var_sd=sd(var,na.rm=T))
  #For Year 2023:
  df.use_2023<-df.proc %>%
    dplyr::filter(Date>=as.Date("2023-01-01") & Date<=as.Date("2023-12-31"))
  #
  p_plot<-df.use %>%
    ggplot()+
    geom_ribbon(
      aes(x = DoY, ymin = var_mean - var_sd, 
          ymax = var_mean + var_sd, fill=sitename),
      alpha = 0.2
    ) +
    xlab("DOY")+
    geom_line(aes(x=DoY,y=var_mean,col=sitename),size=1.1)+
    scale_color_manual(values = c("CH-Dav"="brown1",
                                  "DE-Tha"="orange"))+
    scale_fill_manual(values = c("CH-Dav"="brown1",
                                 "DE-Tha"="orange"))+
    theme(legend.text = element_text(size = 20),
          legend.background = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size=16),
          axis.title = element_text(size=20))
  #add the lines for 2023:
  p_plot<-p_plot+
    geom_line(aes(x=DoY,y=var,col=sitename),data=df.use_2023)
  
  if(var_name=="TS_1"){
    p_plot<-p_plot+
      xlim(0,250)+
      # ylim(0,750)+
      ylab(expression("T"[S]*" ("* "°C)"))+
      theme_light()+
      theme(legend.position = c(0.8,0.3),
            legend.title = element_blank())
  }
  if(var_name %in% c("SWC_1","SWC_shallow","SWC_medium")){
    p_plot<-p_plot+
      xlim(0,250)+
      # ylim(0,750)+
      ylab(expression("SWC (%)"))+
      theme_light()+
      theme(legend.position = c(0.8,0.3))
  }
  p_plot<-p_plot+
    theme(legend.text = element_text(size = 20),
          legend.background = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size=16),
          axis.title = element_text(size=20))
  if(legend_flag==FALSE){
    p_plot<-p_plot+theme(legend.position = "none")
  }
  
  return(p_plot)
}

#
p_SWC_surface<-plot_fun_meteo(df,"SWC_1",FALSE)+
  ylab(expression("SWC"[surface]*" (%)"))
p_SWC_shallow<-plot_fun_meteo(df,"SWC_shallow",FALSE)+
  ylab(expression("SWC"[shallow]*" (%)"))
p_SWC_medium<-plot_fun_meteo(df,"SWC_medium",FALSE)+
  ylab(expression("SWC"[medium]*" (%)"))
p_Ts<-plot_fun_meteo(df,"TS_1",FALSE)


##-----------
#(3)adding the focused period
##-----------
#load the phenology data:
load.path<-"./data/Comprehensive_plot_data/Fig1/"
df_pheno<-readRDS(file=paste0(load.path,"df_pheno.RDS"))
#
Dav_pheno<-df_pheno[df_pheno$sitename=="CH-Dav",]
Tha_pheno<-df_pheno[df_pheno$sitename=="DE-Tha",]

#for Ta
rect.coord_isevent=data.frame(x1=df_pheno[df_pheno$sitename=="CH-Dav",]$sos10[1]-60,
           x2=df_pheno[df_pheno$sitename=="CH-Dav",]$sos10[1],
           x3=df_pheno[df_pheno$sitename=="CH-Dav",]$sos10[1]+60,
           y1=-Inf, 
            y2=Inf)
#for Ts and SWC
p_Ts<-p_Ts+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=0,label="sos",col="forestgreen",size=6)+
  xlab("")
p_SWC_surface<-p_SWC_surface+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=0,label="sos",col="forestgreen",size=6)+
  xlab("")
p_SWC_shallow<-p_SWC_shallow+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=0,label="sos",col="forestgreen",size=6)
p_SWC_medium<-p_SWC_medium+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=0,label="sos",col="forestgreen",size=6)


#-------------------------
#(4)Merge the plots
#-------------------------
p_Merge_soilvars<-plot_grid(p_Ts,p_SWC_surface,p_SWC_shallow,p_SWC_medium,
          nrow=2,ncol = 2,
          align = "hv",
          labels = c("(a)","(b)","(c)","(d)"))
###save the ggplot plots:
# p_MultiYears_Meteo<-list("p_Ta"=p_Ta,
#                       "p_PPFD"=p_PPFD,
#                       "p_boxplot"=p_boxplot)
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig1_supp1.png"),p_Merge_soilvars,width = 15,height=12)
