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
##save the data:
save.path<-"./data/Comprehensive_plot_data/Fig1/"
saveRDS(df,paste0(save.path,"df.Meteo_and_SoilVars.RDS"))

################################################
#update in April,2026-->calculate the meteos in different seasons
#######
#-----------
#summarize the meteos
#-----------
df_sum_yearly_season<-df %>%
  #separate the temperature in different season
  mutate(month=month(Date),
         season=case_when(month>=3 & month<=5 ~"spring",
                          month>5 & month<=8 ~"summer",
                          month>8 & month<=11 ~"autumn",
                          month>11 | month<3 ~"winter"))%>%
  group_by(sitename,Year,season) %>%
  dplyr::summarise(temp=mean(TA),
                   prec=sum(P,na.rm = T), 
                   vpd=mean(VPD,na.rm=T),
                   ppfd=mean(PPFD_IN,na.rm=T),
                   .groups = "drop"
  )
df_sum_yearly_2season<-df %>%
  #separate the temperature in different season
  mutate(month=month(Date),
         season=case_when(month>=3 & month<=5 ~"spring",
                          month>5 & month<=8 ~"summer",
                          month>8 & month<=11 ~"autumn",
                          month>11 | month<3 ~"winter"))%>%
  mutate(season_2=ifelse(season=="spring"|season=="winter","spring-winter","not-spring-winter"))%>%
  group_by(sitename,Year,season_2) %>%
  dplyr::summarise(temp=mean(TA),
                   prec=sum(P,na.rm = T), 
                   vpd=mean(VPD,na.rm=T),
                   ppfd=mean(PPFD_IN,na.rm=T),
                   .groups = "drop"
  )
df_sum_yearly_final<-list("yearly_season"=df_sum_yearly_season,
                          "yearly_2season"=df_sum_yearly_2season)

#---
#summary site-years(different seasons) for site
#---
df_sum_season<-df_sum_yearly_season %>%
  group_by(sitename,season) %>%
  summarise_at(vars(temp:ppfd),mean,na.rm=T)

df_sum_2season<-df_sum_yearly_2season %>%
  group_by(sitename,season_2) %>%
  summarise_at(vars(temp:ppfd),mean,na.rm=T)
df_sum_season_final<-list("season"=df_sum_season,
                          "2season"=df_sum_2season)

#save the data:
file.path<-"./data/EC_MeteoandFlux/site_meteo/"
save(df_sum_yearly_final,file=paste0(file.path,"site_meteo_yearly.RDA"))
save(df_sum_season_final,file=paste0(file.path,"site_meteo.RDA"))

################################################

#calculate the mean annual temperature and precipitation 
#in winter
df %>%
  group_by(sitename,Year)%>%
  filter(DoY>=335 | DoY<=59)%>%
  summarise(TA_mean=mean(TA,na.rm = T),
            P_sum=sum(P,na.rm = T))%>%
  group_by(sitename)%>%
  summarise(TA_mean=mean(TA_mean,na.rm = T),
            P_sum=mean(P_sum,na.rm = T))
#in spring(Mar-May):
#+radiation
df %>%
  group_by(sitename,Year)%>%
  filter(DoY>=60 & DoY<=151)%>%
  summarise(TA_mean=mean(TA,na.rm = T),
            P_sum=sum(P,na.rm = T),
            PAR_mean=mean(PPFD_IN,na.rm=T)
            )%>%
  group_by(sitename)%>%
  summarise(TA_mean=mean(TA_mean,na.rm = T),
            P_sum=mean(P_sum,na.rm = T),
            PAR_mean=mean(PAR_mean,na.rm=T)
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
  ggplot(aes(x=DoY,y=SWC_medium))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

##-----------
#making the plots:
##-----------
plot_fun_meteo_2Sepsites<-function(df,var_name,legend_flag){
  # df<-df
  # var_name<-"TA"
  # legend_flag<-FALSE

  df.proc<-df %>%
    dplyr::select(sitename,Date,var_name,DoY)
  names(df.proc)<-c("sitename","Date","var","DoY")
  
  df.use<-df.proc %>%
    group_by(sitename,DoY)%>%
    dplyr::summarise(var_mean=mean(var,na.rm=T),
                     var_sd=sd(var,na.rm=T))
  #Data for sampling dates in 2023
  df_sel_Tha_Samp<-df.proc %>%
    filter(sitename=="DE-Tha")%>%
    filter(Date %in% as.Date(c("2023-03-02","2023-03-22","2023-04-13",
                               "2023-04-28","2023-05-17","2023-07-14")))
  df_sel_Dav_Samp<-df.proc %>%
    filter(sitename=="CH-Dav")%>%
    filter(Date %in% as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                               "2023-05-03","2023-05-22","2023-07-17")))
  #
  p_plot1<-df.use %>%
    filter(sitename=="CH-Dav")%>%
    ggplot()+
    geom_ribbon(
      aes(x = DoY, ymin = var_mean - var_sd, 
          ymax = var_mean + var_sd, fill=sitename),
      alpha = 0.2
    ) +
    xlab("DOY")+
    geom_line(aes(x=DoY,y=var_mean,col=sitename),size=1.1)+
    geom_point(aes(DoY,var),data=df_sel_Dav_Samp,col="brown1",size=5)+
    geom_line(aes(x=DoY,y=var,col=sitename),df.proc %>%
                filter(sitename=="CH-Dav")%>%
                mutate(Year=year(Date))%>%
                filter(Year==2023)
                )+
    scale_color_manual(values = c("CH-Dav"="brown1",
                                  "DE-Tha"="orange"))+
    scale_fill_manual(values = c("CH-Dav"="brown1",
                                 "DE-Tha"="orange"))
  p_plot2<-df.use %>%
    filter(sitename=="DE-Tha")%>%
    ggplot()+
    geom_ribbon(
      aes(x = DoY, ymin = var_mean - var_sd, 
          ymax = var_mean + var_sd, fill=sitename),
      alpha = 0.2
    ) +
    xlab("DOY")+
    geom_line(aes(x=DoY,y=var_mean,col=sitename),size=1.1)+
    geom_point(aes(DoY,var),data=df_sel_Tha_Samp,col="orange",size=5)+
    geom_line(aes(x=DoY,y=var,col=sitename),df.proc %>%
                filter(sitename=="DE-Tha")%>%
                mutate(Year=year(Date))%>%
                filter(Year==2023)
    )+
    scale_color_manual(values = c("CH-Dav"="brown1",
                                  "DE-Tha"="orange"))+
    scale_fill_manual(values = c("CH-Dav"="brown1",
                                 "DE-Tha"="orange"))
  
  if(var_name=="TA"){
    p_plot1<-p_plot1+
      annotate(geom="text",x=df_sel_Dav_Samp$DoY,y=-10,
                 # df_sel_Dav_Samp$var+2,
               label=paste0("C",1:6),col="brown1",size=6)+
      #adding the flag to indicate cold events in Davos
      geom_segment(
        aes(x = df_sel_Dav_Samp$DoY[2], y = -9,
            xend = df_sel_Dav_Samp$DoY[2], yend = -6),
        arrow = arrow(type = "closed", 
                      length = unit(0.2, "inches")),
        color = "red", size = 0.8)+
      xlim(0,250)+
      ylim(-15,28)+
      ylab(expression("Ta ("* "°C)"))+
      xlab("DOY (Multi-Years)")+
      theme_classic()+
      theme(
        legend.position = c(0.8,0.3),
        legend.text = element_text(size = 20),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=22))
    
    p_plot2<-p_plot2+
      annotate(geom="text",x=df_sel_Tha_Samp$DoY,y=-10,
                 # df_sel_Tha_Samp$var-2,
               label=paste0("C",1:6),col="orange",size=6)+
      xlim(0,250)+
      ylim(-15,28)+
      ylab(expression("Ta ("* "°C)"))+
      xlab("DOY (Multi-Years)")+
      theme_classic()+
      theme(
        legend.position = c(0.8,0.3),
        legend.text = element_text(size = 20),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=22))
  }
  
  if(var_name=="PPFD_IN"){
    p_plot1<-p_plot1+
      annotate(geom="text",x=df_sel_Dav_Samp$DoY,y=20,
               label=paste0("C",1:6),col="brown1",size=6)+
      #adding the flag to indicate cold events in Davos
      geom_segment(
        aes(x = df_sel_Dav_Samp$DoY[2], y = 150,
            xend = df_sel_Dav_Samp$DoY[2], yend = 250),
        arrow = arrow(type = "closed", 
                      length = unit(0.2, "inches")),
        color = "red", size = 0.8)+
      xlim(0,250)+
      ylim(-50,750)+
      ylab(expression("PAR ("*mu*"mol m"^-2*s^-1*")"))+
      xlab("DOY (Multi-Years)")+
      theme_classic()+
      theme(
        legend.position = c(0.8,0.3),
        legend.text = element_text(size = 20),
            legend.background = element_blank(),
            legend.title = element_blank(),
            axis.text = element_text(size=20),
            axis.title = element_text(size=22))

    p_plot2<-p_plot2+
      annotate(geom="text",x=df_sel_Tha_Samp$DoY,y=20,
               label=paste0("C",1:6),col="orange",size=6)+
      xlim(0,250)+
      ylim(-50,750)+
      ylab(expression("PAR ("*mu*"mol m"^-2*s^-1*")"))+
      xlab("DOY (Multi-Years)")+
      theme_classic()+
      theme(
        legend.position = c(0.8,0.3),
        legend.text = element_text(size = 20),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=22))
  }

  if(legend_flag==FALSE){
    p_plot1<-p_plot1+theme(legend.position = "none")
    p_plot2<-p_plot2+theme(legend.position = "none")
  }
  if(legend_flag==TRUE){
    p_plot1<-p_plot1
    p_plot2<-p_plot2
    }
  p_plot<-list(p_plot1,p_plot2)
  names(p_plot)<-c("plot1","plot2")
  
  return(p_plot)
}

#
p_Ta<-plot_fun_meteo_2Sepsites(df,"TA",FALSE)
p_PPFD<-plot_fun_meteo_2Sepsites(df,"PPFD_IN",FALSE)

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

###
p_Ta_Dav<-p_Ta$plot1+
  # geom_rect(data=rect.coord_isevent,
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+15,y=-15,label="sos",col="forestgreen",size=7)

p_Ta_Tha<-p_Ta$plot2+
  # geom_rect(data=rect.coord_isevent,
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+15,y=-15,label="sos",col="forestgreen",size=7)
###
p_PPFD_Dav<-p_PPFD$plot1+
  # geom_rect(data=rect.coord_isevent,
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+15,y=-50,label="sos",col="forestgreen",size=7)

p_PPFD_Tha<-p_PPFD$plot2+
  # geom_rect(data=rect.coord_isevent,
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+15,y=-50,label="sos",col="forestgreen",size=7)

#-------------------------
#(4)making the boxplots
#-------------------------
df_plot<-df %>%
  group_by(sitename,DoY)%>%
  dplyr::summarise(Ta_mean=mean(TA,na.rm=T),
                   PAR_mean=mean(PPFD_IN,na.rm = T))

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
df_add<-rbind(df_add_1,df_add_2)%>%
  mutate(period=factor(period,levels=c("pre-sos","post-sos")))
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
  filter(period=="post-sos")%>%
  ggplot(aes(x=period,y=var_value,fill=period))+
  geom_hline(yintercept = 0,lty=2,size=1.1)+
  geom_violin()+
  geom_boxplot(width = 0.1,position = position_dodge(0.9),col="grey",size=1.05) +
  stat_summary(fun = mean, geom = "point", size=3, color = "black")+
  ylab(expression(paste(Delta*"Ta (", "°C)")))+
  xlab("CH-Dav — DE-Tha")+
  scale_fill_manual(values = c("pre-sos"=adjustcolor("blue",0.3),
                               "post-sos"=adjustcolor("blue",0.1)))+
  theme_classic()+
  theme(legend.position = "none",
        # axis.text.x = element_blank(),
        axis.text = element_text(size=20),
        axis.text.x = element_blank(),
        axis.title = element_text(size=22)
  )
p_PAR_boxplot<-df_add_final %>%
  filter(var=="PAR")%>%
  filter(period=="post-sos")%>%
  ggplot(aes(x=period,y=var_value,fill=period))+
  geom_hline(yintercept = 0,lty=2,size=1.1)+
  geom_violin()+
  geom_boxplot(width = 0.1,position = position_dodge(0.9),col="grey",size=1.05) +
  stat_summary(fun = mean, geom = "point", size=3, color = "black")+
  ylab(expression(paste(Delta*"PAR (",mu*"mol m"^-2,"s"^-1,")")))+
  xlab("CH-DAV — DE-Tha")+
  scale_fill_manual(values = c("pre-sos"=adjustcolor("blue",0.3),
                                       "post-sos"=adjustcolor("blue",0.1)))+
  theme_classic()+
  theme(legend.position = "none",
        legend.background = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=22)
  )
##add sig. test
#PAR
var_PAR<-as.numeric(unlist(as.data.frame(df_add_final)%>%
                               filter(period=="post-sos" & var=="PAR")%>%
                               dplyr::select(var_value)))
ks.test(var_PAR,"pnorm")
wilcox.test(var_PAR)

#Ta
var_Ta<-as.numeric(unlist(as.data.frame(df_add_final)%>%
                             filter(period=="post-sos" & var=="Ta")%>%
                             dplyr::select(var_value)))
ks.test(var_Ta,"pnorm")
wilcox.test(var_Ta)
#
p_PAR_boxplot<-p_PAR_boxplot +
  annotate("text",x=1,y=200,label=c("*"),size=10)
p_Ta_boxplot<-p_Ta_boxplot +
  annotate("text",x=1,y=-2,label=c("*"),size=10)

##merge the plots:
# p_PAR<-plot_grid(p_PPFD_Dav,p_PPFD_Tha,p_PAR_boxplot,nrow = 1,align="hv",
#                  labels = c("(g)","(h)","(i)"))
# p_Ta<-plot_grid(p_Ta_Dav,p_Ta_Tha,p_Ta_boxplot,nrow = 1,align="hv",
#                  labels = c("(j)","(k)","(m)"))
p_PAR<-list("p_CH_Dav_PAR"=p_PPFD_Dav,
                  "p_DE_Tha_PAR"=p_PPFD_Tha,
                  "p_PAR_boxplot"=p_PAR_boxplot)
p_Ta<-list("p_CH_Dav_Ta"=p_Ta_Dav,
            "p_DE_Tha_Ta"=p_Ta_Tha,
            "p_Ta_boxplot"=p_Ta_boxplot)


###save the ggplot plots:
# p_MultiYears_Meteo<-list("p_Ta"=p_Ta,
#                       "p_PPFD"=p_PPFD,
#                       "p_boxplot"=p_boxplot)
save.path<-"./data/Comprehensive_plot_data/Fig1/"
save(p_PAR,file=paste0(save.path,"p_MultiYear_PAR.RDA"))
save(p_Ta,file=paste0(save.path,"p_MultiYear_Ta.RDA"))

