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
load.path<-"./data/EC_MeteoandFlux/"
load(paste0(load.path,"df_daily_from_ICOS.RDA"))

df.Dav<-df_DD$Dav
df.Tha<-df_DD$Tha

#mrege data from two sites
df.Dav<-df.Dav %>%
  mutate(sitename="CH-Dav")
df.Tha<-df.Tha %>%
  mutate(sitename="DE-Tha")
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
  ggplot(aes(x=DoY,y=SW_IN))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

df %>%
  filter(sitename=="CH-Dav")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=TA))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)


##-----------
#making the plots:
##-----------
plot_fun_meteo<-function(df,var_name,legend_flag){
  # df<-df
  # var_name<-"TA"
  # legend_flag<-TRUE

  df.proc<-df %>%
    dplyr::select(sitename,Date,var_name,DoY)
  names(df.proc)<-c("sitename","Date","var","DoY")
  
  df.use<-df.proc %>%
    group_by(sitename,DoY)%>%
    dplyr::summarise(var_mean=mean(var,na.rm=T),
                     var_sd=sd(var,na.rm=T))
  #
  p_plot<-df.use %>%
    ggplot()+
    geom_ribbon(
      aes(x = DoY, ymin = var_mean - var_sd, 
          ymax = var_mean + var_sd, fill=sitename),
      alpha = 0.2
    ) +
    xlab("DOY")+
    geom_line(aes(x=DoY,y=var_mean,col=sitename))+
    scale_color_manual(values = c("CH-Dav"="brown1",
                                  "DE-Tha"="orange"))+
    scale_fill_manual(values = c("CH-Dav"="brown1",
                                 "DE-Tha"="orange"))+
    theme(legend.text = element_text(size = 20),
          legend.background = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size=16),
          axis.title = element_text(size=20))
  if(var_name=="PPFD_IN"){
    p_plot<-p_plot+
      xlim(0,250)+
      # ylim(0,750)+
      ylab(expression("PAR ("*mu*"mol m"^-2*s^-1*")"))+
      theme_light()+
      theme(legend.position = c(0.8,0.3))+
      theme(legend.text = element_text(size = 20),
            legend.background = element_blank(),
            legend.title = element_blank(),
            axis.text = element_text(size=16),
            axis.title = element_text(size=20))
  }
  if(var_name=="TA"){
    p_plot<-p_plot+
      xlim(0,250)+
      # ylim(0,750)+
      ylab(expression("Ta ("* "°C)"))+
      theme_light()+
      theme(legend.position = c(0.8,0.3),
            legend.title = element_blank())+
      theme(legend.text = element_text(size = 20),
            legend.background = element_blank(),
            legend.title = element_blank(),
            axis.text = element_text(size=16),
            axis.title = element_text(size=20))
  }
  if(legend_flag==FALSE){
    p_plot<-p_plot+theme(legend.position = "none")
  }
  
  return(p_plot)
}

#
p_Ta<-plot_fun_meteo(df,"TA",TRUE)
p_PPFD<-plot_fun_meteo(df,"PPFD_IN",FALSE)

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
p_Ta<-p_Ta+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.4)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.2)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=-10,label="sos",col="forestgreen",size=6)

p_PPFD<-p_PPFD+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.4)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.2)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=0,label="sos",col="forestgreen",size=6)

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
  # xlab("CH-DAV - DE-Tha")+
  xlab("")+
  scale_fill_manual(values = c("pre-sos"=adjustcolor("blue",0.4),
                               "post-sos"=adjustcolor("blue",0.2)))+
  theme_light()+
  theme(legend.position = "none",
        legend.background = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        # axis.text.x = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20)
  )

##
p_boxplot<-plot_grid(p_Ta_boxplot,p_PAR_boxplot,nrow = 2,align = "h")

p_MultiYears_Meteo<-plot_grid(p_Ta,p_PPFD,p_boxplot,nrow=1,
          labels = c("(d)","(e)","(f)"))
###save the ggplot plots:
# p_MultiYears_Meteo<-list("p_Ta"=p_Ta,
#                       "p_PPFD"=p_PPFD,
#                       "p_boxplot"=p_boxplot)
save.path<-"./data/Comprehensive_plot_data/Fig1/"
save(p_MultiYears_Meteo,file=paste0(save.path,"p_MultiYear_Meteo.RDA"))

