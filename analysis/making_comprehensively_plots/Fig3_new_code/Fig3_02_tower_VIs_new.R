##################################
##Aim:tower-based NDVI and PRI variations in Tharandt and Davos
##################################
library(dplyr)
library(tidyverse)
library(readxl)
#----------
#(1)load the data
#----------
##A.load the tower-based VIs
load.path<-"./data/Tower_based_VIs/"
load(paste0(load.path,"Davos_SIF_and_VIs.RDA"))
load(paste0(load.path,"Tharandt_VIs.RDA"))

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
df.all<-full_join(df.all,df.Meteo)
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

df.filter_max_DE_Tha<-max.filter(df.all[df.all$sitename=="DE-Tha",],
                                 c("PRI_norm"),
                          act.opts = data.frame(w=7,qt=0.5))
df.filter_max_CH_Dav<-max.filter(df.all[df.all$sitename=="CH-Dav",],
                                 c("PRI_norm"),
                                 act.opts = data.frame(w=7,qt=0.5))
df.filter_max<-rbind(data.frame(df.filter_max_CH_Dav,"sitename"="CH-Dav"),
                     data.frame(df.filter_max_DE_Tha,"sitename"="DE-Tha"))
df.all<-left_join(df.all%>%mutate(Date=as.Date(Date)),
                  df.filter_max%>%mutate(Date=as.Date(Date)),by=c("sitename","Date"))
#save the data:
file.path<-"./data/Tower_based_VIs/"
save(df.all,file=paste0(file.path,"Twosites_VIs.RDA"))

#test:
# df.all.tt<-df.all[df.all$sitename=="DE-Tha",]
# ggplot()+
#   geom_point(aes(x=Date,y=PRI_norm),data=df.all.tt)+
#   geom_point(aes(x=Date,y=PRI_norm.max.filtered),data = df.all.tt,col="red")
#----------
#(2)plotting
#----------
#using the snow data updated by Thomas in Mar, 2023:
snow_data_path<-"D:/EE_WSL/IMPACT_project/data_collection/THA_Data/Other_data_from_collabrators/Snow_depth/"
Tha_snow_depth<-read_excel(paste0(snow_data_path,"DE-Tha_snow.xlsx"),sheet = "snowdepth")
Tha_snow_depth<-Tha_snow_depth %>%
  mutate(sitename="DE-Tha")
names(Tha_snow_depth)<-c("Date","D_SNOW","sitename")

plot_fun<-function(df,var_name,Tha_snow){
  # df<-df.all
  # var_name<-"PRI_norm"
  # Tha_snow<-Tha_snow_depth

  #
  df<-df%>%
    mutate(doy=yday(Date))
  df_sel<-df%>%
      dplyr::select(Date,var_name,paste0(var_name,".max.filtered"),sitename,doy)%>%
      dplyr::filter(Date>=as.Date("2023-01-01") & Date<=as.Date("2023-12-31"))%>%
      filter(doy<=300)
  names(df_sel)<-c("Date","y","y.max.filtered","sitename","doy")
  ##sig.test:
  df_THA_stats<-df_sel%>%
    filter(sitename=="DE-Tha")%>%
    dplyr::select(sitename,Date,y)
  df_DAV_stats<-df_sel%>%
    filter(sitename=="CH-Dav")%>%
    dplyr::select(sitename,Date,y)
  #using data from Dav - Tha
  df_THA_stats<-df_THA_stats[,-1];names(df_THA_stats)<-c("Date","y.x")
  df_DAV_stats<-df_DAV_stats[,-1];names(df_DAV_stats)<-c("Date","y.y")
  df_retidy_stats<-left_join(df_DAV_stats,df_THA_stats,by=c("Date"))%>%
    mutate(diff_y=y.y - y.x,
           diff_Date=Date)%>%
    dplyr::select(diff_Date,diff_y)
  
  ##add sig.test:
  sig_test<-c()
  if(var_name %in% c("NDVI","PRI_norm")){
  var_VIs<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
                               mutate(doy=yday(diff_Date))%>%
                                 #select DOY<121--C1-C4
                                 filter(doy<121)%>%dplyr::select(diff_y)))
  var_VIs<-var_VIs[!is.na(var_VIs)]
  norm_test_Lower<-shapiro.test(var_VIs)
  
  ifelse(norm_test_Lower$p.value>0.05,
         sig.res<-t.test(var_Lower),sig.res<-wilcox.test(var_VIs))
  sig_test<-list("sig_pvalue"=round(sig.res$p.value,2),
                 "sig_n"=length(var_VIs))
  }
  print(sig_test)
  
  ## in Tharand as the VIs are only avaiable from April, 2023-->try to use the data
  ## in 2024 to illustrate the VI variation in Tharandt
  df_Tha_add<-df %>%
    dplyr::select(Date,var_name,paste0(var_name,".max.filtered"),sitename)%>%
    filter(sitename=="DE-Tha")%>%
    filter(Date>=as.Date("2024-01-01") & Date<=as.Date("2024-04-03"))%>%
    mutate(doy=yday(Date))
  #
  df_Tha_add<-left_join(df_Tha_add,Tha_snow)
  names(df_Tha_add)<-c("Date","y","y.max.filtered","sitename","doy","D_SNOW")
  df_Tha_add$pesudo_Date<-as.POSIXct(as.Date("2023-01-01")+yday(df_Tha_add$Date)-1)
  
  #plotting
  p_plot<-ggplot()+
    geom_point(aes(x=doy,y=y,col=sitename),data=df_sel)+
    scale_color_manual(values = c("CH-Dav"=adjustcolor("red",0.4),
                                  "DE-Tha"=adjustcolor("orange",0.4)))+
    geom_line(aes(doy,y.max.filtered),data=df_sel[df_sel$sitename=="DE-Tha",],
              col="orange",linewidth=1.1)+
    geom_line(aes(doy,y.max.filtered),data=df_sel[df_sel$sitename=="CH-Dav",],
              col="red",linewidth=1.1)+
    ##add additional data for Tharandt-->using 2024-01--2024-04 data-->using dashed line
    #update in Jan,2026-->do not use this
    # geom_point(aes(x=doy,y=y,col=sitename),data=df_Tha_add)+
    # geom_line(aes(doy,y.max.filtered),data=df_Tha_add[df_Tha_add$sitename=="DE-Tha",],
    #           col="orange",linewidth=1.1,lty=2)+
    theme_classic()+
    ylim(-0.2,1)+
    ylab(var_name)+
    xlab("")+
    theme(legend.position = c(0.8,0.25),
          legend.title = element_blank(),
          legend.text = element_text(size = 22),
          legend.background = element_blank(),
          axis.text = element_text(size=20),
          axis.title = element_text(size=22)
    )
  if(var_name=="NDVI"){
    p_plot<-p_plot+
      ylim(-0.05,0.9)
  }
  #
  if(var_name=="PRI"){
    p_plot<-p_plot+
      ylim(-0.05,1.2)
  }
  
  ###adding snow:
    p_plot<-p_plot+
      #adding snowing period
      geom_bar(aes(x=doy,y=D_SNOW/50),stat="identity",fill=adjustcolor("red",0.2),
               data = df %>% filter(sitename=="CH-Dav")%>%
                 filter(Date>=as.Date("2023-01-01") & doy<=300)%>%
                 filter(!is.na(D_SNOW) & D_SNOW>0))+
      #2023-DE-Tha
      geom_bar(aes(x=doy,y=D_SNOW/50),stat="identity",fill=adjustcolor("orange",0.6),
               data = df %>% filter(sitename=="DE-Tha")%>%
                 filter(Date>=as.Date("2023-04-03")& doy<=300)%>%
                 filter(!is.na(D_SNOW) & D_SNOW>0))+
      #2024-DE-Tha
      # geom_bar(aes(x=doy,y=D_SNOW/50),stat="identity",fill=adjustcolor("orange",0.6),
      #          data = df_Tha_add %>%
      #            filter(!is.na(D_SNOW) & D_SNOW>0))+
      #add second axix
      scale_y_continuous(
        limits = c(-0.05,1.2),
        sec.axis = sec_axis(~ . *50, name = expression("D"[snow]*" (cm)" ))  # 反向缩放
      )
      #2024-DE-Tha
      # geom_bar(aes(x=Date,y=D_SNOW/50),stat="identity",fill=adjustcolor("orange",0.6),
      #          data = df %>% filter(sitename=="DE-Tha")%>%
      #            filter(Date>=as.Date("2024-01-01")&Date<=as.Date("2024-04-03"))%>%
      #            #pesudo Date
      #            mutate(Date=Date-365)%>%
      #            filter(!is.na(D_SNOW) & D_SNOW>0))
      # +
      # scale_fill_manual(values = c("CH-Dav"=adjustcolor("tomato",0.1),
      #                               "DE-Tha"=adjustcolor("cyan3",0.1)))
      return(p_plot)
}
##
p_NDVI<-plot_fun(df.all,"NDVI",Tha_snow_depth)+
  xlab("DOY (2023)")+
  ylab(expression("NDVI"[Crown]))
# plot_fun(df.all,"PRI") # the absolute PRI in two sites are different-->hence
# normalize it
p_PRI<-plot_fun(df.all,"PRI_norm",Tha_snow_depth)+
  ylab(expression("Norm PRI"[Crown]))+
  # theme(legend.position = "none")+
  xlab("DOY (2023)")
p_SIF<-plot_fun(df.all,"SIF_a_sfm",Tha_snow_depth)+
  theme(legend.position = "none")+
  ylab(expression("SIF (mW m"^-2*"nm "^-1*sr^-1*")"))
#further need to plot SIF/PAR-->the pattern does not look very clear
p_SIF_over_PAR<-plot_fun(df.all,"SIF_over_PAR",Tha_snow_depth)+
  theme(legend.position = "none")+
  ylab(expression("SIF/PAR (nm "^-1*")"))

#merge the plots:
library(cowplot)
p_crown_NDVI<-p_NDVI
# plot_grid(p_NDVI,nrow=1,
#                        labels = c("(b)"))
p_crown_PRI<-p_PRI
# plot_grid(p_PRI,nrow=1,
#                         labels = c("(b)"))
##adding the lines to indicate the field sampling dates:
Date.Dav<-as.Date(c("2023-03-08","2023-03-27","2023-04-21",
          "2023-05-03","2023-05-22","2023-07-17"))
Date.Tha<-as.Date(c("2023-03-02","2023-03-22","2023-04-13",
          "2023-04-28","2023-05-17","2023-07-14"))
#
Date.Dav_DOY<-yday(Date.Dav)
Date.Tha_DOY<-yday(Date.Tha)

#
p_crown_PRI<-p_crown_PRI+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  xlim(0,250)+
  annotate(geom = "text",x=c(64),y=rep(-0.05),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.05),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.05),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.05),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.05),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.05),label=c("C6"),size=6)

p_crown_NDVI<-p_crown_NDVI+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  xlim(0,250)+
  annotate(geom = "text",x=c(64),y=rep(-0.05),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.05),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.05),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.05),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.05),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.05),label=c("C6"),size=6)

##adding the DSPR period and sig. symbol
Dav_pheno<-data.frame("sos10"=61,"sos25"=65,"peak"=180,"eos25"=296,"eos10"=322)
Tha_pheno<-data.frame("sos10"=61,"sos25"=70,"peak"=177,"eos25"=285,"eos10"=312)

#period:[sos-60,sos+60] is set to red
#adding the sos
#for CH-Dav
rect.coord_isevent_Dav=data.frame("x1"=as.numeric(Dav_pheno[1])-60,
                                  "x2"=as.numeric(Dav_pheno[1]),
                                  "x3"=as.numeric(Dav_pheno[1]+60),
                                  "y1"=-Inf,
                                  "y2"=Inf)
rect.coord_isevent_Tha=data.frame("x1"=as.numeric(Tha_pheno[1])-60,
                                  "x2"=as.numeric(Tha_pheno[1]),
                                  "x3"=as.numeric(Tha_pheno[1]+60),
                                  "y1"=-Inf,
                                  "y2"=Inf)
plot_add_rectanges<-function(df.plot,rect.coord_isevent,x_text,y_text){
  # df.plot<-p_Fv.Fm_Date_Dav
  # rect.coord_isevent<-rect.coord_isevent_Dav
  # x_text<-Dav_pheno[1]
  # y_text<-0.4
  
  df_add<-df.plot+
    # annotate("rect",xmin = rect.coord_isevent$x1,
    #          xmax=rect.coord_isevent$x2,
    #          ymin = -Inf, ymax = Inf,
    #          fill="blue",alpha=0.3
    #          )+
    annotate("rect",xmin = rect.coord_isevent$x2,
             xmax=rect.coord_isevent$x3,
             ymin = -Inf, ymax = Inf,
             fill="blue",alpha=0.1
    )
  # geom_vline(xintercept=as.numeric(x_text),col="forestgreen",size=1.1)+
  # annotate(geom = "text",x=as.numeric(x_text)+10,y=y_text,label="sos",col="forestgreen",size=6)
  
  return(df_add)
}

#for DSPR period for pigments:
p_crown_NDVI<-plot_add_rectanges(p_crown_NDVI,rect.coord_isevent_Dav,Dav_pheno[1],0.5)
p_crown_PRI<-plot_add_rectanges(p_crown_PRI,rect.coord_isevent_Dav,Dav_pheno[1],0.5)

##adding the sig. symbol:
p_crown_NDVI<-p_crown_NDVI +
  annotate("text",x=c(100),y=c(0.82),label=c("***"),
           size=10)
p_crown_PRI<-p_crown_PRI +
  annotate("text",x=c(100),y=c(1.1),label=c("*"),
           size=10)

#save the plot
save.path<-"./data/Comprehensive_plot_data/Fig3/"
save(p_crown_NDVI,file=paste0(save.path,"p_crown_NDVI_new.RDA"))
save(p_crown_PRI,file=paste0(save.path,"p_crown_PRI_new.RDA"))
