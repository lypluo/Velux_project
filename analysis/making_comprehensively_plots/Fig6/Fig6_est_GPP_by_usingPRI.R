########################################################
#Aim: compare the EC GPP and PRI for Davos:
#to check if PRI could represent the stress of site
########################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(phenopix)
library(cowplot)
#----------------------
#(1)load the data
#----------------------
##----------------------
##load the VI and SIF from the CH-Dav:May, 2021 - Dec,2023
##----------------------
load.path<-"./data/Tower_based_VIs/"
load(paste0(load.path,"Davos_SIF_and_VIs.RDA"))
df.ts_VIs<-df.all.Dav %>%
  mutate(Date=as.Date(Date))
rm(df.all.Dav)

##load the EC GPP from the CH-Dav:
load.path<-"./data/EC_MeteoandFlux/"
load(paste0(load.path,"df_daily_from_ICOS.RDA"))
df.ts_GPP<-df_DD$Dav

##load the simulated GPP from P-model for CH-Dav:
load.path<-"./data/Simulate_GPP_Pmodel_in2sites/"
load(paste0(load.path,"Sim_GPP.RDA"))
df.ts_GPP_sim<-df_gpp_sim%>%
  filter(sitename=="CH-Dav")%>%
  mutate(Date=date,date=NULL)

#merge the datasets:
df.ts_GPP_20212023<-df.ts_GPP %>%
  filter(Date>=as.Date("2021-01-01")& Date<=as.Date("2023-12-31"))
df.all<-left_join(df.ts_GPP_20212023,df.ts_VIs)
df.all<-left_join(df.all,df.ts_GPP_sim)

##---------------------------
##load the snow data
##---------------------------
load.path<-"D:/data/Velux_shared_data/CH-Dav/Snow_depth_fromICOS/"
df.Meteo.Dav<-read.csv(file=paste0(load.path,"ICOSETC_CH-Dav_METEO_INTERIM_L2.csv"),header = T)
df.Meteo.Dav_sel<-df.Meteo.Dav %>%
  dplyr::select(TIMESTAMP_START,TA,D_SNOW,RH,LW_IN,LW_OUT)
names(df.Meteo.Dav_sel)<-c("rDate","Ta","D_SNOW","RH","LW_IN","LW_OUT")
#
df.Snow_Daily<-df.Meteo.Dav_sel %>%
  mutate(rDate=ymd_hm(as.character(rDate)))%>%
  mutate(Date=as.Date(rDate))%>%
  mutate(D_SNOW=ifelse(D_SNOW==-9999,NA,D_SNOW),
         Ta=ifelse(Ta==-9999,NA,Ta),
         RH=ifelse(RH==-9999,NA,RH),
         LW_IN=ifelse(LW_IN==-9999,NA,LW_IN),
         LW_OUT=ifelse(LW_OUT==-9999,NA,LW_OUT)
         )%>%
  group_by(Date)%>%
  mutate(Month=month(Date))%>%
  ##addtional filter:set snow depth data to NA if the D_SNOW>0 in summer(June-Sep)
  mutate(D_SNOW=ifelse(Month>=6 & Month<=9,NA,D_SNOW))%>%
  dplyr::summarise(D_SNOW=mean(D_SNOW,na.rm = T),
            Ta_mean=mean(Ta,na.rm = T),
            Ta_min=min(Ta,na.rm = T),
            RH=mean(RH,na.rm =T),
            LW_IN=mean(LW_IN,na.rm=T),LW_OUT=mean(LW_OUT,na.rm=T)
            )%>%
  #remove the Inf
  mutate(D_SNOW=ifelse(is.infinite(D_SNOW),NA,D_SNOW),
    Ta_mean=ifelse(is.infinite(Ta_mean),NA,Ta_mean),
    Ta_min=ifelse(is.infinite(Ta_min),NA,Ta_min),
    )%>%
  #set the negative value as NA
  mutate(D_SNOW=ifelse(D_SNOW<0,NA,D_SNOW))

###merge the data
#update in Oct, 2025:
df.Snow_Daily<-df.Snow_Daily %>%
  #remove the variables(with many NAs)
  mutate(LW_IN=NULL,LW_OUT=NULL)

df.all<-left_join(df.all,df.Snow_Daily)%>%
  #calculate the dew temperature:T_dew:
  #based on Magnus equation:
  #C2 in https://hess.copernicus.org/articles/26/6263/2022/#bib1.bibx88
  mutate(T_dew=17.62 *c(log(RH/100)+243.12*TA/c(17.62+TA))/c(243.12-c(log(RH/100)+243.12*TA/c(17.62+TA))))%>%
  #another equation from chatgpt
  # mutate(T_dew=c(237.7*c(log(RH/100)+17.27*TA/c(237.7+TA)))/c(17.27 - c(log(RH/100)+17.27*TA/c(237.7+TA))))%>%
  #frost temperature (Tf)
  mutate(T_frost=T_dew - c(T_dew/c(1+0.000425*T_dew)))%>%
  #calculate the surface temperature:C1 in 
  #https://hess.copernicus.org/articles/26/6263/2022/#bib1.bibx88
  #
  mutate(Ts=c(c(LW_OUT-(1-0.98)*LW_IN)/c(0.98*5.67*10^(-8)))^(1/4)-273.15)

##---------------------------
##load the absorbed PAR and LUE data:
##---------------------------
df_LUE<-readRDS(paste0("./data/Comprehensive_plot_data/Fig2/","LUE_2sites.RDS"))
df_LUE_Dav<-df_LUE%>%
  filter(Sitename=="CH-Dav")%>%
  dplyr::select(Date,PAR_above:LUE)
#merge the data:
df.all<-left_join(df.all,df_LUE_Dav)%>%
  mutate(LUE_ori=GPP/PAR_absorbed)

#----------------------
#(2)making the plots
#----------------------
#test:checking the GPP and PRI
# par(mfrow=c(2,1))
# plot(df.all$Date,df.all$GPP)
# plot(df.all$Date,df.all$PRI)
# points(df.all$Date,df.all$PRI.max.filtered,col="red")

#filter the PRI--remove the outliers
source("./R/SplineFilter_gapfill_VIs.R")
tt<-SplineFilter_ts(df.ts_VIs,"PRI",-1)
df.ts_PRI<-df.ts_VIs %>%
  dplyr::select(Date,PRI)
  
df.ts_PRI_new<-data.frame(df.ts_PRI,
  "PRI.max.filtered"=as.numeric(tt))
df.final<-left_join(df.all,df.ts_PRI_new)
#test
# par(mfrow=c(2,1))
# plot(df.final$Date,df.final$GPP)
# plot(df.final$Date,df.final$PRI)
# points(df.final$Date,df.final$PRI.max.filtered,col="red")

#----------------------
#making the plots--comparing the PRI and GPP
#----------------------
#norm the PRI
norm_01<-function(y){
  y_95<-quantile(y,0.95,na.rm=T)
  y_5<-quantile(y,0.05,na.rm=T)
  y_norm<-c(y-y_5)/c(y_95-y_5)
  return(y_norm)
}
#
df_final_plot<-df.final %>%
  mutate(PRI_norm=norm_01(PRI),
         PRI_filter_norm=norm_01(PRI.max.filtered))
###norm PRI_norm with APAR
df_final_plot<-df_final_plot %>%
  mutate(PRI_norm_over_APAR=PRI_norm/PAR_absorbed)

#plot1:
#with two y axis--y1:GPP, y2:PRI
p_GPP_PRI <- ggplot(df_final_plot, aes(x = Date)) +
  geom_point(aes(y = GPP), color = "blue") +  # 第一个 Y 轴的线
  ylim(-4.5,12)+
  geom_point(aes(y = PRI_norm * 8), color = adjustcolor("red",0.2)) +  # 第二个 Y 轴的线，缩放
  geom_line(aes(y = PRI_filter_norm * 8), color = "red") +  # 第二个 Y 轴的线，缩放
  scale_y_continuous(
    name = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
    sec.axis = sec_axis(~ . /8, name = "Norm PRI")  # 反向缩放
  ) +
  # labs(title = "Dual Y-Axis Plot", x = "X-axis") +
  theme_classic()+
  ##adding the CH-Dav
  annotate(geom = "text",x=as.Date("2021-03-31"),
           y=12,label = "CH-Dav",size=8)+
  theme(
    axis.title.y = element_text(color="blue"),
    axis.text.y = element_text(color="blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color="red"),
    legend.text = element_text(size=20),
    axis.title = element_text(size=24),
    axis.text = element_text(size = 20),
    text = element_text(size=24)
    )
##adding overestimation period:
Dav_sos<-61 #or 65
rect.coord_isevent_2021=data.frame(x1=Dav_sos-60+as.Date("2021-01-01"),
                              x2=Dav_sos+as.Date("2021-01-01"),
                              x3=Dav_sos+60+as.Date("2021-01-01"),
                              y1=-Inf, 
                              y2=Inf)
rect.coord_isevent_2022=data.frame(x1=Dav_sos-60+as.Date("2022-01-01"),
                                   x2=Dav_sos+as.Date("2022-01-01"),
                                   x3=Dav_sos+60+as.Date("2022-01-01"),
                                   y1=-Inf, 
                                   y2=Inf)
rect.coord_isevent_2023=data.frame(x1=Dav_sos-60+as.Date("2023-01-01"),
                                   x2=Dav_sos+as.Date("2023-01-01"),
                                   x3=Dav_sos+60+as.Date("2023-01-01"),
                                   y1=-Inf, 
                                   y2=Inf)
##adding DSPR period:
p_GPP_PRI<-p_GPP_PRI+
  # geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #         mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  # geom_rect(data=rect.coord_isevent_2022,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2022,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  # geom_rect(data=rect.coord_isevent_2023,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2023,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  ##adding the extreme cold event:
  geom_vline(xintercept = as.Date("2023-03-27"),col="purple")+
  annotate(geom="text",x=as.Date("2023-03-27"),y=-2,
           label="Mar,27",col="purple",size=6)
  # geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  # annotate(geom = "text",x=as.numeric(Dav_pheno[1])+10,y=0,label="sos",col="forestgreen",size=6)
p_GPP_PRI<-p_GPP_PRI+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

#plot2: plotting Ta and Snow depth:
#with two y axis--y1:Ta, y2:Snow depth
p_Ta_Snow <- ggplot(df_final_plot, aes(x = Date)) +
  geom_point(aes(y = Ta_min), color = "black") +  # 第一个 Y 轴的线
  geom_point(aes(y = Ta_mean), color = adjustcolor("grey",0.5)) +  # 第一个 Y 轴的线
  ##add -5 degree as the cold spell
  geom_hline(yintercept = -5, lty=2,size=1.1)+
  # ylim(-20,25)+
  geom_bar(aes(x=Date,y=c(D_SNOW/3)),stat="identity",fill=adjustcolor("red",0.4),
           data=df_final_plot)+# 第二个 Y 轴的线，缩放
  scale_y_continuous(
    name = expression("Ta ("* "°C)"),
    limits = c(-12,20),
    sec.axis = sec_axis(~ . *3, name = expression("D"[snow]*" (cm)" ))  # 反向缩放
  ) +
  # labs(title = "Dual Y-Axis Plot", x = "X-axis") +
  theme_classic()+
  ##adding the CH-Dav
  # annotate(geom = "text",x=as.Date("2021-03-31"),
  #          y=15,label = "CH-Dav",size=8)+
  theme(
    axis.title.y = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color="red"),
    legend.text = element_text(size=20),
    axis.title = element_text(size=24),
    axis.text = element_text(size = 20),
    text = element_text(size=24)
  )
##adding DSPR period:
p_Ta_Snow<-p_Ta_Snow+
  # geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  # geom_rect(data=rect.coord_isevent_2022,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2022,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  # geom_rect(data=rect.coord_isevent_2023,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2023,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  ##adding the extreme cold event:
  geom_vline(xintercept = as.Date("2023-03-27"),col="purple")+
  annotate(geom="text",x=as.Date("2023-03-27"),y=-10,
           label="Mar,27",col="purple",size=6)

#merge plot1 &2
# p_GPP_PRI_merge<-plot_grid(p_GPP_PRI,p_Ta_Snow,align="v",
#                              ncol=1,nrow=2,
#           rel_heights = c(0.6,0.4)
#           )

################
#(3)test relationship between PRI and GPP along the Tmin
################
#indicate the data when snow happens in growing season period:
#according to previous study, the sos10(sos) of Davos is 61
#peak = 180
#eos= 322/296
df_final_plot_f<-df_final_plot %>%
  mutate(doy=yday(Date))%>%
  mutate(greenup=ifelse(doy>=61 & doy<=180,"GP","NoGP"))%>%
  mutate(W_to_S=ifelse(c(doy>=1 & doy <=243)|doy>334,"WintertoSummer","Other"))%>%
  mutate(GS=ifelse(doy>=61 & doy <=296,"GS","NoGS"))%>%
  mutate(snow_event=ifelse(!is.na(D_SNOW) & D_SNOW>0,"Snow","NoSnow"))%>%
  #add the cold event:
  mutate(cold_event=ifelse(!is.na(Ta_min) & Ta_min< -5,"cold","notcold"))%>%
  mutate(frost_event=ifelse(!is.na(T_dew) & c(Ts <T_dew) & Ts <0, "frost","nofrost"))
  # mutate(frost_event=ifelse(!is.na(T_dew) & T_dew <-20, "frost","nofrost"))

##########
##A. test GPP/LUE vs LUE along Tmin
#########
##a.using GPP data:
p_GPP_Tmin<-df_final_plot_f%>%
  ggplot(aes(x=Ta_min,y=GPP,col=frost_event))+
  geom_point()+
  geom_vline(xintercept = c(-5,0),col="blue",lty=2)

p_PRI_Tmin<-df_final_plot_f%>%
  ggplot(aes(x=Ta_min,y=PRI_norm,col=frost_event))+
  geom_point()+
  #mark the data with GPP<0
  geom_point(aes(x=Ta_min,y=PRI_norm),col=adjustcolor("gray",0.5),
             data=df_final_plot_f%>%filter(GPP<0))+
  geom_vline(xintercept = c(-5,0),col="blue",lty=2)

# p_PRI_overAPAR_Tmin<-df_final_plot_f%>%
#   ggplot(aes(x=Ta_min,y=PRI_norm_over_APAR,col=frost_event))+
#   geom_point()+
#   ylim(0,0.01)+
#   #mark the data with GPP<0
#   geom_point(aes(x=Ta_min,y=PRI_norm_over_APAR),col=adjustcolor("gray",0.5),
#              data=df_final_plot_f%>%filter(GPP<0))+
#   geom_vline(xintercept = c(-5,0),col="blue",lty=2)
# plot_grid(p_PRI_Tmin,
#           p_PRI_overAPAR_Tmin,ncol=1)

##b.using LUE data
p_LUE_Tmin<-df_final_plot_f%>%
  ggplot(aes(x=Ta_min,y=LUE_ori,col=frost_event))+
  geom_point()+
  ylim(-0.05,0.075)+
  geom_vline(xintercept = c(-5,0),col="blue",lty=2)

##
plot_grid(p_GPP_Tmin,p_LUE_Tmin,
          p_PRI_Tmin,
          align="hv",
          ncol=1,nrow=3,
          labels = c("(a)","(b)","(c)"))

##put PRI and GPP together:
df_sel<-df_final_plot_f %>%
  dplyr::select(Date,GPP,gpp_mod,LUE_ori,LUE,PRI_norm,PRI_filter_norm,Ta_min,frost_event,cold_event)%>%
  pivot_longer(c(GPP,gpp_mod,LUE,LUE_ori,PRI_norm,PRI_filter_norm),names_to = "Indices",values_to = "y")

###show Indices vs Tmin in each panel:
df_sel.proc<-df_sel%>%
  filter(Indices %in% c("LUE","PRI_filter_norm"))%>%
  filter(!is.na(y))

###For frost condition:
plot_Tmin_Y_underFrost<-function(df,var_name,y_range,text_y,legend_flag){
  # df<-df_sel.proc
  # var_name<-"LUE"
  # y_range<-c(-0.01,0.07)
  # text_y<- -0.3
  # legend_flag<-TRUE
  
  ##
  p_LUE_PRI_Tmin_varywith_frost<-df%>%
    filter(!is.na(frost_event))%>%
    filter(Indices==var_name)%>%
    ggplot(aes(x=Ta_min,y=y,color=frost_event))+
    geom_point()+
    scale_color_manual(values = c("frost"=adjustcolor("red",alpha.f =  0.5),
                                  "nofrost"=adjustcolor("orange",alpha.f =  0.5))
                       )+
    geom_vline(xintercept = c(-5,0),col="blue",lty=2)+
    geom_smooth(data=df%>%filter(Indices==var_name),
                method = "loess",
                se=TRUE,fill=adjustcolor("black",0.4),color="black")+
    geom_smooth(data=df%>%filter(Indices==var_name & frost_event=="nofrost"),
                method = "loess",
                se=TRUE,fill=adjustcolor("orange",0.4),color="orange")+
    geom_smooth(data=df%>%filter(Indices==var_name & frost_event=="frost"),
                method = "loess",
                se=TRUE,fill=adjustcolor("red",0.4),color="red")
    # geom_smooth(data=df_sel.proc,  #logistic regression-->y必须设置到0-1范围
    #             method = "glm", 
    #             method.args = list(family = "binomial"), 
    #             formula = y ~ x,
    #             se=TRUE,fill=adjustcolor("orange",0.4),color="orange")+
  p_LUE_PRI_Tmin_varywith_frost<-p_LUE_PRI_Tmin_varywith_frost+
    theme_classic()+
    xlab(expression("T"[min]*" (°C)"))+
    ylab(var_name)+
    ylim(y_range)+
    #
    annotate(geom = "text",x=-5+1.5,y=text_y,label="-5",size=5)+
    theme(
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text = element_text(size=16),
      legend.position = c(0.1,0.9),
      axis.text = element_text(size=16),
      axis.title = element_text(size=20))
  if(legend_flag==FALSE){
  p_LUE_PRI_Tmin_varywith_frost<-p_LUE_PRI_Tmin_varywith_frost+
    theme(legend.position = "none")
  }
  
  return(p_LUE_PRI_Tmin_varywith_frost)
}
p_LUE_Tmin_underFrost<-plot_Tmin_Y_underFrost(df_sel.proc,
                                              "LUE",c(-0.01,0.07),
                                              -0.01,TRUE)+
  ylab(expression("LUE"*" ("*mu*"mol m"^-2*mu*"mol"^-1*")"))+
  xlab("")
p_PRI_Tmin_underFrost<-plot_Tmin_Y_underFrost(df_sel.proc,
                                              "PRI_filter_norm",c(-0.3,1.1),
                                              -0.3,FALSE)+
  ylab("Filtered Norm PRI")+
  xlab(expression("T"[min]*" (°C)"))
#
plot_grid(p_LUE_Tmin_underFrost,
          p_PRI_Tmin_underFrost,ncol=1)

###For cold condition
plot_Tmin_Y_underCold<-function(df,var_name,y_range,text_y,legend_flag){
  # df<-df_sel.proc
  # var_name<-"LUE"
  # y_range<-c(-0.01,0.07)
  # text_y<- -0.3
  #legend_flag<-TRUE
  
  ##
  p_LUE_PRI_Tmin_varywith_cold<-df%>%
    filter(!is.na(cold_event))%>%
    filter(Indices==var_name)%>%
    ggplot(aes(x=Ta_min,y=y,color=cold_event))+
    geom_point()+
    scale_color_manual(values = c("cold"=adjustcolor("red",alpha.f =  0.5),
                                  "notcold"=adjustcolor("orange",alpha.f =  0.5)),
                       #update in Oct, 2026
                       labels = c("cold"=expression("T"[min]*" < -5 °C"),
                                  "notcold"=expression("T"[min]*" >= -5 °C"))
                       )+
    geom_vline(xintercept = c(-5,0),col="blue",lty=2)+
    geom_smooth(data=df%>%filter(Indices==var_name),
                method = "loess",
                se=TRUE,fill=adjustcolor("black",0.4),color="black")+
    geom_smooth(data=df%>%filter(Indices==var_name & cold_event=="notcold"),
                method = "loess",
                se=TRUE,fill=adjustcolor("orange",0.4),color="orange")+
    geom_smooth(data=df%>%filter(Indices==var_name & cold_event=="cold"),
                method = "loess",
                se=TRUE,fill=adjustcolor("red",0.4),color="red")
  # geom_smooth(data=df_sel.proc,  #logistic regression-->y必须设置到0-1范围
  #             method = "glm", 
  #             method.args = list(family = "binomial"), 
  #             formula = y ~ x,
  #             se=TRUE,fill=adjustcolor("orange",0.4),color="orange")+
  p_LUE_PRI_Tmin_varywith_cold<-p_LUE_PRI_Tmin_varywith_cold+
    theme_classic()+
    xlab(expression("T"[min]*" (°C)"))+
    ylab(var_name)+
    ylim(y_range)+
    #
    annotate(geom = "text",x=-5+1.5,y=text_y,label="-5",size=5)+
    theme(
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text = element_text(size=16),
      legend.position = c(0.15,0.9),
      axis.text = element_text(size=16),
      axis.title = element_text(size=20))
  if(legend_flag==FALSE){
    p_LUE_PRI_Tmin_varywith_cold<-p_LUE_PRI_Tmin_varywith_cold+
      theme(legend.position = "none")
  }
  
  return(p_LUE_PRI_Tmin_varywith_cold)
}
p_LUE_Tmin_underCold<-plot_Tmin_Y_underCold(df_sel.proc,
                                              "LUE",c(-0.01,0.07),
                                              -0.01,TRUE)+
  ylab(expression("LUE"*" ("*mu*"mol C m"^-2*mu*"mol"^-1*")"))+
  xlab("")
p_PRI_Tmin_underCold<-plot_Tmin_Y_underCold(df_sel.proc,
                                              "PRI_filter_norm",c(-0.3,1.1),
                                              -0.3,FALSE)+
  ylab("Filtered Norm PRI")+
  xlab(expression("T"[min]*" (°C)"))

#
plot_grid(p_LUE_Tmin_underCold,
          p_PRI_Tmin_underCold,ncol=1)

##########
##B. test GPP or LUE vs PRI along Tmin
#########
library(ggpmisc)
###
p_lm_cold<-df_final_plot_f%>%
  filter(!is.na(cold_event))%>%
  ggplot(aes(x=PRI_filter_norm,y=LUE,color=cold_event))+
  geom_point()+
  scale_color_manual(values = c("cold"=adjustcolor("red",alpha.f =  0.5),
                                "notcold"=adjustcolor("orange",alpha.f =  0.5)),
                     #update in Oct, 2026
                     labels = c("cold"=expression("T"[min]*" < -5 °C"),
                                "notcold"=expression("T"[min]*" >= -5 °C"))
                     )+
  geom_smooth(data=df_final_plot_f%>%filter(cold_event=="notcold"),
              method = "lm",
              se=TRUE,fill=adjustcolor("orange",0.4),color="orange")+
  stat_poly_eq(data=df_final_plot_f%>%filter(cold_event=="notcold"),
               geom = "text",position = "identity",
               col="orange",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = 0.4, label.y = 0.04)+
  annotate(geom = "text",x=0.3,y=0.037,
           label = paste0("p < 0.001"),col="orange",size=6)+
  ylab(expression("LUE"*" ("*mu*"mol m"^-2*mu*"mol"^-1*")"))+
  xlab("Filtered Norm PRI")+
  ylim(0,0.04)+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = c(0.12,0.9),
    axis.text = element_text(size=16),
    axis.title = element_text(size=20))

#using the PRI_filter_norm to develop the linear relationship-->estimate GPP is better
#compared using PRI_norm
t_lm1<-lm(data=df_final_plot_f %>%filter(cold_event=="notcold"),
         LUE ~ PRI_filter_norm)
t_lm1_cold<-lm(data=df_final_plot_f %>%filter(cold_event=="cold"),
          LUE ~ PRI_filter_norm)
stat_t_lm1<-summary(t_lm1)
summary(t_lm1_cold)

###
p_lm_frost<-df_final_plot_f%>%
  filter(!is.na(frost_event))%>%
  ggplot(aes(x=PRI_filter_norm,y=LUE,color=frost_event))+
  geom_point()+
  scale_color_manual(values = c("frost"=adjustcolor("red",alpha.f =  0.5),
                                "nofrost"=adjustcolor("orange",alpha.f =  0.5)))+
  geom_smooth(data=df_final_plot_f%>%filter(frost_event=="nofrost"),
              method = "lm",
              se=TRUE,fill=adjustcolor("orange",0.4),color="orange")+
  stat_poly_eq(data=df_final_plot_f %>%filter(frost_event=="nofrost" ),
               geom = "text",position = "identity",
               col="orange",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = 0.4, label.y = 0.04)+
  annotate(geom = "text",x=0.3,y=0.037,
           label = paste0("p < 0.001"),col="orange",size=6)+
  ylab(expression("LUE"*" ("*mu*"mol m"^-2*mu*"mol"^-1*")"))+
  xlab("Filtered Norm PRI")+
  ylim(0,0.04)+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = c(0.1,0.9),
    axis.text = element_text(size=16),
    axis.title = element_text(size=20))

t_lm2<-lm(data=df_final_plot_f %>%filter(frost_event=="nofrost"),
         LUE ~ PRI_filter_norm)
t_lm2_frost<-lm(data=df_final_plot_f %>%filter(frost_event=="frost"),
          LUE ~ PRI_filter_norm)
stat_t_lm2<-summary(t_lm2)
summary(t_lm2_frost)
##
plot_grid(p_lm_cold,p_lm_frost)

##########
##C. Check the GPP estimation by using PRI estimation
#########
#
df_final_add_model<-df_final_plot_f %>%
  mutate(LUE_est_with_cold_PRI=PRI_filter_norm*coef(t_lm1)[2]+coef(t_lm1)[1])%>%
  mutate(LUE_est_with_cold_PRI=ifelse(cold_event=="notcold",
                                      LUE_est_with_cold_PRI,0))%>%
  mutate(LUE_est_with_frost_PRI=PRI_filter_norm*coef(t_lm2)[2]+coef(t_lm2)[1])%>%
  mutate(LUE_est_with_frost_PRI=ifelse(frost_event=="nofrost",
                                      LUE_est_with_frost_PRI,0))%>%
  mutate(GPP_est_with_cold_PRI=LUE_est_with_cold_PRI*PAR_absorbed,
         GPP_est_with_frost_PRI=LUE_est_with_frost_PRI*PAR_absorbed
         )

##making plots:
df_final_add_model %>%
  ggplot()+
  geom_point(aes(x=Date,y=GPP))+
  geom_line(aes(x=Date,y=gpp_mod,col="sim GPP"))+
  geom_point(aes(x=Date,y=GPP_est_with_cold_PRI,col="LUE_est_by_LUE_cold"))+
  geom_point(aes(x=Date,y=GPP_est_with_frost_PRI,col="LUE_est_by_LUE_frost"))

##only targetting GPP before end of the summer
df_final_1<-df_final_add_model %>%
  dplyr::select(Date,GPP,gpp_mod,GPP_est_with_frost_PRI)%>%
  pivot_longer(c(gpp_mod,GPP_est_with_frost_PRI),names_to = "Model_GPP",
               values_to = "Model_value")%>%
  mutate(DOY=yday(Date))%>%
  filter(DOY<=181)
df_final_2<-df_final_add_model %>%
  dplyr::select(Date,GPP,gpp_mod,GPP_est_with_cold_PRI)%>%
  pivot_longer(c(gpp_mod,GPP_est_with_cold_PRI),names_to = "Model_GPP",
               values_to = "Model_value")%>%
  mutate(DOY=yday(Date))%>%
  filter(DOY<=181)

p_obs_mod_comp_frost<-df_final_1 %>%
  ggplot(aes(x =Model_value, y= GPP,col=Model_GPP,fill = Model_GPP))+
  geom_point()+
  #adding relationship between GPP and PRI:
  geom_smooth(data=df_final_1,
              aes(x = Model_value, y= GPP,fill = Model_GPP),
              method = "lm",
              se=TRUE,
              # fill=adjustcolor("black",0.4),color="black"
              )+
  stat_poly_eq(data=df_final_1%>%filter(Model_GPP=="GPP_est_with_frost_PRI"),
               geom = "text",position = "identity",
               col="black",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = 7, label.y = -1)+
  annotate(geom = "text",x=13.5,y=-1,
           label = paste0("p < 0.001"),col="black",size=6)+
  stat_poly_eq(data=df_final_1%>%filter(Model_GPP=="gpp_mod"),
               geom = "text",position = "identity",
               col="steelblue2",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = 7, label.y = -2)+
  annotate(geom = "text",x=13.5,y=-2,
           label = paste0("p < 0.001"),col="steelblue2",size=6)+
  geom_abline(intercept = 0,slope=1,lty=2)+
  scale_color_manual(values = c("GPP_est_with_frost_PRI"="black","gpp_mod"="steelblue2"),
                     labels=c(expression("GPP"[Model-use-PRI(frost)]),expression("GPP"[Model-original])))+
  scale_fill_manual(values = c("GPP_est_with_frost_PRI"="black","gpp_mod"="steelblue2"),
                    labels=c(expression("GPP"[Model-use-PRI(frost)]),expression("GPP"[Model-original])))+
  ylab(expression( paste("GPP"[EC]*" (g C m"^-2, " d"^-1, ")" ) ))+
  xlab(expression( paste("GPP"[Modelled]*" (g C m"^-2, " d"^-1, ")" )))+
  # xlab(expression( paste("GPP"[Model-use-PRI]*" (g C m"^-2, " d"^-1, ")" )))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = c(0.2,0.9),
    axis.text = element_text(size=16),
    axis.title = element_text(size=20))

p_obs_mod_comp_cold<-df_final_2 %>%
  ggplot(aes(x =Model_value, y= GPP,col=Model_GPP,fill = Model_GPP))+
  geom_point()+
  #adding relationship between GPP and PRI:
  geom_smooth(data=df_final_2,
              aes(x = Model_value, y= GPP,fill = Model_GPP),
              method = "lm",
              se=TRUE,
              # fill=adjustcolor("black",0.4),color="black"
              )+
  stat_poly_eq(data=df_final_2%>%filter(Model_GPP=="GPP_est_with_cold_PRI"),
               geom = "text",position = "identity",
               col="black",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = 7, label.y = -1)+
  annotate(geom = "text",x=13.5,y=-1,
           label = paste0("p < 0.001"),col="black",size=6)+
  stat_poly_eq(data=df_final_2%>%filter(Model_GPP=="gpp_mod"),
               geom = "text",position = "identity",
               col="steelblue2",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = 7, label.y = -2)+
  annotate(geom = "text",x=13.5,y=-2,
           label = paste0("p < 0.001"),col="steelblue2",size=6)+
  geom_abline(intercept = 0,slope=1,lty=2)+
  scale_color_manual(values = c("GPP_est_with_cold_PRI"="black","gpp_mod"="steelblue2"),
                     labels=c(expression("GPP"[Model-use-PRI]),expression("GPP"[Model-original])))+
  scale_fill_manual(values = c("GPP_est_with_cold_PRI"="black","gpp_mod"="steelblue2"),
                    labels=c(expression("GPP"[Model-use-PRI]),expression("GPP"[Model-original]))
  )+
  ylab(expression( paste("GPP"[EC]*" (g C m"^-2, " d"^-1, ")" ) ))+
  xlab(expression( paste("GPP"[Modelled]*" (g C m"^-2, " d"^-1, ")" )))+
  # xlab(expression( paste("GPP"[Model-use-PRI]*" (g C m"^-2, " d"^-1, ")" )))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = c(0.15,0.9),
    axis.text = element_text(size=16),
    axis.title = element_text(size=20))

##
plot_grid(p_obs_mod_comp_cold,p_obs_mod_comp_frost)
##
t_lm_obs_mod_frost<-lm(data=df_final_add_model%>%mutate(DOY=yday(Date))%>%
                         # filter(DOY<=243),
                         filter(DOY<=181),
   GPP ~ GPP_est_with_frost_PRI)
t_lm_obs_mod_cold<-lm(data=df_final_add_model%>%mutate(DOY=yday(Date))%>%
                        # filter(DOY<=243
                        filter(DOY<=181),
                 GPP ~ GPP_est_with_cold_PRI)
# tt<-lm(data=df_final_add_model%>%mutate(DOY=yday(Date))%>%
#      filter(DOY<=181),
#    GPP ~ gpp_mod)
# summary(tt)
summary(t_lm_obs_mod_frost)
summary(t_lm_obs_mod_cold)
##test if PRI_filter_norm better correlate with GPP or PRI_filter_norm
test_df<-df_final_add_model%>%mutate(DOY=yday(Date))%>%
  filter(DOY<=181)
p_test1<-test_df%>%
  ggplot(aes(x=PRI_filter_norm,y=LUE))+
  geom_point()+
  geom_smooth(data=test_df,
              aes(x = PRI_filter_norm, y= LUE),
              method = "lm",
              se=TRUE,
              # fill=adjustcolor("black",0.4),color="black"
  )+
  stat_poly_eq(data=test_df,
               geom = "text",position = "identity",
               col="black",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = 0.2, label.y = 0.035)+
  theme_classic()
p_test2<-test_df%>%
  ggplot(aes(x=PRI_filter_norm,y=GPP))+
  geom_point()+
  geom_smooth(data=test_df,
              aes(x = PRI_filter_norm, y= GPP),
              method = "lm",
              se=TRUE,
              # fill=adjustcolor("black",0.4),color="black"
  )+
  stat_poly_eq(data=test_df,
               geom = "text",position = "identity",
               col="black",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = 0, label.y = 9)+
  theme_classic()
plot_grid(p_test1,p_test2)

######save the data:
save.path<-"./data/Comprehensive_plot_data/Fig6/"
save(df_final_add_model,
     file=paste0(save.path,"df_final_add_GPP_est_usePRI.RDA"))


################
#(4) Merge the plots
################
p_GPP_vs_GPP_coldPRI<-plot_grid(p_LUE_Tmin_underCold,p_lm_cold,
                                p_PRI_Tmin_underCold,
                                p_obs_mod_comp_cold,
                   align="hv",
                   ncol=2,nrow=2,
                   label_size = 20,
                   labels = c("(a)","(b)","(c)","(d)"))
p_GPP_vs_GPP_frostPRI<-plot_grid(p_LUE_Tmin_underFrost,p_lm_frost,
                                p_PRI_Tmin_underFrost,
                                p_obs_mod_comp_frost,
                                align="hv",
                                ncol=2,nrow=2,
                                label_size = 20,
                                labels = c("(a)","(b)","(c)","(d)"))
##save the plots:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig6.png"),
       p_GPP_vs_GPP_coldPRI,width = 15,height=10)
ggsave(paste0(save.path,"Fig6_supp1_analysis_base_on_frost.png"),
       p_GPP_vs_GPP_frostPRI,width = 15,height=10)
##adding the plot data:
data.path<-"./data/Comprehensive_plot_data/Fig6/"
load(file=paste0(data.path,"model_performance.RDA"))
p_merge_new<-plot_grid(p_GPP_vs_GPP_coldPRI,
                    p_with_coldPRI_season_plot_f,
                    align="hv",
                    ncol=1,
                    rel_heights = c(0.6,0.4)
)
ggsave(paste0(save.path,"Fig6_new.png"),
       p_merge_new,width = 18,height=15)




