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

#merge two datasets:
df.ts_GPP_20212023<-df.ts_GPP %>%
  filter(Date>=as.Date("2021-01-01")& Date<=as.Date("2023-12-31"))
df.all<-left_join(df.ts_GPP_20212023,df.ts_VIs)

##---------------------------
##load the snow data
##---------------------------
load.path<-"D:/data/Velux_shared_data/CH-Dav/Snow_depth_fromICOS/"
df.Meteo.Dav<-read.csv(file=paste0(load.path,"ICOSETC_CH-Dav_METEO_INTERIM_L2.csv"),header = T)
df.Meteo.Dav_sel<-df.Meteo.Dav %>%
  dplyr::select(TIMESTAMP_START,TA,D_SNOW)
names(df.Meteo.Dav_sel)<-c("rDate","Ta","D_SNOW")
#
df.Snow_Daily<-df.Meteo.Dav_sel %>%
  mutate(rDate=ymd_hm(as.character(rDate)))%>%
  mutate(Date=as.Date(rDate))%>%
  mutate(D_SNOW=ifelse(D_SNOW==-9999,NA,D_SNOW),
         Ta=ifelse(Ta==-9999,NA,Ta)
         )%>%
  group_by(Date)%>%
  summarise(D_SNOW=mean(D_SNOW,na.rm = T),
            Ta_mean=mean(Ta,na.rm = T),
            Ta_min=min(Ta,na.rm = T))%>%
  #remove the Inf
  mutate(D_SNOW=ifelse(is.infinite(D_SNOW),NA,D_SNOW),
    Ta_mean=ifelse(is.infinite(Ta_mean),NA,Ta_mean),
    Ta_min=ifelse(is.infinite(Ta_min),NA,Ta_min))%>%
  #set the negative value as NA
  mutate(D_SNOW=ifelse(D_SNOW<0,NA,D_SNOW))

###merge the data
df.all<-left_join(df.all,df.Snow_Daily)

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
  theme_light()+
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
  geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
          mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_rect(data=rect.coord_isevent_2022,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2022,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_rect(data=rect.coord_isevent_2023,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2023,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
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
  theme_light()+
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
  geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_rect(data=rect.coord_isevent_2022,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2022,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_rect(data=rect.coord_isevent_2023,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2023,inherit.aes = FALSE,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  ##adding the extreme cold event:
  geom_vline(xintercept = as.Date("2023-03-27"),col="purple")+
  annotate(geom="text",x=as.Date("2023-03-27"),y=-10,
           label="Mar,27",col="purple",size=6)



#merge plot1 &2
p_GPP_PRI_merge<-plot_grid(p_GPP_PRI,p_Ta_Snow,align="v",
                             ncol=1,nrow=2,
          rel_heights = c(0.6,0.4)
          )

#plot3:
#indicate the data when snow happens in growing season period:
#according to previous study, the sos10(sos) of Davos is 61
#peak = 180
#eos= 322/296
df_final_plot_f<-df_final_plot %>%
  mutate(doy=yday(Date))%>%
  mutate(greenup=ifelse(doy>=61 & doy<=180,"GP","NoGP"))%>%
  mutate(GS=ifelse(doy>=61 & doy <=296,"GS","NoGS"))%>%
  mutate(snow_event=ifelse(!is.na(D_SNOW) & D_SNOW>0,"Snow","NoSnow"))%>%
  #add the cold event:
  mutate(cold_event=ifelse(!is.na(Ta_min) & Ta_min< -5,"cold","notcold"))

#
library(ggpmisc) #adding R2..
#All data
lm_all<-lm(data=df_final_plot_f,
           GPP ~ PRI.max.filtered)
stat_all<-summary(lm_all)

#All data no snow
lm_nosnow<-lm(data=df_final_plot_f %>% filter(snow_event=="NoSnow"),
                 GPP ~ PRI.max.filtered)
stat_nosnow<-summary(lm_nosnow)

#All data notcold
lm_notcold<-lm(data=df_final_plot_f %>% filter(cold_event=="notcold"),
              GPP ~ PRI.max.filtered)
stat_notcold<-summary(lm_notcold)

#growing season :
lm_GS<-lm(data=df_final_plot_f %>% filter(GS=="GS"),
                 GPP ~ PRI.max.filtered)
stat_GS<-summary(lm_GS)
#growing season without snow:
lm_GS_nosnow<-lm(data=df_final_plot_f %>% filter(snow_event=="NoSnow" & GS=="GS"),
               GPP ~ PRI.max.filtered)
stat_GS_nosnow<-summary(lm_GS_nosnow)
#growing season not cold:
lm_GS_notcold<-lm(data=df_final_plot_f %>% filter(cold_event=="notcold" & GS=="GS"),
                 GPP ~ PRI.max.filtered)
stat_GS_notcold<-summary(lm_GS_notcold)

#green-up:
lm_GP<-lm(data=df_final_plot_f %>%filter(greenup=="GP"),
                 GPP ~ PRI.max.filtered)
stat_GP<-summary(lm_GP)
#green-up without snow:
lm_GP_nosnow<-lm(data=df_final_plot_f %>%filter(snow_event=="NoSnow" & greenup=="GP"),
                 GPP ~ PRI.max.filtered)
stat_GP_nosnow<-summary(lm_GP_nosnow)
#green-up but not cold
lm_GP_notcold<-lm(data=df_final_plot_f %>%filter(cold_event=="notcold" & greenup=="GP"),
                 GPP ~ PRI.max.filtered)
stat_GP_notcold<-summary(lm_GP_notcold)
#green-up but not cold and without snow
# lm_GP_nosnow_notcold<-lm(data=df_final_plot_f %>%filter(snow_event=="NoSnow" & cold_event=="notcold" & greenup=="GP"),
#                   GPP ~ PRI.max.filtered)
# stat_GP_nosnow_notcold<-summary(lm_GP_nosnow_notcold)

##
extract_paras<-function(df){
  # df<-stat_all
  
  para<-as.numeric(coef(df)[,1])
  r2<-df$r.squared
  #
  para_sum<-as.data.frame(t(c(para,r2)))
  names(para_sum)<-c("intercept","slope","R2")
  return(para_sum)
}
###
t_GS<-extract_paras(stat_GS)
t_GS_nosnow<-extract_paras(stat_GS_nosnow)
t_GS_notcold<-extract_paras(stat_GS_notcold)
t_GP<-extract_paras(stat_GP)
t_GP_nosnow<-extract_paras(stat_GP_nosnow)
t_GP_notcold<-extract_paras(stat_GP_notcold)
t_all<-extract_paras(stat_all)
t_all_nosnow<-extract_paras(stat_nosnow)
t_all_notcold<-extract_paras(stat_notcold)

#
stat_sum<-rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(t_GS,t_GS_nosnow),
            t_GS_notcold),t_GP),
            t_GP_nosnow),t_GP_notcold),t_all),t_all_nosnow),t_all_notcold)
rownames(stat_sum)<-c("GS","GS_nosnow","GS_notcold",
  "Greenup","Greenup_nosnow","Greenup_notcold","All","All_nosnow","All_notcold")
#
p_corr<-df_final_plot_f %>%
  ggplot(aes(x=PRI.max.filtered,y=GPP,col=greenup))+
  geom_point()+
  scale_color_manual(values = c("GP"="forestgreen","NoGP"="grey"),
                     labels=c("GP"="Greenup","NoGP"="Non-greenup"))+
  geom_smooth(data=df_final_plot_f%>%filter(greenup=="GP"),
              method = "lm",
              se=TRUE,
              color="forestgreen")+
  stat_poly_eq(data=df_final_plot_f%>%filter(greenup=="GP"),
               geom = "text",position = "identity",
               size=6,
               col="forestgreen",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = -0.06, label.y = -1.5)+  # 显示回归方程和R²
  #not clearly to show the p-value
  # stat_poly_eq(data=df_final_plot %>%filter(greenup=="yes"),
  #              geom = "text",position = "identity",
  #              col="black",
  #              aes(label = sprintf("p = %.2f",..p.value..)),
  #              formula = y ~ x, parse = TRUE,
  #              label.x = 0, label.y = -1.5)+
  annotate(geom = "text",x=-0.01,y=-1.5,
           label = paste0("p < 0.001"),col="forestgreen",size=6)+
  #add description label:
  annotate(geom = "text",x=-0.1,y=-1.5,
           label = paste0("Greenup:"),col="forestgreen",size=6)+
  #for all the data:
  geom_smooth(data=df_final_plot_f,
              method = "lm",se=TRUE,color="black")+
  stat_poly_eq(data=df_final_plot_f,
               geom = "text",position = "identity",
               col="black",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = -0.06, label.y = -2.5)+
  annotate(geom = "text",x=-0.01,y=-2.5,
             label = paste0("p < 0.001"),col="black",size=6)+
  #add description label:
  annotate(geom = "text",x=-0.1,y=-2.5,
           label = paste0("All:"),col="black",size=6)+
  ylab(expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ))+
  xlab("Filtered PRI")+
  ##adding the CH-Dav
  annotate(geom = "text",x=-0.125,
           y=15,label = "CH-Dav",size=8)+
  #change the names for the legend
  guides(color = guide_legend(title = " "))+
  theme_light()+
  theme(
    legend.text = element_text(size=20),
    axis.title = element_text(size=24),
    axis.text = element_text(size = 20),
    text = element_text(size=24),
    legend.position = c(0.2,0.85),
    legend.background = element_blank()
  )+
  ylim(-4.5,15)

#merge the plots:
# library(cowplot)
# p_merge_GPP_with_PRI<-plot_grid(p_GPP_PRI,p_corr,align="hv",
#                    ncol=2,nrow=1,labels = c("(b)","(c)"))
save.path<-"./data/Comprehensive_plot_data/Fig5/"
save(p_corr,
     file=paste0(save.path,"p_corr.RDA"))
save(p_GPP_PRI_merge,
     file=paste0(save.path,"p_GPP_PRI.RDA"))
# save(p_merge_GPP_with_PRI,
#      file=paste0(save.path,"p_merge_GPP_with_PRI.RDA"))

