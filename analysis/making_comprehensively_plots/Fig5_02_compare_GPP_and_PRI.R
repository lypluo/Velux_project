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
##load the VI and SIF from the CH-Dav:May, 2021 - Dec,2023
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
  "PRI_filter_update"=as.numeric(tt))
df.final<-left_join(df.all,df.ts_PRI_new)
#test
# par(mfrow=c(2,1))
# plot(df.final$Date,df.final$GPP)
# plot(df.final$Date,df.final$PRI)
# points(df.final$Date,df.final$PRI_filter_update,col="red")

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
         PRI_filter_norm=norm_01(PRI_filter_update))

#plot1:
#with two y axis--y1:GPP, y2:PRI
p_GPP_PRI <- ggplot(df_final_plot, aes(x = Date)) +
  geom_point(aes(y = GPP), color = "blue") +  # 第一个 Y 轴的线
  ylim(-4.5,15)+
  geom_point(aes(y = PRI_norm * 8), color = adjustcolor("red",0.2)) +  # 第二个 Y 轴的线，缩放
  geom_line(aes(y = PRI_filter_norm * 8), color = "red") +  # 第二个 Y 轴的线，缩放
  scale_y_continuous(
    name = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
    sec.axis = sec_axis(~ . /8, name = "Norm PRI")  # 反向缩放
  ) +
  # labs(title = "Dual Y-Axis Plot", x = "X-axis") +
  theme_light()+
  ##adding the CH-Dav
  annotate(geom = "text",x=as.Date("2021-02-25"),
           y=15,label = "CH-Dav",size=8)+
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

#plot2:
#indicate the data in the green-up period:
#according to previous study, the sos10(sos) of Davos is 61
#peak = 180
df_final_plot<-df_final_plot %>%
  mutate(doy=yday(Date))%>%
  mutate(greenup=ifelse(doy>=61 & doy<=180,"yes","no"))

#
library(ggpmisc) #adding R2..
lm_greenup<-lm(data=df_final_plot %>%filter(greenup=="yes"),
               GPP ~ PRI_filter_update)
summary(lm_greenup)
lm_all<-lm(data=df_final_plot,
           GPP ~ PRI_filter_update)
summary(lm_all)

#
df_final_plot<-df_final_plot %>%
  mutate(greenup=ifelse(greenup=="yes","Greenup","Greendown"))%>%
  mutate(greenup=factor(greenup,levels=c("Greenup","Greendown")))
p_corr<-df_final_plot %>%
  ggplot(aes(x=PRI_filter_update,y=GPP,col=greenup))+
  geom_point()+
  scale_color_manual(values = c("Greenup"="forestgreen","Greendown"="grey"))+
  geom_smooth(data=df_final_plot%>%filter(greenup=="Greenup"),
              method = "lm",
              se=TRUE,
              color="forestgreen")+
  stat_poly_eq(data=df_final_plot%>%filter(greenup=="Greenup"),
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
  #for all the data:
  geom_smooth(data=df_final_plot%>%filter(greenup=="Greendown"),
              method = "lm",se=TRUE,color="black")+
  stat_poly_eq(data=df_final_plot%>%filter(greenup=="Greendown"),
               geom = "text",position = "identity",
               col="black",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = -0.06, label.y = -2.5)+
  annotate(geom = "text",x=-0.01,y=-2.5,
             label = paste0("p < 0.001"),col="black",size=6)+
  ylab(expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ))+
  xlab("Filtered PRI")+
  ##adding the CH-Dav
  annotate(geom = "text",x=-0.135,
           y=15,label = "CH-Dav",size=8)+
  #change the names for the legend
  guides(color = guide_legend(title = " "))+
  theme_light()+
  theme(
    legend.text = element_text(size=20),
    axis.title = element_text(size=24),
    axis.text = element_text(size = 20),
    text = element_text(size=24),
    legend.position = c(0.15,0.85),
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
save(p_GPP_PRI,
     file=paste0(save.path,"p_GPP_PRI.RDA"))
# save(p_merge_GPP_with_PRI,
#      file=paste0(save.path,"p_merge_GPP_with_PRI.RDA"))

