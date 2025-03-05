########################################################
#Aim: to compare the variation of GPP with variation of (SoilT and Ta)--variation along DoY
########################################################
#
library(tidyverse)
library(dplyr)
library(lubridate)
library(phenopix)
library(cowplot)
#----------------------
#(1)load the data
#----------------------
#---------
#load the GPP data tidy before-->adopt from Fig1_01_check_GPP_estimation
#---------
load("./test/test_datasets/df.merge.RDA")
###doy of mean and sd
df_meandoy <- df.merge %>%
  mutate(doy=yday(date),gpp_res=gpp_mod-gpp_obs)%>%
  group_by(sitename, doy) %>%
  dplyr::summarise(across(starts_with("gpp_"), mean, na.rm = TRUE))
df_sddoy<-df.merge%>%
  mutate(doy=yday(date),gpp_res=gpp_mod-gpp_obs)%>%
  group_by(sitename, doy) %>%
  summarise(gpp_obs_sd=sd(gpp_obs),
            gpp_mod_sd=sd(gpp_mod),
            gpp_res_sd=sd(gpp_res))
df_GPP<-left_join(df_meandoy,df_sddoy)%>%
  filter(sitename=="CH-Dav" | sitename=="DE-Tha")

#---------
#load the Meteo and soil Temperature data:
#---------
df_Meteo_andSoilVars<-readRDS(paste0("./data/Comprehensive_plot_data/Fig1/","df.Meteo_and_SoilVars.RDS"))
#select the air temperature and soil temperature:
df_sel<-df_Meteo_andSoilVars %>%
  dplyr::select(sitename,Date,GPP,PPFD_IN,TA,TS_1,
                SWC_shallow,SWC_medium)%>%
  mutate(TS=TS_1,TS_1=NULL,
         doy=yday(Date)
         )
##obtain doy of mean and sd
df_sel_meandoy <- df_sel %>%
    group_by(sitename,doy) %>%
    dplyr::summarise(gpp_tidy_EC_mean=mean(GPP,na.rm = T),
                     PPFD_mean=mean(PPFD_IN,na.rm = T),
                     Ta_mean=mean(TA,na.rm = T),
                     Ts_mean=mean(TS,na.rm = T),
                     SWC_shallow=mean(SWC_shallow,na.rm = T),
                     SWC_medium=mean(SWC_medium,na.rm = T)
                     )
##
df_sel_tidy<-left_join(df_GPP%>%select(sitename,doy,gpp_obs),
                       df_sel_meandoy)
#----------------------
#(2)plotting
#----------------------
df_plot<-df_sel_tidy %>%
  pivot_longer(gpp_obs:SWC_medium,names_to = "vars",values_to = "y")
##
library(ggplot2)
##
plot_theme<-theme_light()+
theme(
  legend.position = "none",
  legend.title = element_blank(),
  legend.background = element_blank(),
  legend.text = element_text(size=16),
  axis.text = element_text(size=16),
  axis.title = element_text(size=20))
#
p_CH_Dav_GPP_Ta<-df_plot %>%
  filter(vars!="gpp_tidy_EC_mean" & vars=="gpp_obs")%>%
  filter(sitename=="CH-Dav")%>%
  ggplot(aes(x=doy,y=y,col=vars))+
  geom_point()+
  annotate(geom = "text",x=50,y=10,label="CH-Dav",size=6)+
  #
  geom_point(aes(x=doy,y=(y+3)/18*11),
           data = df_plot %>% filter(sitename=="CH-Dav")%>%
             filter(vars=="Ta_mean"))+
  ylab(expression("GPP (g C m"^-2* " d"^-1* ")" ))+
  xlab("")+
  #add second axix
  scale_y_continuous(
    # limits = c(-0.05,1.2),
    sec.axis = sec_axis(~ . *18/11-3, name = expression("Ta ("* "°C)"))  # 反向缩放
  )+
  scale_color_manual("",
    values = c("gpp_obs"="black","Ta_mean"="red"),
                     labels= c("gpp_obs"="GPP","Ta_mean"="Ta")
                     )+
  plot_theme+
  theme(legend.position = c(0.5,0.3))

p_DE_Tha_GPP_Ta<-df_plot %>%
  filter(vars!="gpp_tidy_EC_mean" & vars=="gpp_obs")%>%
  filter(sitename=="DE-Tha")%>%
  ggplot(aes(x=doy,y=y,col=vars))+
  geom_point()+
  annotate(geom = "text",x=50,y=10,label="DE-Tha",size=6)+
  #
  geom_point(aes(x=doy,y=y/2),
             data = df_plot %>% filter(sitename=="DE-Tha")%>%
               filter(vars=="Ta_mean"))+
  ylab(expression("GPP (g C m"^-2* " d"^-1* ")" ))+
  xlab("")+
  #add second axix
  scale_y_continuous(
    # limits = c(-0.05,1.2),
    sec.axis = sec_axis(~ . *2, name = expression("Ta ("* "°C)"))  # 反向缩放
  )+
  scale_color_manual("",
                     values = c("gpp_obs"="black","Ta_mean"="red"),
                     labels= c("gpp_obs"="GPP","Ta_mean"="Ta"))+
  plot_theme

#
p_CH_Dav_GPP_Ts<-df_plot %>%
  filter(vars!="gpp_tidy_EC_mean" & vars=="gpp_obs")%>%
  filter(sitename=="CH-Dav")%>%
  ggplot(aes(x=doy,y=y,col=vars))+
  geom_point()+
  annotate(geom = "text",x=50,y=10,label="CH-Dav",size=6)+
  #
  geom_point(aes(x=doy,y=y/12*10),
             data = df_plot %>% filter(sitename=="CH-Dav")%>%
               filter(vars=="Ts_mean"))+
  ylab(expression("GPP (g C m"^-2* " d"^-1* ")" ))+
  xlab("DOY")+
  #add second axix
  scale_y_continuous(
    # limits = c(-0.05,1.2),
    sec.axis = sec_axis(~ . *12/10, name = expression("Ts ("* "°C)"))  # 反向缩放
  )+
  scale_color_manual("",
                     values = c("gpp_obs"="black","Ts_mean"="red"),
                     labels= c("gpp_obs"="GPP","Ts_mean"="Ts"))+
  plot_theme+
  theme(legend.position = c(0.5,0.3))

p_DE_Tha_GPP_Ts<-df_plot %>%
  filter(vars!="gpp_tidy_EC_mean" & vars=="gpp_obs")%>%
  filter(sitename=="DE-Tha")%>%
  ggplot(aes(x=doy,y=y,col=vars))+
  geom_point()+
  annotate(geom = "text",x=50,y=10,label="DE-Tha",size=6)+
  #
  geom_point(aes(x=doy,y=y/1.8),
             data = df_plot %>% filter(sitename=="DE-Tha")%>%
               filter(vars=="Ts_mean"))+
  ylab(expression("GPP (g C m"^-2* " d"^-1* ")" ))+
  xlab("DOY")+
  #add second axix
  scale_y_continuous(
    # limits = c(-0.05,1.2),
    sec.axis = sec_axis(~ . *12/10, name = expression("Ts ("* "°C)"))  # 反向缩放
  )+
  scale_color_manual("",
                     values = c("gpp_obs"="black","Ts_mean"="red"),
                     labels= c("gpp_obs"="GPP","Ts_mean"="Ts"))+
  plot_theme

#
p_CH_Dav_GPP_PPFD<-df_plot %>%
  filter(vars!="gpp_tidy_EC_mean" & vars=="gpp_obs")%>%
  filter(sitename=="CH-Dav")%>%
  ggplot(aes(x=doy,y=y,col=vars))+
  annotate(geom = "text",x=50,y=10,label="CH-Dav",size=6)+
  geom_point()+
  #
  geom_point(aes(x=doy,y=c(y-100)/50),
             data = df_plot %>% filter(sitename=="CH-Dav")%>%
               filter(vars=="PPFD_mean"))+
  ylab(expression("GPP (g C m"^-2* " d"^-1* ")" ))+
  xlab("")+
  #add second axix
  scale_y_continuous(
    # limits = c(-0.05,1.2),
    sec.axis = sec_axis(~ . *50+100 , name = expression("PAR ("*mu*"mol m"^-2*s^-1*")"))  # 反向缩放
  )+
  scale_color_manual("",
                     values = c("gpp_obs"="black","PPFD_mean"="red"),
                     labels= c("gpp_obs"="GPP","PPFD_mean"="PPFD"))+
  plot_theme+
  theme(legend.position = c(0.5,0.3))

p_DE_Tha_GPP_PPFD<-df_plot %>%
  filter(vars!="gpp_tidy_EC_mean" & vars=="gpp_obs")%>%
  filter(sitename=="DE-Tha")%>%
  ggplot(aes(x=doy,y=y,col=vars))+
  geom_point()+
  annotate(geom = "text",x=50,y=10,label="DE-Tha",size=6)+
  #
  geom_point(aes(x=doy,y=y/61),
             data = df_plot %>% filter(sitename=="DE-Tha")%>%
               filter(vars=="PPFD_mean"))+
  ylab(expression("GPP (g C m"^-2* " d"^-1* ")" ))+
  xlab("")+
  #add second axix
  scale_y_continuous(
    # limits = c(-0.05,1.2),
    sec.axis = sec_axis(~ . *61 , name = expression("PAR ("*mu*"mol m"^-2*s^-1*")"))  # 反向缩放
  )+
  scale_color_manual("",
                     values = c("gpp_obs"="black","PPFD_mean"="red"),
                     labels= c("gpp_obs"="GPP","PPFD_mean"="PPFD"))+
  plot_theme

#for SWC:
p_CH_Dav_GPP_SWC<-df_plot %>%
  filter(vars!="gpp_tidy_EC_mean" & vars=="gpp_obs")%>%
  filter(sitename=="CH-Dav")%>%
  ggplot(aes(x=doy,y=y,col=vars))+
  annotate(geom = "text",x=50,y=10,label="CH-Dav",size=6)+
  geom_point()+
  #
  geom_point(aes(x=doy,y=c(y-20)/2),
             data = df_plot %>% filter(sitename=="CH-Dav")%>%
               filter(vars=="SWC_medium"))+
  ylab(expression("GPP (g C m"^-2* " d"^-1* ")" ))+
  xlab("")+
  #add second axix
  scale_y_continuous(
    # limits = c(-0.05,1.2),
    sec.axis = sec_axis(~ .*2+20, name = expression("SWC"[medium]*" (%)"))  # 反向缩放
  )+
  scale_color_manual("",
                     values = c("gpp_obs"="black","SWC_medium"="red"),
                     labels= c("gpp_obs"="GPP","SWC_medium"="SWC_medium"))+
  plot_theme+
  theme(legend.position = c(0.5,0.4))

p_DE_Tha_GPP_SWC<-df_plot %>%
  filter(vars!="gpp_tidy_EC_mean" & vars=="gpp_obs")%>%
  filter(sitename=="DE-Tha")%>%
  ggplot(aes(x=doy,y=y,col=vars))+
  annotate(geom = "text",x=50,y=10,label="DE-Tha",size=6)+
  geom_point()+
  #
  geom_point(aes(x=doy,y=c(y-5)*1.2),
             data = df_plot %>% filter(sitename=="DE-Tha")%>%
               filter(vars=="SWC_medium"))+
  ylab(expression("GPP (g C m"^-2* " d"^-1* ")" ))+
  xlab("")+
  #add second axix
  scale_y_continuous(
    # limits = c(-0.05,1.2),
    sec.axis = sec_axis(~ ./2+5, name = expression("SWC"[medium]*" (%)"))  # 反向缩放
  )+
  scale_color_manual("",
                     values = c("gpp_obs"="black","SWC_medium"="red"),
                     labels= c("gpp_obs"="GPP","SWC_medium"="SWC_medium"))+
  plot_theme


###merge the plot:
plot_merge<-plot_grid(p_CH_Dav_GPP_Ta,p_DE_Tha_GPP_Ta,
          p_CH_Dav_GPP_PPFD,p_DE_Tha_GPP_PPFD,
          p_CH_Dav_GPP_Ts,p_DE_Tha_GPP_Ts,
          p_CH_Dav_GPP_SWC,p_DE_Tha_GPP_SWC,
          labels = c("(a)","(b)","(c)","(d)",
                     "(e)","(f)","(g)","(h)"),
          align = "hv",
          ncol=2,nrow=4
          )
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig5_supp1_Ta_PPFD_Ts_SWC_vsGPP.png"),plot_merge,
       width = 20,height=16)

