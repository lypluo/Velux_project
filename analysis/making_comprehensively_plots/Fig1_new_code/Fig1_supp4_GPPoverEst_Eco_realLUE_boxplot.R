########################################################
#Aim: to merge plots related to GPP estimation, LUE, and leaf physiology
########################################################
#update in April,2025:mainly focused on LUE
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
df_GPP<-left_join(df_meandoy,df_sddoy)

#---------
#load the LUE tidy before-->adopt from Fig2_supp1_04_Eco_LUE
#---------
df_LUE<-readRDS(paste0("./data/Comprehensive_plot_data/Fig2/","LUE_2sites.RDS"))

#-------------------------
#(2)primary plotting: adding lines and rectanges in plot
#-------------------------
#-----------
#ploting making--1. making the delta GPP(GPP_mod-GPP_obs) for CH-Dav and DE-Tha
#-----------
##1a.
df_plot_2sites<-df_GPP%>%
  filter(sitename=="CH-Dav"|sitename=="DE-Tha")

p_GPP_plot<-df_plot_2sites %>%
  ggplot()+
  geom_line(aes(x=doy,y=gpp_res,col=sitename),size=1.1)+
  xlim(0,250)+
  # ylim(-1,3)+
  labs(y = expression( paste(Delta*"GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "")+
  # geom_ribbon(
  #   aes(x = doy, ymin = gpp_res - gpp_res_sd, 
  #       ymax = gpp_res + gpp_res_sd, fill=sitename),
  #   alpha = 0.2
  # ) +
  scale_color_manual(values = c("CH-Dav"="brown1",
                                "DE-Tha"="orange"))+
  # scale_fill_manual(values = c("CH-Dav"="brown1",
  #                              "DE-Tha"="orange"))+
  geom_hline(yintercept = 0,lty=2,size=1.1)+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = c(0.8,0.88),
    axis.text = element_text(size=16),
    axis.title = element_text(size=20))
##1b.adding overestimated periods of GPP in early spring:
##comparing EC GPP with GPP simulated by P-model
#source the functions:
source("./R/separate_norm_GPPmismatch_period_trs_diff0_3SD.R")

##
df_andPlot_DE_Tha<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="DE-Tha",],
                                                0.05,"Cfb","ENF","DE-Tha",c(1996:2018))
df_andPlot_CH_Dav<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="CH-Dav",],
                                                0.05,"Dfc","ENF","CH-Dav",c(1997:2018))
#tidy the timing of mean of phenology:
Dav_pheno<-round(colMeans(df_andPlot_CH_Dav$pos_agg,na.rm = T),0)
THA_pheno<-round(colMeans(df_andPlot_DE_Tha$pos_agg,na.rm = T),0)

#-----------
#ploting making--2. making the delta LUE for CH-Dav and DE-Tha
#-----------
df_LUE_Dav<-df_LUE%>%
  filter(Sitename=="CH-Dav")%>%
  dplyr::select(Date,LUE)%>%
  mutate(LUE_Dav=LUE,LUE=NULL)
df_LUE_Tha<-df_LUE%>%
  filter(Sitename=="DE-Tha")%>%
  dplyr::select(Date,LUE)%>%
  mutate(LUE_Tha=LUE,LUE=NULL)
df_delta_LUE<-left_join(df_LUE_Dav,df_LUE_Tha)%>%
  mutate(LUE_diff=LUE_Tha - LUE_Dav,
         doy=yday(Date))
#using the df_LUE at the end for better demonstration:
df_LUE<-df_LUE%>%
  mutate(doy=yday(Date))%>%
  group_by(Sitename,doy)%>%
  summarise(LUE_mean=mean(LUE,na.rm = T))
#
library(ggbreak)
p_LUE<-df_LUE%>%
  ggplot(aes(x=doy,y=LUE_mean,col=Sitename))+
  geom_point()+
  xlim(0,250)+
  ylim(0,0.05)+
  # scale_y_break(c(0.04,0.07))+ #对y轴进行截断
  geom_smooth(span=0.3)+
  scale_color_manual(values = c("CH-Dav"=adjustcolor("red",0.5),
                                "DE-Tha"=adjustcolor("orange",0.5)))+
  # addding date: "2023-03-27"
  # geom_vline(xintercept = 86,col="black")+
  # annotate(geom="text",x=110,y=-50,
  #          label="Mar,27",col="black",size=6)+
  ylab(expression("LUE"*" ("*mu*"mol C m"^-2*mu*"mol"^-1*")"))+
  xlab("DOY (Multi-Years)")+
  # annotate(geom = "text",x=20,y=4,label="CH-Tha",size=6)+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = c(0.8,0.88),
        # legend.text = element_text(size = 16),
        # legend.title = element_text(size=18),
        # legend.background = element_blank(),
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20)
  )

#-------------------------
#(3)further plotting: adding lines and rectanges in plot
#-------------------------
#period:[sos-60,sos+60] is set to red
#adding the sos
#for CH-Dav
rect.coord_isevent=data.frame(x1=Dav_pheno[1]-60,
                              x2=Dav_pheno[1],
                              x3=Dav_pheno[1]+60,
                              y1=-Inf, 
                              y2=Inf)
#1. for DSPR period for GPP:
p_plot_deltaGPP_2sites<-p_GPP_plot+
  # geom_rect(data=rect.coord_isevent,
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+15,y=-0.5,label="sos",col="forestgreen",size=6)

#2. for DSPR period for LUE:
p_plot_LUE_2sites<-p_LUE+
  # geom_rect(data=rect.coord_isevent,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_pheno[1]),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_pheno[1])+15,y=0.05,label="sos",col="forestgreen",size=6)

#-------------------------
#(4)boxplot for LUE
#-------------------------
#
sos10<-Dav_pheno[1]
#
df_add_Dav<-df_LUE %>%
  filter(Sitename=="CH-Dav")
df_add_Tha<-df_LUE %>%
  filter(Sitename=='DE-Tha')
df_add_LUE<-left_join(as.data.frame(df_add_Dav%>%dplyr::select(doy,LUE_mean)%>%
                                      mutate(LUE_Dav=LUE_mean,LUE_mean=NULL))[,-1],
                      as.data.frame(df_add_Tha%>%dplyr::select(doy,LUE_mean)%>%
                                      mutate(LUE_Tha=LUE_mean,LUE=NULL))[,-1])%>%
          mutate(LUE_res=LUE_Dav-LUE_Tha)
#
df_add_1<-df_add_LUE%>%
  #only keep the data between [sos10-60,sos10_60]
  filter(doy>=sos10-60 & doy<sos10)%>%
  mutate(period="pre-sos")
df_add_2<-df_add_LUE%>%
  #only keep the data between [sos10-60,sos10_60]
  filter(doy>sos10 & doy<=sos10+60)%>%
  mutate(period="post-sos")
df_add_final<-rbind(df_add_1,df_add_2)%>%
  mutate(period=factor(period,levels=c("pre-sos","post-sos")))%>%
  ##update in Oct,2025--only focus on post-sos
  filter(period=="post-sos")
#
#sig. test for post-sos:
var_LUE<-as.numeric(unlist(as.data.frame(df_add_final)%>%
                             filter(period=="post-sos")%>%
                             dplyr::select(LUE_res)))
ks.test(var_LUE,"pnorm")
wilcox.test(var_LUE)

p_LUE_boxplot<-df_add_final %>%
  ggplot(aes(x=period,y=LUE_res,fill=period))+
  geom_hline(yintercept = 0,lty=2,size=1.1)+
  geom_violin()+
  geom_boxplot(width = 0.1,position = position_dodge(0.9),col="grey",size=1.05) +
  stat_summary(fun = mean, geom = "point", size=3, color = "black")+
  ylab(expression(Delta*"LUE"*" ("*mu*"mol m"^-2*mu*"mol"^-1*")"))+
  xlab("CH-Dav — DE-Tha")+
  scale_fill_manual(values = c("pre-sos"=adjustcolor("blue",0.3),
                               "post-sos"=adjustcolor("blue",0.1)))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(size=16),
        axis.title = element_text(size=20)
  )+
  annotate("text",x=1,y=0.005,label=c("***"),size=10)

#-------------------------
#(4)merge the plots:
#-------------------------
p_merge<-plot_grid(
  p_plot_LUE_2sites,p_LUE_boxplot,
  ncol = 2, nrow=1,
  align = "hv",
  labels = c("(a)","(b)"),
  axis="tblr",
  label_size = 22,
  # rel_heights = c(0.19,0.19,0.19,0.22,0.2)
  theme(plot.margin = margin(0,0,0,0),
        panel.spacing = unit(0,"lines"))
  )


##save the plot:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig1_supp2_real_LUE.png"),p_merge,
       width = 10,height=5,
       # width = 22,height=15
       )