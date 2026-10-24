########################################################
#Aim: to compare LUE between CH-Dav and DE-Tha in early spring
########################################################
#the site we preliminary chose is NL-Loo; FI-Hyy; DE-Tha, and CH-Dav
#apart from CH-Dav, we already know the GPP overstimation status in the other three sites
#hence, here we mainly focus on checking the GPP estimation in CH-Dav
#
library(tidyverse)
library(dplyr)
library(lubridate)
library(phenopix)
library(cowplot)
#----------------------
#(1)load the data
#----------------------
#load the data from the flux_data_kit(tidy by Koen):
#1) for the GPP from EC and environmental drivers:
df.drivers<-readRDS(paste0("D:/Github/flux_data_kit/data/p_model_drivers/","site_based_drivers.rds"))
# select the CH-Dav
df.obs_koen<-df.drivers %>%
  filter(sitename=="FI-Hyy"|sitename=="NL-Loo"|sitename=="DE-Tha"|sitename=="CH-Dav")
df.obs_Datakit<-c()
for (i in 1:length(df.obs_koen$sitename)) {
  df.temp<-as.data.frame(df.obs_koen$forcing[i])
  df.temp<-df.temp %>%
    mutate(sitename=rep(df.obs_koen$sitename[i],nrow(df.temp)))
  df.obs_Datakit<-rbind(df.obs_Datakit,df.temp)
}

# update in the Oct, 2022-->use the downloaded data from the ICOS2018 datasets-->tidy by YP:
load(file = "D:/EE_WSL/Data_for_use/Data_from_ICOS_sites/processed_data_from_ICOS/Daily_data.RDA")
df.obs_YP<-df_all_sel_daily
##compare the df.obs_Datakit and df.obs
t_1<-df.obs_Datakit %>%
  dplyr::select(sitename,date,gpp)%>%
  mutate(gpp_1=gpp,gpp=NULL)
t_2<-df.obs_YP%>%
  dplyr::select(sitename,Date,GPP_NT_mean)%>%
  mutate(date=as.Date(Date),Date=NULL,gpp_2=GPP_NT_mean,GPP_NT_mean=NULL)
t_merge<-left_join(t_2,t_1)
#
plot(t_merge$gpp_1,t_merge$gpp_2) #the two datasets are not comparable
abline(0,1,lty=2,col="blue")

#2) for the simulated GPP from P-model:
df.model<-readRDS(paste0("D:/Github/flux_data_kit/data/p_model_output/","site_based_p-model_output.rds"))
df.mod<-df.model %>%
  filter(sitename=="FI-Hyy"|sitename=="NL-Loo"|sitename=="DE-Tha"|sitename=="CH-Dav")

#3)merge the data in CH-Dav together
# df<-as.data.frame(df.CH_Dav$forcing)
# df<-df %>%
#   mutate(gpp_obs=gpp,gpp=NULL)
#using the gpp.obs from the koen's tidy up
df<-df.obs_Datakit %>%
  dplyr::select(sitename,date:ppfd)%>%
  mutate(gpp_obs=gpp,gpp=NULL
         )
#also tidy the gpp.obs from the Yunpeng's tidy up
df_YP<-df.obs_YP %>%
  dplyr::select(sitename,Date:VPD_day_mean,GPP_NT_mean)%>%
  mutate(date=as.Date(Date),Date=NULL)%>%
  mutate(gpp_obs=GPP_NT_mean,GPP_NT_mean=NULL)
#
df.model<-c()
for (i in 1:length(df.mod$sitename)) {
  df.temp<-as.data.frame(df.mod$data[i])
  df.temp<-df.temp %>%
    mutate(sitename=rep(df.mod$sitename[i],nrow(df.temp)))
  df.model<-rbind(df.model,df.temp)
}
#
df.model<-df.model%>%
  dplyr::select(sitename,date,fapar,gpp)%>%
  mutate(gpp_mod=gpp,gpp=NULL)

##update in Oct,05,2022-->as fapar ranges 1 to 1 from Koen's datasets
##hence now redownload the fapar from MODIS (MCD15A3H, 500m, 4-day) and linearly
##interpolated to the daily values:
fapar.new.path<-"./data-raw/download_from_MODIS/fPAR/"
load(paste0(fapar.new.path,"fPAR_itpl.RDA"))
df.fapar<-df_fPAR%>%
  dplyr::select(sitename,Date,fPAR,fPAR_itpl)%>%
  mutate(date=as.Date(Date),Date=NULL)
df.model.new<-left_join(df.model,df.fapar)
# plot(df.model.new$fPAR_itpl,df.model.new$fapar)
###adding in April, 2025-->interpolate the fPAR_itpl:
library(zoo)
df.model.new<-df.model.new %>%
  mutate(Year=year(date))%>%
  group_by(sitename,Year)%>%
  mutate(fPAR_itpl=as.numeric(na.fill(fPAR_itpl,c("NA","extend","NA"))))
#
df.merge<-left_join(df,df.model.new)
df.merge.test<-left_join(df_YP,df.model.new) ##also save merged data using gpp_obs tidied by YP:
#!!save the data:
# save(df.merge,file = "./test/test_datasets/df.merge.RDA")
# save(df.merge.test,file = "./test/test_datasets/df.merge.test.RDA")
##adding a test plot to check what's the differences between ppfd 
#from Koen's dataset and the datasets tidied by YP:
df.ppfd_1<-df.merge %>%
  dplyr::select(sitename,date,ppfd)
df.ppfd_2<-df.merge.test %>%
  dplyr::select(sitename,date,PPFD_IN_fullday_mean)
df.ppfd_test<-left_join(df.ppfd_1,df.ppfd_2)
##ppfd from Koen's datasets have different unit-->but generally the ppfd is comparable from two datasets
plot(df.ppfd_test$ppfd*1000000,df.ppfd_test$PPFD_IN_fullday_mean) 
abline(0,1,lty=2,col="blue")
###update in April, 2025-->add LUE:
df.merge<-df.merge %>%
  #convert the unit-->GPP: gC m-2 d-1; PAR_absorbed-->convert to u mol m-2 s-1(10e6)
  #convert the GPP unit-->umol C m-2 s-1-->
  #LUE unit: umol C m-2 umol-1
  #did not add fPAR_itpl as much valuess are missing:
  mutate(LUE_proxy=c(gpp_obs/12*10^6/c(24*60*60))/c(fPAR_itpl*ppfd*1000000))%>%
  # mutate(LUE_proxy=c(gpp_obs/12*10^6/c(24*60*60))/c(ppfd*1000000))%>%
  # mutate(LUE=ifelse(LUE_proxy< 0 |LUE_proxy >1,NA,LUE_proxy))%>%
  mutate(LUE=LUE_proxy)%>%
  #select CH-Dav and DE-Tha
  filter(sitename %in% c("DE-Tha","CH-Dav"))
###addding a test plot:
# library(zoo)
df_meandoy <- df.merge %>%
  mutate(doy=yday(date))%>%
  group_by(sitename, doy) %>%
  dplyr::summarise(across(c(starts_with("gpp_"),"LUE"), mean, na.rm = TRUE))

df_sddoy<-df.merge%>%
  mutate(doy=yday(date))%>%
  group_by(sitename, doy) %>%
  summarise(gpp_obs_sd=sd(gpp_obs,na.rm = T),
            gpp_mod_sd=sd(gpp_mod,na.rm = T),
            LUE_sd=sd(LUE,na.rm = T)
            )
df_plot<-left_join(df_meandoy,df_sddoy)
#
plot_fun_gpp<-function(df,site,ylab_flag){
  # df<-df_plot
  # site<-"DE-Tha"
  # ylab_flag<-FALSE
  #
  df.proc<-df %>%filter(sitename==site)
  p_plot<-df.proc %>%
    ggplot()+
    geom_line(aes(x=doy,y=gpp_obs),size=1.1)+
    geom_ribbon(
      aes(x = doy, ymin = gpp_obs - gpp_obs_sd, 
          ymax = gpp_obs + gpp_obs_sd),
      fill = "grey",
      alpha = 0.4
      ) +
    # geom_ribbon(
    #   aes(x = doy, ymin = gpp_mod - gpp_mod_sd, 
    #       ymax = gpp_mod + gpp_mod_sd),
    #   fill = "red",
    #   alpha = 0.4
    # )+
    xlim(0,250)+
    ylim(-3,15)+
    labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
         x = "DOY") +
    annotate(geom = "text",x=25,y=14,label=site,size=5)+
    theme_classic()+
    theme(axis.text = element_text(size=16),
           axis.title = element_text(size=20))
  #
  if(site=="CH-Dav"){
    p_plot<-p_plot+
      geom_line(aes(x=doy,y=gpp_mod),col="red",size=1.1)  
  }
  if(site=="DE-Tha"){
    p_plot<-p_plot+
      geom_line(aes(x=doy,y=gpp_mod),col="orange",size=1.1)  
  }
  if(ylab_flag==FALSE){
    p_plot<-p_plot+ylab("")
  }
   return(p_plot)
}
#For GPP:
p_CH_Dav_gpp<-plot_fun_gpp(df_plot,"CH-Dav",TRUE)+
  xlab("")
p_DE_Tha_gpp<-plot_fun_gpp(df_plot,"DE-Tha",TRUE)+
  xlab("")
plot_grid(p_CH_Dav_gpp,p_DE_Tha_gpp,nrow = 1)


#For LUE:
plot_fun_LUE<-function(df,site,ylab_flag){
  # df<-df_plot
  # site<-"CH-Dav"
  # ylab_flag<-FALSE
  #
  df.proc<-df %>%filter(sitename==site)
  p_plot<-df.proc %>%
    ggplot()+
    geom_line(aes(x=doy,y=LUE,col=sitename),size=1.1)+
    geom_ribbon(
      aes(x = doy, ymin = LUE - LUE_sd, 
          ymax = LUE + LUE_sd, fill=sitename),
      alpha = 0.4
    ) +
    scale_color_manual(values = c("CH-Dav"="brown1",
                                  "DE-Tha"="orange"))+
    scale_fill_manual(values = c("CH-Dav"="brown1",
                                 "DE-Tha"="orange"))+
    xlim(0,250)+
    ylim(-0.18,0.15)+
    labs(y = expression("LUE"*" ("*mu*"mol m"^-2*mu*"mol"^-1*")"),
         x = "DOY") +
    annotate(geom = "text",x=25,y=14,label=site,size=5)+
    theme_classic()+
    theme(
      # legend.position = c(0.8,0.3),
      legend.position = "none",
      legend.text = element_text(size = 20),
      legend.background = element_blank(),
      legend.title = element_blank(),
      axis.text = element_text(size=20),
      axis.title = element_text(size=22))
  #
  # if(site=="CH-Dav"){
  #   p_plot<-p_plot+
  #     geom_line(aes(x=doy,y=gpp_mod),col="red",size=1.1)  
  # }
  # if(site=="DE-Tha"){
  #   p_plot<-p_plot+
  #     geom_line(aes(x=doy,y=gpp_mod),col="orange",size=1.1)  
  # }
  if(ylab_flag==FALSE){
    p_plot<-p_plot+ylab("")
  }
  return(p_plot)
  
}
#
p_CH_Dav_LUE<-plot_fun_LUE(df_plot,"CH-Dav",TRUE)+
  xlab("")
p_DE_Tha_LUE<-plot_fun_LUE(df_plot,"DE-Tha",TRUE)+
  xlab("")
# plot_grid(p_CH_Dav_LUE,p_DE_Tha_LUE,nrow = 1)

#-------------------------
#(2)explore overestimated period of GPP in early spring
#-------------------------
#load the phenology data:
load.path<-"./data/Comprehensive_plot_data/Fig1/"
df_pheno<-readRDS(file=paste0(load.path,"df_pheno.RDS"))
#
Dav_pheno<-df_pheno[df_pheno$sitename=="CH-Dav",]
Tha_pheno<-df_pheno[df_pheno$sitename=="DE-Tha",]

#-------------------------
#(3)adding lines and rectanges in plot
#-------------------------
#for CH-Dav
rect.coord_isevent=data.frame(
                              x1=Dav_pheno$sos10[1]-60,
                              x2=Dav_pheno$sos10[1],
                              x3=Dav_pheno$sos10[1]+60,
                              y1=-Inf, 
                              y2=Inf)
p_CH_Dav_LUE<-p_CH_Dav_LUE+
  # geom_rect(data=rect.coord_isevent,
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
    mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=Dav_pheno$sos10[1],col="forestgreen",size=1.1)+
  annotate(geom = "text",x=Dav_pheno$sos10[1]+15,y=-0.15,label="sos",col="forestgreen",size=7)

#for DE-Tha
rect.coord_isevent=data.frame(x1=Tha_pheno$sos10[1]-60,
                              x2=Tha_pheno$sos10[1],
                              x3=Tha_pheno$sos10[1]+60,
                              y1=-Inf, 
                              y2=Inf)
p_DE_Tha_LUE<-p_DE_Tha_LUE+
  # geom_rect(data=rect.coord_isevent,
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=Dav_pheno$sos10[1],col="forestgreen",size=1.1)+
  annotate(geom = "text",x=Dav_pheno$sos10[1]+15,y=-0.15,label="sos",col="forestgreen",size=7)
plot_grid(p_CH_Dav_LUE,p_DE_Tha_LUE,nrow = 1)

#-------------------------
#(4)making the boxplots
#-------------------------
df_add<-left_join(df_plot,df_pheno)%>%
  filter(sitename %in% c("DE-Tha","CH-Dav"))
df_add_Dav<-df_add %>%
  filter(sitename=="CH-Dav")
df_add_Tha<-df_add %>%
  filter(sitename=='DE-Tha')
df_add_LUE<-left_join(as.data.frame(df_add_Dav%>%dplyr::select(doy,sos10,LUE)%>%
                                      mutate(LUE_Dav=LUE,LUE=NULL))[,-1],
                      as.data.frame(df_add_Tha%>%dplyr::select(doy,LUE)%>%
                                      mutate(LUE_Tha=LUE,LUE=NULL))[,-1])%>%
  mutate(LUE_res=LUE_Dav-LUE_Tha)
###
df_add_1<-df_add_LUE%>%
  #only keep the data between [sos10-60,sos10_60]
  filter(doy>=sos10-60 & doy<sos10)%>%
  mutate(period="pre-sos")
df_add_2<-df_add_LUE%>%
  #only keep the data between [sos10-60,sos10_60]
  filter(doy>sos10 & doy<=sos10+60)%>%
  mutate(period="post-sos")
df_add_final<-rbind(df_add_1,df_add_2)%>%
  mutate(period=factor(period,levels=c("pre-sos","post-sos")))
#
p_LUE_boxplot<-df_add_final %>%
  ggplot(aes(x=period,y=LUE_res,fill=period))+
  geom_hline(yintercept = 0,lty=2,size=1.1)+
  geom_violin()+
  geom_boxplot(width = 0.1,position = position_dodge(0.9),col="grey",size=1.05) +
  stat_summary(fun = mean, geom = "point", size=3, color = "black")+
  ylab(expression(Delta*"LUE"*" ("*mu*"mol m"^-2*mu*"mol"^-1*")"))+
  xlab("")+
  scale_fill_manual(values = c("pre-sos"=adjustcolor("blue",0.3),
                               "post-sos"=adjustcolor("blue",0.1)))+
  theme_light()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=22)
  )
#update:only targets for post-sos:
p_LUE_boxplot<-df_add_final %>%
  filter(period=="post-sos")%>%
  ggplot(aes(x=period,y=LUE_res,fill=period))+
  geom_hline(yintercept = 0,lty=2,size=1.1)+
  geom_violin()+
  geom_boxplot(width = 0.1,position = position_dodge(0.9),col="grey",size=1.05) +
  stat_summary(fun = mean, geom = "point", size=3, color = "black")+
  ylab(expression(Delta*"LUE"*" ("*mu*"mol m"^-2*mu*"mol"^-1*")"))+
  xlab("CH-DAV — DE-Tha")+
  scale_fill_manual(values = c("pre-sos"=adjustcolor("blue",0.3),
                               "post-sos"=adjustcolor("blue",0.1)))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20)
  )
#sig.test:
x_lue_Dav<-as.numeric(unlist(as.data.frame(df_add_final)%>%
                               dplyr::select(LUE_Dav)))
x_lue_Tha<-as.numeric(unlist(as.data.frame(df_add_final)%>%
                               dplyr::select(LUE_Tha)))
ks.test(x_lue_Dav,"pnorm")
ks.test(x_lue_Tha,"pnorm")
sig.test<-wilcox.test(x_lue_Dav,x_lue_Tha)
#
p_LUE_boxplot<-p_LUE_boxplot +
  annotate("text",x=1,y=0.015,label=c("*"),size=10)

#merge the plots:
# plot_grid(p_CH_Dav,p_DE_Tha,p_boxplot,rows = 1)

# p_check_LUE<-plot_grid(p_CH_Dav_LUE,p_DE_Tha_LUE,p_LUE_boxplot,nrow=1,align = "hv",
#                               labels = c("(d)","(e)","(f)"))
p_check_LUE<-list("p_CH_Dav_LUE"=p_CH_Dav_LUE,
                      "p_DE_Tha_LUE"=p_DE_Tha_LUE,
                      "p_LUE_boxplot"=p_LUE_boxplot)
save.path<-"./data/Comprehensive_plot_data/Fig1/"
save(p_check_LUE,file=paste0(save.path,"p_check_LUE.RDA"))

