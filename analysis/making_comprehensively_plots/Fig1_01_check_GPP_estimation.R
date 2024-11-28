########################################################
#Aim: to firstly check if GPP is overestimation in early spring
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
###addding a test plot:
df_meandoy <- df.merge %>%
  mutate(doy=yday(date))%>%
  group_by(sitename, doy) %>%
  dplyr::summarise(across(starts_with("gpp_"), mean, na.rm = TRUE))
df_sddoy<-df.merge%>%
  mutate(doy=yday(date))%>%
  group_by(sitename, doy) %>%
  summarise(gpp_obs_sd=sd(gpp_obs),
            gpp_mod_sd=sd(gpp_mod))
df_plot<-left_join(df_meandoy,df_sddoy)
#
plot_fun<-function(df,site,ylab_flag){
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
    geom_line(aes(x=doy,y=gpp_mod),col="red",size=1.1)+
    xlim(0,250)+
    ylim(-3,15)+
    labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
         x = "DOY") +
    annotate(geom = "text",x=25,y=14,label=site,size=5)+
    theme_light()+
    theme(axis.text = element_text(size=16),
           axis.title = element_text(size=20))
  if(ylab_flag==FALSE){
    p_plot<-p_plot+ylab("")
  }
   return(p_plot)
    
}
p_CH_Dav<-plot_fun(df_plot,"CH-Dav",TRUE)
p_DE_Tha<-plot_fun(df_plot,"DE-Tha",TRUE)
plot_grid(p_CH_Dav,p_DE_Tha,rows = 1)


#-------------------------
#(2)explore overestimated period of GPP in early spring
#-------------------------
sites<-c("DE-Tha","CH-Dav")
###method 1: comparing EC GPP with GPP simulated by P-model
#source the functions:
source("./R/separate_norm_GPPmismatch_period_trs_diff0_3SD.R")

##
df_andPlot_DE_Tha<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="DE-Tha",],
                                                0.05,"Cfb","ENF","DE-Tha",c(1996:2018))
df_andPlot_CH_Dav<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="CH-Dav",],
                                                0.05,"Dfc","ENF","CH-Dav",c(1997:2018))

length(df_andPlot_CH_Dav$p_isevent)
p_diffYears<-df_andPlot_CH_Dav$p_isevent
p_merge_1<-plot_grid(p_diffYears[[1]],p_diffYears[[2]],p_diffYears[[3]],
                     p_diffYears[[4]],p_diffYears[[5]],p_diffYears[[6]],
                     p_diffYears[[7]],p_diffYears[[8]],p_diffYears[[9]],
                     p_diffYears[[10]],p_diffYears[[11]],p_diffYears[[12]],
                     labels = "auto",ncol=3,label_size = 12,align = "hv")
p_merge_2<-plot_grid(p_diffYears[[13]],p_diffYears[[14]],p_diffYears[[15]],
                     p_diffYears[[16]],p_diffYears[[17]],
                     p_diffYears[[18]],
                     p_diffYears[[19]],p_diffYears[[20]],p_diffYears[[21]],
                     p_diffYears[[22]],
                     # p_diffYears[[23]],
                     labels = "auto",ncol=3,label_size = 12,align = "hv")

#tidy the timing of mean of phenology:
Dav_pheno<-round(colMeans(df_andPlot_CH_Dav$pos_agg,na.rm = T),0)
THA_pheno<-round(colMeans(df_andPlot_DE_Tha$pos_agg,na.rm = T),0)

#-------------------------
#(3)adding lines and rectanges in plot
#-------------------------
#period:[sos-60,sos+60] is set to red
#adding the sos
#for CH-Dav
rect.coord_isevent=data.frame(x1=Dav_pheno[1],
                         x2=Dav_pheno[1]+60,
                         y1=-Inf, 
                         y2=Inf)
p_CH_Dav<-p_CH_Dav+
  geom_rect(data=rect.coord_isevent,
    mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.2)+
  geom_vline(xintercept=Dav_pheno[1],col="forestgreen",size=1.1)+
  annotate(geom = "text",x=Dav_pheno[1]+10,y=-1,label="sos",col="forestgreen",size=6)

#for DE-Tha
rect.coord_isevent=data.frame(x1=THA_pheno[1],
                              x2=THA_pheno[1]+60,
                              y1=-Inf, 
                              y2=Inf)
p_DE_Tha<-p_DE_Tha+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.2)+
  geom_vline(xintercept=Dav_pheno[1],col="forestgreen",size=1.1)+
  annotate(geom = "text",x=Dav_pheno[1]+10,y=-1,label="sos",col="forestgreen",size=6)

#
df_pheno<-as.data.frame(rbind(THA_pheno,Dav_pheno))
df_pheno$sitename<-c("DE-Tha","CH-Dav")
#save the phenos
save.path<-"./data/Comprehensive_plot_data/Fig1/"
saveRDS(df_pheno,file=paste0(save.path,"df_pheno.RDS"))
#-------------------------
#(4)making the boxplots
#-------------------------
df_add<-left_join(df_plot,df_pheno)%>%
  filter(sitename %in% c("DE-Tha","CH-Dav"))%>%
  #only keep the data between [sos10-60,sos10_60]
  filter(doy>sos10 & doy<sos10+60)%>%
  mutate(gpp_res=gpp_mod-gpp_obs)
#
p_boxplot<-df_add %>%
  ggplot(aes(x=sitename,y=gpp_res,fill=sitename))+
  geom_violin()+
  geom_boxplot(width = 0.1, position = position_dodge(0.9),col="grey",size=1.05) +
  stat_summary(fun = mean, geom = "point", size=3, color = "black")+
  labs(y = expression( paste(Delta*"GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "")+
  scale_fill_manual(values = c("CH-Dav"="orange",
                               "DE-Tha"="blue"))+
  theme_light()+
  theme(legend.position = "none",
        axis.text = element_text(size=16),
        axis.title = element_text(size=20)
        )

#merge the plots:
# plot_grid(p_CH_Dav,p_DE_Tha,p_boxplot,rows = 1)

p_check_overest<-plot_grid(p_CH_Dav,p_DE_Tha,p_boxplot,nrow=1,
                              labels = c("(a)","(b)","(c)"))
###save the ggplot plots:
# p_check_overest<-list("p_over_CH_Dav"=p_CH_Dav,
#                       "p_over_DE_Tha"=p_DE_Tha,
#                       "p_overGPP_boxplot"=p_boxplot)
save.path<-"./data/Comprehensive_plot_data/Fig1/"
save(p_check_overest,file=paste0(save.path,"p_check_overest.RDA"))

