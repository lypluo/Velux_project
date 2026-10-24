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
#test:
tt<-df.merge%>%filter(sitename=="CH-Dav")
ggplot()+
  geom_point(data=tt%>%filter(date>=as.Date("2018-01-01")),aes(x=date,y=gpp_obs,col="obs"))+
  geom_line(data=tt%>%filter(date>=as.Date("2018-01-01")),aes(x=date,y=gpp_mod,col="mod"))
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
    xlim(0,250)+
    ylim(-3,15)+
    labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
         x = "DOY") +
    # annotate(geom = "text",x=25,y=14,label=site,size=5)+
    theme_classic()+
    theme(axis.text = element_text(size=20),
           axis.title = element_text(size=22))
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
p_CH_Dav<-plot_fun(df_plot,"CH-Dav",TRUE)+
  xlab("")+
  labs(title = "CH-Dav")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold")
  )
p_DE_Tha<-plot_fun(df_plot,"DE-Tha",TRUE)+
  xlab("")+
  labs(title = "DE-Tha")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold")
  )
plot_grid(p_CH_Dav,p_DE_Tha,nrow = 1)

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

#update in Apr,2026: calculate the overestimated days:
# the stats that were summarized:
  #A.sos,peak
  #B.Over_start,Over_end,Over_period_length,Over_days_length(for GPP overestmation)
  #C.Over_start_T10,Over_end_T10,Over_period_length_T10,Over_days_length_T10(for GPP overestmation)
library(plyr)
summary_fun<-function(df){
  # df<-df_no_oversites_sel
  
  df_greenup<-df[df$greenup=="yes",]
  df_greenup_sum<-ddply(df_greenup,.(sitename,Year),summarise,sos=min(doy),peak=max(doy))
  
  df_event<-df[df$is_event=="yes",]
  df_event_sum<-ddply(df_event,.(sitename,Year),summarise,Over_start=min(doy),Over_end=max(doy),
                      Over_period_length=max(doy)-min(doy)+1,Over_days_length=length(is_event))
  df_event_T10<-df[df$is_event_less10=="yes",]
  df_event_T10_sum<-ddply(df_event_T10,.(sitename,Year),summarise,Over_start_T10=min(doy),Over_end_T10=max(doy),
                          Over_period_length_T10=max(doy)-min(doy)+1,Over_days_length_T10=length(is_event))
  #
  df_event_agg_temp<-merge(df_greenup_sum,df_event_sum,by = c("sitename","Year"),all.x = T)
  df_event_agg<-merge(df_event_agg_temp,df_event_T10_sum,by= c("sitename","Year"),all.x = T)
  return(df_event_agg)
}
#
df_add<-rbind(df_andPlot_CH_Dav$df_agg,df_andPlot_DE_Tha$df_agg)
df_events_Velux_sites<-summary_fun(df_add)
df_GPP_overperiod_Velux_sites<-df_events_Velux_sites %>%
  group_by(sitename)%>%
  dplyr::summarise(GPPover_length=round(mean(Over_days_length,na.rm=T),0),
                   nyear=n())%>%
  mutate(site_flag=ifelse(GPPover_length<=20|is.na(GPPover_length),"Not_OverGPP","OverGPP"))
#save the GPP over days:
file.path<-"./data/EC_MeteoandFlux/site_meteo/"
save(df_GPP_overperiod_Velux_sites,file = paste0(file.path,"df_event_length_Velux_sites.RDA"))
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
    mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=Dav_pheno[1],col="forestgreen",size=1.1)+
  annotate(geom = "text",x=Dav_pheno[1]+15,y=-1,label="sos",col="forestgreen",size=7)

#for DE-Tha
rect.coord_isevent=data.frame(x1=THA_pheno[1],
                              x2=THA_pheno[1]+60,
                              y1=-Inf, 
                              y2=Inf)
p_DE_Tha<-p_DE_Tha+
  geom_rect(data=rect.coord_isevent,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=Dav_pheno[1],col="forestgreen",size=1.1)+
  annotate(geom = "text",x=Dav_pheno[1]+15,y=-1,label="sos",col="forestgreen",size=7)

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
#update the plot:
NA_df<-as.data.frame(t(c("Empty",rep(NA,ncol(df_add)-1))))
names(NA_df)<-names(df_add)
NA_df[,2:ncol(NA_df)]<-as.numeric(NA_df[,2:ncol(NA_df)])
df_add<-bind_rows(df_add,NA_df)
df_add$sitename <- factor(df_add$sitename, 
                          levels = c("CH-Dav", "DE-Tha", "Empty"))
#
p_boxplot<-df_add %>%
  filter(sitename!="Empty")%>%
  ggplot(aes(x=sitename,y=gpp_res,fill=sitename))+
  geom_hline(yintercept = 0,lty=2,size=1.1)+
  geom_violin()+
  geom_boxplot(width = 0.1, position = position_dodge(0.9),col="grey",size=1.05) +
  stat_summary(fun = mean, geom = "point", size=3, color = "black")+
  labs(y = expression( paste(Delta*"GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "",
       title = expression(paste(Delta*" Vars"))
       )+
  scale_fill_manual(values = c("CH-Dav"="brown1",
                               "DE-Tha"="orange",
                               "Empty" = "white"))+
  scale_x_discrete(labels = c("CH-Dav" = "CH-Dav",
                              "DE-Tha" = "DE-Tha",
                              "Empty" = "CH-Dav − DE-Tha")) +  
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(size=20),
        axis.title = element_text(size=22),
        )+
  theme(
    plot.title = element_text(hjust = 0.5, size = 24,face = "bold")
  )
#add significant test--gpp_obs significantly differ gpp_mod
#normality test:
# shapiro.test(as.numeric(unlist(df_add[df_add$sitename=="CH-Dav","gpp_res"])))
# sig.Dav<-wilcox.test(as.data.frame(df_add%>%filter(sitename=="CH-Dav" & !is.na(gpp_res))
#                                    %>%dplyr::select("gpp_res"))$gpp_res)

x_gpp_mod<-as.numeric(unlist(as.data.frame(df_add)%>%
                     filter(sitename=="CH-Dav")%>%dplyr::select(gpp_mod)))
x_gpp_obs<-as.numeric(unlist(as.data.frame(df_add)%>%
                     filter(sitename=="CH-Dav")%>%dplyr::select(gpp_obs)))
ks.test(x_gpp_mod,"pnorm")
ks.test(x_gpp_obs,"pnorm")
sig.Dav<-wilcox.test(x_gpp_mod,x_gpp_obs)
#
# shapiro.test(as.numeric(unlist(df_add[df_add$sitename=="DE-Tha","gpp_res"])))
# sig.Tha<-t.test(as.data.frame(df_add%>%filter(sitename=="DE-Tha" & !is.na(gpp_res))
#                               %>%dplyr::select("gpp_res"))$gpp_res)
x_gpp_mod<-as.numeric(unlist(as.data.frame(df_add)%>%
                               filter(sitename=="DE-Tha")%>%dplyr::select(gpp_mod)))
x_gpp_obs<-as.numeric(unlist(as.data.frame(df_add)%>%
                               filter(sitename=="DE-Tha")%>%dplyr::select(gpp_obs)))
ks.test(x_gpp_mod,"pnorm")
ks.test(x_gpp_obs,"pnorm")
sig.Tha<-wilcox.test(x_gpp_mod,x_gpp_obs)

#
p_boxplot<-p_boxplot +
  annotate("text",x=c("CH-Dav","DE-Tha"),y=c(2.8,1),label=c("*",""),size=10)


#merge the plots:
# plot_grid(p_CH_Dav,p_DE_Tha,p_boxplot,rows = 1)

# p_check_overest<-plot_grid(p_CH_Dav,p_DE_Tha,p_boxplot,nrow=1,align="hv",
#                              labels = c("(a)","(b)","(c)"))
# p_check_overest<-plot_grid(p_CH_Dav,p_DE_Tha,nrow=1,
#                            labels = c("(a)","(b)"))
###save the ggplot plots:
p_check_overest<-list("p_over_CH_Dav"=p_CH_Dav,
                      "p_over_DE_Tha"=p_DE_Tha,
                      "p_overGPP_boxplot"=p_boxplot)
save.path<-"./data/Comprehensive_plot_data/Fig1/"
save(p_check_overest,file=paste0(save.path,"p_check_overest.RDA"))

