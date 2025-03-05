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
###addding a test plot-->update in Nov,2022
df_meandoy_mean <- df.merge %>%
  mutate(doy=yday(date))%>%
  group_by(sitename, doy) %>%
  # dplyr::summarise(across(starts_with("gpp_"), mean, na.rm = TRUE))
  dplyr::summarise(gpp_obs=mean(gpp_obs,na.rm=T),gpp_mod=mean(gpp_mod,na.rm=T))%>%
  pivot_longer(c(gpp_obs, gpp_mod), names_to = "source", values_to = "gpp")

df_meandoy_sd <- df.merge %>%
  mutate(doy=yday(date))%>%
  group_by(sitename, doy) %>%
  dplyr::summarise(gpp_obs=sd(gpp_obs,na.rm = T),gpp_mod=sd(gpp_mod,na.rm = T))%>%
  pivot_longer(c(gpp_obs, gpp_mod), names_to = "source", values_to = "gpp_sd")

df_meandoy<-left_join(df_meandoy_mean,df_meandoy_sd)
#
##plot by site:
general_mod_vs_obs<-df_meandoy %>%
  # pivot_longer(c(gpp_obs_sd,gpp_mod_sd),names_to = "source",values_to = "gpp_sd")
  #fct_relevel: in tidyverse package
  ggplot() +
  geom_ribbon(
    aes(x = doy, ymin = gpp-gpp_sd, ymax = gpp+gpp_sd,fill = source),
    alpha = 0.2
    ) +
  geom_line(aes(x = doy, y = gpp, color = source), size = 0.4) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DOY") +
  facet_wrap( ~sitename, ncol = 2 ) +    # , labeller = labeller(climatezone = list_rosetta)
  theme_light() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    name="GPP mean ",
    values=c("gpp_obs"="black","gpp_mod"="red")
  )+
  scale_fill_manual(
    name="GPP sd",
    values=c("gpp_obs"="black","gpp_mod"="red")
  )
#
save.path<-"./test/test_figs/"
ggsave(general_mod_vs_obs,filename = paste0(save.path,"general_mod_vs_obs.png"))


##-------------------------
## Normalise to peak season
##-------------------------
#!!update Nov, 2022:normalization has not been updated-->need to be updated if want to use
norm_to_peak <- function(df, mod, obs){
  # df<-ddf_t
  # mod<-"gpp_lmer"
  # obs<-"gpp_obs"
  
  q75_obs <- quantile(df[[obs]], probs = 0.75, na.rm = TRUE)
  q75_mod <- quantile(df[[mod]], probs = 0.75, na.rm = TRUE)
  
  ## normalise mod
  #add by YP:first need to change infinite values to NA:
  # df[[mod]][is.nan(df[[mod]])]<-NA
  df[[mod]][is.infinite(df[[mod]])]<-NA
  df[[mod]] <- df[[mod]] *
    mean(df[[obs]][df[[obs]]>q75_obs], na.rm = TRUE) /
    mean(df[[mod]][df[[mod]]>q75_mod], na.rm = TRUE)  #YP revised here:some error from Beni's functions
  
  return(df)
}

ddf_norm <- df_meandoy %>%  
  group_by(sitename) %>%
  nest() %>%
  mutate(data = purrr::map(data, ~norm_to_peak(., "gpp_mod", "gpp_obs"))) %>%
  unnest(data)
##plotting the norm data:
df_meandoy_norm <- ddf_norm %>%
  group_by(sitename, doy) %>%
  dplyr::summarise(across(starts_with("gpp_"), mean, na.rm = TRUE))
#
Norm_GPP_comp<-df_meandoy_norm %>%
  pivot_longer(c(gpp_obs, gpp_mod), names_to = "source", values_to = "gpp") %>%
  #fct_relevel: in tidyverse package
  ggplot() +
  geom_line(aes(x = doy, y = gpp, color = source), size = 0.4) +
  labs(y = expression( paste("Norm GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DOY") +
  facet_wrap( ~sitename, ncol = 2 ) +    # , labeller = labeller(climatezone = list_rosetta)
  theme_light() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    name="GPP sources ",
    values=c("gpp_obs"="black","gpp_mod"="red")
  )
save.path<-"./test/test_figs/"
ggsave(Norm_GPP_comp,filename = paste0(save.path,"norm_GPP_Pmodl_comp.png"),
       height = 6,width = 8)
#-------------------------
#(2)explore if GPP is overestimated in early spring
#-------------------------
sites<-c("FI-Hyy","NL-Loo","DE-Tha","CH-Dav")
###method 1: comparing EC GPP with GPP simulated by P-model
#source the functions:
source("./R/separate_norm_GPPmismatch_period_trs_diff0_3SD.R")
###one test example:###
#update in Oct,2022-->the simulated GPP from P-model should has some probelm,
#as now there is no spring gpp overestimation anymore in four sites.
df_andPlot_FI_Hyy<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="FI-Hyy",],
                                                0.05,"Dfc","ENF","FI-Hyy",c(1996:2018))
#
df_andPlot_NL_Loo<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="NL-Loo",],
                                                0.05,"Cfb","ENF","NL-Loo",c(1997:2013)) #model GPP only avialble until 2013
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
                     p_diffYears[[18]],p_diffYears[[19]],p_diffYears[[20]],
                     p_diffYears[[21]],p_diffYears[[22]],
                     # p_diffYears[[23]],p_diffYears[[24]],
                     labels = "auto",ncol=3,label_size = 12,align = "hv")
plot(p_merge_1)
plot(p_merge_2)

