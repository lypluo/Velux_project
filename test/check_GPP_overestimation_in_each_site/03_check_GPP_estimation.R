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
###addding a test plot:
df_meandoy <- df.merge %>%
  mutate(doy=yday(date))%>%
  group_by(sitename, doy) %>%
  dplyr::summarise(across(starts_with("gpp_"), mean, na.rm = TRUE))
#
##plot by site:
df_meandoy %>%
  pivot_longer(c(gpp_obs, gpp_mod), names_to = "source", values_to = "gpp") %>%
  #fct_relevel: in tidyverse package
  ggplot() +
  # geom_ribbon(
  #   aes(x = doy, ymin = obs_min, ymax = obs_max),
  #   fill = "black",
  #   alpha = 0.2
  #   ) +
  geom_line(aes(x = doy, y = gpp, color = source), size = 0.4) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DOY") +
  facet_wrap( ~sitename, ncol = 2 ) +    # , labeller = labeller(climatezone = list_rosetta)
  theme_gray() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    name="GPP source ",
    values=c("gpp_obs"="black","gpp_mod"="red")
  )

##-------------------------
## Normalise to peak season
##-------------------------
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
                     p_diffYears[[18]],
                     p_diffYears[[19]],p_diffYears[[20]],p_diffYears[[21]],
                     p_diffYears[[22]],
                     # p_diffYears[[23]],
                     labels = "auto",ncol=3,label_size = 12,align = "hv")
plot(p_merge_1)
plot(p_merge_2)

###method 2: comparing EC GPP with GPP simulated by LUE-lme model:
#first merge the phenophase with df.merge
df.pheno<-rbind(df_andPlot_FI_Hyy$pos_agg,
                df_andPlot_NL_Loo$pos_agg,
                df_andPlot_DE_Tha$pos_agg,
                df_andPlot_CH_Dav$pos_agg)
df.pheno$Year<-c(c(1996:2018),c(1997:2013),c(1996:2018),c(1997:2018))
#
df.merge$Year<-year(df.merge$date)
#2a. using four sites to develop the lmer model 
library(ingestr)
df.update<-left_join(df.merge,df.pheno)
df.update<-df.update %>%
  mutate(sos=sos10)%>%   #set sos=sos10
  # mutate(sos=60)%>%#update 2022,Oct:to simplize the analysis--> set sos=60(March,01)
  #！need to check tomorrow for ppfd
  mutate(doy=yday(date),ppfd=ppfd*100000)%>% #ppfd-->change to unit:umol m-2s-1update in Oct,2022
  mutate(greenup = ifelse(doy > sos & doy < peak, TRUE, FALSE))%>%
  mutate(lue = gpp_obs / (fPAR * ppfd)) %>%
  mutate(lue = remove_outliers(lue)) #using the functions in the ingestr

##remove the na values and infinite data
##develop lme model for lue
tmp_4sites <- df.update %>%
  dplyr::filter(!greenup) %>%
  dplyr::filter(ppfd > 5) %>%
  dplyr::select(temp, vpd, lue, sitename, Year) %>%
  drop_na() %>%
  dplyr::filter(!is.infinite(lue) & !is.nan(lue)) %>%
  dplyr::filter(!is.infinite(temp) & !is.nan(temp)) %>%
  dplyr::filter(!is.infinite(vpd) & !is.nan(vpd)) %>%
  filter(vpd > 0 & lue > 0)

#
library(lme4)
mod_lmer_4sites <- lmer(lue ~ temp+log(vpd) + (1|sitename),
                 data=tmp_4sites)
summary(mod_lmer_4sites)
#2b. using ca. 40 sites to develop the lmer model-->using the data in the previous study:
#update in Oct, 05
load(file=paste0("D:/Github/photocold_manuscript/test/test_dataset/","GPP_andLUE.RDA"))
tmp_39sites <- ddf %>% 
  dplyr::filter(!greenup) %>%
  dplyr::filter(PPFD_IN_fullday_mean_fluxnet2015 > 5) %>% 
  dplyr::select(temp_day_fluxnet2015, vpd_day_fluxnet2015, lue, sitename, year) %>%
  drop_na() %>% 
  dplyr::filter(!is.infinite(lue) & !is.nan(lue)) %>% 
  dplyr::filter(!is.infinite(temp_day_fluxnet2015) & !is.nan(temp_day_fluxnet2015)) %>% 
  dplyr::filter(!is.infinite(vpd_day_fluxnet2015) & !is.nan(vpd_day_fluxnet2015)) %>% 
  filter(vpd_day_fluxnet2015 > 0 & lue > 0) %>% 
  droplevels()
#change the names:
tmp_39sites<-tmp_39sites%>%
  mutate(temp=temp_day_fluxnet2015,vpd=vpd_day_fluxnet2015,
         temp_day_fluxnet2015=NULL,vpd_day_fluxnet2015=NULL,
         Year=year,year=NULL)
#also add the CH-Dav site:
tmp_CH_Dav<-tmp_4sites[tmp_4sites$sitename=="CH-Dav",]
tmp_CH_Dav<-tmp_CH_Dav %>%
  dplyr::select(lue,sitename,temp,vpd,Year)
tmp_40sites<-rbind(tmp_39sites,tmp_CH_Dav)
##
mod_lmer_40sites <- lmer(lue ~ temp+log(vpd) + (1|sitename),
                 data=tmp_40sites)
summary(mod_lmer_40sites)

#predict the gpp using the developed lmer...
#update from the results:Oct,5-->the results are better when using the
#model built by 4 sites
#for the model using the for 4 sites
ddf_lmer_4sites <- df.update %>%
  mutate(lue_lmer = predict(mod_lmer_4sites, newdata = .)) %>%
  mutate(gpp_lmer = lue_lmer * fPAR * ppfd)
ddf_lmer_40sites <- df.update %>%
  mutate(lue_lmer = predict(mod_lmer_40sites, newdata = .)) %>%
  mutate(gpp_lmer = lue_lmer * fPAR * ppfd)

#compare the modelled gpp with observed gpp
devtools::load_all("D:/Github/rbeni/")
library(rbeni)

##-----------
#seasonal cycle
##-----------
df_meandoy <- ddf_lmer_4sites %>%   #4 sites data using the lmer model trained by 40 sites
  group_by(sitename, doy) %>%
  dplyr::summarise(across(starts_with("gpp_"), mean, na.rm = TRUE))

##plot by site:
df_meandoy %>%
  filter(!is.nan(gpp_lmer) & !is.infinite(gpp_lmer))%>% ##this filter is important
  pivot_longer(c(gpp_obs, gpp_mod, gpp_lmer), names_to = "model", values_to = "gpp") %>%
  #fct_relevel: in tidyverse package
  mutate(model = fct_relevel(model, "gpp_obs", "gpp_mod","gpp_lmer")) %>%
  dplyr::filter((model %in% c( "gpp_obs", "gpp_mod","gpp_lmer"))) %>%
  ggplot() +
  # geom_ribbon(
  #   aes(x = doy, ymin = obs_min, ymax = obs_max),
  #   fill = "black",
  #   alpha = 0.2
  #   ) +
  geom_line(aes(x = doy, y = gpp, color = model), size = 0.4) +
  labs(y = expression( paste("Simulated GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DOY") +
  facet_wrap( ~sitename, ncol = 2 ) +    # , labeller = labeller(climatezone = list_rosetta)
  theme_gray() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    name="Model: ",
    values=c("black", "red", "royalblue")
  )

##normalize the data
ddf_norm <- ddf_lmer_4sites %>%  
  group_by(sitename) %>%
  nest() %>%
  mutate(data = purrr::map(data, ~norm_to_peak(., "gpp_mod", "gpp_obs"))) %>%
  mutate(data = purrr::map(data, ~norm_to_peak(., "gpp_lmer", "gpp_obs"))) %>%
  unnest(data)

### Plot normalised by site
df_meandoy_norm <- ddf_norm %>%
  group_by(sitename, doy) %>%
  dplyr::summarise(across(starts_with("gpp_"), mean, na.rm = TRUE))

#
df_meandoy_norm %>%
  filter(!is.nan(gpp_lmer) & !is.infinite(gpp_lmer))%>% ##this filter is important
  pivot_longer(c(gpp_obs, gpp_mod, gpp_lmer), names_to = "model", values_to = "gpp") %>%
  mutate(model = fct_relevel(model, "gpp_obs", "gpp_mod", "gpp_lmer")) %>%
  dplyr::filter((model %in% c( "gpp_obs", "gpp_mod","gpp_lmer"))) %>%  ##only select one model
  ggplot() +
  # geom_ribbon(
  #   aes(x = doy, ymin = obs_min, ymax = obs_max),
  #   fill = "black",
  #   alpha = 0.2
  #   ) +
  geom_line(aes(x = doy, y = gpp, color = model)) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DoY") +
  facet_wrap( ~sitename) +
  # theme_gray() +
  scale_color_manual("GPP sources",values = c("gpp_obs" = "black",
       "gpp_mod" = "red","gpp_lmer"="dodgerblue"),
                     labels = c("Obervations","P-model","LME"))+
  theme(
    legend.text = element_text(size=20),
    legend.key.size = unit(2, 'lines'),
    axis.title = element_text(size=24),
    axis.text = element_text(size = 20),
    text = element_text(size=24),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour ="grey",fill="white"),
    # legend.background = element_blank(),
    legend.position = c(0.75,0.2)
  )

##only check the GPP predicted by the lmer model
df_meandoy_norm %>%
  filter(!is.nan(gpp_lmer) & !is.infinite(gpp_lmer))%>% ##this filter is important
  pivot_longer(c(gpp_obs, gpp_mod, gpp_lmer), names_to = "model", values_to = "gpp") %>%
  mutate(model = fct_relevel(model, "gpp_obs", "gpp_mod", "gpp_lmer")) %>%
  dplyr::filter((model %in% c( "gpp_obs", "gpp_lmer"))) %>%  ##only select one model
  ggplot() +
  # geom_ribbon(
  #   aes(x = doy, ymin = obs_min, ymax = obs_max),
  #   fill = "black",
  #   alpha = 0.2
  #   ) +
  geom_line(aes(x = doy, y = gpp, color = model)) +
  labs(y = expression( paste("Norm GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DoY") +
  facet_wrap( ~sitename) +
  # theme_gray() +
  scale_color_manual("GPP sources",values = c("gpp_obs" = "black",
      "gpp_lmer"="red"),
                     labels = c("Obervations","LME"))+
  theme_light() +
  theme(legend.position = "bottom") 
  # theme(
  #   legend.text = element_text(size=20),
  #   legend.key.size = unit(2, 'lines'),
  #   axis.title = element_text(size=24),
  #   axis.text = element_text(size = 20),
  #   text = element_text(size=24),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   panel.background = element_rect(colour ="grey",fill="white"),
  #   # legend.background = element_blank(),
  #   legend.position = c(0.75,0.15)
  # )
##--------------------------
#Additional check the year variation in CH-Dav:
##--------------------------
df_meandoy_norm_Year <- ddf_norm %>%
  filter(sitename=="CH-Dav")%>%
  group_by(Year,doy) %>%
  dplyr::summarise(across(starts_with("gpp_"), mean, na.rm = TRUE))
#
df_meandoy_norm_Year %>%
  filter(!is.nan(gpp_lmer) & !is.infinite(gpp_lmer))%>% ##this filter is important
  pivot_longer(c(gpp_obs, gpp_mod, gpp_lmer), names_to = "model", values_to = "gpp") %>%
  mutate(model = fct_relevel(model, "gpp_obs", "gpp_mod", "gpp_lmer")) %>%
  dplyr::filter((model %in% c( "gpp_obs", "gpp_lmer"))) %>%  ##only select one model
  ggplot() +
  # geom_ribbon(
  #   aes(x = doy, ymin = obs_min, ymax = obs_max),
  #   fill = "black",
  #   alpha = 0.2
  #   ) +
  geom_line(aes(x = doy, y = gpp, color = model)) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DoY") +
  facet_wrap( ~Year) +
  # theme_gray() +
  scale_color_manual("GPP sources",values = c("gpp_obs" = "black",
                                              "gpp_lmer"="dodgerblue"),
                     labels = c("Obervations","LME"))+
  theme(
    legend.text = element_text(size=20),
    legend.key.size = unit(2, 'lines'),
    axis.title = element_text(size=24),
    axis.text = element_text(size = 20),
    text = element_text(size=24),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour ="grey",fill="white"),
    # legend.background = element_blank(),
    legend.position = c(0.9,0.05)
  )

###method 3: comparing EC GPP with GPP simulated by GAM (general additaive model):
#first merge the phenophase with df.merge
df.pheno<-rbind(df_andPlot_FI_Hyy$pos_agg,
                df_andPlot_NL_Loo$pos_agg,
                df_andPlot_DE_Tha$pos_agg,
                df_andPlot_CH_Dav$pos_agg)
df.pheno$Year<-c(c(1996:2018),c(1997:2013),c(1996:2018),c(1997:2018))
#
df.merge$Year<-year(df.merge$date)
#2a. using four sites to develop the gam model 
library(ingestr)
df.update<-left_join(df.merge,df.pheno)
df.update<-df.update %>%
  mutate(sos=sos10)%>%   #set sos=sos10
  # mutate(sos=60)%>%#update 2022,Oct:to simplize the analysis--> set sos=60(March,01)
  #！need to check tomorrow for ppfd
  mutate(doy=yday(date),ppfd=ppfd*100000)%>% #ppfd-->change to unit:umol m-2s-1update in Oct,2022
  mutate(greenup = ifelse(doy > sos & doy < peak, TRUE, FALSE))%>%
  mutate(lue = gpp_obs / (fPAR * ppfd)) %>%
  mutate(lue = remove_outliers(lue)) #using the functions in the ingestr

##remove the na values and infinite data
##develop gam model for lue
tmp_4sites <- df.update %>%
  dplyr::filter(!greenup) %>%
  dplyr::filter(ppfd > 5) %>%
  dplyr::select(temp, vpd,lue, sitename, Year) %>%
  drop_na() %>%
  dplyr::filter(!is.infinite(lue) & !is.nan(lue)) %>%
  dplyr::filter(!is.infinite(temp) & !is.nan(temp)) %>%
  dplyr::filter(!is.infinite(vpd) & !is.nan(vpd)) %>%
  # dplyr::filter(!is.infinite(ppfd) & !is.nan(ppfd)) %>%
  filter(vpd > 0 & lue > 0)

#using the gam model
library(mgcv)
mod_gam_4sites<-gam(lue ~ s(temp) +s(log(vpd)), data=tmp_4sites)
summary(mod_gam_4sites)
#
preds<-predict.gam(mod_gam_4sites,newdata=tmp_4sites)
##does not working for the model
ddf_gam_4sites <- df.update %>%
  mutate(lue_gam = predict.gam(mod_gam_4sites, newdata = .)) %>%
  mutate(gpp_gam = lue_gam * fPAR * ppfd)
