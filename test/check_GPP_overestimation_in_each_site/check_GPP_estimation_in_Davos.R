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
# df.drivers<-readRDS(paste0("D:/Github/flux_data_kit/data/p_model_drivers/","site_based_drivers.rds"))
#select the CH-Dav
# df.CH_Dav<-df.drivers %>%
#   filter(sitename=="CH-Dav")
# update in the Oct, 2022-->use the downloaded data from the ICOS2018 datasets:
load(file = "./data-raw/Data_from_ICOS_sites/processed_data_from_ICOS/Daily_data.RDA")
df.obs<-df_all_sel_daily

#2) for the simulated GPP from P-model:
df.model<-readRDS(paste0("D:/Github/flux_data_kit/data/p_model_output/","site_based_p-model_output.rds"))
df.mod<-df.model %>%
  filter(sitename=="FI-Hyy"|sitename=="NL-Loo"|sitename=="DE-Tha"|sitename=="CH-Dav")

#3)merge the data in CH-Dav together
# df<-as.data.frame(df.CH_Dav$forcing)
# df<-df %>%
#   mutate(gpp_obs=gpp,gpp=NULL)
df<-df.obs %>%
  dplyr::select(sitename,Date:GPP_DT_mean)%>%
  mutate(date=as.Date(Date),Date=NULL,
         gpp_obs=GPP_NT_mean
         )
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
df.merge<-left_join(df,df.model)

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
for (i in 1:length(sites)) {
  i=2
  site_name<-sites[i]
  df<-df.merge %>%
    dplyr::filter(sitename==site_name)
  df_andPlot<-sep_data_indiffY_sepMismatch(df,0.05,"Dfc","ENF",site_name,c(1997:2018))
  length(df_andPlot$p_isevent)
  p_diffYears<-df_andPlot$p_isevent
  p_merge_1<-plot_grid(p_diffYears[[1]],p_diffYears[[2]],p_diffYears[[3]],
                       p_diffYears[[4]],p_diffYears[[5]],p_diffYears[[6]],
                       p_diffYears[[7]],p_diffYears[[8]],p_diffYears[[9]],
                       p_diffYears[[10]],p_diffYears[[11]],p_diffYears[[12]],
                       labels = "auto",ncol=3,label_size = 12,align = "hv")
  p_merge_2<-plot_grid(p_diffYears[[13]],p_diffYears[[14]],p_diffYears[[15]],
                       p_diffYears[[16]],p_diffYears[[17]],p_diffYears[[18]],
                       p_diffYears[[19]],p_diffYears[[20]],p_diffYears[[21]],
                       #p_diffYears[[22]],p_diffYears[[23]],p_diffYears[[24]],
                       labels = "auto",ncol=3,label_size = 12,align = "hv")
  plot(p_merge_1)
  plot(p_merge_2)
}

###method 2: comparing EC GPP with GPP simulated by LUE-lme model:
#first merge the phenophase with df.merge
df_andPlot_FI_Hyy<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="FI-Hyy",],
    0.05,"Dfc","ENF","FI-Hyy",c(1996:2018))
df_andPlot_NL_Loo<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="NL-Loo",],
    0.05,"Cfb","ENF","NL-Loo",c(1997:2013)) #model GPP only avialble until 2013
df_andPlot_DE_Tha<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="DE-Tha",],
    0.05,"Cfb","ENF","DE-Tha",c(1996:2018))
df_andPlot_CH_Dav<-sep_data_indiffY_sepMismatch(df.merge[df.merge$sitename=="CH-Dav",],
    0.05,"Dfc","ENF","CH-Dav",c(1997:2018))
#
df.pheno<-rbind(df_andPlot_FI_Hyy$pos_agg,
                df_andPlot_NL_Loo$pos_agg,
                df_andPlot_DE_Tha$pos_agg,
                df_andPlot_CH_Dav$pos_agg)
df.pheno$Year<-c(c(1996:2018),c(1997:2013),c(1996:2018),c(1997:2018))
#
df.merge$Year<-year(df.merge$date)
#
library(ingestr)
df.update<-left_join(df.merge,df.pheno)
df.update<-df.update %>%
  mutate(sos=sos10)%>%   #set sos=sos10
  mutate(doy=yday(date),ppfd=PPFD_IN_fullday_mean,temp=Ta_mean,vpd=VPD_day_mean)%>% #update in Oct,2022
  mutate(greenup = ifelse(doy > sos & doy < peak, TRUE, FALSE))%>%
  mutate(lue = gpp_obs / (fapar * ppfd)) %>%
  mutate(lue = remove_outliers(lue)) #using the functions in the ingestr

##remove the na values and infinite data
##develop lme model for lue
tmp <- df.update %>%
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
mod_lmer <- lmer(lue ~ temp+log(vpd) + (1|sitename),
                 data=tmp)
# mod_lmer <- lm(lue ~ temp+log(vpd),  #update in Oct,2022
#                  data=tmp)

summary(mod_lmer)
#working here!!
