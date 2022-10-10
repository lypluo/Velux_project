#########################################################################
#Aim:compare the difference between flux data (especially modeleld and observed GPP)
#between flux2015(tidy by Beni) with data from flux data kit(tidy by Koen)
#-->to check the inconsistency 
#########################################################################
library(dplyr)
library(lubridate)

#load the data tidied by Beni:
##a. load Beni processed data:
load.path<-"D:/Github/photocold_manuscript/data-raw/raw_data/Data_sent_by_Beni/"
df.Beni<-read.csv(file=paste0(load.path,"ddf_fluxnet2015_pmodel_with_forcings_stocker19gmd.csv"))
#select the data from the sites(FI-Hyy,NL-Loo, and DE-Tha, CH-Dav is not available):
df.Beni<-df.Beni%>%
  filter(sitename %in% c("FI-Hyy","NL-Loo","DE-Tha"))


##b.load merged data processed data(GPP_mod tidied from Koen, 
#GPP_obs from ICOS datasets tidied by Yunpeng):
load(paste0("D:/Github/Velux_project/test/test_datasets/df.merge.test.RDA"))

##c.match df.Beni and df.merge to compare the differences(GPP_obs and GPP_mod):
df.Beni_sel<-df.Beni %>%
  dplyr::select(sitename,date,gpp_mod_FULL,gpp_obs)%>%
  mutate(date=mdy(date))%>%
  mutate(gpp_mod_Beni=gpp_mod_FULL,gpp_obs_Beni=gpp_obs,
         gpp_mod_FULL=NULL,gpp_obs=NULL)
#
df.merge_sel<-df.merge.test %>%
  dplyr::select(sitename,date,gpp_mod,gpp_obs)%>%
  mutate(gpp_mod_merge=gpp_mod,gpp_obs_merge=gpp_obs,
         gpp_mod=NULL,gpp_obs=NULL)
#compare the data:
df.comp<-left_join(df.Beni_sel,df.merge_sel)
#
library(ggplot2)
#I found that the gpp_mod have some difference between 
#different datasets(but in a reasonable range),
#but gpp_obs has very large differences(gpp tidied by YP using the ICOS)...
#for modelled gpp:
df.comp%>%
  group_by(sitename)%>%
  ggplot(aes(x=gpp_mod_Beni,y=gpp_mod_merge))+
  geom_point()+
  geom_abline(slope = 1,intercept = 0,col="blue",lty=2)+
  facet_wrap(.~sitename)
#for observed gpp:
df.comp%>%
  group_by(sitename)%>%
  ggplot(aes(x=gpp_obs_Beni,y=gpp_obs_merge))+
  geom_point()+
  geom_abline(slope = 1,intercept = 0,col="blue",lty=2)+
  facet_wrap(.~sitename)

##additional check-->using Koen tidied gpp_obs to compare the Beni's dataset
#!!the results show that the gpp_obs tidied by Koen is much more close to 
#Beni's data compared to mine--> so would now use the gpp_obs from Koen to do the analysis:
# but the reason why the gpp_obs differ a lot between YP and Koen's tidy need to explore!!
df.drivers<-readRDS(paste0("D:/Github/flux_data_kit/data/p_model_drivers/",
                           "site_based_drivers.rds"))
# select the sites
df.obs_koen<-df.drivers %>%
  filter(sitename=="FI-Hyy"|sitename=="NL-Loo"|sitename=="DE-Tha"|sitename=="CH-Dav")
df.obs_Datakit<-c()
for (i in 1:length(df.obs_koen$sitename)) {
  df.temp<-as.data.frame(df.obs_koen$forcing[i])
  df.temp<-df.temp %>%
    mutate(sitename=rep(df.obs_koen$sitename[i],nrow(df.temp)))
  df.obs_Datakit<-rbind(df.obs_Datakit,df.temp)
}
#
df.Koen_sel<-df.obs_Datakit %>%
  dplyr::select(sitename,date,gpp)%>%
  mutate(gpp_obs_Koen=gpp,gpp=NULL)
#
df.comp_more<-left_join(df.comp,df.Koen_sel)
df.comp_more%>%
  group_by(sitename)%>%
  ggplot(aes(x=gpp_obs_Beni,y=gpp_obs_Koen))+
  geom_point()+
  geom_abline(slope = 1,intercept = 0,col="blue",lty=2)+
  facet_wrap(.~sitename)
