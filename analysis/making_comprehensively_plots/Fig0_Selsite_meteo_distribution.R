#####################################################
#Aim: check the where the CH-Dav and DE-Tha (Meteo aspects) located in
#####################################################
library(tidyselect)
library(dplyr)
library(lubridate)
library(ggplot2)
#------------
#load the data
#------------
#meteo from previous studies:
load.path1<-"D:/Github/photocold_manuscript/data/site_meteo/"
#meteo from Davos and Tharandt:
load.path2<-"./data/EC_MeteoandFlux/site_meteo/"
#
load(paste0(load.path1,"site_meteo_season.RDA"))
df_meteo_1<-df_sum_season_final$season
load(paste0(load.path2,"site_meteo.RDA"))
df_meteo_2<-df_sum_season_final$season
df_meteo_2_2season<-df_sum_season_final$`2season`
#-----------
#some wrong info for ppfdin df_meteo_1,using another datasets
#----------
load.path1_rev<-"D:/Github/photocold_manuscript/data/data_used/"
load(paste0(load.path1_rev,"ddf_labeled_norm_trs_newmethod_all_overestimation_Fluxnet2015_sites.RDA"))
df_meteo_1_rev<-ddf_labeled %>%
  dplyr::select(sitename,date,Year,temp_day_fluxnet2015,prec_fluxnet2015,vpd_day_fluxnet2015,PPFD_IN_fullday_mean_fluxnet2015)
#-----------
#summarize the meteos
#-----------
df_sum_yearly_1_1<-df_meteo_1_rev %>%
  #separate the temperature in different season
  mutate(month=month(date),
         season=case_when(month>=3 & month<=5 ~"spring",
                          month>5 & month<=8 ~"summer",
                          month>8 & month<=11 ~"autumn",
                          month>11 | month<3 ~"winter"),
         )%>%
  group_by(sitename,Year,season) %>%
  dplyr::summarise(temp=mean(temp_day_fluxnet2015,na.rm=T),
                   prec=sum(prec_fluxnet2015,na.rm = T), 
                   vpd=mean(vpd_day_fluxnet2015,na.rm=T),
                   ppfd=mean(PPFD_IN_fullday_mean_fluxnet2015,na.rm=T),
                   .groups = "drop"
  )
df_sum_yearly_1_2<-df_meteo_1_rev %>%
  #separate the temperature in different season
  mutate(month=month(date),
         season=case_when(month>=3 & month<=5 ~"spring",
                          month>5 & month<=8 ~"summer",
                          month>8 & month<=11 ~"autumn",
                          month>11 | month<3 ~"winter"),
  )%>%
  mutate(season_2=ifelse(season=="spring"|season=="winter","spring-winter","not-spring-winter"))%>%
  group_by(sitename,Year,season_2) %>%
  dplyr::summarise(temp=mean(temp_day_fluxnet2015,na.rm=T),
                   prec=sum(prec_fluxnet2015,na.rm = T), 
                   vpd=mean(vpd_day_fluxnet2015,na.rm=T),
                   ppfd=mean(PPFD_IN_fullday_mean_fluxnet2015,na.rm=T),
                   .groups = "drop"
  )
df_sum_yearly_final<-list("yearly_season"=df_sum_yearly_1_1,
                          "yearly_2season"=df_sum_yearly_1_2)
#---
#summary site-years(different seasons) for site
#---
df_sum_season<-df_sum_yearly_1_1 %>%
  group_by(sitename,season) %>%
  summarise_at(vars(temp:ppfd),mean,na.rm=T)
df_sum_2season<-df_sum_yearly_1_2 %>%
  group_by(sitename,season_2) %>%
  summarise_at(vars(temp:ppfd),mean,na.rm=T)
df_sum_season_final<-list("season"=df_sum_season,
                          "2season"=df_sum_2season)

#save the data:
# file.path<-load.path1
# save(df_sum_yearly_final,file=paste0(file.path,"site_meteo_yearly_new.RDA"))
# save(df_sum_season_final,file=paste0(file.path,"site_meteo_new.RDA"))
#
#focus on one season
df_meteo_1_update<-df_sum_season_final$season%>%
  filter(sitename!="DE-Tha")
df_meteo_merge<-bind_rows(df_meteo_2,df_meteo_1_update)

#focus on two seasons(winter+spring)
# df_meteo_1_update<-df_sum_season_final$`2season`%>%
#   filter(sitename!="DE-Tha")
# df_meteo_2<-df_meteo_2_2season
# df_meteo_merge<-bind_rows(df_meteo_2,df_meteo_1_update) 

#----------------
#load the “overestimated spring days" info: 
#----------------
load.path_add<-"D:/Github/photocold_manuscript/data/event_length/"
load(paste0(load.path_add,"df_events_length.RDA"))
df_GPP_overperiod<-df_events_all %>%
  group_by(sitename)%>%
  dplyr::summarise(GPPover_length=round(mean(Over_days_length_T10,na.rm=T),0),
                   nyear=n()
                   )%>%
  mutate(site_flag=ifelse(GPPover_length<=20|is.na(GPPover_length),"Not_OverGPP","OverGPP"))
#load for the Velux sites
file.path<-"./data/EC_MeteoandFlux/site_meteo/"
load(paste0(file.path,"df_event_length_Velux_sites.RDA")) # 值有些高估-->相对于Beni上一版的数据
df_GPP_overperiod_all<-bind_rows(df_GPP_overperiod,df_GPP_overperiod_Velux_sites%>%
                               filter(sitename=="CH-Dav"))
#----------------
#load the “site" info including PFT etc.
#----------------
file.path<-"D:/Github/photocold_manuscript/data-raw/raw_data/sites_info/"
load(paste0(file.path,"Available_sites_info.RDA"))
df_sites_avai<-rbind(df_sites_avai,
                     c("CH-Dav",9.86,46.82,1639,1997,2020,"ENF",FALSE,NA,"Dfc",
                       "Evergreen Needleleaf Forest","Evergreen Needleleaf Forest"))
#----------------
#merge meteo, GPP_over info, and site_info:
#----------------
#calcualte the mean meteo 
df_meteo_merge_season<-df_meteo_merge %>%
  filter(season=="spring")%>%
  # filter(season=="winter")%>%
  # filter(season_2=="spring-winter")%>%
  group_by(sitename)%>%
  summarise_at(vars(temp:ppfd),mean,na.rm=T)

df_merge<-left_join(df_meteo_merge_season,df_GPP_overperiod_all)
df_merge_final<-left_join(df_merge,df_sites_avai)%>%
  mutate(GPPover_length = ifelse(is.nan(GPPover_length), NA, GPPover_length))
#################
##plotting
#################
#some filter:
df_merge_final_1<-df_merge_final%>% 
 filter(nyear>2)

#mean (spring Ta)-->有些规律
t<-dplyr::ungroup(df_merge_final_1) %>%
  dplyr::filter(igbp_land_use=="Evergreen Needleleaf Forest")
theme_common<-theme_classic()+
  theme(
        # legend.position = "none",
        legend.background = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=20)
  )
df_plot<-dplyr::ungroup(df_merge_final_1) %>%
  dplyr::filter(igbp_land_use=="Evergreen Needleleaf Forest")%>%
  # dplyr::filter(classid=="ENF")%>%
  # dplyr::filter(koeppen_code=="Dfc")%>%
  ggplot(aes(x=temp,y=ppfd,col=site_flag, fill=site_flag))+
  geom_point()+
  #level:confidential interval
  stat_ellipse(alpha = 0.2, geom = "polygon",level = 0.7,type = "t")+ 
  #adding the site location of CH-Dav, DE-Tha
  geom_point(aes(x=temp,y=ppfd),
             data=df_merge_final %>%
               filter(sitename=="CH-Dav")
             # %>%filter(season=="spring")
             ,
             col=adjustcolor("red",0.5),size=6
             )+
  geom_point(aes(x=temp,y=ppfd),
             data=df_merge_final %>%
               filter(sitename=="DE-Tha")
             # %>%filter(season=="spring")
             ,
             col=adjustcolor("orange",0.5),size=6
  )+
  scale_color_manual(values = c("Not_OverGPP"=adjustcolor("orange",0.5),
                                "OverGPP"=adjustcolor("red",0.5)),
                     labels=c(expression(Site[0]),expression(Site[DSPR])))+
  scale_fill_manual(values = c("Not_OverGPP"=adjustcolor("orange",0.4),
                                "OverGPP"=adjustcolor("red",0.4)),
                    labels=c(expression(Site[0]),expression(Site[DSPR])))+
  xlab(expression(paste("Spring Ta (", "°C)")))+
  ylab(expression(paste("Spring PAR (",mu*"mol m"^-2,"s"^-1,")")))+
  #adding sitename:
  annotate(geom = "text",x=c(3.5,8.5),y=c(450,350),
           label=c("CH-Dav","DE-Tha"),size=6)+
  theme_common+
  theme(legend.position = c(0.85,0.85))
print(df_plot)

#----------------
#save the plot:
#----------------
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig0_Sel_sites_meteo_distribution.png"),df_plot,
       height = 4,width = 8)

