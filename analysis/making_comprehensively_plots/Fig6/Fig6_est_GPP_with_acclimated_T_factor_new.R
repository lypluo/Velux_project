########################################################
#Aim: to improve the GPP estimation by adopting the acclimated factor
#considering the Tmin
########################################################
#checking the GPP estimation in CH-Dav
#
library(tidyverse)
library(dplyr)
library(lubridate)
library(phenopix)
library(cowplot)
library(ggpmisc)
#----------------------
#(1)load the data
#----------------------
##load the df.merge data (2020 and before):
load.path<-"./data/Comprehensive_plot_data/Fig5/"
load(paste0(load.path,"df.merge.RDA"))
##load the df.merge data (after 2020):
load.path<-"./data/Simulate_GPP_Pmodel_in2sites/"
load(paste0(load.path,"Sim_GPP_full.RDA"))
#keep data after the 2020 as the same format as before 2020:
df_gpp_sim_withdrivers_2021to2023<-df_gpp_sim_withdrivers_2021to2023 %>%
  mutate(prec=rain,doy=yday(date))
#
df.merge<-bind_rows(df.merge,df_gpp_sim_withdrivers_2021to2023)
#----------------------
#(2)optimilized the temperature modifier f_Ts 
#by refer Luo et al., 2023
#----------------------
#source the function:
source(paste0("./R/model_fT_rev.R"))

#--------------------------------------------------------------
#(2) retreive the optimized parameter for the selected sites
#--------------------------------------------------------------
#------------------
#set initial value
#------------------
par <- c("tau"=5,"X0"=-10,"Smax"=5,"k"=1)
#
lower=c(1,-10,5,0)
upper=c(25,10,25,2)

#------------------
#set cost function
#------------------
# run model and compare to true values
# returns the MSE
cost <- function(data,par) {
  
  scaling_factor <- data %>%
    # group_by(sitename) %>%
    do({
      scaling_factor <- f_Ts_rev(
        .,
        par
      )
      
      data.frame(
        sitename = .$sitename,
        date = .$date,
        scaling_factor = scaling_factor
      )
    })
  
  df <- left_join(data, scaling_factor)
  #rmse
  # rmse <- sqrt(
  #   sum(
  #     (df$gpp - df$gpp_mod * df$scaling_factor)^2)
  #   )/nrow(df)
  #mse:mean square error
  mse<-mean((df$gpp - df$gpp_mod * df$scaling_factor)^2,na.rm=T)
  #mae:mean absolute error:
  # mae<-sum(abs(df$gpp - df$gpp_mod * df$scaling_factor))/nrow(df)
  # This visualizes the process,
  # comment out when running for real
  # plot(df$gpp, type = 'p',ylim=c(0,12))
  # lines(df$gpp_mod, col = "red")
  # lines(df$gpp_mod * df$scaling_factor, col = "blue",cex=1.2)
  # Sys.sleep(0.1)
  
  return(mse)
}

#------------------------------------------
#normalized the GPP-->for each site,
#normalized the "gpp" and "gpp_mod" through their 90 percentiles
#------------------------------------------
#I should use the same value to normlize the gpp and gpp_mod:
gpp_P95<-df.merge %>%
  group_by(sitename) %>%
  dplyr::summarise(gpp_norm_p95=quantile(c(gpp_obs,gpp_mod),0.95,na.rm=T))
#
df_merge.new<-left_join(df.merge,gpp_P95,by="sitename")
df_merge.new<-df_merge.new %>%
  mutate(gpp=gpp_obs/gpp_norm_p95,gpp_mod=gpp_mod/gpp_norm_p95)
#
sel_sites<-unique(df_merge.new$sitename)
# optimize for each site-->Oct,2025: currently using the optimilized paramters
# calibrated using the data before 2020
# library(tictoc)#-->record the parameterization time
# library(GenSA)
# tic("start to parameterize")
# par_mutisites<-c()
# for(i in 1:length(sel_sites)){
#   df_sel<-df_merge.new %>%
#     dplyr::filter(sitename==sel_sites[i])
# 
#   optim_par <- GenSA::GenSA(
#   par = par,
#   fn = cost,
#   data = df_sel,
#   lower = lower,
#   upper = upper,
#   control = list(max.call=5000))$par
# 
#   print(i)
#   par_mutisites[[i]]<-optim_par
# }
# print("finish parameterization")
# toc()
# #
# names(par_mutisites)<-sel_sites
# print(par_mutisites)
# # save the optimized data
# save(par_mutisites,
#  file = paste0("./data/Comprehensive_plot_data/Fig5/model_parameters/parameters_MAE_newfT/",
#  "optim_par_run5000_eachsite_new.rds"))

#--------------------------------------------------------------
#(3) compare the gpp_obs, ori modelled gpp, and gpp modelled using optimated parameters
#--------------------------------------------------------------
load(paste0("./data/Comprehensive_plot_data/Fig5/model_parameters/parameters_MAE_newfT/",
            "optim_par_run5000_eachsite_new.rds"))
#a.get the stress factor(calibration factor) for each site
df_final<-c()
for (i in 1:length(sel_sites)) {
  df_sel<-df_merge.new %>%
    dplyr::filter(sitename==sel_sites[i])
  
  scaling_factors <- df_sel %>%
    # group_by(sitename, year) %>%
    do({
      # scaling_factor <- f_Ts_rev(.,par_mutisites[[i]])
      scaling_factor <- f_Ts_rev(.,par_mutisites[[i]])
      data.frame(
        sitename = .$sitename,
        date = .$date,
        scaling_factor_optim = scaling_factor
      )
    })
  df_sel <- left_join(df_sel, scaling_factors)
  
  #merge different sites:
  df_final<-rbind(df_final,df_sel)
}

#-----------------------------
#need to back-convert the normalized gpp to gpp
#-----------------------------
df_final_new<-df_final %>%
  mutate(gpp=gpp*gpp_norm_p95,
         gpp_mod=gpp_mod*gpp_norm_p95)

#b.make evaluation plots
#!!first need to merge the modelled gpp from different sources:
df_final_new$year<-lubridate::year(df_final_new$date)
df_final_new<-df_final_new %>%
  mutate(gpp_obs_recent=gpp,
         gpp_obs_old=gpp_obs,
         gpp_mod_recent_ori=gpp_mod,
         gpp_mod_recent_optim=gpp_mod*scaling_factor_optim,
         gpp=NULL,
         gpp_obs=NULL,
         gpp_mod=NULL)

#--------------------------
#(4).modelled and observed gpp:scatter plots
#-------------------------
#load the heatmap function
source("./R/analyse_modobs2.R")
plot_modobs_general<-c()
df_modobs<-c()
for(i in 1:length(sel_sites)){
  
  df_modobs_each<-df_final_new %>%
    filter(sitename==sel_sites[[i]])%>%
    dplyr::select(sitename,date,gpp_obs_old,gpp_obs_recent,
           gpp_mod_recent_ori,gpp_mod_recent_optim) %>%
    mutate(gpp_obs=gpp_obs_old,
           gpp_obs_add=gpp_obs_recent,
           gpp_mod_recent_ori=gpp_mod_recent_ori,
           gpp_mod_recent_optim=gpp_mod_recent_optim) %>%
    mutate(gpp_obs_old=NULL,gpp_obs_add=NULL
           )
  #
  df_modobs<-rbind(df_modobs,df_modobs_each)
  
  # #scatter plots to compare the model and observation gpp
  # gpp_modobs_comp1<-df_modobs_each %>%
  #   analyse_modobs2("gpp_mod_recent_ori", "gpp_obs", type = "heat")
  #   # ylim(-5,15)+
  #   # xlim(-5,15)
  # gpp_modobs_comp2<-df_modobs_each %>%
  #   analyse_modobs2("gpp_mod_recent_optim", "gpp_obs", type = "heat")
  # # add the site-name:
  # gpp_modobs_comp1$gg<-gpp_modobs_comp1$gg+
  #   geom_abline(intercept=0, slope=1, linetype="dotted") +
  #   annotate(geom="text",x=15,y=0,label=sel_sites[i])
  # gpp_modobs_comp2$gg<-gpp_modobs_comp2$gg+
  #   annotate(geom="text",x=15,y=0,label=sel_sites[i])
  

  # #merge two plots
  # evaulation_merge_plot<-plot_grid(gpp_modobs_comp1$gg,
  #                                  gpp_modobs_comp2$gg,gpp_modobs_comp3$gg,
  #                                  widths=15,heights=4,
  #                                  labels = "auto",ncol =3,nrow = 1,label_size = 12,align = "hv")
  # # plot(evaulation_merge_plot)
  # 
  # # put all the plots together:
  # plot_modobs_general[[i]]<-evaulation_merge_plot
}
# names(plot_modobs_general)<-sel_sites

#(2) For Seasonality
#b. Seasonal course for each sites in different PFTs:
# df_modobs %>%
#   mutate(doy = lubridate::yday(date)) %>%
#   group_by(sitename, doy) %>%
#   summarise(obs = mean(gpp_obs, na.rm = TRUE),
#             mod_old_ori=mean(gpp_mod_old_ori, na.rm = TRUE),
#             mod_recent_ori=mean(gpp_mod_recent_ori, na.rm = TRUE),
#             mod_recent_optim=mean(gpp_mod_recent_optim,na.rm = TRUE)) %>%
#   pivot_longer(c(obs,mod_old_ori,mod_recent_ori,mod_recent_optim), names_to = "Source", values_to = "gpp") %>%
#   ggplot(aes(doy, gpp, color = Source)) +
#   geom_line() +
#   scale_color_manual(values = c("mod_old_ori" = "red","mod_recent_ori"="steelblue2",
#                                 "mod_recent_optim" = "orange", "obs" = "black"),
#                      labels = c("Old P-model","Recent Ori P-model", "Recent Optim P-model","Obs.")) +
#   labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
#        x = "Day of year") +
#   facet_wrap(~sitename)
#update using original p-model
df_plot<-df_modobs %>%
  mutate(doy = lubridate::yday(date)) %>%
  #select CH-Dav
  filter(sitename=="CH-Dav")

###update in April, 2025:add estimated GPP using PRI:
##update in Oct,2025: updated the estimated GPP from PRI
load.path<-"./data/Comprehensive_plot_data/Fig6/"
load(paste0(load.path,"df_final_add_GPP_est_usePRI.RDA"))
df_add<-df_final_add_model %>%
  dplyr::select(Date,GPP,gpp_mod,GPP_est_with_cold_PRI,GPP_est_with_frost_PRI,doy,PRI_norm,LUE)%>%
  mutate(gpp_obs=GPP,GPP=NULL,
         date=Date,Date=NULL,
         sitename="CH-Dav"
         )
df_plot_20212023<-df_plot %>% filter(date>=as.Date("2021-01-01"))
df_plot_new<-left_join(df_add,df_plot_20212023%>%
                         mutate(gpp_obs=NULL,doy=NULL),by=c("sitename","date"))
##
#get the estimated period between 2021-2023:
#source the functions:
source("./R/separate_norm_GPPmismatch_period_trs_diff0_3SD.R")
##
library(dplyr)
#
df_plot_pheno<-df_plot_new 
df_andPlot_CH_Dav<-sep_data_indiffY_sepMismatch(df_plot_pheno,
                                                0.05,"Dfc","ENF","CH-Dav",c(2021:2023))

length(df_andPlot_CH_Dav$p_isevent)
p_diffYears<-df_andPlot_CH_Dav$p_isevent
p_merge_1<-plot_grid(p_diffYears[[1]],p_diffYears[[2]],p_diffYears[[3]],
                     # p_diffYears[[4]],p_diffYears[[5]],p_diffYears[[6]],
                     # p_diffYears[[7]],p_diffYears[[8]],p_diffYears[[9]],
                     # p_diffYears[[10]],p_diffYears[[11]],p_diffYears[[12]],
                     labels = "auto",ncol=3,label_size = 12,align = "hv")
p_pheno<-df_andPlot_CH_Dav$pos_agg
round(colMeans(p_pheno),0)
#sos10-->64;peak-->147
#testing the relationship between GPP bias and LUE
df_plot_test<-df_plot_new %>%
  mutate(gpp_bias=gpp_mod - gpp_obs,doy=yday(date))%>%
  ##between long time sos and before peak(181-->visually check)
  filter(doy>=61&doy<=181)
#lm regression test:
t_lm1<-lm(data=df_plot_test,
          gpp_bias ~ PRI_norm)
t_lm2<-lm(data=df_plot_test,
          gpp_mod ~ PRI_norm)
t_lm3<-lm(data=df_plot_test,
          gpp_obs ~ PRI_norm)
summary(t_lm1)
summary(t_lm2)
summary(t_lm3)

##adding p-value
P_test1<-df_plot_test %>%
  ggplot(aes(x=PRI_norm,y=gpp_bias),col="grey")+
  geom_point()+
  geom_smooth(data=df_plot_test,
              method = "lm",
              se=TRUE,fill=adjustcolor("black",0.4),color="black")+
  stat_poly_eq(data=df_plot_test,
               geom = "text",position = "identity",
               col="black",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = 0.3, label.y = 10)+
  annotate(geom = "text",x=0.2,y=8.8,
           label = paste0("p < 0.001"),col="black",size=6)+
  ylab(expression("GPP Bias"*" ("*mu*"mol m"^-2*"s"^-1*")"))+
  xlab("Norm PRI")+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = c(0.1,0.9),
    axis.text = element_text(size=16),
    axis.title = element_text(size=20))
P_test2<-df_plot_test %>%
  ggplot(aes(x=PRI_norm,y=gpp_mod),col="grey")+
  geom_point()+
  geom_smooth(data=df_plot_test,
              method = "lm",
              se=TRUE,fill=adjustcolor("black",0.4),color="black")+
  stat_poly_eq(data=df_plot_test,
               geom = "text",position = "identity",
               col="black",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = 0.3, label.y = 15)+
  annotate(geom = "text",x=0.2,y=13.5,
           label = paste0("ns"),col="black",size=6)+
  ylab(expression("Orig. Modelled GPP "*" ("*mu*"mol m"^-2*"s"^-1*")"))+
  xlab("Norm PRI")+
  ylim(-5,15)+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = c(0.1,0.9),
    axis.text = element_text(size=16),
    axis.title = element_text(size=20))
P_test3<-df_plot_test %>%
  ggplot(aes(x=PRI_norm,y=gpp_obs),col="grey")+
  geom_point()+
  geom_smooth(data=df_plot_test,
              method = "lm",
              se=TRUE,fill=adjustcolor("black",0.4),color="black")+
  stat_poly_eq(data=df_plot_test,
               geom = "text",position = "identity",
               col="black",
               size=6,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = 0.3, label.y = 15)+
  annotate(geom = "text",x=0.2,y=13.5,
           label = paste0("p < 0.001"),col="black",size=6)+
  ylab(expression("Obs. GPP "*" ("*mu*"mol m"^-2*"s"^-1*")"))+
  xlab("Norm PRI")+
  ylim(-5,15)+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=16),
    legend.position = c(0.1,0.9),
    axis.text = element_text(size=16),
    axis.title = element_text(size=20))
####
p_PRI_GPP_bias<-plot_grid(P_test1,P_test2,P_test3,
          ncol=3,
          nrow=1,
          labels=c("(a)","(b)","(c)"))
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig6_supp_comp_between_PRI_GPPobs_oriGPPmod.png"),
       p_PRI_GPP_bias,
       width = 15,height=10
)
######
#making plots:
######
df_plot_final<-df_plot_new%>%
  group_by(sitename, doy) %>%
  dplyr::summarise(obs = mean(gpp_obs, na.rm = TRUE),
                   obs_sd=sd(gpp_obs,na.rm=TRUE),
                   mod_ori=mean(gpp_mod, na.rm = TRUE),
                   mod_sd=mean(gpp_mod,na.rm=TRUE),
                   mod_recent_ori=mean(gpp_mod_recent_ori, na.rm = TRUE),
                   mod_recent_optim=mean(gpp_mod_recent_optim,na.rm = TRUE),
                   mod_cold_PRI=mean(GPP_est_with_cold_PRI,na.rm=T),
                   mod_frost_PRI=mean(GPP_est_with_frost_PRI,na.rm=T),
                   ) %>%
  pivot_longer(c(obs,mod_recent_ori,mod_recent_optim,mod_cold_PRI,mod_frost_PRI), 
               names_to = "Source", values_to = "gpp")

##For original P-model:
df_final_1<-df_plot_final %>%
  filter(Source %in% c("obs","mod_recent_ori","mod_recent_optim"))%>%
  mutate(Source=factor(Source,levels=c("obs","mod_recent_ori","mod_recent_optim")))
p_with_adjT_season_plot<-df_final_1 %>%
  ggplot(aes(doy, gpp, color = Source)) +
  geom_ribbon(
    aes(x = doy, ymin = gpp - obs_sd, 
        ymax = gpp + obs_sd),
    fill="grey",data=df_final_1%>%filter(Source=="obs"),
    alpha = 0.2,
    col=adjustcolor("grey",0.2)
  ) +
  geom_line() +
  scale_color_manual("GPP sources",values = c("mod_recent_ori" = "red",
       "mod_recent_optim" = "dodgerblue", "obs" = "black"),
        labels = c("Observations","Orig. P-model","Accli. P-model")) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DOY (2021-2023)",
       title = "CH-Dav"
       ) +
  # facet_wrap(~sitename)+
  # annotate(geom = "text",x=20,y=15,label = "CH-Dav",size=8)+
  theme_classic()+
  theme(
    legend.background = element_blank(),
    legend.text = element_text(size=18),
    legend.key.size = unit(2, 'lines'),
    axis.title = element_text(size=24),
    axis.text = element_text(size = 20),
    text = element_text(size=24),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    # panel.background = element_rect(colour ="grey",fill="white"),
    # legend.background = element_blank(),
    legend.position = c(0.15,0.75)
  )+
  ylim(-3.5,15)+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"))

##For PRI model:
##A:for mod_cold_PRI
df_final_2<-df_plot_final %>%
  filter(Source %in% c("obs","mod_recent_ori","mod_recent_optim","mod_cold_PRI"))%>%
  mutate(Source=factor(Source,levels=c("obs","mod_recent_ori","mod_recent_optim","mod_cold_PRI")))
p_with_coldPRI_season_plot<-df_final_2 %>%
  ggplot(aes(doy, gpp, color = Source)) +
  geom_ribbon(
    aes(x = doy, ymin = gpp - obs_sd, 
        ymax = gpp + obs_sd),
    fill="grey",data=df_final_2%>%filter(Source=="obs"),
    alpha = 0.2,
    col=adjustcolor("grey",0.2)
  ) +
  geom_line() +
  scale_color_manual("GPP sources",values = c("mod_recent_ori" = "red",
                     "mod_recent_optim" = "dodgerblue",
                     "mod_cold_PRI" = "orange",
                     "obs" = "black"),
                     labels = c("Observations",
                                "Orig. P-model",
                                "Empirical Accli. P-model ",
                                "Cold. PRI-model")) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DOY (2021-2023)",
       title = "CH-Dav",
       ) +
  # facet_wrap(~sitename)+
  # annotate(geom = "text",x=20,y=15,label = "CH-Dav",size=8)+
  theme_classic()+
  theme(
    legend.background = element_blank(),
    legend.text = element_text(size=18),
    legend.key.size = unit(2, 'lines'),
    axis.title = element_text(size=24),
    axis.text = element_text(size = 20),
    text = element_text(size=24),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    # panel.background = element_rect(colour ="grey",fill="white"),
    # legend.background = element_blank(),
    legend.position = c(0.15,0.75)
  )+
  ylim(-3.5,15)+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"))

##B:for mod_frost_PRI
df_final_3<-df_plot_final %>%
  filter(Source %in% c("obs","mod_recent_ori","mod_recent_optim","mod_frost_PRI"))%>%
  mutate(Source=factor(Source,levels=c("obs","mod_recent_ori","mod_recent_optim","mod_frost_PRI")))
p_with_frostPRI_season_plot<-df_final_3 %>%
  ggplot(aes(doy, gpp, color = Source)) +
  geom_ribbon(
    aes(x = doy, ymin = gpp - obs_sd, 
        ymax = gpp + obs_sd),
    fill="grey",data=df_final_1%>%filter(Source=="obs"),
    alpha = 0.2,
    col=adjustcolor("grey",0.2)
  ) +
  geom_line() +
  scale_color_manual("GPP sources",values = c("mod_recent_ori" = "red",
                    "mod_recent_optim" = "dodgerblue", 
                    "mod_frost_PRI" = "orange",
                    "obs" = "black"),
                     labels = c("Observations","Orig. P-model",
                                "Empirical Accli. P-model ",
                                "Frost. PRI-model")) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DOY (2021-2023)",
       title = "CH-Dav"
       ) +
  # facet_wrap(~sitename)+
  # annotate(geom = "text",x=20,y=15,label = "CH-Dav",size=8)+
  theme_classic()+
  theme(
    legend.background = element_blank(),
    legend.text = element_text(size=18),
    legend.key.size = unit(2, 'lines'),
    axis.title = element_text(size=24),
    axis.text = element_text(size = 20),
    text = element_text(size=24),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    # panel.background = element_rect(colour ="grey",fill="white"),
    # legend.background = element_blank(),
    legend.position = c(0.15,0.75)
  )+
  ylim(-3.5,15)+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"))

##C.using all data:
p_with_season_plot<-df_plot_final %>%
  ggplot(aes(doy, gpp, color = Source)) +
  geom_ribbon(
    aes(x = doy, ymin = gpp - obs_sd, 
        ymax = gpp + obs_sd),
    fill="grey",data=df_plot_final%>%filter(Source=="obs"),
    alpha = 0.4,
    col=adjustcolor("grey",0.4)
  ) +
  geom_line() +
  geom_line(aes(doy, gpp),data=df_plot_final %>%filter(Source=="obs"),size=1.1,col="black") +
  scale_color_manual("GPP sources",values = c("mod_recent_ori" = "red",
                                              "mod_recent_optim" = "dodgerblue",
                                              "mod_cold_PRI" = "blue", "mod_frost_PRI" = "purple",
                                              "obs" = "black"),
                     labels = c("Observations",
                                "Orig. P-model",
                                "Accli. P-model",
                                "Cold. PRI-model","Frost.PRI-model")) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DOY (2021-2023)",
       title = "CH-Dav"
       ) +
  # facet_wrap(~sitename)+
  # annotate(geom = "text",x=20,y=15,label = "CH-Dav",size=8)+
  theme_classic()+
  theme(
    legend.background = element_blank(),
    legend.text = element_text(size=18),
    legend.key.size = unit(2, 'lines'),
    axis.title = element_text(size=24),
    axis.text = element_text(size = 20),
    text = element_text(size=24),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    # panel.background = element_rect(colour ="grey",fill="white"),
    # legend.background = element_blank(),
    legend.position = c(0.15,0.75)
  )+
  ylim(-3.5,15)+
  theme(plot.title = element_text(hjust = 0.5, size = 24,face = "bold"))

##adding the overestimated period:
p_pheno
round(colMeans(p_pheno),0)
Dav_sos<-61 #or 65
Dav_peak<-165 #visual heck-->peak=165
rect.coord_isevent_2021=data.frame(x1=Dav_sos-60,
                                   x2=Dav_sos,
                                   x3=Dav_sos+60,
                                   y1=-Inf, 
                                   y2=Inf)
p_with_adjT_season_plot<-p_with_adjT_season_plot +
  # geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_sos),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_sos)+10,y=-1,label="sos",col="forestgreen",size=6)+
  geom_vline(xintercept=as.numeric(Dav_peak),col="black",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_peak)+10,y=-1,label="peak",col="black",size=6)

p_with_coldPRI_season_plot<-p_with_coldPRI_season_plot +
  # geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_sos),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_sos)+10,y=-1,label="sos",col="forestgreen",size=6)+
  geom_vline(xintercept=as.numeric(Dav_peak),col="black",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_peak)+10,y=-1,label="peak",col="black",size=6)

p_with_frostPRI_season_plot<-p_with_frostPRI_season_plot +
  # geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE, ## this is important-->geom_rect不继承p_LUE的x,y
  #           mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
  geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_sos),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_sos)+10,y=-1,label="sos",col="forestgreen",size=6)+
  geom_vline(xintercept=as.numeric(Dav_peak),col="black",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_peak)+10,y=-1,label="peak",col="black",size=6)
p_with_season_plot<-p_with_season_plot+
  geom_rect(data=rect.coord_isevent_2021,inherit.aes = FALSE,
            mapping=aes(xmin=x2, xmax=x3, ymin=y1, ymax=y2), fill="blue",alpha=0.1)+
  geom_vline(xintercept=as.numeric(Dav_sos),col="forestgreen",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_sos)+10,y=-1,label="sos",col="forestgreen",size=6)+
  geom_vline(xintercept=as.numeric(Dav_peak),col="black",size=1.1)+
  annotate(geom = "text",x=as.numeric(Dav_peak)+10,y=-1,label="peak",col="black",size=6)
##
# plot_grid(p_with_adjT_season_plot,p_with_coldPRI_season_plot,p_with_frostPRI_season_plot,ncol=1)
 
#--------------------------
#(5).estimated how much overestimation is reduced in the green-up period
#-------------------------
#sos = 61
#peak = 180  #visual heck-->peak=165
#
library(caret) #-->calculate the RMSE
# RMSE(y_pred, y_true)
df_bias<-df_plot_final %>%
  filter(doy>=61 & doy<=181)%>%
  pivot_wider(names_from = Source,values_from = gpp)%>%
  mutate(res_optim=mod_recent_optim - obs,
         res_coldPRI=mod_cold_PRI - obs,
         res_frostPRI=mod_frost_PRI - obs,
         res_ori=mod_recent_ori - obs,
         #RMSE
         rmse_optim=caret::RMSE(mod_recent_optim,obs,na.rm = T),
         rmse_coldPRI=caret::RMSE(mod_cold_PRI,obs,na.rm = T),
         rmse_frostPRI=caret::RMSE(mod_frost_PRI,obs,na.rm = T),
         rmse_ori=caret::RMSE(mod_recent_ori,obs,na.rm = T),
         )
df_bias_perc<-df_bias %>%
  dplyr::summarise(
    bias_optim=mean(res_optim),
    bias_coldPRI=mean(res_coldPRI),
    bias_frsotPRI=mean(res_frostPRI),
    bias_ori=mean(res_ori),
    obs_cum=mean(obs),
  )%>%
  mutate(perc_bias_optim=bias_optim/obs_cum*100,
         perc_bias_coldPRI=bias_coldPRI/obs_cum*100,
         perc_bias_frostPRI=bias_frsotPRI/obs_cum*100,
         perc_bias_ori=bias_ori/obs_cum*100)
df_bias_rmse<-df_bias %>%
  summarise(
    rmse_optim=mean(rmse_optim),
    rmse_coldPRI=mean(rmse_coldPRI),
    rmse_frsotPRI=mean(rmse_frostPRI),
    rmse_ori=mean(rmse_ori)
  )


###########
#adding reduced bias precentage into the figure:
p_with_adjT_season_plot<-p_with_adjT_season_plot +
  annotate(geom = "text",x=78,y=-2.5,label="Greenup Bias:",col="black",size=6)+
  annotate(geom = "text",x=100,y=-2.5,label="49.7%",col="red",size=6)+
  annotate(geom = "text",x=118,y=-2.5,label="-->",col="black",size=6)+
  annotate(geom = "text",x=130,y=-2.5,label="-11.8%",col="steelblue2",size=6)+
  xlim(0,250)

p_with_coldPRI_season_plot<-p_with_coldPRI_season_plot +
  annotate(geom = "text",x=78,y=-2.5,label="Greenup Bias:",col="black",size=6)+
  annotate(geom = "text",x=100,y=-2.5,label="49.7%",col="red",size=6)+
  annotate(geom = "text",x=118,y=-2.5,label="-->",col="black",size=6)+
  annotate(geom = "text",x=132,y=-2.5,label="-11.8%,",col="steelblue2",size=6)+
  annotate(geom = "text",x=150,y=-2.5,label="8.2%",col="orange",size=6)+
  xlim(0,250)

#save the plot and data:
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig6_supp2_model_comparison_model_ori_and_withcoldPRI.png"),
       p_with_coldPRI_season_plot,
       width = 15,height=10
       )
##Nov,2025: save the data as well
save.data.path<-"./data/Comprehensive_plot_data/Fig6/"
p_with_coldPRI_season_plot<-p_with_coldPRI_season_plot+
  theme(plot.title = element_blank())
p_with_coldPRI_season_plot_f<-plot_grid(p_with_coldPRI_season_plot,
                                        labels = c("(e)"),
                                        label_size = 20,
                                        nrow=1
                                        )
save(p_with_coldPRI_season_plot_f,
     file=paste0(save.data.path,"model_performance.RDA"))
##
ggsave(paste0(save.path,"Fig6_supp2_model_comparison_adjT.png"),
       p_with_adjT_season_plot,
       width = 15,height=10
)


#data
save.path<-"./data/Comprehensive_plot_data/Fig5/"
save(p_with_adjT_season_plot,
     file=paste0(save.path,"p_with_adjT_season_plot_new.RDA"))


