##########################################
#model the GPP in CH-Dav and DE-Tha after 2021
#with help from Fabian and Beni
##########################################
#using the site drivers data from Beni and
#refer to p-model vignette: https://geco-bern.github.io/rsofun/articles/
library(rsofun)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
#----------------------
#(1)load the data:
#----------------------
load.path<-"./data-raw/Model_GPP_after2021_usingPmodel/Data_sent_by_Beni/"
df_drivers<-read_rds(paste0(load.path,"rsofun_drivers_CH-Dav_DE-Tha.rds"))

#----------------------
#(2)running the model
#----------------------
#using the default parameters of vignette from Beni:
params_modl <- list(
  kphio              = 5.000000e-02, # chosen to be too high for demonstration
  kphio_par_a        =-2.289344e-03,
  kphio_par_b        = 1.525094e+01,
  soilm_thetastar    = 1.577507e+02,
  soilm_betao        = 1.169702e-04,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,
  tau_acclim         = 20,
  kc_jmax            = 0.41
)

# Run the model these parameters.
##there is some bugs-->Fabian found we need to set ccov=0(always sunny) to run the model successfully
df_output <- rsofun::runread_pmodel_f(
  df_drivers |> mutate(forcing = purrr::map(forcing, ~mutate(., ccov = 0))),
  par = params_modl
)


#plotting output
# Create data.frame for plotting
df_gpp_plot <- df_output |>
  tidyr::unnest(data) |>
  dplyr::select(sitename,date, gpp_mod = gpp) 

ggplot(data = df_gpp_plot) +
  geom_point(
    aes(x = date,y = gpp_mod,color = sitename)
  )
##save the plot:
save.path<-"./data/Simulate_GPP_Pmodel_in2sites/"
df_gpp_sim<-df_gpp_plot
save(df_gpp_sim,file = paste0(save.path,"Sim_GPP.RDA"))
#-------------------------------
##save the model gpp with meteos:
#-------------------------------
df_gpp_sim<-df_output |>
  tidyr::unnest(data) |>
  mutate(gpp_mod = gpp)%>%
  dplyr::select(sitename,date,gpp_mod)
df_gpp_drivers<-df_drivers|>
  tidyr::unnest(forcing) %>%
  mutate(gpp_obs = gpp, gpp=NULL)
# ##load the EC GPP from the 2 sites from 2021
# load.path<-"./data/EC_MeteoandFlux/"
# load(paste0(load.path,"df_daily_from_ICOS.RDA"))
# df.ts_GPP_Dav<-df_DD$Dav %>%
#   select(Date,GPP)%>%
#   mutate(date=Date,Date=NULL,
#          gpp_obs=GPP,GPP=NULL,
#          sitename=rep("CH-Dav"))%>%
#   filter(date>=as.Date("2021-01-01"))
# df.ts_GPP_Tha<-df_DD$Tha %>%
#   select(Date,GPP)%>%
#   mutate(date=Date,Date=NULL,
#          gpp_obs=GPP,GPP=NULL,
#          sitename=rep("DE-Tha"))%>%
#   filter(date>=as.Date("2021-01-01"))
# df.ts_GPP<-rbind(df.ts_GPP_Dav,df.ts_GPP_Tha)
#
df_gpp_sim_withdrivers_2021to2023<-left_join(x=df_gpp_drivers %>%
   filter(date>=as.Date("2021-01-01") & date<=as.Date("2023-12-31")),                          ,
   y=df_gpp_sim,by=c("sitename","date"))

#save the data:
save(df_gpp_sim_withdrivers_2021to2023,file = paste0(save.path,"Sim_GPP_full.RDA"))
#----------------------------------------------------
# Assess sensitivity of gpp to ccov: 
#------------------------------------------------------
#the results show there were barely no impact from ccov(changes from 0 to 1)
df_output_1 <- rsofun::runread_pmodel_f(
  df_drivers |> mutate(forcing = purrr::map(forcing, ~mutate(., ccov = 1))),
  par = params_modl
)
df_gpp_plot_1 <- df_output_1 |>
  tidyr::unnest(data) |>
  dplyr::select(sitename, date, gpp_mod = gpp) 

dat_ccov_sensitivity <- bind_rows(
  df_gpp_plot |> mutate(ccov = 0),
  df_gpp_plot_1 |> mutate(ccov = 1)
)

ggplot(data = dat_ccov_sensitivity |> tidyr::pivot_wider(values_from = "gpp_mod", names_from = "ccov")) +
  geom_point(aes(x=`0`, y=`1`))
ggplot() +
  geom_point(data = dat_ccov_sensitivity %>%filter(ccov==0),
    aes(x = date,y = gpp_mod,
      color = "0")) + 
  geom_line(data = dat_ccov_sensitivity %>%filter(ccov==1),
             aes(x = date,y = gpp_mod,
                 color = "1")) + 
  facet_grid(sitename~.)

#----------------------
#(3)comparison between modelled GPP and GPP from EC(2021-2024):
#----------------------
#load the "observed" data:
df_Meteo_andSoilVars<-readRDS(paste0("./data/Comprehensive_plot_data/Fig1/","df.Meteo_and_SoilVars.RDS"))
df_obs_sel<-df_Meteo_andSoilVars %>%
  filter(Date>=as.Date("2020-01-01"))
#
df_gpp_plot<-df_gpp_plot %>%
  mutate(Date=date,date=NULL)
df_merge<-left_join(df_obs_sel,df_gpp_plot,by=c("sitename","Date"))
##plotting
#every year:
df_merge %>%
  group_by(sitename)%>%
  ggplot()+
    geom_point(aes(x=Date,y=GPP,col="Obs."))+
    geom_line(aes(x=Date,y=gpp_mod,col="Mod."))+
  facet_grid(sitename~.)+
  theme_classic()

#merge years after 2021:
df_merge%>%
  filter(Date>=as.Date("2021-01-01"))%>%
  mutate(gpp_obs=GPP)%>%
  group_by(sitename, DoY) %>%
  summarise(
    gpp_obs_mean=mean(gpp_obs,na.rm=T),
    gpp_mod_mean=mean(gpp_mod,na.rm=T),
    gpp_obs_sd=sd(gpp_obs,na.rm=T),
    gpp_mod_sd=sd(gpp_mod,na.rm=T))%>%
  ggplot()+
  geom_line(aes(x=DoY,y=gpp_obs_mean),size=1.1)+
  geom_ribbon(
    aes(x = DoY, ymin = gpp_obs_mean - gpp_obs_sd, 
        ymax = gpp_obs_mean + gpp_obs_sd),
    fill = "grey",
    alpha = 0.4
  ) +
  geom_line(aes(x=DoY,y=gpp_mod_mean,col=sitename),size=1.1)+
  facet_grid(sitename~.)
