##########################################
#model the GPP in CH-Dav and DE-Tha after 2021
##########################################
#using the site drivers data from Beni and
#refer to p-model vignette: https://geco-bern.github.io/rsofun/articles/pmodel_use.html

library(rsofun)
library(dplyr)
library(tidyr)
library(ggplot2)
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
  tau_acclim         = 20.0,
  kc_jmax            = 0.41
)

# Run the model these parameters.
df_output <- rsofun::runread_pmodel_f(
  df_drivers,
  par = params_modl
)

#plotting output
# Create data.frame for plotting
df_gpp_plot <- df_output |>
  tidyr::unnest(data) |>
  dplyr::select(date, gpp_mod = gpp) 

ggplot(data = df_gpp_plot) +
  geom_point(
    aes(
      x = date,
      y = gpp_mod,
      color = "P-model"
    )
  )

