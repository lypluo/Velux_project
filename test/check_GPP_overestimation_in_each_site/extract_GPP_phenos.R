##########################################
##extracting the phenology from GPP_obs
##########################################
# remotes::install_github("eco-hydro/phenofit")
library(phenofit)
library(data.table)
library(dplyr)
library(ggplot2)
#-----------------
#(1)load the data:
#-----------------
load.path<-"./test/test_datasets/"
load(paste0(load.path,"df.merge.RDA"))
head(df.merge)
#tidy the dateformat:
sites<-unique(df.merge$sitename)
#----------------------
#(2)To extract the phenology:
#trying to using phenofit R package: refer Kong et al., 2022:
#https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13870
#----------------------
Phenos_doy<-c()

for (i in 1:length(sites)) {
  sitename<-sites[i]
  #------------------
  #a.using check_input--> is used to pre-process the input VI time series;
  #------------------
  df.temp<-df.merge %>%
    filter(sitename==sitename)
  df_input<-check_input(t=df.temp$date,y=df.temp$gpp_obs,wmin = 0.2)
  plot_input(df_input)
  
  #------------------
  #b.using season_mov--> 
  #conducts the rough fitting and performs growing season division
  #------------------
  df_mov<-season_mov(df_input,list(rFUN="smooth_wWHIT",wFUN="wTSM",
                                   maxExtendMonth=12,r_max=0.1,r_min=0.05))
  plot_season(df_input,df_mov)
  
  #------------------
  #c.using curvefits--> 
  # implements the fine fitting and reconstructs the daily VI 
  #series in every growing season;
  #------------------
  #facing some probelm here...
  VI_fit<-curvefits(VI_input,VI_mov,
                    list(maxExtendMonth=3,minExtendMonth=1,minPercValid = 0.2,
                         methods = c("AG", "Zhang", "Beck", "Elmore")))
  
  #---------------------
  #d.get_pheno--> extracts the vegetation phenological metrics 
  #from the reconstructed daily VI time series
  #---------------------
  VI_pheno<-get_pheno(VI_fit,"Elmore",TRS=c(0.25,0.5))
  
  ## check the extracted phenology
  # pheno <- get_pheno(VI_fit[1:6], "Elmore", IsPlot = T,TRS=c(0.25,0.5))
  
  #-----------------------
  #e.tidy the extract Phenos
  #-----------------------
  #only using the Elmore as the fitting:
  temp_doy<-VI_pheno$doy$Elmore
  temp_doy$sitename<-rep(sitename,nrow(temp_doy))
  Phenos_doy<-rbind(Phenos_doy,temp_doy)
  
  temp_date<-VI_pheno$date$Elmore
  temp_date$sitename<-rep(sitename,nrow(temp_date))
  Phenos_date<-rbind(Phenos_date,temp_date)
  
  rm(temp_doy,temp_date)
  print(paste0("The",i,"site>>>>>>>"))
}


#further tidy:
Phenos_date_final<-Phenos_date %>%
  mutate(year=substr(flag,1,4),
         flag=NULL,origin=NULL)
Phenos_doy_final<-Phenos_doy %>%
  mutate(year=substr(flag,1,4),
         flag=NULL,origin=NULL)

#-----------------------
#f.save the data:
#-----------------------
save.path<-"./data/GIMMS/Phenology/"
write.csv(Phenos_date_final,paste0(save.path,"Pheno_Date.csv"))
write.csv(Phenos_doy_final,paste0(save.path,"Pheno_DoY.csv"))
