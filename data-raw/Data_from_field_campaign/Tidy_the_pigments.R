###############################################
##Aim: tidy the lab measured pigments data from the field campaigns 
#Haoyu lead the measurements
###############################################
library(readxl)
library(ggplot2)
library(tidyverse)
library(plyr)

#----------------------
#(1)load the data 
#----------------------
base_path<-"D:/EE_WSL/IMPACT_project/data_collection/Pigment_data/"
df.Pigment<-read_xlsx(path=paste0(base_path,"Pigment_result_20231018.xlsx"))

#----------------------
#(2)tidy the data and keep consistent the variables names with other analysis 
#----------------------
df.Pigment<-df.Pigment %>%
  mutate(Date=Sampling_Date,ID=Label,
         CampaignNum=Campaign,sitename=Site,
         CartoCab_ratio=Car/c(Cha+Chb),
         Sampling_Date=NULL,Label=NULL,Campaign=NULL,Site=NULL,SampleID=NULL
         )
#save the data:
save.path<-"./data/"
save(df.Pigment,file = paste0(save.path,"Pigment.data.RDA"))
