###############################################
##Aim: tidy the Li-cor data(NPQ) from the field campaigns
###############################################
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)
#modeling tools for C3 photosynthesis, as well as analytical tools for curve-fitting plant ecophysiology responses
library(photosynthesis) 
#install one package from LI-cor engineer:
# devtools::install_github("zhujiedong/readphoto")
library(readphoto)
#----------------------
#(1)load the data from LIcor 6800 and LIcor6400
#----------------------
base_path<-"D:/EE_WSL/IMPACT_project/data_collection/"
#for site paths
Dav.path<-paste0(base_path,"Dav_Data/")
Tha.path<-paste0(base_path,"Tha_Data/")
##notes: before load the data before, tidy the data in Amax folder in two separate folders according to their data format:
#put the data files into 1)"ori_format_files" and 2)"excel_format_files"

############
#For Davos:
############
df.Dav<-c()
campaign.folder<-list.files(paste0(Dav.path,"LiCor/"))
#temporarily select the campaign 1-5:C1-C5
# campaign.Amax.folder<-campaign.folder[grep("amax",campaign.folder)]
campaign.NPQ.folder<-campaign.folder[grep("NPQ",campaign.folder)]

#A.For Amax data..
df.Dav_NPQ_C1C5<-c()
#first for campaign 1-5:
for (i in 1:5) {
  ##For C1-C5:using the LI-cor8600:
    #using the function in readphoto r package:
    #for campaign 3: deleted the duplicated lines in file "2023-04-21-1417_logdata_c3_dav_t3_u_amax"
    Dav_temp<-read_bat_6800(paste0(Dav.path,"LiCor/",campaign.NPQ.folder[i],"/ori_format_files/"), data_start = 66)
    df.Dav_NPQ_C1C5<-rbind(df.Dav_NPQ_C1C5,Dav_temp)
  
}

##For C6:using the LI-cor6400:
#-->there is no specific NPQ measurements for Davos at 6th campaign

###############
#For Tharandt:
###############
df.Tha<-c()
campaign.folder<-list.files(paste0(Tha.path,"LiCor/"))
#temporarily select the campaign 1-5:C1-C5
# campaign.Amax.folder<-campaign.folder[grep("amax",campaign.folder)]
campaign.NPQ.folder<-campaign.folder[grep("NPQ",campaign.folder)]

#A.For Amax data..
df.Tha_NPQ<-c()
for (i in 1:length(campaign.NPQ.folder)) {
  #For C1-C5:using the LI-cor8600 from Arthur's group
  #For C6: using the LI-cor8600 from Tarek's group
  #for campaign 6: deleted the duplicated lines in file "2023-07-13-1104_logdata_c6_tha_t1_l_amax"
  #using the function in readphoto r package:
  if(i<=5){
  Tha_temp<-read_bat_6800(paste0(Tha.path,"LiCor/",campaign.NPQ.folder[i],"/ori_format_files/"), data_start = 66)
  df.Tha_NPQ<-rbind(df.Tha_NPQ,Tha_temp)
  }
  if(i==6){
  Tha_temp<-read_bat_6800(paste0(Tha.path,"LiCor/",campaign.NPQ.folder[i],"/ori_format_files/"), data_start = 56)
  df.Tha_NPQ<-bind_rows(df.Tha_NPQ,Tha_temp)
  }

}

#----------------------
#(2)load the measured leaf area:
#----------------------
load("./data/Leaf_traits.data.RDA")
#selected the leaf traits for two sites
df.Area<-df.traits %>%
  filter(Measurement=="Amax")%>%
  select(c(sample_ID,ImageJ_leaf_area))%>%
  mutate_at("ImageJ_leaf_area",as.numeric)%>%
  #name the practical value of leaf area to Sadj(-->corresponding to the S in the orignal name)
  mutate(S_adj=ImageJ_leaf_area,
         ImageJ_leaf_area=NULL)%>%
  mutate_at("S_adj",round,4)
#
df.Tha_Area<-df.Area[grep("THA",df.Area$sample_ID),]
df.Dav_Area<-df.Area[grep("DAV",df.Area$sample_ID),]

#---------------------------
#(3)unify the format of Amax data and readjust the measurements with update leaf area
#---------------------------
#source the datasets:
source("./R/Adj_leafA_6800.R")
source("./R/Adj_leafA_6400.R")
#---------
#Davos:
#---------
##1) For C1-C5: measured with LIcor8600:
df.Dav_Amax_C1C5$files<-gsub("_logdata","",df.Dav_Amax_C1C5$files)
#adding the sample_ID in the dataset:
df.Dav_Amax_C1C5$sample_ID<-toupper(substr(df.Dav_Amax_C1C5$files,17,27))
#merge the datasets:
df.Dav_Amax_C1C5<-left_join(df.Dav_Amax_C1C5,df.Dav_Area)
#recompute the data according to the udpated Area:
df.Dav_Amax_C1C5_adj<-recomp_6800_adjA(df.Dav_Amax_C1C5,S=df.Dav_Amax_C1C5$S_adj)
plot(df.Dav_Amax_C1C5$A,df.Dav_Amax_C1C5_adj$A,ylim=c(0,10),xlim=c(0,10))
abline(0,1,lty=1,col="blue")

##2) For C6: measured with LIcor6400:
df.Dav_Amax_C6$files<-tolower(paste0("2023-07-17_c6_dav_",substr(df.Dav_Amax_C6$files,13,16),"_amax"))
#convert "-" to "_"
df.Dav_Amax_C6$files<-gsub("-","_",df.Dav_Amax_C6$files)
#adding the sample_ID in the dataset:
df.Dav_Amax_C6$sample_ID<-toupper(substr(df.Dav_Amax_C6$files,12,22))
#merge the datasets:
df.Dav_Amax_C6<-left_join(df.Dav_Amax_C6,df.Dav_Area)
#recompute the data according to the udpated Area:
df.Dav_Amax_C6_adj<-recomp_6400_adjA(df.Dav_Amax_C6,S=df.Dav_Amax_C6$S_adj)
plot(df.Dav_Amax_C6$Photo,df.Dav_Amax_C6_adj$Photo,ylim=c(0,10),xlim=c(0,10))
abline(0,1,lty=1,col="blue")

##merge the C1-C5 and C6 data:
#only selected most important variables or known meaning varables
df.Dav_Amax_C6_adj_sel<-df.Dav_Amax_C6_adj %>%
  select(c(files,Obs,HHMMSS,Cond,Ci,Trmmol,VpdL,CTleaf,Area,
           BLCond,Tair,Tleaf,CO2R,CO2S,H2OR,H2OS,Flow,
           PARi,PARo,sample_ID,S_adj))%>%
##change the names-->change the names corresponding to LI6800:
  #refer the variable names in Licor8600 and 6400
  mutate(obs=as.numeric(Obs),hhmmss=HHMMSS,gsw=Cond,E=Trmmol,VPDleaf=VpdL,
         TleafEB=CTleaf,S=Area,gbw=BLCond,Qin=PARi)%>%
  mutate(Obs=NULL,HHMMSS=NULL,Cond=NULL,Trmmol=NULL,
         VpdL=NULL,CTleaf=NULL,Area=NULL,BLCond=NULL,PARi=NULL)
##
df.Dav_Amax_adj<-bind_rows(df.Dav_Amax_C1C5_adj,df.Dav_Amax_C6_adj_sel)
  
#---------
#Tharandt
#---------
#remove the logdata in the string:
df.Tha_Amax$files<-gsub("_logdata","",df.Tha_Amax$files)
#remove the "test" measurement in the dataset:
pos<-grep("_test",df.Tha_Amax$files)
df.Tha_Amax<-df.Tha_Amax[-pos,]
#adding the sample_ID in Amax dataset:
df.Tha_Amax$sample_ID<-toupper(substr(df.Tha_Amax$files,17,27))
#merge the datasets:
df.Tha_Amax<-left_join(df.Tha_Amax,df.Tha_Area)
#recompute the data according to the udpated Area:
df.Tha_Amax_adj<-recomp_6800_adjA(df.Tha_Amax,S=df.Tha_Amax$S_adj)


#----------------------
#(4)save the data
#----------------------
save.path<-"./data/LIcor/"
df.Amax<-list(Tha=df.Tha_Amax_adj,Dav=df.Dav_Amax_adj)
save(df.Amax,file=paste0(save.path,"df.Amax.RDA"))
