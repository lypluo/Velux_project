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
    #for campaign 3: deleted the duplicated lines in file "2023-04-21-1417_logdata_c3_dav_t3_u_npq"
    #for campaign 4: remove the additional text in npq files
    Dav_temp<-read_bat_6800(paste0(Dav.path,"LiCor/",campaign.NPQ.folder[i],"/ori_format_files/"), data_start = 66)
    df.Dav_NPQ_C1C5<-rbind(df.Dav_NPQ_C1C5,Dav_temp)
  
}

##For C6:using the LI-cor6400:
#-->there is no specific NPQ measurements for Davos at 6th campaign
df.Dav_NPQ<-df.Dav_NPQ_C1C5
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
  #For C5: remove the dupliated lines in npq files
  #for campaign 6: deleted the duplicated lines in file "2023-07-13-1104_logdata_c6_tha_t1_l_amax"
  #using the function in readphoto r package:
  if(i<=5){
  Tha_temp<-read_bat_6800(paste0(Tha.path,"LiCor/",campaign.NPQ.folder[i],"/ori_format_files/"), data_start = 66)
  df.Tha_NPQ<-rbind(df.Tha_NPQ,Tha_temp)
  }
  if(i==6){
  #change data type in df.Tha_NPQ data.frame:
  all_chr_vars<-c("files","date","hhmmss",
                  "LightAdaptedID","DarkAdaptedID","DarkPulseID","Geometry")
  pos_all<-match(all_chr_vars,names(df.Tha_NPQ))
  df.Tha_NPQ[,-pos_all]<-apply(df.Tha_NPQ[,-pos_all], 2,as.numeric)
   
  Tha_temp<-read_bat_6800(paste0(Tha.path,"LiCor/",campaign.NPQ.folder[i],"/ori_format_files/"), data_start = 56)
  df.Tha_NPQ<-bind_rows(df.Tha_NPQ,Tha_temp)
  }

}

#---------------------------
#(3)tidy the data format
#---------------------------
#---------
#Davos:
#---------
##1) For C1-C5: measured with LIcor8600:
df.Dav_NPQ$files<-gsub("_logdata","",df.Dav_NPQ$files)
#adding the sample_ID in the dataset:
df.Dav_NPQ$sample_ID<-toupper(substr(df.Dav_NPQ$files,17,27))

#---------
#Tharandt
#---------
#remove the logdata in the string:

df.Tha_NPQ$files<-gsub("_logdata","",df.Tha_NPQ$files)
#adding the sample_ID in NPQ dataset:
df.Tha_NPQ$sample_ID<-toupper(substr(df.Tha_NPQ$files,17,27))

#----------------------
#(3)save the data
#----------------------
save.path<-"./data/LIcor/"
df.NPQ<-list(Tha=df.Tha_NPQ,Dav=df.Dav_NPQ)
save(df.NPQ,file=paste0(save.path,"df.NPQ.RDA"))
