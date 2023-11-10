###############################################
##Aim: tidy the Li-cor data from the field campaigns
###############################################
library(readxl)
library(ggplot2)
library(tidyverse)
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
campaign.Amax.folder<-campaign.folder[grep("amax",campaign.folder)]
campaign.NPQ.folder<-campaign.folder[grep("NPQ",campaign.folder)]

#A.For Amax data..
df.Dav_Amax<-c()
#first for campaign 1-5:
for (i in 1:5) {
  ##For C1-C5:using the LI-cor8600:
    #using the function in readphoto r package:
    #for campaign 3: deleted the duplicated lines in file "2023-04-21-1417_logdata_c3_dav_t3_u_amax"
    Dav_temp<-read_bat_6800(paste0(Dav.path,"LiCor/",campaign.Amax.folder[i],"/ori_format_files/"), data_start = 66)
    df.Dav_Amax<-rbind(df.Dav_Amax,Dav_temp)
  
}

##For C6:using the LI-cor6400:
Dav_temp<-read_bat_6400(paste0(Dav.path,"LiCor/",campaign.Amax.folder[6],"/ori_format_files/"),
                          header_line = 17, data_start = 25)
  



###############
#For Tharandt:
###############
df.Tha<-c()
campaign.folder<-list.files(paste0(Tha.path,"LiCor/"))
#temporarily select the campaign 1-5:C1-C5
campaign.Amax.folder<-campaign.folder[grep("amax",campaign.folder)]
campaign.NPQ.folder<-campaign.folder[grep("NPQ",campaign.folder)]

#A.For Amax data..
df.Tha_Amax<-c()
for (i in 1:length(campaign.Amax.folder)) {
  #For C1-C5:using the LI-cor8600 from Arthur's group
  #For C6: using the LI-cor8600 from Tarek's group
  #for campaign 6: deleted the duplicated lines in file "2023-07-13-1104_logdata_c6_tha_t1_l_amax"
  #using the function in readphoto r package:
  if(i<=5){
  Tha_temp<-read_bat_6800(paste0(Tha.path,"LiCor/",campaign.Amax.folder[i],"/ori_format_files/"), data_start = 66)
  df.Tha_Amax<-rbind(df.Tha_Amax,Tha_temp)
  }
  if(i==6){
  Tha_temp<-read_bat_6800(paste0(Tha.path,"LiCor/",campaign.Amax.folder[i],"/ori_format_files/"), data_start = 61)
  df.Tha_Amax<-bind_rows(df.Tha_Amax,Tha_temp)
  }

}

#----------------------
#(2)load the measured leaf area:
#----------------------
load("./data/Leaf_traits.data.RDA")
#a.selected the leaf traits for two sites
df.Area<-df.traits %>%
  filter(Measurement=="Amax")%>%
  select(c(sample_ID,ImageJ_leaf_area))%>%
  mutate_at("ImageJ_leaf_area",as.numeric)%>%
  mutate(LArea=ImageJ_leaf_area,
         ImageJ_leaf_area=NULL)%>%
  mutate_at("LArea",round,4)
#b.unify the format of Amax data:
#remove the logdata in the string:
df.Tha_Amax$files<-gsub("_logdata","",df.Tha_Amax$files)


  mutate(sample_ID=substr(files,17,25))


