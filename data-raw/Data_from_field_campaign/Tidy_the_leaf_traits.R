###############################################
##Aim: tidy the data from the field campaigns 
###############################################
library(readxl)
library(ggplot2)
library(tidyverse)
library(plyr)

#----------------------
#(1)load the data 
#----------------------
base_path<-"D:/EE_WSL/IMPACT_project/data_collection/"
#for site paths
Dav.path<-paste0(base_path,"Dav_Data/")
Tha.path<-paste0(base_path,"Tha_Data/")

#---------load the leaf traits data-----------
#For Davos:
df.Dav.mor<-c()
df.Dav.agg<-c()
#needle area and morphometric traits(need width and length)
leaf.mor.path<-paste0(Dav.path,"Leaf_traits/leaf_area/")
#aggregate leaf info:leaf area and leaf mass
leaf.agg.path<-paste0(Dav.path,"Leaf_traits/")
#
Campaigns<-c("C1","C2","C3","C4","C5","C6")
for (i in 1:length(Campaigns)) {
  #leaf area and morphometric traits
  Dav_temp1<-read_xlsx(paste0(leaf.mor.path,"DAV_leaf_area_agg.xlsx"),sheet =Campaigns[i])
  Dav_temp1<-Dav_temp1[-1,]  #remove the unit line
  df.Dav.mor<-rbind(df.Dav.mor,Dav_temp1)
  #
  Dav_temp2<-read_xlsx(paste0(leaf.agg.path,"Davos_LeafArea _ NeedleWeight.xlsx"),sheet =Campaigns[i])
  Dav_temp2<-Dav_temp2[-1,] #remove the unit line
  df.Dav.agg<-rbind(df.Dav.agg,Dav_temp2)
}

#For Tharandt:
df.Tha.mor<-c()
df.Tha.agg<-c()
#needle area and morphometric traits(need width and length)
leaf.mor.path<-paste0(Tha.path,"Leaf_traits/leaf_area/")
#aggregate leaf info:leaf area and leaf mass
leaf.agg.path<-paste0(Tha.path,"Leaf_traits/")
#
Campaigns<-c("C1","C2","C3","C4","C5","C6")
for (i in 1:length(Campaigns)) {
  #leaf area and morphometric traits
  Tha_temp1<-read_xlsx(paste0(leaf.mor.path,"THA_leaf_area_agg.xlsx"),sheet =Campaigns[i])
  Tha_temp1<-Tha_temp1[-1,]  #remove the unit line
  df.Tha.mor<-rbind(df.Tha.mor,Tha_temp1)
  #
  Tha_temp2<-read_xlsx(paste0(leaf.agg.path,"Tharandt_LeafArea _ NeedleWeight.xlsx"),sheet =Campaigns[i])
  Tha_temp2<-Tha_temp2[-1,] #remove the unit line
  df.Tha.agg<-rbind(df.Tha.agg,Tha_temp2)
}

#----------------------
#(2)Tidy leaf traits together in each traits
#-----------------------
tidy_fun<-function(df.mor,df.agg){
  # df.mor<-df.Dav.mor
  # df.agg<-df.Dav.agg
  
  ##re-tidy the data for df.mor:
  df.mor_proc<-df.mor %>%
    mutate(sample_ID=toupper(substr(`Picture file`,1,11)),
           Measurement=toupper(substr(`Picture file`,13,15)))%>%
    mutate(Measurement=if_else(Measurement=="AMA","Amax",Measurement))%>%
    select(sample_ID,Object_number,Measurement,Total_area:Average_width)
  names(df.mor_proc)<-c("sample_ID","Object_number","Measurement",
                        "ImageJ_total_area","ImageJ_average_area",
                        "ImageJ_total_length","ImageJ_average_length",
                        "ImageJ_total_width","ImageJ_average_width")
  #re-tidy for df.agg:
  df.agg_proc<-df.agg %>%
    mutate(Measurement=if_else(Measurement=="Anet / Amax","Amax",Measurement))
  #merge two dfs:
  df.final<-left_join(df.agg_proc,df.mor_proc)
  #
  return(df.final)
}
#
df.Dav.tidy<-tidy_fun(df.Dav.mor,df.Dav.agg)
df.Tha.tidy<-tidy_fun(df.Tha.mor,df.Tha.agg)
#----------------------
#(3)merge the datasets and save data
#-----------------------
#
df.traits<-rbind(df.Dav.tidy,df.Tha.tidy)
df.traits<-as.data.frame(df.traits)

#save the data:
save.path<-"./data/"
save(df.traits,file = paste0(save.path,"Leaf_traits.data.RDA"))






  