######################################
#Aim: calculate the TWD for trees in Davos--data from Mirko and calculate the TWD by myself:
######################################
library(dendRoAnalyst)  ## Rpackage that used to calculate the TWD:
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
#------------
#(1)load the data
#------------
#-------------------
#A.load the metadata that Roman sent me (the seriesID in the table link with
#the data Mirko sent to me)
#-------------------
load.path<-"D:/data/Upscale_project_data/From_TreeNet/Meta_data/"
df.Metadata<-read_excel(paste0(load.path,"Metadata.xlsx"),sheet = "CH.tree")
#select the variables
df.Metadata<-df.Metadata %>%
  select(measure_point:sensor_active)

#---------------
#B.load the data Mirko sent to me:
#---------------
#the original data is hourly:
stemgrowth.path<-"D:/data/Upscale_project_data/From_TreeNet/Data_sent_from_Mirko/daily_resolution_by_year/"
#tidy the data to daily:
filenames<-list.files(stemgrowth.path)
df.all<-c()
for(i in 1:length(filenames)){
  df.proc<-read.csv(paste0(stemgrowth.path,filenames[i]))
  
  #calculate the TWD-->using zero-growth appraoch:
  if(nrow(df.proc)>48){
    dendro.temp<-dendRoAnalyst::phase.zg(df.proc%>%select(ts,value),1)
    dendro.temp_2<-dendro.temp$ZG_phase %>%
      mutate(year=year(TIME),doy=yday(TIME))%>%
      dplyr::select(TIME,TWD:doy)
    dendro_daily<-dendro.temp_2 %>%
      mutate(Date=as.Date(TIME))%>%
      group_by(Date,year,doy)%>%
      #choose the maximum value for TWD and GRO for daily
      summarise(TWD=max(TWD,na.rm = T),
                GRO=max(GRO,na.rm = T))
    ##
    df.proc_temp<-df.proc %>%
      group_by(series_id,year,doy)%>%
      #start_gro: the value at the begining of the day;
      #end_gro:the value that in the end of the day;
      #dgro:daily growth-->the value of the end of the day
      #minus the values at the beginning of the day
      #amp_gro: maximum value - minimum value in one day
      summarise(start_gro=first(value),
                end_gro=last(value),
                min_gro=min(value),
                max_gro=max(value)
      )%>%
      ungroup()%>%
      mutate(dgro=end_gro-start_gro,
             amp_gro=max_gro-min_gro)
    df.proc_temp<-left_join(dendro_daily,df.proc_temp)
  }
  if(nrow(df.proc)<=48){
    df.proc_temp<-c()
  }
  
  #
  if (i==1){
    df.all<-df.proc_temp
  }
  if(i>1){
    df.all<-rbind(df.all,df.proc_temp)
  }
  rm(df.proc_temp)
}

#---------------
#(3)merge the data and metedata for switzerland
#---------------
metadata<-df.Metadata %>%
  select(series_id:tree_id,site_id,sensor_active,measure_point)
#only select the series ID that avaiable from the data from Mirko:
avaiable.pos<-unique(df.all$series_id)
#
metadata.sel<-metadata %>%
  filter(series_id %in% avaiable.pos)
df.merge<-left_join(df.all,metadata.sel,by="series_id")

#check the year length of data
df.period<-df.merge %>%
  group_by(series_id)%>%
  summarise(Nyear=length(unique(year)))
#
df.final<-left_join(df.merge,df.period)

####save the data
#save to the disk as it is big size of data
save.path<-"D:/data/Upscale_project_data/From_TreeNet/Data_sent_from_Mirko/Stemgrowth_daily_data/"
saveRDS(df.final,file = paste0(save.path,"daily_stemgrowth_withTWD.RDS"))

# select Davos data:
df.final_Dav<-df.final %>%
  filter(site_id==36)
save.path<-"D:/Github/Velux_project/data/Tree_growth/"
saveRDS(df.final_Dav,file = paste0(save.path,"Davos_daily_TWD.RDS"))

#simply check the data:
unique(df.final_Dav$series_id)
df.final_Dav%>%
  group_by(series_id)%>%
  ggplot(aes(x=Date,y=TWD))+
  geom_point()+
  facet_grid(.~series_id)
#
df.final_Dav %>%
  filter(series_id==221)%>%
  ggplot(aes(x=Date,y=TWD))+
  geom_point()
