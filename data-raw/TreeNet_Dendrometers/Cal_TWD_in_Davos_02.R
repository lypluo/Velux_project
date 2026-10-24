######################################
#Aim: calculate the TWD for trees in Davos--data from Roman directly
######################################
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
  select(measure_point:sensor_active,site_name:country)

#---------------
#B.load the data Roman sent to me:
#---------------
#the original data is hourly:
stemgrowth.path<-"D:/data/Velux_shared_data/CH-Dav/TreeNet_Davos/Data_from_Roman/"
#tidy the data to daily:
filenames<-list.files(stemgrowth.path)
##10 min resolution--including twd and 
#twd_norm(refer Richard Peter paper: https://nph.onlinelibrary.wiley.com/doi/full/10.1111/nph.70266)
df.dendro_davos<-readRDS(paste0(stemgrowth.path,filenames[1]))

#---------------
#(3)merge the data and metedata for switzerland
#---------------
metadata<-df.Metadata %>%
  select(series_id:tree_id,site_id,sensor_active,measure_point,site_name)
#only select the series ID that avaiable from the data from Mirko:
avaiable.pos<-unique(df.dendro_davos$series_id)
#
metadata.sel<-metadata %>%
  filter(series_id %in% avaiable.pos)%>%
  ##do not keep the duplicate rows
  distinct(series_id,.keep_all = TRUE)
#####convert the 10min data to daily data:
df.dendro_davos_daily<-df.dendro_davos %>%
  mutate(Date=date(ts),
    Hour=hour(ts)
    )%>%
  group_by(series_id,Date)%>%
  summarise(twd=mean(twd),
            twd_min=min(twd),
            twd_norm=mean(twd_norm)
            )
df.merge<-left_join(df.dendro_davos_daily,metadata.sel,by="series_id",keep = FALSE)

#check the year length of data
df.period<-df.merge %>%
  mutate(year=year(Date))%>%
  group_by(series_id)%>%
  summarise(year_start=min(year),year_end=max(year),
            Nyear=length(unique(year)))
#
df.final<-left_join(df.merge,df.period)

####save the data
#save to the disk as it is big size of data
save.path<-"D:/data/Velux_shared_data/CH-Dav/TreeNet_Davos/Data_from_Roman/Stemgrowth_daily_data/"
saveRDS(df.final,file = paste0(save.path,"daily_stemgrowth_withTWD.RDS"))

# select Davos data:
#change the twd to TWD
df.final_Dav<-df.final%>%
  mutate(TWD=twd,twd=NULL,
         TWD_min=twd_min,twd_min=NULL,
         TWD_norm=twd_norm,twd_norm=NULL
         )
save.path<-"D:/Github/Velux_project/data/Tree_growth/"
saveRDS(df.final_Dav,file = paste0(save.path,"Davos_daily_TWD_updated.RDS"))

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
