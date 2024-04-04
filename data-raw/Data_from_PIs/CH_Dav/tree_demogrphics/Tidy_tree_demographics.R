##################################################
#Aim: tidy the trees' demographic information from Davos 
##################################################
library(readxl)
library(tidyverse)
library(dplyr)
library(cowplot)
#----------------------
#(1)load and tidy the data
#---------------------
#data sent by Mana-->after checking the data-->it seems trees in CP01 and CP03 are
#might be close to eddy covariance tower-->use the trees' info in these two for
#further analysis:
load.path<-"D:/EE_WSL/IMPACT_project/data_collection/Dav_Data/Other_data_from_collabrators/trees_demographic_data/"
filename<-list.files(load.path)[grep(".xlsx",list.files(load.path))]
df.demo<-readxl::read_xlsx(paste0(load.path,filename))

#only using the trees' information in CP01 and CP03:
df.Dav.sel<-df.demo%>%
  filter(TREE_PLOT=="CP_01"|TREE_PLOT=="CP_03")%>%
  #only keep the information for the trees that in Minor diseases or healthy conditon
  filter(TREE_STATUS=="Minor diseases"|TREE_STATUS=="Healthy")%>%
  #remove the trees that with DBH=0
  filter(TREE_DBH>0)
#save the data:
save(df.Dav.sel,file = paste("./data/Sapflow/df.Dav.Demographics.RDA"))

##
p1<-df.Dav.sel %>%
  ggplot()+
  geom_histogram(aes(TREE_DBH),binwidth = 2)+
  theme_bw()
p2<-df.Dav.sel %>%
  ggplot()+
  geom_density(aes(TREE_DBH),col="red")+
  geom_vline(aes(xintercept=mean(TREE_DBH)),
             color="red", linetype="dashed")+
  theme_bw()
#save the plot:
p_merge<-plot_grid(p1,p2,nrow=1)
ggsave(p_merge,filename = "./manuscript/Ecosystem_observations/Davos_tree_demographic.png")
