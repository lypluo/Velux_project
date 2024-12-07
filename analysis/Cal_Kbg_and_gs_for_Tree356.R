########################################################
#Calculate the belowground hydraulic conductance(Kbg) & canopy conductance(gs) for the trees
########################################################
#refer the Anna's paper:
#The influence of soil temperature and water content on belowground hydraulic 
#conductance and leaf gas exchange in mature trees of three boreal species
library(cowplot)

#----------------------
#(1)load the data:
#----------------------
#-->currently, for Davos site

#load the dendrometer, sap flow data, and vpd data to calculate the Kbg and gs
#only Tree 356 have both dendrometer and sap flow that covers the period in 2023:
#A.dendrometer:um
load("./data/Tree_growth/df.dendro_T356_Daily.RDA")

#B.sap flow-->need to load the original data:
#unit:cm/hour
load.path<-"./data-raw/Data_from_PIs/CH_Dav/sap_flow/"
df.SFD<-read.csv(file=paste0(load.path,"Dav_SFD_2010_2023_Daily_15trees.csv"))
#selected Tree 356:(the data is the daily mean data that Ankit sent to me before)
#daily maximum data that will be ideally
df.SFD_T356<-df.SFD %>%
  filter(TreeNumber==356)%>%
  mutate(Tree_ID=TreeNumber,TreeNumber=NULL,
         Date=as.Date(Date))

#C. VPD data:
#load the meteological data from EC towers:
#from the test->the maximum VPD noramlly happens at 14-15, hence it kind of makes sense to 
# use the data in midday:10-14
load("./data/EC_MeteoandFlux/df_midday_from_ICOS.RDA")
#unit:bar
df.VPD<-df_midday$Dav_midday %>%
  select(Date,Hour,VPD)%>%
  group_by(Date)%>%
  summarise(VPD_max=max(VPD,na.rm = T))

#merge the data
df.merge_1<-left_join(df.dendro_T356_Daily,df.SFD_T356)
df.merge<-left_join(df.merge_1,df.VPD)

#----------------------
#(2)calculating the Kbg and canopy conductance
#----------------------
norm_var<-function(x){
  perc90<-quantile(x,0.9,na.rm=T)
  x_norm<-x/perc90
  
  return(x_norm)
}
df.final<-df.merge %>%
  mutate(K_bg=SFDm/delta_dendro_D,
         gs=SFDm/VPD_max)%>%
  #and adding the f_bg--normalized the K_bg
  mutate(f_bg=norm_var(K_bg))


df.final %>%
  filter(!is.na(SFDm))%>%
  # ggplot(aes(x=Date,y=SFDm))+
  # ggplot(aes(x=Date,y=VPD_max))+
  ggplot(aes(x=Date,y=delta_dendro_D))+
  geom_smooth()+
  geom_point()

#only keep the available data from 2021:
#test
df.test<-df.final %>%
  filter(!is.na(SFDm))%>%
  pivot_longer(cols = c("delta_dendro_D","VPD_max","SFDm"),
               names_to = "Focused_vars",
               values_to = "Value")
#
df.test %>%
  group_by(Focused_vars)%>%
  ggplot(aes(x=Date,y=Value))+
  geom_smooth()+
  geom_point()+
  facet_wrap(~Focused_vars,scales = "free_y")


#Kbg+fbg
p_Kbg<-df.final %>%
  filter(!is.na(SFDm))%>%
  ggplot(aes(x=Date,y=K_bg))+
  geom_smooth()+
  geom_point()+
  ylim(0,0.15)
p_fbg<-df.final %>%
  filter(!is.na(SFDm))%>%
  ggplot(aes(x=Date,y=f_bg))+
  geom_smooth()+
  geom_point()+
  ylim(0,1)

#gs
p_gs<-df.final %>%
  filter(!is.na(SFDm))%>%
  ggplot(aes(x=Date,y=gs))+
  geom_smooth()+
  geom_point()+
  ylim(0,1.5)

#
plot_grid(p_Kbg,p_gs,nrow=2)

