########################################################
#Aim: After discussion with Richard, we are aimed to get the better estimation of 
#stand-level of transpiration (sap flow)
#data sent by Ankit at end of Mar, 2024
########################################################
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)

#logic flow:
#1) to develop the realationship between Y(sap wood depth)--X(DBH); calculate the relative sap wood depth
#2) to develop the relationship between Y(Rleative sap flow)--X(Relative sapwood depth)

#----------------
#(0)load the data
#----------------
data.path<-'D:/EE_WSL/IMPACT_project/data_collection/Dav_Data/Other_data_from_collabrators/sap_flow_data/Data_sent_from_RichardPeter/'
#load the sapwood thickness(depth) data--general data for Norway spruce:
data_temp1<-readxl::read_xlsx(paste0(data.path,"Sapwood_thickness.xlsx"))
#load the sapwood thickness(depth) data--data specifically for Davos
data_temp2<-read.csv(file = paste0(data.path,"Sapwood_thickness_Davos.csv"),sep = ",")
##first check for the Davos data, if data_temp1 and data_temp2 has some redandecy:
t_temp1<-data_temp1%>%
  filter(Site=="DAV1840")%>%
  filter(Tree %in% paste0("B",c(28:37)))
#-->by visually the data-->the data_temp1 and data_temp2 are independant-->hence merge two data:







#load the sap flow density (SFD) data:
load.path<-"./data-raw/Data_from_PIs/CH_Dav/sap_flow/"
files<-list.files(load.path)
sel_files<-files[grep(".csv",files)]
df.sap<-read.csv2(paste0(load.path,sel_files[1]),sep = ",")
df.sap<-df.sap%>%
  mutate(sitename="CH-Dav")

#load the details for different trees:DBH and tree height
df.DBH_H<-read_excel(paste0(load.path,"TreeNumber_details.xlsx"))
df.DBH_H<-df.DBH_H%>%
  mutate(sitename=Site,Site=NULL)

#load the sap wood area info sent by Roman
df.A_sapwood<-read.csv2(paste0(load.path,sel_files[2]),sep = ",")
df.A_sapwood<-df.A_sapwood %>%
  mutate(TreeNumber=tree_id,tree_id=NULL,
         sitename="CH-Dav",site_id=NULL,
         tree_sapwood_thickness_cm=as.numeric(tree_sapwood_thickness_cm))
#
df.metadata<-left_join(df.DBH_H,df.A_sapwood)%>%
  #calculate the sap wood area(cm2)
  mutate(A_sapwood=pi*c(c(DBH/2)^2-(DBH/2 - tree_sapwood_thickness_cm)^2))%>%
  mutate(sapwood_thick=tree_sapwood_thickness_cm,
         tree_sapwood_thickness_cm=NULL)
#
df.sap.agg<-left_join(df.sap,df.metadata)%>%
  mutate(Date=as.Date(Date),
         SFDm=as.numeric(SFDm))

#----------------
#(1)test plotting
#----------------
#a.overview:
df.sap.agg %>%
  group_by(TreeNumber)%>%
  ggplot()+
  geom_point(aes(x=Date,y=SFDm))+
  facet_wrap(~TreeNumber)+
  ylab(expression("SFD (cm h"^-1*")"))

#-------------->
#b.to briefly check if the SFD density differs in trees with different DBHs:
#--------------->
df.sap.agg_Monthly<-df.sap.agg %>%
  mutate(Year=year(Date),Month=month(Date))%>%
  ##only select the data after 2021 when all the data seems stable
  filter(Year>=2021)%>%
  group_by(DBH,Month)%>%
  summarise(SFDm_m=mean(SFDm,na.rm=T))

#it seems there is no direct relationship between SFD vs DBH
df.sap.agg_Monthly%>%
  ggplot()+
  geom_point(aes(x=DBH,y=SFDm_m,col=as.factor(DBH)))

#-------------->
#c.to briefly check the realtionship between DBH and sap wood area
#--------------->  
df.metadata %>%
  ggplot()+
  geom_point(aes(x=DBH,y=A_sapwood))+
  stat_smooth(aes(x=DBH,y=A_sapwood),method = 'lm')
##
lm_fit<-lm(A_sapwood ~ DBH,data=df.metadata)
summary(lm_fit)
df.test1<-df.metadata %>%
  mutate(A_sapwood_lm=coef(lm_fit)[1]+DBH*coef(lm_fit)[2])

#
nls_fit <- nls(A_sapwood ~ a * DBH^b, data = df.metadata, start = list(a = 1, b = 1.5))
summary(nls_fit)
df.test2<-df.metadata %>%
  mutate(A_sapwood_nls=coef(nls_fit)[1]*DBH^coef(nls_fit)[2])
##finally use the lm_fit for the further analysis
plot(df.metadata$DBH,df.metadata$A_sapwood)
points(df.test1$DBH,df.test1$A_sapwood_lm,col="blue")
points(df.test2$DBH,df.test2$A_sapwood_nls,col="red")

#----------------
#(2)calculating the sapwood area and get the stand-level sapflow
#----------------
#-----
#a. calculate the sapwood area value for those trees that do not have direct measurements 
#-----
#by referring to the relationship constructed by (1c)

df.metadata<-df.metadata %>%
  mutate(A_sapwood_new=A_sapwood)%>%
  mutate(A_sapwood_new=ifelse(!is.na(A_sapwood_new),A_sapwood_new,
        coef(lm_fit)[1]+coef(lm_fit)[2]*DBH))

#updated datasets
df.sap_final<-left_join(df.sap.agg,df.metadata)

#--------
#b.calculate the mean sap flow(SF)
#--------
#load the tree demographic data:
load(paste0("./data/Sapflow/df.Dav.Demographics.RDA"))
df.demo<-df.Dav.sel%>%
  mutate(DBH_flag=case_when(
    TREE_DBH<20 ~ "low",
    TREE_DBH>=20 & TREE_DBH<=40 ~"mid",
    TREE_DBH>40 ~"high"
  ))
#summary the proporation for different DBH classes: low, mid, and high
df.demo_summary<-df.demo %>%
  group_by(DBH_flag)%>%
  summarise(prop=length(DBH_flag)/nrow(df.demo))

#only using the data from 2021 for this calculation:
df.new<-df.sap_final %>%
  mutate(DBH_flag=case_when(
    DBH<20 ~ "low",
    DBH>=20 & DBH<=40 ~"mid",
    DBH>40 ~"high"))
#to link DBH with their trees' demographics:
df.final<-left_join(df.new,df.demo_summary)%>%
  mutate(Year=year(Date))%>%
  filter(Year>=2021)%>%
  #convert sap to mm d-1
  #SFDm unit: cm h-1(confirmed with Ankit), A_sapwood_new = cm2
  #calcualte the sap flow:method 1: no weight of demographic info for different DBH classes
  mutate(
    #need to over nrow(df.metadata)-->calculate the weighted mean sap area 
    sap=SFDm*A_sapwood_new/nrow(df.metadata)*24*10*10^-4)

#aggregate different trees to calculate daily mean SF:
df.sap.daily<-df.final %>%
  select(Date,TreeNumber,sap,SFDm,A_sapwood_new,prop)%>%
  group_by(Date)%>%
  summarise(sap_m=mean(sap,na.rm=T),
            #calcuclate sap flow:method2:consideirng tree demogrphaic info for different DBH calesses
            sap_m_adj=sum(SFDm * A_sapwood_new*prop,na.rm = T)/sum(A_sapwood_new*prop,na.rm=T)
            )
df.sap.daily %>%
  ggplot()+
  geom_point(aes(x=Date,y=sap_m))+
  geom_point(aes(x=Date,y=sap_m_adj),col="red")+
  ylab(expression("Sap flow (mm d"^-1*")"))

#----------------
#(3)save the data
#----------------
df.all<-list(df.metadata,
             df.sap.daily)
names(df.all)<-c("metadata","sap.daily")
#save the data:
save.path<-"./data/Sapflow/"
save(df.sap.daily,file = paste0(save.path,"df.Davos.sap.RDA"))

#save the data for Weigeng's analysis:
df.sent<-df.sap.daily %>%
  select(Date,sap_m_adj)%>%
  mutate(sap_daily=sap_m_adj,sap_m_adj=NULL)
#save the data in csv:
write.csv(df.sent,file = paste0(save.path,"Davos_daily_sapflow.csv"))
