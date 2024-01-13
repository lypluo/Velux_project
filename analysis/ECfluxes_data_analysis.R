##--------------------------------------------------------
#Aim: checking EC fluxes in Dav and Tha
##--------------------------------------------------------
library(lubridate)
library(ggplot2)

#-------------
#(1)load the data
#-------------
load.path<-"./data/EC_MeteoandFlux/"
load(paste0(load.path,"df_daily_from_ICOS.RDA"))

df.Dav<-df_DD$Dav
df.Tha<-df_DD$Tha

#mrege data from two sites
df.Dav<-df.Dav %>%
  mutate(sitename="CH-Dav")
df.Tha<-df.Tha %>%
  mutate(sitename="DE-Tha")
df<-rbind(df.Dav,df.Tha)

#-------------------
#(2) tidy the data 
#-------------------
#---merge the data according to doy
df<-df %>%
  mutate(DoY=yday(Date),
         Year=year(Date)
  )
df_mean<-df%>%
  group_by(sitename,DoY)%>%
  summarise(GPP_mean=mean(GPP),NEE_mean=mean(NEE),LE_mean=mean(LE),
            SW_IN_mean=mean(SW_IN),PPFD_IN_mean=mean(PPFD_IN),
            TA_mean=mean(TA),VPD_mean=mean(VPD),P_mean=mean(P)
            )

#---------------------------
#3) making the demonstration plot
#---------------------------
#####fast checking
#Davos
df %>%
  filter(sitename=="CH-Dav")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=GPP))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)

df %>%
  filter(sitename=="CH-Dav")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=NEE))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)
#Tharandt
df %>%
  filter(sitename=="DE-Tha")%>%
  group_by(Year)%>%
  ggplot(aes(x=DoY,y=GPP))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Year)



