library(ggplot2)
library(ggmap)
require(rgdal)
require(rgeos)
require(maptools)
require(dplyr)
library(tidyr)
library(tmap)

#library(ggmap)
#library(mapproj)
library(mapdata)
library(rworldmap)
library(rworldxtra)
library(colorRamps)
library(graphics)
library(jpeg)
 # map <- get_map(location = 'Europe', zoom = 5)-->need to get a register key in google
 # Google now requires an API key.
 # See ?register_google for details.
 # ggmap(map)

#---------------------------------------------
#(1)load the sample sites informatoin:
#---------------------------------------------
load.path<-"D:/Github/Velux_project/data-raw/"
library(readxl)
sampling_sites_info<-read_xlsx(paste0(load.path,"site_info_more_details.xlsx"))
#---------------------------------------------
#(2)plotting
#---------------------------------------------
library(RColorBrewer)
library(grDevices)
############
# map theme
############
#can refer:http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
theme_map <- 
  # theme_dark() +    # theme_minimal()
  theme(
    #add by YP:
    # panel.background = element_rect(fill = "gray60",
    #                                 colour = "gray60",
    #                                 size = 0.5, linetype = "solid"),
    # #add by YP:
    # plot.background = element_rect(fill="gray60"),
    #
    plot.title = element_text(hjust = 0, face="bold", size = 18),
    
    # legend.position = "right", # c(0.07, 0.35), #"left"
    # legend.key.size = unit(c(5, 1), "mm"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=10),
    
    # axis.line = element_blank(),
    # axis.text = element_blank(),
    # axis.title = element_blank(),
    
    # panel.grid.major = element_line(colour="black",size = 0.5,linetype = "solid"),
    panel.grid.minor = element_blank(),
    # plot.margin = unit( c(0, 0, 0, 5) , "mm")
  )

# define labels
lat.labels <- seq(30, 90, 30)
lat.short  <- seq(30, 90, 10)
lon.labels <- seq(-180, 180, 60)
lon.short  <- seq(-180, 180, 10)

a <- sapply( lat.labels, function(x) if (x>0) {parse(text = paste0(x, "*degree ~ N"))} else if (x==0) {parse(text = paste0(x, "*degree"))} else {parse(text = paste0(-x, "*degree ~ S"))} )
b <- sapply( lon.labels, function(x) if (x>0) {parse(text = paste0(x, "*degree ~ E"))} else if (x==0) {parse(text = paste0(x, "*degree"))} else {parse(text = paste0(-x, "*degree ~ W"))} )
#update in March, 2022
a<-expression("30°" ~ N, "60°" ~ N, "90°" ~ N)
b<-expression("30°"  ~ W, "120°" ~ W, "60°" ~ W,"0°",
              "60°" ~ E, "120°" ~ E, "180°" ~ E)

#---------------------------------------------
# 1. Create ggplot object
#---------------------------------------------
lonmin=-60
lonmax=150
latmin=30
latmax=90
#group=group-->results in the wrong map background:ask Beni's advices
#something need to be paid attention-->to make sure the plot looks right
#-->should set latmin=-90; latmax=90
#-->and also leave some place for latitude and longtitude-->set the limits in scale_x/y_continous adding or minus some numbers
gg <- ggplot() +
  theme_map+
  # background countries
  # geom_polygon(data=newmap, aes(long, lat, group=group), color=NA, fill='grey75') +
  # Coastline
  geom_path(data=coastsCoarse, aes(long, lat, group=group), color='black',size=1.02) +
  # 
  scale_x_continuous(expand = c(0,0), limits = c(-1+lonmin,lonmax+1), breaks = lon.labels, labels = b) +
  scale_y_continuous(expand = c(0,0), limits = c(-1+latmin,latmax+1), breaks = lat.labels, labels = a) +
  labs( x = "longtitude", y = "latitude")
#---------------------------------------------
# 2. add sites information
#---------------------------------------------
#----------
#plot1:the analyzed sites-->the sites Beni sent to me
#----------
library(ggrepel)  #add the labels
gg_sampling_sites<-gg+
  geom_point(data=sampling_sites_info,aes(x=Longitude,y=Latitude,col=Clim.,shape=Dominant.species),size=3)+
  geom_label_repel(data=sampling_sites_info,aes(x=Longitude,y=Latitude,label = Sitename,fill = Clim.),
                   color = 'black',max.overlaps = 50,label.size = 0.1,arrow = arrow(ends = "first",length = unit(0.1,"inch")),
                   size = 2.5) +
  theme(legend.position = "bottom")
gg_sampling_sites_update<-gg+
  geom_point(data=sampling_sites_info%>%filter(Sitename=="CH-Dav"|Sitename=="DE-Tha"),
             aes(x=Longitude,y=Latitude,col=Clim.,shape=Dominant.species),size=3)+
  geom_label_repel(data=sampling_sites_info%>%filter(Sitename=="CH-Dav"|Sitename=="DE-Tha"),
                   aes(x=Longitude,y=Latitude,label = Sitename,fill = Clim.),
                   color = 'black',max.overlaps = 50,label.size = 0.1,arrow = arrow(ends = "first",length = unit(0.1,"inch")),
                   size = 4) +
  theme_bw()+
  theme(legend.position = "bottom")


#
save.path<-"D:/Papers_prep/Velux_experiments/Paper_prep/plot/"
plot(gg_sampling_sites)
ggsave(file=paste0(save.path,"sites_sampling_sites_locations.png"),gg_sampling_sites_update,dev="png",width = 10,height=4)

