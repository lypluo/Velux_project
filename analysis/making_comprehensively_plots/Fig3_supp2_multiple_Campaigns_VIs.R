############################################
#Aim:to present the processed TIFF images from Drones
############################################
library(terra)
library(tidyverse)
library(dplyr)
library(cowplot)
library(patchwork)
#-------------------
#(1) load the data
#-------------------
load.path<-"D:/EE_WSL/IMPACT_project/Drones_data/ArcGIS_processing/Davos_test_Project/exported_tiff/"

#-------------------
#(2) checking the drones' data
#-------------------
#write the functions for the plotting
plot_function<-function(tiff.path,VI_name,VI_scale){
  # tiff.path<-load.path
  # VI_name<-"NDVI"
  # VI_scale<-"Landscape"
  
  df_agg<-c()
  for (i in 1:6){
    if(VI_scale=="Landscape"){
      rast_data<-rast(paste0(tiff.path,"C",i,"_final_",VI_name,".tif"))  
    }
    if(VI_scale=="Crown"){
      rast_data<-rast(paste0(tiff.path,"C",i,"_final_",VI_name,"_Crown.tif"))  
    }
    
    rast_df<-as.data.frame(rast_data,xy=TRUE)
    names(rast_df)<-c("x","y","VI")
    #
    if(VI_name=="NDVI"){
      df.proc<-rast_df %>%
        filter(VI>0 & VI<1)
    }
    if(VI_name=="PRI"){
      df.proc<-rast_df %>%
        filter(VI<1)%>%
        #after checking the data,limit the range of the data
        filter(VI!=0 & VI>-0.2 &VI<0.5)
    }
    p.proc<-ggplot(df.proc,aes(x=x,y=y))+
      geom_raster(aes(fill=VI))+
      scale_fill_viridis_c(option = "D") +
      coord_fixed()+
      labs(fill=VI_name)+
      theme_light()+
      theme(legend.text = element_text(size = 26),
            legend.title = element_blank(),
            legend.background = element_blank(),
            axis.text = element_text(size=24),
            axis.text.x = element_text(angle = 12.5),
            axis.title = element_text(size=26)
      )
    if(VI_name=="NDVI"){
      p.proc<-p.proc+
        scale_fill_gradientn(colors = c(
          "#440154","#482878","#3E4B8B","#31688E","#26828E",
          "#1F9B77","#5CDB5A","#B8DE29", "#FDE725"),
          limits=c(0,1))+
        # annotate(geom = "text",x=2784385,y=1187640,label="C5",size=10)+
        theme(axis.title = element_blank())
    }
    if(VI_name=="PRI"){
      p.proc<-p.proc+
        scale_fill_gradientn(colors = c(
          "#440154","#482878","#3E4B8B","#31688E","#26828E",
          "#1F9B77","#5CDB5A","#B8DE29", "#FDE725"),
          limits=c(-0.2,0.5))+
        # annotate(geom = "text",x=2784385,y=1187640,label="C5",size=10)+
        theme(axis.title = element_blank())
    }
    
    df_agg[[i]]<-p.proc
    rm(p.proc)
  }
  names(df_agg)<-paste0("C",1:6,"_",VI_name,"_",VI_scale)
  return(df_agg)
}

##
load.path<-"D:/EE_WSL/IMPACT_project/Drones_data/ArcGIS_processing/Davos_test_Project/exported_tiff/"
###############
### For Landscape NDVI
p_Landscape_NDVI<-plot_function(load.path,"NDVI","Landscape")
#
p_UAV_Eco_NDVI<-plot_grid(p_Landscape_NDVI[[1]],p_Landscape_NDVI[[2]],p_Landscape_NDVI[[3]],
          p_Landscape_NDVI[[4]],p_Landscape_NDVI[[5]],p_Landscape_NDVI[[6]],
          align = "hv",nrow=2,ncol=3,
          labels = c(paste0("C",1:6))
          )

### For PRI
p_Landscape_PRI<-plot_function(load.path,"PRI","Landscape")
#
p_UAV_Eco_PRI<-plot_grid(p_Landscape_PRI[[1]],p_Landscape_PRI[[2]],p_Landscape_PRI[[3]],
          p_Landscape_PRI[[4]],p_Landscape_PRI[[5]],p_Landscape_PRI[[6]],
          align = "hv",nrow=2,ncol=3,
          labels = c(paste0("C",1:6))
)

###############
### For crown NDVI
load.path<-"D:/EE_WSL/IMPACT_project/Drones_data/ArcGIS_processing/Davos_test_Project/exported_tiff/Crowns/"
p_Crown_NDVI<-plot_function(load.path,"NDVI","Crown")
#
p_UAV_Crown_NDVI<-plot_grid(p_Crown_NDVI[[1]],p_Crown_NDVI[[2]],p_Crown_NDVI[[3]],
          p_Crown_NDVI[[4]],p_Crown_NDVI[[5]],p_Crown_NDVI[[6]],
          align = "hv",nrow=2,ncol=3,
          labels = c(paste0("C",1:6))
          )

### For PRI
p_Crown_PRI<-plot_function(load.path,"PRI","Crown")

p_UAV_Crown_PRI<-plot_grid(p_Crown_PRI[[1]],p_Crown_PRI[[2]],p_Crown_PRI[[3]],
          p_Crown_PRI[[4]],p_Crown_PRI[[5]],p_Crown_PRI[[6]],
          align = "hv",nrow=2,ncol=3,
          labels = c(paste0("C",1:6))
)

#-------------------
#(3) save the plots
#-------------------
save.path<-"./manuscript/comprehensive_plot/"
#For PRI_Eco
ggsave(paste0(save.path,"Fig3_supp2_NDVI_landscape.png"),
       p_UAV_Eco_NDVI,width = 19.9,height=11)
#For PRI_Eco
ggsave(paste0(save.path,"Fig3_supp3_PRI_landscape.png"),
       p_UAV_Eco_PRI,width = 19.9,height=11)

