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
  # VI_scale<-"Crown"
  
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
            axis.title = element_text(size=26)
      )
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
# plot_grid(p_Landscape_NDVI[[1]],p_Landscape_NDVI[[2]],p_Landscape_NDVI[[3]],
#           p_Landscape_NDVI[[4]],p_Landscape_NDVI[[5]],p_Landscape_NDVI[[6]],
#           align = "hv",nrow=2,ncol=3,
#           labels = c(paste0("C",1:6))
#           )

### For PRI
p_Landscape_PRI<-plot_function(load.path,"PRI","Landscape")
#
# plot_grid(p_Landscape_PRI[[1]],p_Landscape_PRI[[2]],p_Landscape_PRI[[3]],
#           p_Landscape_PRI[[4]],p_Landscape_PRI[[5]],p_Landscape_PRI[[6]],
#           align = "hv",nrow=2,ncol=3,
#           labels = c(paste0("C",1:6))
# )

###############
### For crown NDVI
load.path<-"D:/EE_WSL/IMPACT_project/Drones_data/ArcGIS_processing/Davos_test_Project/exported_tiff/Crowns/"
p_Crown_NDVI<-plot_function(load.path,"NDVI","Crown")
#
# plot_grid(p_Crown_NDVI[[1]],p_Crown_NDVI[[2]],p_Crown_NDVI[[3]],
#           p_Crown_NDVI[[4]],p_Crown_NDVI[[5]],p_Crown_NDVI[[6]],
#           align = "hv",nrow=2,ncol=3,
#           labels = c(paste0("C",1:6))
#           )

### For PRI
p_Crown_PRI<-plot_function(load.path,"PRI","Crown")
#
# plot_grid(p_Crown_PRI[[1]],p_Crown_PRI[[2]],p_Crown_PRI[[3]],
#           p_Crown_PRI[[4]],p_Crown_PRI[[5]],p_Crown_PRI[[6]],
#           align = "hv",nrow=2,ncol=3,
#           labels = c(paste0("C",1:6))
# )

#-------------------
#(3) start to make the plots for the mauscript
#-------------------
##Plot-A: merge Landscape+Crown PRI(Campaign 5 as an example)
p_PRI_C5_Landscape<-p_Landscape_PRI$C5_PRI_Landscape+
  scale_fill_gradientn(colors = c(
    "#440154","#482878","#3E4B8B","#31688E","#26828E",
    "#1F9B77","#5CDB5A","#B8DE29", "#FDE725"),
                       limits=c(-0.2,0.5))+
  annotate(geom = "text",x=2784385,y=1187640,label="C5",size=10)+
  theme(axis.title = element_blank())
p_PRI_C5_Crown<-p_Crown_PRI$C5_PRI_Crown+
  # theme(legend.position = c(0.86,0.3))+
  scale_fill_gradientn(colors = c(
    "#440154","#482878","#3E4B8B","#31688E","#26828E",
    "#1F9B77","#5CDB5A","#B8DE29", "#FDE725"),
    limits=c(-0.2,0.5))+
  theme_light()+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
#insert crown plot into landscape as insert plot
p_merge_image_PRI<-p_PRI_C5_Landscape +
  inset_element(p_PRI_C5_Crown,left = 0.72,right =0.99,
                bottom = 0.72,top=0.99)
# p_merge_image_PRI<-plot_grid(p_merge_image_PRI,
#                 labels = c("(c)"),label_x = 0,label_y=1,
#                          nrow=1)
##Plot-B: the data distribution of VI from C1-C6:
plot_boxplot<-function(tiff.path,VI_name,VI_scale){
  # tiff.path<-load.path
  # VI_name<-"NDVI"
  # VI_scale<-"Crown"
  
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
    df.proc<-df.proc %>%
      mutate(Cam=paste0("C",i),
             Scale=VI_scale)
    df_agg<-rbind(df_agg,df.proc)
    rm(p.proc)
  }
  names(df_agg)<-c("x","y","VI_value","Campaign","Scale")
  #plotting:
  if(VI_scale=="Crown"){
    p.run<-ggplot(df_agg,aes(x=Campaign,y=VI_value))+
      geom_jitter(width = 0.2, color = adjustcolor("gray",0.01),
                  alpha = 0.5) +
      geom_boxplot()+
      # ylab(expression("Drones' "[Crown]))+
    theme_light()+
    theme(axis.text = element_text(size=22),
            axis.title = element_text(size=26),
            legend.text = element_text(size=22)
      )
  }
  if(VI_scale=="Landscape"){
  p.run<-ggplot(df_agg,aes(x=Campaign,y=VI_value))+
    # geom_jitter(width = 0.2, color = adjustcolor("gray",0.01),
    #             alpha = 0.5) +
    geom_boxplot()+
    # ylab(paste0(VI_scale," ",VI_name))+
    # ylab(expression(paste0("Drones'",VI_name)[Eco]))+
    theme_light()+
    theme(axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          legend.text = element_text(size=22)
    )
  }
  #
  return(p.run)
}

### For Landscape VI
load.path<-"D:/EE_WSL/IMPACT_project/Drones_data/ArcGIS_processing/Davos_test_Project/exported_tiff/"
p_Landscape_NDVI_boxplot<-plot_boxplot(load.path,"NDVI","Landscape")+
  ylab(expression("Drones' NDVI"[Eco]))
p_Landscape_PRI_boxplot<-plot_boxplot(load.path,"PRI","Landscape")+
  ylab(expression("Drones' PRI"[Eco]))

### For Crown VI
load.path<-"D:/EE_WSL/IMPACT_project/Drones_data/ArcGIS_processing/Davos_test_Project/exported_tiff/Crowns/"
p_crown_NDVI_boxplot<-plot_boxplot(load.path,"NDVI","Crown")+
  ylab(expression("Drones' NDVI"[Crown]))
  
p_crown_PRI_boxplot<-plot_boxplot(load.path,"PRI","Crown")+
  ylab(expression("Drones' PRI"[Crown]))

#-------------------
#merge the plot
#-------------------
#put the crown PRI as an insert plot
p_crown_PRI_boxplot<-p_crown_PRI_boxplot+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p_PRI_boxplot<-plot_grid(p_crown_PRI_boxplot,p_Landscape_PRI_boxplot,
          # labels = c("(d)","(e)"),
          nrow=2)

#-------------------
#(4) save the plots
#-------------------
save.path<-"D:/data/Velux_shared_data/CH-Dav/HighSpec/Github_data/"
save(p_merge_image_PRI,file=paste0(save.path,"p_Drones_PRI_image.RDA"))
save(p_PRI_boxplot,file=paste0(save.path,"p_Drones_PRI_boxplot.RDA"))
