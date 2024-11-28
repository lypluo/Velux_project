#################################################
#Aim: structure the plot temporally-->for the leaf scale measurements
#e.g. x-->Date, y=Amax in different sites and height
##################################################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)
#Date of 6 campaigns:
#For Tharandt: Mar,02; Mar,22; Apr,13; Apr,28; May,17; July,14
#For Davos:Mar,08; Mar,27; Apr,21; May,03; May,22; July,17 
#------------------
#(1)load the data
#------------------
#----------------------
#E.load the water potential data:
#----------------------
load.path<-"data/Water_Potential/"
load(paste0(load.path,"WaterPotential.data.cleaned.RDA"))
df.WP<-df.WaterP.final%>%
  mutate(Date=as.Date(Date))%>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="U") ~"Upper",
                            c(Position=="D") ~"Trunk"
                            ))

#------------------
#(2)plotting
#------------------
#WP_trunk + 
#addding mean and sd
plot_point_fun_meansd<-function(df,var_name,legend.xy,arrow.xy,arrow.flag){
#also adding arrows in C2:
# made for the water potential 
  # df<-df.WP
  # var_name<-"WP_Trunk"
  # legend.xy<-c(0.1,0.9)
  # arrow.xy<-data.frame(x=4,xend=4,y=-25,yend=-30)
  # arrow.flag<-TRUE
  
  df$y<-as.numeric(unlist(df[,var_name]))
  #
  if(var_name=="WP_Branch"|var_name=="WP_twig"){
    df_sel<-df%>%
      filter(Position!="Trunk")
  }
  if(var_name=="WP_Trunk"){
    df_sel<-df%>%
      filter(Position!="Upper" & Position!="Lower")
  }
    p_var_Date<-df_sel%>%
    # filter(Position!="Middle")%>%
    group_by(Position)%>%
    ggplot(aes(x=as.factor(substr(Date,6,10)),y=y))+
    geom_point(size=2,shape=4)+
    # geom_line(size=1.2)+
    stat_summary(aes(x=as.factor(substr(Date,6,10)),y=y,color=sitename),
                 fun.data=mean_sdl, fun.args = list(mult=1),
                 geom="pointrange",size=1.8,linewidth=1.2,shape=16)+
    facet_wrap(~Position)+
    scale_color_manual(values = c("DAV"=adjustcolor("red",0.5),
                                  "THA"=adjustcolor("orange",0.5)))+
    # labs(color = "Sitename")+
    # labs(
    #   # x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    #   y = expression(A[max] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
    # )
    ##separate the campaigns-adding in 2024
    geom_vline(xintercept = c(2.5,4.5,6.5,8.5,10.5),lty=2)+
    theme_light()+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=20),
          axis.text.x = element_text(angle = 35,hjust = 1),
          legend.text = element_text(size=16),
          legend.title = element_blank(),
          strip.text.x = element_text(size=18),
          legend.position = legend.xy,
          legend.background = element_blank()
    )
  #
    if(var_name=="WP_Trunk"){
      p_var_Date<-p_var_Date+
        geom_vline(xintercept = c(1.5,3.5,5.5),lty=2)
    }
  #
  if(arrow.flag==TRUE){
    p_var_Date<-p_var_Date+
      #adding the flag to indicate cold events in Davos
      geom_segment(
        aes(x = arrow.xy$x, y = arrow.xy$y,
            xend = arrow.xy$xend, yend = arrow.xy$yend),
        arrow = arrow(type = "closed", 
                      length = unit(0.2, "inches")),
        color = "blue", size = 0.8)
  }
  return(p_var_Date)
}


##-------------
#branch water potential
##-------------
#Branch
p_WP_Branch_Date<-plot_point_fun_meansd(df.WP,"WP_Branch",c(0.36,0.2),
               data.frame(x=4,xend=4,y=-25,yend=-30),
                                      TRUE)+
  labs(
    x="2023",
    y = expression(psi[branch]*" (bar)")
  )+
  theme(legend.position = "none")+
  #adding Campaign Nr.
  annotate(geom = "text",x=c(1.5),y=rep(-32),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(3.5),y=rep(-32),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(5.5),y=rep(-32),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(7.5),y=rep(-32),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(9.5),y=rep(-32),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(11.5),y=rep(-32),label=c("C6"),size=6)
#big branch
p_WP_Trunk_Date<-plot_point_fun_meansd(df.WP,"WP_Trunk",c(0.36,0.2),
                 data.frame(x=4,xend=4,y=-20,yend=-30),
                                        TRUE)+
  labs(
    x="2023",
    y = expression(psi[trunk]*" (bar)")
  )+
  theme(legend.position = "none")+
  #adding Campaign Nr.
  annotate(geom = "text",x=c(1),y=rep(-32),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(2),y=rep(-32),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(3),y=rep(-32),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(4),y=rep(-32),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(5),y=rep(-32),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(6),y=rep(-32),label=c("C6"),size=6)


#!merge the Gs, and,leaf water potential
p_WP_Branch_and_Trunk<-plot_grid(p_WP_Branch_Date,p_WP_Trunk_Date,nrow=1,
                         align = "v",labels = c("(a)","(b)"),
                rel_widths = c(0.65,0.35))
# ggsave(p_hydro_merge,filename = paste("./manuscript/Summary_Vars_with_Time/P_hydro_merge.png"),
#        width = 10,height = 8)
##save the ggplot plots:
# save.path<-"./data/Comprehensive_plot_data/Fig4/"
# save(p_WP_Branch_and_Trunk,
#      file=paste0(save.path,"p_WP_Branch_and_Trunk.RDA"))
#
save.path<-"./manuscript/comprehensive_plot/"
ggsave(paste0(save.path,"Fig4_supp1_WP_Branch_and_Trunk.png"),
       p_WP_Branch_and_Trunk,width = 13,height=5)

