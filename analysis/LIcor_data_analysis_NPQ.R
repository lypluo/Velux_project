###############################################
##Aim: tidy the data and analyze the data 
###############################################
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)
library(ggplot2)
#----------------------
#(1)load the NPQ data
#----------------------
load.path<-"./data/LIcor/"
load(paste0(load.path,"df.NPQ.RDA"))
##merge data in two sites:
#there are some mismatch regarding the data format-->now resolved!
df.Tha<-df.NPQ$Tha %>%
  mutate(sitename="THA",
         hhmmss.1=as.character(hhmmss.1))
df.Dav<-df.NPQ$Dav %>%
  mutate(sitename="DAV")%>%
  mutate(Qmax=as.numeric(Qmax),
         averaging=as.numeric(averaging),
         X=NULL)
#
df.NPQ.merge<-bind_rows(df.Tha,df.Dav)

#-------------------------
#(2)tidy the data
#------------------------
#taking out several important variables:
df.NPQ.merge<-df.NPQ.merge %>%
  mutate(ID=`sample_ID`)%>%
  mutate(CampaignNum=substr(ID,1,2),
         Position=substr(ID,11,11))

#----------------------
#(3)##Plotting
#----------------------
###########
#plotting variables
###########
#specifically: NPQ(non-photochemical quenching)

plot_physio<-function(df,plot_var){
  # df<-df.NPQ.merge
  # plot_var<-"NPQ"
  
  #
  df.sel<-df[,c("sitename","ID","CampaignNum","Position",plot_var)]
  names(df.sel)<-c("sitename","ID","CampaignNum","Position","y")
  
  p_plot<-df.sel%>%
    group_by(CampaignNum) %>%
    ggplot(aes(x=Position,y=y,col=sitename,group=sitename))+
    geom_point(size=1.5)+
    stat_summary(aes(x=Position,y=y,col=sitename),fun.data=mean_sdl, fun.args = list(mult=1),
                 geom="pointrange",size=3,linewidth=4)+
    scale_color_manual(values = c("DAV"=adjustcolor("tomato",0.5),
                                  "THA"=adjustcolor("cyan4",0.5)))+
    facet_wrap(~CampaignNum)+
    theme_light()+
    theme(axis.text = element_text(size=14),
          axis.title = element_text(size=16))
  #
  if(plot_var=="NPQ"){
    p_plot<-p_plot+
      ylab(expression("NPQ"))
  }
  if(plot_var=="qN"){
    p_plot<-p_plot+
      ylab(expression("qN"))
  }
  if(plot_var=="qN_Fo"){
    p_plot<-p_plot+
      ylab(expression("qN_Fo"))
  }
  #the value of Fv/Fm
  if(plot_var=="Fv.Fm"){
    p_plot<-p_plot+
      ylab(expression("F"[v]*"/F"[m]))
  }
 return(p_plot) 
}
###plotting
plot_Fv.Fm.<-plot_physio(df.NPQ.merge,"Fv..Fm.")
plot_FvFm<-plot_physio(df.NPQ.merge,"Fv.Fm")
plot_NPQ<-plot_physio(df.NPQ.merge,"NPQ")
plot_qN<-plot_physio(df.NPQ.merge,"qN")
plot_qN_Fo<-plot_physio(df.NPQ.merge,"qN_Fo")
#save the plots:
ggsave(plot_FvFm,filename = paste("./manuscript/FvFm_campaigns.png"),
       width = 9,height = 6)
ggsave(plot_NPQ,filename = paste("./manuscript/NPQ_campaigns.png"),
       width = 9,height = 6)
ggsave(plot_qN,filename = paste("./manuscript/qN_campaigns.png"),
       width = 9,height = 6)
ggsave(plot_qN_Fo,filename = paste("./manuscript/qN_Fo_campaigns.png"),
       width = 9,height = 6)
