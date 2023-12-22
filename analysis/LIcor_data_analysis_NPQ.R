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
#
df.NPQ.merge<-df.NPQ.merge %>%
  #2023-Dec, focusing some more variables after discussion with colleagues after AGU
  select(sitename,ID,CampaignNum,Position,Fv,Fv.,Fv.Fm,Fv..Fm.,
         #PS2.1-->Ratio between PSII and PSI
         #PhiPS2-->ΦPSII (also known as ΔF/Fm') is the fraction of absorbed PSII photons that are used in photochemistry
         #qN,qP-->Two competing processes that quench (decrease) the level of chlorophyll fluorescence
         #in the leaf are referred to as photochemical (qP) and non-photochemical (qN) quenching.
         NPQ,PS2.1,PhiPS2,qL,qN,qN_Fo,qP,qP_Fo)%>%
  #additionally add parameters:ratio between qN and qP(qN/QP)
  mutate(qN.qP=qN/qP,qN_Fo.qP_Fo=qN_Fo/qP_Fo)
#save the data:
save(df.NPQ.merge,file = paste0("./data/LIcor/df.NPQ.cleaned.RDA"))

#----------------------
#(3)##Plotting
#----------------------
###########
#plotting variables
###########
#specifically: NPQ(non-photochemical quenching)

plot_physio<-function(df,plot_var){
  # df<-df.NPQ.merge
  # plot_var<-"Fv..Fm."
  
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
plot_Fv.Fm.<-plot_physio(df.NPQ.merge,"Fv..Fm.")+
  ylab(expression("F'"[v]*"/F'"[m]))
plot_FvFm<-plot_physio(df.NPQ.merge,"Fv.Fm")
plot_NPQ<-plot_physio(df.NPQ.merge,"NPQ")
plot_qN<-plot_physio(df.NPQ.merge,"qN")
plot_qN_Fo<-plot_physio(df.NPQ.merge,"qN_Fo")
#additional variables:
plot_qL<-plot_physio(df.NPQ.merge,"qL")+
  ylab(expression("qL"))
plot_qP<-plot_physio(df.NPQ.merge,"qP")+
  ylab(expression("qP"))
plot_qP_Fo<-plot_physio(df.NPQ.merge,"qP_Fo")+
  ylab(expression("qP_Fo"))
plot_PS2.1<-plot_physio(df.NPQ.merge,"PS2.1")+
  ylab(expression("PSII/PSI"))
plot_PhiPS2<-plot_physio(df.NPQ.merge,"PhiPS2")+
  ylab(expression(phi[PSII]))
plot_qN.qP<-plot_physio(df.NPQ.merge,"qN.qP")+
  ylab(expression("qN/qP"))+
  #there is outlier in the measurements-->hence limit the y range below 10
  ylim(0,10)
plot_qN_Fo.qP_Fo<-plot_physio(df.NPQ.merge,"qN_Fo.qP_Fo")+
  ylab(expression("qN_Fo/qP_Fo"))+
  #there is outlier in the measurements-->hence limit the y range below 10
  ylim(0,10)

#save the plots:
library(cowplot)
#merge some plots:
plot_FvtoFm<-plot_grid(plot_FvFm,plot_Fv.Fm.,ncol=2)
plot_qPqN<-plot_grid(plot_qP,plot_qN,ncol=2)
plot_qPqN_Fo<-plot_grid(plot_qP_Fo,plot_qN_Fo,ncol=2)
plot_qL
plot_PSIIandI<-plot_grid(plot_PS2.1,plot_PhiPS2,ncol=2)
plot_qNtoqP<-plot_grid(plot_qN.qP,plot_qN_Fo.qP_Fo,ncol=2)
#further tidy:
#put the Fv/Fm, PhiPS2,and qN/qP together
plot_PSII_effienciy<-plot_grid(plot_FvFm,plot_PhiPS2,plot_qN.qP,ncol=3)
#
# ggsave(plot_FvtoFm,filename = paste("./manuscript/FvFm_campaigns.png"),
#        width = 9,height = 6)
# ggsave(plot_NPQ,filename = paste("./manuscript/NPQ_campaigns.png"),
#        width = 9,height = 6)
# ggsave(plot_qN,filename = paste("./manuscript/qN_campaigns.png"),
#        width = 9,height = 6)
# ggsave(plot_qN_Fo,filename = paste("./manuscript/qN_Fo_campaigns.png"),
#        width = 9,height = 6)
ggsave(plot_PSII_effienciy,filename = paste("./manuscript/PSII_effiency_campaigns.png"),
       width = 12,height = 6)
