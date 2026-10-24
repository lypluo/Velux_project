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
#A.load the spectral meaurements 
#----------------------
load.path<-"data/"
load(paste0(load.path,"Polypen.data.cleaned.RDA"))
df.Poly.sepctra<-df.Poly.sel%>%
  mutate(Date=as.Date(Date))%>%
  mutate(Position=case_when(c(Position=="L") ~"Lower",
                            c(Position=="M") ~"Middle",
                            c(Position=="U") ~"Upper"))

#------------------
#(2)merge the data and change the Position names
#------------------
#.....

#------------------
#(3)plotting
#------------------
#addding mean and sd--arrange the VI(CH-Dav vs DE-Tha) from top to bottom: 
plot_meansd_fun<-function(df,var_name,y_range,legend.xy,arrow.xy){
  # df<-df.Poly.sepctra
  # var_name<-"PRI"
  # y_range<-c(-0.15,0.05)
  # # legend.xy<-c(0.2,0.9)
  # arrow.xy<-data.frame(x=86,xend=86,
  #                      y1=-0.15,y1_end=-0.13,
  #                      y2=-0.10,y2_end=-0.08,
  #                      y3=-0.03,y3_end=-0.01
  #                      )
  
  df$y<-as.numeric(unlist(df[,var_name]))
  #
  df.proc<-df %>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)%>%
    mutate(sitename=case_when(sitename=="DAV" ~ "CH-Dav",
                              sitename=="THA" ~ "DE-Tha"
                              ))%>%
    ##summary the data
    group_by(sitename,Date,CampaignNum,Position)%>%
    summarise(y_mean=mean(y,na.rm = T),
              y_sd=sd(y,na.rm=T))%>%
    mutate(doy=yday(Date))
  #
  df_THA_stats<-df%>%
    filter(sitename=="THA")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)
  df_DAV_stats<-df%>%
    filter(sitename=="DAV")%>%
    dplyr::select(sitename,CampaignNum,Date,Position,y)
  #using data from Dav - Tha
  df_retidy_stats<-left_join(df_THA_stats,df_DAV_stats,by=c("CampaignNum","Position"))%>%
    mutate(diff_y=y.y - y.x,
           diff_Date=round(as.numeric(c(yday(Date.x) + yday(Date.y))/2)))%>%
    dplyr::select(sitename.x,diff_Date,CampaignNum,Position,diff_y)%>%
    mutate(sitename.x=NULL)
  
  ##add sig.test:
  sig_test<-c()
  var_Lower<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
                                 #select DOY<140--C1-C4
                                 filter(Position=="Lower" & diff_Date<140)%>%dplyr::select(diff_y)))
  var_Middle<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
                                  filter(Position=="Middle"& diff_Date<140)%>%dplyr::select(diff_y)))
  var_Upper<-as.numeric(unlist(as.data.frame(df_retidy_stats)%>%
                                 filter(Position=="Upper"& diff_Date<140)%>%dplyr::select(diff_y)))
  norm_test_Lower<-ks.test(var_Lower,"pnorm")
  norm_test_Middle<-ks.test(var_Middle,"pnorm")
  norm_test_Upper<-ks.test(var_Upper,"pnorm")
  
  ifelse(norm_test_Lower$p.value>0.05,
         sig.Lower<-t.test(var_Lower),sig.Lower<-wilcox.test(var_Lower))
  ifelse(norm_test_Middle$p.value>0.05,
         sig.Middle<-t.test(var_Middle),sig.Middle<-wilcox.test(var_Middle))
  ifelse(norm_test_Upper$p.value>0.05,
         sig.Upper<-t.test(var_Upper),sig.Upper<-wilcox.test(var_Upper))
  sig_test<-list("sig_Lower_pvalue"=round(sig.Lower$p.value,2),
                 "sig_Lower_n"=length(var_Lower),
                 "sig_Middle_pvalue"=round(sig.Middle$p.value,2),
                 "sig_Middle_n"=length(var_Middle),
                 "sig_Upper_pvalue"=round(sig.Upper$p.value,2),
                 "sig_Upper_n"=length(var_Upper)
  )
  print(sig_test)
  #
    p_var_Date_upper<-df.proc%>%
      filter(Position=="Upper")%>%
      ggplot(aes(x=doy,y=y_mean,col=sitename))+
      geom_point(size=3)+
      geom_line(size=1.1,lty=2)+
      geom_errorbar(aes(ymin = y_mean - y_sd, 
                        ymax = y_mean + y_sd), 
                    width = 2) +
      scale_color_manual(values = c("CH-Dav"="red",
                                    "DE-Tha"="orange"))+
      xlab("DOY (2023)")+
      xlim(0,250)+
      ylim(y_range)+
      theme_classic()+
      theme(axis.text = element_text(size=20),
            axis.title = element_text(size=22),
            # axis.text.x = element_text(angle = 35,hjust = 1),
            #increasing text size of facet grid labels
            strip.text.x = element_text(size=18),
            legend.text = element_text(size=16),
            legend.title = element_blank(),
            legend.box.background = element_blank(),
            legend.position = legend.xy,
            legend.background = element_blank()
      )
    
    p_var_Date_middle<-df.proc%>%
      filter(Position=="Middle")%>%
      ggplot(aes(x=doy,y=y_mean,col=sitename))+
      geom_point(size=3)+
      geom_line(size=1.1,lty=2)+
      geom_errorbar(aes(ymin = y_mean - y_sd, 
                        ymax = y_mean + y_sd), 
                    width = 2) +
      scale_color_manual(values = c("CH-Dav"="red",
                                    "DE-Tha"="orange"))+
      xlab("DOY (2023)")+
      xlim(0,250)+
      ylim(y_range)+
      theme_classic()+
      theme(
        legend.position = 'none',
        axis.text = element_text(size=20),
            axis.title = element_text(size=22),
            # axis.text.x = element_text(angle = 35,hjust = 1),
            #increasing text size of facet grid labels
            strip.text.x = element_text(size=18),
            legend.text = element_text(size=16),
            legend.title = element_blank(),
            legend.box.background = element_blank(),
            # legend.position = legend.xy,
            legend.background = element_blank()
      )
    p_var_Date_lower<-df.proc%>%
      filter(Position=="Lower")%>%
      ggplot(aes(x=doy,y=y_mean,col=sitename))+
      geom_point(size=3)+
      geom_line(size=1.1,lty=2)+
      geom_errorbar(aes(ymin = y_mean - y_sd, 
                        ymax = y_mean + y_sd), 
                    width = 2) +
      scale_color_manual(values = c("CH-Dav"="red",
                                    "DE-Tha"="orange"))+
      xlab("DOY (2023)")+
      xlim(0,250)+
      ylim(y_range)+
      theme_classic()+
      theme(
        legend.position='none',
        axis.text = element_text(size=20),
            axis.title = element_text(size=22),
            # axis.text.x = element_text(angle = 35,hjust = 1),
            #increasing text size of facet grid labels
            strip.text.x = element_text(size=18),
            legend.text = element_text(size=16),
            legend.title = element_blank(),
            legend.box.background = element_blank(),
            # legend.position = legend.xy,
            legend.background = element_blank()
      )
      
  #add arrows:
    p_var_Date_upper<-p_var_Date_upper+
      #adding the flag to indicate cold events in Davos
      geom_segment(
        aes(x = arrow.xy$x, y = arrow.xy$y1,
            xend = arrow.xy$xend, yend = arrow.xy$y1_end),
        arrow = arrow(type = "closed", 
                      length = unit(0.2, "inches")),
        color = "red", size = 0.8)
    
    p_var_Date_middle<-p_var_Date_middle+
      #adding the flag to indicate cold events in Davos
      geom_segment(
        aes(x = arrow.xy$x, y = arrow.xy$y2,
            xend = arrow.xy$xend, yend = arrow.xy$y2_end),
        arrow = arrow(type = "closed", 
                      length = unit(0.2, "inches")),
        color = "red", size = 0.8)
    
    p_var_Date_lower<-p_var_Date_lower+
      #adding the flag to indicate cold events in Davos
      geom_segment(
        aes(x = arrow.xy$x, y = arrow.xy$y3,
            xend = arrow.xy$xend, yend = arrow.xy$y3_end),
        arrow = arrow(type = "closed", 
                      length = unit(0.2, "inches")),
        color = "red", size = 0.8)
    
  # plot_grid(p_var_Date_upper,p_var_Date_middle,p_var_Date_lower,nrow=1)
   p_var_Date<-list("Upper"=p_var_Date_upper,
                    "Middle"=p_var_Date_middle,
                    "Lower"=p_var_Date_lower) 
  return(p_var_Date)
}

##-------------
#leaf spectral data
##-------------
#1) NDVI-->only for the upper layer
p_NDVI_Date<-plot_meansd_fun(df.Poly.sepctra,"NDVI",c(0.5,0.9),
                             c(0.15,0.85),
                             data.frame(x=86,xend=86,
                             y1=0.55,y1_end=0.6,
                             y2=0.6,y2_end=0.65,
                             y3=0.6,y3_end=0.65))
#
p_NDVI_Date_upper<-p_NDVI_Date$Upper+
  labs(
    x="DOY (2023)",
    y = expression(" NDVI"[Needle]),
    title = "Upper"
  )+
  # annotate(geom = "text",x=25,y=0.9,label="Upper",size=6)
  theme(
    plot.title = element_text(hjust = 0.5, size = 22)
  )
#
p_NDVI_Date_middle<-p_NDVI_Date$Middle+
  labs(
    x="DOY (2023)",
    y = expression(" NDVI"[Needle]),
    title = "Middle"
  )+
  # annotate(geom = "text",x=25,y=0.9,label="Middle",size=6)
  theme(
    plot.title = element_text(hjust = 0.5, size = 22)
  )
p_NDVI_Date_lower<-p_NDVI_Date$Lower+
  labs(
    x="DOY (2023)",
    y = expression(" NDVI"[Needle]),
    title = "Lower"
  )+
  # annotate(geom = "text",x=25,y=0.9,label="Lower",size=6)
  theme(
    plot.title = element_text(hjust = 0.5, size = 22)
  )

#plot_grid(p_NDVI_Date_upper,p_NDVI_Date_middle,p_NDVI_Date_lower,nrow=1)

##adding the lines to indicate the field sampling dates:
Date.Dav<-as.Date(c("2023-03-08","2023-03-27","2023-04-21",
                    "2023-05-03","2023-05-22","2023-07-17"))
Date.Tha<-as.Date(c("2023-03-02","2023-03-22","2023-04-13",
                    "2023-04-28","2023-05-17","2023-07-14"))
#
Date.Dav_DOY<-yday(Date.Dav)
Date.Tha_DOY<-yday(Date.Tha)

########
p_NDVI_Date_lower<-p_NDVI_Date_lower+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  annotate(geom = "text",x=c(64),y=rep(0.5),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(0.5),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(0.5),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(0.5),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(0.5),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(0.5),label=c("C6"),size=6)

p_NDVI_Date_middle<-p_NDVI_Date_middle+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  annotate(geom = "text",x=c(64),y=rep(0.5),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(0.5),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(0.5),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(0.5),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(0.5),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(0.5),label=c("C6"),size=6)

p_NDVI_Date_upper<-p_NDVI_Date_upper+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  annotate(geom = "text",x=c(64),y=rep(0.5),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(0.5),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(0.5),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(0.5),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(0.5),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(0.5),label=c("C6"),size=6)
# plot_grid(p_NDVI_Date_upper,p_NDVI_Date_middle,p_NDVI_Date_lower,nrow=1)

##adding the DSPR period and sig. symbol
Dav_pheno<-data.frame("sos10"=61,"sos25"=65,"peak"=180,"eos25"=296,"eos10"=322)
Tha_pheno<-data.frame("sos10"=61,"sos25"=70,"peak"=177,"eos25"=285,"eos10"=312)

#period:[sos-60,sos+60] is set to red
#adding the sos
#for CH-Dav
rect.coord_isevent_Dav=data.frame("x1"=as.numeric(Dav_pheno[1])-60,
                                  "x2"=as.numeric(Dav_pheno[1]),
                                  "x3"=as.numeric(Dav_pheno[1]+60),
                                  "y1"=-Inf,
                                  "y2"=Inf)
rect.coord_isevent_Tha=data.frame("x1"=as.numeric(Tha_pheno[1])-60,
                                  "x2"=as.numeric(Tha_pheno[1]),
                                  "x3"=as.numeric(Tha_pheno[1]+60),
                                  "y1"=-Inf,
                                  "y2"=Inf)
plot_add_rectanges<-function(df.plot,rect.coord_isevent,x_text,y_text){
  # df.plot<-p_Fv.Fm_Date_Dav
  # rect.coord_isevent<-rect.coord_isevent_Dav
  # x_text<-Dav_pheno[1]
  # y_text<-0.4
  
  df_add<-df.plot+
    # annotate("rect",xmin = rect.coord_isevent$x1,
    #          xmax=rect.coord_isevent$x2,
    #          ymin = -Inf, ymax = Inf,
    #          fill="blue",alpha=0.3
    #          )+
    annotate("rect",xmin = rect.coord_isevent$x2,
             xmax=rect.coord_isevent$x3,
             ymin = -Inf, ymax = Inf,
             fill="blue",alpha=0.1
    )
    # geom_vline(xintercept=as.numeric(x_text),col="forestgreen",size=1.1)+
    # annotate(geom = "text",x=as.numeric(x_text)+10,y=y_text,label="sos",col="forestgreen",size=6)
  
  return(df_add)
}

#for DSPR period for VIs:
p_NDVI_Date_Upper<-plot_add_rectanges(p_NDVI_Date_upper,rect.coord_isevent_Dav,Dav_pheno[1],0.5)
p_NDVI_Date_Middle<-plot_add_rectanges(p_NDVI_Date_middle,rect.coord_isevent_Dav,Dav_pheno[1],0.5)
p_NDVI_Date_Lower<-plot_add_rectanges(p_NDVI_Date_lower,rect.coord_isevent_Dav,Dav_pheno[1],0.5)

##adding the sig. symbol:
p_NDVI_Date_Upper<-p_NDVI_Date_Upper +
  annotate("text",x=c(100),y=c(0.85),label=c("***"),
           size=10)
p_NDVI_Date_Middle<-p_NDVI_Date_Middle +
  annotate("text",x=c(100),y=c(0.85),label=c("***"),
           size=10)
p_NDVI_Date_Lower<-p_NDVI_Date_Lower +
  annotate("text",x=c(100),y=c(0.85),label=c("***"),
           size=10)

#2) PRI
p_PRI_Date<-plot_meansd_fun(df.Poly.sepctra,"PRI",c(-0.15,0.05),
                            c(0.15,0.85),
                            data.frame(x=86,xend=86,
                            y1=-0.15,y1_end=-0.13,
                            y2=-0.10,y2_end=-0.08,
                            y3=-0.03,y3_end=-0.01))
p_PRI_Date_upper<-p_PRI_Date$Upper+
  labs(
    x="DOY (2023)",
    y = expression(" PRI"[Needle]),
    title = "Upper"
  )+
  # annotate(geom = "text",x=25,y=0.05,label="Upper",size=6)
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )
p_PRI_Date_middle<-p_PRI_Date$Middle+
  labs(
    x="DOY (2023)",
    y = expression(" PRI"[Needle]),
    title = "Middle"
  )+
  # annotate(geom = "text",x=25,y=0.05,label="Middle",size=6)
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )
p_PRI_Date_lower<-p_PRI_Date$Lower+
  labs(
    x="DOY (2023)",
    y = expression(" PRI"[Needle]),
    title = "Lower"
  )+
  # annotate(geom = "text",x=25,y=0.05,label="Lower",size=6)
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )
# plot_grid(p_PRI_Date_upper,p_PRI_Date_middle,p_PRI_Date_lower,nrow=1)

######
p_PRI_Date_upper<-p_PRI_Date_upper+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  annotate(geom = "text",x=c(64),y=rep(-0.15),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.15),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.15),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.15),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.15),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.15),label=c("C6"),size=6)

p_PRI_Date_middle<-p_PRI_Date_middle+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  annotate(geom = "text",x=c(64),y=rep(-0.15),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.15),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.15),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.15),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.15),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.15),label=c("C6"),size=6)

p_PRI_Date_lower<-p_PRI_Date_lower+
  geom_vline(xintercept = Date.Dav_DOY,col="red",lty=2)+
  geom_vline(xintercept = Date.Tha_DOY,col="orange",lty=2)+
  annotate(geom = "text",x=c(64),y=rep(-0.15),label=c("C1"),size=6)+
  annotate(geom = "text",x=c(84),y=rep(-0.15),label=c("C2"),size=6)+
  annotate(geom = "text",x=c(107),y=rep(-0.15),label=c("C3"),size=6)+
  annotate(geom = "text",x=c(120),y=rep(-0.15),label=c("C4"),size=6)+
  annotate(geom = "text",x=c(140),y=rep(-0.15),label=c("C5"),size=6)+
  annotate(geom = "text",x=c(196),y=rep(-0.15),label=c("C6"),size=6)
# plot_grid(p_PRI_Date_upper,p_PRI_Date_middle,p_PRI_Date_lower,nrow=1)

##adding the DSPR period and sig. symbol
p_PRI_Date_Upper<-plot_add_rectanges(p_PRI_Date_upper,rect.coord_isevent_Dav,Dav_pheno[1],0.5)
p_PRI_Date_Middle<-plot_add_rectanges(p_PRI_Date_middle,rect.coord_isevent_Dav,Dav_pheno[1],0.5)
p_PRI_Date_Lower<-plot_add_rectanges(p_PRI_Date_lower,rect.coord_isevent_Dav,Dav_pheno[1],0.5)

##adding the sig. symbol:
p_PRI_Date_Upper<-p_PRI_Date_Upper +
  annotate("text",x=c(100),y=c(0.9),label=c("***"),
           size=10)
p_PRI_Date_Middle<-p_PRI_Date_Middle +
  annotate("text",x=c(100),y=c(0.9),label=c("***"),
           size=10)
p_PRI_Date_Lower<-p_PRI_Date_Lower +
  annotate("text",x=c(100),y=c(0.9),label=c("***"),
           size=10)
##adding the sig. symbol:
p_PRI_Date_Upper<-p_PRI_Date_Upper +
  annotate("text",x=c(100),y=c(0.05),label=c("***"),
           size=10)
p_PRI_Date_Middle<-p_PRI_Date_Middle +
  annotate("text",x=c(100),y=c(0.05),label=c("***"),
           size=10)
p_PRI_Date_Lower<-p_PRI_Date_Lower +
  annotate("text",x=c(100),y=c(0.05),label=c("***"),
           size=10)

################
###Merge plots:
################
p_leaf_spectra_NDVI<-plot_grid(p_NDVI_Date_Upper,p_NDVI_Date_Middle,
                               p_NDVI_Date_Lower,
                               labels = c("(a)","(b)","(c)"
                               ),
                               axis="tblr",
                               label_size = 22,
                               nrow=1)

##
p_leaf_spectra_PRI<-plot_grid(p_PRI_Date_Upper,p_PRI_Date_Middle,
                              p_PRI_Date_Lower,
                              labels = c("(a)","(b)","(c)"
                              ),
                              axis="tblr",
                              label_size = 22,
                              nrow=1)

# ggsave(paste0(save.path,"Fig2_temp.png"),p_leaf_physio,width = 12,height=15)
##save the ggplot plots:
save.path<-"./data/Comprehensive_plot_data/Fig3/"
#
save(p_leaf_spectra_NDVI,file=paste0(save.path,"p_leaf_spectra_NDVI_new.RDA"))
save(p_leaf_spectra_PRI,file=paste0(save.path,"p_leaf_spectra_PRI_new.RDA"))
