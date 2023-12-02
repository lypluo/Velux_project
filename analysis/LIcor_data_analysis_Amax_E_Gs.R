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
#(1)load the Amax data
#----------------------
load.path<-"./data/LIcor/"
load(paste0(load.path,"df.Amax.RDA"))
##merge data in two sites:
df.Tha<-df.Amax$Tha %>%
  mutate(sitename="THA")
df.Dav<-df.Amax$Dav %>%
  mutate(sitename="DAV")
#
df.Amax.merge<-bind_rows(df.Tha,df.Dav)

#-------------------------
#(2)tidy the data
#------------------------
#taking out several important variables:
df.Amax.merge<-df.Amax.merge %>%
  mutate(ID=`sample_ID`)%>%
  mutate(CampaignNum=substr(ID,1,2),
         Position=substr(ID,11,11))

#----------------------
#(3)light response curve(LRC) fitting 
#----------------------
library(photosynthesis)
#refering the information from 
#https://cran.r-project.org/web/packages/photosynthesis/vignettes/light-response.html

dat<-df.Amax.merge %>%
  select(sitename,ID,CampaignNum,Position,Qin,A,CO2_s)%>%
  mutate(group = round(CO2_s, digits = 0)) |>
  #round sequentially due to CO2_s set points
  mutate(group = as.factor(round(group, digits = -2)))%>%
  ##filter the data that does not make sense:
  #A-->A>0
  filter(A>0)%>%
  mutate(A=ifelse(sitename=="THA"&CampaignNum=="C2"&A>12,NA,A))
##save the cleaned data:
save.path<-"./data/LIcor/"
save(dat,file = paste0(save.path,"df.Amax.cleaned.RDA"))

# Fit light-response curves
library(purrr)
#develop the funtion:
fit_LRC<-function(df,site,CamN,Pos){
  # df<-dat
  # site<-"DAV"
  # CamN<-"C5"
  # Pos<-"U"
  
  
  df.sel<-df %>%
    filter(sitename==site) %>%
    filter(CampaignNum == CamN)%>%
    filter(Position == Pos)%>%
    filter(A>0)
  #for THA-->C2 measured A is too high-->outlier-->filter A<13
  if(site=="THA" & CamN=="C2"){
  df.sel<-df.sel %>%
    filter(sitename==site) %>%
    filter(CampaignNum == CamN)%>%
    filter(Position == Pos)%>%
    filter(A>0 & A<12)
  }
  
  fit = fit_photosynthesis(
    .data = filter(df.sel,group == 400),
    .photo_fun = "aq_response",
    .vars = list(.A = A, .Q = Qin),
  )
  return(fit)
}

##----------------
#A. For Tharandt
##----------------
fit_LRC(dat,"THA","C1","L")
##
CamN<-paste0("C",c(1:6))
Pos<-c("L","U")
#for Tharandt
fit_LRC_curves_Tha<-c()
k=1
for (i in 1:length(CamN)) {
  for (j in 1:length(Pos)) {
    fit_LRC_curves_Tha[[k]]<-fit_LRC(dat,"THA",CamN[i],Pos[j])
    k=k+1
  }
}
##
names_ID<-c("C1-L","C1-U","C2-L","C2-U","C3-L","C3-U",
            "C4-L","C4-U","C5-L","C5-U","C6-L","C6-U")
names(fit_LRC_curves_Tha)<-names_ID
LRC_THA<-fit_LRC_curves_Tha |>
  map(coef) |>
  map(t) |>
  map(as.data.frame) |>
  imap_dfr(~ mutate(.x, ID = .y))%>%
  mutate(sitename="THA")

##----------------
#B. For Davos
##----------------
fit_LRC(dat,"DAV","C1","L")
##
CamN<-paste0("C",c(1:6))
Pos<-c("L","U")
#for Tharandt
fit_LRC_curves_Dav<-c()
k=1
for (i in 1:length(CamN)) {
  for (j in 1:length(Pos)) {
    fit_LRC_curves_Dav[[k]]<-fit_LRC(dat,"DAV",CamN[i],Pos[j])
    k=k+1
  }
}
##
names_ID<-c("C1-L","C1-U","C2-L","C2-U","C3-L","C3-U",
            "C4-L","C4-U","C5-L","C5-U","C6-L","C6-U")
names(fit_LRC_curves_Dav)<-names_ID
LRC_DAV<-fit_LRC_curves_Dav |>
  map(coef) |>
  map(t) |>
  map(as.data.frame) |>
  imap_dfr(~ mutate(.x, ID = .y))%>%
  mutate(sitename="DAV")

#-------------------------------
#save the LRC fitting parameters
#-------------------------------
df.LRC.paras<-rbind(LRC_THA,LRC_DAV)
save.path<-"./data/LIcor/"
save(df.LRC.paras,file = paste0(save.path,"LRC.parameters.RDA"))

#----------------------
#(4)##Plot model fit and raw data
#----------------------
###########
#A.plotting light response curve
###########
plot_LRC_fun<-function(df,df.Tha,df.Dav,CamN,xlab,ylab,legend){
  #dataset:
  # df<-dat
  # #fitting lines:
  # df.Tha<-LRC_THA
  # df.Dav<-LRC_DAV
  # CamN<-"C6"
  # xlab<-TRUE
  # ylab<-TRUE
  # legend<-TRUE
  
  #
  pos<-grep(CamN,df.Tha$ID)
  df.Tha_sel<-df.Tha[pos,]%>%
    mutate(CampaignNum=substr(ID,1,2),
           Position=substr(ID,4,4),
           sitename="THA")
  df.Dav_sel<-df.Dav[pos,]%>%
    mutate(CampaignNum=substr(ID,1,2),
           Position=substr(ID,4,4),
           sitename="DAV")
  
  #fitting parameter
  para = rbind(df.Tha_sel,df.Dav_sel) 
  #original data:
  df_sel<-df %>%
    filter(CampaignNum==CamN)%>%
    filter(A>0)
  
  #
  df_predict=list()
  if(CamN!="C6"){
    for (i in 1:nrow(para)) {
      df.temp<-data.frame(Qin = seq(0, 1.02 * 1500, length.out = 100)) |>
        mutate(
          A = marshall_biscoe_1980(
            #keep the original variable name Q_abs here
            Q_abs = Qin,
            k_sat = para[i,"k_sat"],
            para[i,"phi_J"],
            para[i,"theta_J"]
          ) - para[i,"Rd"]
        )
      df_predict[[i]]<-df.temp
    }
  }
  if(CamN=="C6"){
    for (i in 1:nrow(para)) {
      if(i!=2){
        df.temp<-data.frame(Qin = seq(0, 1.02 * 1500, length.out = 100)) 
      }
      if(i==2){
        #for THA-U:light intensity measurement range is limited:
        df.temp<-data.frame(Qin = seq(0, 1 * 500, length.out = 100)) 
      }
      df.temp<-df.temp|>
        mutate(
          A = marshall_biscoe_1980(
            #keep the original variable name Q_abs here
            Q_abs = Qin,
            k_sat = para[i,"k_sat"],
            para[i,"phi_J"],
            para[i,"theta_J"]
          ) - para[i,"Rd"]
        )
      df_predict[[i]]<-df.temp
    }
  }
  
  names(df_predict)<-para$ID
  
  ##plotting
  p_plot<-ggplot(mapping = aes(Qin, A)) +
    geom_line(data = df_predict[[1]],aes(col="THA-L"),linewidth=1.5)+
    geom_point(data = filter(df_sel, c(sitename == "THA" & group == 400 & CampaignNum == CamN & Position==para$Position[1])),
               col=adjustcolor("cyan2",0.5)) +
    geom_line(data = df_predict[[2]],aes(col="THA-U"),linewidth=1.5)+
    geom_point(data = filter(df_sel, c(sitename == "THA" & group == 400 & CampaignNum == CamN & Position==para$Position[2])),
               col=adjustcolor("cyan4",0.5)) +
    geom_line(data = df_predict[[3]],aes(col="DAV-L"),linewidth=1.5)+
    geom_point(data = filter(df_sel, c(sitename == "DAV" & group == 400 & CampaignNum == CamN & Position==para$Position[3])),
               col=adjustcolor("red1",0.5)) +
    geom_line(data = df_predict[[4]],aes(col="DAV-U"),linewidth=1.5)+
    geom_point(data = filter(df_sel, c(sitename == "DAV" & group == 400 & CampaignNum == CamN & Position==para$Position[4])),
               col=adjustcolor("red4",0.5))+
    scale_color_manual("",values = c("DAV-L"="red1","DAV-U"="red4","THA-L"="cyan2","THA-U"="cyan4"))+
    labs(
      x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
      y = expression(A[net] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
    ) +
    ylim(-1,15)+
    annotate(geom="text",x=100,y=14,label=paste0(para$CampaignNum),size=6)+
    theme_bw()+
    theme(axis.text = element_text(size=14),
          axis.title = element_text(size=16),
          legend.text = element_text(size=14),
          legend.position = c(0.7,0.82),
          legend.background = element_blank()
          )
  if(xlab==FALSE){
    p_plot<-p_plot+
      xlab("")
  }
  if(ylab==FALSE){
    p_plot<-p_plot+
      ylab("")
  }
  if(legend==FALSE){
    p_plot<-p_plot+
      theme(legend.position = "none")
  }
  return(p_plot)
}

###
# df<-dat
# #fitting lines:
# df.Tha<-LRC_THA
# df.Dav<-LRC_DAV
# CamN<-"C1"
# xlab<-TRUE
# ylab<-TRUE
p1<-plot_LRC_fun(dat,LRC_THA,LRC_DAV,"C1",FALSE,TRUE,TRUE)
p2<-plot_LRC_fun(dat,LRC_THA,LRC_DAV,"C2",FALSE,FALSE,FALSE)
p3<-plot_LRC_fun(dat,LRC_THA,LRC_DAV,"C3",FALSE,FALSE,FALSE)
p4<-plot_LRC_fun(dat,LRC_THA,LRC_DAV,"C4",FALSE,TRUE,FALSE)
p5<-plot_LRC_fun(dat,LRC_THA,LRC_DAV,"C5",TRUE,FALSE,FALSE)
p6<-plot_LRC_fun(dat,LRC_THA,LRC_DAV,"C6",FALSE,FALSE,FALSE)
#merge the plots:
library(cowplot)
p_Amax<-plot_grid(p1,p2,p3,
          p4,p5,p6,align = "hv")

###save the plots:
ggsave(p_Amax,filename = paste("./manuscript/Amax_var_campaigns.png"),
       width = 10,height = 8)
###########
#B.plotting other variables
###########
#specifically: E(transpiration rate); stomatal conductance to water vapor(gsw);
#gtc: Total conductance to CO2
df.physio<-df.Amax.merge %>%
  #Gs:stomatal conductance-->refer to Tang et al., 2022:
  #https://nph.onlinelibrary.wiley.com/doi/full/10.1111/nph.18649
  mutate(Gs=E*VPDleaf/Pa)%>%
  select(sitename,ID,CampaignNum,Position,Gs,E,gsw,gtc)%>%
  filter(Gs>0 & E>0)
##save the data:
save(df.physio,file = paste0("./data/LIcor/Gs_E.cleaned.RDA"))

plot_physio<-function(df,plot_var){
  # df<-df.physio
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
  if(plot_var=="Gs"){
    p_plot<-p_plot+
      ylab(expression(Gs ~ "(" * mol ~ m^{-2} ~ s^{-1} * ")"))
  }

  if(plot_var=="E"){
    p_plot<-p_plot+
    ylab(expression(Transpiration ~ "(" * mol ~ m^{-2} ~ s^{-1} * ")"))
  }
  #
  if(plot_var=="gsw"){
    p_plot<-p_plot+
      ylab(expression(Gsw ~ "(" * mol ~ m^{-2} ~ s^{-1} * ")"))
  }
 return(p_plot) 
}
###plotting
plot_E<-plot_physio(df.physio,"E")
plot_Gs<-plot_physio(df.physio,"Gs")

#save the plots:
ggsave(plot_E,filename = paste("./manuscript/transpiration_campaigns.png"),
       width = 9,height = 6)
ggsave(plot_Gs,filename = paste("./manuscript/Gs_campaigns.png"),
       width = 9,height = 6)
