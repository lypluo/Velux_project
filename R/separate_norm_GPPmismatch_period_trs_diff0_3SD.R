#---------------------------------------------------------
#function used to separate the GPP overestimation period
#---------------------------------------------------------
##Seprating the time period when model early estimation of GPP

### Part1: find the method to determine the period that with early GPP estimation
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)

####1)determine the green-up period
library(phenopix)
library(zoo)
#function: to smooth the time series
extract_smooth_y<-function(x,df){
  # x<-df_new$gpp_mod_FULL_f
  # df<-0.05
  x_sm<-SplineFit(x,df=df)
  y<-x_sm$fit$predicted
  return(y)
}

#function: to determine the green-up period
#using the threshold method:
Deter_greenup<-function(df,pos_max){
  # df<-df_subset
  # pos_max<-pos_max

  df_sel_sos<-df %>%
    dplyr::filter(doy<=pos_max)
  #1) calculate the amplitude of the time series
  #based on the 10% or 25% of amplitude:
  mn_sos<-min(df_sel_sos$gpp_mod_norm_sm,na.rm = T)
  mm<-max(df_sel_sos$gpp_mod_norm_sm,na.rm = T)
  amp_sos<-mm-mn_sos

  trs_10_sos<-mn_sos+0.1*amp_sos
  trs_25_sos<-mn_sos+0.25*amp_sos

  #2)determine the start of season(sos10, and sos25)
  #set the sos beyond Feburary
  N_length<-length(df_sel_sos$gpp_mod_norm_sm)
  sos10<-which.min(abs(as.numeric(df_sel_sos$gpp_mod_norm_sm[60:N_length] - trs_10_sos)))+59
  sos25<-which.min(abs(as.numeric(df_sel_sos$gpp_mod_norm_sm[60:N_length] - trs_25_sos)))+59
  #3)the end of the green-up period
  peak<-pos_max
  #4)additional:also determine eos10 and eos25
  df_sel_eos<-df %>%
    dplyr::filter(doy>=pos_max)
  mn_eos<-min(df_sel_eos$gpp_mod_norm_sm,na.rm = T)
  amp_eos<-mm-mn_eos
  #
  trs_10_eos<-mn_eos+0.1*amp_eos
  trs_25_eos<-mn_eos+0.25*amp_eos
  #
  eos10<-which.min(abs(as.numeric(df_sel_eos$gpp_mod_norm_sm - trs_10_eos)))
  eos25<-which.min(abs(as.numeric(df_sel_eos$gpp_mod_norm_sm - trs_25_eos)))
  #adjust the eos 10 and eos25
  eos25<-eos25+peak-1
  eos10<-eos10+peak-1
  #
  pos_agg<-data.frame(sos10=sos10,sos25=sos25,peak=peak,eos25=eos25,eos10=eos10)
  #plotting demonstration:
  # plot(df_subset$date,df$gpp_mod_norm,ylab = "Norm GPP",xlab = "",pch=".")
  # lines(df_subset$date,df$gpp_mod_roll20,ylab = "Norm GPP",xlab = "",lwd=1.2,col="red")
  # abline(h=c(trs_10_sos,trs_25_sos,trs_25_eos,trs_10_eos),col="red",lty=2)
  # abline(v=df_subset$date[c(sos10,sos25,eos25,eos10)],col="red")
  # text(df_subset$date[c(sos10,sos25,eos25,eos10)],0.8,labels = c("sos10","sos25","eos25","eos10"),col = "red",pos = c(2,4,2,4))
  # abline(v=df_subset$date[c(peak)],col="red")
  # text(df_subset$date[c(peak)],0.8,labels = c("peak"),col = "red",pos = c(4))
  return(pos_agg)
}
#
##function to seprate the early GPP estimation period:
#after checking the data(data avaiablity, data quality) in all the sites(47 sites)-->at the end, I selecting 39 sites at end:

sep_data_indiffY_sepMismatch<-function(ddf,degf,Clim,PFT,site_name,avai_years){
  # ddf<-df.merge
  # degf<-0.05  #degree of freedom for the spline smooth
  # Clim<-"Dfc"
  # PFT<-"ENF"
  # site_name<-"CH-Dav"
  # avai_years<-c(1997:2020)

  ##remember add dplyr:: for instance as sometimes two same name functions might conflict with each other
  df<-ddf %>%
    dplyr::filter(sitename==site_name) %>%
    dplyr::mutate(doy = yday(date), Year=year(date)) %>%
    dplyr::filter(Year %in% avai_years)
  #
  Years_unique<-unique(df$Year)

  #-------------------------------------------------------------------------------
  #Step1: normlization for all the years in one site
  #normalized the gpp_obs and gpp_mod using the gpp_max(95 percentile of gpp)
  #-------------------------------------------------------------------------------
  gpp_max_obs<-quantile(df$gpp_obs,0.95,na.rm = T)
  gpp_max_mod<-quantile(df$gpp_mod,0.95,na.rm = T)
  df$gpp_mod_norm<-df$gpp_mod/gpp_max_mod
  df$gpp_obs_norm<-df$gpp_obs/gpp_max_obs
  #first to gap-fill the ts to enable the ts smoothing
  #then smooth the time series of all the years uisng the spline(did not use at the moment)
  #hints:do not gap-fill gpp_obs if there are long gaps in the time sereis-->for biases calculation in step 4-->need to gapfill each year
  if(length(df$gpp_obs[!is.na(df$gpp_obs)])>30){
    df_new<-c()
    for(i in 1:length(Years_unique)){
      df_t<-df[df$Year==Years_unique[i],]
      df_t<-df_t %>%
        mutate(gpp_mod_norm_f=na.fill(gpp_mod_norm,c("extend")),gpp_obs_norm_f= na.fill(gpp_obs_norm,c(NA,"extend",NA))) %>%
        mutate(gpp_mod_norm_sm = extract_smooth_y(gpp_mod_norm_f,degf))
      df_new<-rbind(df_new,df_t)
    }
    #coercian gpp_obs to numeric as sometimes long NA makes the rbind wrong to convert to numeric format
    df_new$gpp_obs_norm_f<-as.numeric(df_new$gpp_obs_norm_f)
  }
  #
  N<-length(df$gpp_mod)
  #-------------------------------------------------------------------------------
  #Step 2:Determine the green-up period for each year(using spline smoothed values):
  #followed analysis is based on the normlized "GPP_mod"time series(determine earlier sos)
  #-------------------------------------------------------------------------------
  #
  library(zoo)
  pos_diffYears<-c()
  #also subset data beyond the green-up period
  df_outgreenup<-c()
  df_final<-c()
  for(i in 1:length(Years_unique)){
    df_subset<-df_new[df_new$Year==Years_unique[i],]
    #spot t
    #calculate the rollmean
    df_subset<-df_subset%>%
      dplyr::mutate(
        gpp_mod_roll5=rollapply(df_subset$gpp_mod_norm_f,5,mean,fill=NA,align="center"),
        gpp_mod_roll7=rollapply(df_subset$gpp_mod_norm_f,7,mean,fill=NA,align="center"),
        gpp_mod_roll10=rollapply(df_subset$gpp_mod_norm_f,10,mean,fill=NA,align="center"),
        gpp_mod_roll15=rollapply(df_subset$gpp_mod_norm_f,15,mean,fill=NA,align="center"),
        gpp_mod_roll20=rollapply(df_subset$gpp_mod_norm_f,20,mean,fill=NA,align="center"),
        gpp_obs_roll5=rollapply(df_subset$gpp_obs_norm_f,5,mean,fill=c(NA,"extend",NA),align="center"),
        gpp_obs_roll7=rollapply(df_subset$gpp_obs_norm_f,7,mean,fill=c(NA,"extend",NA),align="center"),
        gpp_obs_roll10=rollapply(df_subset$gpp_obs_norm_f,10,mean,fill=c(NA,"extend",NA),align="center"),
        gpp_obs_roll15=rollapply(df_subset$gpp_obs_norm_f,15,mean,fill=c(NA,"extend",NA),align="center"),
        gpp_obs_roll20=rollapply(df_subset$gpp_obs_norm_f,20,mean,fill=c(NA,"extend",NA),align="center"),
        temp_obs_roll20=rollapply(df_subset$temp_day_fluxnet2015,20,mean,fill=c(NA,"extend",NA),align="center")
      )
    # pos_sim_max<-match(max(df_subset$gpp_mod_roll20,na.rm = T),df_subset$gpp_mod_roll20)
    pos_sim_max<-match(max(df_subset$gpp_mod_norm_sm),df_subset$gpp_mod_norm_sm)
    pos_max<-pos_sim_max
    #finding out the start and end of the green-up period:
    pos_agg<-Deter_greenup(df_subset,pos_max)
    pos_diffYears<-rbind(pos_diffYears,pos_agg)
    #separate the data beyond greenup period:
    # pos_beyond<-c(1:c(pos_agg$sos10-1),c(c(pos_agg$peak+1):length(df_subset$date)))
    #pos_beyond<-c(pos_agg$peak+1):length(df_subset$date)
    pos_beyond<-c(1:c(pos_agg$sos10-1),c(c(pos_agg$eos10+1):length(df_subset$date)))
    t_outgreenup<-df_subset[pos_beyond,]
    df_outgreenup<-rbind(df_outgreenup,t_outgreenup)
    df_final<-rbind(df_final,df_subset)
  }
  rownames(pos_diffYears)<-Years_unique
  #-------------------------------------------------------------------------------
  #Step 3:rolling mean of GPPobs and GPPmod for data for all the years(moving windown:5,7,10, 15, 20days)
  #also for the data beyond green-up period-->moving to second step
  #-------------------------------------------------------------------------------
  # df_outgreenup<-df_outgreenup%>%
  #     mutate(gpp_mod_roll5=rollapply(df_outgreenup$gpp_mod_norm,5,mean,na.rm=T,fill=NA,align="center"),
  #            gpp_mod_roll7=rollapply(df_outgreenup$gpp_mod_norm,7,mean,na.rm=T,fill=NA,align="center"),
  #            gpp_mod_roll10=rollapply(df_outgreenup$gpp_mod_norm,10,mean,na.rm=T,fill=NA,align="center"),
  #            gpp_mod_roll15=rollapply(df_outgreenup$gpp_mod_norm,15,mean,na.rm=T,fill=NA,align="center"),
  #            gpp_mod_roll20=rollapply(df_outgreenup$gpp_mod_norm,20,mean,na.rm=T,fill=NA,align="center"),
  #            gpp_obs_roll5=rollapply(df_outgreenup$gpp_obs_norm,5,mean,na.rm=T,fill=NA,align="center"),
  #            gpp_obs_roll7=rollapply(df_outgreenup$gpp_obs_norm,7,mean,na.rm=T,fill=NA,align="center"),
  #            gpp_obs_roll10=rollapply(df_outgreenup$gpp_obs_norm,10,mean,na.rm=T,fill=NA,align="center"),
  #            gpp_obs_roll15=rollapply(df_outgreenup$gpp_obs_norm,15,mean,na.rm=T,fill=NA,align="center"),
  #            gpp_obs_roll20=rollapply(df_outgreenup$gpp_obs_norm,20,mean,na.rm=T,fill=NA,align="center")
  #            )
  #-------------------------------------------------------------------------------
  #Step 4:Fit the Guassian norm distribution for residuals beyond the green-up period
  #-------------------------------------------------------------------------------
  library(MASS)
  #calculate the residuals
  res.gpp<-data.frame(date=df_outgreenup$date,res=c(df_outgreenup$gpp_mod_roll20 - df_outgreenup$gpp_obs_roll20))
  res.gpp<-res.gpp[!is.na(res.gpp$res),]
  fit_res<-fitdistr(res.gpp$res,"normal")
  para <- fit_res$estimate
  # hist(res_gpp,probability = TRUE)
  # curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)
  mu<-mean(res.gpp$res,na.rm = T)
  p_density_res_outgreenup<-ggplot(res.gpp, aes(x=res))+
    geom_histogram(aes(y=..density..),binwidth = 0.01,position="identity", alpha=0.3,fill="white",col="green3",)+
    geom_density(col="forestgreen",size=1.05)+
    stat_function(fun = dnorm, args = list(mean = mean(res.gpp$res), sd = sd(res.gpp$res)),size=1.05,col="black")+
    geom_vline(xintercept=mu,linetype="dashed",size=1.05)+
    theme_minimal()
  #-------------------------------------------------------------------------------
  #step 5:determine the GPP overestimation("over_estim") within green-up and growing season(sos-eos)
  #and GPP overstimation happens less than 10 degree in the spring
  # ("is_event_less10")
  #-------------------------------------------------------------------------------
  Sys.setenv(tz="UTC")
  df_diffYears<-c()
  for(i in 1:length(Years_unique)){
    df_subset<-df_final[df_final$Year==Years_unique[i],]
    #A.
    #a1.add one column to indicate the "greenup"
    df_subset$greenup<-rep("no",length(df_subset$date))
    pos_agg<-pos_diffYears[i,]
    #green-up date=yes
    df_subset[c(df_subset$doy>=pos_agg$sos10) & c(df_subset$doy<=pos_agg$peak),]$greenup<-"yes"
    #a2.add one column to indicate the GS(growing season)
    df_subset$GS<-rep("no",length(df_subset$date))
    #GS date=yes
    df_subset[c(df_subset$doy>=pos_agg$sos10) & c(df_subset$doy<=pos_agg$eos10),]$GS<-"yes"

    #B.
    #b1.add one column to indciate "is_event"
    #set the biases(gppmod-gppobs) are positive and bigger than 1.2sd as the "is_event"
    df_subset$gpp_res<-df_subset$gpp_mod_roll20-df_subset$gpp_obs_roll20
    #remove the NA
    # df_subset_rmNA<-df_subset[!is.na(df_subset$gpp_res),]
    thrs<-para[2]*3
    #"overestimation" in the growing season
    df_subset$over_estim<-rep("no",length(df_subset$date))
    #for the data is not equal to NA
    if(nrow(df_subset[c(df_subset$GS=="yes")&c(df_subset$gpp_res>thrs)&c(!is.na(df_subset$gpp_res)),])>0){
      df_subset[c(df_subset$GS=="yes")&c(df_subset$gpp_res>thrs)&c(!is.na(df_subset$gpp_res)),]$over_estim<-"yes"}
    #for the data is equal to NA-->not used any more
    # temp_subset<-df_subset[c(df_subset$GS=="yes")&c(df_subset$gpp_res>thrs)&c(is.na(df_subset$gpp_res)),]
    # if(length(temp_subset$date)>0){
    # df_subset[c(df_subset$GS=="yes")&c(df_subset$gpp_res>thrs)&c(is.na(df_subset$gpp_res)),]$over_estim<-rep(NA,length(temp_subset$date))
    # }
    #b2.add on column to indicate is_event-->GPP overestimation in the green-up period
    #"is_event"should be in the green-up period
    df_subset$is_event<-rep("no",length(df_subset$date))
    #for the data is not equal to NA
    if(nrow(df_subset[c(df_subset$greenup=="yes")&c(!is.na(df_subset$gpp_res)&c(df_subset$gpp_res>thrs)),])>0){
      df_subset[c(df_subset$greenup=="yes")&c(!is.na(df_subset$gpp_res)&c(df_subset$gpp_res>thrs)),]$is_event<-"yes"}
    #for the data is equal to NA
    if(length(df_subset[c(df_subset$greenup=="yes")&c(is.na(df_subset$gpp_res)),]$is_event)>0){
      df_subset[c(df_subset$greenup=="yes")&c(is.na(df_subset$gpp_res)),]$is_event<-"NA"
    }
    #b3. additional filter: only selecting the time period in the first 2/3 green-up for the determination-->
    greenup_subset<-subset(df_subset,greenup=="yes")
    sel_thres<-min(greenup_subset$doy)+round(c(max(greenup_subset$doy)-min(greenup_subset$doy))*2/3,0)
    df_subset[df_subset$doy>sel_thres,]$is_event<-"no"
    ############################
    #plotting
    #for the "GS"
    pos_GS<-df_subset$doy[which(df_subset$GS=="yes")]
    ##plotting
    # par(mfrow=c(2,1))
    # plot(df_subset$date,df_subset$temp_day_fluxnet2015,pch=1)
    # points(df_subset$date,df_subset$temp_obs_roll20,col="red")
    # abline(h=c(0,10,15),col="red")
    #
    # plot(df_subset$date,df_subset$gpp_mod_roll20,col="red",ylim=c(-0.1,1.2))
    # points(df_subset$date,df_subset$gpp_obs_roll20)
    # abline(v=df_subset$date[c(min(pos_GS),max(pos_GS))])
    #method:remove the positon of pos_overstimation when air temperature >10 (should be in the green-up period)
    pos_temp_less10<-df_subset$doy[which(c(df_subset$is_event=="yes") & c(df_subset$temp_obs_roll20<=10))]
    # abline(v=df_subset$date[c(min(pos_temp_less10),max(pos_temp_less10))],col="red")
    df_subset$is_event_less10<-df_subset$GS
    pos_rm<-setdiff(pos_GS,pos_temp_less10)
    df_subset$is_event_less10[pos_rm]<-rep("no",length(pos_rm))
    ##############################
    df_subset$date<-as.POSIXct(strptime(df_subset$date,format = "%Y-%m-%d"))
    #c.merge the data.frame
    df_diffYears<-rbind(df_diffYears,df_subset)
  }
  #-------------------------------------------------------------------------------
  #step 6:Evaluation "is_event"-->visualization and stats
  #-------------------------------------------------------------------------------
  #plotting+stats
  p_diffYears<-c()
  Pfalse_diffYears<-c()
  for(i in 1:length(Years_unique)){
    df_subset<-df_diffYears[df_diffYears$Year==Years_unique[i],]
    #a.stats:positive false%= days(gpp_mod underestimation for days flaged as is_event)/days(is_event)
    df_stats<-subset(df_subset,is_event=="yes"&!is.na(gpp_obs))
    df_stats$flag<-rep("overEsti",length(df_stats$date))
    for(k in 1:length(df_stats$date)){
      ifelse(df_stats$gpp_mod_FULL[k]-df_stats$gpp_obs[k]<0,df_stats$flag[k]<-"underEsti",df_stats$flag[k]<-df_stats$flag[k])
    }
    Pfalse<-round(c(length(which(df_stats$flag=="underEsti"))/length(df_stats$date)*100),1)
    Pfalse_diffYears<-rbind(Pfalse_diffYears,Pfalse)
    #b.plotting:demonstration the original value + moving average + dividing lines
    pos_agg<-pos_diffYears[i,]
    pos_overestim<-df_subset$doy[which(df_subset$over_estim=="yes")]
    pos_isevent<-df_subset$doy[which(df_subset$is_event=="yes")]
    pos_isevent_less10<-df_subset$doy[which(df_subset$is_event_less10=="yes")]

    rect.coord_GS=data.frame(x1=df_subset$date[pos_agg[,"sos10"]],x2=df_subset$date[pos_agg[,"eos10"]], y1=min(df_subset$gpp_mod_norm,na.rm = T)-0.05, y2=max(df_subset$gpp_mod_norm,na.rm = T)+0.02)
    rect.coord_isevent=data.frame(x1=df_subset$date[min(pos_isevent)],x2=df_subset$date[max(pos_isevent)], y1=min(df_subset$gpp_mod_norm,na.rm = T)-0.05, y2=max(df_subset$gpp_mod_norm,na.rm = T)+0.02)
    rect.coord_isevent_less10=data.frame(x1=df_subset$date[min(pos_isevent_less10)],x2=df_subset$date[max(pos_isevent_less10)], y1=min(df_subset$gpp_mod_norm,na.rm = T)-0.05, y2=max(df_subset$gpp_mod_norm,na.rm = T)+0.02)
    #

    gg_plot<-ggplot() +
      geom_point(data=df_subset,aes(x=date,y = gpp_mod_norm),col=adjustcolor("tomato",0.5),pch=1)+
      geom_line(data=df_subset,aes(x=date,y = gpp_mod_roll20,col="modelled"),lwd=1.2)+
      geom_point(data=df_subset,aes(x=date,y = gpp_obs_norm),col=adjustcolor("black",0.5),pch=1)+
      geom_line(data=df_subset,aes(x=date,y = gpp_obs_roll20,col="observed"),lwd=1.2)+
      geom_vline(xintercept = df_subset$date[pos_agg[,"sos10"]],col="forestgreen",lty=2,lwd=1.2)+
      geom_vline(xintercept = df_subset$date[pos_agg[,"peak"]],col="forestgreen",lty=2,lwd=1.2)+
      scale_color_manual("",values = c("modelled"="red","observed"="black")) +
      labs(title = paste0(Clim,"-",PFT,"-",site_name,":",Years_unique[i]), x = "", y = "norm GPP" )+  #(g C m-2 d-1 )
      geom_rect(data=rect.coord_GS,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="green3",alpha=0.1)+
      geom_vline(xintercept = df_subset[df_subset$over_estim=="yes",]$date,col=adjustcolor("steelblue2",0.3))+
      geom_rect(data=rect.coord_isevent,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="blue",alpha=0.3)+
      # geom_rect(data=rect.coord_isevent_less10,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="orange",alpha=0.5)+
      theme_classic()+
      theme(legend.position = c(0.15,0.9),legend.background = element_blank())

    ## merge the plot
    ifelse(i==1,gg_plot<-gg_plot+theme(),gg_plot<-gg_plot+theme(legend.position = "none"))
    # plot(gg_plot)
    p_diffYears[[i]]<-gg_plot
  }
  Pfalse_diffYears<-as.data.frame(Pfalse_diffYears)
  names(Pfalse_diffYears)<-"Pflase";rownames(Pfalse_diffYears)<-Years_unique
  #-------------------------------------------------------------------------------
  #step 7:Summary
  #-------------------------------------------------------------------------------
  df_agg<-c()
  df_agg$pos_agg<-pos_diffYears
  df_agg$df_agg<-df_diffYears
  df_agg$p_den_outgreenup<-p_density_res_outgreenup
  df_agg$Pflase<-Pfalse_diffYears
  df_agg$p_isevent<-p_diffYears
  return(df_agg)
}

# ###one test example:###
# site_name<-"CH-Dav"
# df<-df.merge %>%
#   dplyr::filter(sitename==site_name)
# df_andPlot_CH_Dav<-sep_data_indiffY_sepMismatch(df.merge,0.05,"Dfc","ENF",site_name,c(1997:2020))
# length(df_andPlot_CH_Dav$p_isevent)
# p_diffYears<-df_andPlot_CH_Dav$p_isevent
# p_merge_1<-plot_grid(p_diffYears[[1]],p_diffYears[[2]],p_diffYears[[3]],
#                    p_diffYears[[4]],p_diffYears[[5]],p_diffYears[[6]],
#                    p_diffYears[[7]],p_diffYears[[8]],p_diffYears[[9]],
#                    p_diffYears[[10]],p_diffYears[[11]],p_diffYears[[12]],
#                    labels = "auto",ncol=3,label_size = 12,align = "hv")
# p_merge_2<-plot_grid(p_diffYears[[13]],p_diffYears[[14]],p_diffYears[[15]],
#                      p_diffYears[[16]],p_diffYears[[17]],p_diffYears[[18]],
#                      p_diffYears[[19]],p_diffYears[[20]],p_diffYears[[21]],
#                      p_diffYears[[22]],p_diffYears[[23]],p_diffYears[[24]],
#                      labels = "auto",ncol=3,label_size = 12,align = "hv")
# plot(p_merge_1)
# plot(p_merge_2)

