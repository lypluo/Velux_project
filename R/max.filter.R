max.filter <- function(Data, name, act.opts,...) {
  # library(zoo)
  # Data=df.VIs.Dav
  # name="NDVI";
  # act.opts<-data.frame(w=15,qt=0.5)
  
  Data$daily.time<-as.POSIXct(strftime(Data$Date, format="%Y-%m-%d"))
  Data$doy<-format(Data$daily.time,format = '%j')
  
  w <- act.opts$w
  qt <-act.opts$qt
  .max.fun <- function(x) {
    quantile(x, qt, na.rm = TRUE)
  }

  computed.frequency <- median(diff(as.numeric(Data$Date)), 
                               na.rm = TRUE)
  Data.per.day <- median(aggregate(Data, by = list(Data$daily.time), 
                                   FUN = length)[, 2], na.rm = T)
  true.window <- Data.per.day * w
  #n<-length(name)
  filtered.VIs<-data.frame(time=Data$Date,doy=Data$doy,daily.time=Data$daily.time);
  name_filtered<-names(filtered.VIs)
  
  ##
  new.max <- rollapply(Data[, name], FUN = .max.fun, width = true.window,
                       fill = c('extend',NA,'extend'))
  filtered.VIs<-cbind(filtered.VIs,new.max)
  name_filtered<-c(name_filtered,paste(name,'.max.','filtered',sep=''))
  names(filtered.VIs)<-name_filtered
  rm(new.max)
  
  pos.time <- which(names(filtered.VIs) == "time" | names(filtered.VIs) == 
                      "doy" | names(filtered.VIs) == "daily.time")
  daily.agg <- aggregate(filtered.VIs[,-pos.time], by = list(filtered.VIs$daily.time), 
                         FUN = "median", na.rm = T)
  names(daily.agg) <- c("Date",paste0(name,'.max.','filtered'))
  return(daily.agg)
}
