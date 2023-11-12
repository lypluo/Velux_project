recomp_6400_adjA<-function (df, S = 6, K = 0.5) 
{
  # read_bat_6400 <- match.fun(read_bat_6400)
  # df <- read_bat_6400(file_dir, header_line = header_line, 
  #                     data_start = data_start)
  df<-df
  df$Trmmol = df$Flow * (df$H2OS - df$H2OR)/(1000 - df$H2OS)/(100 * 
                                                                S) * 1000
  df$Photo = df$Flow * (df$CO2R - df$CO2S * (1000 - df$H2OR)/(1000 - 
                                                                df$H2OS))/(100 * S)
  df$SVTleaf = 0.61365 * exp(17.502 * df$CTleaf/(240.97 + 
                                                   df$CTleaf))
  df$h2o_i = df$SVTleaf * 1000/df$Press
  df$CndTotal = df$Trmmol * ((1000 - (df$h2o_i + df$H2OS)/2))/(df$h2o_i - 
                                                                 df$H2OS)/1000
  df$BLC_1 = S * df$BLCslope + df$BLCoffst
  df$Kf = (K^2 + 1)/(K + 1)^2
  df$Cond = 1/(1/df$CndTotal - df$Kf/df$BLC_1)
  df$gtc = 1/(1.6/df$Cond + 1.378 * df$Kf/df$BLC_1)
  df$Ci = ((df$gtc - df$Trmmol/2000) * df$CO2S - df$Photo)/(df$gtc + 
                                                              df$Trmmol/2000)
  return(df)
}
