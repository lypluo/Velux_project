####Aim: update the LIcor measurements such as photosynthesis(A) after adjusting the leaf area:
recomp_6800_adjA<-function (df, S = 6, K = 0.5) 
{
  # read_regex68 <- match.fun("read_regex68")
  # df <- read_regex68(file_dir)
  df<-df
  
  df$E = df$CorrFact * df$Flow * (df$H2O_s - df$H2O_r)/(100 * 
                                                          S * (1000 - df$CorrFact * df$H2O_s))
  df$A = df$Flow * df$CorrFact * (df$CO2_r - df$CO2_s * (1000 - 
                                                           df$CorrFact * df$H2O_r)/(1000 - df$CorrFact * df$H2O_s))/(100 * 
                                                                                                                       S)
  # df$gbw = df$blfa_3 + df$blfa_2 * S + df$blfa_1 * S * S -->For Davos, did not have the measurements for blfa_3/_2/_1
  df$gtw = df$E * (1000 - (1000 * 0.61365 * exp(17.502 * df$TleafCnd/(240.97 + 
                                                                        df$TleafCnd))/(df$Pa + df$VPcham) + df$H2O_s)/2)/(1000 * 
                                                                                                                            0.61365 * exp(17.502 * df$TleafCnd/(240.97 + df$TleafCnd))/(df$Pa + 
                                                                                                                                                                                          df$VPcham) - df$H2O_s)
  df$gsw = 2/((1/df$gtw - 1/df$gbw) + sqrt((1/df$gtw - 1/df$gbw) * 
                                             (1/df$gtw - 1/df$gbw) + 4 * K/((K + 1) * (K + 1)) * 
                                             (2 * 1/df$gtw * 1/df$gbw - 1/df$gbw * 1/df$gbw)))
  df$gtc = 1/((K + 1)/(df$gsw/1.6) + 1/(df$gbw/1.37)) + K/((K + 
                                                              1)/(df$gsw/1.6) + K/(df$gsw/1.37))
  df$Ci = ((df$gtc - df$E/2) * df$Ca - df$A)/(df$gtc + df$E/2)
  return(df)
}
