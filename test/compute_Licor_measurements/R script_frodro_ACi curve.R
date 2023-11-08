###################################################
#This code is sent by Luo Na->thanks!
###################################################
setwd("D:/Github/Velux_project/test/compute_Licor_measurements/")
# setwd("D:/【Desktop Bulks】/work directory_R/data_frodro")
##get VJmax values____________________________####
##read in data####
require(readxl)##read in data
#data.aciori=read_excel("data_frodro_ACicurve.xlsx",sheet = "all")
data.aciori=read_excel("data_frodro_all_GasEx.xlsx",sheet = "all_1st2nd3rdcurves")

# attach(data.aciori)
str(data.aciori)## to view the name of variables
##merge variables into TRE
library(dplyr)
data.aci <- data.aciori %>% 
  mutate(TRE = paste(curveID,subtitle, frost,water, treeID, sep = "_"))
str(data.aci)
head(data.aci)
colnames(data.aci)
##rename columns to make it correspond to the arguments of the package
colnames(data.aci)[colnames(data.aci)=="A"] <- "Photo"
colnames(data.aci)[colnames(data.aci)=="Q"] <- "PARi"
colnames(data.aci)[colnames(data.aci)=="Ca"] <- "CO2S"
colnames(data.aci)[colnames(data.aci)=="Tleaf"] <- "Tleaf1"
colnames(data.aci)[colnames(data.aci)=="Tair"] <- "Tleaf"

##clean data_remove rows when Ci<0&Ci>2000

data.aci_reduced<-data.aci[data.aci$Ci>0&data.aci$Ci<2000,]

nrow(data.aci_reduced)
nrow(data.aci)

##plantecophys package####
citation("plantecophys")
packageVersion("plantecophys")


##for all campaign####
##using a for loop to get all results from multiple curves
#View(data.aci_reduced)
require(plantecophys)
##to view the curves
par(mfrow = c(1, 2))

# Open a PNG graphics device
# png("plot.png", width = 800, height = 600, res = 300)  # Set appropriate width, height, and resolution
for (i in 1:5) {
  ti<-subset(data.aci_reduced,curveID==i)
  t<- fitaci(ti, Tcorrect=FALSE, Tleaf=ti$Tleaf, fitmethod = "default",fitTPU=TRUE)
  c<-data.frame(round(coef(t),2))
  colnames(c)=c(ti[[1,176]])
  #data.aci<-cbind(data.aci,c)
  plot(t,addlegend=T)
  title(paste("treeID :",ti[[1,176]]))
  mtext(paste("Vcmax =",c[1,],"/","Jmax =",c[2,],"/","Rd =",c[3,]),col="red")
  #text = element_text(size=25)
}

# dev.off()
##to store results for all curves#####
which(names(data.aci) == "TRE")
data.aci[1,176]
which(names(data.aci) == "curveID")
data.aci[1,6]

c_list <- list()
for (i in 1:247) {
  ti <- subset(data.aci_reduced, curveID == i)
  t <- fitaci(ti, Tcorrect = FALSE, Tleaf = ti$Tleaf, fitmethod = "default", fitTPU = TRUE)
  c <- data.frame(round(coef(t), 2))
  colnames(c) <- c(ti[[1, 176]])
  c_list <- append(c_list, c)
}


VJmax_all<-data.frame(c_list)
write.csv(VJmax_all,"results_frodro_VJmax_all.csv")

##

c_list <- list()
for (i in 1:247) {
  ti <- subset(data.aci_reduced, curveID == i)
  t <- fitaci(ti, Tcorrect = FALSE, Tleaf = ti$Tleaf, fitmethod = "bilinear", fitTPU = TRUE)
  c <- data.frame(round(coef(t), 2))
  colnames(c) <- c(ti[[1, 176]])
  c_list <- append(c_list, c)
}


VJmax_all_bilinear<-data.frame(c_list)
write.csv(VJmax_all_bilinear,"results_frodro_VJmax_all_bllinear.csv")

##_________________________________________####
##Vcmax&Jmax analysis####
##read in data####
require(readxl)##read in data
data.vjmax=read_excel("data_frodro_VJmax.xlsx",sheet = "reduced")
attach(data.vjmax)
str(data.vjmax)## to view the name of variables
##merge frost ans water into treats

data.vjmax$treats <- ifelse(data.vjmax$frost == "CK" & data.vjmax$water == "con", "Control", 
                            ifelse(data.vjmax$frost == "CK" & data.vjmax$water == "dro", "Drought",
                                   ifelse(data.vjmax$frost == "fro" & data.vjmax$water == "con", "Frost",
                                          ifelse(data.vjmax$frost == "fro" & data.vjmax$water == "dro", "Frost+Drought","NA"))))



##subset data###
data.vjmax.1st=subset(data.vjmax,subtitle=="1st")
data.vjmax.1st

data.vjmax.2nd=subset(data.vjmax,subtitle=="2nd")
data.vjmax.2nd

data.vjmax.3rd=subset(data.vjmax,subtitle=="3rd")
data.vjmax.3rd


##summary####
summary.Vcmax<-data.vjmax %>%group_by(measuredate,subtitle,treats,species_abbr,frost,water) %>%
  get_summary_stats(Vcmax,type="full")
summary.Vcmax.sub<-summary.Vcmax[,c(1:7,16:19)]
summary.Vcmax.sub
##

summary.Jmax<-data.vjmax %>%group_by(measuredate,subtitle,treats,species_abbr,frost,water) %>%
  get_summary_stats(Jmax,type="full")
summary.Jmax.sub<-summary.Jmax[,c(1:7,16:19)]
summary.Jmax.sub

##

summary.vjmax<-rbind(summary.Vcmax.sub,summary.Jmax.sub)
write.csv(summary.vjmax,"summary_vjmax.csv")


##summary.treeID####
summary.Vcmax.treeID<-data.vjmax %>%group_by(measuredate,subtitle,treats,species_abbr,frost,water,treeID) %>%
  get_summary_stats(Vcmax,type="full")
summary.Vcmax.treeID.sub<-summary.Vcmax.treeID[,c(1:8,17:20)]
summary.Vcmax.treeID.sub
##

summary.Jmax.treeID<-data.vjmax %>%group_by(measuredate,subtitle,treats,species_abbr,frost,water,treeID) %>%
  get_summary_stats(Jmax,type="full")
summary.Jmax.treeID.sub<-summary.Jmax.treeID[,c(1:8,17:20)]
summary.Jmax.treeID.sub

##

summary.vjmax.treeID<-rbind(summary.Vcmax.treeID.sub,summary.Jmax.treeID.sub)
write.csv(summary.vjmax.treeID,"summary_vjmax_treeID.csv")



##lm.Vcmax.1st####
fit.1st.Vcmax<-lm(Vcmax~frost*species_abbr,data=data.vjmax.1st)
summary(fit.1st.Vcmax)##sig in fro
anova(fit.1st.Vcmax)
library(performance)
require(see)
check_model(fit.1st.Vcmax)##to view normality
shapiro.test(residuals(fit.1st.Vcmax))##yes
qqnorm(resid(fit.1st.Vcmax))##yes
qqline(resid(fit.1st.Vcmax))

#Skim the performance of the model
library(sjPlot)
require(ggplot2)

plot_model(fit.1st.Vcmax, terms=c("species_abbr", "frost"), 
           type="emm",show.data = T, dot.size = 1)+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

##lm.Vcmax_2nd####
fit.2nd.Vcmax<-lm(Vcmax~frost*species_abbr+
                    water*species_abbr+
                    frost*water,data=data.vjmax.2nd)
summary(fit.2nd.Vcmax)##sig in species & fro*species
anova(fit.2nd.Vcmax)
library(performance)
require(see)
check_model(fit.2nd.Vcmax)##to view normality
shapiro.test(residuals(fit.2nd.Vcmax))##no##take
qqnorm(resid(fit.2nd.Vcmax))##yes
qqline(resid(fit.2nd.Vcmax))
#Skim the performance of the model
library(sjPlot)
require(ggplot2)
plot_model(fit.2nd.Vcmax, terms=c("species_abbr", "frost","water"), type="emm",
           show.data = T, dot.size = 1)+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")


##lm.Vcmax_3rd####
fit.3rd.Vcmax<-lm(Vcmax~frost*species_abbr+
                    water*species_abbr+
                    frost*water,data=data.vjmax.3rd)
summary(fit.3rd.Vcmax)##sig in species & fro*species
anova(fit.3rd.Vcmax)
library(performance)
require(see)
check_model(fit.3rd.Vcmax)##to view normality
shapiro.test(residuals(fit.3rd.Vcmax))##no##take
qqnorm(resid(fit.3rd.Vcmax))##yes
qqline(resid(fit.3rd.Vcmax))
#Skim the performance of the model
library(sjPlot)
require(ggplot2)
plot_model(fit.3rd.Vcmax, terms=c("species_abbr", "frost","water"), type="emm",
           show.data = T, dot.size = 1)+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")


##Mulcomp_1st_____####
##contr####
require(emmeans)
pairs(emmeans(fit.1st.Vcmax, ~species_abbr),adjust = "tukey")
pairs(emmeans(fit.1st.Vcmax, ~frost|species_abbr),adjust = "tukey")
pairs(emmeans(fit.1st.Vcmax, ~species_abbr|frost),adjust = "tukey")
pairs(emmeans(fit.1st.Vcmax, ~species_abbr+frost),adjust = "tukey")

##
contr.1st.Vcmax.FinS<-pairs(emmeans(fit.1st.Vcmax, ~frost|species_abbr),adjust = "tukey")
contr.1st.Vcmax.FinS.df<-as.data.frame(contr.1st.Vcmax.FinS)
contr.1st.Vcmax.FinS.df.sub<-contr.1st.Vcmax.FinS.df[c(1:4),c(1,7)]
##add contr.label###
contr.1st.Vcmax.FinS.df.sub$contr.label <- ifelse(contr.1st.Vcmax.FinS.df.sub$"p.value"> 0.05 , "ns", 
                                                  ifelse(contr.1st.Vcmax.FinS.df.sub$"p.value">0.01, "*",
                                                         ifelse(contr.1st.Vcmax.FinS.df.sub$"p.value">0.001, "**",
                                                                ifelse(contr.1st.Vcmax.FinS.df.sub$"p.value"<=0.001, "***", "NA"))))


##add appendix rows 
contr.1st.Vcmax.FinS.df.sub
appendix.1st<-data.frame(species_abbr=c("AC","FS","QP","QR"),
                         contrast=c("CK - CK","CK - CK","CK - CK","CK - CK"),
                         p.value=c(1,1,1,1),contr.label=c("","","",""))

head(appendix.1st)
names(appendix.1st)<-c("species_abbr","contrast","p.value","contr.label")
appendix.1st
species_abbr=c("AC","FS","QP","QR")
contr.1st.Vcmax.FinS.df.sub1<-rbind(cbind(species_abbr,
                                          contr.1st.Vcmax.FinS.df.sub),
                                    appendix.1st)
contr.1st.Vcmax.FinS.df.sub1
##seperate column###
require(ggpubr)
library(stringr)#required for str_split()
##separate column
contr.1st.Vcmax.FinS.df.sub2<-contr.1st.Vcmax.FinS.df.sub1%>% 
  mutate(frost_ref = str_split(contrast, " - ", simplify = T)[,1])%>% 
  mutate(frost = str_split(contrast, " - ", simplify = T)[,2])
contr.1st.Vcmax.FinS.df.sub2
str(contr.1st.Vcmax.FinS.df.sub2)

##add water variable
contr.1st.Vcmax.FinS.df.sub3<-contr.1st.Vcmax.FinS.df.sub2%>%mutate(water=paste("con"))
contr.1st.Vcmax.FinS.df.sub3
str(contr.1st.Vcmax.FinS.df.sub3)

##
contr.1st.Vcmax.FinS.df.sub3$treats <- ifelse(contr.1st.Vcmax.FinS.df.sub3$frost== "CK" & contr.1st.Vcmax.FinS.df.sub3$water == "con", "Control", 
                                              ifelse(contr.1st.Vcmax.FinS.df.sub3$frost == "fro" & contr.1st.Vcmax.FinS.df.sub3$water == "con", "Frost","NA"))
contr.1st.Vcmax.FinS.df.sub3
##

##cld####
require(multcomp)
cld(emmeans(fit.1st.Vcmax, ~species_abbr),Letters = letters)
cld(emmeans(fit.1st.Vcmax, ~frost|species_abbr),Letters = letters)
cld(emmeans(fit.1st.Vcmax, ~species_abbr|frost),Letters = letters)
cld(emmeans(fit.1st.Vcmax, ~species_abbr:frost),Letters = letters)
##
cld.1st.Vcmax.FinS<-cld(emmeans(fit.1st.Vcmax, ~frost|species_abbr),Letters = letters)
cld.1st.Vcmax.FinS.df<-as.data.frame(cld.1st.Vcmax.FinS)
cld.1st.Vcmax.FinS.df.sub<-cld.1st.Vcmax.FinS.df[c(1:8),c(1,8)]
cld.1st.Vcmax.FinS.df.sub
species_abbr<-c("AC","AC","FS","FS","QP","QP","QR","QR")
cld.1st.Vcmax.FinS.df.sub1<-cbind(species_abbr,cld.1st.Vcmax.FinS.df.sub)
cld.1st.Vcmax.FinS.df.sub1 <-cld.1st.Vcmax.FinS.df.sub1 %>% 
  mutate(water = paste("con"))
cld.1st.Vcmax.FinS.df.sub1
names(cld.1st.Vcmax.FinS.df.sub1)<-c("species_abbr","frost","cld.group","water")
cld.1st.Vcmax.FinS.df.sub1
str(cld.1st.Vcmax.FinS.df.sub1)
##
cld.1st.Vcmax.FinS.df.sub1$treats <- ifelse(cld.1st.Vcmax.FinS.df.sub1$frost== "CK" , "Control", 
                                            ifelse(cld.1st.Vcmax.FinS.df.sub1$frost == "fro" , "Frost","NA"))
cld.1st.Vcmax.FinS.df.sub1

##pred####
require(ggeffects)
pred.1st.Vcmax<-ggpredict(fit.1st.Vcmax,c("species_abbr","frost"),type="fe",ci.lvl = 0.95)
names(pred.1st.Vcmax)<-c("species_abbr","mean","se","conf.low","conf.high","frost")
pred.1st.Vcmax.df<-as.data.frame(pred.1st.Vcmax)
pred.1st.Vcmax.df
str(pred.1st.Vcmax.df)
##add water variable
pred.1st.Vcmax.df <-pred.1st.Vcmax.df %>% 
  mutate(water = paste("con"))
##merge varibles and be added into a dataset###
##add treats
pred.1st.Vcmax.df$treats <- ifelse(pred.1st.Vcmax.df$frost== "fro", "Frost", 
                                   ifelse(pred.1st.Vcmax.df$frost == "CK", "Control","NA"))

pred.1st.Vcmax.df
##newdataset####
pred.1st.Vcmax.df
contr.1st.Vcmax.FinS.df.sub3
cld.1st.Vcmax.FinS.df.sub1
##
cld.contr.pred.1st.Vcmax<-merge(merge(cld.1st.Vcmax.FinS.df.sub1,
                                      pred.1st.Vcmax.df,
                                      by=c("species_abbr","frost","water","treats")),
                                contr.1st.Vcmax.FinS.df.sub3,by=c("species_abbr","frost","water","treats"))

##
head(cld.contr.pred.1st.Vcmax)
cld.contr.pred.1st.Vcmax.c<-cld.contr.pred.1st.Vcmax
names(cld.contr.pred.1st.Vcmax)<-c("species_abbr","frost","water","treats","cld.group","mean","se","conf.low","conf.high","contrast","contr.Pvalue","contr.label","frost_ref")
cld.contr.pred.1st.Vcmax

##Mulcomp_2nd______####
##contr####
require(emmeans)
pairs(emmeans(fit.2nd.Vcmax, ~species_abbr),adjust = "tukey")
pairs(emmeans(fit.2nd.Vcmax, ~frost+water|species_abbr),adjust = "tukey")
pairs(emmeans(fit.2nd.Vcmax, ~frost|species_abbr),adjust = "tukey")
pairs(emmeans(fit.2nd.Vcmax, ~water|species_abbr),adjust = "tukey")
##
contr.2nd.Vcmax.FWinS<-pairs(emmeans(fit.2nd.Vcmax, ~frost+water|species_abbr),adjust = "tukey")
contr.2nd.Vcmax.FWinS.df<-as.data.frame(contr.2nd.Vcmax.FWinS)
contr.2nd.Vcmax.FWinS.df.sub<-contr.2nd.Vcmax.FWinS.df[c(1:3,7:9,13:15,19:21),c(1,7)]
contr.2nd.Vcmax.FWinS.df.sub

##add contr.label###
contr.2nd.Vcmax.FWinS.df.sub$contr.label <- ifelse(contr.2nd.Vcmax.FWinS.df.sub$"p.value"> 0.05 , "ns", 
                                                   ifelse(contr.2nd.Vcmax.FWinS.df.sub$"p.value">0.01, "*",
                                                          ifelse(contr.2nd.Vcmax.FWinS.df.sub$"p.value">0.001, "**",
                                                                 ifelse(contr.2nd.Vcmax.FWinS.df.sub$"p.value"<=0.001, "***", "NA"))))


##add appendix rows 
contr.2nd.Vcmax.FWinS.df.sub
##
appendix.2nd<-data.frame(species_abbr.2nd=c("AC","FS","QP","QR"),
                         contrast=c("CK con - CK con","CK con - CK con","CK con - CK con","CK con - CK con"),
                         p.value=c(1,1,1,1),contr.label=c("","","",""))

head(appendix.2nd)
species_abbr.2nd=c("AC","AC","AC","FS","FS","FS","QP","QP","QP","QR","QR","QR")
contr.2nd.Vcmax.FWinS.df.sub1<-rbind(cbind(species_abbr.2nd,
                                           contr.2nd.Vcmax.FWinS.df.sub),
                                     appendix.2nd)
contr.2nd.Vcmax.FWinS.df.sub1
names(contr.2nd.Vcmax.FWinS.df.sub1)<-c("species_abbr","contrast","contr.Pvalue","contr.label")
##seperate column###
require(ggpubr)
library(stringr)#required for str_split()
##separate column
contr.2nd.Vcmax.FWinS.df.sub2<-contr.2nd.Vcmax.FWinS.df.sub1%>% 
  mutate(frost_water = str_split(contrast, " - ", simplify = T)[,2])%>% 
  mutate(frost = str_split(frost_water, " ", simplify = T)[,1])%>% 
  mutate(water = str_split(frost_water, " ", simplify = T)[,2])

contr.2nd.Vcmax.FWinS.df.sub2


##
contr.2nd.Vcmax.FWinS.df.sub2$treats <- ifelse(contr.2nd.Vcmax.FWinS.df.sub2$frost== "CK" & contr.2nd.Vcmax.FWinS.df.sub2$water == "con", "Control", 
                                               ifelse(contr.2nd.Vcmax.FWinS.df.sub2$frost == "fro" & contr.2nd.Vcmax.FWinS.df.sub2$water == "con", "Frost",
                                                      ifelse(contr.2nd.Vcmax.FWinS.df.sub2$frost == "CK" & contr.2nd.Vcmax.FWinS.df.sub2$water == "dro", "Drought",
                                                             ifelse(contr.2nd.Vcmax.FWinS.df.sub2$frost == "fro" & contr.2nd.Vcmax.FWinS.df.sub2$water == "dro", "Frost+Drought","NA"))))
contr.2nd.Vcmax.FWinS.df.sub2
##cld####
require(multcomp)
cld(emmeans(fit.2nd.Vcmax, ~species_abbr),Letters=letters)
cld(emmeans(fit.2nd.Vcmax, ~frost+water|species_abbr),Letters=letters)
cld(emmeans(fit.2nd.Vcmax, ~frost|species_abbr),Letters=letters)
cld(emmeans(fit.2nd.Vcmax, ~water|species_abbr),Letters=letters)
##
##
cld.2nd.Vcmax.FWinS<-cld(emmeans(fit.2nd.Vcmax, ~frost+water|species_abbr),Letters = letters)
cld.2nd.Vcmax.FWinS.df<-as.data.frame(cld.2nd.Vcmax.FWinS)
cld.2nd.Vcmax.FWinS.df.sub<-cld.2nd.Vcmax.FWinS.df[c(1:16),c(1,2,9)]
cld.2nd.Vcmax.FWinS.df.sub
species_abbr.2nd<-c("AC","AC","AC","AC","FS","FS","FS","FS","QP","QP","QP","QP","QR","QR","QR","QR")
cld.2nd.Vcmax.FWinS.df.sub1<-cbind(species_abbr.2nd,
                                   cld.2nd.Vcmax.FWinS.df.sub)
names(cld.2nd.Vcmax.FWinS.df.sub1)<-c("species_abbr","frost","water","cld.group")
cld.2nd.Vcmax.FWinS.df.sub1
##add treats
cld.2nd.Vcmax.FWinS.df.sub1$treats <- ifelse(cld.2nd.Vcmax.FWinS.df.sub1$frost== "CK" & cld.2nd.Vcmax.FWinS.df.sub1$water == "con", "Control", 
                                             ifelse(cld.2nd.Vcmax.FWinS.df.sub1$frost == "fro" & cld.2nd.Vcmax.FWinS.df.sub1$water == "con", "Frost",
                                                    ifelse(cld.2nd.Vcmax.FWinS.df.sub1$frost == "CK" & cld.2nd.Vcmax.FWinS.df.sub1$water == "dro", "Drought",
                                                           ifelse(cld.2nd.Vcmax.FWinS.df.sub1$frost == "fro" & cld.2nd.Vcmax.FWinS.df.sub1$water == "dro", "Frost+Drought","NA"))))
cld.2nd.Vcmax.FWinS.df.sub1

##pred####
require(ggeffects)
pred.2nd.Vcmax<-ggpredict(fit.2nd.Vcmax,c("species_abbr","frost","water"),type="fe",ci.lvl = 0.95)
names(pred.2nd.Vcmax)<-c("species_abbr","mean","se","conf.low","conf.high","frost","water")
pred.2nd.Vcmax.df<-as.data.frame(pred.2nd.Vcmax)
pred.2nd.Vcmax.df
##merge varibles and be added into a dataset###
##add treats
pred.2nd.Vcmax.df$treats <- ifelse(pred.2nd.Vcmax.df$frost== "CK" & pred.2nd.Vcmax.df$water == "con", "Control", 
                                   ifelse(pred.2nd.Vcmax.df$frost == "fro" & pred.2nd.Vcmax.df$water == "con", "Frost",
                                          ifelse(pred.2nd.Vcmax.df$frost == "CK" & pred.2nd.Vcmax.df$water == "dro", "Drought",
                                                 ifelse(pred.2nd.Vcmax.df$frost == "fro" & pred.2nd.Vcmax.df$water == "dro", "Frost+Drought","NA"))))


pred.2nd.Vcmax.df
##newdataset####
pred.2nd.Vcmax.df
contr.2nd.Vcmax.FWinS.df.sub2
cld.2nd.Vcmax.FWinS.df.sub1
##
cld.contr.pred.2nd.Vcmax<-merge(merge(cld.2nd.Vcmax.FWinS.df.sub1,
                                      pred.2nd.Vcmax.df,
                                      by=c("species_abbr","frost","water","treats")),
                                contr.2nd.Vcmax.FWinS.df.sub2,by=c("species_abbr","frost","water","treats"))

##
head(cld.contr.pred.2nd.Vcmax)
cld.contr.pred.2nd.Vcmax.c<-cld.contr.pred.2nd.Vcmax

##Mulcomp_3rd______####
##contr####
require(emmeans)
pairs(emmeans(fit.3rd.Vcmax, ~species_abbr),adjust = "tukey")
pairs(emmeans(fit.3rd.Vcmax, ~frost+water|species_abbr),adjust = "tukey")
pairs(emmeans(fit.3rd.Vcmax, ~frost|species_abbr),adjust = "tukey")
pairs(emmeans(fit.3rd.Vcmax, ~water|species_abbr),adjust = "tukey")
##
contr.3rd.Vcmax.FWinS<-pairs(emmeans(fit.3rd.Vcmax, ~frost+water|species_abbr),adjust = "tukey")
contr.3rd.Vcmax.FWinS.df<-as.data.frame(contr.3rd.Vcmax.FWinS)
contr.3rd.Vcmax.FWinS.df.sub<-contr.3rd.Vcmax.FWinS.df[c(1:3,7:9,13:15,19:21),c(1,7)]
contr.3rd.Vcmax.FWinS.df.sub

##add contr.label###
contr.3rd.Vcmax.FWinS.df.sub$contr.label <- ifelse(contr.3rd.Vcmax.FWinS.df.sub$"p.value"> 0.05 , "ns", 
                                                   ifelse(contr.3rd.Vcmax.FWinS.df.sub$"p.value">0.01, "*",
                                                          ifelse(contr.3rd.Vcmax.FWinS.df.sub$"p.value">0.001, "**",
                                                                 ifelse(contr.3rd.Vcmax.FWinS.df.sub$"p.value"<=0.001, "***", "NA"))))


##add appendix rows 
contr.3rd.Vcmax.FWinS.df.sub
##
appendix.3rd<-data.frame(species_abbr.3rd=c("AC","FS","QP","QR"),
                         contrast=c("CK con - CK con","CK con - CK con","CK con - CK con","CK con - CK con"),
                         p.value=c(1,1,1,1),contr.label=c("","","",""))

head(appendix.3rd)
species_abbr.3rd=c("AC","AC","AC","FS","FS","FS","QP","QP","QP","QR","QR","QR")
contr.3rd.Vcmax.FWinS.df.sub1<-rbind(cbind(species_abbr.3rd,
                                           contr.3rd.Vcmax.FWinS.df.sub),
                                     appendix.3rd)
contr.3rd.Vcmax.FWinS.df.sub1
names(contr.3rd.Vcmax.FWinS.df.sub1)<-c("species_abbr","contrast","contr.Pvalue","contr.label")
##seperate column###
require(ggpubr)
library(stringr)#required for str_split()
##separate column
contr.3rd.Vcmax.FWinS.df.sub2<-contr.3rd.Vcmax.FWinS.df.sub1%>% 
  mutate(frost_water = str_split(contrast, " - ", simplify = T)[,2])%>% 
  mutate(frost = str_split(frost_water, " ", simplify = T)[,1])%>% 
  mutate(water = str_split(frost_water, " ", simplify = T)[,2])

contr.3rd.Vcmax.FWinS.df.sub2


##
contr.3rd.Vcmax.FWinS.df.sub2$treats <- ifelse(contr.3rd.Vcmax.FWinS.df.sub2$frost== "CK" & contr.3rd.Vcmax.FWinS.df.sub2$water == "con", "Control", 
                                               ifelse(contr.3rd.Vcmax.FWinS.df.sub2$frost == "fro" & contr.3rd.Vcmax.FWinS.df.sub2$water == "con", "Frost",
                                                      ifelse(contr.3rd.Vcmax.FWinS.df.sub2$frost == "CK" & contr.3rd.Vcmax.FWinS.df.sub2$water == "dro", "Drought",
                                                             ifelse(contr.3rd.Vcmax.FWinS.df.sub2$frost == "fro" & contr.3rd.Vcmax.FWinS.df.sub2$water == "dro", "Frost+Drought","NA"))))
contr.3rd.Vcmax.FWinS.df.sub2
##cld####
require(multcomp)
cld(emmeans(fit.3rd.Vcmax, ~species_abbr),Letters=letters)
cld(emmeans(fit.3rd.Vcmax, ~frost+water|species_abbr),Letters=letters)
cld(emmeans(fit.3rd.Vcmax, ~frost|species_abbr),Letters=letters)
cld(emmeans(fit.3rd.Vcmax, ~water|species_abbr),Letters=letters)
##
##
cld.3rd.Vcmax.FWinS<-cld(emmeans(fit.3rd.Vcmax, ~frost+water|species_abbr),Letters = letters)
cld.3rd.Vcmax.FWinS.df<-as.data.frame(cld.3rd.Vcmax.FWinS)
cld.3rd.Vcmax.FWinS.df.sub<-cld.3rd.Vcmax.FWinS.df[c(1:16),c(1,2,9)]
cld.3rd.Vcmax.FWinS.df.sub
species_abbr.3rd<-c("AC","AC","AC","AC","FS","FS","FS","FS","QP","QP","QP","QP","QR","QR","QR","QR")
cld.3rd.Vcmax.FWinS.df.sub1<-cbind(species_abbr.3rd,
                                   cld.3rd.Vcmax.FWinS.df.sub)
names(cld.3rd.Vcmax.FWinS.df.sub1)<-c("species_abbr","frost","water","cld.group")
cld.3rd.Vcmax.FWinS.df.sub1
##add treats
cld.3rd.Vcmax.FWinS.df.sub1$treats <- ifelse(cld.3rd.Vcmax.FWinS.df.sub1$frost== "CK" & cld.3rd.Vcmax.FWinS.df.sub1$water == "con", "Control", 
                                             ifelse(cld.3rd.Vcmax.FWinS.df.sub1$frost == "fro" & cld.3rd.Vcmax.FWinS.df.sub1$water == "con", "Frost",
                                                    ifelse(cld.3rd.Vcmax.FWinS.df.sub1$frost == "CK" & cld.3rd.Vcmax.FWinS.df.sub1$water == "dro", "Drought",
                                                           ifelse(cld.3rd.Vcmax.FWinS.df.sub1$frost == "fro" & cld.3rd.Vcmax.FWinS.df.sub1$water == "dro", "Frost+Drought","NA"))))
cld.3rd.Vcmax.FWinS.df.sub1

##pred####
require(ggeffects)
pred.3rd.Vcmax<-ggpredict(fit.3rd.Vcmax,c("species_abbr","frost","water"),type="fe",ci.lvl = 0.95)
names(pred.3rd.Vcmax)<-c("species_abbr","mean","se","conf.low","conf.high","frost","water")
pred.3rd.Vcmax.df<-as.data.frame(pred.3rd.Vcmax)
pred.3rd.Vcmax.df
##merge varibles and be added into a dataset###
##add treats
pred.3rd.Vcmax.df$treats <- ifelse(pred.3rd.Vcmax.df$frost== "CK" & pred.3rd.Vcmax.df$water == "con", "Control", 
                                   ifelse(pred.3rd.Vcmax.df$frost == "fro" & pred.3rd.Vcmax.df$water == "con", "Frost",
                                          ifelse(pred.3rd.Vcmax.df$frost == "CK" & pred.3rd.Vcmax.df$water == "dro", "Drought",
                                                 ifelse(pred.3rd.Vcmax.df$frost == "fro" & pred.3rd.Vcmax.df$water == "dro", "Frost+Drought","NA"))))


pred.3rd.Vcmax.df
##newdataset####
pred.3rd.Vcmax.df
contr.3rd.Vcmax.FWinS.df.sub2
cld.3rd.Vcmax.FWinS.df.sub1
##
cld.contr.pred.3rd.Vcmax<-merge(merge(cld.3rd.Vcmax.FWinS.df.sub1,
                                      pred.3rd.Vcmax.df,
                                      by=c("species_abbr","frost","water","treats")),
                                contr.3rd.Vcmax.FWinS.df.sub2,by=c("species_abbr","frost","water","treats"))

##
head(cld.contr.pred.3rd.Vcmax)
cld.contr.pred.3rd.Vcmax.c<-cld.contr.pred.3rd.Vcmax



##merge for Vcmax####
cld.contr.pred.1st.Vcmax<-cld.contr.pred.1st.Vcmax%>%mutate(subtitle=paste("1st"),variable=paste("Vcmax"),measuredate=as.Date("2022-06-11"))
cld.contr.pred.2nd.Vcmax<-cld.contr.pred.3rd.Vcmax%>%mutate(subtitle=paste("2nd"),variable=paste("Vcmax"),measuredate=as.Date("2022-07-15"))
cld.contr.pred.3rd.Vcmax<-cld.contr.pred.3rd.Vcmax%>%mutate(subtitle=paste("3rd"),variable=paste("Vcmax"),measuredate=as.Date("2022-08-20"))

cld.contr.pred.Vcmax<-rbind(cld.contr.pred.1st.Vcmax[,c(1:12,14,15,16)],
                            cld.contr.pred.3rd.Vcmax[,c(1:12,14,15,16)],
                            cld.contr.pred.2nd.Vcmax[,c(1:12,14,15,16)])
cld.contr.pred.Vcmax

##_____________________________________________####
##lm.Jmax.1st####
fit.1st.Jmax<-lm(Jmax~frost*species_abbr,data=data.vjmax.1st)
summary(fit.1st.Jmax)##sig in fro
anova(fit.1st.Jmax)
library(performance)
require(see)
check_model(fit.1st.Jmax)##to view normality
shapiro.test(residuals(fit.1st.Jmax))##yes
qqnorm(resid(fit.1st.Jmax))##yes
qqline(resid(fit.1st.Jmax))

#Skim the performance of the model
library(sjPlot)
require(ggplot2)

plot_model(fit.1st.Jmax, terms=c("species_abbr", "frost"), 
           type="emm",show.data = T, dot.size = 1)+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

##lm.Vcmax_2nd####
fit.2nd.Jmax<-lm(Jmax~frost*species_abbr+
                   water*species_abbr+
                   frost*water,data=data.vjmax.2nd)
summary(fit.2nd.Jmax)##sig in species & fro*species
anova(fit.2nd.Jmax)
library(performance)
require(see)
check_model(fit.2nd.Jmax)##to view normality
shapiro.test(residuals(fit.2nd.Jmax))##no##take
qqnorm(resid(fit.2nd.Jmax))##yes
qqline(resid(fit.2nd.Jmax))
#Skim the performance of the model
library(sjPlot)
require(ggplot2)
plot_model(fit.2nd.Jmax, terms=c("species_abbr", "frost","water"), type="emm",
           show.data = T, dot.size = 1)+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")


##lm.Vcmax_3rd####
fit.3rd.Jmax<-lm(Jmax~frost*species_abbr+
                   water*species_abbr+
                   frost*water,data=data.vjmax.3rd)
summary(fit.3rd.Jmax)##sig in species & fro*species
anova(fit.3rd.Jmax)
library(performance)
require(see)
check_model(fit.3rd.Jmax)##to view normality
shapiro.test(residuals(fit.3rd.Jmax))##no##take
qqnorm(resid(fit.3rd.Jmax))##yes
qqline(resid(fit.3rd.Jmax))
#Skim the performance of the model
library(sjPlot)
require(ggplot2)
plot_model(fit.3rd.Jmax, terms=c("species_abbr", "frost","water"), type="emm",
           show.data = T, dot.size = 1)+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

##Mulcomp_1st_____####
##contr####
require(emmeans)
pairs(emmeans(fit.1st.Jmax, ~species_abbr),adjust = "tukey")
pairs(emmeans(fit.1st.Jmax, ~frost|species_abbr),adjust = "tukey")
pairs(emmeans(fit.1st.Jmax, ~species_abbr|frost),adjust = "tukey")
pairs(emmeans(fit.1st.Jmax, ~species_abbr+frost),adjust = "tukey")

##
contr.1st.Jmax.FinS<-pairs(emmeans(fit.1st.Jmax, ~frost|species_abbr),adjust = "tukey")
contr.1st.Jmax.FinS.df<-as.data.frame(contr.1st.Jmax.FinS)
contr.1st.Jmax.FinS.df.sub<-contr.1st.Jmax.FinS.df[c(1:4),c(1,7)]
##add contr.label###
contr.1st.Jmax.FinS.df.sub$contr.label <- ifelse(contr.1st.Jmax.FinS.df.sub$"p.value"> 0.05 , "ns", 
                                                 ifelse(contr.1st.Jmax.FinS.df.sub$"p.value">0.01, "*",
                                                        ifelse(contr.1st.Jmax.FinS.df.sub$"p.value">0.001, "**",
                                                               ifelse(contr.1st.Jmax.FinS.df.sub$"p.value"<=0.001, "***", "NA"))))


##add appendix rows 
contr.1st.Jmax.FinS.df.sub
appendix.1st<-data.frame(species_abbr=c("AC","FS","QP","QR"),
                         contrast=c("CK - CK","CK - CK","CK - CK","CK - CK"),
                         p.value=c(1,1,1,1),contr.label=c("","","",""))

head(appendix.1st)
names(appendix.1st)<-c("species_abbr","contrast","p.value","contr.label")
appendix.1st
species_abbr=c("AC","FS","QP","QR")
contr.1st.Jmax.FinS.df.sub1<-rbind(cbind(species_abbr,
                                         contr.1st.Jmax.FinS.df.sub),
                                   appendix.1st)
contr.1st.Jmax.FinS.df.sub1
##seperate column###
require(ggpubr)
library(stringr)#required for str_split()
##separate column
contr.1st.Jmax.FinS.df.sub2<-contr.1st.Jmax.FinS.df.sub1%>% 
  mutate(frost_ref = str_split(contrast, " - ", simplify = T)[,1])%>% 
  mutate(frost = str_split(contrast, " - ", simplify = T)[,2])
contr.1st.Jmax.FinS.df.sub2
str(contr.1st.Jmax.FinS.df.sub2)

##add water variable
contr.1st.Jmax.FinS.df.sub3<-contr.1st.Jmax.FinS.df.sub2%>%mutate(water=paste("con"))
contr.1st.Jmax.FinS.df.sub3
str(contr.1st.Jmax.FinS.df.sub3)

##
contr.1st.Jmax.FinS.df.sub3$treats <- ifelse(contr.1st.Jmax.FinS.df.sub3$frost== "CK" & contr.1st.Jmax.FinS.df.sub3$water == "con", "Control", 
                                             ifelse(contr.1st.Jmax.FinS.df.sub3$frost == "fro" & contr.1st.Jmax.FinS.df.sub3$water == "con", "Frost","NA"))
contr.1st.Jmax.FinS.df.sub3
##

##cld####
require(multcomp)
cld(emmeans(fit.1st.Jmax, ~species_abbr),Letters = letters)
cld(emmeans(fit.1st.Jmax, ~frost|species_abbr),Letters = letters)
cld(emmeans(fit.1st.Jmax, ~species_abbr|frost),Letters = letters)
cld(emmeans(fit.1st.Jmax, ~species_abbr:frost),Letters = letters)
##
cld.1st.Jmax.FinS<-cld(emmeans(fit.1st.Jmax, ~frost|species_abbr),Letters = letters)
cld.1st.Jmax.FinS.df<-as.data.frame(cld.1st.Jmax.FinS)
cld.1st.Jmax.FinS.df.sub<-cld.1st.Jmax.FinS.df[c(1:8),c(1,8)]
cld.1st.Jmax.FinS.df.sub
species_abbr<-c("AC","AC","FS","FS","QP","QP","QR","QR")
cld.1st.Jmax.FinS.df.sub1<-cbind(species_abbr,cld.1st.Jmax.FinS.df.sub)
cld.1st.Jmax.FinS.df.sub1 <-cld.1st.Jmax.FinS.df.sub1 %>% 
  mutate(water = paste("con"))
cld.1st.Jmax.FinS.df.sub1
names(cld.1st.Jmax.FinS.df.sub1)<-c("species_abbr","frost","cld.group","water")
cld.1st.Jmax.FinS.df.sub1
str(cld.1st.Jmax.FinS.df.sub1)
##
cld.1st.Jmax.FinS.df.sub1$treats <- ifelse(cld.1st.Jmax.FinS.df.sub1$frost== "CK" , "Control", 
                                           ifelse(cld.1st.Jmax.FinS.df.sub1$frost == "fro" , "Frost","NA"))
cld.1st.Jmax.FinS.df.sub1

##pred####
require(ggeffects)
pred.1st.Jmax<-ggpredict(fit.1st.Jmax,c("species_abbr","frost"),type="fe",ci.lvl = 0.95)
names(pred.1st.Jmax)<-c("species_abbr","mean","se","conf.low","conf.high","frost")
pred.1st.Jmax.df<-as.data.frame(pred.1st.Jmax)
pred.1st.Jmax.df
str(pred.1st.Jmax.df)
##add water variable
pred.1st.Jmax.df <-pred.1st.Jmax.df %>% 
  mutate(water = paste("con"))
##merge varibles and be added into a dataset###
##add treats
pred.1st.Jmax.df$treats <- ifelse(pred.1st.Jmax.df$frost== "fro", "Frost", 
                                  ifelse(pred.1st.Jmax.df$frost == "CK", "Control","NA"))

pred.1st.Jmax.df
##newdataset####
pred.1st.Jmax.df
contr.1st.Jmax.FinS.df.sub3
cld.1st.Jmax.FinS.df.sub1
##
cld.contr.pred.1st.Jmax<-merge(merge(cld.1st.Jmax.FinS.df.sub1,
                                     pred.1st.Jmax.df,
                                     by=c("species_abbr","frost","water","treats")),
                               contr.1st.Jmax.FinS.df.sub3,by=c("species_abbr","frost","water","treats"))

##
head(cld.contr.pred.1st.Jmax)
cld.contr.pred.1st.Jmax.c<-cld.contr.pred.1st.Jmax
names(cld.contr.pred.1st.Jmax)<-c("species_abbr","frost","water","treats","cld.group","mean","se","conf.low","conf.high","contrast","contr.Pvalue","contr.label","frost_ref")
cld.contr.pred.1st.Jmax

##Mulcomp_2nd______####
##contr####
require(emmeans)
pairs(emmeans(fit.2nd.Jmax, ~species_abbr),adjust = "tukey")
pairs(emmeans(fit.2nd.Jmax, ~frost+water|species_abbr),adjust = "tukey")
pairs(emmeans(fit.2nd.Jmax, ~frost|species_abbr),adjust = "tukey")
pairs(emmeans(fit.2nd.Jmax, ~water|species_abbr),adjust = "tukey")
##
contr.2nd.Jmax.FWinS<-pairs(emmeans(fit.2nd.Jmax, ~frost+water|species_abbr),adjust = "tukey")
contr.2nd.Jmax.FWinS.df<-as.data.frame(contr.2nd.Jmax.FWinS)
contr.2nd.Jmax.FWinS.df.sub<-contr.2nd.Jmax.FWinS.df[c(1:3,7:9,13:15,19:21),c(1,7)]
contr.2nd.Jmax.FWinS.df.sub

##add contr.label###
contr.2nd.Jmax.FWinS.df.sub$contr.label <- ifelse(contr.2nd.Jmax.FWinS.df.sub$"p.value"> 0.05 , "ns", 
                                                  ifelse(contr.2nd.Jmax.FWinS.df.sub$"p.value">0.01, "*",
                                                         ifelse(contr.2nd.Jmax.FWinS.df.sub$"p.value">0.001, "**",
                                                                ifelse(contr.2nd.Jmax.FWinS.df.sub$"p.value"<=0.001, "***", "NA"))))


##add appendix rows 
contr.2nd.Jmax.FWinS.df.sub
##
appendix.2nd<-data.frame(species_abbr.2nd=c("AC","FS","QP","QR"),
                         contrast=c("CK con - CK con","CK con - CK con","CK con - CK con","CK con - CK con"),
                         p.value=c(1,1,1,1),contr.label=c("","","",""))

head(appendix.2nd)
species_abbr.2nd=c("AC","AC","AC","FS","FS","FS","QP","QP","QP","QR","QR","QR")
contr.2nd.Jmax.FWinS.df.sub1<-rbind(cbind(species_abbr.2nd,
                                          contr.2nd.Jmax.FWinS.df.sub),
                                    appendix.2nd)
contr.2nd.Jmax.FWinS.df.sub1
names(contr.2nd.Jmax.FWinS.df.sub1)<-c("species_abbr","contrast","contr.Pvalue","contr.label")
##seperate column###
require(ggpubr)
library(stringr)#required for str_split()
##separate column
contr.2nd.Jmax.FWinS.df.sub2<-contr.2nd.Jmax.FWinS.df.sub1%>% 
  mutate(frost_water = str_split(contrast, " - ", simplify = T)[,2])%>% 
  mutate(frost = str_split(frost_water, " ", simplify = T)[,1])%>% 
  mutate(water = str_split(frost_water, " ", simplify = T)[,2])

contr.2nd.Jmax.FWinS.df.sub2


##
contr.2nd.Jmax.FWinS.df.sub2$treats <- ifelse(contr.2nd.Jmax.FWinS.df.sub2$frost== "CK" & contr.2nd.Jmax.FWinS.df.sub2$water == "con", "Control", 
                                              ifelse(contr.2nd.Jmax.FWinS.df.sub2$frost == "fro" & contr.2nd.Jmax.FWinS.df.sub2$water == "con", "Frost",
                                                     ifelse(contr.2nd.Jmax.FWinS.df.sub2$frost == "CK" & contr.2nd.Jmax.FWinS.df.sub2$water == "dro", "Drought",
                                                            ifelse(contr.2nd.Jmax.FWinS.df.sub2$frost == "fro" & contr.2nd.Jmax.FWinS.df.sub2$water == "dro", "Frost+Drought","NA"))))
contr.2nd.Jmax.FWinS.df.sub2
##cld####
require(multcomp)
cld(emmeans(fit.2nd.Jmax, ~species_abbr),Letters=letters)
cld(emmeans(fit.2nd.Jmax, ~frost+water|species_abbr),Letters=letters)
cld(emmeans(fit.2nd.Jmax, ~frost|species_abbr),Letters=letters)
cld(emmeans(fit.2nd.Jmax, ~water|species_abbr),Letters=letters)
##
##
cld.2nd.Jmax.FWinS<-cld(emmeans(fit.2nd.Jmax, ~frost+water|species_abbr),Letters = letters)
cld.2nd.Jmax.FWinS.df<-as.data.frame(cld.2nd.Jmax.FWinS)
cld.2nd.Jmax.FWinS.df.sub<-cld.2nd.Jmax.FWinS.df[c(1:16),c(1,2,9)]
cld.2nd.Jmax.FWinS.df.sub
species_abbr.2nd<-c("AC","AC","AC","AC","FS","FS","FS","FS","QP","QP","QP","QP","QR","QR","QR","QR")
cld.2nd.Jmax.FWinS.df.sub1<-cbind(species_abbr.2nd,
                                  cld.2nd.Jmax.FWinS.df.sub)
names(cld.2nd.Jmax.FWinS.df.sub1)<-c("species_abbr","frost","water","cld.group")
cld.2nd.Jmax.FWinS.df.sub1
##add treats
cld.2nd.Jmax.FWinS.df.sub1$treats <- ifelse(cld.2nd.Jmax.FWinS.df.sub1$frost== "CK" & cld.2nd.Jmax.FWinS.df.sub1$water == "con", "Control", 
                                            ifelse(cld.2nd.Jmax.FWinS.df.sub1$frost == "fro" & cld.2nd.Jmax.FWinS.df.sub1$water == "con", "Frost",
                                                   ifelse(cld.2nd.Jmax.FWinS.df.sub1$frost == "CK" & cld.2nd.Jmax.FWinS.df.sub1$water == "dro", "Drought",
                                                          ifelse(cld.2nd.Jmax.FWinS.df.sub1$frost == "fro" & cld.2nd.Jmax.FWinS.df.sub1$water == "dro", "Frost+Drought","NA"))))
cld.2nd.Jmax.FWinS.df.sub1

##pred####
require(ggeffects)
pred.2nd.Jmax<-ggpredict(fit.2nd.Jmax,c("species_abbr","frost","water"),type="fe",ci.lvl = 0.95)
names(pred.2nd.Jmax)<-c("species_abbr","mean","se","conf.low","conf.high","frost","water")
pred.2nd.Jmax.df<-as.data.frame(pred.2nd.Jmax)
pred.2nd.Jmax.df
##merge varibles and be added into a dataset###
##add treats
pred.2nd.Jmax.df$treats <- ifelse(pred.2nd.Jmax.df$frost== "CK" & pred.2nd.Jmax.df$water == "con", "Control", 
                                  ifelse(pred.2nd.Jmax.df$frost == "fro" & pred.2nd.Jmax.df$water == "con", "Frost",
                                         ifelse(pred.2nd.Jmax.df$frost == "CK" & pred.2nd.Jmax.df$water == "dro", "Drought",
                                                ifelse(pred.2nd.Jmax.df$frost == "fro" & pred.2nd.Jmax.df$water == "dro", "Frost+Drought","NA"))))


pred.2nd.Jmax.df
##newdataset####
pred.2nd.Jmax.df
contr.2nd.Jmax.FWinS.df.sub2
cld.2nd.Jmax.FWinS.df.sub1
##
cld.contr.pred.2nd.Jmax<-merge(merge(cld.2nd.Jmax.FWinS.df.sub1,
                                     pred.2nd.Jmax.df,
                                     by=c("species_abbr","frost","water","treats")),
                               contr.2nd.Jmax.FWinS.df.sub2,by=c("species_abbr","frost","water","treats"))

##
head(cld.contr.pred.2nd.Jmax)
cld.contr.pred.2nd.Jmax.c<-cld.contr.pred.2nd.Jmax

##Mulcomp_3rd______####
##contr####
require(emmeans)
pairs(emmeans(fit.3rd.Jmax, ~species_abbr),adjust = "tukey")
pairs(emmeans(fit.3rd.Jmax, ~frost+water|species_abbr),adjust = "tukey")
pairs(emmeans(fit.3rd.Jmax, ~frost|species_abbr),adjust = "tukey")
pairs(emmeans(fit.3rd.Jmax, ~water|species_abbr),adjust = "tukey")
##
contr.3rd.Jmax.FWinS<-pairs(emmeans(fit.3rd.Jmax, ~frost+water|species_abbr),adjust = "tukey")
contr.3rd.Jmax.FWinS.df<-as.data.frame(contr.3rd.Jmax.FWinS)
contr.3rd.Jmax.FWinS.df.sub<-contr.3rd.Jmax.FWinS.df[c(1:3,7:9,13:15,19:21),c(1,7)]
contr.3rd.Jmax.FWinS.df.sub

##add contr.label###
contr.3rd.Jmax.FWinS.df.sub$contr.label <- ifelse(contr.3rd.Jmax.FWinS.df.sub$"p.value"> 0.05 , "ns", 
                                                  ifelse(contr.3rd.Jmax.FWinS.df.sub$"p.value">0.01, "*",
                                                         ifelse(contr.3rd.Jmax.FWinS.df.sub$"p.value">0.001, "**",
                                                                ifelse(contr.3rd.Jmax.FWinS.df.sub$"p.value"<=0.001, "***", "NA"))))


##add appendix rows 
contr.3rd.Jmax.FWinS.df.sub
##
appendix.3rd<-data.frame(species_abbr.3rd=c("AC","FS","QP","QR"),
                         contrast=c("CK con - CK con","CK con - CK con","CK con - CK con","CK con - CK con"),
                         p.value=c(1,1,1,1),contr.label=c("","","",""))

head(appendix.3rd)
species_abbr.3rd=c("AC","AC","AC","FS","FS","FS","QP","QP","QP","QR","QR","QR")
contr.3rd.Jmax.FWinS.df.sub1<-rbind(cbind(species_abbr.3rd,
                                          contr.3rd.Jmax.FWinS.df.sub),
                                    appendix.3rd)
contr.3rd.Jmax.FWinS.df.sub1
names(contr.3rd.Jmax.FWinS.df.sub1)<-c("species_abbr","contrast","contr.Pvalue","contr.label")
##seperate column###
require(ggpubr)
library(stringr)#required for str_split()
##separate column
contr.3rd.Jmax.FWinS.df.sub2<-contr.3rd.Jmax.FWinS.df.sub1%>% 
  mutate(frost_water = str_split(contrast, " - ", simplify = T)[,2])%>% 
  mutate(frost = str_split(frost_water, " ", simplify = T)[,1])%>% 
  mutate(water = str_split(frost_water, " ", simplify = T)[,2])

contr.3rd.Jmax.FWinS.df.sub2


##
contr.3rd.Jmax.FWinS.df.sub2$treats <- ifelse(contr.3rd.Jmax.FWinS.df.sub2$frost== "CK" & contr.3rd.Jmax.FWinS.df.sub2$water == "con", "Control", 
                                              ifelse(contr.3rd.Jmax.FWinS.df.sub2$frost == "fro" & contr.3rd.Jmax.FWinS.df.sub2$water == "con", "Frost",
                                                     ifelse(contr.3rd.Jmax.FWinS.df.sub2$frost == "CK" & contr.3rd.Jmax.FWinS.df.sub2$water == "dro", "Drought",
                                                            ifelse(contr.3rd.Jmax.FWinS.df.sub2$frost == "fro" & contr.3rd.Jmax.FWinS.df.sub2$water == "dro", "Frost+Drought","NA"))))
contr.3rd.Jmax.FWinS.df.sub2
##cld####
require(multcomp)
cld(emmeans(fit.3rd.Jmax, ~species_abbr),Letters=letters)
cld(emmeans(fit.3rd.Jmax, ~frost+water|species_abbr),Letters=letters)
cld(emmeans(fit.3rd.Jmax, ~frost|species_abbr),Letters=letters)
cld(emmeans(fit.3rd.Jmax, ~water|species_abbr),Letters=letters)
##
##
cld.3rd.Jmax.FWinS<-cld(emmeans(fit.3rd.Jmax, ~frost+water|species_abbr),Letters = letters)
cld.3rd.Jmax.FWinS.df<-as.data.frame(cld.3rd.Jmax.FWinS)
cld.3rd.Jmax.FWinS.df.sub<-cld.3rd.Jmax.FWinS.df[c(1:16),c(1,2,9)]
cld.3rd.Jmax.FWinS.df.sub
species_abbr.3rd<-c("AC","AC","AC","AC","FS","FS","FS","FS","QP","QP","QP","QP","QR","QR","QR","QR")
cld.3rd.Jmax.FWinS.df.sub1<-cbind(species_abbr.3rd,
                                  cld.3rd.Jmax.FWinS.df.sub)
names(cld.3rd.Jmax.FWinS.df.sub1)<-c("species_abbr","frost","water","cld.group")
cld.3rd.Jmax.FWinS.df.sub1
##add treats
cld.3rd.Jmax.FWinS.df.sub1$treats <- ifelse(cld.3rd.Jmax.FWinS.df.sub1$frost== "CK" & cld.3rd.Jmax.FWinS.df.sub1$water == "con", "Control", 
                                            ifelse(cld.3rd.Jmax.FWinS.df.sub1$frost == "fro" & cld.3rd.Jmax.FWinS.df.sub1$water == "con", "Frost",
                                                   ifelse(cld.3rd.Jmax.FWinS.df.sub1$frost == "CK" & cld.3rd.Jmax.FWinS.df.sub1$water == "dro", "Drought",
                                                          ifelse(cld.3rd.Jmax.FWinS.df.sub1$frost == "fro" & cld.3rd.Jmax.FWinS.df.sub1$water == "dro", "Frost+Drought","NA"))))
cld.3rd.Jmax.FWinS.df.sub1

##pred####
require(ggeffects)
pred.3rd.Jmax<-ggpredict(fit.3rd.Jmax,c("species_abbr","frost","water"),type="fe",ci.lvl = 0.95)
names(pred.3rd.Jmax)<-c("species_abbr","mean","se","conf.low","conf.high","frost","water")
pred.3rd.Jmax.df<-as.data.frame(pred.3rd.Jmax)
pred.3rd.Jmax.df
##merge varibles and be added into a dataset###
##add treats
pred.3rd.Jmax.df$treats <- ifelse(pred.3rd.Jmax.df$frost== "CK" & pred.3rd.Jmax.df$water == "con", "Control", 
                                  ifelse(pred.3rd.Jmax.df$frost == "fro" & pred.3rd.Jmax.df$water == "con", "Frost",
                                         ifelse(pred.3rd.Jmax.df$frost == "CK" & pred.3rd.Jmax.df$water == "dro", "Drought",
                                                ifelse(pred.3rd.Jmax.df$frost == "fro" & pred.3rd.Jmax.df$water == "dro", "Frost+Drought","NA"))))


pred.3rd.Jmax.df
##newdataset####
pred.3rd.Jmax.df
contr.3rd.Jmax.FWinS.df.sub2
cld.3rd.Jmax.FWinS.df.sub1
##
cld.contr.pred.3rd.Jmax<-merge(merge(cld.3rd.Jmax.FWinS.df.sub1,
                                     pred.3rd.Jmax.df,
                                     by=c("species_abbr","frost","water","treats")),
                               contr.3rd.Jmax.FWinS.df.sub2,by=c("species_abbr","frost","water","treats"))

##
head(cld.contr.pred.3rd.Jmax)
cld.contr.pred.3rd.Jmax.c<-cld.contr.pred.3rd.Jmax
##merge for Jmax####
cld.contr.pred.1st.Jmax<-cld.contr.pred.1st.Jmax%>%mutate(subtitle=paste("1st"),variable=paste("Jmax"),measuredate=as.Date("2022-06-11"))
cld.contr.pred.2nd.Jmax<-cld.contr.pred.3rd.Jmax%>%mutate(subtitle=paste("2nd"),variable=paste("Jmax"),measuredate=as.Date("2022-07-15"))
cld.contr.pred.3rd.Jmax<-cld.contr.pred.3rd.Jmax%>%mutate(subtitle=paste("3rd"),variable=paste("Jmax"),measuredate=as.Date("2022-08-20"))

cld.contr.pred.Jmax<-rbind(cld.contr.pred.1st.Jmax[,c(1:12,14,15,16)],
                           cld.contr.pred.3rd.Jmax[,c(1:12,14,15,16)],
                           cld.contr.pred.2nd.Jmax[,c(1:12,14,15,16)])
cld.contr.pred.Jmax


cld.contr.pred.VJmax<-rbind(cld.contr.pred.Vcmax,cld.contr.pred.Jmax)









##______________________________________________####
##merge all variables####
cld.contr.pred.VJmax<-rbind(cld.contr.pred.Vcmax,cld.contr.pred.Jmax)
cld.contr.pred.VJmax
write.csv(cld.contr.pred.VJmax,"cld.contr.pred.VJmax.csv")

##read in data for plot####
cld.contr.pred.VJmax<-read.csv("cld.contr.pred.VJmax.csv")

##plot####
##reorder####
cld.contr.pred.VJmax$treats<-factor(cld.contr.pred.VJmax$treats,levels=c("Control","Frost","Drought","Frost+Drought"))
cld.contr.pred.Jmax$treats<-factor(cld.contr.pred.Jmax$treats,levels=c("Control","Frost","Drought","Frost+Drought"))
cld.contr.pred.Vcmax$treats<-factor(cld.contr.pred.Vcmax$treats,levels=c("Control","Frost","Drought","Frost+Drought"))

##plot.theme####
##set lables for modify the name of facet strip
labels_species<-c(AC="Acer campestre",FS="Fagus sylvatica",QP="Quercus petraea",QR="Quercus robur")
labels_water<-c(con = "Watered", dro = "Drought")
labels_frost<-c(CK = "Control", fro = "Frost")
labels_frost<-c(CK = "Non-frost", fro = "Frost")
labels_treats<-c(Control="Control",Frost="Frost",Drought="Drought",'Frost+Drought'="Frost+Drought")
labels_date<-c('1st'="Jun 15",'2nd'="Jul15",'3rd'="Aug 15",'4th'="Sep 15")

##change font
windowsFonts(Times=windowsFont("Times New Roman"))

##set theme
theme_frodro_gasexchangeforscatterplot<-theme(line = element_blank(),
                                              legend.box = "horizontal",
                                              legend.background = element_blank(),
                                              #legend.position = 'none',
                                              legend.position = "top",
                                              legend.title =element_blank(),
                                              legend.text = element_text(size=6,family = "Times"),
                                              legend.direction = "horizontal",
                                              legend.key.size = unit(0.4,"cm"),
                                              axis.line = element_line(colour = "black"),
                                              axis.title = element_text(size=10,family = "Times"),
                                              axis.ticks=element_line(),
                                              axis.text = element_text(family = "Times"),
                                              axis.text.x = element_text(face="plain",size=7),
                                              strip.background = element_rect(colour = "gray",fill="gray"),
                                              strip.text.x = element_text(size=8,angle=0,face = "italic",family = "Times"),
                                              strip.text.y=element_text(size=8,angle=0,face="bold"),
                                              panel.background = element_rect(fill = "white", color = NA))



##
##scatterplot_Jmax####
Jmax.all<-ggplot(cld.contr.pred.Jmax,aes(x=measuredate, y=mean, colour=treats))+ 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high),size=0.3,position=position_dodge(20))+
  ylim(0,120)+
  xlim(as.Date("2022-06-01"),as.Date("2022-10-01"))+
  labs(x="",y="Jmax (μmol m⁻² s⁻¹)")+
  facet_wrap(~species_abbr,scales='free',ncol=1,labeller = labeller(species_abbr=labels_species))+
  scale_color_manual(values = c("grey", "lightblue","orange","purple4"), labels=labels_treats)+
  #scale_x_discrete(labels=labels_date)+
  theme_frodro_gasexchangeforscatterplot+
  coord_cartesian(clip = "off")+
  geom_text(aes(label=cld.group), position=position_dodge2(width=20), vjust=-2,hjust=0.5,size=3,fontface = "plain",family="Times")+
  geom_vline(xintercept = as.Date("2022-07-03"), linetype = "dashed",color = "black")+
  geom_vline(xintercept = as.Date("2022-09-01"), linetype = "dashed",color = "black")


Jmax.all


ggsave("pointfigs_frodro_Jmax.all.tiff", plot=Jmax.all, 
       path="figs_frodro_GasEx",
       height=19, width=9, units="cm", 
       dpi=600,compression="lzw")



##scatterplot_Vcmax####
##
Vcmax.all<-ggplot(cld.contr.pred.Vcmax,aes(x=measuredate, y=mean, colour=treats))+ 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high),size=0.3,position=position_dodge(20))+
  ylim(0,70)+
  xlim(as.Date("2022-06-01"),as.Date("2022-10-01"))+
  labs(x="",y="Vcmax (µmol m⁻² s⁻¹)")+
  facet_wrap(~species_abbr,scales='free',ncol=1,labeller = labeller(species_abbr=labels_species))+
  scale_color_manual(values = c("grey", "lightblue","orange","purple4"), labels=labels_treats)+
  #scale_x_discrete(labels=labels_date)+
  theme_frodro_gasexchangeforscatterplot+
  coord_cartesian(clip = "off")+
  geom_text(aes(label=cld.group), position=position_dodge2(width=20), vjust=-2.5,hjust=0.5,size=3,fontface = "plain",family="Times")+
  geom_vline(xintercept = as.Date("2022-07-03"), linetype = "dashed",color = "black")+
  geom_vline(xintercept = as.Date("2022-09-01"), linetype = "dashed",color = "black")


Vcmax.all


ggsave("pointfigs_frodro_Vcmax.all.tiff", plot=Vcmax.all, 
       path="figs_frodro_GasEx",
       height=19, width=9, units="cm", 
       dpi=600,compression="lzw")
##___________________________________________________####
##___________________________________________________####
##********************************************####