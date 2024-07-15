########################################################
#Aim: After discussion with Richard, we are aimed to get the better estimation of 
#stand-level of transpiration (sap flow)
#data sent by Ankit at end of Mar, 2024
########################################################
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(ggpubr) #添加回归方程和R2值
library(lubridate)
library(cowplot)
#logic flow:
#1) to develop the realationship between Y(sap wood depth)--X(DBH); calculate the relative sap wood depth
#2) to develop the relationship between Y(Rleative sap flow)--X(Relative sapwood depth)
#3）to check the relation between sap flow with DBH-->A.if sap flow density has the direct relationship
# with DBH, when we estimate stand-level sap flow, we need to consider DBH distibution in study site
#B) If sap flow density is independent with DBH, we do not need to consider DBH when estimate stand-lve sap flow

#----------------
#(0)load the data
#----------------
#-----------
#Tree Basic Info:
#-----------
data.path<-'D:/EE_WSL/IMPACT_project/data_collection/Dav_Data/Other_data_from_collabrators/sap_flow_data/Data_sent_from_RichardPeter/'
#load the sapwood thickness(depth) data--general data for Norway spruce:
data_temp1<-readxl::read_xlsx(paste0(data.path,"Sapwood_thickness.xlsx"))
#load the sapwood thickness(depth) data--data specifically for Davos
data_temp2<-read.csv(file = paste0(data.path,"Sapwood_thickness_Davos.csv"),sep = ",")
##first check for the Davos data, if data_temp1 and data_temp2 has some redandecy:
t_temp1<-data_temp1%>%
  filter(Site=="DAV1840")%>%
  filter(Tree %in% paste0("B",c(28:37)))
#-->by visually the data-->the data_temp1 and data_temp2 are independant-->hence merge two data:
#keep two datasets variables consistent:
data_temp1<-data_temp1 %>%
  mutate(SiteCode=Code,Code=NULL,TreeCode=Tree,Tree=NULL)%>%
  mutate(across(c(StemLength:SWYear),as.numeric))  #convert to numeric values

data_temp2<-data_temp2%>%
  mutate(site.id=NULL,SiteCode="DAV",
         TreeID=tree.id,tree.id=NULL,
         TreeCode=tree.code,tree.code=NULL,
         Species=tree.species,tree.species=NULL,
         DBH=diameter.breast.height_cm,diameter.breast.height_cm=NULL,
         Height=height_m,height_m=NULL,
         SWThick=sapwood.thickness_cm,sapwood.thickness_cm=NULL,
         BarkThick=bark.thickness_cm,bark.thickness_cm=NULL,
         PholemThick=phloem.thickness_mm/10,phloem.thickness_mm=NULL #unit: convert from mm to cm
         )
#merge two datasets:
df.TreeInfo<-full_join(data_temp1,data_temp2)

#-----------
#Relative sap flow ~ relative sapwood depth data
#-----------
df.sap_SWThick<-readxl::read_xlsx(path=paste0(data.path,"Radial_SFD.xlsx"))

#-----------
#DBH and Height information in Davos:
#data sent by Mana and Roman before
#-----------
#I.Data from Mana-->All the tree DBH and Height info
load.path<-"./data/Sapflow/"
load(paste0(load.path,"df.Dav.Demographics.RDA"))
df.DBH_H<-df.Dav.sel%>%
  dplyr::select(SITE_ID,TREE_ID,TREE_DBH,TREE_HEIGHT)%>%
  mutate(sitename=SITE_ID,SITE_ID=NULL)%>%
  mutate(DBH=TREE_DBH,TREE_DBH=NULL,
         Height=TREE_HEIGHT,TREE_HEIGHT=NULL)
#II. Data from Roman-->DBH and Height info only for the trees that have sap flow:
#load the sap flow density (SFD) data:
load.path<-"./data-raw/Data_from_PIs/CH_Dav/sap_flow/"
files<-list.files(load.path)
sel_files<-files[grep(".csv",files)]

#load the details for different trees:DBH and tree height
df.DBH_H_Roman<-read_excel(paste0(load.path,"TreeNumber_details.xlsx"))
df.DBH_H_Roman<-df.DBH_H_Roman%>%
  mutate(sitename=Site,Site=NULL)
#load the sap wood area info sent by Roman
df.A_sapwood_Roman<-read.csv2(paste0(load.path,sel_files[2]),sep = ",")
df.A_sapwood_Roman<-df.A_sapwood_Roman %>%
  mutate(TreeNumber=tree_id,tree_id=NULL,
         sitename="CH-Dav",site_id=NULL,
         SWThick=as.numeric(tree_sapwood_thickness_cm),
         tree_sapwood_thickness_cm=NULL
         )
#
df.metadata_Roman<-left_join(df.DBH_H_Roman,df.A_sapwood_Roman)

#-----------
#sap flow from Davos-->sent by Richard Peter"
#-----------
sap.path<-"D:/EE_WSL/IMPACT_project/data_collection/Dav_Data/Other_data_from_collabrators/sap_flow_data/Data_sent_from_RichardPeter/"
df.sap1<-readRDS(paste0(sap.path,"PCAB_DAV_2010_2019.Rds"))
df.sap2<-readRDS(paste0(sap.path,"PCAB_DAV_2020_2022.Rds"))
#
df.sap1$timestamp<-ymd_hms(df.sap1$timestamp)
df.sap2$timestamp<-ymd_hms(df.sap2$timestamp)
#change the variable names"
df.sap1<-df.sap1 %>%
  rename_with(~sub("PCAB_","Sap_",.))
#merge sap1 and sap2:
df.sap<-bind_rows(df.sap1,df.sap2)
#aggregate to daily-->original SFD unit:cm3 cm-2 h-1
df.sap.daily<-df.sap%>%
  mutate(Date=as.Date(timestamp),HH=hour(timestamp))%>%
  group_by(Date)%>%
  summarise(across(c(starts_with("Sap"),"sfd_mean"),sum,na.rm=TRUE))
#convert the unit of SFD from cm3 cm-2 h-1 to mm m-2 d-1
df.sap.daily<-df.sap.daily %>%
  mutate(across(c(starts_with("Sap"),"sfd_mean"),
                ~ (.*10*24)/c(100*100)))
#
df.sap.daily[df.sap.daily==0]<-NA
df.sap_Richard<-df.sap.daily %>%
  mutate(sfd_mean=NULL)%>%
  pivot_longer(starts_with("Sap"),values_to = "SFDm",names_to = "TreeNumber")%>%
  mutate(TreeNumber=as.numeric(substr(TreeNumber,5,7)))

###compare with the data from Ankit:
load.path<-"./data-raw/Data_from_PIs/CH_Dav/sap_flow/"
df.sap_Ankit<-read.csv2(paste0(load.path,"Dav_SFD_2010_2023_Daily_15trees.csv"),sep = ",")
#checking if the data is consistent:
df1_test<-df.sap_Richard %>%
  mutate(data_source="FromRichard")
df2_test<-df.sap_Ankit %>%
  mutate(data_source="FromAnkit")
#
df_test<-rbind(df1_test,df2_test)
df_test<-df_test %>%
  mutate(SFDm=as.numeric(SFDm))%>%
  filter(!is.na(SFDm))
p_test1<-df_test %>%
  filter(data_source=="FromRichard")%>%
  group_by(TreeNumber)%>%
  ggplot()+
  geom_point(aes(x=Date,y=SFDm),alpha=0.6,col="black")+
  facet_wrap(.~TreeNumber)
p_test2<-df_test %>%
  filter(data_source=="FromAnkit")%>%
  group_by(TreeNumber)%>%
  ggplot()+
  geom_point(aes(x=Date,y=SFDm),alpha=0.6,col="tomato")+
  facet_wrap(.~TreeNumber)
p_comp<-plot_grid(p_test1,p_test2,ncol=2)
#the comparison show the datasets are different-->hence, adopt the Richard's data-->adjusted data
ggsave(p_comp,file=paste0("./test/check_sapflow/","compare_sapflow.png"),width = 10,height = 5)

df.sap<-df.sap
df.sap.daily<-df.sap.daily
#save the sap data:
# save.path<-"./data/Sapflow/"
# save(df.sap,file = paste0(save.path,"df.Davos.sap_hourly_fromRichard.RDA")) #unit:cm3 cm-2 h-1
# save(df.sap.daily,file = paste0(save.path,"df.Davos.sap_daily_fromRichard.RDA")) #unit: mm m-2 d-1

#----------------
#(1)Develop the relationship betwen DBH and SWThick
#----------------
#exploring:
#a.using all the data to develop the linear regression
ggplot(data=df.TreeInfo)+
  geom_point(aes(x=DBH,y=SWThick,col=SiteCode),shape=16,size=5)+
  # geom_smooth(method = "lm", se = FALSE, color = "blue") + # 添加线性回归线
  stat_smooth(aes(x=DBH,y=SWThick),method = "lm", 
              # col = "blue", 
              se = TRUE, level = 0.95) + # 添加置信区间
  stat_regline_equation(aes(x=DBH,y=SWThick,label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        label.x = 3, label.y = 10, 
                        formula = y ~ x) + # 添加回归方程和 R²
  xlab("DBH (cm)")+
  ylab("SWThick (cm)")+
  theme_light()+
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=18))

#b. using the logged data to develop the linear regression
df.TreeInfo<-df.TreeInfo %>%
  mutate(DBH_log=log10(DBH),
         SWThick_log=log10(SWThick)
         )
ggplot(data=df.TreeInfo)+
  geom_point(aes(x=DBH_log,y=SWThick_log,col=SiteCode),shape=16,size=5)+
  # geom_smooth(method = "lm", se = FALSE, color = "blue") + # 添加线性回归线
  stat_smooth(aes(x=DBH_log,y=SWThick_log),method = "lm", col = "blue", se = TRUE, level = 0.95) + # 添加置信区间
  stat_regline_equation(aes(x=DBH_log,y=SWThick_log,label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        label.x = 1, label.y = 1, 
                        formula = y ~ x) + # 添加回归方程和 R²
  xlab("DBH_log (cm)")+
  ylab("SWThick_log (cm)")+
  theme_light()+
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=18))

 #c. develop relationship between SWThick with (DBH and Height)
 lm_2vars<-lm(SWThick ~ DBH + Height,data=df.TreeInfo)  
 summary(lm_2vars) ##R2=0.24-->Height is non-significant

 #hence-->at the end, adpot the linear relationship between SWThick and DBH
 
 #----------------
 #(2)Develop the relationship between relative sapwood depth and relative sap flow
 #----------------
 library(ggpmisc) #显示拟合方程
 #check different speices:
 unique(df.sap_SWThick$Species)
 df.sap_SWThick<-df.sap_SWThick %>%
   mutate(x=as.numeric(`Relative depth`),y= as.numeric(`Relative Fd`)) #for easily to plot 
 #a. only using Norway spruce(Picea abies) data to develop the relationship:
 ggplot(data=df.sap_SWThick%>%filter(Species=="Picea abies"))+
   geom_point(aes(x=x,y=y),shape=16,size=5)+
   stat_smooth(aes(x=x,y=y),method = "loess", 
               # col = "blue", 
               se = TRUE, level = 0.95)+
   # stat_poly_eq(aes(label = after_stat(eq.label)),
   #              formula = y ~ poly(x, 2, raw = TRUE), # 二次多项式拟合
   #              parse = TRUE, label.x.npc = "left", label.y.npc = 0.9) + # 在图中显示拟合方程
   xlab("Relative sapwood depth")+
   ylab("Relative sap flow")+
   theme_light()+
   theme(axis.text = element_text(size=16),
         axis.title = element_text(size=18))
   
 #b. using all the Needle forests data to develop the relationship:
 ggplot(df.sap_SWThick%>%
          filter(Species%in%c("Picea abies","Pinus sylvestris","Conifers")))+
   geom_point(aes(x=`Relative depth`,y=`Relative Fd`,col=Species),shape=16,size=5)+
   stat_smooth(aes(x=`Relative depth`,y=`Relative Fd`,col=Species),method = "loess", 
               # col = "blue", 
               se = TRUE, level = 0.95)+
   xlab("Relative sapwood depth")+
   ylab("Relative sap flow")+
   theme_light()+
   theme(axis.text = element_text(size=16),
         axis.title = element_text(size=18))
 
## the pattern are generally similar and finally taking the relationship using the Norway spruce data
#c. using segmented package to do the segement regression:
 library(segmented)
 # 进行线性回归
 lin.mod <- lm(y ~ x, data = df.sap_SWThick%>%filter(Species=="Picea abies"))
 # 进行分段回归分析
 seg.mod <- segmented(lin.mod, seg.Z = ~x, psi = list(x = c(1)))
 summary(seg.mod)
 coef(seg.mod)
 #plotting
 ggplot(df.sap_SWThick%>%filter(Species=="Picea abies"), aes(x = x, y = y)) +
   geom_point(shape=16,size=5) + # 添加散点
   # geom_line(aes(y = fitted(lin.mod)), color = "red", 
   #           linetype = "dashed",size=1.1) + # 添加初始线性回归线
   geom_line(aes(y = fitted(seg.mod)), color = "blue",size=1.2) + # 添加分段回归线
   xlab("Relative sapwood depth")+
   ylab("Relative sap flow")+
   theme_light()+
   theme(axis.text = element_text(size=16),
         axis.title = element_text(size=18))+
   annotate("text", x = 1.2, y = 1.1, label = paste("Segment 1: y =", 
   round(coef(seg.mod)[1], 2), "", round(coef(seg.mod)[2], 2), "* x",
   "       x<=1"), color = "blue",size=6) +
   annotate("text", x = 1.2, y = 1, label = paste("Segment 2: y = 0", 
   # round(coef(seg.mod)[1] + coef(seg.mod)[3], 2), "+", round(coef(seg.mod)[2] + coef(seg.mod)[4], 2), "* x"), color = "blue")
   "       x>1"),color="blue",size=6)

 #----------------
 #(3)Check the DBH distribution and its relation with sap flow
 #----------------
 #a. check the distribution of DBH of all trees in Davos(data sent by Mana):
 df.DBH_H %>%
   ggplot(aes(x=DBH))+
   geom_histogram(aes(y = ..density..),fill="tomato",alpha=0.5)+
   geom_density(col="blue")+
   theme_light()
 
 #b. check the realtionship betwen SFD (data from Richard) and DBH(data from Roman):
 #sap data
 df.sap.daily_adj<-df.sap.daily %>%
   mutate(sfd_mean=NULL)%>%
   pivot_longer(starts_with("Sap"),values_to = "SFDm",names_to = "TreeNumber")%>%
   mutate(TreeNumber=as.numeric(substr(TreeNumber,5,7)))
 #SWThick and SW area
 #calculate missing SWThick in df.metadata_Roman based on the relationship build by (1)
 lm_SWThick_DBH<-lm(SWThick~DBH,data=df.TreeInfo)
 summary(lm_SWThick_DBH)
 df.meta_update<-df.metadata_Roman %>%
   mutate(SWThick=ifelse(!is.na(SWThick),SWThick,
          coef(lm_SWThick_DBH)[1]+coef(lm_SWThick_DBH)[2]*DBH))
 #calculate the SW area:
 df.meta_update<-df.meta_update %>%
   #calculate the sap wood area(cm2)
   mutate(A_sapwood=pi*c(c(DBH/2)^2-(DBH/2 - SWThick)^2))
 #aggregate the daily.sap with meta.data
 df.sap.agg<-left_join(df.sap.daily_adj,
                       df.meta_update %>% dplyr::select(TreeNumber:sitename,SWThick:A_sapwood))
 ##
 df.sap.agg_Monthly<-df.sap.agg %>%
   mutate(Year=year(Date),Month=month(Date))%>%
   group_by(TreeNumber,DBH,A_sapwood,Month)%>%
   summarise(SFDm_m=mean(as.numeric(SFDm),na.rm=T))
 #-----------------------
 #exploring sap.Monthly with DBH with plotting
 #-----------------------
 #the results shows the relationship between DBH and SFD has a linear relationship
 #check using the daily data
 # df.sap.agg %>%
 #   ggplot()+
 #   geom_point(aes(x=DBH,y=SFDm,col=as.factor(TreeNumber)))+
 #   stat_smooth(aes(x=DBH,y=SFDm),method = "lm", col = "blue", se = TRUE, level = 0.95) + # 添加置信区间
 #   stat_regline_equation(aes(x=DBH,y=SFDm,label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
 #                         label.x = 45, label.y = 2.5,
 #                         formula = y ~ x) + # 添加回归方程和 R²
 #   xlab("DBH(cm)")+
 #   ylab("SFD (mm d-1)")+
 #   theme_light()+
 #   theme(axis.text = element_text(size=16),
 #         axis.title = element_text(size=18))
 
 df.sap.agg_Monthly%>%
   ggplot()+
   geom_point(aes(x=DBH,y=SFDm_m,col=as.factor(TreeNumber)))+
   stat_smooth(aes(x=DBH,y=SFDm_m),method = "lm", col = "blue", se = TRUE, level = 0.95) + # 添加置信区间
   stat_regline_equation(aes(x=DBH,y=SFDm_m,label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                         label.x = 45, label.y = 2.5, 
                         formula = y ~ x) + # 添加回归方程和 R²
   xlab("DBH(cm)")+
   ylab("SFD (mm d-1)")+
   theme_light()+
   theme(axis.text = element_text(size=16),
         axis.title = element_text(size=18))
 
 df.sap.agg_Monthly%>%
   ggplot()+
   geom_point(aes(x=A_sapwood,y=SFDm_m,col=as.factor(TreeNumber)))+
   stat_smooth(aes(x=A_sapwood,y=SFDm_m),method = "lm", col = "blue", se = TRUE, level = 0.95) + # 添加置信区间
   stat_regline_equation(aes(x=A_sapwood,y=SFDm_m,label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                         label.x = 45, label.y = 2.5, 
                         formula = y ~ x,size=6) + # 添加回归方程和 R²
   xlab("A_sapwood(cm2)")+
   ylab("SFD (mm d-1)")+
   theme_light()+
   theme(axis.text = element_text(size=16),
         axis.title = element_text(size=18))
 
 #----------------
 #(4)calculate the stand-level sap flow:
 #----------------
 #Now I will use two ways of approach to estimate the stand-level sapflow:
 #A.sap flow data only based no the DBH classes
 #B. based on the SFD linear relationship with DBH(monthly data)-->based on tree demographics in Davos,
 #calculate the weight-averaged stand-level sap flow:
 #C. based on 1) the relationship between relative SFD and relative SWthick, 2) based on DBH classes
 
 
 #A.first approach-->do not consider the variation of sap-flow in depth for each trees
 lm_SFD_DBH<-lm(SFDm_m ~ DBH,data=df.sap.agg_Monthly)
 summary(lm_SFD_DBH)
 
 #demographics of trees in Davos:
 hist(df.DBH_H$DBH)
 
 #estimate all the trees SFD in Davos (currently only considering the trees with DBH >20 cm)
 #becuase of all the trees with sap flow are >20
 df.DBH_reclass<-df.DBH_H %>%
   filter(DBH>=20)%>%
   #categories the trees with different DBH classes
   mutate(DBH_flag=case_when(
     # DBH<20 ~ "<20",
     DBH>=20 & DBH<=30 ~"20-30",
     DBH>=30 & DBH<=40 ~"30-40",
     DBH>=40 & DBH<=50 ~"40-50",
     DBH>50 ~">50"
     ))
 df.DBH_sum<-df.DBH_reclass %>%
  group_by(DBH_flag)%>%
  summarise(prop=length(DBH_flag)/nrow(df.DBH_reclass))
 #-------------------
 #estimate stand-level sap flow
 #------------------
 df.sap.agg_f<-df.sap.agg %>%
   mutate(DBH_flag=case_when(
     # DBH<20 ~ "<20",
     DBH>=20 & DBH<=30 ~"20-30",
     DBH>=30 & DBH<=40 ~"30-40",
     DBH>=40 & DBH<=50 ~"40-50",
     DBH>50 ~">50"
   ))
 #
 df.sap.agg_f<-left_join(df.sap.agg_f,df.DBH_sum)
 #stand-level daily SFD
 df_stand.sap.daily<-df.sap.agg_f %>%
   group_by(Date)%>%
   summarise(SFD_mean=mean(SFDm,na.rm=T),
             #calcuclate sap flow:method2:consideirng tree demogrphaic info for different DBH calesses
             SFD_mean_adj=sum(SFDm*A_sapwood*prop,na.rm = T)/sum(A_sapwood*prop,na.rm=T)
   )
 df_stand.sap.daily %>%
   ggplot()+
   geom_point(aes(x=Date,y=SFD_mean,col="SFD_mean"))+
   geom_point(aes(x=Date,y=SFD_mean_adj,col="Adjusted SFD_mean"))+
   ylab(expression("Sap flow (mm d"^-1*")"))
 
 #B and C approach are both too complicated to calculate-->hence only using method A to calculate the stand-level sap flow
 #C. considering the variation of sap flow in different depth
 #calculate the relative SWThick:
d
#----------------
#(5)save the data
#----------------
df.all<-list(df.meta_update,
             df_stand.sap.daily)
names(df.all)<-c("metadata","stand.sap.daily")
#save the data:
save.path<-"./data/Sapflow/"
save(df_stand.sap.daily,file = paste0(save.path,"df.Davos.sap_daily_fromRichard.RDA"))

#save the data for Weigeng's analysis:
df.sent<-df_stand.sap.daily %>%
  dplyr::select(Date,SFD_mean_adj)%>%
  mutate(sap_daily=SFD_mean_adj,SFD_mean_adj=NULL)
#save the data in csv:
write.csv(df.sent,file = paste0(save.path,"Davos_daily_sapflow_sent.csv"))
