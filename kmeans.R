CA_data=read.csv('E:/学习/CV/2019HW/DATA/road-traffic-injuries-2002-2010.csv',stringsAsFactors = F)
library(caret)
library(gmodels)
library(dplyr)
library(gplots)
library(ROCR)
library(plotROC)
library(rpart)
library(rpart.plot)
library(ranger)
library(tidyverse)
library(e1071)
library(cluster)
library(factoextra)
CD_data=CA_data%>%filter(geotype=='CD')
CD_data=CD_data%>%filter(mode=='All modes',reportyear==2002)
new_CD=CD_data[,c(9,14,15)]
new_CD = new_CD %>% 
  mutate(severity = factor(if_else(severity == "Killed", "1", "0"), 
                           levels = c("0", "1")))
head(new_CD)
new_CD_0<-new_CD%>%filter(severity==0)
new_CD_1<-new_CD%>%filter(severity==1)
new_CD$severity1<-new_CD_0
write.csv(new_CD_1,'E:/学习/CV/2019HW/DATA/new_CD_1.csv',row.names = F)
write.csv(new_CD_0,'E:/学习/CV/2019HW/DATA/new_CD_0.csv',row.names = F)
CD=read.csv('E:/学习/CV/2019HW/DATA/CD.csv',stringsAsFactors = F)
row.names(CD)<-CD$county_name
CD=CD[,-1]
CD=CD[-19,]
a=scale(CD[,c(2,4)])
a
k2 <- kmeans(a, centers = 5, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = a)
set.seed(123)
gap_stat <- clusGap(a, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
print(gap_stat, method = "firstmax")
fviz_nbclust(a, kmeans, method = "silhouette")
p1 <- fviz_cluster(k2, geom = "point", data = a) + ggtitle("k = 2")
p1
