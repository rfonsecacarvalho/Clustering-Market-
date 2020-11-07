

#---------------------------------
#Libraries
#--------------------------------
library(readr)
library(cluster) 
library(dplyr) 
library(ggplot2) 
library(caret)
library(factoextra)
library(Rtsne)
#-----------------------------------
#Set work directory
#------------------------------------

setwd("C:/Users/rfons/OneDrive/DANA 4840/Assignment2")

#--------------------------------------
#Load the data
#--------------------------------------
data <- read.csv("SMART.csv")
View(data)
summary(data)

data1 <- data

#Data class
class(data1)

#Dimensions
dim(data1)

#Column Names
names(data1)

#Structure of the data
str(data1)

#-------------------------------
#Data preprocessing
#-------------------------------

#Data has many missing values so imputation of values was done
data1$SEX[data1$SEX=="0"]<-2 #convert to the most frequent value "female"
data1$FEH[data1$FEH=="0"]<-1  # Most frequent value is vegetarian
data1$MT[data1$MT=="0"]<-10 #most common language in India(HINDI)
data1$HS[data1$HS=="0"]<-5 #average number of people in household
data1$EDU[data1$EDU=="0"]<-5 # majority of student have 12th standard education
data1$CS[data1$CS=="0"]<-2 #We can't assume that the majority has TV


#---------------------------------
#Creating New variable
#---------------------------------

#creating Max score column
max(data1$Br..Cd..24)#1
max(data1$Br..Cd..272)#0.96
max(data1$Br..Cd..352)#0.99
max(data1$Br..Cd..481)#0.9
max(data1$Br..Cd..5)#0.97
max(data1$Br..Cd..55)#1
max(data1$Br..Cd..57..144)#1
max(data1$Br..Cd..286)#1


#creating new column called max score for all the brands
data1$max_num_score<-apply(data1[, 23:30], 1, max)

#Removing member ID
data1$ï..Member.id=NULL
View(data1)

#-----------------------------------
#Subset the data for k medoids
#-----------------------------------

var1 <- select(data1, No..of.Brands , Brand.Runs, Trans...Brand.Runs,
               No..of..Trans,Value, Vol.Tran, Avg..Price, Others.999, 
               max_num_score)

var1.norm <- mutate_all(var1, scale)

var2 <- select(data1, Pur.Vol.No.Promo....,Pur.Vol.Promo.6..,
               Pur.Vol.Other.Promo..,Pr.Cat.1,Pr.Cat.2,Pr.Cat.3,Pr.Cat.4,
               PropCat.5,PropCat.6, PropCat.7,PropCat.8, PropCat.9, PropCat.14)

var2.norm <- mutate_all(var2, scale)

var3 <- select(data1, No..of.Brands , Brand.Runs, Trans...Brand.Runs, 
               No..of..Trans,Value, Trans...Brand.Runs, Vol.Tran,Avg..Price, Others.999,
               max_num_score,Pur.Vol.No.Promo....,Pur.Vol.Promo.6..,
               Pur.Vol.Other.Promo..,Pr.Cat.1,Pr.Cat.2,Pr.Cat.3,Pr.Cat.4, PropCat.5,
               PropCat.6, PropCat.7, PropCat.8, PropCat.9,PropCat.14 )

var3.norm <- mutate_all(var3, scale)

#------------------------------
#k-medoids
#------------------------------
#Finding K

fviz_nbclust(var1.norm, pam, method = "silhouette")+
  theme_classic() #k =3

fviz_nbclust(var2.norm, pam, method = "silhouette")+
  theme_classic() #k=2

fviz_nbclust(var3.norm, pam, method = "silhouette")+
  theme_classic() #k=2

#----------------------------------
#Purchase behavior cluster
#----------------------------------

pam1 <- eclust(var1.norm,FUNcluster="pam", k=3,hc_metric = "manhattan")
summary(pam1)
pam1$medoids
head(pam1$clustering)
pam1$size


#Data frame with cluster result for purchase behavior
pb_cluster <- cbind(var1, cluster = pam1$cluster)
head(pb_cluster, n = 3)


#Graphical Display

pam1.sil<-silhouette(pam1$cluster, dist(var1.norm))
fviz_silhouette(pam1.sil)

#convert the data set into principle component object
pcadat1 <- prcomp(var1.norm, scale. = TRUE)
#plot the pca with the corresponding RKM clustering result 
pcabiplot(pcadat1, colobj = pam1$cluster, o.size = 2)


#Barplot
barplotnum(var1.norm, pam1$cluster, alpha = 0.05)


#-----------------------------------------
#Basis for purchase clustering
#-----------------------------------------

pam2 <- eclust(var2.norm,FUNcluster="pam", k=2,hc_metric = "manhattan")
summary(pam2)
pam2$medoids
head(pam2$clustering)


#Data frame with cluster result for basis for purchase
bp_cluster <- cbind(var2, cluster = pam2$cluster)
head(bp_cluster, n = 3)


#Graphical Display

pam2.sil<-silhouette(pam2$cluster, dist(var2.norm))
fviz_silhouette(pam2.sil)

#convert the data set into principle component object
pcadat2 <- prcomp(var2.norm, scale. = TRUE)
#plot the pca with the corresponding RKM clustering result 
pcabiplot(pcadat2, colobj = pam2$cluster, o.size = 2)


#Barplot
barplotnum(var2.norm, pam2$cluster, alpha = 0.05)

#---------------------------------------------------------------
#Combined
#---------------------------------------------------------------

pam3 <- eclust(var3.norm,FUNcluster="pam", k=2,hc_metric = "manhattan")
summary(pam3)
pam3$medoids
head(pam3$clustering)


#Data frame with cluster result for purchase behavior and basis for purchase combined
comb_cluster <- cbind(var2, cluster = pam2$cluster)
head(comb_cluster, n = 3)


#Graphical Display

pam3.sil<-silhouette(pam3$cluster, dist(var3.norm))
fviz_silhouette(pam3.sil)


#convert the data set into principle component object
pcadat3 <- prcomp(var3.norm, scale. = TRUE)
#plot the pca with the corresponding RKM clustering result 
pcabiplot(pcadat3, colobj = pam3$cluster, o.size = 2)


#Barplot
barplotnum(var3.norm, pam3$cluster, alpha = 0.05)


#--------------------------------
#GROUPING PER SELECTED VARIABLES
#---------------------------------

#Purchase Behavior Cluster

data1$segment <- pam1$cluster


demographic.profiles1 <- summarize_at(
  group_by(data1, segment),
  vars(SEC, FEH,SEX, AGE,EDU,HS,CHILD, CS, Affluence.Index), 
  mean
)
head(demographic.profiles1)

price.profiles1 <- summarize_at(
  group_by(data1,segment),
  vars(Pr.Cat.1,Pr.Cat.2,Pr.Cat.3,Pr.Cat.4),
  mean
)

head(price.profiles1)

#Basis for purchase

data1$segment2 <- pam2$cluster


demographic.profiles2 <- summarize_at(
  group_by(data1, segment),
  vars(SEC, FEH,SEX, AGE,EDU,HS,CHILD, CS, Affluence.Index), 
  mean
)
head(demographic.profiles2)

price.profiles2 <- summarize_at(
  group_by(data1,segment2),
  vars(Pr.Cat.1,Pr.Cat.2,Pr.Cat.3,Pr.Cat.4),
  mean
)

head(price.profiles2)

#Combined

data1$segment3 <- pam3$cluster


demographic.profiles3 <- summarize_at(
  group_by(data1, segment3),
  vars(SEC, FEH,SEX, AGE,EDU,HS,CHILD, CS, Affluence.Index), 
  mean
)
head(demographic.profiles3)

price.profiles3 <- summarize_at(
  group_by(data1,segment),
  vars(Pr.Cat.1,Pr.Cat.2,Pr.Cat.3,Pr.Cat.4),
  mean
)

head(price.profiles3)