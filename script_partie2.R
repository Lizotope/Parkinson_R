################ Practice to be given before 18th of Jan 2022 ##################
###############################################################################
# This file presents source code giving Parkinson dataset analysis
# Team : Franck Saoude and Elisabeth Bourgeois 
################################################################################
# START
################################################################################
# 
# Packages needed (not exhaustive list):
# factoextra, Hmisc, tidyverse, Nbclust, ggplot2
# uncomment the following cmd to install one of them, if needed : 
# install.packages("package_name") 
# example : install.packages("NbClust") 
#
################################################################################
# STEP 1 : DATA PREPARATION
################################################################################
getwd()
setwd("/home/liz/Documents/MS_Big_Data_TP_et_projets/Data Mining/Projet/Parkinson_R")
P_init<-read.table("parkinsons.data", header = FALSE, sep = ",", quote = "", dec = ".", 
                   row.names=1, col.names=c("Signal_Id","MDVP_Fo", "MDVP_Fhi", "MDVP_Flow ","MDVP_JitterRel", 
                                            "MDVP_JitterAbs","MDVP_Rap","MDVP_PPQ","Jitter_DDP",
                                            "MDVP_Shimmer","MDVP_ShimmerDB","Shimmer_APQ3",
                                            "Shimmer_APQ5","MDVP_APQ","Shimmer_DDA","NHR","HNR",
                                            "status","RPDE","DFA","spread1","spread2",
                                            "D2","PPE"),
                   as.is = FALSE, na.strings = "NA",
                   colClasses = NA, nrows = -1,
                   skip = 1, check.names = TRUE, fill = !TRUE,
                   strip.white = FALSE, blank.lines.skip = TRUE,
                   comment.char = "#")

# je vérifie le bon nommage des attributs/individus, la dim, le type des attributs
rownames(P_init)    # lines
colnames(P_init)    # columns
names(P_init)       # attributes
dim(P_init)         # dim
str(P_init)         # types
# visualisation en tableau du dataset de travail
View(P_init)

################################################################################
# STEP 2 : DESCRIPTIVE ANALYSIS
################################################################################
# Statistiques descriptives
#
# Min, max, moy, mediane, 1er et 3ème quartiles,  des 23 paramètres quantitatifs 
# étudiés
summary(P_init)
# stats complémentaires
# infos sur le nb d'observations, sur l'existence de val manquantes, 
# et autres quantiles
library(Hmisc)
describe(P_init)

# Représentation graphique



################################################################################
# STEP 3 : EXPLORATING ANALYSIS
################################################################################
#

library(tidyverse) 

#
# je m'assure que toutes les données sont définies
all(!is.na(P_init))
# si il y en a une qui ne l'est pas, je cherche laquelle
lapply(P_init,function(x) which(is.na(x)))

# standardisation des données
P_sc<-scale(P_init)
label<-attributes(P_sc)$dimnames[[1]]
plot(P_sc, type="n")
text(P_sc,label)

boxplot(rang)


# Algo kmeans sur 2 clusters
P2<-kmeans(P_sc,2)
P2

# Eval du nb de cluster optimal
######################################

library(factoextra)
library(NbClust)

#Tracé de la fonction du courde
{
  Tab<- NULL
  for(k in 1:10){
    Res<-kmeans(P_sc,k)
    Tab[k]= Res$tot.withinss/Res$totss
  }
  plot(Tab, typ='l')
}

# Tracé de la valeur silhouette
fviz_nbclust(P_sc, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method") 

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(P_sc, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method") 


# visu des clusters
km.out2=kmeans(P_sc,centers=2,nstart =20)
km.out2
pairs(B_full_sc, col=c(1:2)[km.out2$cluster]) 
km.out$cluster
# coord des centroïdes
km.out=kmeans(P_sc,centers=4,nstart =20)
km.out$centers

# acp
# visu des val aberrantes !
fviz_cluster(km.out2, P_sc, ellipse.type = "norm") 
################################################################################
# END
################################################################################
