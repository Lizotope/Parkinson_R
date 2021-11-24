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
A_init<-read.table("parkinsons.data_headerless")
# voir si read.csv peut convenir
dim(A_init)
label<-attributes(A_init)$row.names
# librairie pour permettre le changement des col
library(dplyr, quietly = TRUE)
A_full <- A_init %>% rename( SignalId= V1, MDVP_Fo = V2 , MDVP_Fhi = V3, MDVP_Flow = V4, 
                    MDVP_JitterRel = V5, MDVP_JitterAbs =V6, MDVP_Rap=V7, 
                    MDVP_PPQ = V8, Jitter_DDP = V9, MDVP_Shimmer = V10, 
                    MDVP_ShimmerDB = V11, Shimmer_APQ3 = V12, 
                    Shimmer_APQ5 = V13, MDVP_APQ = V14, Shimmer_DDA = V15, 
                    NHR = V16, HNR = V17, status = V18, RPDE = V19, DFA = V20, 
                    spread1 = V21, spread2 = V22, D2 = V23, PPE = V24
)
# verification du changement de nom des colonnes
names(A_full)
# visualisation tableau du dataset de travail
View(A_full)

################################################################################
# STEP 2 : DESCRIPTIVE ANALYSIS
################################################################################
# Statistiques descriptives
#
# Min, max, moy, mediane, 1er et 3ème quartiles,  des 23 paramètres quantitatifs 
# étudiés
summary(A_full)
# stats compkémentaires
# infos sur le nb d'observations, sur l'existence de val manquantes, 
# et autres quantiles
library(Hmisc)
describe(A_full)

# Représentation graphique



################################################################################
# STEP 3 : EXPLORATING ANALYSIS
################################################################################
#


# le kmeans ne fonctionne pas en présence de la colonne SignalId car 
# ce sont des chaînes de caractères
# retrait de cette colonne 1
B_full<-A_full[-1]
B_full

#
# je m'assure que toutes les données sont définies
all(!is.na(B_full))
# si il y en a une qui ne l'est pas, je cherche laquelle
lapply(B_full,function(x) which(is.na(x)))


# Standardisation des données
B_full_sc <- scale(B_full)
view(B_full_sc)

library(tidyverse) 
# Algo kmeans sur 2 clusters
B2<-kmeans(B_full_sc,2)
B2

# Eval du nb de cluster optimal
######################################

library(factoextra)
library(NbClust)

#Tracé de la fonction du courde
#source("/home/liz/Documents/MS Big Data/Data Mining/TP3-Classification/fct_coude.R")
{
  Tab<- NULL
  for(k in 1:10){
    Res<-kmeans(B_full_sc,k)
    Tab[k]= Res$tot.withinss/Res$totss
  }
  plot(Tab, typ='l')
}

# Tracé de la valeur silhouette
fviz_nbclust(B_full_sc, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method") 

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(B_full_sc, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method") 


# visu des clusters
km.out2=kmeans(B_full_sc,centers=2,nstart =20)
pairs(B_full_sc, col=c(1:2)[km.out2$cluster]) 

# acp
# visu des val aberrantes !
library(factoextra)
fviz_cluster(km.out2, B_full_sc, ellipse.type = "norm") 
################################################################################
# END
################################################################################
