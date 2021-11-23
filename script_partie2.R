################ TP A RENDRE POUR LE 18 JANVIER 2021 ##########################
###############################################################################
# Ce fichier présente le code source permettant l'analyse du dataset d'étude
# Binôme : Franck Saoude et Elisabeth Bourgeois 
################################################################################
# DEBUT
################################################################################
# 
# Etape 1 : PREPARATION DES DONNEES
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
# Etape 2 : ANALYSE DESCRIPTIVE

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


library(tidyverse) 


B_full<-A_full[-1]
B_full

all(!is.na(B_full))
lapply(B_full,function(x) which(is.na(x)))
Bk<-kmeans(B_full,2)
Bk
#Tracé de la fonction du courde
#source("/home/liz/Documents/MS Big Data/Data Mining/TP3-Classification/fct_coude.R")
{
  Tab<- NULL
  for(k in 1:10){
    Res<-kmeans(B_full,k)
    Tab[k]= Res$tot.withinss/Res$totss
  }
  plot(Tab, typ='l')
}


################################################################################
# FIN
################################################################################
