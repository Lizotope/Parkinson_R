################ Practice to be given before 18th of Jan 2022 ##################
###############################################################################
# This file presents source code giving Parkinson dataset analysis
# Team : Franck Saoude and Elisabeth Bourgeois 
################################################################################
# START
################################################################################
# 
# Packages needed (not exhaustive list):
# factoextra, Hmisc, tidyverse, Nbclust, ggplot2, plotrix,corrplot, 
# sm, zoo, vioplot
# uncomment the following cmd to install one of them, if needed : 
# install.packages("package_name") 
# example : install.packages("plotrix") 
#
################################################################################
# STEP 1 : DATA PREPARATION
################################################################################

# Libraries
library(sm)
library(zoo)
library(ggplot2)
library(vioplot)
library(Hmisc)
library("FactoMineR")
library("factoextra")
library("corrplot")
library(reshape2)
library(plotrix)
library(tidyverse) 
library(NbClust)
library(cluster)

getwd()
setwd("/home/liz/Documents/MS_Big_Data_TP_et_projets/Data Mining/Projet/Parkinson_R")
P_init<-read.table("parkinsons.data", header = FALSE, sep = ",", quote = "", dec = ".", 
                   row.names=1, col.names=c("Signal_Id","MDVP_Fo", "MDVP_Fhi", "MDVP_Flow","MDVP_JitterRel", 
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

# standardisation des données (Ca va servir pr kmeans, pam et pca)
P_initsc<-scale(P_init)
P_hsc<-scale(P_h)
P_pdsc<-scale(P_pd)

# Split des données patients malades vs.sains pour voir si les outliers seraient 
# en lien avec le caractère malade du patient
# P_pd est le dataset contenant les signaux de patients malades
P_pd <- P_init[P_init$status==1,]
describe(P_pd)
# P_h est le dataset contenant les signaux de patients malades
P_h <- P_init[P_init$status==0,]
describe(P_h)

M_init <- as.matrix(P_init, rownames.force = TRUE)

