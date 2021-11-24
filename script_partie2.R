################ Practice to be given before 18th of Jan 2022 ##################
###############################################################################
# This file presents source code giving Parkinson dataset analysis
# Team : Franck Saoude and Elisabeth Bourgeois 
################################################################################
# START
################################################################################
# 
# Packages needed (not exhaustive list):
# factoextra, Hmisc, tidyverse, Nbclust, ggplot2, plotrix
# uncomment the following cmd to install one of them, if needed : 
# install.packages("package_name") 
# example : install.packages("plotrix") 
#
################################################################################
# STEP 1 : DATA PREPARATION
################################################################################
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

# méthodologie : 
# https://odr.inra.fr/intranet/carto/cartowiki/index.php/Statistiques_descriptives_avec_R

head(P_init)

# Représentation graphique

#boxplot d'attributs de même ordre de grandeur
boxplot(P_init[,c('MDVP_Fo', 'MDVP_Fhi', 'MDVP_Flow')],
        col = c("yellow"),           #Pour la couleur
        main = paste("MDVP Frequencies Boxplot"),     #Pour le titre
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées
# --> on visualise des données déjà "originales"


# camembert 3d pour attribut binaire
# permet de visualiser le ratio des signaux de patients sains vs malades
library(plotrix)
mytable <- table(P_init$status)
names(mytable)
lbls <- paste(c("Parkinson Disease", "Heathly"), "\n",mytable, sep="")
pie3D(mytable, col=c("purple","#dd00dd"), labels = lbls, explode=0.1,
    main="Nb of signals corresponding \n with status patient") 

# les boxplots suivants permettent de juger la dispersion et repérer les 
# éventuelles valeurs aberrantes

#boxplot d'attributs de même ordre de grandeur
boxplot(P_init[,c('MDVP_ShimmerDB')],
        col = c("purple"),           #Pour la couleur
        main = paste("MDVP_ShimmerDB Boxplot"),     #Pour le titre
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

#boxplot d'attributs de même ordre de grandeur
boxplot(P_init[,c('RPDE','DFA','spread2','PPE')],
        col = c("pink"),           #Pour la couleur
        main = paste("RPDE, DFA, spread2 and PPE Boxplot"),     #Pour le titre
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

#boxplot d'attributs de même ordre de grandeur
boxplot(P_init[,c('MDVP_JitterRel', 
                  'MDVP_JitterAbs','MDVP_Rap','MDVP_PPQ','Jitter_DDP')],
        col = c("pink"),           #Pour la couleur
        main = paste("Boxplot"),     #Pour le titre
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

#boxplot d'attributs de même ordre de grandeur
boxplot(P_init[,c('MDVP_Shimmer','Shimmer_APQ3',
                  'Shimmer_APQ5','MDVP_APQ','Shimmer_DDA','NHR')],
        col = c("pink"),           #Pour la couleur
        main = paste("Boxplot"),     #Pour le titre
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

#boxplot d'attributs de même ordre de grandeur
boxplot(P_init[,c('spread1')],
        col = c("green"),           #Pour la couleur
        main = paste("Spread1 Boxplot"),     #Pour le titre
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

#boxplot d'attributs de même ordre de grandeur
boxplot(P_init[,c('D2')],
        col = c("blue"),           #Pour la couleur
        main = paste("D2 Boxplot"),     #Pour le titre
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

#boxplot d'attributs de même ordre de grandeur
boxplot(P_init$HNR,
        col = c("yellow"),           #Pour la couleur
        main = paste("HNR"),     #Pour le titre
        sub= paste("Boxplot"),      # pour le sous-titre
        ylab = "Quantiles")         #Pour le titre de l’axe des ordonnées

# Ccl : des val aberrantes potentielles ?

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



######## voir ce qui se fait ici :
# https://www.tidymodels.org/learn/statistics/k-means/
# https://delladata.fr/kmeans/

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
