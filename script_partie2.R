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
lbls <- paste(c("Healthly", "Parkinson Disease"), "\n",mytable, sep="")
pie3D(mytable, col=c("purple","#dd00dd"), labels = lbls, explode=0.1,
    main="Nb of signals corresponding \n with status patient") 

# les boxplots suivants permettent de juger la dispersion et repérer les 
# éventuelles valeurs aberrantes

# Split des données patients malades vs.sains pour voir si les outliers seraient 
# en lien avec le caractère malade du patient
# P_pd est le dataset contenant les signaux de patients malades
P_pd <- P_init[P_init$status==1,]
describe(P_pd)
# P_h est le dataset contenant les signaux de patients malades
P_h <- P_init[P_init$status==0,]
describe(P_h)

# le split ci dessus sert à repérer sur quels sous partie du dataset se 
# focaliserait les outliers
# cette recherche ne se fera que pr une partie des attributs pr lesquels nous 
# constatons bcp de point en dehors de la boxplot

#boxplot de l'attribut MDVP_ShimmerDB en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$MDVP_ShimmerDB, P_pd$MDVP_ShimmerDB, P_h$MDVP_ShimmerDB,
        col = c("purple"),           #Pour la couleur
        main = paste("MDVP_ShimmerDB Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées


#boxplot d'attributs (regroupements avec attributs de même ordre de grandeur)
boxplot(P_init[,c('RPDE','DFA','spread2')],
        col = c("pink"),           #Pour la couleur
        main = paste("RPDE, DFA, spread2 and PPE Boxplot"),     #Pour le titre
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

#boxplot de l'attribut PPE en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$PPE, P_pd$PPE, P_h$PPE,
        col = c("purple"),           #Pour la couleur
        main = paste("PPE Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

#boxplot de l'attribut MDVP_ShimmerDB en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$MDVP_ShimmerDB, P_pd$MDVP_ShimmerDB, P_h$MDVP_ShimmerDB,
        col = c("purple"),           #Pour la couleur
        main = paste("MDVP_ShimmerDB Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

boxplot(P_init$MDVP_JitterAbs, P_pd$MDVP_JitterAbs, P_h$MDVP_JitterAbs,
        col = c("purple"),           #Pour la couleur
        main = paste("MDVP_JitterAbs Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# on voit ci-dessous que le caractère malade ne change pas vraiment l'occurence 
# d'outliers
boxplot(P_init$Jitter_DDP, P_pd$Jitter_DDP, P_h$Jitter_DDP,
        col = c("purple"),           #Pour la couleur
        main = paste("Jitter_DDP Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# a spliter
#boxplot d'attributs de même ordre de grandeur
boxplot(P_init[,c('MDVP_JitterRel','MDVP_Rap','MDVP_PPQ')],
        col = c("pink"),           #Pour la couleur
        main = paste("Boxplot"),     #Pour le titre
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# a spliter
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
# --> split des dataset machant avec des sujets sains et de ceux qui sont malades

# graphique en ligne de crêtes, exemple avec HNR et D2
# Libraries
library(sm)
library(zoo)
library(ggplot2)
library(vioplot)

vioplot( P_pd$HNR, P_pd$D2, col = "palevioletred", plotCentre = "line", 
        side = "left",  names=c("HNR", "D2"))
vioplot(P_h$HNR, P_h$D2 , data = P_h, col = "lightblue", plotCentre = "line", 
        side = "right", add = T)
legend("bottomright", fill = c("palevioletred", "lightblue"), legend = c("PD", 
                                                                         "Healthly"), title = "Status")
stripchart(P_h$HNR, method = "jitter", col = "blue",
           vertical = TRUE, pch = 50, add = TRUE)
stripchart(P_pd$HNR, method = "jitter", col = "pink",
           vertical = TRUE, pch = 50, add = TRUE)


# jenleve le status
P_stless <- P_init[ , -17]
P_stless

# mat de correlation (coeff de Peason par defaut)
Mcor_P_stless <- round(cor(P_stless),2)
View(Mcor_P_stless)

# heatmap
library(corrplot)
corrplot(Mcor_P_stless, type="upper", order="hclust", tl.col="black", tl.srt=45)

################################################################################
# Fondre la matrice de corrélation
library(reshape2)
melted_cormat <- melt(Mcor_P_stless)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Obtenir le triangle inférieur
get_lower_tri<-function(Mcor_P_stless){
  Mcor_P_stless[upper.tri(Mcor_P_stless)] <- NA
  return(Mcor_P_stless)
}
# Obtenir le triangle supérieur
get_upper_tri <- function(Mcor_P_stless){
  Mcor_P_stless[lower.tri(Mcor_P_stless)]<- NA
  return(Mcor_P_stless)
}
#utilisation
upper_tri <- get_upper_tri(Mcor_P_stless)
upper_tri
# Fondre la matrice de corrélation
melted_cormat <- melt(upper_tri, na.rm = TRUE)

library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


reorder_cormat <- function(Mcor_P_stless){
  # Utiliser la corrélation entre les variables
  # comme mésure de distance
  dd <- as.dist((1-Mcor_P_stless)/2)
  hc <- hclust(dd)
  Mcor_P_stless <-Mcor_P_stless[hc$order, hc$order]
}


# Reordonner la matrice de corrélation
Mcor_P_stless <- reorder_cormat(Mcor_P_stless)
upper_tri <- get_upper_tri(Mcor_P_stless)
# Fondre la matrice de corrélation
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Créer un ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Afficher heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
# source : http://www.sthda.com/french/wiki/ggplot2-heatmap-d-une-matrice-de-corr-lation-logiciel-r-et-visualisation-de-donn-es
################################################################################
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# utiliser une heatmap : il faut transformer le dataframe em matrice
M_init <- as.matrix(P_init, rownames.force = TRUE)
# je vérifie le type
class(M_init)
class(P_init)

heatmap(M_init)

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
