
################################################################################
# STEP 3 : EXPLORATING ANALYSIS
################################################################################
#
# STEP 3. 1 CLASSIFICATION NON SUPERVISEE
################################################################################


#
# je m'assure que toutes les données sont définies
all(!is.na(P_init))
# si il y en a une qui ne l'est pas, je cherche laquelle
lapply(P_init,function(x) which(is.na(x)))

#affichage boite à moustaches centré réduit
boxplot(P_initsc)
boxplot(P_hsc)
boxplot(P_pdsc)
# les autres boxplot précédents sont inutiles ?

label<-attributes(P_initsc)$dimnames[[1]]
plot(P_initsc, type="n")
text(P_initsc,label)


######## voir ce qui se fait ici :
# https://www.tidymodels.org/learn/statistics/k-means/
# https://delladata.fr/kmeans/

# Algo kmeans sur 2 clusters
fit.P2<-kmeans(P_initsc,4, 25)
fit.P2
str(fit.P2)
fit.P2$withinss
fit.P2$tot.withinss
# les clusters ne sont pas assez éloignés en comparents la distance inter et intra cluster !
# néanmoins 4 clusters semblent ne pas être si mauvais

# within cluster sum of squares by cluster (WCSSC)
# squared average distance of all the points within a cluster to the cluster centroid
fit.P2$tot.withinss/fit.P2$totss 

#between cluster sum of squares (BSSC)
# squared average distance of all the points within a cluster to the cluster centroid
fit.P2$betweenss/fit.P2$totss 

# visualisation 2d - donc totalement imcomplète - des 4 clusters Kmeans

fviz_cluster(fit.P2, data = P_initsc)
plot(P_initsc, col = P2$cluster)
points(P2$centers, col = 1:5, pch = 8)
# afficher le kmeans ne fait pas de sens ! Un point est à 22 dimensions ! 
# LEs clusters ne seront pas visibles, et ne nous permettent pas de déterminer les features prédictifs
# Eval du nb de cluster optimal
######################################

#Tracé de la fonction du courde
# se base sur les distances intraclusters
{
  Tab<- NULL
  for(k in 1:10){
    Res<-kmeans(P_initsc,k)
    Tab[k]= Res$tot.withinss/Res$totss
  }
  plot(Tab, typ='l')
}

# Tracé de la valeur silhouette
# se base sur L'analyse des silhouettes peut être utilisée pour étudier la 
# distance de séparation entre les clusters résultants.Le tracé de la silhouette 
#affiche une mesure de la proximité de chaque point d'un cluster par rapport aux 
#points des clusters voisins et fournit ainsi un moyen d'évaluer visuellement des 
# paramètres tels que le nombre de clusters.Cette mesure a une plage de [-1,1].
fviz_nbclust(P_initsc, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method") 
#la silhouette max ne dépasse pas 0,4 pour k=2, les autres val de k sont en dessous de 0,25
# "il faut retourner sur les données"
# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(P_initsc, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method") 


# visu des clusters
km.out2=kmeans(P_initsc,centers=2,nstart =20)
km.out2
pairs(P_initsc, col=c(1:2)[km.out2$cluster]) 
km.out$cluster
# coord des centroïdes
km.out=kmeans(P_initsc,centers=4,nstart =20)
km.out$centers

# acp
# visu des val aberrantes !
fviz_cluster(km.out2, P_initsc, ellipse.type = "norm") 


# CAH
################################################################################
DP_sc <- dist(P_initsc)

# DIFFERENTES CONSTRUCTIONS DE DENDOGRAMMES

# méthode WARD (basée sur les variances)
reshcW <- hclust(DP_sc, method="ward.D2")  # pour qu'on ait des variances comparables
plot(reshcW) #pour avoir des classes homogènes en terme de variance, D2 joue selon
#que l'on aait des petites ou des grandes variances'

# méthode MIN (on dit "single")
reshcMIN <- hclust(DP_sc, method="single")
plot(reshcMIN)

# méthode MAX (on dit "complete")
reshcMAX <- hclust(DP_sc, method="conplete")
plot(reshcMAX)

# méthode MEAN (on dit "average")
reshcMEAN <- hclust(DP_sc, method="average")
plot(reshcMEAN)

# Multidimensional Scaling Analysis (doit toujours être faite à la fin des plot)
# permet de vérifier que les classes correspondent à ce qu'on voit sur les dendogrammes
CordTP3<-cmdscale(DP_sc, k=3)
label3<-attributes(CordTP3)$dimnames[[1]]
plot(CordTP3[,1],CordTP3[,3], type="n")
text(CordTP3[,1],CordTP3[,3],label3,cex=0.7)

# Conclusion de la méthode CAH :  mieux que les kmeans qui ne donne que des sacs,
# on sait qui est près de qui
# Dans les classes, on peut visualiser les sous-groupes
# hiérarchisation des groupes : de singletons à classes (la méthode divisi part
# de classes va jusqu'aux singletons)
# Le nb optimal de groupe  = 3, voire 5 (voir fct du coude)
# Quand le saut est petit, cela veut dire que l'on est en train de saucissoner à
# l'intérieur d'un groupe qui est compact'


# Algo PAM
################################################################################

Respam2 <- pam(DP_sc,2)
plot(Respam2)
Respam2
# Plus la silhouette est grande, mieux c'est (plus c'est proche de 1)
# il faut essayer avec un autre k

Respam3 <- pam(DP_sc,4)
plot(Respam3)
Respam2
# le résultat des objective function build et swap sont peu différents donc
# l'optimisation par swap est faible !
Respam2$silinfo

