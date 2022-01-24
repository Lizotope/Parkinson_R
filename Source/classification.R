#Fichier global prenant l’essentiel des commandes des parties classification supervisée et #non supervisée

# Chargement du dataset
A=read.csv("parkinsons.data")
#on enlève la colonne des noms et la variable classe
A2=A[,-1]
A2=A2[,-17]

# On normalise notre dataset
A2N<-scale(A2)

#KMEANS

#utilisation du package fpc pour la fonction kmeansruns()
library(fpc)
#évaluation des solutions
sol.kmeans <- kmeansruns(A2,krange=2:10,criterion="asw")
#graphique
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")

#utilisation du package factoextra
factoextra::fviz_nbclust(A2N, FUNcluster =kmeans, method = "silhouette")

#Application du kmeans avec 2 groupes
reskmeansN<-kmeans(A2N,2)

# Pour le graphique on utilise le package factoextra
library(factoextra)
fviz_cluster(reskmeansN, A2N)


#PAM.
#library(cluster)
# estimation du nombre de clusters 
fviz_nbclust(A2N, pam, method = "silhouette")
#pam avec 2 groupes
respamN<-pam(A2N,2)
# pour le graphique
plot(respamN)

#CAH

# On prend la matrice de distance du dataset
DparksN<-dist(A2N)
reshcN<-hclust(DparksN, method = "ward.D2")

# Calcul du dendogramme
plot(reshcN)

#library(devtools)
#execution du script de la fonction best.cutree pour couper le dendogramme
source("clustering.R")
best.cutree(reshcN, min = 2)

#Graphique des pertes relatives d’inertie avec graph=TRUE. 
best.cutree(arbre, min = 2, graph = TRUE, xlab = "Nombre de classes", ylab = "Inertie relative")

# Groupes du dendrogramme à 2 et à 3 classes

plot(reshcN, labels = FALSE, main = "Partition en 2 ou 3 classes", xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(reshcN, 2, border = "green3")
rect.hclust(reshcN, 3, border = "red3")

##découpage en k=2 groupes
groupes.cah <- cutree(reshcN,k=2)
groupes.cah
source("stat_comp.R")
print(sapply(A2,stat.comp,y=groupes.cah))

#ACP pour une analyse tenant compte des liaisons entre variables
#ACP normée
acp <- princomp(A2,cor=T,scores=T)

#screeplot - 2 axes retenus
plot(1:9,acp$sdev^2,type="b",xlab="Nb. de facteurs",ylab="Val. Propres")

#biplot
biplot(acp,cex=0.70)


#Classification supervisée

# exécution du fichier "bootstrap.R"
source("bootstrap.R")
# exécution de la fonction "bootstrap" avec le dataset de départ, 100 itérations et 50% de données pour l’apprentissage
x<-bootstrap(A, 100, 0.5)
x
#tableau des erreurs obtenues au cours des 100 itérations
x$tab_perf
# résumé des caractéristiques de la distribution des erreurs 
summary(x$tab_perf)
boxplot(x$tab_perf)
# variance de la distribution 
var(x$tab_perf)
# échantillon qui minimise l’erreur
sub<-x$list_sub[67][[1]]
sub
# construction (calcul) de l’arbre d’apprentissage
fit <- rpart(S$status~ ., data=S, subset=sub)
# graphique de l’arbre
plot(fit)
text(fit)


