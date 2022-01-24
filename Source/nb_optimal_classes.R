#(1)évaluer la proportion d'inertie expliquée
inertie.expl <- rep(0,times=10)
for (k in 2:10){
  #clus <- kmeans(A2N,centers=k,nstart=5)
  clus <- kmeans(A2N,centers=k)
  inertie.expl[k] <- clus$betweenss/clus$totss
}