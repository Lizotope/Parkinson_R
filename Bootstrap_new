library(rpart)
# Initialisation de liste de subset (ls) et de tableau de performances (tp)
ls=NULL
tp=NULL


# extraction du test-set, on fixe un taux à extraire
#pourc = 0.5
# on a 147 malades et 48 sains
# 0.5 semble etre bien car 0.2 de tests donne 29.4 malades et 9.6 sains qui sont plutot faibles
# 0.5 donnerait 73.5 malades et 24 sains ça nous fait un échantillon test d'environ 100 sur une pop totale de 195 (vraiment 50-50)
# Nb_iter est le nombre d'iterations
bootstrap <- function(A, Nb_iter, pourc)
{
  S<-A[,-1]
  attach(S)
  S<- S[order(status),]
 
  # on cherche le nombre d'observations de S
  nb_obs <- nrow(A) # 195 obs dans notre cas
  
  for (i in 1:Nb_iter) {
    S$status <- factor(S$status)
    sub <- c(sample(1:max(which(S$status==0)), round(pourc*max(which(S$status==0)),digits=0)),sample((max(which(S$status==0))+1):nb_obs,round(pourc*length(which(S$status==1)),digits=0)))
    #print(sub)
    
    # On conserve l'échantillon (subset) tiré aléatoirement
    ls[[i]] <- sub
    
    fit <- rpart(S$status~ ., data=S, subset=sub)
    # plot(fit)
    # text(fit)
    
    # tab_contingence
    # On effectue le test et on conserve la table de contingence
    tab_cont<-table(predict(fit, S[-sub,], type="class"), S[-sub, "status"]) # fit=prévision des états en malade/sain des personnes qui ne sont pas dans sub (d'où "-sub")
    # le tableau de contingence est une matrice carrée d'ordre 2 car on a 2 modalités pour la classe, on peut se limiter à cette formule particulière
    # après si on veut on peut regarder le cas général
    # sum prend tous les elts de la matrice
    tp <- c(tp, (tab_cont[1,2]+tab_cont[2,1])/sum(tab_cont))
    
    # essayer la formule sum-tr(tab_cont)
    # mais avant, installer le package psych pour calculer la trace... si ça complique je laisse d'abord, le temps de produire un rapport complet de l'analyse supervisée
    # en tout cas ça marche très bien avec une mat d'ordre 2. 
    # On peut s'arrêter à ça
    
    
  }
  
  return(list(tab_perf=tp, list_sub=ls))

}
