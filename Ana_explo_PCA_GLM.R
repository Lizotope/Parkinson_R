# on va essayer les mêmes algos en réduisant du mieux possible la dimension
# REDUC DIM
#########################################
##
## PCA : va permettre de passer de 22 à 5 dimension, ce qui permettra de faire 
## une regression logistique sans trop de souci car nb individu doit être > nb d'attributs * 10
#
# Mode d'emploi : http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/#packages-r
#

res.pca <- PCA(P_initsc, scale.unit = TRUE, graph = TRUE)
print(res.pca)

# desc des val propres portant le plus l'information

eig.val <- get_eigenvalue(res.pca)
eig.val
# a priori 5 dimensuions suffisent (87% de l'info, c'est bien)
# graphique des val propres
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
var
head(var$cos2, 10)
var$cos2

# graphique des var

corrplot(var$cos2, is.corr=FALSE)
# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)
#
#Contributions des variables aux axes principaux
head(var$contrib, 4)
var$contrib
# on voit que DFA, MDVP_Fo, PRDE ont des gros coeff contributeurs aux nouveaux composants

corrplot(var$contrib, is.corr=FALSE)   
# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10) # MDVP_Fo et MDVP_Flow surpassent toutes les autres
# Contributions des variables à PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10) # D2, DFA, spread2...
# Contributions des variables à PC4
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10) # DFA, Shimmet_APQ5
# Contributions des variables à PC5
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10) # RPDE, DFA, spread2
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


###############################################################################3
#autre essai de combinaison pca (sur matrice de corrélation!! contrairement à prcomp et PCA) / 
# a priori, le scaling n'est pas nécessaire car l'ACP/reg log s'intéresse  aux corrélations
#https://online.stat.psu.edu/stat857/node/130/
predictorX <- M_init[ , -17]
#responseY <- M_init[, 17] sert à rien car après on split et on en refait 2
pca <- princomp(predictorX, cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores

#training 
train <- pc.comp[1:150,1:5]
train_df <- as.data.frame(train)
responseYtrain <- M_init[1:150, 17]
model_logistic <- glm(responseYtrain~.,
                            data= train_df,
                            family=binomial("logit"))
glm.train.probs <- predict(model_logistic,type = "response")
glm.train.preds <- ifelse(glm.train.probs > 0.5,1,0)
# Accuracy
table(glm.train.preds,responseYtrain)
mean(glm.train.preds == responseYtrain)

#testing
test <- pc.comp[1:15,1:5]
test_df <- as.data.frame(test)
responseYtest <- M_init[1:15, 17]
glm.test.probs <- predict(model_logistic,newdata=test_df, type = "response")
glm.test.preds <- ifelse(glm.test.probs > 0.5,1,0)
# Accuracy
table(glm.test.preds,responseYtest)
mean(glm.test.preds == responseYtest)

################################################################################
set.seed(1234)

# on aléatoirise le split 80-20
splitSample <- sample(1:2, size=nrow(pc.comp), prob=c(0.8,0.2), replace = TRUE)

#training 
train <- pc.comp[splitSample==1,1:5]
train_df <- as.data.frame(train)
responseYtrain <- M_init[splitSample==1, 17]
model_logistic <- glm(responseYtrain~.,
                      data= train_df,
                      family=binomial("logit"))
glm.train.probs <- predict(model_logistic,type = "response")
glm.train.preds <- ifelse(glm.train.probs > 0.5,1,0)
# Accuracy
table(glm.train.preds,responseYtrain)
mean(glm.train.preds == responseYtrain)

#testing
test <- pc.comp[splitSample==2,1:5]
test_df <- as.data.frame(test)
responseYtest <- M_init[splitSample==2, 17]
glm.test.probs <- predict(model_logistic,newdata=test_df, type = "response")
glm.test.preds <- ifelse(glm.test.probs > 0.5,1,0)
# Accuracy
table(glm.test.preds,responseYtest)
mean(glm.test.preds == responseYtest)


# bootstrap sur ces splits aléatoires
nboot <- 30
bres <- matrix(NA,nrow=nboot, ncol=1, byrow =TRUE, dimnames=list(num_boot=seq(nboot), "accuracy"))

for (i in seq(nboot)) {
  
  # on aléatoirise le split 80-20
  splitSample <- sample(1:2, size=nrow(pc.comp), prob=c(0.8,0.2), replace = TRUE)
  
  #training 
  train <- pc.comp[splitSample==1,1:5]
  train_df <- as.data.frame(train)
  responseYtrain <- M_init[splitSample==1, 17]
  model_logistic <- glm(responseYtrain~.,
                        data= train_df,
                        family=binomial("logit"))
  glm.train.probs <- predict(model_logistic,type = "response")
  glm.train.preds <- ifelse(glm.train.probs > 0.5,1,0)
  # Accuracy
  #table(glm.train.preds,responseYtrain)
  #mean(glm.train.preds == responseYtrain)
  
  #testing
  test <- pc.comp[splitSample==2,1:5]
  test_df <- as.data.frame(test)
  responseYtest <- M_init[splitSample==2, 17]
  glm.test.probs <- predict(model_logistic,newdata=test_df, type = "response")
  glm.test.preds <- ifelse(glm.test.probs > 0.5,1,0)
  # Accuracy
  table(glm.test.preds,responseYtest)
  bres[i,] <- mean(as.numeric(glm.test.preds == responseYtest))
}

print(bres)
paste("Accuracy moyenne sur les ", nboot, " itérations : ", colMeans(bres))

# il suffira d'ajouter un novuel individu dans le dataset pour avoir sa prédiction
# STEP 3. 1 CLASSIFICATION  SUPERVISEE
#
# CART (cf Franck)
# XGBOOST (cf web)  
# SVM (cf méthode auteur)
################################################################################
#
################################################################################
# END
################################################################################
