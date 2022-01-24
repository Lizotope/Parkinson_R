
###############################################################################3
# Régression logistique binaire 

predictorX <- M_init[ , -17]
pca <- princomp(predictorX, cor=T) # principal components analysis using correlation  matrix
pc.comp <- pca$scores


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

# XGBOOST (cf web)  
# SVM (cf méthode auteur)
################################################################################
#
################################################################################
# END
################################################################################