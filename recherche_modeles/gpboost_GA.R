rm(list=ls())
set.seed(123)
library(gpboost)
source("fonctions.R")
data=read.csv("donnees/data.csv")
data=conversion_type_variables_modele(data)
###################
#on définit les variables
features_i = which(colnames(data)%in%setdiff(colnames(data),c("PMA","Day","ID","Term")))
PMA_i = which(colnames(data)=="PMA")
groups_i = which(colnames(data) %in% c("ID","Term"))
#################################################
#grille de recherche des hyperparametres
hyper_grid <- expand.grid(
  num_iter = seq(110,135,1),
  min_data_in_leaf = seq(13,16,1),
  MSE1   = 0,
  MSE2   = 0,
  MSE3   = 0,
  MSE4   = 0,
  MSE5   = 0,
  MSE = 0
)
#################################################

bloc1 = read.csv("donnees/bloc1.csv")
bloc2 = read.csv("donnees/bloc2.csv")
bloc3 = read.csv("donnees/bloc3.csv")
bloc4 = read.csv("donnees/bloc4.csv")
bloc5 = read.csv("donnees/bloc5.csv")
bloc1=conversion_type_variables_modele(bloc1)
bloc2=conversion_type_variables_modele(bloc2)
bloc3=conversion_type_variables_modele(bloc3)
bloc4=conversion_type_variables_modele(bloc4)
bloc5=conversion_type_variables_modele(bloc5)
blocs = list(bloc1,bloc2,bloc3,bloc4,bloc5)

n1=length(bloc1[,1])
n2=length(bloc2[,1])
n3=length(bloc3[,1])
n4=length(bloc4[,1])
n5=length(bloc5[,1])
n=n1+n2+n3+n4+n5

#test des hyperparametres par validation k blocs
for(k in 1:length(blocs)){
  indices=seq(1:5)[-k]
  train = rbind(blocs[[indices[[1]]]],blocs[[indices[[2]]]],blocs[[indices[[3]]]],blocs[[indices[[4]]]])
  test = blocs[[k]]
  for(i in 1:length(hyper_grid[,1])){
    print(i)
    groups_train = train[,groups_i]
    X_train=as.matrix(train[,features_i])
    Y_train=as.matrix(train[,PMA_i])
    gp_model <- GPModel(group_data = groups_train, likelihood = "gaussian")
    bst <- gpboost(data = X_train, label = Y_train, gp_model = gp_model,objective = "regression_l2",verbose = 0,params = list(num_iterations=hyper_grid$num_iter[i],min_data_in_leaf=hyper_grid$min_data_in_leaf[i],learning_rate = 0.05))
    X_test=as.matrix(test[,features_i])
    groups_test = test[,groups_i]
    
    pred <- predict(bst, data = X_test, group_data_pred = groups_test)
    prediction <- pred$response_mean
    hyper_grid[i,2+k]=mean((test$PMA - prediction)**2)
  }
}

for(i in 1:length(hyper_grid[,1])){
  hyper_grid$MSE[i]=(n1*hyper_grid$MSE1[i]+n2*hyper_grid$MSE2[i]+n3*hyper_grid$MSE3[i]+n4*hyper_grid$MSE4[i]+n5*hyper_grid$MSE5[i])/n
}
plot(hyper_grid$num_iter,hyper_grid$MSE,xlab = "nombre d'itérations",ylab="MSE",main = "optimisation du nombre d'itérations pour gpboost_GA")
plot(hyper_grid$num_iter,hyper_grid$min_data_in_leaf,xlab = "min data/feuille",ylab="MSE",main = "optimisation du nombre de données par feuille pour gpboost_GA")

#num_iter = 122
#min_data_in_leaf = 14