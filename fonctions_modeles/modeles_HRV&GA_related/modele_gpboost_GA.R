library(gpboost)

modele_gpboost_GA = function(train,test){
  features_i = which(colnames(train)%in%setdiff(colnames(train),c("PMA","Day","ID","Term")))
  PMA_i = which(colnames(train)=="PMA")
  groups_i = which(colnames(train) %in% c("ID","Term"))
  groups_train = train[,groups_i]
  X_train=as.matrix(train[,features_i])
  Y_train=as.matrix(train[,PMA_i])
  gp_model <- GPModel(group_data = groups_train, likelihood = "gaussian")
  bst <- gpboost(data = X_train, label = Y_train, gp_model = gp_model,objective = "regression_l2",verbose = 0,params = list(num_iterations=122,min_data_in_leaf=14,learning_rate = 0.05))
  X_test=as.matrix(test[,features_i])
  groups_test = test[,groups_i]
  pred <- predict(bst, data = X_test, group_data_pred = groups_test)
  prediction <- pred$response_mean
  result=cbind(test$PMA,prediction)
  return(result)
}
