library(xgboost)

modele_XGboost = function(train,test){
  tr = model.matrix(~.+0,data=train[,-which(colnames(train)%in%c("ID","Day","Term","GA"))])
  te = model.matrix(~.+0,data=test[,-which(colnames(test)%in%c("ID","Day","Term","GA"))])
  modele=xgboost(data=tr[,which(colnames(tr)!="PMA")],label = tr[,which(colnames(tr)=="PMA")],nrounds = 500,params=list(max_depth=5,min_child_weight = 6,gamma = 3,eta=0.01,objective = "reg:squarederror",colsample_bytree = 0.5 ,subsample = 0.5),verbose = 0)
  prediction <- predict(modele,te[,which(colnames(te)!="PMA")])
  result=cbind(te[,which(colnames(te)=="PMA")],prediction)
  return(result)
}