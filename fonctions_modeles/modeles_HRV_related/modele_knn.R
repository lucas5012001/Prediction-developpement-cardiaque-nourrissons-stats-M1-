library(caret)

modele_knn = function(train,test){
  tr = model.matrix(~.+0,data=train[,-which(colnames(train)%in%c("ID","Day","Term","GA"))])
  te = as.data.frame(model.matrix(~.+0,data=test[,-which(colnames(test)%in%c("ID","Day","Term","GA"))]))
  model=knnreg(PMA~., data=tr, k = 9)
  prediction <- predict(model, as.data.frame(te[,which(colnames(te)!="PMA")]))
  result=cbind(te[,which(colnames(te)=="PMA")],prediction)
  return(result)
}