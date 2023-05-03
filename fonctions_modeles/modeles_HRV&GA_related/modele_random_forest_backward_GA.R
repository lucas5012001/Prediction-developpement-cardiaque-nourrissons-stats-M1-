library(ranger)

modele_random_forest_backward_GA = function(train,test){
  modele_random_forest0 <- ranger(
    formula         = PMA ~ MeanRR+maxRR+minRR+AC+LFnu+Alpha1+C_VG+r_VG+MD_HVG+C_HVG+r_HVG+GA, 
    data            = train, 
    num.trees       = 500,
    mtry            = 11,
    min.node.size   = 3,
    sample.fraction = 0.8,
    seed            = 123
  )
  prediction <- predict(modele_random_forest0,test)$predictions
  result=cbind(test$PMA,prediction)
  return(result)
}