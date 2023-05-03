library(ranger)

modele_random_forest_backward = function(train,test){
  modele_random_forest0 <- ranger(
    formula         = PMA ~ MeanRR+Rmssd+maxRR+minRR+Skewness+Kurtosis+AC+DC+HF+LFnu+HFnu+SampEn+ApEn+Alpha1+Alpha2+MD_VG+C_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+r_HVG+nbHighqual, 
    data            = train, 
    num.trees       = 500,
    mtry            = 21,
    min.node.size   = 1,
    sample.fraction = 0.632,
    seed            = 123
  )
  prediction <- predict(modele_random_forest0,test)$predictions
  result=cbind(test$PMA,prediction)
  return(result)
}