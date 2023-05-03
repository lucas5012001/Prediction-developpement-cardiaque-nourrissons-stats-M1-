library(ranger)
source("Fonctions.R")
modele_EML_GA = function(train,test){
  loo_list=loo(train)
  train$preds_lin = 0
  for(i in 1:length(loo_list)){
    train$preds_lin[loo_list[[i]]] = predict(lm(PMA ~ 1 + sdRR + minRR + Skewness + Kurtosis + LFnu + HFnu + ApEn + MD_VG + C_VG + r_VG + C_HVG + Tr_HVG + GA,data=train[-loo_list[[i]],]), newdata = train[loo_list[[i]],])
  }
  modele_random_forest0 <- ranger(
    formula         = PMA ~ minRR+LF+HF+LFnu+HFnu+LF_HF+ApEn+C_HVG+GA+preds_lin, 
    data            = train, 
    num.trees       = 500,
    mtry            = 10,
    min.node.size   = 1,
    sample.fraction = 0.632,
    seed            = 123
  )
  test$preds_lin = predict(lm(PMA ~ 1 + sdRR + minRR + Skewness + Kurtosis + LFnu + HFnu + ApEn + MD_VG + C_VG + r_VG + C_HVG + Tr_HVG + GA,data=train), newdata = test)
  prediction <- predict(modele_random_forest0,test)$predictions
  return(cbind(test$PMA,prediction))
  return(result)
}
