library(ranger)
source("Fonctions.R")
source("fonctions_modeles/modeles_HRV_related/modele_lineaire_both.R")
modele_lin_forest = function(train,test){
  loo_list=loo(train)
  train$preds_lin = 0
  for(i in 1:length(loo_list)){
    train$preds_lin[loo_list[[i]]] = modele_lineaire_both(train[-loo_list[[i]],],train[loo_list[[i]],])[,2]
  }
  modele_random_forest0 <- ranger(
    formula         = PMA ~ MeanRR+Rmssd+maxRR+minRR+Skewness+Kurtosis+AC+DC+HF+LFnu+HFnu+SampEn+ApEn+Alpha1+Alpha2+MD_VG+C_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+r_HVG+nbHighqual+preds_lin, 
    data            = train, 
    num.trees       = 500,
    mtry            = 21,
    min.node.size   = 1,
    sample.fraction = 0.632,
    seed            = 123
  )
  test$preds_lin = modele_lineaire_both(train,test)[,2]
  prediction <- predict(modele_random_forest0,test)$predictions
  return(cbind(test$PMA,prediction))
  return(result)
}
