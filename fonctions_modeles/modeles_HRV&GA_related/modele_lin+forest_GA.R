library(ranger)
source("Fonctions.R")
source("fonctions_modeles/modeles_HRV&GA_related/modele_lineaire_both_GA.R")
modele_lin_forest_GA = function(train,test){
  loo_list=loo(train)
  train$preds_lin = 0
  for(i in 1:length(loo_list)){
    train$preds_lin[loo_list[[i]]] = modele_lineaire_both_GA(train[-loo_list[[i]],],train[loo_list[[i]],])[,2]
  }
  modele_random_forest0 <- ranger(
    formula         = PMA ~ MeanRR+maxRR+minRR+AC+LFnu+Alpha1+C_VG+r_VG+MD_HVG+C_HVG+r_HVG+GA+preds_lin, 
    data            = train, 
    num.trees       = 500,
    mtry            = 11,
    min.node.size   = 3,
    sample.fraction = 0.8,
    seed            = 123
  )
  test$preds_lin = modele_lineaire_both_GA(train,test)[,2]
  prediction <- predict(modele_random_forest0,test)$predictions
  return(cbind(test$PMA,prediction))
  return(result)
}
