library(ranger)
source("Fonctions.R")
source("fonctions_modeles/modeles_HRV_related/modele_mean_rf_lin.R")
source("fonctions_modeles/modeles_HRV_related/modele_lin+forest.R")
modele_combine = function(train,test){
  preds1=modele_mean_rf_lin(train,test)[,2]
  preds2=modele_lin_forest(train,test)[,2]
  prediction <- (preds1+preds2)/2
  return(cbind(test$PMA,prediction))
  return(result)
}
