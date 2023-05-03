library(ranger)
source("Fonctions.R")
source("fonctions_modeles/modeles_HRV_related/modele_lineaire_both.R")
source("fonctions_modeles/modeles_HRV_related/modele_random_forest_backward.R")
modele_mean_rf_lin = function(train,test){
  pred1 = modele_random_forest_backward(train,test)[,2]
  pred2 = modele_lineaire_both(train,test)[,2]
  prediction = (pred1+pred2)/2
  return(cbind(test$PMA,prediction))
  return(result)
}