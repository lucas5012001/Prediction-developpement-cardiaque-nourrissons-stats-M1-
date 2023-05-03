library(ranger)
source("Fonctions.R")
source("fonctions_modeles/modeles_HRV&GA_related/modele_lineaire_both_GA.R")
source("fonctions_modeles/modeles_HRV&GA_related/modele_gpboost_GA.R")
modele_mean_gpb_lin_GA = function(train,test){
  pred1 = modele_gpboost_GA(train,test)[,2]
  pred2 = modele_lineaire_both_GA(train,test)[,2]
  prediction = (pred1+pred2)/2
  return(cbind(test$PMA,prediction))
  return(result)
}
