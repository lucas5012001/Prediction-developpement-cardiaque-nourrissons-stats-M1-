library(ranger)
source("Fonctions.R")
source("fonctions_modeles/modeles_HRV&GA_related/modele_mean_gpb_lin_GA.R")
source("fonctions_modeles/modeles_HRV&GA_related/modele_lin+forest_GA.R")
source("fonctions_modeles/modeles_HRV&GA_related/modele_EML_GA.R")
modele_combine_GA = function(train,test){
  preds1=modele_mean_gpb_lin_GA(train,test)[,2]
  preds2=modele_lin_forest_GA(train,test)[,2]
  preds3=modele_EML_GA(train,test)[,2]
  prediction <- (preds1+preds2+preds3)/3
  return(cbind(test$PMA,prediction))
  return(result)
}
