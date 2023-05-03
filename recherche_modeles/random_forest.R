rm(list=ls())
set.seed(123)
library(randomForest)
library(ranger) #implémentation en c++ (6 fois plus rapide) de randomforest 
library(dplyr)
library(ggplot2)
library(broom)
source("fonctions.R")
data=read.csv("donnees/data.csv")
data=conversion_type_variables_modele(data)
###################
#on définit les variables explicatives
features = setdiff(colnames(data),c("PMA","Day","GA","Term","ID"))
#################################################
#grille de recherche des hyperparametres
hyper_grid <- expand.grid(
  mtry       = seq(10, length(features), by = 1),
  node_size  = seq(1, 5, by = 1),
  sampe_size = c(.55, .632, .70, .80),
  MSE1   = 0,
  MSE2   = 0,
  MSE3   = 0,
  MSE4   = 0,
  MSE5   = 0,
  MSE = 0
)
#################################################

bloc1 = read.csv("donnees/bloc1.csv")
bloc2 = read.csv("donnees/bloc2.csv")
bloc3 = read.csv("donnees/bloc3.csv")
bloc4 = read.csv("donnees/bloc4.csv")
bloc5 = read.csv("donnees/bloc5.csv")
bloc1=conversion_type_variables_modele(bloc1)
bloc2=conversion_type_variables_modele(bloc2)
bloc3=conversion_type_variables_modele(bloc3)
bloc4=conversion_type_variables_modele(bloc4)
bloc5=conversion_type_variables_modele(bloc5)
blocs = list(bloc1,bloc2,bloc3,bloc4,bloc5)

n1=length(bloc1[,1])
n2=length(bloc2[,1])
n3=length(bloc3[,1])
n4=length(bloc4[,1])
n5=length(bloc5[,1])
n=n1+n2+n3+n4+n5

# default RF model
m1 <- randomForest(
  formula = PMA ~ MeanRR+sdRR+Rmssd+maxRR+minRR+Skewness+Kurtosis+AC+DC+LF+HF+LFnu+HFnu+LF_HF +SD1+SD2 +SampEn+ApEn+Alpha1+Alpha2+MD_VG+C_VG+Tr_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+r_HVG+nbHighqual,
  data    = data
)
plot(m1)
m1

#test des hyperparametres par validation k blocs
for(k in 1:length(blocs)){
  indices=seq(1:5)[-k]
  train = rbind(blocs[[indices[[1]]]],blocs[[indices[[2]]]],blocs[[indices[[3]]]],blocs[[indices[[4]]]])
  test = blocs[[k]]
  for(i in 1:length(hyper_grid[,1])){
    print(i)
    model <- ranger(
      formula         = PMA ~ MeanRR+sdRR+Rmssd+maxRR+minRR+Skewness+Kurtosis+AC+DC+LF+HF+LFnu+HFnu+LF_HF +SD1+SD2 +SampEn+ApEn+Alpha1+Alpha2+MD_VG+C_VG+Tr_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+r_HVG+nbHighqual, 
      data            = train, 
      num.trees       = 500,
      mtry            = hyper_grid$mtry[i],
      min.node.size   = hyper_grid$node_size[i],
      sample.fraction = hyper_grid$sampe_size[i],
      seed            = 123
    )
    pred <- predict(model, test)
    hyper_grid[i,3+k]=mean((test$PMA - pred$predictions)**2)
  }
}

for(i in 1:length(hyper_grid[,1])){
  hyper_grid$MSE[i]=(n1*hyper_grid$MSE1[i]+n2*hyper_grid$MSE2[i]+n3*hyper_grid$MSE3[i]+n4*hyper_grid$MSE4[i]+n5*hyper_grid$MSE5[i])/n
}

#Affichage des résultats
plot(hyper_grid$mtry,hyper_grid$MSE)
plot(hyper_grid$node_size,hyper_grid$MSE)
plot(hyper_grid$sampe_size,hyper_grid$MSE)
#Paramètres minimisant le MSE
params_min =hyper_grid[which.min(hyper_grid$MSE),c(1,2,3,9)]

#meilleur modele
best_model = ranger(
  formula         = PMA ~ MeanRR+sdRR+Rmssd+maxRR+minRR+Skewness+Kurtosis+AC+DC+LF+HF+LFnu+HFnu+LF_HF +SD1+SD2 +SampEn+ApEn+Alpha1+Alpha2+MD_VG+C_VG+Tr_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+r_HVG+nbHighqual, 
  data            = data, 
  num.trees       = 500,
  mtry            = params_min$mtry,
  min.node.size   = params_min$node_size,
  sample.fraction = params_min$sampe_size,
  importance      = 'impurity',
  seed            = 123
)

#importance de variables
best_model$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(29) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("important variables")

#meilleurs paramètres
params_min
plot(hyper_grid$mtry,hyper_grid$MSE,xlab = "mtry", ylab = "MSE", main = "optimisation random_forest")
#resultat :
#mtry node_size sampe_size      MSE
#27   1         0.632           3.7749