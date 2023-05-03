rm(list=ls())
set.seed(123)
library(glmnet)
library(lme4)
source("fonctions.R")

######################
nombre_de_alpha = 100
nombre_de_lambda = 350
######################

hyper_grid <- expand.grid(
  lambdas = seq(0,nombre_de_lambda,1),
  alphas   = seq(0,1,1/nombre_de_alpha),
  MSE1 =0,
  MSE2   = 0,
  MSE3   = 0,
  MSE4   = 0,
  MSE5   = 0,
  MSE = 0
)

bloc1 = read.csv("donnees/bloc1.csv")
bloc2 = read.csv("donnees/bloc2.csv")
bloc3 = read.csv("donnees/bloc3.csv")
bloc4 = read.csv("donnees/bloc4.csv")
bloc5 = read.csv("donnees/bloc5.csv")
data=read.csv("donnees/data.csv")
bloc1=conversion_type_variables_modele(bloc1)
bloc2=conversion_type_variables_modele(bloc2)
bloc3=conversion_type_variables_modele(bloc3)
bloc4=conversion_type_variables_modele(bloc4)
bloc5=conversion_type_variables_modele(bloc5)
data=conversion_type_variables_modele(data)
blocs = list(bloc1,bloc2,bloc3,bloc4,bloc5)
n1=length(bloc1[,1])
n2=length(bloc2[,1])
n3=length(bloc3[,1])
n4=length(bloc4[,1])
n5=length(bloc5[,1])
n=n1+n2+n3+n4+n5

X_data = model.matrix(PMA~0+MeanRR+sdRR+Rmssd+maxRR+minRR+Skewness+Kurtosis+
                        AC+DC+LF+HF+LFnu+HFnu+LF_HF+SD1+SD2+SampEn+ApEn+Alpha1+
                        Alpha2+MD_VG+C_VG+Tr_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+
                        r_HVG+nbHighqual+GA+Term, data = data)
vars = colnames(X_data)[-c(31,32,33,34,35)]
vars=c(vars,"Term")
for(i in 1:length(hyper_grid[,1])){
  coeffs=coef(glmnet(X_data,data$PMA,family = "gaussian",alpha= hyper_grid$alphas[i], lambda = hyper_grid$lambdas[i]))@i
  if(all(c(31,32,33,34,35)%in%coeffs)){
    coeffs=coeffs[-which(coeffs%in%c(31,32,33,34,35))]
    coeffs=c(coeffs,31)
  } else{
    coeffs = coeffs[which(!coeffs%in%c(31,32,33,34,35))]
  }
  for(k in 1:length(blocs)){
    indices=seq(1:5)[-k]
    train = rbind(blocs[[indices[[1]]]],blocs[[indices[[2]]]],blocs[[indices[[3]]]],blocs[[indices[[4]]]])
    test = blocs[[k]]
    fmla <- as.formula(paste("PMA ~ 1+", paste(vars[coeffs], collapse= "+")))
    model=lm(fmla,data = train)
    predicted = predict(model,test)
    MSE=mean((test$PMA - predicted)**2)
    hyper_grid[i,2+k] = MSE
  }
}

for(i in 1:length(hyper_grid[,1])){
  hyper_grid$MSE[i]=(n1*hyper_grid$MSE1[i]+n2*hyper_grid$MSE2[i]+n3*hyper_grid$MSE3[i]+n4*hyper_grid$MSE4[i]+n5*hyper_grid$MSE5[i])/n
}

indice_meilleur_mse = which.min(hyper_grid$MSE)
alpha = hyper_grid$alphas[indice_meilleur_mse]
lambda = hyper_grid$lambdas[indice_meilleur_mse]
coef(glmnet(X_data,data$PMA,family = "gaussian",alpha= alpha, lambda = lambda))

#> alpha
#[1] 0.35
#> lambda
#[1] 2
#MSE = 2.891510

#(Intercept) 32.7404712435
#MeanRR       .           
#sdRR         .           
#Rmssd        .           
#maxRR        .           
#minRR       -0.0761953169
#Skewness    -0.0343597524
#Kurtosis     .           
#AC           0.2177628838
#DC           0.1749179869
#LF           .           
#HF           .           
#LFnu         0.0009929874
#HFnu         .           
#LF_HF        .           
#SD1          .           
#SD2          .           
#SampEn       .           
#ApEn         .           
#Alpha1       .           
#Alpha2       .           
#MD_VG        .           
#C_VG         0.2855465192
#Tr_VG        .           
#r_VG         0.3285755232
#MD_HVG       0.5990870164
#C_HVG        0.4087589778
#Tr_HVG       0.0306611360
#r_HVG        .           
#nbHighqual   .           
#GA           0.5127940430
#TermEP      -0.1525006643
#TermET       .           
#TermFT       .           
#TermLP       .           
#TermVP       .           
