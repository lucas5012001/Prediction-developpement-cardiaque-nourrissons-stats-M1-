rm(list=ls())
############
nombre_de_lambdas=1500
chiffres_significatifs = 2
############
set.seed(123)
library(glmnet)
library(lme4)
library(nlme)
library(glmmLasso)
source("fonctions.R")
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
n1=length(bloc1[,1])
n2=length(bloc2[,1])
n3=length(bloc3[,1])
n4=length(bloc4[,1])
n5=length(bloc5[,1])
n=n1+n2+n3+n4+n5
colnames(bloc1)[-c(30,31,33,34)]=paste0("x", 1:30)
colnames(bloc1)[31]="x31"
colnames(bloc2)[-c(30,31,33,34)]=paste0("x", 1:30)
colnames(bloc2)[31]="x31"
colnames(bloc3)[-c(30,31,33,34)]=paste0("x", 1:30)
colnames(bloc3)[31]="x31"
colnames(bloc4)[-c(30,31,33,34)]=paste0("x", 1:30)
colnames(bloc4)[31]="x31"
colnames(bloc5)[-c(30,31,33,34)]=paste0("x", 1:30)
colnames(bloc5)[31]="x31"
blocs = list(bloc1,bloc2,bloc3,bloc4,bloc5)


results = list()
for(i in 0:nombre_de_lambdas){
  model=glmmLasso(PMA ~MeanRR+sdRR+Rmssd+maxRR+minRR+Skewness+Kurtosis+
                    AC+DC+LF+HF+LFnu+HFnu+LF_HF+SD1+SD2+SampEn+ApEn+Alpha1+
                    Alpha2+MD_VG+C_VG+Tr_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+
                    r_HVG+nbHighqual+GA+as.factor(Term), rnd = list(ID=~1),lambda = i,data = data)
  coeffs=model$coefficients[-1]
  selection = which(round(abs(as.numeric(coeffs)),chiffres_significatifs)>0)
  if(all(c(31,32,33,34)%in%selection)){
    selection=selection[which(!selection%in%c(31,32,33,34))]
    selection=c(selection,31)
  } else{
    selection = selection[which(!selection%in%c(31,32,33,34))]
  }
  if(length(selection)==0){
    fmla=as.formula(paste("PMA~1+(1|ID)"))
  } else{
    xnam=paste0("x", selection)
    fmla <- as.formula(paste("PMA ~ 1+", paste(xnam, collapse= "+"),"+(1|ID)"))
  }
  for(k in 1:length(blocs)){
    indices=seq(1:5)[-k]
    train = rbind(blocs[[indices[[1]]]],blocs[[indices[[2]]]],blocs[[indices[[3]]]],blocs[[indices[[4]]]])
    model_lme4=lmer(fmla,data = train)
    #test
    test=blocs[[k]]
    predicted = predict(model_lme4, newdata = test, newparams = NULL,
                        re.form = NULL,
                        random.only=FALSE, terms = NULL,
                        type = c("link", "response"), allow.new.levels = TRUE,
                        na.action = na.pass)
    MSE=mean((test$PMA - predicted)**2)
    results[[i*5+k]] = MSE
  }
}
lambdas = seq(0:nombre_de_lambdas)-1
MSE_moyens = c()
for(i in 0:nombre_de_lambdas){
  MSE = (n1*results[[i*5+1]]+n2*results[[i*5+2]]+n3*results[[i*5+3]]+n4*results[[i*5+4]]+n5*results[[i*5+5]])/n
  MSE_moyens = c(MSE_moyens,MSE)
}
resultats = as.data.frame(cbind(lambdas,MSE_moyens))
resultats <- resultats[order(resultats$MSE_moyens), ]
coeffs=glmmLasso(PMA ~MeanRR+sdRR+Rmssd+maxRR+minRR+Skewness+Kurtosis+
                  AC+DC+LF+HF+LFnu+HFnu+LF_HF+SD1+SD2+SampEn+ApEn+Alpha1+
                  Alpha2+MD_VG+C_VG+Tr_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+
                  r_HVG+nbHighqual+GA+as.factor(Term), rnd = list(ID=~1),lambda = resultats$lambdas[1],data = data)$coefficients
print(names(coeffs)[which(abs(round(coeffs,chiffres_significatifs))>0)])
#lamda_opti = 585
#MSE = 3.340276
#effets fixes selectionnés
#[1] "(Intercept)"       "sdRR"              "Rmssd"             "AC"               
#[5] "DC"                "LF"                "LFnu"              "SD1"              
#[9] "SD2"               "SampEn"            "ApEn"              "C_VG"             
#[13] "r_VG"              "MD_HVG"            "C_HVG"             "Tr_HVG"           
#[17] "GA"                "Term"