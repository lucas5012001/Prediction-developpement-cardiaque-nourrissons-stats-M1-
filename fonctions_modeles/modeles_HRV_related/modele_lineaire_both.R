modele_lineaire_both = function(train,test){
  modele=lm(PMA ~ Rmssd + maxRR + minRR + Skewness + Kurtosis + 
              AC + DC + HF + LFnu + SD1 + SD2 + SampEn + Alpha1 + MD_VG + 
              r_VG + MD_HVG + C_HVG + Tr_HVG + r_HVG,data=train)
  prediction = predict(modele, newdata = test)
  result=cbind(test$PMA,prediction)
  return(result)
}
