modele_lineaire_both_GA = function(train,test){
  modele=lm(PMA ~ 1 + Rmssd + maxRR + minRR + Skewness + Kurtosis + 
              DC + LF + LFnu + SD1 + Alpha1 + MD_VG + r_VG + MD_HVG + 
              C_HVG + Tr_HVG + r_HVG + GA + Term
              ,data=train)
  prediction = predict(modele, newdata = test)
  result=cbind(test$PMA,prediction)
  return(result)
}