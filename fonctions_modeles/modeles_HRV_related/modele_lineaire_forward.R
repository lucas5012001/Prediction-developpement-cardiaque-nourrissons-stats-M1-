modele_lineaire_forward = function(train,test){
  modele=lm(PMA ~ 1 + Rmssd + Skewness + Kurtosis + 
              AC + DC + LFnu + SD1 + Alpha1 + MD_VG + 
              r_VG + MD_HVG + C_HVG + Tr_HVG + r_HVG,data=train)
  prediction = predict(modele, newdata = test)
  result=cbind(test$PMA,prediction)
  return(result)
}
