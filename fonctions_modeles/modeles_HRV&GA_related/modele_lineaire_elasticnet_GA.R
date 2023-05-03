modele_lineaire_elasticnet_GA = function(train,test){
  modele=lm(PMA ~ 1 + minRR + Skewness + AC + DC + LFnu + C_VG + r_VG + MD_HVG + 
              C_HVG + Tr_HVG + GA + Term,data=train)
  prediction = predict(modele, newdata = test)
  result=cbind(test$PMA,prediction)
  return(result)
}
