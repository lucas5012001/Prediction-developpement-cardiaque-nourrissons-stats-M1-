modele_lineaire_elasticnet = function(train,test){
  modele=lm(PMA~1+MeanRR+sdRR+Rmssd+Skewness+Kurtosis+
              AC+DC+LF+LFnu+SD1+SD2+SampEn+ApEn+Alpha1+
              Alpha2+MD_VG+C_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+
              r_HVG,data=train)
  prediction = predict(modele, newdata = test)
  result=cbind(test$PMA,prediction)
  return(result)
}