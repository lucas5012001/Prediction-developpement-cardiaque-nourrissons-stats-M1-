library(e1071)

modele_svm = function(train,test){
  modele <- svm(
    formula         = PMA ~ MeanRR+sdRR+Rmssd+maxRR+minRR+Skewness+Kurtosis+AC+DC+LF+HF+LFnu+HFnu+LF_HF +SD1+SD2 +SampEn+ApEn+Alpha1+Alpha2+MD_VG+C_VG+Tr_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+r_HVG+nbHighqual, 
    data            = train, 
    cost = 1.65
  )
  prediction <- predict(modele,test)
  result=cbind(test$PMA,prediction)
  return(result)
}