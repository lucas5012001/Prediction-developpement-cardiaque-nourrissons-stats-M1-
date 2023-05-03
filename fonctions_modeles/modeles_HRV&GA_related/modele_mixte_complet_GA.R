library(lme4)

modele_mixte_complet_GA = function(train,test){
  modele=lmer(formula = PMA~1+MeanRR+sdRR+Rmssd+maxRR+minRR+Skewness+Kurtosis+
                              AC+DC+LF+HF+LFnu+HFnu+LF_HF+SD1+SD2+SampEn+ApEn+Alpha1+
                              Alpha2+MD_VG+C_VG+Tr_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+
                              r_HVG+nbHighqual+GA+Term+(1|ID), 
                            data = train)
  prediction = predict(modele, newdata = test, newparams = NULL,
                       re.form = NULL,
                       random.only=FALSE, terms = NULL,
                       type = c("link", "response"), allow.new.levels = TRUE,
                       na.action = na.pass)
  result=cbind(test$PMA,prediction)
  return(result)
}