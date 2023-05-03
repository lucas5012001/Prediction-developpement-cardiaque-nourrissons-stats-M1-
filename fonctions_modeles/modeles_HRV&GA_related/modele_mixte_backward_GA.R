library(lme4)

modele_mixte_backward_GA = function(train,test){
  modele=lmer(PMA~1+Rmssd+maxRR+minRR+Skewness+Kurtosis+AC+DC+LFnu+SD1+SD2+Alpha1+r_VG+MD_HVG+C_HVG+Tr_HVG+r_HVG+GA+Term+(1|ID),data=train)
  prediction = predict(modele, newdata = test, newparams = NULL,
                       re.form = NULL,
                       random.only=FALSE, terms = NULL,
                       type = c("link", "response"), allow.new.levels = TRUE,
                       na.action = na.pass)
  result=cbind(test$PMA,prediction)
  return(result)
}