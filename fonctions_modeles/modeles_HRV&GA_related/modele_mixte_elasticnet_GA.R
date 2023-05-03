library(lme4)

modele_mixte_elasticnet_GA = function(train,test){
  modele=lmer(PMA~1+minRR+Skewness+AC+DC+LFnu+C_VG+r_VG+MD_HVG+C_HVG+Tr_HVG+GA+Term+(1|ID),data=train)
  prediction = predict(modele, newdata = test, newparams = NULL,
                       re.form = NULL,
                       random.only=FALSE, terms = NULL,
                       type = c("link", "response"), allow.new.levels = TRUE,
                       na.action = na.pass)
  result=cbind(test$PMA,prediction)
  return(result)
}