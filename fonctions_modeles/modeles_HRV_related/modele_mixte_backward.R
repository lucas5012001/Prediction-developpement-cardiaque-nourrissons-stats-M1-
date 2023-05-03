library(lme4)

modele_mixte_backward = function(train,test){
  modele=lmer(PMA ~ Rmssd + minRR + Skewness + Kurtosis + AC + DC + LFnu + SD1 + MD_VG + r_VG + MD_HVG + C_HVG + Tr_HVG + r_HVG + (1 | ID),data=train)
  prediction = predict(modele, newdata = test, newparams = NULL,
                       re.form = NULL,
                       random.only=FALSE, terms = NULL,
                       type = c("link", "response"), allow.new.levels = TRUE,
                       na.action = na.pass)
  result=cbind(test$PMA,prediction)
  return(result)
}