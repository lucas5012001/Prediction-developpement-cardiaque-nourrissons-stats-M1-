rm(list = ls())
library(DT)
source("fonctions.R")
source("fonctions_modeles/modeles_HRV&GA_related/modele_lineaire_backward_GA.R")
bloc1 = read.csv("donnees/bloc1.csv")
bloc2 = read.csv("donnees/bloc2.csv")
bloc3 = read.csv("donnees/bloc3.csv")
bloc4 = read.csv("donnees/bloc4.csv")
bloc5 = read.csv("donnees/bloc5.csv")
bloc1=conversion_type_variables_modele(bloc1)
bloc2=conversion_type_variables_modele(bloc2)
bloc3=conversion_type_variables_modele(bloc3)
bloc4=conversion_type_variables_modele(bloc4)
bloc5=conversion_type_variables_modele(bloc5)
#################################################
eval1=modele_lineaire_backward_GA(train=rbind(bloc5,bloc2,bloc3,bloc4),test=bloc1)
eval2=modele_lineaire_backward_GA(train=rbind(bloc1,bloc5,bloc3,bloc4),test=bloc2)
eval3=modele_lineaire_backward_GA(train=rbind(bloc1,bloc2,bloc5,bloc4),test=bloc3)
eval4=modele_lineaire_backward_GA(train=rbind(bloc1,bloc2,bloc3,bloc5),test=bloc4)
eval5=modele_lineaire_backward_GA(train=rbind(bloc1,bloc2,bloc3,bloc4),test=bloc5)
#################################################
n1=length(bloc1[,1])
n2=length(bloc2[,1])
n3=length(bloc3[,1])
n4=length(bloc4[,1])
n5=length(bloc5[,1])
n=n1+n2+n3+n4+n5

e1=eval1[,1]-eval1[,2]
e2=eval2[,1]-eval2[,2]
e3=eval3[,1]-eval3[,2]
e4=eval4[,1]-eval4[,2]
e5=eval5[,1]-eval5[,2]

mean1=mean(e1)
mean2=mean(e2)
mean3=mean(e3)
mean4=mean(e4)
mean5=mean(e5)

mean_abs1=mean(abs(e1))
mean_abs2=mean(abs(e2))
mean_abs3=mean(abs(e3))
mean_abs4=mean(abs(e4))
mean_abs5=mean(abs(e5))

MSE1=mean(e1**2)
MSE2=mean(e2**2)
MSE3=mean(e3**2)
MSE4=mean(e4**2)
MSE5=mean(e5**2)

moyenne_residus = (mean1*n1+mean2*n2+mean3*n3+mean4*n4+mean5*n5)/n
moyenne_residus_abs = (mean_abs1*n1+mean_abs2*n2+mean_abs3*n3+mean_abs4*n4+mean_abs5*n5)/n
MSE=(MSE1*n1+MSE2*n2+MSE3*n3+MSE4*n4+MSE5*n5)/n

bloc1$error = e1
bloc2$error = e2
bloc3$error = e3
bloc4$error = e4
bloc5$error = e5

data = rbind(bloc1,bloc2,bloc3,bloc4,bloc5)
results=data[,c("Term","error")]
results$abs_error = abs(results$error)
results$MSE=results$error**2
results$compteur = 1
results=aggregate(results[,-1],list(results$Term),sum)
results$error=results$error/results$compteur
results$abs_error=results$abs_error/results$compteur
results$MSE=results$MSE/results$compteur
results$Group.1 = as.character(results$Group.1)
results = rbind(results,c("general",moyenne_residus,moyenne_residus_abs,MSE,n))
colnames(results)[5]="nombre observations"
colnames(results)[1]="groupe"
results = as.data.frame(results)
R_carre=1-MSE/mean((data$PMA-mean(data$PMA))**2)
R_carre_EP=1-as.numeric(results$MSE[1])/mean((data$PMA[which(data$Term=="EP")]-mean(data$PMA[which(data$Term=="EP")]))**2)
R_carre_ET=1-as.numeric(results$MSE[2])/mean((data$PMA[which(data$Term=="ET")]-mean(data$PMA[which(data$Term=="ET")]))**2)
R_carre_FT=1-as.numeric(results$MSE[3])/mean((data$PMA[which(data$Term=="FT")]-mean(data$PMA[which(data$Term=="FT")]))**2)
R_carre_LP=1-as.numeric(results$MSE[4])/mean((data$PMA[which(data$Term=="LP")]-mean(data$PMA[which(data$Term=="LP")]))**2)
R_carre_VP=1-as.numeric(results$MSE[5])/mean((data$PMA[which(data$Term=="VP")]-mean(data$PMA[which(data$Term=="VP")]))**2)
vecteur_R = as.data.frame(rbind(R_carre_EP,R_carre_ET,R_carre_FT,R_carre_LP,R_carre_VP,R_carre))
vecteur_RSE = 1-vecteur_R$V1
results$RSE = vecteur_RSE
results$`1-RSE` = vecteur_R$V1
results$MeanErrorPercentage=0
results$MeanErrorPercentage[1]=100*mean(abs(data$error[which(data$Term=="EP")]/data$PMA[which(data$Term=="EP")]))
results$MeanErrorPercentage[2]=100*mean(abs(data$error[which(data$Term=="ET")]/data$PMA[which(data$Term=="ET")]))
results$MeanErrorPercentage[3]=100*mean(abs(data$error[which(data$Term=="FT")]/data$PMA[which(data$Term=="FT")]))
results$MeanErrorPercentage[4]=100*mean(abs(data$error[which(data$Term=="LP")]/data$PMA[which(data$Term=="LP")]))
results$MeanErrorPercentage[5]=100*mean(abs(data$error[which(data$Term=="VP")]/data$PMA[which(data$Term=="VP")]))
results$MeanErrorPercentage[6]=100*mean(abs(data$error/data$PMA))
c = results$`nombre observations`
results = results[,-5]
results$`nombre observations` = c

#affichage des resultats
View(results)
#graphique
data$PMA_estim = data$PMA - data$error
k=unique(data$ID)
for(i in k){
  d=data[which(data$ID==i),]
  plot(d$Day,d$PMA_estim,col="red",type = "o",lwd = 2, main = paste("identifiant de l'individu : ",paste0(i)), xlab = "Day", ylab = "PMA_estimé", ylim = c(min(d$PMA_estim, d$PMA), max(d$PMA_estim, d$PMA)))
  points(d$Day, d$PMA, type = "o", col = "blue", pch = 16)
}
library(ggplot2)
palette = c("black", "blue", "#007F00", "#FF7F00", "#5FA5E5")
ggplot(data, aes(x = PMA_estim, y = PMA, color = Term)) + 
  geom_point(shape = 16, size = 2) +
  scale_color_manual(values = palette) +
  xlab("PMA ESTIME") + ylab("PMA") +
  # Ajout de la droite y = x
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  # Ajout de la légende
  guides(color = guide_legend(title = NULL, override.aes = list(linewidth = 2))) +
  labs(color = "Term / Groupe", 
       linetype = "Droite", 
       title = "Relation entre PMA réel et PMA estimé modèle linéaire backward GA") +
  scale_linetype_manual(values = c("solid")) +
  theme_classic()

#enregistrement
results[2:7] <- lapply(results[2:7], as.numeric)
results[2:7] = round(results[2:7],3)
htmlwidgets::saveWidget(datatable(results), "plots/eval_modele_lineaire_backward_GA.html")

