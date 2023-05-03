rm(list = ls())
library(ggplot2)
source("fonctions.R")
source("fonctions_modeles/modeles_HRV&GA_related/modele_combine_GA.R")
healthy = read.csv("donnees/healthy_stats_descriptives.csv")[,-c(35,36)]
healthy = conversion_type_variables_modele(healthy)
healthy[,-c(30,31,33,34)] = scale(healthy[,-c(30,31,33,34)])
data = read.csv("donnees/data.csv")
data = conversion_type_variables_modele(data)
##############################
#1 : leave_one_out données médianes
loo_list_mediane = loo(data)
predictions_mediane = rep(0,length(data[,1]))
for(i in 1:length(loo_list_mediane)){
  print(paste("médianes",i))
  predictions_mediane[loo_list_mediane[[i]]] = modele_combine_GA(data[-loo_list_mediane[[i]],],data[loo_list_mediane[[i]],])[,2]
}
data$PMA_estim = predictions_mediane
MSE_mediane = mean((data$PMA_estim - data$PMA)**2)
MAE_mediane = mean(abs(data$PMA_estim - data$PMA))
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
       title = "Relation entre PMA réel et PMA estimé combine GA loo (data)") +
  scale_linetype_manual(values = c("solid")) +
  theme_classic()
##############################
#2 : leave_one_out données complètes
loo_list_healthy = loo(healthy)
predictions_healthy = rep(0,length(healthy[,1]))
for(i in 1:length(loo_list_healthy)){
  print(paste("healthy",i))
  predictions_healthy[loo_list_healthy[[i]]] = modele_combine_GA(healthy[-loo_list_healthy[[i]],],healthy[loo_list_healthy[[i]],])[,2]
}
healthy$PMA_estim = predictions_healthy
healthy=aggregate(healthy[,c(34,35)],list(healthy$ID,healthy$Day,healthy$Term),median)
colnames(healthy) = c("ID","Day","Term","PMA","PMA_estim")
MSE_healthy = mean((healthy$PMA_estim - healthy$PMA)**2)
MAE_healthy = mean(abs(healthy$PMA_estim - healthy$PMA))
palette = c("black", "blue", "#007F00", "#FF7F00", "#5FA5E5")
ggplot(healthy, aes(x = PMA_estim, y = PMA, color = Term)) + 
  geom_point(shape = 16, size = 2) +
  scale_color_manual(values = palette) +
  xlab("PMA ESTIME") + ylab("PMA") +
  # Ajout de la droite y = x
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  # Ajout de la légende
  guides(color = guide_legend(title = NULL, override.aes = list(linewidth = 2))) +
  labs(color = "Term / Groupe", 
       linetype = "Droite", 
       title = "Relation entre PMA réel et PMA estimé combine GA loo (healthy)") +
  scale_linetype_manual(values = c("solid")) +
  theme_classic()
##############################
MSE_mediane
MAE_mediane
MSE_healthy
MAE_healthy

#> MSE_mediane
#[1] 2.17513
#> MAE_mediane
#[1] 1.137196
#> MSE_healthy
#[1] 2.658756
#> MAE_healthy
#[1] 1.209492