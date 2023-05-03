rm(list = ls())
source("Fonctions.R")
library(corrplot)
library(FactoMineR)
library(Factoshiny)
library(plotly)
#On compte le nombre d'observations par jour et par nourrisson.
#On remarque que nous avons plus d'observations pour des jours
#proches de 0 et de moins en moins quand le jour augmente.
#On voit également que certains nourrissons ont moins nettement moins
#d'observations que d'autres.
######################################################
healthy = read.csv2("donnees/healthy_stats_descriptives.csv",sep=",")
healthy$compteur = 1
decompte=aggregate(healthy[,c(37)],list(healthy$ID,healthy$Day),sum)
colnames(decompte) = c("ID","Day","n_obs")
parallelPlot::parallelPlot(decompte)
decompte$ID = as.factor(decompte$ID)
decompte$Day = as.factor(decompte$Day)
fig <- plot_ly(
  x = decompte$ID, y = decompte$Day,
  z = decompte$n_obs, type = "heatmap"
) %>% layout(title = 'Quantité enregistrements par jour et par nourrisson', xaxis = list(title = 'identifiant nourrisson'), 
             yaxis = list(title = 'jour du nourrisson'), legend = list(title=list(text='<b> nombre enregistrements </b>')))

fig
######################################################
#On importe maintenant les données que nous allons utiliser avec
#nos modèles pour les étudier. Ces données sont issues d'une médiane
#par jour et par bébé des données précédentes. (pondérées par la qualité nbHighqual)
data=conversion_type_variables_modele(read.csv2("donnees/data.csv",sep=","))
summary(data)
#Valeurs manquantes ?
any(is.na(data))
#dispertion des derniers jours d'observation par bébé
dernier_jour_par_bebe=aggregate(data[,c(33)],list(data$ID),max)
colnames(dernier_jour_par_bebe) = c("ID","dernier_jour")
boxplot(dernier_jour_par_bebe$dernier_jour,ylab="dernier jour d'observation par bébé")
title("dispertion des derniers jours d'observation")
#durees totales d'observations par jours
tot_par_jour=aggregate(decompte[,3],list(decompte$Day),sum)
colnames(tot_par_jour)=c("Day","n_obs")
plot(tot_par_jour$Day,tot_par_jour$n_obs,xlab = "jour",ylab = "nombre total d'observations ce jour")
#durees totales d'observations par PMA
data_PMA = healthy[,c(37,34)]
data_PMA$PMA = round(as.numeric(data_PMA$PMA)*7)
tot_par_PMA = aggregate(data_PMA[,1],list(data_PMA$PMA),sum)
colnames(tot_par_PMA) = c("PMA","n_obs")
plot(tot_par_PMA$PMA,tot_par_PMA$n_obs,xlab = "PMA (en jours)",ylab = "nombre total d'observations pour ce PMA")
#nombre de bébés observés par PMA
bebes_par_PMA = healthy[,c(30,34,37)]
bebes_par_PMA$PMA = round(as.numeric(bebes_par_PMA$PMA)*7)
bebes_par_PMA <- subset(bebes_par_PMA, !duplicated(bebes_par_PMA))
bebes_par_PMA = aggregate(bebes_par_PMA[,3],list(bebes_par_PMA$PMA),sum)
colnames(bebes_par_PMA)=c("PMA_en_jours","nb_nourrissons_obs")
plot(bebes_par_PMA$PMA_en_jours,bebes_par_PMA$nb_nourrissons_obs,xlab = "PMA (en jours) du nourrisson", ylab = "nombre de nourrissons observés pour ce PMA")
#Variables corrélées
corrplot(cor(data[,-c(30,31)]), method = "ellipse")
#toile d'araignée
data$ID = as.factor(data$ID)
data$Term=as.factor(data$Term)
parallelPlot::parallelPlot(data)

