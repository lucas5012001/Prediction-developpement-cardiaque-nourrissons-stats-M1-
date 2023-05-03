rm(list=ls())
library(readxl)
library(matrixStats)
source("fonctions.R")

## Code à executer pour creer les donnees.
##########################################
ponderer_par_nbHighqual = TRUE
scale_les_donnees = TRUE
##########################################

#nettoyage de la table et conversion des types de variables.
X50healthy <- as.data.frame(read_excel("donnees/50healthy.xlsx"))
donnees_brutes=read.csv2("donnees/HRV_dataset_30min.csv",sep = ",")[,-c(1,34)]
for(i in 1:ncol(donnees_brutes)){
  if(!colnames(donnees_brutes)[i] %in% c("Date", "Term","ID")){
    donnees_brutes[,i] = as.numeric(donnees_brutes[,i])
  }
}
donnees_brutes$tqrs.diff = donnees_brutes$tqrs.end.ind - donnees_brutes$tqrs.start.ind
donnees_brutes$duree = donnees_brutes$Time.end - donnees_brutes$Time.start
#selection des bébés sains puis enregistrement.
healthy = donnees_brutes[which(donnees_brutes$ID %in% X50healthy$ID),][,-c(1,2,3,4)]
write.csv(healthy, file = "donnees/healthy_stats_descriptives.csv",row.names = FALSE)

#mediane en fonction de Day et ID
healthy_median=aggregate(healthy[,-c(35,36)],list(healthy$ID,healthy$Day),median)
healthy_median = healthy_median[,-c(1,2)]
table_de_correspondances = unique(healthy[,c(30,31)])
for(i in 1:length(table_de_correspondances[,1])){
  healthy_median$Term[which(healthy_median$ID == table_de_correspondances$ID[i])] = table_de_correspondances$Term[i]
}

#On reprend seulement la structure du tableau précédent.
#On va refaire des médianes mais cette fois ci en les pondérant
#par nbHighqual.
#On commence par reinitialiser toutes les valeurs qui vont
#être recalculées cette fois ci avec pondérations.
healthy_median_quality = healthy_median
for(j in 1:ncol(healthy_median_quality)){
  if(!colnames(healthy_median_quality)[j] %in% c("Day", "Term","ID")){
    healthy_median_quality[,j] = 0
  }
}

#On recalcule chaque mediane avec ponderation en fonction de nbhighqual
for(j in 1:ncol(healthy_median_quality)){
  if(!colnames(healthy_median_quality)[j] %in% c("Day", "Term","ID")){
    for(i in 1:length(healthy_median_quality[,1])){
      pos_id_day = which(healthy$ID == healthy_median_quality$ID[i] & healthy$Day == healthy_median_quality$Day[i])
      weights_id_day = healthy$nbHighqual[pos_id_day]
      if(sum(weights_id_day==0)){
        weights_id_day = weights_id_day + 1
      }
      healthy_median_quality[i,j]=matrixStats::weightedMedian(healthy[,colnames(healthy_median_quality)[j]][pos_id_day],weights_id_day)
    }
  }
}

#On a obtenu deux nouveaux tableaux : healthy_median et healthy_median_quality.
#On va maintenant selectionner les donnees a creer en fonction des parametres.
if(ponderer_par_nbHighqual == TRUE){
  data = conversion_type_variables_modele(healthy_median_quality)
} else{
  data = conversion_type_variables_modele(healthy_median)
}
if(scale_les_donnees == TRUE){
  d=data[,-which(colnames(data) %in% c("Day","PMA","Term","ID"))]
  d=scale(d)
  centres <- attr(d, "scaled:center")
  ecarts_types <- attr(d, "scaled:scale")
  centres = as.data.frame(t(centres))
  ecarts_types = as.data.frame(t(ecarts_types))
  scale_parameters = rbind(centres,ecarts_types)
  rownames(scale_parameters) = c("mean","sd")
  write.csv(scale_parameters, file = "donnees/scale_parameters.csv",row.names = TRUE)
  data[,-which(colnames(data) %in% c("Day","PMA","Term","ID"))]=d
} else{
  if (file.exists("donnees/scale_parameters.csv")) {
    file.remove("donnees/scale_parameters.csv")
  }
}

#enregistrement de data
write.csv(data, file = "donnees/data.csv",row.names = FALSE)

#On créée les 5 blocs pour la validation croisée, on les enregistre
l=kblocs(data,5)
bloc1=l[[1]]
bloc2=l[[2]]
bloc3=l[[3]]
bloc4=l[[4]]
bloc5=l[[5]]
write.csv(bloc1, file = "donnees/bloc1.csv",row.names = FALSE)
write.csv(bloc2, file = "donnees/bloc2.csv",row.names = FALSE)
write.csv(bloc3, file = "donnees/bloc3.csv",row.names = FALSE)
write.csv(bloc4, file = "donnees/bloc4.csv",row.names = FALSE)
write.csv(bloc5, file = "donnees/bloc5.csv",row.names = FALSE)

