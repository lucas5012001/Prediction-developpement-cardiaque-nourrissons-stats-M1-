conversion_type_variables_modele = function(data_modele){
  #cette fonction va convertir les données de modèle au bon
  #format (numeric et factor) et les trier par identifiant.
  #cette fonction est très spécifiques aux données utilisées dans
  #le projet car elle utilise les nom des colonnes de nos jeux de données. 
  for(j in 1:ncol(data_modele)){
    if(!colnames(data_modele)[j] %in% c("Term","ID")){
      data_modele[,j] = as.numeric(data_modele[,j])
    }
  }
  data_modele$Term = as.factor(data_modele$Term)
  data_modele$ID = as.factor(data_modele$ID)
  data_modele = data_modele[order(data_modele$ID,decreasing=T), ]
  rownames(data_modele)=seq(1,length(data_modele[,1]),1)
  return(data_modele)
}

kblocs = function(data_modele,nbblocs){
  data=conversion_type_variables_modele(data_modele)
  data$Term = as.character(data$Term)
  data$ID = as.character(data$ID)
  id_EP = unique(data[which(data$Term=="EP"),]$ID)
  id_VP = unique(data[which(data$Term=="VP"),]$ID)
  id_LP = unique(data[which(data$Term=="LP"),]$ID)
  id_ET = unique(data[which(data$Term=="ET"),]$ID)
  id_FT = unique(data[which(data$Term=="FT"),]$ID)
  id_tot = c(id_EP,id_VP,id_LP,id_ET,id_FT)
  liste_blocs = list()
  for(k in 1:nbblocs){
    i=k
    id = c()
    while(i<=length(id_tot)){
      id = c(id,id_tot[i])
      i=i+nbblocs
    }
    d=data[which(data$ID %in% id),]
    liste_blocs[[k]]=d
  }
  return(liste_blocs)
}

loo = function(data_modele){
  data = conversion_type_variables_modele(data_modele)
  data$Term = as.character(data$Term)
  data$ID = as.character(data$ID)
  id = unique(data$ID)
  liste_pos = list()
  for(i in 1:length(id)){
    liste_pos[[i]] = which(data$ID == id[i])
  }
  return(liste_pos)
}