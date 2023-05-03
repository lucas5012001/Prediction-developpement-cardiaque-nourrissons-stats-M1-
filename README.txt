Nous avons travaillé sur une base de données contenant des variables issues de la décomposition du signal d'électrocardiogrammes de nourrissons. Le but était de prédire leur niveau de développement cardiaque à partir de ces données. Pour cela, nous avons entrainé plusieurs modèles à prédire l'âge post menstruel de nourrissons dont on considérait le développement cardiaque normal. Nous avons notamment utilisé les modèles suivants : régression linéaire, régression linéaire à effets mixtes, forêts aléatoires (random forests), forêts aléatoires prenant en compte des effets mixtes (librairie GPboost), support vector machine (svm), k plus proches voisins (knn) et XGboost. Pour chaque modèle, les hyperparamètres ont été optimisés par validation croisée. Nous avons également utilisé des méthodes de sélection de variables (Lasso, Ridge, Elastic Net, Forward, Backward, Both-directions). Les méthodes qui se sont avérées être les plus efficaces sont celles reposant sur des méthodes d'Ensemble Learning, basées sur la combinaison de forêts aléatoires, de modèles linéaires et de modèles GPboost (forêts aléatoires avec effets mixtes).

Interprétation des modèles :
_____________________________________________________________________________________________________________________________________________________________________________

Modeles n'utilisant pas la variable GA : "(PMA_estimé_par_le_modèle)" représente l'âge cardiaque absolu du nourrisson en semaines.

Modeles utilisant la variable GA : "(PMA_réel - PMA_estimé_par_le_modèle)" représente le retard cardiaque algébrique en semaines (retard réel si positif et avance si négatif)
du nourrisson par rapport aux nourrissons en bonne santé de niveau de prématurité comparable.

Organisation du code :
_____________________________________________________________________________________________________________________________________________________________________________

- fonctions.R : c'est ici que nous avons codé les fonctions qui servent à plusieurs reprises dans le code.

- prepa_donnees.R : c'est ici que le code permettant d'isoler les nourrissons sains des autres, de faire les médianes et de scale les données est mis. Au début,
on peut choisir si on souhaite pondérer les médianes par la qualité des données ou non et si on veut ou non les scale. En fonction de ces paramètres, l'execution du code
va créer les fichiers nécéssaires dans le dossier données.

- donnees : c'est dans ce fichier que se trouvent les données de base du projet mais c'est aussi ici que les nouvelles données créées à partir de ces dernières sont stockées
lorsque le fichier préparation des données les enregistre.

- recherche modèles : c'est dans ce fichier que nous avons mis les codes permettant d'optimiser les hyperaramètres ou bien de mettre au point nos modèles. Toute la partie
"recherche" du code s'y trouve.

- fonctions modèles : c'est ici que nous avons créé les fichiers contenant le code des modèles une fois leurs hyperparamètres optimaux définis. Ce fichier contient les
modèles présentés dans le rapport.

- évaluation modèles : c'est ici que se trouve le code permettant de tester nos modèles par validation croisée.

- plots : dans ce fichier, on enregistre les graphiques montrant la performance de nos modèles. Ces graphiques sont générés quand on execute un fichier du dossier 
évaluation modèles.