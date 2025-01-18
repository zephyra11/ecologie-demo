#charger les packages
library(tidyverse)
library(ratdat)
#graphique
ggplot(data = complete_old, aes(x=weight, y=hindfoot_length)) + geom_point(color="red", alpha=0.1)

#commande pour git
#git add/ git commit -m / git status / git log
#git add analyse.R / git commit -m "ajout de la couleur/ 

#faut tjs faire save le fichier, add dans terminal, commit
#onglet à droite en haut, cliquer sur le nom (case) cliquer sur commit, ecrire le message
#history pour voir les modif
