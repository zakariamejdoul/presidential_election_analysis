install.packages(c("readr","FactoMineR"))
install.packages(c("factoextra"))

library(readr)
library(FactoMineR)
library(factoextra)


# En premier lieu on lie le fichier CSV "Presidentielle_2017_Resultats_Communes_T1_clean.csv" qui est un DataSet
# qui comprend les résultats des élections présidentielle de 2017
Presidentielle_2017_Resultats_Communes_T1_clean <- read_csv("Presidentielle_2017_Resultats_Communes_T1_clean.csv")

colnames(Presidentielle_2017_Resultats_Communes_T1_clean)[20] <- "MELENCHON" # pour résoudre un probleme lie a l'accentuation du E

# Extraction des données par colonnes qui représentent les candidats des élections 
donnees_var <- Presidentielle_2017_Resultats_Communes_T1_clean[,c('Abstentions','Blancs','Nuls','LE PEN','MELENCHON','MACRON','FILLON','LASSALLE','DUPONT-AIGNAN','HAMON','ASSELINEAU','POUTOU','ARTHAUD','CHEMINADE')]
# Extraction des lignes qui représentent les départements
departements <- factor(Presidentielle_2017_Resultats_Communes_T1_clean$Département)
# Création de la matrice de données qui représente le tableau de contingence
donnees_elections <- matrix(NA,nlevels(departements),ncol(donnees_var))

# La sommation des votes de chaque candidat selon les départements extraits dans la variable "departements"
for (j in 1:nlevels(departements)){
  dep = levels(departements)[j]
  donnees_elections[j,] <- colSums(donnees_var[departements==dep,])
}

# Caster la matrice "donnees_elections" en un Data.frame en nommant les lignes par les noms des départements 
# et les colonnes par les noms des candidats
donnees_elections <- data.frame(donnees_elections,row.names = levels(departements))
colnames(donnees_elections) <- colnames(donnees_var)

# Test du Chi-2 pour vérifier la dépendance significative entre les lignes et les colonnes du tableau
chisq.test(donnees_elections)

# AFC sur le tableau de données 
res.CA <- CA(donnees_elections, graph = FALSE)
# Biplot symétrique
fviz_ca_biplot (res.CA, repel = TRUE)
# Biplot asymétrique
fviz_ca_biplot (res.CA,map = "rowprincipal", arrow = c(TRUE, TRUE), repel = TRUE)

# Statistique du Chi-2, valeurs propres et pourcentage d’inertie expliqué par chacun des axes, description des lignes et des colonnes
summary(res.CA)

# Selection des résultats pour les lignes 
row <- get_ca_row(res.CA)

# Qualité de représentation des lignes sur l'axe 1 et 2
fviz_ca_row (res.CA, col.row = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, title="Qualité de représentation des lignes")

# Contribution des lignes aux axes 1 et 2
fviz_ca_row (res.CA, col.row = "contrib",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,title="Contribution des lignes aux axes 1 & 2")

# Selection des résultats pour les colonnes 
col <- get_ca_col(res.ca)

# Qualité de représentation des colonnes sur l'axe 1 et 2
fviz_ca_col (res.CA, col.col = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, title="Qualité de représentation des colonnes")

# Contribution des colonnes aux axes 1 et 2
fviz_ca_col (res.CA, col.col = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, title="Contribution des colonnes aux axes 1 & 2")

# Visualiser la répartition des valeurs propres
barplot(res.CA$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.CA$eig))
eig.val <- get_eigenvalue(res.CA)
head(eig.val)
fviz_screeplot(res.CA, addlabels = TRUE, ylim = c(0, 50), main="Répartition des valeurs propres")


## Etude du 1er axe

# Sélection des 20 lignes dont la somme au carré des contributions est importante
fviz_ca_biplot (res.CA, map = "rowgreen", arrow = c (TRUE, FALSE),repel = TRUE,selectRow="contrib 20")

# Description de la Dim 1
dimdesc(res.CA) -> res.dimdesc
res.dimdesc$`Dim 1`

# Représentation des colonnes sur Dim 1
sort(round(100*res.CA$col$cos2[,1],2),decreasing = TRUE)

# Calcul des profils lignes & profils colonnes (Outres-Mer)
profils.lignes <- round(100*prop.table(as.matrix(donnees_elections),margin=1),2)
profil.moyen.colonnes <- round(100*margin.table(as.matrix(donnees_elections),margin=2)/sum(donnees_elections),2)
rbind(profils.lignes[c('Martinique','Guadeloupe','Guyane','Mayotte','La Réunion','Polynésie française','Saint-Pierre-et-Miquelon','Wallis et Futuna'),],profil.moyen.colonnes)

# Calcul des profils lignes & profils colonnes (Paris/Hauts-de-Seine)
rbind(profils.lignes[c('Paris','Hauts-de-Seine'),],profil.moyen.colonnes)


## Etude du 2ème axe

# Description de la Dim 2
res.dimdesc$`Dim 2`

# Représentation des colonnes sur Dim 2
sort(round(100*res.CA$col$cos2[,2],2),decreasing = TRUE)

# Outre-Mer/Île-de-France qui ont moins voté Le Pen opposés aux Grand-Est/Hauts-de-France qui ont plus voté pour Le Pen
profil.moyen.OM <- colMeans(profils.lignes[c('Martinique','Guadeloupe','Guyane','Mayotte','La Réunion','Polynésie française','Saint-Pierre-et-Miquelon','Wallis et Futuna'),])
profil.moyen.IDF <- colMeans(profils.lignes[c('Paris','Hauts-de-Seine','Seine-Saint-Denis','Val-de-Marne'),])
profil.moyen.GE <- colMeans(profils.lignes[c('Ardennes', 'Aube', 'Bas-Rhin', 'Haute-Marne', 'Haut-Rhin', 'Marne', 'Meurthe-et-Moselle', 'Meuse', 'Moselle', 'Vosges'),])
profil.moyen.HDF <- colMeans(profils.lignes[c('Aisne', 'Nord', 'Oise', 'Pas-de-Calais', 'Somme'),])
data.frame(rbind(profil.moyen.OM,profil.moyen.IDF,profil.moyen.GE,profil.moyen.HDF,profil.moyen.colonnes),row.names=c("Outre-Mer","Île-de-France","Grand Est","Haut-de-France","Moyenne nationale"))


## Etude du 3ème axe


#  Sélection des 20 lignes dont la somme au carré des contributions est importante
plot(res.CA,selectRow="contrib 20",axes=c(3,4))
fviz_ca_biplot (res.CA, map = "rowgreen", arrow = c(TRUE, FALSE),repel = TRUE,selectRow="contrib 20",axes=c(3,4))

# Description de la Dim 3
res.dimdesc$`Dim 3`

# Res par colonne de qualité de représentation et contribution à la Dim 3
sort(round(100*res.CA$col$cos2[,3],2),decreasing = TRUE)
sort(round(100*res.CA$col$contrib[,3],2),decreasing = TRUE)

# Vote de Hautes-Pyrénées/Ariège/Pyrénées-Atlantiques
rbind(profils.lignes[c('Hautes-Pyrénées','Ariège','Pyrénées-Atlantiques'),],profil.moyen.colonnes)

# Contribution de Hautes-Pyrénées/Ariège/Pyrénées-Atlantiques à la construction de l'axe 3
res.CA$row$cos2[c('Hautes-Pyrénées','Ariège','Pyrénées-Atlantiques'),3]
sort(res.CA$row$contrib[,3],decreasing=TRUE)[1:15]

# Qualité de représentation des lignes sur l'axe 3 et 4
fviz_ca_row (res.CA, col.row = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, title="Qualité de représentation des lignes",axes=c(3,4))

# Contribution des lignes aux axes 3 et 4
fviz_ca_row (res.CA, col.row = "contrib",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,title="Contribution des lignes aux axes 3 & 4",axes=c(3,4))

## Etude du 4ème axe


# Description de la Dim 4
dimdesc(res.CA,axes=4)

# Vote de Corse-du-Sud/Haute-Corse/Pyrénées-Atlantiques
rbind(profils.lignes[c('Corse-du-Sud','Haute-Corse', 'Pyrénées-Atlantiques'),],profil.moyen.colonnes)

# Contribution de Pyrénées-Atlantiques/Corse-du-Sud/Haute-Corse à la construction de l'axe 4
res.CA$row$cos2[c('Pyrénées-Atlantiques','Corse-du-Sud','Haute-Corse'),4]
sort(res.CA$row$contrib[,4],decreasing=TRUE)[1:15]
