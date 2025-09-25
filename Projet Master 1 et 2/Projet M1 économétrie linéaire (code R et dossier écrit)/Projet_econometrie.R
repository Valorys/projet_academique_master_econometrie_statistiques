
  
# Preparation des données
  
# Librairies

library(readxl)
library(writexl)
library(outliers)
library(dplyr)
library(VIM)
library(car)
library(MASS)
library(ggplot2)
library(EnvStats)
library(pander)
library(corrplot)
library(PerformanceAnalytics)
library(tidyverse)
library(questionr)
library(leaps)
library(stats)
library(lmtest)
library(AER)

# chemin
#getwd() 


# Vecteurs de couleur


couleurs_genre <- c("darkorchid4", "blue4")
couleurs <- c("darkorchid4", "blue4")


## Préparation et visualitation des données

#Téléchargement et visualition du fichier, à l'aide de la librairie "readxl"
#Supprimer la colonne "horrodateur" qui est inutile



ecran <- read_excel("data/BDD_propre.xlsx")
head(ecran)


ecran <- ecran[,-1]


### Renommer les colonnes


colnames(ecran) <- c("age", "genre", "csp", "lieu_de_vie", "nb_personnes", "revenu", "temps_ecran", "nb_appareils", "internet", "fibre", "forfait_tel", "telephone_pro", "ecran_travail", "sommeil", "stress", "loisir", "vie_sociale", "ecran_jour", "reseaux_sociaux", "nb_abonnements")




### Mettre en numérique les variables


str(ecran)
ecran$loisir <- as.numeric(ecran$loisir)
ecran$vie_sociale <- as.numeric(ecran$vie_sociale)
str(ecran)
summary(ecran)

# Mettre en facteurs les variables


ecran$age <- as.factor(ecran$age)
ecran$genre <- as.factor(ecran$genre)
ecran$csp <- as.factor(ecran$csp)
ecran$lieu_de_vie <- as.factor(ecran$lieu_de_vie)
ecran$revenu <- as.factor(ecran$revenu)
ecran$nb_appareils <- as.factor(ecran$nb_appareils)
ecran$internet <- as.factor(ecran$internet)
ecran$fibre <- as.factor(ecran$fibre)
ecran$forfait_tel <- as.factor(ecran$forfait_tel)
ecran$telephone_pro <- as.factor(ecran$telephone_pro)
ecran$ecran_travail <- as.factor(ecran$ecran_travail)
ecran$sommeil <- as.factor(ecran$sommeil)
ecran$stress <- as.factor(ecran$stress)
ecran$ecran_jour <- as.factor(ecran$ecran_jour)
ecran$reseaux_sociaux <- as.factor(ecran$reseaux_sociaux)
ecran$nb_abonnements <- as.factor(ecran$nb_abonnements)



#Résumé des toutes les variables


str(ecran)
summary(ecran)


# Valeurs manquantes - visualisation


sum(is.na(ecran))

lapply(ecran, function(x) which(is.na(x)))


# Nous avons au total 41 valeurs manquantes

# Résumé des variables numériques



summary(select_if(ecran, is.numeric))
sapply(select_if(ecran, is.numeric), sd)


# Imputation des valeurs manquantes

# L'algorithme des k-plus proches voisins utilisera le voisin le plus proche pour l'imputation


ecran <- kNN(ecran, dist_var = colnames(ecran), k = 1)
ecran <- ecran[, -c(21:40)]


# Exportation de la base de données sous format excel
# write_xlsx(x = ecran, path = "data/BDD_sans_NA.xlsx")


# Utilisation de cette nouvelle base pour la suite du travail


ecran <- read_excel("data/BDD_sans_NA.xlsx", col_names = T)


# Mettre en facteurs les variables


str(ecran)
ecran$age <- as.factor(ecran$age)
ecran$genre <- as.factor(ecran$genre)
ecran$csp <- as.factor(ecran$csp)
ecran$lieu_de_vie <- as.factor(ecran$lieu_de_vie)
ecran$revenu <- as.factor(ecran$revenu)
ecran$nb_appareils <- as.factor(ecran$nb_appareils)
ecran$internet <- as.factor(ecran$internet)
ecran$fibre <- as.factor(ecran$fibre)
ecran$forfait_tel <- as.factor(ecran$forfait_tel)
ecran$telephone_pro <- as.factor(ecran$telephone_pro)
ecran$ecran_travail <- as.factor(ecran$ecran_travail)
ecran$sommeil <- as.factor(ecran$sommeil)
ecran$stress <- as.factor(ecran$stress)
ecran$ecran_jour <- as.factor(ecran$ecran_jour)
ecran$reseaux_sociaux <- as.factor(ecran$reseaux_sociaux)
ecran$nb_abonnements <- as.factor(ecran$nb_abonnements)
str(ecran)


# Statistiques descriptives

### Analyse univariée des variables qualitatives

#### Tableaux de contingence


table(ecran$genre,ecran$age)
table(ecran$csp)
table(ecran$csp, ecran$revenu)
table(ecran$revenu)


#### Diagrammes en bâtons


ecran |>
  ggplot() +
  aes(x = age, fill = age) +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition de l'âge") +
  labs(x = "âge", y="effectif") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))


ecran |>
  count(genre) |>
  ggplot() +
  aes(x = "", y = n, fill = genre) +
  geom_bar(stat = "identity", color="white") +
  ggtitle("Répartition du genre") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  coord_polar("y") +
  scale_fill_manual(values = couleurs_genre) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "white",size = 5, fontface = "bold")




ecran |>
  ggplot() +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition des lieux de résidence") +
  aes(x = fct_infreq(lieu_de_vie), fill = lieu_de_vie) +
  labs(x = "lieux de résidence",y="effectif") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) 




# Nous utilisons 'levels' pour renommer nos variables afin d'améliorer la visualisation. Cependant, il est important de noter que cela modifie également les noms dans la base de données


#levels(ecran$csp) <- c("agriculteurs","artisans","sans activité", "cadres","employés","ouvriers", "intermédiaire","retraités")
ecran |>
  ggplot() +
  aes(x =fct_infreq(csp), fill = csp) +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition de la catégorie socio-professionnelle") +
  labs(x = "csp", y="effectif") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1))




ecran |>
  ggplot() +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition du revenu") +
  aes(x = fct_infreq(revenu), fill = revenu) +
  labs(x = "Revenu", y="effectif") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12),
         axis.text.x = element_text(angle = 45, hjust = 1)) 




#levels(ecran$internet) <- c("Non","ADSL","Fibre", "Autre solution")
ecran |>
  ggplot(aes(x = internet, fill = internet)) +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition des moyens de connexion") +
  aes(x = fct_infreq(internet), fill = internet) +
  labs(x = "internet", y="effectif") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))




ecran |>
  ggplot(aes(x = fct_infreq(nb_appareils), fill = nb_appareils)) +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition du nombre d'appareils") +
  labs(x = "appareils", y="effectif") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) 




ecran |>
  count(fibre) |>
  ggplot() +
  aes(x = "", y = n, fill = fibre) +
  geom_bar(stat = "identity", color = "white") +
  ggtitle("Répartition de l'accès à la fibre") +
  coord_polar("y") +
  scale_fill_manual(values = couleurs) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "white",size = 5, fontface = "bold")



ecran |>
  ggplot(aes(x = fct_infreq(forfait_tel), fill = forfait_tel)) +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition des forfaits téléphoniques") +
  labs(x = "forfait", y="effectif")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
         axis.text.x = element_text(angle = 45, hjust = 1)) 






ecran |>
  count(telephone_pro) |>
  ggplot() +
  aes(x = "", y = n, fill = telephone_pro) +
  geom_bar(stat = "identity", color = "white") +
  ggtitle("Répartition de la possession d'un téléphone professionnel") +
  coord_polar("y") +
  scale_fill_manual(values = couleurs) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
   geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "white",size = 5, fontface = "bold") 
  




#levels(ecran$ecran_travail) <- c("Fréquemment", "Jamais", "Parfois", "Rarement", "Toute la journée")

ecran |>
  ggplot(aes(x = fct_infreq(ecran_travail), fill = ecran_travail)) +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition de la consommation numérique au travail") +
  labs(x="fréquence",y="effectif")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))
  




ecran |>
  ggplot(aes(x = fct_infreq(sommeil), fill = sommeil)) +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition du temps de sommeil") +
  labs(x="durée",y="effectif")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 12))



ecran |>
  ggplot(aes(x = fct_infreq(stress), fill = stress)) +
  geom_bar(fill = "blue4") +
  labs(x="fréquence", y="effectif")+
  ggtitle("Répartition du stress") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 12))



ecran |>
  ggplot(aes(x = fct_infreq(ecran_jour), fill = ecran_jour)) +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition du moment de la journée") +
  labs(x="fréquence", y="effectif")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1))



ecran |>
  ggplot(aes(x = fct_infreq(reseaux_sociaux), fill = reseaux_sociaux)) +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition des réseaux sociaux") +
  labs(x="quantité",y="effectif")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 12))
  



ecran |>
  ggplot(aes(x = fct_infreq(nb_abonnements), fill = nb_abonnements)) +
  geom_bar(fill = "blue4") +
  ggtitle("Répartition des abonnements") +
  labs(x="quantité", y="effectif")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))


### Analyse bivariée des variables qualitatives


ecran|>
pivot_longer(
cols = temps_ecran,
names_to = "mesure",
values_to = "valeur"
) |>
ggplot() +
aes(y = valeur, x = age, color = age) +
geom_boxplot() +
geom_jitter(alpha = 0.3) +
#scale_color_manual(values = vecteur_couleur)
facet_wrap(~ mesure, scales = "free_y") +
theme_bw()

ecran|>
pivot_longer(
cols = temps_ecran,
names_to = "mesure",
values_to = "valeur"
) |>
ggplot() +
aes(y = valeur, x = csp, color=csp) +
geom_boxplot() +
geom_jitter(alpha = 0.3) +
#scale_color_manual(values = vecteur_couleur)
facet_wrap(~ mesure, scales = "free_y") +
theme_bw() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

ecran|>
pivot_longer(
cols = temps_ecran,
names_to = "mesure",
values_to = "valeur"
) |>
ggplot() +
aes(y = valeur, x = ecran_jour, color=ecran_jour) +
geom_boxplot() +
geom_jitter(alpha = 0.3) +
#scale_color_manual(values = vecteur_couleur)
facet_wrap(~ mesure, scales = "free_y") +
theme_bw() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(ecran) +
    geom_boxplot(aes(x = csp, y = temps_ecran))



### Analyse univariée des variables quantitatives


str(ecran)
summary(select_if(ecran, is.numeric))


#### Histogrammes


hist(ecran$nb_personnes, main= "Répartition du nombre de personnes dans le foyer", xlab="personnes", ylab = "nombre d'individus", col = "blue4")

hist(ecran$temps_ecran, main= "Répartition du temps d'écrans", xlab="durée",ylab = "nombre d'individus", col = "blue4")

hist(ecran$loisir, main= "Répartition du temps consacré aux loisirs", xlab="durée", ylab = "nombre d'individus", col = "blue4")

hist(ecran$vie_sociale, main= "Répartition du temps consacré à la vie sociale", xlab="durée",ylab = "nombre d'individus", col = "blue4")


#### Boxplots


ecran|>
pivot_longer(
cols = where(is.numeric)
) |>
ggplot() +
aes(y = value) +
facet_wrap(~ name, scales = "free_y") +
geom_boxplot() +
theme_light()


### Valeurs atypiques - Test Rosner


rosnerTest(ecran$temps_ecran,k=2, alpha=0.05)
rosnerTest(ecran$loisir,k=2, alpha=0.05)
rosnerTest(ecran$vie_sociale,k=5, alpha=0.05)


# D'apres le test de Rosner nous avons 2 valeurs atypiques dans la variable "loisir" et 1 valeur atypique dans la variable "vie_sociale". Les observations 80, 61 et 34 sont donc des valeurs atypiques

## Supprimer les valeurs atypiques


ecran <- ecran[-c(9, 61, 34),]
dim(ecran)


# Statistiques descriptives sans les valeurs atypiques

## Analyse univariée des variables quantitatives


str(ecran)
summary(ecran)
summary(select_if(ecran, is.numeric))


## Histogrammes


par(mfrow = c(2,2))
hist(ecran$nb_personnes, main= "Répartition du nombre de personnes dans le foyer", xlab="personnes",ylab = "nombre d'individus", col = "blue4")
hist(ecran$temps_ecran, main= "Répartition du temps d'écrans", xlab="durée", ylab = "nombre d'individus", col = "blue4")
hist(ecran$loisir, main= "Répartition du temps consacré aux loisirs", xlab="durée", ylab = "nombre d'individus", col = "blue4")
hist(ecran$vie_sociale, main= "Répartition du temps consacré à la vie sociale", xlab="durée",ylab = "nombre d'individus", col = "blue4")


#  Analyse bivariée des variables quantitatives

# Nous observons à l'aide de nuages de points le lien entre "temps_ecran" (y) et les variables explicatives


par(mfrow = c(2,2))
purrr::map2(
  .x = c("nb_personnes", "loisir", "vie_sociale"),
  .y = c("temps_ecran", "temps_ecran", "temps_ecran"),
  .f = ~ ggplot(ecran) +
    aes(x = get(.x), y = get(.y)) +
    geom_point() +
    labs(x = .x, y = .y) +
    ggtitle(paste("Corrélation entre", .x, "et", .y)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))
)



# Correlation

#Sous forme de matrice :
  
  
mydata <- ecran[,c("nb_personnes","temps_ecran","loisir","vie_sociale")]
corr_mat=cor(mydata,method="s")
corr_mat
corrplot(corr_mat, method = 'number',type="upper", tl.srt=45)
corrplot(corr_mat,type="upper")



#Nous pouvons voir que la corrélation est très faible entre chaque variable explicative.De plus, la variable à expliquer a une corrélation négative avec les variables "loisir" et "vie_sociale" et positive avec la variable "nb_personnes".

# Test de normalité des variables numeriques :
  
  
ks.test(ecran$nb_personnes,"pnorm",mean(ecran$nb_personnes),sd(ecran$nb_personnes))
ks.test(ecran$loisir,"pnorm",mean(ecran$loisir),sd(ecran$loisir))
ks.test(ecran$vie_sociale,"pnorm",mean(ecran$vie_sociale),sd(ecran$vie_sociale))


#Les variables ne suivent pas la loi normale , on va donc utiliser le coefficient de Spearman

#Sous forme de tableau :
  
  
cor(ecran[,c("nb_personnes","temps_ecran","loisir","vie_sociale")],
    use="complete.obs",method = c("spearman"))


#Pour avoir un aperçu visuel, nous utilisons une matrice de corrélation complétée par les nuages de points et les histogrammes.


chart.Correlation(mydata, histogram=TRUE, pch=19,method = c("spearman"))


# Conclusion : Pas de corrélation entre ces variables explicatives donc nous allons les conserver.

# Categories

#Pour avoir un nombre de variables correctes et pas démesurés ; Creation ecran2


View(ecran)
ecran2 <- ecran
summary(ecran2)


# Regrouper en catégories plus larges et creation de categories binaires

# Variables quali en binaire

# Reseaux sociaux


summary(ecran2$reseaux_sociaux)
ecran2 <- ecran2 |> 
  mutate(cat_reseau_0_2 = ifelse(reseaux_sociaux %in% c("0","1","2"), 1, 0),
         cat_reseau_3_5 = ifelse(reseaux_sociaux %in% c("3","4","5"), 1, 0),
         cat_reseau_6_sup6 = ifelse(reseaux_sociaux %in% c("6","7","8","9","Plus de 10"), 1, 0))



# Age


ecran2 <- ecran2 |> 
  mutate(cat_18_24 = ifelse(age == "18 - 24 ans", 1, 0),
         cat_25_49 = ifelse(age == "25 - 49 ans", 1, 0),
         cat_plus_50 = ifelse(age %in% c("50- 69 ans","70 ans et plus"), 1, 0))



#  Forfait telephone


summary(ecran2$forfait_tel)
ecran2 <- ecran2 |> 
  mutate(tel_inf_15 = ifelse(forfait_tel %in% c("0", "Moins  de 5 €","Entre 5 et 10 €","10 et 15 €" ), 1, 0),
         tel_15_25 = ifelse(forfait_tel %in% c("15 et 20 €","20 et 25 €"), 1, 0),
         tel_sup_25 = ifelse(forfait_tel %in% c("25 et 30 €","30 et 35 €","Plus de 35€"), 1, 0))



#  Nombre d'appareils


summary(ecran2$nb_appareils)
ecran2 <- ecran2 |> 
  mutate(appareils_1_2 = ifelse(nb_appareils %in% c("1", "2"), 1, 0),
         appareils_3_4 = ifelse(nb_appareils %in% c("3", "4"), 1, 0),
         appareils_plus_4 = ifelse(nb_appareils %in% c("5","6","7","8","9","10","Plus de 10"), 1, 0))


#  Nombre d'abonnements


summary(ecran2$nb_abonnements)
ecran2 <- ecran2 |> 
  mutate(abo_0 = ifelse(nb_abonnements == "0", 1, 0),
         abo_1 = ifelse(nb_abonnements == "1", 1, 0),
         abo_2 = ifelse(nb_abonnements == "2", 1, 0),
         abo_3_sup = ifelse(nb_abonnements %in% c("3","4","5","6","7", "8","9","10","Plus de 10"), 1, 0))



#  Genre


summary(ecran2$genre)
ecran2 <- ecran2 |> 
  mutate(homme = ifelse(genre == "Homme", 1, 0),
         femme = ifelse(genre == "Femme", 1, 0))



# Lieu de vie


summary(ecran2$lieu_de_vie)
ecran2 <- ecran2 |> 
  mutate(pas_ville = ifelse(lieu_de_vie %in% c("Campagne","Périurbaine/ Banlieue"), 1, 0),
         ville = ifelse(lieu_de_vie == "Ville", 1, 0))


#  CSP


summary(ecran2$csp)
ecran2 <- ecran2 |> 
  mutate(cat_Employes = ifelse(csp == "Employés", 1, 0),
         cat_sans_activite = ifelse(csp == "Autres personnes sans activité professionnelle (par exemple les chômeurs ou les étudiants)", 1, 0),
         cat_cadres = ifelse(csp == "Cadres et professions intellectuelles supérieures", 1, 0),
         cat_autres = ifelse(csp %in% c("Professions intermédiaires","Retraités", "Ouvriers", "Artisan/commerçant/Chef d’entreprise","Agriculteurs exploitants"), 1, 0))



#  Ecran au travail


summary(ecran2$ecran_travail)
ecran2 <- ecran2 |> 
  mutate(ecran_travail_plein = ifelse(ecran_travail == "Toute la journée", 1, 0),
         ecran_travail_bcp = ifelse(ecran_travail == "Fréquemment (Plus de la moitié du temps)", 1, 0),
         ecran_travail_peu = ifelse(ecran_travail %in% c("Parfois (Entre un quart et la moitié du temps)","Rarement","Jamais"), 1, 0))



#  Internet


summary(ecran2$internet)
ecran2 <- ecran2 |> 
  mutate(pas_internet = ifelse(internet == "Non", 1, 0),
         internet_autre = ifelse(internet %in% c("Oui, avec l’ADSL", "Oui, avec une autre solution"), 1, 0),
         internet_fibre = ifelse(internet == "Oui, avec la fibre", 1, 0))



#  Sommeil


summary(ecran2$sommeil)
ecran2 <- ecran2 |> 
  mutate(nuit_inf_7 = ifelse(sommeil %in% c("3h – 5h", "5h – 7h"), 1, 0),
         nuit_5_7 = ifelse(sommeil %in% c("7h et 9h","Plus de 9h"), 1, 0))



#  Ecran jour


summary(ecran2$ecran_jour)
ecran2 <- ecran2 |> 
  mutate(jour_3 = ifelse(ecran_jour == "Le matin, L’après midi, Le soir", 1, 0),
         jour_2 = ifelse(ecran_jour %in% c("L’après midi, Le soir", "Le matin, L’après midi", "Le matin, Le soir"), 1, 0),
         jour_1 = ifelse(ecran_jour %in% c("L’après midi","Le matin","Le soir"), 1, 0))



## Variables instruments

#  Revenu


summary(ecran2$revenu)
ecran2 <- ecran2 |> 
  mutate(cat_inf_20000 = ifelse(revenu %in% c("< 10 000","10 000 - 20 000"), 1, 0),
         cat_20_40000 = ifelse(revenu %in% c("20 000 - 30 000","30 000 - 40 000"), 1, 0),
         cat_sup_40000 = ifelse(revenu %in% c("40 000 - 50 000","50 000 - 60 000","60 000 - 70 000", "70 000 - 80 000","80 000 et plus"), 1, 0))



 # Fibre


summary(ecran2$fibre)
ecran2 <- ecran2 |> 
  mutate(fibre_oui = ifelse(fibre == "Oui", 1, 0),
         fibre_non = ifelse(fibre == "Non", 1, 0))



# Telephone pro



summary(ecran2$telephone_pro)
ecran2 <- ecran2 |> 
  mutate(tel_pro_non = ifelse(telephone_pro == "Non", 1, 0),
         tel_pro_oui = ifelse(telephone_pro == "Oui", 1, 0))



#  Stress


summary(ecran2$stress)
ecran2 <- ecran2 |> 
  mutate(stress_toujours = ifelse(stress == "Toujours", 1, 0),
         stress_bcp = ifelse(stress == "Fréquemment", 1, 0),
         stress_peu = ifelse(stress %in% c("Parfois", "Rarement","Jamais"),1, 0))




summary(ecran2)
str(ecran2)


# Creation 2 base de données

#  Base sans les instruments et la deuxieme avec les instruments. Les deux **avec** la variable de reférence


ecran_sans_instru <- ecran2[, -c(1,2,3,4,5,6,8,9,10,11,12,13,14,15,18,19,20,56,57,58,59,60,61,62,63,64,65)]
ecran_avec_instru <- ecran2[, -c(1,2,3,4,6,8,9,10,11,12,13,14,15,18,19,20)]

save(ecran_sans_instru, file = "data/ecran_sans_instru.rda")
save(ecran_avec_instru, file = "data/ecran_avec_instru.rda")



#  Base sans instruments et la deuxieme avec les instruments. Les deux **sans** la variable de reférence


ecran_sans_instru_sansref <- ecran_sans_instru[, -c(5,7,11,14,17,21,23,24,28,33,35,38)]

ecran_avec_instru_sansref <- ecran_avec_instru[, -c(6,8,12,15,18,22,24,25,29,34,36,39,42,43,46,49)]

save(ecran_sans_instru_sansref, file = "data/ecran_sans_instru_sansref.rda")
save(ecran_avec_instru_sansref, file = "data/ecran_avec_instru_sansref.rda")



# Analyse économetrique

## Recuperation de données

#Utilisation de la base de données ecran sans les instruments et sans la reference


load("data/ecran_sans_instru_sansref.rda")
ecran <- ecran_sans_instru_sansref
dim(ecran)



str(ecran)
View(ecran)



ecran[4:26] <- lapply(ecran[4:26], as.factor)
str(ecran)
summary(ecran)


# MCO

## Hypothèses sous-jacentes à la méthode des Moindres Carrés Ordinaires

# Modele lineaire

# Modele avec toutes les variables, ne sera pas utilisé dans notre analyse - voir méthode STEP


modele1 <- lm(temps_ecran ~ loisir + vie_sociale + cat_reseau_0_2 + cat_reseau_6_sup6 + cat_25_49 + cat_plus_50 + tel_inf_15 + tel_sup_25 + appareils_1_2 + appareils_plus_4 + abo_0 + abo_2 + abo_3_sup + homme + pas_ville + cat_sans_activite + cat_cadres + cat_autres + ecran_travail_bcp + ecran_travail_peu + pas_internet + internet_autre + nuit_inf_7 + jour_3 + jour_2, data=ecran)

summary(modele1)


# R = 38.39% de la variance dans temps_ecran est expliquée par le modèle. R- ajusté : 16,07

# P-value associée au test de Fisher \< 0,05 : H0 est refusée ==\> 0.040, le modèle, dans son ensemble, est statistiquement significatif, mais pas très fortement.


plot(modele1)



#D'apres les graphiques, le modele1

#Ne suivent pas a priori une loi normale

# Ne vérifie pas à priori l'hypothèse d'homoscédasticité car variance de la variable Mesure augmente à droite



# Test de normalité de residus

# Hypothèse H0 : les erreurs suivent une loi normale au seuil de risque de 5 %



qqnorm(residuals(modele1))

hist(residuals(modele1), breaks=30, main="Histogramme des Résidus", xlab="Résidus")

residus<-residuals(modele1) 

shapiro.test(residus)


ks.test(residus, "pnorm", mean = mean(residus), sd = sd(residus))



# p-value = 0,16 : hypothèse de normalité des résidus OK au seuil de risque de 5%

#### **Test d'homoscédasticité des résidus**
  
 # H0 = homoscedasticité


bptest(modele1)


# p-value = 0.986 \> 0,05 On ne rejette pas l'hypothèse nulle (à savoir l'homoscédasticité des résidus)

# Test de la linéarité de la forme fonctionnelle



reset(modele1)


# p-value = 0.3172

# p_value\> 0,05 forme fonctionnelle linéaire du modèle spécifié est acceptée au seuil de 5%

#### Analyse des observations influençant l'estimation


plot(cooks.distance(modele1),type="h")

# OU option avec le seuil 

cooks_d <- cooks.distance(modele1)
# Afficher le graphique des distances de Cook
plot(cooks_d, type="h", main="Distances de Cook")



#### Vérification de la multicolinéarité après estimation


vif(modele1)


# Pas de problème de multicolinéarité entre les différentes variables explicatives du modèle



# Step : Sélection des variables explicatives significatives (modèle linéaire = modele1 )


modele0 <- (lm(temps_ecran~1,data=ecran))
summary(modele0)


### Méthode ascendante



step(modele0, scope=list(lower=modele0, upper=modele1), data=ecran, direction="forward")

#AIC=513.47


### Méthode descendante



step(modele1, data=ecran,direction="backward")
# Step:  AIC=511.42


### Méthode double


#méthode dans les 2 sens
step(modele0, scope = list(upper=modele1),data=ecran,direction="both")
#Step:  AIC=513.47


# methode descendente 

modele_d <- lm(temps_ecran ~ vie_sociale + cat_reseau_6_sup6 + 
                 tel_sup_25 + appareils_1_2 + abo_0 + homme + pas_ville + 
                 cat_cadres + ecran_travail_bcp + ecran_travail_peu + internet_autre, data = ecran)

summary(modele_d)



# methode ascendante 

modele_a <-lm(temps_ecran ~ ecran_travail_peu + homme + cat_reseau_6_sup6 +  ecran_travail_bcp + internet_autre + vie_sociale, data = ecran)
summary(modele_a)




# méthode double
modele_double <- lm(temps_ecran ~ ecran_travail_peu + homme + cat_reseau_6_sup6 +  ecran_travail_bcp + internet_autre + vie_sociale, data = ecran)
summary(modele_double)


### Modele choisi = modele_d

###  Modele_d - Tests


residus3<-residuals(modele_d) 

ks.test(residus3, "pnorm", mean = mean(residus3), sd = sd(residus3))

reset(modele_d)

bptest(modele_d)

vif(modele_d)

plot(cooks.distance(modele_d), type = "h", main = "Distance de cook")


### Possible endogénéité de certaines variables explicatives


load("data/ecran_avec_instru_sansref.rda")
ecran_i <- ecran_avec_instru_sansref


# à partir base de données avec les instruments, nous allons choisir les variables que conforment le modele retenue ulterieurement, le modele_d



modele_di <- lm(temps_ecran ~ vie_sociale + cat_reseau_6_sup6 + 
                  tel_sup_25 + appareils_1_2 + abo_0 + homme + pas_ville + 
                  cat_cadres + ecran_travail_bcp + ecran_travail_peu + internet_autre, 
                data = ecran_i)
summary(modele_di)


# variables endogenes = + tel_sup_25 + appareils_1_2 + abo_0 + internet_autre + vie_sociale

# variables exogenes = cat_reseau_6_sup6 + homme + pas_ville + cat_cadres + ecran_travail_bcp + ecran_travail_peu

# instruments =stress_toujours + stress_bcp + tel_pro_non + cat_inf_20000 + cat_20_40000 + nb_personnes + fibre_non



# ivreg
reg_DMC <- ivreg(temps_ecran ~ vie_sociale + tel_sup_25 + appareils_1_2 + abo_0 + internet_autre + cat_reseau_6_sup6 + homme + pas_ville + cat_cadres + ecran_travail_bcp + ecran_travail_peu |  cat_reseau_6_sup6 + homme + pas_ville + cat_cadres + ecran_travail_bcp + ecran_travail_peu  +  tel_pro_non + cat_inf_20000 + cat_20_40000 + nb_personnes + fibre_non + stress_toujours + stress_bcp, data=ecran_i)

summary(reg_DMC, diagnostics = TRUE)

