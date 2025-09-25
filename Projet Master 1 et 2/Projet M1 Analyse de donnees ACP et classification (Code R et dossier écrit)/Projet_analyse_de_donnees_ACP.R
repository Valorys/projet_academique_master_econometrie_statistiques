#Les librairies utilisées
library(readxl) #Lire le excel
library(outliers)
library(EnvStats)
library(FactoMineR)#Faire l'ACP
library(moments) 
library(factoextra)
library(gridExtra) #faire tableau grid-table
library(RColorBrewer)
library(corrplot) #Faire les corrplot
library(lmtest)
library(forecast)
library(gtsummary)
library(Factoshiny)

Donnée<-read_xlsx(file.choose())
#Renommer les variables
colnames(Donnée)
names(Donnée)[names(Donnée) == "Dépenses publiques\r\n\r\n"] <- "Dépenses publiques"
names(Donnée)[names(Donnée) == "Infirmiers et personnels obstétriques\r\n\r\n"] <- "Infirmiers et personnels obstétriques"
names(Donnée)[names(Donnée) == "Espérance de vie à la naissance\r\n"] <- "Espérance"
names(Donnée)[names(Donnée) == "Eau\r\n"] <- "Eau"
colnames(Donnée)
View(Donnée)
#Nettoyer la base
BaseA<-na.omit(Donnée)
#Base sans la variable illustrative
base1<-BaseA[,4:15]
#Base avec la variable illustrative
base2<-BaseA[,3:15]
#Analyse univariée sous forme de tableau
summary(base2)
tbl_summary(base2,statistic = all_continuous() ~ "Moy = {mean} [Etendue = {max}-{min}] écart-type = {sd} Q1 = {p25} médiane={median} et Q3= {p75}")
quantile(base2)

#Graphiques de corrélation entre la variable illustrative et les variables actives
plot(BaseA$`Prévalence Hépatite B`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et Hépatite B",xlab="Hépatite B en %",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`Prévalence Hépatite B`),col="red",lwd=2)

plot(BaseA$`Anémie`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et Anémie",xlab="Anémie en %",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`Anémie`),col="red",lwd=2)

plot(BaseA$`Maigreur extrême`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et Maigreur extrême",xlab="Maigreur extrême en %",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`Maigreur extrême`),col="red",lwd=2)

plot(BaseA$`Recherche et santé`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et Recherche et santé",xlab="Recherche et santé",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`Recherche et santé`),col="red",lwd=2)

plot(BaseA$`Scolarisation`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et Scolarisation",xlab="Scolarisation",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`Scolarisation`),col="red",lwd=2)

plot(BaseA$`Vaccin DTC3`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et Vaccin DTC3",xlab="Vaccin DTC3",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`Vaccin DTC3`),col="red",lwd=2)

plot(BaseA$`PIB/hab`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et PIB/hab",xlab="PIB/hab",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`PIB/hab`),col="red",lwd=2)

plot(BaseA$`Eau`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et Eau",xlab="Eau",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`Eau`),col="red",lwd=2)

plot(BaseA$`Médecins`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et Médecins",xlab="Médecins",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`Médecins`),col="red",lwd=2)

plot(BaseA$`Infirmière et sage-femme`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et Infirmière et sage-femme",xlab="Infirmière et sage-femme",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`Infirmière et sage-femme`),col="red",lwd=2)

plot(BaseA$`Dépenses publiques`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et Dépenses publiques",xlab="Dépenses publiques",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`Dépenses publiques`),col="red",lwd=2)

plot(BaseA$`Espérance`,BaseA$Mortalité,type="p", main="Corrélation Mortalité et Espérance",xlab="Espérance",ylab="Mortalité infanto-juvénile", font.lab=3)
abline(lm(BaseA$Mortalité~BaseA$`Espérance`),col="red",lwd=2)

#Boxplot afin d’observer les valeurs atypiques
boxplot(base2$Mortalité, xlab ="Mortlité infanto-juvénile")
par(mfrow=c(3,4))
boxplot(base2$Médecins, xlab ="Médecins pour 10 000 hab")
boxplot(base2$`Maigreur extrême`, xlab ="Maigreur extême")
boxplot(base2$`PIB/hab`, xlab ="PIB/hab")
boxplot(base2$Anémie, xlab ="Anémie")
boxplot(base2$Scolarisation, xlab ="Scolarisation")
boxplot(base2$Eau, xlab ="Eau")
boxplot(base2$`Recherche et santé`, xlab ="Recherche et santé")
boxplot(base2$`Vaccin DTC3`, xlab ="Vaccin DTC3 (%)")
boxplot(base2$`Prévalence Hépatite B`, xlab ="Prévalence Hepatite B (%)")
boxplot(base2$Espérance, xlab ="Espérance de vie")
boxplot(base2$`Infirmière et sage-femme`, xlab ="Infirmière et sage-femme")
boxplot(base2$`Dépenses publiques`, xlab ="Dépense publiques")

#Test de Grubbs pour vérifier la présence d’une valeuratypique
grubbs.test(base2$Mortalité,type=10,two.sided=TRUE)
#108 est atypique => sierra leone 
grubbs.test(base2$Médecins,type=10,two.sided=TRUE)
#84,2 est atypique => Cuba
grubbs.test(base2$Eau,type=10,two.sided=TRUE)
#35,5 est atypique => Congo (démocratique) 
grubbs.test(base2$`Infirmière et sage-femme`,type=10,two.sided=TRUE)
#112,8 est atypique => Ouzbékistan => vérifier sur les bases de données d'origine
grubbs.test(base2$`Dépenses publiques`,type=10,two.sided=TRUE)
#25,2 est atypique => Costa Rica  => vérifier sur les bases de données d'origine

#Test de Rosner plus ouvert qui identifie jusqu’à dix valeurs atypiques
rosnerTest(base2$`Maigreur extrême`,k=2, alpha=0.05)
#21,5 et 17,3 sont atypiques => Djibouti et Inde
rosnerTest(base2$`PIB/hab`,k=2, alpha=0.05)
#Pas de valeur atypique
rosnerTest(base2$Scolarisation,k=5, alpha=0.05)
#Pas de valeur atypique 
rosnerTest(base2$`Recherche et santé`,k=5, alpha=0.05)
# 69,47 et 62,37 et 58,92 et 33,94 et 29,44 sont atypiques => Maldives, Tonga, Kiribati, Vanuata et Timor-Leste 
rosnerTest(base2$`Vaccin DTC3`,k=2, alpha=0.05)
#Pas de valeur atypique
rosnerTest(base2$`Prévalence Hépatite B`,k=4, alpha=0.05)
#6,07 est atypique => Guinée 

#Enlever les pays ayant des valeurs atypiques
BaseTypique<- Donnée[-c(50,17,14,44,15,18,25,33,57,28,60,55,24),]
#Base ayant ni les valeurs atypiques ni la variable illustrative
BaseTypique1 <- BaseTypique[,4:15]
#Base ayantla variable illustrative sans les valeurs atypiques 
BaseTypique2 <- BaseTypique[,3:15]

#Comparaison base avec et sans valeurs atypiques
skewness(base2$Mortalité)
kurtosis(base2$Mortalité)
skewness(BaseTypique2$Mortalité)
kurtosis(BaseTypique2$Mortalité)

#ACP
#Nous faisons l’ACP sur la base n’ayant pas la variable illustrative
res.pca=PCA(BaseA[,4:15])

#Valeurs propres
Valeurs_Propres<-round(res.pca$eig,2)
Valeurs_Propres
#Etude des variables
#Coordonnées des variables
Coordonnees_var<-round(res.pca$var$coord,2)
dim_coord<- Coordonnees_var[,1:3]
corrplot(dim_coord, is.corr=FALSE, method="circle",tl.srt=45, tl.col="#004400", col=brewer.pal(n=9,name="Spectral"),addCoef.col="black")
#Contribution des variables
par(mfrow=c(1,1))
Contributions_var<-round(res.pca$var$contrib,2)
dim_contrib<- Contributions_var[, 1:3]
corrplot(dim_contrib, is.corr=FALSE, method="circle",tl.srt=45, tl.col="#003300", col=brewer.pal(n=9,name="OrRd"),addCoef.col="black")
Contributions_var
#Cosinus 2 des vars
Cosinus2_Var=round(res.pca$var$cos2,2)
dim_cos2 <- Cosinus2_Var[, 1:3]
corrplot(dim_cos2, is.corr=FALSE, method="circle",tl.srt=45, tl.col="#004400", col=brewer.pal(n=9,name="PiYG"),addCoef.col="black")
Cosinus2_Var

res.pca=PCA(BaseA[,4:15])
plot.PCA(res.pca, axes=c(1,3),choix="var", title ="Cercle de corrélation Axe 1 et 3")

#Cercle de corrélation sur les axes 1 et 2
pca_12<-fviz_pca_var(res.pca,axes=c(1,2),col.var="cos2", repel = TRUE)+
  scale_color_gradient2(low="lightsteelblue3",mid="orange",
                        high="red",midpoint=0.65,space="Lab")
pca_12 + ggtitle("Cercle de corrélation Axe 1 et 2")+
  theme(plot.title = element_text(hjust = 0.5))
#Cercle de corrélation sur les axes 1 et 3
pca_13<-fviz_pca_var(res.pca,axes=c(1,3),col.var="cos2", repel = TRUE)+
  scale_color_gradient2(low="lightsteelblue3",mid="orange",
                        high="red",midpoint=0.65,space="Lab")
pca_13 + ggtitle("Cercle de corrélation Axe 1 et 3")+
  theme(plot.title = element_text(hjust = 0.5))
#Projection de la variable illustrative
#Cercle de corrélation avec la variable illustrative sur les axes 1 et 2
res.pca.I=PCA(BaseA[,3:15])
pca_12_I<-fviz_pca_var(res.pca.I,axes=c(1,2),col.var="cos2", col.quanti.sup = "purple4", repel = TRUE)+ scale_color_gradient2(low="lightsteelblue3",mid="orange", high="red",midpoint=0.65,space="Lab")
pca_12_I + ggtitle("Cercle de corrélation Axe 1 et 2 avec la mortalité")+
  theme(plot.title = element_text(hjust = 0.5))
#Cercle de corrélation avec la variable illustrative sur les axes 1 et 3
pca_13_I<-fviz_pca_var(res.pca.I,axes=c(1,3),col.var="cos2", col.quanti.sup = "purple4",repel = TRUE)+ scale_color_gradient2(low="lightsteelblue3",mid="orange",
                                                                                                                             high="red",midpoint=0.65,space="Lab")
pca_13_I + ggtitle("Cercle de corrélation Axe 1 et 3 avec la mortalité")+
  theme(plot.title = element_text(hjust = 0.5))
#Etude des individus
#Coordonnées des individus
Coordonnees_ind<-round(res.pca$ind$coord,2)
grid.table(Coordonnees_ind)
Coordonnees_ind
#Contribution des individus
Contribution_ind<-round(res.pca$ind$contrib,2)
grid.table(Contribution_ind)
Contribution_ind
#Cos2 des individus
Cosinus2_ind <-round(res.pca$ind$cos2,2)
grid.table(Cosinus2_ind)
Cosinus2_ind
#Autre méthode pour copier-coller les caractéristiques des individusdans Excel


PCAshiny(base1) #sans la variable illustrative

#Projection des individus les plus contributifs
#Axe 1 et 2
plot(res.pca, title= "Les 20 pays les plus contributifs", select="contrib 20", col.ind="red")
#Axe 1 et 3
plot(res.pca,axes=c(1,3), title= "Les 20 pays les plus contributifs", select="contrib 20",col.ind="red" )

#Régression du modèle axe 1 et 2
modele<- lm(base2$Mortalité~res.pca$ind$coord[,1]+res.pca$ind$coord[,2])
summary(modele)

#Régression du modèle axe 1 et 3
modele1<- lm(base2$Mortalité~res.pca$ind$coord[,1]+res.pca$ind$coord[,3])
summary(modele1)

#Résidus sur le modèle des axes 1 et 2
base2$residu=round(residuals(modele),2)
base2$residu
#Résidus sur le modèle des axes 1 et 3
base2$residu1=round(residuals(modele1),2)
base2$residu1
#Boxplot et histogramme résidus
par(mfrow=c(1,2))
boxplot(base2$residu, main = "Boxplot résidu axe 1 et 2")
hist(base2$residu, main="résidu", freq=TRUE, col="gray", cex.main =0.8)

rosnerTest(base2$residu ,k=4, alpha=0.05)
rosnerTest(base2$residu1 ,k=4, alpha=0.05)

#Skewness et kurtosis sur les résidus du 1er modèle
skewness(base2$residu)
kurtosis(base2$residu)
#Skewness et kurtosis sur les résidus du 2ème  modèle
skewness(base2$residu1)
kurtosis(base2$residu1)

#Test de normalité
ks.test(base2$residu,"pnorm",mean(base2$residu),sd(base2$residu))
ks.test(base2$residu1,"pnorm",mean(base2$residu1),sd(base2$residu1))
qqnorm(resid(modele))
qqline(resid(modele), col="red")

#homoscédasticité des erreurs
bptest(modele)
bptest(modele1)

#non corrélation des erreurs 
plot(modele)
plot(modele1)

#Pour l'axe 1 et 3
par(mfrow=c(1,3))
boxplot(base2$residu1, main = "Boxplot résidu axe 1 et 3")
hist(base2$residu1, main="résidu1", freq=TRUE, col="gray", cex.main =0.8)
qqnorm(resid(modele1))
qqline(resid(modele1), col="red")
ks.test(base2$residu1,"pnorm",mean(base2$residu1),sd(base2$residu1))
bptest(modele1)

#Modèle prédit vs observé axe 1 et 2
base2$predict=predict(modele)
round(base2$predict,2)
base2$Mortalité
plot(base2$predict,base2$Mortalité, col=0,main="Mortalité prédit par le modèle et Mortalité réel", abline(a=0, b=1, col="red"))
text(base2$predict,base2$Mortalité,row.names(base2), cex=.6)

#Modèle prédit vs observé axe 1 et 3
base2$predict1=predict(modele1)
round(base2$predict1,2)
base2$Mortalité
plot(base2$predict1,base2$Mortalité, col=0,main="Mortalité prédit par le modèle dans l'axe 1 et 3 et Mortalité réel", abline(a=0, b=1, col="red"))
text(base2$predict1,base2$Mortalité,row.names(base2), cex=.6)

#Logit médiane = 25
base2$MortalitéN <- ifelse(base2$Mortalité > 25, 1, 0)
base2$MortalitéN=as.numeric(base2$MortalitéN)
base2$MortalitéN
#Logit sur les axes 1 et 2
lmmodel=glm(base2$MortalitéN~res.pca$ind$coord[,1]+res.pca$ind$coord[,2],family=binomial)
summary(lmmodel)
#logit axe 1 et 3
lmmodel1=glm(base2$MortalitéN~res.pca$ind$coord[,1]+res.pca$ind$coord[,3],family=binomial)
summary(lmmodel1)
#Logit Q1 = 12,25
summary(base2)
base2$MortalitéN <- ifelse(base2$Mortalité > 12.25, 1, 0)
base2$MortalitéN=as.numeric(base2$MortalitéN)
base2$MortalitéN

lmmodel=glm(base2$MortalitéN~res.pca$ind$coord[,1]+res.pca$ind$coord[,2],family=binomial)
summary(lmmodel)

#logit axe 1 et 3
lmmodel1=glm(base2$MortalitéN~res.pca$ind$coord[,1]+res.pca$ind$coord[,3],family=binomial)
summary(lmmodel1)

#Logit seuil = 20
summary(base2)
base2$MortalitéN <- ifelse(base2$Mortalité > 20, 1, 0)
base2$MortalitéN=as.numeric(base2$MortalitéN)
base2$MortalitéN

lmmodel=glm(base2$MortalitéN~res.pca$ind$coord[,1]+res.pca$ind$coord[,2],family=binomial)
summary(lmmodel)

#logit axe 1 et 3
lmmodel1=glm(base2$MortalitéN~res.pca$ind$coord[,1]+res.pca$ind$coord[,3],family=binomial)
summary(lmmodel1)
