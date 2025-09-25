# IMPORTATION DES LIBRAIRIES

library(EnvStats)
library(corrplot)
library(tidyverse)
library(data.table)
library(ggplot2)
library(tidyr)
library(PerformanceAnalytics)
library(xts)
library(stargazer)


# IMPORTATION DES BASES DE DONNEES

AIS <- read.csv("AIS Mandarine.csv")
Amundi <- read.csv("Amundi CPR.csv")
ANDE <- read.csv("ANDE.csv")
Carmignac <- read.csv("Carmignac.csv")
Epsens <- read.csv("Epsens.csv")
Fidelity <- read.csv("Fidelity.csv")
Mainfirst <- read.csv("MainFirst.csv")
Pluvalca <- read.csv("Pluvalca.csv")
Thematics <- read.csv("Thematics.csv")

# ANALYSE DES TYPES DES DONNEES

str(AIS)
str(Amundi)
str(ANDE)
str(Carmignac)
str(Epsens)
str(Fidelity)
str(Mainfirst)
str(Pluvalca)
str(Thematics)

# SUPPRESSION DES COLONNES NON UTILES

AIS <- AIS[,c(1,6)]
Amundi <- Amundi[,c(1,6)]
ANDE <- ANDE[,c(1,6)]
Carmignac <-Carmignac[,c(1,6)]
Epsens <- Epsens[,c(1,6)]
Fidelity <- Fidelity[,c(1,6)]
Mainfirst <- Mainfirst[,c(1,6)]
Pluvalca <- Pluvalca[,c(1,6)]
Thematics <- Thematics[,c(1,6)]

# TRANFORMATION DES TYPES DES DONNEES

AIS$Adj.Close=as.numeric(AIS$Adj.Close)
AIS$Date=as.Date(AIS$Date)

Amundi$Adj.Close=as.numeric(Amundi$Adj.Close)
Amundi$Date=as.Date(Amundi$Date)

ANDE$Adj.Close=as.numeric(ANDE$Adj.Close)
ANDE$Date=as.Date(ANDE$Date)

Carmignac$Adj.Close=as.numeric(Carmignac$Adj.Close)
Carmignac$Date=as.Date(Carmignac$Date)

Epsens$Adj.Close=as.numeric(Epsens$Adj.Close)
Epsens$Date=as.Date(Epsens$Date)

Fidelity$Adj.Close=as.numeric(Fidelity$Adj.Close)
Fidelity$Date=as.Date(Fidelity$Date)

Mainfirst$Adj.Close=as.numeric(Mainfirst$Adj.Close)
Mainfirst$Date=as.Date(Mainfirst$Date)

Pluvalca$Adj.Close=as.numeric(Pluvalca$Adj.Close)
Pluvalca$Date=as.Date(Pluvalca$Date)

Thematics$Adj.Close=as.numeric(Thematics$Adj.Close)
Thematics$Date=as.Date(Thematics$Date)

# RENOMMAGE DES COLONNES ET SUPPRESSION DES VALEURS MANQUANTES

colnames(AIS) <- c("Date","AIS")
AIS <- na.omit(AIS)

colnames(Amundi) <- c("Date","AMU")
Amundi <- na.omit(Amundi)

colnames(ANDE) <- c("Date","ANDE")
ANDE <- na.omit(ANDE)

colnames(Carmignac) <- c("Date","CAR")
Carmignac <- na.omit(Carmignac)

colnames(Epsens) <- c("Date","EPS")
Epsens <- na.omit(Epsens)

colnames(Fidelity) <- c("Date","FID")
Fidelity <- na.omit(Fidelity)

colnames(Mainfirst) <- c("Date","MF")
Mainfirst <- na.omit(Mainfirst)

colnames(Pluvalca) <- c("Date","PLU")
Pluvalca <- na.omit(Pluvalca)

colnames(Thematics) <- c("Date","THE")
Thematics <- na.omit(Thematics)



# EXERCICE 1


# RESPRESENTATION DE L'EVOLUTION DES PRIX


AIS |> 
  pivot_longer(
    cols=c(2)
  ) |> 
  ggplot()+
  aes(x=Date,y=value,color="AIS")+
  labs(title="AIS Mandarine Global Transition", x = "Date en jours", y = "Prix en euros") +
  geom_line(color="darkblue")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 8)) +
  labs(subtitle = "Evolution des prix de fermeture du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance")

Amundi |> 
  pivot_longer(
    cols=c(2)
  ) |> 
  ggplot()+
  aes(x=Date,y=value,color="AMU")+
  labs(title="Amundi CPR Climate Action I", x = "Date en jours", y = "Prix en euros") +
  geom_line(color="darkblue")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 8)) +
  labs(subtitle = "Evolution des prix de fermeture du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance")

ANDE |> 
  pivot_longer(
    cols=c(2)
  ) |> 
  ggplot()+
  aes(x=Date,y=value,color="ANDE")+
  labs(title="The Andersons : (ANDE)", x = "Date en jours", y = "Prix en euros") +
  geom_line(color="darkblue")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 8)) +
  labs(subtitle = "Evolution des prix de fermeture du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance")

Carmignac |> 
  pivot_longer(
    cols=c(2)
  ) |> 
  ggplot()+
  aes(x=Date,y=value,color="CAR")+
  labs(title="Carmignac Emergents", x = "Date en jours", y = "Prix en euros") +
  geom_line(color="darkblue")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 8)) +
  labs(subtitle = "Evolution des prix de fermeture du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance")

Epsens |> 
  pivot_longer(
    cols=c(2)
  ) |> 
  ggplot()+
  aes(x=Date,y=value,color="EPS")+
  labs(title="Epsens EdR Tricolore Rendement", x = "Date en jours", y = "Prix en euros") +
  geom_line(color="darkblue")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 8)) +
  labs(subtitle = "Evolution des prix de fermeture du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance")

Fidelity |> 
  pivot_longer(
    cols=c(2)
  ) |> 
  ggplot()+
  aes(x=Date,y=value,color="FID")+
  labs(title="Fidelity Sustainable Water & Waste R Acc", x = "Date en jours", y = "Prix en euros") +
  geom_line(color="darkblue")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 8)) +
  labs(subtitle = "Evolution des prix de fermeture du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance")

Mainfirst |> 
  pivot_longer(
    cols=c(2)
  ) |> 
  ggplot()+
  aes(x=Date,y=value,color="MF")+
  labs(title="MainFirst Global Equities X", x = "Date en jours", y = "Prix en euros") +
  geom_line(color="darkblue")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 8)) +
  labs(subtitle = "Evolution des prix de fermeture du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance")

Pluvalca |> 
  pivot_longer(
    cols=c(2)
  ) |> 
  ggplot()+
  aes(x=Date,y=value,color="PLU")+
  labs(title="Pluvalca Sustainable Opportunities", x = "Date en jours", y = "Prix en euros") +
  geom_line(color="darkblue")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 8)) +
  labs(subtitle = "Evolution des prix de fermeture du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance")

Thematics |> 
  pivot_longer(
    cols=c(2)
  ) |> 
  ggplot()+
  aes(x=Date,y=value,color="THE")+
  labs(title="Thematics Water R/A USD", x = "Date en jours", y = "Prix en euros") +
  geom_line(color="darkblue")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 8)) +
  labs(subtitle = "Evolution des prix de fermeture du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance")


# CREATION D'UNE BDD AVEC TOUS LES FONDS

AIS <- pivot_longer(AIS,cols = -Date,names_to = "ticker",values_to = "price")
Amundi <- pivot_longer(Amundi,cols = -Date,names_to = "ticker",values_to = "price")
ANDE <- pivot_longer(ANDE,cols = -Date,names_to = "ticker",values_to = "price")
Carmignac <- pivot_longer(Carmignac,cols = -Date,names_to = "ticker",values_to = "price")
Epsens <- pivot_longer(Epsens,cols = -Date,names_to = "ticker",values_to = "price")
Fidelity <- pivot_longer(Fidelity,cols = -Date,names_to = "ticker",values_to = "price")
Mainfirst <- pivot_longer(Mainfirst,cols = -Date,names_to = "ticker",values_to = "price")
Pluvalca <- pivot_longer(Pluvalca,cols = -Date,names_to = "ticker",values_to = "price")
Thematics <- pivot_longer(Thematics,cols = -Date,names_to = "ticker",values_to = "price")

base <- rbind(AIS,Amundi,ANDE,Carmignac,Epsens,Fidelity,Mainfirst,Pluvalca,Thematics)
base <- as.data.table(base)
base[,idx_price := price/price[1],by=ticker]


# COMPARAISON DE L'EVOLUTION DES PRIX

ggplot(base, aes(x = Date, y = price, color = ticker)) +
  geom_line() +
  labs(x = "Date", y = "Prix", color = "Fonds") +
  facet_wrap(~ ticker, scales = "free_y", ncol = 3) +
  theme_bw() +
  ggtitle("Évolution des prix de chacun des 9 fonds ISR") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(subtitle = "du 1er janvier au 31 décembre 2023",
       caption = "Source des données : Yahoo Finance") +
  theme(plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))


# EVOLUTION DE L'INDICE DES PRIX

base |> 
  pivot_longer(
    cols=c(2)
  ) |> 
  ggplot()+
  aes(x = Date, y =idx_price, color = base$ticker)+
  labs(color="Actifs",y="Prix") +
  geom_line()+
  theme_light() +
  theme_bw() + ggtitle("Evolution de l'indice des prix de chacun des 9 fonds ISR") +
  xlab("Date") + ylab("Indice des prix") +
  scale_color_discrete(name = "fonds") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(subtitle = "du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance") +
  theme(plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))


# CALCUL ET REPRESENTATION DU RENDEMENT DE CHAQUE FONDS

# Calcul
base[,rendement := price / shift(price) - 1, by=ticker]

# Représentation sur 9 graphiques différents
ggplot(base, aes(x = Date, y = rendement, color = ticker)) +
  geom_line() +
  labs(x = "Date", y = "Rendements", color = "Fonds") +
  facet_wrap(~ ticker, scales = "free_y", ncol = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_bw() +
  ggtitle("Évolution des rendements de chacun des 9 fonds ISR") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(subtitle = "du 1er janvier au 31 décembre 2023",
       caption = "Source des données : Yahoo Finance") +
  theme(plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

# Représentation sur le même graphique
ggplot(base, aes(x = Date, y = rendement, color = ticker)) +
  geom_line() +
  theme_bw() + ggtitle("Evolution des rendements de chacun des 9 fonds ISR") +
  xlab("Date") + ylab("Indice des prix") +
  scale_color_discrete(name = "fonds") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(subtitle = "du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance") +
  theme(plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))


# DISTRIBUTION DES RENDEMENTS


ggplot(base, aes(x = ticker, y = rendement, fill = ticker)) +
  geom_boxplot() +
  labs(x = "Fonds financier", y = "Rendements", fill= "fonds") +
  theme_bw() + ggtitle("Distribution des rendements de chacun des 9 fonds ISR") +
   ylab("Rendements") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(subtitle = "du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance") +
  theme(plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

# DISTRIBUTION DES PRIX

ggplot(base, aes(y = price, color = ticker)) +
  geom_boxplot() +
  facet_wrap(~ticker, scales = "free_y", ncol = 3) +
  labs(title = "Distribution des prix des 9 fonds ISR", y = "Prix") +
  labs(subtitle = "du 1er janvier au 31 décembre 2023",
       caption = "Data source : Yahoo Finance") +
  theme(plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  theme(plot.title = element_text(hjust = 0.5))



# STATISTIQUES DESCRIPTIVES

# Rendements
base |> 
  group_by(ticker) |> 
  summarise(
    moyenne = mean(rendement, na.rm=TRUE),
    médiane = median(rendement, na.rm=TRUE),
    variance = var(rendement, na.rm=TRUE),
    écart_type = sd(rendement, na.rm=TRUE),
    kurtosis = kurtosis(rendement, na.rm=TRUE),
    skewness =skewness(rendement, na.rm=TRUE)
  )

# Prix
base |> 
  group_by(ticker) |> 
  summarise(
    moyenne = mean(price, na.rm=TRUE),
    médiane = median(price, na.rm=TRUE),
    variance = var(price, na.rm=TRUE),
    écart_type = sd(price, na.rm=TRUE),
    kurtosis = kurtosis(price, na.rm=TRUE),
    skewness =skewness(price, na.rm=TRUE)
  )


# Ratio de Sharpe

base<-na.omit(base)

base_bis <-base |> 
  select(ticker,rendement,Date)

base_bis_wide <- base_bis |> 
  pivot_wider(
    names_from = ticker,
    values_from = rendement
  )

data = na.omit(base_bis_wide)
dataxts <- xts(data[,-1], order.by = as.Date(data$Date))

rdm_moy <- tapply(base$rendement,base$ticker,mean)
rdm_sd <- tapply(base$rendement,base$ticker,sd)
r <- 0.062/252
ratio_sharpe<-round((rdm_moy-r)/rdm_sd,5)
ratio_sharpe


# MATRICE VARIANCE COVARIANCE

cov_var <- cov(dataxts,method="p")
stargazer(cov_var, type = "text", title = "Matrice de Variance-Covariance", header = TRUE, digits = 7)

# MATRICE DES CORRELATIONS

corrplot::corrplot(cor(dataxts,method="p"),method="circle",type="upper", tl.col="black",addCoef.col="black")


# CALCUL DES MESURES DE PERFORMANCE

# Ratio de Treynor
round(TreynorRatio(dataxts[,drop=FALSE],dataxts[,"AMU",drop=FALSE],Rf=0.062/252),5)

# Alpha de Jensen
round(SFM.jensenAlpha(dataxts[,drop=FALSE],dataxts$AMU[,drop=FALSE],Rf=0.062/252),5)

# Ratio de Sortino
round(SortinoRatio(dataxts, Rf = 0.062/252),5)

# Ratio de l'information
round(InformationRatio(dataxts[,drop=FALSE],dataxts$AMU[,drop=FALSE]),5)

# Ratio de Roy
moy<-mean(dataxts$AMU,na.rm=TRUE)
round(SharpeRatio(dataxts,moy,Rf=0.062/252),5)

# Ratio de Calmar
round(CalmarRatio(dataxts, scale = 252),5)

# Ratio de Sterling
round(SterlingRatio(dataxts, scale = 252),5)


# EXERCICE 2

# CREATION D'UNE BASE AVEC MOYENNE ET ECART TYPE DES RENDEMENTS DES 9 FONDS
base_port <- base |> group_by(ticker) |>  summarise(
    ret_moy = mean(rendement),ret_Sd =sd(rendement)
  )

# CREATION D'UNE BASE AVEC RENDEMENT ET VAR DU PORTEFEUILLE
tab <- base[,c(2,5)]
tab <- tab[, .(ret_moy = mean(rendement),
               ret_var = var(rendement)),
           by = "ticker"]


portefeuille <- tibble( "ret_moy"=mean(tab$ret_moy),
                        "ret_Sd"=sqrt(mean(tab$ret_var))
)
portefeuille

# BASE PORTEFEUILLE + 9 fonds
portefeuille[,"ticker"]="portefeuille"
portefeuille <- portefeuille[,c(3,1,2)]
base_port<- rbind(base_port,portefeuille)
base_port

# CREATION DE LA BASE DE DONNEES AVEC LES 3 FONDS AMU, MF ET ANDE
data=data.table(dataxts$AMU,dataxts$MF,dataxts$ANDE)
names(data) <- c("AMU","MF","ANDE")
View(data)


# MOYENNE REDEMENT
Ex <- mean(data$AMU)
Ey <- mean(data$MF)
Ez <- mean(data$ANDE)

# ECART TYPE
Sdx <- sd(data$AMU)
Sdy <- sd(data$MF)
Sdz <- sd(data$ANDE)

# COVARIANCE
Covxy <- cov(data$AMU, data$MF)
Covxz <- cov(data$AMU, data$ANDE)
Covyz <- cov(data$MF, data$ANDE)

# CREATION DU BASE AVEC LES POIDS DU PORTEFEUILLE poids
poids <- seq(from = 0, to = 1, length.out = 500)

# CREATION D'UNE BASE AVEC LES 3 ACTIFS
base_poids <- data.table(wx = rep(poids, each = length(poids)),
                         wy = rep(poids, length(poids)))
base_poids[, wz := 1 - wx - wy]

# CALCUL RENDEMENT ET VOLALITE POUR LES 1000 POIDS DE PORTEUILLES
base_poids[, ':=' (Ep = wx * Ex + wy * Ey + wz * Ez,
                   Sdp = sqrt(wx^2 * Sdx^2 +
                                wy^2 * Sdy^2 +
                                wz^2 * Sdz^2 +
                                2 * wx * wy * Covxy +
                                2 * wx * wz * Covxz +
                                2 * wy * wz * Covyz))]

# POIDS SUPERIEURE A 0 
base_poids <- base_poids[wx >= 0 & wy >= 0 & wz >= 0]

# GRAPHIQUE RENDEMENT/RISQUE : FONDS ET PORTEFEUILLE
base_port |> ggplot()+
  aes(x=ret_Sd,y=ret_moy,color=ticker)+
  geom_point(size=5)+xlab("Volatilité") + ylab("Rendement espéré")+
  theme_light()+
  scale_y_continuous(labels = scales::percent, limits = c(-0.0001, max(base_port$ret_moy) * 1.2)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.001, max(base_poids$Sdp) * 1.2))

# FRONTIERE EFFIECIENTE
base_poids |> ggplot() +
  geom_point(data = base_poids, aes(x = Sdp, y = Ep, color = wx - wz)) +
  geom_point(data = data.table(Sd = c(Sdx, Sdy, Sdz), E = c(Ex, Ey, Ez)),
             aes(x = Sd, y = E), color = "black", size = 3, shape = 18) +
  scale_color_gradientn(colors = c("#000080", "#2ecc71", "#ffd700"),
                        name = expression(omega[x] - omega[z]))+theme_light()+
  ggtitle("Frontière d'efficience du portefeuille composé des fonds AMU, MF, ANDE") +
  xlab("Volatilité") + ylab("Espérance de rentabilité") +
  scale_y_continuous(labels = scales::percent, limits = c(0.0001, max(base_poids$Ep) * 1.2)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.001, max(base_poids$Sdp) * 1.2)) 

# PORTEFEUILLE A VARIANCE MINIMALE
variance_min <- min(base_poids$Sdp^2)
portefeuille_variance_min <- base_poids[base_poids$Sdp^2 == variance_min, ]
portefeuille_variance_min

# PORTEFEUILLE TANGENT
assetSymbols <- c('AMU','MF','ANDE')
assetReturns <- data
assetReturns <- data.frame(assetReturns)
mu <- colMeans(assetReturns) 
cov.mat <- cov(assetReturns) 

assetsNames <- c('AMU','MF','ANDE')
E = c(Ex,Ey,Ez)

rf = 0.025/252
cov.mat <- cov(assetReturns) 

E <- as.vector(E)
cov.mat <- as.matrix(cov.mat)
cov.mat.inv <- solve(cov.mat)

w.t <- cov.mat.inv %*% (E - rf)
w.t <- as.vector(w.t/sum(w.t))

names(w.t) <- assetsNames
E.t <- crossprod(w.t,E)
Sd.t <- sqrt(t(w.t) %*% cov.mat %*% w.t)
Portefeuille_Tangent <- list("Weights" = w.t,
                 "ExpReturn" = as.vector(E.t),
                 "Risk" = as.vector(Sd.t))
Portefeuille_Tangent 

library(data.table)

# AJOUT DES POINTS DE PORTEFEUILLES A VARIANCE MINIMALE ET TANGENT 
base_poids |> ggplot() +
  geom_point(aes(x = Sdp, y = Ep, color = wx - wz)) +
  geom_point(data = data.table(Sd = c(Sdx, Sdy, Sdz), E = c(Ex, Ey, Ez)),
             aes(x = Sd, y = E), color = "black", size = 3, shape = 18) +
  scale_color_gradientn(colors = c("#000080", "#2ecc71", "#ffd700"),
                        name = expression(omega[x] - omega[z])) +
  geom_point(data = portefeuille_variance_min, aes(x = Sdp, y = Ep),
             color = "red", size = 3, shape = 16, show.legend=TRUE) +  # Ajouter le portefeuille à variance minimale
  geom_point(data = data.frame(Sd = Sd.t, E = E.t), aes(x = Sd, y = E),
             color = "blue", size = 3, shape = 16, show.legend=TRUE) +   # Ajouter le portefeuille tangent
  theme_light() +
  ggtitle("Frontière d'efficience avec le portefeuille à variance minimale et tangent") +
  xlab("Volatilité") + ylab("Espérance de rentabilité") +
  scale_y_continuous(labels = scales::percent, limits = c(0.0001, max(base_poids$Ep) * 1.2)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.001, max(base_poids$Sdp) * 1.2)) 

# REPRESENTATION DES 9 FONDS + DES 3 PORTEFEUILLES
base_port |>
  ggplot() +
  aes(x = ret_Sd, y = ret_moy, color = ticker) +
  geom_point(size = 5) +
  geom_point(data = portefeuille_variance_min, aes(x = Sdp, y = Ep, color = "Portefeuille à variance minimale"),
             size = 5, show.legend = TRUE) +
  geom_point(data = data.frame(Sd = Sd.t, E = E.t), aes(x = Sd, y = E, color = "Portefeuille tangent"),
             size = 5, show.legend = TRUE) +
  xlab("Volatilité") + ylab("Rendement espéré") +
  theme_light() +
  scale_y_continuous(labels = scales::percent, limits = c(-0.0001, max(base_port$ret_moy) * 1.2)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.001, max(base_poids$Sdp) * 1.2))