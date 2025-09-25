library(readxl)
library(smooth)
library(tsoutliers)
library(EnvStats)
library(seastests)
library(TSA)
library(forecast)
library(RJDemetra)
library(seasonal)
library(timeSeries)
library(forecast)
library(ggplot2)

# Import de la base
tourisme <- read_excel("donnée CA.xlsx")

str(tourisme)
summary(tourisme)

# Graphique de la série brute
plot(tourisme, type="l")

# Recherche points atypiques
y <- tourisme[,-c(1)]
yy <- ts(data = y, start=c(2018,08),frequency=12)
fit <- tso(yy)
fit
plot(fit)

# Série corrigé
adj <- fit$yadj

# Statistique descriptive
boxplot(adj)
str(adj)
mean(adj)
sd(adj)
var(adj)
adjstat <- as.numeric(adj)
skewness(adjstat)
kurtosis(adjstat)
summary(adj)

# Test saisonnalité 
# Seasonal dummies
sd_ts <- seasdum(adj)
show(sd_ts)

# Webel-Ollech test - new version of seastests (2021-09)
wot <- combined_test(adj)
show(wot)

#Détection de la saisonnalité
dyy <- diff(adj, differences = 1)

# méthode du periodogramme
par(mfrow=c(1,2))
periodogram(adj, main="Periodogramme sur la série en niveau")
periodogram(dyy, main="Periodogramme sur la série en différence première")
#permet de mettre en evidence la répétition de la série saisonnière

#schéma de détection
ggseasonplot(adj)
#modèle additif car les courbes sont paralèles

#Log-level
myregx13 <- regarima_x13(adj, spec ="RG5c")
s_transform(myregx13)
myregx13
summary(myregx13)

#Modification de la base de données
adj1 <- adj[1:52,]
adj1 <- ts(data = adj1, start=c(2018,08),frequency=12)
adj1
observe <- adj[53:64,]

#Desaisonnalité et decomposition 
myspec <- x13_spec("RSA5c")
mysax13 <- x13(adj1, myspec)
mysax13
summary(mysax13$regarima)
plot(mysax13$final)

#Estimation et prévision

# Naive
prev_naive_nons <- naive(adj1,h=12)
summary(prev_naive_nons)
MSE_naive_nons <- 18.23917^2

prev_naive <- snaive(adj1,h=12)
summary(prev_naive)
MSE <- 33.82888^2
MSE
plot(snaive(adj1,h=12))

#STL
decompstl <- stl(adj1, s.window="per")
prevstl <- forecast(decompstl, h = 12)
summary(prevstl)
plot(decompstl)
show(plot)
MSE_stl <- 6.003665^2
MSE_stl

#X13 ARIMA SEATS
myregx13 <- regarima_x13(adj1, spec ="RG5c")
summary(myregx13)
MSE_X13 <-7.071^2
MSE_X13
plot(myregx13)
# Forecast 
forex13 <- matrix(myregx13$forecast[1:12])
forex13

#STS
prev_sts <- StructTS(adj1)
prev_sts
summary(prev_sts)
prevsts <- forecast(prev_sts,12)
summary(prevsts)
show(prevsts)
MSE_sts <- 12.35809^2
MSE_sts
plot(prevsts)
AIC(prev_sts)
prevsts$fitted
prev_sts$loglik

k_sts <- length(prev_sts$coef)
n <- length(adj1)
AICc <- -2 * prev_sts$loglik + 2 * k_sts * (n / (n - 1 - 1))
AIC <- -2 * prev_sts$loglik + 2 * k_sts

#Bagged model
fitbag <- baggedModel(adj1)
fitbag$models
prevbag <- forecast(fitbag,12)
summary(prevbag)
MSE_bag <- 6.762608^2
MSE_bag
show(prevbag)
plot(prevbag)

log_likelihood <- sum(dnorm(fitbag$residual, mean = 0, sd = sd(fitbag$residuals), log = TRUE))
n <- length(adj1)
AICc_baggedmodel <- -2 * log_likelihood + 2 * 3 * (n / (n - 3 - 1))
AICc_baggedmodel
AIC_baggedmodel <- -2 * log_likelihood + 2 * 1
AIC_baggedmodel

#SARIMA(p,q,d)(P,D,Q)12
fitsarima <- auto.arima(adj1)
summary(fitsarima)
prevsarima <- forecast(fitsarima, 12)
prevsarima
summary(prevsarima)
plot(prevsarima)
MSE_sarima <- 6.122107^2
MSE_sarima
prevsarima


#Avec lissage exponentiel
HW <- HoltWinters(adj1)
summary(HW)
show(HW)
prevhw <- forecast(HW, 12)
summary(prevhw)
MSE_hw <- 8.232479^2
MSE_hw

residu <- fit$residuals
filtered_residu <- residu[!is.na(residu)]
log_likelihood <- sum(dnorm(filtered_residu, mean = 0, sd = sd(filtered_residu), log = TRUE))
n <- length(adj1)
AIChw <- -2 * log_likelihood + 2 * 1
AIChw
AICcHW <- -2 * log_likelihood + 2 * 1 * (n / (n - 1 - 1))
AICcHW

#ETS
fitets <- ets(adj1)
show(fitets)
plot(fitets)
# Forecasting 
prevets <- forecast(fitets,12)
summary(prevets)
MSE_ets <- 6.706359^2
MSE_ets
show(prevets)9

#TBATS
decomp <- tbats(adj1)
decomp
summary(decomp)
show(decomp)
plot(decomp)
# Forecasting
prev_tbats <- forecast(decomp,12)
summary(prev_tbats)
MSE_tbats <- 7.915939^2
MSE_tbats
show(prev_tbats)
decomp$AIC
#Calcul AICc
n <- length(adj1)
AICc <- 455.851 + (2 * 6 * (6 + 1) / (n - 6 - 1))

#ADAM ETS
fitadam1 <- auto.adam(adj1, model="ZZZ", lags=c(1,12), select=TRUE)
fitadam1
summary(fitadam1)
MSE_adamets <- 7.128^2
MSE_adamets
# Forecasting
prevadam1 <- forecast(fitadam1, h=12, level = 0.90)
summary(prevadam1)
prevadam1
plot(prevadam1)

#ADAM ETS SARIMA
fitadam2 <- auto.adam(adj1, model="ZZN", lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fitadam2
prevadam2 <- forecast(fitadam2, h=12, level = 0.90)
summary(fitadam2)
MSE_aes <- 0.0814^2
MSE_aes

#SSARIMA
fitssarima <- auto.ssarima(adj1, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fitssarima
prevssarima <- forecast(fitssarima, h=12, level = 0.90)
summary(fitssarima)
MSE_ssarima <- 12.2819^2
MSE_ssarima

#CES
fitces <- auto.ces(adj1, models=c("n","s","p","f"))
fitces
MSE_ces <- 6.183^2
MSE_ces
prevces <- forecast(fitces, h=12, level = 0.90)
summary(fitces)
summary(prevces)

#MSE
MSE
MSE_adamets <- 48.13584
MSE_aes
MSE_ces
MSE_ets
MSE_sarima
MSE_ssarima
MSE_stl
MSE_sts
MSE_tbats
MSE_X13
MSE_hw
MSE_naive_nons 
MSE_bag <- 46.3595

#R²OOS avec MSE naif en ref
R2OOS_naive <- 1-(MSE_naive_nons /MSE)
R2OOS_naive
R2OOS_adamets <- 1-(MSE_adamets/MSE)
R2OOS_adamets
R2OOS_aes <- 1-(MSE_aes/MSE)
R2OOS_aes
R2OOS_ces <- 1-(MSE_ces/MSE)
R2OOS_ces
R2OOS_ets <- 1-(MSE_ets/MSE)
R2OOS_ets
R2OOS_sarima <- 1-(MSE_sarima/MSE)
R2OOS_sarima
R2OOS_ssarima <- 1-(MSE_ssarima/MSE)
R2OOS_ssarima
R2OOS_stl <- 1-(MSE_stl/MSE)
R2OOS_stl
R2OOS_sts <- 1-(MSE_sts/MSE)
R2OOS_sts
R2OOS_tbats <- 1-(MSE_tbats/MSE)
R2OOS_tbats
R2OOS_X13 <- 1-(MSE_X13/MSE)
R2OOS_X13
R2OOS_HW <- 1-(MSE_hw/MSE)
R2OOS_HW
R2OOS_BM <- 1-(MSE_bag/MSE)
R2OOS_BM

# Convertir les objets forecast en data frames
prev_naive_df <- as.data.frame(prev_naive)
prev_X13_df <- as.data.frame(forex13)
prev_stl_df <- as.data.frame(prevstl)
prev_sts_df <- as.data.frame(prevsts)
prev_hw_df <- as.data.frame(prevhw)
prev_ets_df <- as.data.frame(prevets)
prev_adamets_df <- as.data.frame(prevadam1)
prev_a_e_s_df <- as.data.frame(prevadam2)
prev_ssarima_df <- as.data.frame(prevssarima)
prev_ces_df <- as.data.frame(prevces)
prev_sarima_df <- as.data.frame(prevsarima)
prev_tbats_df <- as.data.frame(prev_tbats)
prev_bag_df <- as.data.frame(prevbag)
prev_naive_nons_df <- as.data.frame(prev_naive_nons)


observe <- adj[53:64,]
prevnaive_nons_df <- prev_naive_nons_df[,1]
prevnaive_df <- prev_naive_df[,1]
prevX13_df <- prev_X13_df[,1]
prevstl_df <- prev_stl_df[,1]
prevsts_df <- prev_sts_df[,1]
prevhw_df <- prev_hw_df[,1]
prevets_df <- prev_ets_df[,1]
prevadamets_df <- prev_adamets_df[,1]
prevaes_df <- prev_a_e_s_df[,1]
prevssarima_df <- prev_ssarima_df[,1]
prevces_df <- prev_ces_df[,1]
prevsarima_df <- prev_sarima_df[,1]
prevtbats_df <- prev_tbats_df[,1]
prevtbag_df <- prev_bag_df[,1]
dates <- as.Date(c("2022-12-01", "2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01", "2023-05-01",
                   "2023-06-01", "2023-07-01", "2023-08-01", "2023-09-01", "2023-10-01", "2023-11-01"))

# Créer le ggseasonplot
ggplot() +
  geom_line(data = data.frame(Date = dates, Forecast = prevnaive_nons_df, Model = "Naive"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevnaive_df, Model = "Naive saisonnière"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevX13_df, Model = "X13-ARIMA-SEATS"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevstl_df, Model = "STL"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevsts_df, Model = "STS"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevhw_df, Model = "Holt"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevets_df, Model = "ETS"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevadamets_df, Model = "ADAM+ETS"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevaes_df, Model = "ADAM+ETS+SARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevssarima_df, Model = "SSARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevces_df, Model = "CES"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevsarima_df, Model = "SARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevtbats_df, Model = "TBATS"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = prevtbag_df, Model = "Bagged Model"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = observe, Model = "Observé"), aes(x = Date, y = Forecast, color = Model), size=1) +
  labs(x = "Date", y = "Forecast", color = "Model") +
  ggtitle("Seasonal Forecasts") +
  theme_minimal() +
  scale_color_manual(values = c("Observé" = "black", "Naive" = "blue", "X13-ARIMA-SEATS" = "red",
                                "STL" = "green", "STS" = "violet", "Holt" = "purple", 
                                "ETS" = "#ff9896", "ADAM+ETS" = "#7f7f7f", "ADAM+ETS+SARIMA" = "cyan",
                                "SSARIMA" = "#c7b43e", "CES" = "orange", "SARIMA" = "yellow",
                                "TBATS" = "maroon", "Bagged Model"="#4477aa")) + 
  theme(legend.position = "top")

ggplot() +
  geom_line(data = data.frame(Date = dates, Forecast = prevnaive_df, Model = "Naive"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevX13_df, Model = "X13-ARIMA-SEATS"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevstl_df, Model = "STL"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevsts_df, Model = "STS"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevhw_df, Model = "Holt"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevets_df, Model = "ETS"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevadamets_df, Model = "ADAM+ETS"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevaes_df, Model = "ADAM+ETS+SARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevssarima_df, Model = "SSARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevces_df, Model = "CES"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevsarima_df, Model = "SARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevX13_df, Model = "X13-ARIMA-SEATS"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevtbats_df, Model = "TBATS"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevtbag_df, Model = "Bagged Model"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = observe, Model = "Observé"), aes(x = Date, y = Forecast, color = Model), size=1) +
  labs(x = "Date", y = "Forecast", color = "Model") +
  ggtitle("Seasonal Forecasts") +
  theme_minimal() +
  scale_color_manual(values = c("Observé" = "black", "Naive" = "blue", "X13-ARIMA-SEATS" = "red",
                                "STL" = "green", "STS" = "violet", "Holt" = "purple", 
                                "ETS" = "#ff9896", "ADAM+ETS" = "#7f7f7f", "ADAM+ETS+SARIMA" = "cyan",
                                "SSARIMA" = "#c7b43e", "CES" = "orange", "SARIMA" = "yellow",
                                "TBATS" = "maroon", "Bagged Model"="#4477aa")) + 
  theme(legend.position = "top")


#CSPE
CSPE_sarima <- cumsum((prevsarima_df-observe)^2)
CSPE_bag <- cumsum((prevtbag_df-observe)^2)
CSPE_ces <- cumsum((prevces_df-observe)^2)
CSPE_ssarima <- cumsum((prevssarima_df-observe)^2)
CSPE_aes <- cumsum((prevaes_df-observe)^2)
CSPE_ae <- cumsum((prevadamets_df-observe)^2)
CSPE_ets <- cumsum((prevets_df-observe)^2)
CSPE_hw <- cumsum((prevhw_df-observe)^2)
CSPE_sts <- cumsum((prevsts_df-observe)^2)
CSPE_stl <- cumsum((prevstl_df-observe)^2)
CSPE_X13 <- cumsum((prevX13_df-observe)^2)
CSPE_naive <- cumsum((prevnaive_df-observe)^2)
CSPE_tbats <- cumsum((prevtbats_df-observe)^2)

ggplot() +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_sarima, Model = "SARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_bag, Model = "Bagged Model"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_ces, Model = "CES"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_ssarima, Model = "SSARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_aes, Model = "ADAM+ETS+SARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_ae, Model = "ADAM+ETS"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_ets, Model = "ETS"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_hw, Model = "Holt"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_sts, Model = "STS"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_stl, Model = "STL"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_X13, Model = "X13-ARIMA-SEATS"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_naive, Model = "Naive"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_tbats, Model = "Tbats"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  labs(x = "Date", y = "CSPE", color = "Model") +
  ggtitle("Seasonal Forecasts") +
  theme_minimal() +
  scale_color_manual(values = c( "Naive" = "blue", "X13-ARIMA-SEATS" = "red",
                                "STL" = "green", "STS" = "violet", "Holt" = "purple", 
                                "ETS" = "#ff9896", "ADAM+ETS" = "#7f7f7f", "ADAM+ETS+SARIMA" = "cyan",
                                "SSARIMA" = "#c7b43e", "CES" = "orange", "SARIMA" = "yellow",
                                "TBATS" = "maroon", "Bagged Model"="#4477aa")) + 
  theme(legend.position = "top")


ggplot() +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_sarima, Model = "SARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_bag, Model = "Bagged Model"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_ces, Model = "CES"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_ssarima, Model = "SSARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_aes, Model = "ADAM+ETS+SARIMA"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_ae, Model = "ADAM+ETS"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_ets, Model = "ETS"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_hw, Model = "Holt"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_sts, Model = "STS"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_stl, Model = "STL"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_X13, Model = "X13-ARIMA-SEATS"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_naive, Model = "Naive"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_tbats, Model = "TBATS"), aes(x = Date, y = Forecast, color = Model), size=0.5) +
  labs(x = "Date", y = "CSPE", color = "Model") +
  ggtitle("Seasonal Forecasts") +
  theme_minimal() +
  scale_color_manual(values = c( "Naive" = "blue", "X13-ARIMA-SEATS" = "red",
                                 "STL" = "green", "STS" = "violet", "Holt" = "purple", 
                                 "ETS" = "#ff9896", "ADAM+ETS" = "#7f7f7f", "ADAM+ETS+SARIMA" = "cyan",
                                 "SSARIMA" = "#c7b43e", "CES" = "orange", "SARIMA" = "yellow",
                                 "TBATS" = "maroon", "Bagged Model"="#4477aa")) + 
  theme(legend.position = "top")


# test DM
error_Naive <- prevnaive_df - observe
error_Tbats <- prevtbats_df - observe
error_x13 <- prevX13_df - observe
error_stl <- prevstl_df - observe
error_sts <- prevsts_df - observe
error_hw <- prevhw_df - observe
error_ets <- prevets_df - observe
error_AE <- prevadamets_df - observe
error_AES <- prevaes_df - observe
error_ssarima<- prevssarima_df - observe
error_ces <- prevces_df - observe
error_sarima <- prevsarima_df - observe
error_bag <- prevtbag_df - observe


dm.test(error_Naive,error_stl,h=12)
dm.test(error_Naive,error_sts,h=12)
dm.test(error_Naive,error_hw,h=12)
dm.test(error_Naive,error_ets,h=12)
dm.test(error_Naive,error_AE,h=12)
dm.test(error_Naive,error_ssarima,h=12)
dm.test(error_Naive,error_ces,h=12)
dm.test(error_Naive,error_bag,h=12)
dm.test(error_Naive,error_sarima,h=12)
dm.test(error_Naive,error_Tbats,h=12)
dm.test(error_Naive,error_x13,h=12)
dm.test(error_Naive,error_AES,h=12)


# Boucle 

#X13 ARIMA SEATS
estim <- 52
h <- 12
format <- matrix(nrow=h, ncol=1)
adj1 <- adj[1:52,]
adj1 <- ts(adj1, start=c(2018,08),frequency=12)
adj1
observe <- adj[53:64,]

for(i in 1:h) {
  adj2 <- adj1[i:(estim-1+i)]
  adj2 <- ts(adj2, start=c(2018, 8), frequency=12)
  myregx13 <- regarima_x13(adj2, spec ="RG5c") 
  forex13 <- matrix(myregx13$forecast[1])
  forc <- as.numeric(forex13)
  format[i, 1] <- forc
}
format

CSPE_X13 <- cumsum((prevX13_df-observe)^2)
CSPE_X13_2 <- cumsum((format-observe)^2)

ggplot() +
  geom_line(data = data.frame(Date = dates, Forecast = observe, Model = "Observé"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = format, Model = "X13-ARIMA-SEATS rolling"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = prevX13_df, Model = "X13-ARIMA-SEATS récursive"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  labs(x = "Date", y = "Forecast", color = "Model") +
  ggtitle("Seasonal Forecasts") +
  theme_minimal() +
  scale_color_manual(values = c("Observé" = "black", "X13-ARIMA-SEATS récursive" = "blue", "X13-ARIMA-SEATS rolling" = "red")) + 
  theme(legend.position = "top")

ggplot() +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_X13_2, Model = "X13-ARIMA-SEATS rolling"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  geom_line(data = data.frame(Date = dates, Forecast = CSPE_X13, Model = "X13-ARIMA-SEATS récursive"), aes(x = Date, y = Forecast, color = Model), size=0.6) +
  labs(x = "Date", y = "CSPE", color = "Model") +
  ggtitle("Seasonal Forecasts") +
  theme_minimal() +
  scale_color_manual(values = c("X13-ARIMA-SEATS récursive" = "blue", "X13-ARIMA-SEATS rolling" = "red")) + 
  theme(legend.position = "top")
