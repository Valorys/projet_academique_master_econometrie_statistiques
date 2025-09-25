
library(tsoutliers)
library(tseries)
library(moments)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(dplyr)
library(tidyr)
library(gets)
library(glmnet)
library(SIS)
library(readxl)
library(tidyverse)
library(caret)
library(foreach)
library(doParallel)
library(ggrepel)
library(lgarch)
library(randomForest)
library(ggplot2)
library(corrr)
library(dynlm)
library(imputeTS)
library(msaenet)
library(naniar)
library(mice)

dataCAN <- read.csv("balanced_can_md - Copie.csv")
vartotal <- read.csv("balanced_can_md.csv")
str(dataCAN)

summary(dataCAN)
print(dataCAN)

naniar::miss_var_summary(dataCAN)
print(naniar::miss_case_summary(dataCAN), n=50)

colSums(is.na(dataCAN))

#46 valeurs manquantes * 5

which(names(dataCAN) == "CRED_T_discontinued") #116
which(names(dataCAN) == "CRED_HOUS_discontinued") #117
which(names(dataCAN) == "CRED_MORT_discontinued") #118
which(names(dataCAN) == "CRED_CONS_discontinued") #119
which(names(dataCAN) == "CRE_BUS_discontinued") #120

dataCAN <- dataCAN[, -c(116, 117, 118, 119, 120)]

colSums(is.na(dataCAN))

dataCAN <- dataCAN[, -c(1)]
# Recherche points atypiques
y <- dataCAN[, 15]
yy <- ts(data = y, start=c(1981,01),frequency=12)
fit <- tso(yy)
fit
plot(fit)
show(fit)

# Série corrigé
adj <- fit$yadj

empl2 <- dataCAN
empl2$EMP_CAN <- as.numeric(adj)

#Vérifier la stationnarité  coefficient AR(1), test de racine unitaire de la variable dépendante
modelAR <- arima(adj,order=c(1,0,0))
adf.test(modelAR$residuals)
adf.test(adj)

plot(empl2$EMP_CAN, type = "l", col = "blue", 
     main = "Graphique du taux d'emploi en niveau",
     xlab = "Temps", ylab = "Taux d'emploi")


plot(adj, type = "l", col = "blue",
     +      main = "Graphique de la série ajustée",
     +      xlab = "Temps",
     +      ylab = "Taux d'emploi ajusté")


# Statistique descriptive
summary(empl2$EMP_CAN)
boxplot(empl2$EMP_CAN, main="Boxplot de la variable à expliquer")
mean(empl2$EMP_CAN)
sd(empl2$EMP_CAN)
var(empl2$EMP_CAN)
skewness(empl2$EMP_CAN)
kurtosis(empl2$EMP_CAN)

#ACP
# Exécuter l'ACP
var <- empl2[, -15]  # Exclusion de la variable 15
res.pca = PCA(var, ncp = 5)

# Identifier les variables les plus contributives
var_contrib <- get_pca_var(res.pca)$contrib
high_contrib_vars <- rownames(var_contrib)[apply(var_contrib[, 1:2], 1, function(x) sum(x)) > quantile(apply(var_contrib[, 1:2], 1, sum), 0.90)]

# Visualiser toutes les flèches et seulement les noms des plus contributives
# Étape 1: Graphique de base avec toutes les flèches
acp <- fviz_pca_var(res.pca, 
                     axes = c(1, 2), 
                     col.var = "cos2",   # Colorer les flèches selon cos2
                     label = "none"      # Ne pas afficher les labels pour toutes les variables
)+
  scale_color_gradient2(low = "#00AFBB", mid = "#E7B800", high = "#B40707", midpoint = 0.65, space = "Lab") 

# Étape 2: Ajouter les noms des variables les plus contributives
# Utiliser les coordonnées des variables pour ajouter les noms
coord <- get_pca_var(res.pca)$coord  # Extraire les coordonnées
contrib_vars <- rownames(get_pca_var(res.pca)) %in% high_contrib_vars

# Créer un dataframe pour les variables contributives
text_data <- data.frame(x = coord[high_contrib_vars, 1], 
                        y = coord[high_contrib_vars, 2], 
                        var = high_contrib_vars)
high_contrib_vars <- head(high_contrib_vars, 10)  # N'affiche que les 10 premières

# Ajouter les labels des variables contributives sur le graphique
acp + 
  geom_text_repel(data = text_data, 
                  aes(x = x, y = y, label = var), 
                  color = "black",  # Couleur des labels
                  size = 4)  

acp + 
  geom_text_repel(
    data = text_data, 
    aes(x = x, y = y, label = var), 
    color = "black",  # Couleur des labels
    size = 4) + 
  ggtitle("ACP sur les dimensions 1 et 2")  # Ajouter un titre au graphique


fviz_pca_var(res.pca, axes=c(2,3)) # représentation sur dim 2 et 3

#Dim 1 et 3 
### Dm 1 et 3 
# Étape 1: Graphique de base avec toutes les flèches pour les dimensions 1 et 3
acp <- fviz_pca_var(res.pca, 
                    axes = c(1, 3),    
                    col.var = "cos2", 
                    label = "none"    
) +
  scale_color_gradient2(low = "#00AFBB", mid = "#E7B800", high = "#B40707", 
                        midpoint = 0.65, space = "Lab") 

# Étape 2: Extraire les coordonnées et contributions
pca_var <- get_pca_var(res.pca)  
coord <- pca_var$coord  
contrib <- pca_var$contrib  

# Créer un dataframe pour les variables contribuant le plus
high_contrib_vars <- rownames(contrib)[order(-contrib[, 1])]  
high_contrib_vars <- head(high_contrib_vars, 10)  

# Créer un dataframe pour les coordonnées des variables contributives
text_data <- data.frame(x = coord[high_contrib_vars, 1], 
                        y = coord[high_contrib_vars, 3],  
                        var = high_contrib_vars)

# Ajouter les labels des variables contributives sur le graphique
acp + 
  geom_text_repel(data = text_data, 
                  aes(x = x, y = y, label = var), 
                  color = "black",  # Couleur des labels
                  size = 4, 
                  max.overlaps = Inf) + 
  ggtitle("ACP sur les dimensions 1 et 3")  


#kmeans
kmeans_result <- kmeans(res.pca$ind$coord[, 1:2], centers = 3)
clusters <- as.factor(kmeans_result$cluster)

fviz_pca_ind(res.pca,
             geom.ind = "point",
             col.ind = clusters,  
             palette = "jco", 
             addEllipses = TRUE,
             title = "Clustering k-means")

table(clusters) 

kmeans_result_var <- kmeans(res.pca$var$coord[, 1:2], centers = 3)
clusters_var <- as.factor(kmeans_result_var$cluster)

fviz_pca_var(res.pca,
             geom.var = "point",
             col.var = clusters_var,  
             palette = "jco", 
             addEllipses = TRUE,
             title = "Clustering k-means des variables explicatives")

table(clusters_var) 
variable_names <- colnames(empl2)[-15] 

results_df <- data.frame(
  Variable = variable_names,
  Cluster = clusters_var
)

print(results_df)



#Corrélation
# Charger le package ggplot2

# Nom de votre variable Y
target_variable <- "EMP_CAN"  # Remplacez par le nom de votre variable

# Calculer les coefficients de corrélation de Spearman entre Y et toutes les autres variables
correlations_spearman <- sapply(empl2, function(x) cor(empl2[[target_variable]], x, method = "spearman", use = "complete.obs"))

# Créer un DataFrame avec les noms des variables et les coefficients de corrélation
correlation_df_spearman <- data.frame(variable = names(correlations_spearman), correlation = correlations_spearman)

# Filtrer pour exclure Y de la visualisation
correlation_df_spearman <- correlation_df_spearman[correlation_df_spearman$variable != target_variable, ]

# Créer un graphique à barres des coefficients de corrélation
ggplot(correlation_df_spearman, aes(x = reorder(variable, correlation), y = correlation)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = paste("Coefficients de corrélation des variables explicative du taux d'emploi"),
       y = "Coefficient de Corrélation",
       x = "Variables") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) 

# 2. Créer des sous-ensembles de empl2 selon les variables des clusters
# Créer des data frames pour chaque cluster

variables_cluster1 <- results_df$Variable[results_df$Cluster == 1]
variables_cluster2 <- results_df$Variable[results_df$Cluster == 2]
variables_cluster3 <- results_df$Variable[results_df$Cluster == 3]

empl2_cluster1 <- empl2[, colnames(empl2) %in% variables_cluster1]
empl2_cluster2 <- empl2[, colnames(empl2) %in% variables_cluster2]
empl2_cluster3 <- empl2[, colnames(empl2) %in% variables_cluster3]

corrplot(empl2_cluster1, method = "color", type = "upper", tl.col = "black", tl.cex = 0.4)
corrplot(empl2_cluster2, method = "color", type = "upper", tl.col = "black", tl.cex = 0.4)
corrplot(empl2_cluster3, method = "color", type = "upper", tl.col = "black", tl.cex = 0.4)

# Calculer la matrice de corrélation pour le cluster 
correlation_matrix_cluster1 <- cor(empl2_cluster1, use = "complete.obs", method = "spearman") 
corrplot(correlation_matrix_cluster1, method = "color", type = "upper", tl.col = "black", tl.cex = 0.7,addCoef.col = "black", number.cex = 0.5 )

correlation_matrix_cluster2 <- cor(empl2_cluster2, use = "complete.obs", method = "spearman") 
corrplot(correlation_matrix_cluster2, method = "color", type = "upper", tl.col = "black", tl.cex = 0.7,addCoef.col = "black", number.cex = 0.4 )

correlation_matrix_cluster3 <- cor(empl2_cluster3, use = "complete.obs", method = "spearman") 
corrplot(correlation_matrix_cluster3, method = "color", type = "upper", tl.col = "black", tl.cex = 0.55 )


# Utiliser stats::lag()
empl_lag <- empl2 %>%
  mutate(
    across(everything(), lag, n = 1, .names = "{.col}_lag1"), 
    EMP_CAN_lag2 = lag(EMP_CAN, n = 2), 
    EMP_CAN_lag3 = lag(EMP_CAN, n = 3), 
    EMP_CAN_lag4 = lag(EMP_CAN, n = 4)  
  )

empl_lag <- drop_na(empl_lag) 

#GETS
empl_lag <- data.frame(empl_lag) # sinon erreur pour la suite
summary(empl_lag)
str(empl_lag)

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Penalized regressions

# standardized y and x (centered and standardized)
y <- data.frame(empl_lag) %>%
  select(EMP_CAN) %>%
  scale(center = T, scale = T) %>%
  as.matrix()

x <- data.frame(empl_lag) %>%
  select(-EMP_CAN) %>% # on retire la variable   expliquer y
  scale(center = T, scale = T) %>%
  as.matrix()

#GETS
model <- arx(y, mc = T, ar = 1, mxreg = x, vcov.type = "white") 
model
getsm <- getsm(model) 
getsm

# GETS betas
coef.arx(getsm)

# Get the name of relevant variables
names_mX <- names(coef.arx(getsm))
names_mX


#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Ridge regression
model_ridge <- glmnet(x, y, alpha = 0, standardize = T)
plot(model_ridge)

# 10-folds CV to choose lambda
# seq(-3, 5, length.out = 100) : random sequence of 100 numbers betwwene -3 and 5
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 0, implementation of ridge regression
# choix du meilleur lambda parmi 100
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 

# Figures of lambdas
plot(ridge_cv)

# Best lambda obtained from CV (lambda.min) - other possibility: lambda.1se
lambda_cv_ridge <- ridge_cv$lambda.min
lambda_cv_ridge

# Evaluation of the final model with the selected lambda
model_cv_ridge <- glmnet(x, y, alpha = 0, lambda = lambda_cv_ridge, standardize = T)
summary(model_cv_ridge)

# Ridge betas
model_cv_ridge$beta

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# LASSO regression
model_cv_lasso <- glmnet(x, y, alpha = 1, standardize = T)
plot(model_cv_lasso)

# 10-folds CV to choose lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 1, implementation of Lasso regression
lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100

# Figures of lambdas
plot(lasso_cv)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv_lasso <- lasso_cv$lambda.1se
lambda_cv_lasso

# Evaluation of the final model with the selected lambda
model_cv2_lasso <- glmnet(x, y, alpha = 1, lambda = lambda_cv_lasso, standardize = T)

# Lasso betas
model_cv2_lasso$beta

# Get the name of relevant variables
which(! coef(model_cv2_lasso) == 0, arr.ind = TRUE)


#---------------------------------------------------------------------
#---------------------------------------------------------------------
#aLasso
# Adaptive Lasso regression using Ridge to compute the weights in the first step
model_cv_alasso <- glmnet(x, y, alpha = 0, lambda = lambda_cv_ridge, standardize = T)
coef_ridge <- predict(model_cv_alasso,type="coef",s=lambda_cv_ridge)
# Weighted with gamma = 0.5
gamma = 0.5
w0 <- 1/(abs(coef_ridge) + (1/length(y)))
poids.ridge <- w0^(gamma)
poids.ridge <- poids.ridge[2:nrow(poids.ridge),]
poids.ridge <- Matrix(poids.ridge)

# Adaptive LASSO
fit_adalasso <- glmnet(x, y, penalty.factor =poids.ridge)
fit_cv_adalasso <- cv.glmnet(x, y,penalty.factor=poids.ridge)

# Figure of lambdas 
plot(fit_cv_adalasso)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv_alasso <- fit_cv_adalasso$lambda.1se
lambda_cv_alasso
# Evaluation of the final model with the selected lambda
model_cv_alasso <- glmnet(x, y, alpha = 1, lambda = lambda_cv_ridge, standardize = T)

# Lasso betas
model_cv_alasso$beta

# Get the name of relevant variables
which(! coef(model_cv_alasso) == 0, arr.ind = TRUE)



#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Elastic-Net regression with alpha = 0.5
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
en_cv <- cv.glmnet(x, y, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
# Figures of lambdas
plot(en_cv)
lambda_cv_en <- en_cv$lambda.1se
lambda_cv_en
model_cv_en <- glmnet(x, y, alpha = 0.5, lambda = lambda_cv_en, standardize = T)
# EN betas
model_cv_en$beta
# Get the name of relevant variables
which(! coef(model_cv_en) == 0, arr.ind = TRUE)

## Estimate Elastic-Net regression with alpha chosen from caret
#--------------------------------------------------------------------
base <- empl_lag[, c(15, setdiff(1:231, 15))]
# Rename variables (en Y and X1 à X21)
colnames(base)[1] = "Y"
for (i in 1:230) {
  colnames(base)[i+1] = paste("X", i, sep="")
}

# Transform into time-series format ts
data = ts(base[,1:231], start=c(1981,5), freq=12)
total = data
# Standardize variables
y = data.frame(total) %>% dplyr::select(Y) %>% scale(center=T, scale=T) %>% as.matrix()
x = data.frame(total) %>% dplyr::select(-Y) %>% scale(center=T, scale=T) %>% as.matrix()
total2 = data.frame(total) %>% scale(center=T, scale=T)

# Choose alpha with caret package
set.seed(1)	# for reproductability	
model <- train(Y~., data=total2, method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)

# Best tuning parameter
model$bestTune 
carret <- coef(model$finalModel, model$bestTune$lambda)
which(! carret == 0, arr.ind = TRUE)

# Choose alpha with caret package
set.seed(123)	# for reproductability	
model <- train(Y~., data=total2, method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)

# Best tuning parameter
model$bestTune 
coef(model$finalModel, model$bestTune$lambda)
carrat <- coef(model$finalModel, model$bestTune$lambda)
which(! carret == 0, arr.ind = TRUE)



## Estimate Elastic-Net regression with grid search
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Elastic-Net regression with alpha based on dopar - warning: depend on the order of seq((0.1, 0.9, 0.05)
# Choose alpha sequencially with 0 < alpha < 1: a = {0.1,0.2,...,0.9}
# standardized y and x (centered and standardized)
y <- data.frame(empl_lag) %>%
  select(EMP_CAN) %>%
  scale(center = T, scale = T) %>%
  as.matrix()

x <- data.frame(empl_lag) %>%
  select(-EMP_CAN) %>% # on retire la variable   expliquer y
  scale(center = T, scale = T) %>%
  as.matrix()

set.seed(123)	# for reproductability
en_min <- NULL
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
alphalist <- seq(0.1,by=0.1)
elasticnet <- lapply(alphalist, function(a){
  cv.glmnet(x, y, alpha=a, lambda = lambdas_to_try, standardize = T, nfolds = 10)
})
for (i in 1:9) {
  print(min(elasticnet[[i]]$cvm))
  en_min <- c(en_min, min(elasticnet[[i]]$cvm))
}
elasticnet_cvm <- min(en_min)
elasticnet_cvm

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
en_cv <- cv.glmnet(x, y, alpha = elasticnet_cvm, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
# Figures of lambdas
plot(en_cv)
lambda_cv_en_gs <- en_cv$lambda.1se
lambda_cv_en_gs
elasticnet_cvm
model_cv <- glmnet(x, y, alpha = elasticnet_cvm, lambda = lambda_cv_en_gs, standardize = T)
# EN betas
model_cv$beta
# Get the name of relevant variables
which(! coef(model_cv) == 0, arr.ind = TRUE) 


#---------------------------------------------------------------------
#---------------------------------------------------------------------
# SCAD
library(ncvreg)
set.seed(1)	# for reproductability
fit_SCAD=ncvreg(x, y, penalty = c("SCAD"))
plot(fit_SCAD)
summary(fit_SCAD, lambda=0.10)

# Validation crois  pour le meilleur lambda 
cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"))
plot(cvfit_SCAD)

# On attribue le meilleur lambda 
lambda_SCAD <- cvfit_SCAD$lambda.min
lambda_SCAD
#Modele finale 
SCAD_Final=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
SCAD_Final$beta

which(! coef(SCAD_Final) == 0, arr.ind = TRUE)


#---------------------------------------------------------------------
#---------------------------------------------------------------------
# SIS
mod01 <- SIS(x, y, family="gaussian", penalty="MCP", tune="cv", nfolds=10, nsis=100)

# indices selected by only SIS
var <- mod01$sis.ix0	
show(var)

noms_colonnes <- colnames(empl_lag)[var]
empl_lag_selection <- empl_lag[, noms_colonnes]

empl_lag_selection <- data.frame(empl_lag_selection) # sinon erreur pour la suite
summary(empl_lag_selection)
names(empl_lag_selection)
str(empl_lag_selection)

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Penalized regressions

# standardized y and x (centered and standardized)
y_sel <- data.frame(empl_lag_selection) %>%
  select(EMP_CAN) %>%
  scale(center = T, scale = T) %>%
  as.matrix()

x_sel <- data.frame(empl_lag_selection) %>%
  select(-EMP_CAN) %>% # on retire la variable   expliquer y
  scale(center = T, scale = T) %>%
  as.matrix()

#GETS
model_gets_sis <- arx(y_sel, mc = T, ar = 1, mxreg = x_sel, vcov.type = "white") 
model_gets_sis
getsm_gets_sis <- getsm(model_gets_sis) 
getsm_gets_sis

# GETS betas
coef.arx(getsm_gets_sis)

# Get the name of relevant variables
names_mX_gets_sis <- names(coef.arx(getsm_gets_sis))
names_mX_gets_sis

# If problem with diagnostic tests
# GETS modelling without ARCH test
getsm2 <- getsm(model_gets_sis, arch.LjungB=NULL) 
getsm2
# GETS betas
coef.arx(getsm2)
names_mX_getsm2_sis <- names(coef.arx(getsm2))
names_mX_getsm2_sis

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Ridge regression
model_cv_sis_ridge <- glmnet(x_sel, y_sel, alpha = 0, standardize = T)
plot(model_cv_sis_ridge)

# 10-folds CV to choose lambda
# seq(-3, 5, length.out = 100) : random sequence of 100 numbers betwwene -3 and 5
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 0, implementation of ridge regression
# choix du meilleur lambda parmi 100
ridge_cv_sis_ridge <- cv.glmnet(x_sel, y_sel, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 

# Figures of lambdas
plot(ridge_cv_sis_ridge)

# Best lambda obtained from CV (lambda.min) - other possibility: lambda.1se
lambda_cv_sis_ridge <- ridge_cv_sis_ridge$lambda.min
lambda_cv_sis_ridge

# Evaluation of the final model with the selected lambda
model_cv_sis_ridge <- glmnet(x_sel, y_sel, alpha = 0, lambda = lambda_cv_sis_ridge, standardize = T)
summary(model_cv_sis_ridge)

# Ridge betas
model_cv_sis_ridge$beta

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# LASSO regression
model_cv_sis_lasso <- glmnet(x_sel, y_sel, alpha = 1, standardize = T)
plot(model_cv_sis_lasso)

# 10-folds CV to choose lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 1, implementation of Lasso regression
lasso_cv_sis_lasso <- cv.glmnet(x_sel, y_sel, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100

# Figures of lambdas
plot(lasso_cv_sis_lasso)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv_sis_lasso <- lasso_cv_sis_lasso$lambda.1se
lambda_cv_sis_lasso

# Evaluation of the final model with the selected lambda
model_cv_sis_lasso <- glmnet(x_sel, y_sel, alpha = 1, lambda = lambda_cv_sis_lasso, standardize = T)

# Lasso betas
model_cv_sis_lasso$beta

# Get the name of relevant variables
which(! coef(model_cv_sis_lasso) == 0, arr.ind = TRUE)


#---------------------------------------------------------------------
#---------------------------------------------------------------------
#aLasso
# Adaptive Lasso regression using Lasso to compute the weights in the first step
model_cv_sis_alasso <- glmnet(x_sel, y_sel, alpha = 0, lambda = lambda_cv_sis_ridge, standardize = T)
coef_lasso_sis_alasso<-predict(model_cv_sis_alasso,type="coef",s=lambda_cv_sis_ridge)
# Weighted with gamma = 0.5
gamma=0.5
w0<-1/(abs(coef_lasso_sis_alasso) + (1/length(y)))
poids.lasso<-w0^(gamma)
poids.lasso <- poids.lasso[2:nrow(poids.lasso),]
poids.lasso <- Matrix(poids.lasso)

# Adaptive LASSO
fit_adalasso_sis_alasso <- glmnet(x_sel, y_sel, penalty.factor =poids.lasso)
fit_cv_adalasso_sis_alasso<-cv.glmnet(x_sel, y_sel,penalty.factor=poids.lasso)

# Figure of lambdas 
plot(fit_cv_adalasso_sis_alasso)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv_sis_alasso <- fit_cv_adalasso_sis_alasso$lambda.1se
lambda_cv_sis_alasso
# Evaluation of the final model with the selected lambda
model_cv_sis_alasso <- glmnet(x_sel, y_sel, alpha = 1, lambda = lambda_cv_sis_alasso, standardize = T)

# Lasso betas
model_cv_sis_alasso$beta

# Get the name of relevant variables
which(! coef(model_cv_sis_alasso) == 0, arr.ind = TRUE)

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Elastic-Net regression with alpha = 0.5
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
en_cv_sis <- cv.glmnet(x_sel, y_sel, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
# Figures of lambdas
plot(en_cv_sis)
lambda_cv_sis_en <- en_cv_sis$lambda.1se
lambda_cv_sis_en
model_cv_sis_en <- glmnet(x_sel, y_sel, alpha = 0.5, lambda = lambda_cv_sis_en, standardize = T)
# EN betas
model_cv_sis_en$beta
# Get the name of relevant variables
which(! coef(model_cv_sis_en) == 0, arr.ind = TRUE)

## Estimate Elastic-Net regression with alpha chosen from caret
library(readxl)
library(tidyverse)
library(glmnet) 	
library(caret)
library(foreach)
library(doParallel)

#--------------------------------------------------------------------
base <- empl_lag_selection[, c(7, setdiff(1:66, 7))]
# Rename variables (en Y and X1 à X21)
colnames(base)[1] = "Y"
for (i in 1:65) {
  colnames(base)[i+1] = paste("X", i, sep="")
}

# Transform into time-series format ts
data = ts(base[,1:66], start=c(1981,5), freq=12)
total = data
# Standardize variables
y = data.frame(total) %>% dplyr::select(Y) %>% scale(center=T, scale=T) %>% as.matrix()
x = data.frame(total) %>% dplyr::select(-Y) %>% scale(center=T, scale=T) %>% as.matrix()
total2 = data.frame(total) %>% scale(center=T, scale=T)

# Choose alpha with caret package
set.seed(1)	# for reproductability	
model <- train(Y~., data=total2, method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)

# Best tuning parameter
model$bestTune 
coef(model$finalModel, model$bestTune$lambda)
carret <- coef(model$finalModel, model$bestTune$lambda)
which(! carret == 0, arr.ind = TRUE)




## Estimate Elastic-Net regression with grid search
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Elastic-Net regression with alpha based on dopar - warning: depend on the order of seq((0.1, 0.9, 0.05)
# Choose alpha sequencially with 0 < alpha < 1: a = {0.1,0.2,...,0.9}
# standardized y and x (centered and standardized)
y_sel <- data.frame(empl_lag_selection) %>%
  select(EMP_CAN) %>%
  scale(center = T, scale = T) %>%
  as.matrix()

x_sel <- data.frame(empl_lag_selection) %>%
  select(-EMP_CAN) %>% # on retire la variable   expliquer y
  scale(center = T, scale = T) %>%
  as.matrix()

en_min_sis_gr <- NULL
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
alphalist_sis_gr <- seq(0.1,by=0.1)
elasticnet_sis_gr <- lapply(alphalist_sis_gr, function(a){
  cv.glmnet(x_sel, y_sel, alpha=a, lambda = lambdas_to_try, standardize = T, nfolds = 10)
})
for (i in 1:9) {
  print(min(elasticnet_sis_gr[[i]]$cvm))
  en_min_sis_gr <- c(en_min_sis_gr, min(elasticnet_sis_gr[[i]]$cvm))
}
elasticnet_cvm_sis_gr <- min(en_min_sis_gr)
elasticnet_cvm_sis_gr

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
en_cv_sis_gr <- cv.glmnet(x_sel, y_sel, alpha = elasticnet_cvm_sis_gr, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
# Figures of lambdas
plot(en_cv_sis_gr)
lambda_cv_sis_gr <- en_cv_sis_gr$lambda.1se
lambda_cv_sis_gr
elasticnet_cvm_sis_gr
model_cv_sis_gr <- glmnet(x_sel, y_sel, alpha = elasticnet_cvm_sis_gr, lambda = lambda_cv_sis_gr, standardize = T)
# EN betas
model_cv_sis_gr$beta
# Get the name of relevant variables
which(! coef(model_cv_sis_gr) == 0, arr.ind = TRUE)  


#---------------------------------------------------------------------
#---------------------------------------------------------------------
# SCAD
# standardized y and x (centered and standardized)

library(ncvreg)
set.seed(123)	# for reproductability
fit_SCAD_sis=ncvreg(x_sel, y_sel, penalty = c("SCAD"))
plot(fit_SCAD_sis)
summary(fit_SCAD_sis, lambda=0.10)

# Validation crois  pour le meilleur lambda 
cvfit_SCAD_sis=cv.ncvreg(x_sel, y_sel, penalty = c("SCAD"))
plot(cvfit_SCAD_sis)

# On attribue le meilleur lambda 
lambda_SCAD_sis <- cvfit_SCAD_sis$lambda.min
lambda_SCAD_sis
#Modele finale 
SCAD_Final_sis=ncvreg(x_sel, y_sel, lambda=lambda_SCAD_sis, alpha = 1)
SCAD_Final_sis$beta

which(! coef(SCAD_Final_sis) == 0, arr.ind = TRUE)

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Random forest sans sis 
rdf=randomForest(formula=EMP_CAN~.,data=empl_lag, 
                 ntree=1000, mtry=3, nodesize = 5, 
                 importance=TRUE)
print(rdf)
plot(rdf)
varImpPlot(rdf)
varImpPlot(rdf)
Imp=importance(rdf,scale=TRUE)
Imp
rdf$importance[order(rdf$importance[, 1], decreasing = TRUE), ]

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Random forest avec sis 
rdf_sis=randomForest(formula=EMP_CAN~.,data=empl_lag_selection, 
                 ntree=1000, mtry=3, nodesize = 5, 
                 importance=TRUE)
print(rdf_sis)
plot(rdf_sis)
varImpPlot(rdf_sis)
rdf_sis$importance[order(rdf_sis$importance[, 1], decreasing = TRUE), ]


