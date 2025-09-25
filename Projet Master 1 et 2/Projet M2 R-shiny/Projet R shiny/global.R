library(shiny)
library(dplyr)
library(tidyr)
library(shinythemes)
library(rAmCharts)
library(colourpicker)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)
library(rsconnect)
library(DT)
library(shinyjs)
library(pipeR)
library(httr)
library(data.table)
library(ggplot2)
library(ggExtra)
library(plotly)
library(highcharter)
library(sf)
library(EnvStats)
library(outliers)
library(cartography)
library(spdep) 
library(corrplot) 
library(spatialreg)
library(car)
library(leaflet)


`%ni%` <- Negate(`%in%`)

load(file = "Projet.RData")

load("polyg_reg.RData")
polyg_reg <- polyg_reg[which((polyg_reg$reg_type == "région" & polyg_reg$reg_is_ctu == "Non" & polyg_reg$reg_name_up %ni% c("LA REUNION","GUADELOUPE")) | polyg_reg$reg_name_up == "CORSE"),]

polyg_reg$code_insee_region <- substr(polyg_reg$reg_cod, 3, 4)
polyg_reg$code_insee_region <- as.integer(polyg_reg$code_insee_region)

polyg_reg[is.na(polyg_reg)] <- 0

#Appel API

connectApiUrl <- paste("https://odre.opendatasoft.com/api/explore/v2.1/catalog/datasets/consommation-regionale-gnc/exports/csv?")
connectApiKey <- "ede2fbb1348a69a90d8fb517bd9c24699ecd75dbb41f6f6f425af9e1"
req = GET(connectApiUrl,
          accept_json(),
          progress(),
          add_headers('Authorization' = paste('Apikey ', connectApiKey, sep = "")),
          query = list("select" = "*"
                       # "where" = "cd_region = 52"
          ))

resp <- rawToChar(req$content)
basegaz <- fread(resp)

remove(req,connectApiKey, connectApiUrl, resp)

polyg_gaz <- merge(polyg_reg, basegaz, by = "code_insee_region")


connectApiUrl <- paste("https://odre.opendatasoft.com/api/explore/v2.1/catalog/datasets/prod-def-reg-jour-biom-reseau-grtgrd/exports/csv?")
connectApiKey <- "ede2fbb1348a69a90d8fb517bd9c24699ecd75dbb41f6f6f425af9e1"
req = GET(connectApiUrl,
          accept_json(),
          progress(),
          add_headers('Authorization' = paste('Apikey ', connectApiKey, sep = "")),
          query = list("select" = "*"
                       # "where" = "cd_region = 52"
          ))

resp <- rawToChar(req$content)
biomethane <- fread(resp)

connectApiUrl <- paste("https://odre.opendatasoft.com/api/explore/v2.1/catalog/datasets/production-annuelle-de-biomethane-par-site-raccorde-au-reseau-de-transport-et-de/exports/csv?")
connectApiKey <- "ede2fbb1348a69a90d8fb517bd9c24699ecd75dbb41f6f6f425af9e1"
req = GET(connectApiUrl,
          accept_json(),
          progress(),
          add_headers('Authorization' = paste('Apikey ', connectApiKey, sep = "")),
          query = list("select" = "*"
                       # "where" = "cd_region = 52"
          ))

resp <- rawToChar(req$content)
Prod_biomethane_lieu <- fread(resp)

save.image(file = "Projet.RData")


