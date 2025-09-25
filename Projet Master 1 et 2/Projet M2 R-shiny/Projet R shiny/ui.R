library(shiny)
# UI : user Interface
ui <- fluidPage(
  
  # Include our custom CSS
  tags$head(
    includeCSS("www/styles.css")
  ),
  
  # Titre général
  h3("Master 2 ECAP - R Shiny - Valorys TRILLAUD - Analyse de la consommation de Gaz, biométhane en France"),
  
  # Barre de navigation
  navbarPage("",
             
             # Menu "Carte"
             navbarMenu("Carte Gaz",
                        tabPanel("Carte Gaz",
                                 h2("Carte de la consommation de Gaz Naturel Carburant par région et par année"),
                                 
                                 # Utilisation de fluidRow pour placer les éléments l'un sous l'autre
                                 fluidRow(
                                   column(12,  # Utilisation de toute la largeur pour le sélecteur d'année
                                          selectInput("selected_year", "Sélectionner une année :", 
                                                      choices = sort(unique(polyg_gaz$annee), decreasing = TRUE),  
                                                      selected = max(polyg_gaz$annee))
                                   )
                                 ),
                                 fluidRow(
                                   column(12,  # Utilisation de toute la largeur pour la carte
                                          leafletOutput("map")  # Carte affichée sous la sélection
                                   )
                                 )
                        )
             ),
             
             # Menu "Graphique"
             navbarMenu("Graphique",
                        tabPanel("Evolution du biométhane",
                                 h2("Evolution de la production du biométhane sur le réseau de distribution et de transport par région"),
                                 
                                 fluidRow(
                                   column(12,  # Utilisation de toute la largeur pour le sélecteur de région
                                          selectInput("region_select", 
                                                      "Sélectionner une région :", 
                                                      choices = unique(biomethane$nom_region),  # Liste des régions disponibles
                                                      selected = "Île-de-France",
                                                      multiple = TRUE)
                                   )
                                 ),
                                 fluidRow(
                                   column(12, 
                                          textOutput("commentaire_graphique1"))),
                                 fluidRow(
                                   column(12,  # Utilisation de toute la largeur pour le graphique
                                          plotlyOutput("biomethane_plot")
                                   )
                                 )
                        ),
                        tabPanel("Histogramme production",
                                 h2("Histogramme de la production de biométhane par région"),
                                 fluidRow(
                                   column(12,
                                          # Le sélecteur de plage d'années est placé en haut
                                          sliderInput("selected_years",
                                                      "Sélectionner une plage d'années :",
                                                      min = min(Prod_biomethane_lieu$annee), 
                                                      max = max(Prod_biomethane_lieu$annee),
                                                      value = c(min(Prod_biomethane_lieu$annee), max(Prod_biomethane_lieu$annee)), 
                                                      step = 1,
                                                      sep = "")
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          textOutput("commentaire_graphique2"))
                                 ),
                                 fluidRow(
                                   column(12,
                                          plotlyOutput("biomethane_histogram")  # Le graphique est en dessous
                                   )
                                 )
                        )),
             
             # Menu "Tableau"
             navbarMenu("Tableau",
                        tabPanel("Tableau biométhane",
                                 h2("Tableau des données de production de biométhane"),
                                 downloadButton("download_data", "Télécharger les données"),
                                 fluidRow(
                                   column(2,  # Cette colonne va contenir le checkbox
                                          checkboxGroupInput("site_types", 
                                                             "Filtrer par type de site", 
                                                             choices = unique(Prod_biomethane_lieu$site),  # Liste des sites uniques
                                                             selected = unique(Prod_biomethane_lieu$site)) 
                                   ),
                                   column(10,  # Cette colonne va contenir le tableau
                                          DT::DTOutput("biomethane_table")  # Afficher le tableau interactif ici
                                   )
                                 )
                        )
             )
  )
)


