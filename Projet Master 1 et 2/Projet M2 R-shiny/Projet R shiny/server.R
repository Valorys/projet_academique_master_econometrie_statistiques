library(shiny)
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    # Filtrer les données en fonction de l'année sélectionnée
    selected_data <- subset(polyg_gaz, annee == input$selected_year)
    
    # Carte
    leaflet(data = selected_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~colorNumeric("Reds", consommation_gwh_pcs)(consommation_gwh_pcs),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        popup = ~paste("Région: ", region, "<br>Consommation: ", round(consommation_gwh_pcs, 2), " GWh")
      ) %>%
      addLegend("bottomright", pal = colorNumeric("Reds", selected_data$consommation_gwh_pcs), 
                values = selected_data$consommation_gwh_pcs, 
                title = "Consommation de Gaz (GWh)", opacity = 1)
  })
  
  # Filtrer les données selon la région sélectionnée
  filtered_data <- reactive({
    if ("all" %in% input$region_select) {
      return(biomethane)
    } else {
      return(biomethane %>% filter(nom_region %in% input$region_select))
    }
  })
  
  # Graphique 1
  output$biomethane_plot <- renderPlotly({
    
    data <- filtered_data() %>%
      arrange(as.Date(date))  
    
    plot_ly(data, 
            x = ~date, 
            y = ~production_biomethane, 
            color = ~nom_region, 
            type = "scatter", 
            mode = "lines",  # Ligne continue
            hoverinfo = "text",
            text = ~paste("Région: ", nom_region, 
                          "<br>Date: ", date, 
                          "<br>Production: ", production_biomethane, " GWh")) %>%
      layout(title = "Évolution de la production de biométhane",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Production (GWh)"))
  })
  
  # Filtrer les données en fonction de la plage d'années sélectionnée
  filtered_data_histogram <- reactive({
    Prod_biomethane_lieu %>%
      filter(annee >= input$selected_years[1] & annee <= input$selected_years[2]) %>%
      group_by(region) %>%
      summarise(total_production = sum(production_de_biomethane_annuel_gwh_an, na.rm = TRUE))
  })
  
  # Histogramme
  output$biomethane_histogram <- renderPlotly({
    
    data <- filtered_data_histogram()
    
    p <- ggplot(data, aes(x = reorder(region, -total_production), y = total_production, fill = region)) +
      geom_bar(stat = "identity") +
      coord_flip() +  # Met les régions en axe vertical pour une meilleure lisibilité
      labs(title = "Production de biométhane par région",
           x = "Région",
           y = "Production totale (GWh)") +
      theme_minimal() +
      theme(legend.position = "none")  # Supprime la légende inutile
    
    ggplotly(p, tooltip = c("x", "y"))  # Active l'info-bulle sur les barres
  })
  
  
  
  filtered_data2 <- reactive({
    # Filtrer les données en fonction des sites sélectionnés
    data <- Prod_biomethane_lieu %>%
      select(annee, region, departement, commune, site, capacite_de_production_gwh_an, production_de_biomethane_annuel_gwh_an)
    
    # Appliquer le filtre sur le site sélectionné
    if (length(input$site_types) > 0) {
      data <- data %>% filter(site %in% input$site_types)  # Filtrer selon les sites sélectionnés
    }
    
    # Trier par année
    data <- data %>% arrange(desc(annee))
    
    return(data)
  })
  
  
  # Créer le tableau interactif
  output$biomethane_table <- DT::renderDT({
    filtered_data2() %>%
      DT::datatable(
        options = list(
          pageLength = 10,  
          autoWidth = TRUE, 
          searching = TRUE, 
          ordering = TRUE  
        )
      )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("biomethane_data_", Sys.Date(), ".csv", sep = "")  # Le nom du fichier avec la date actuelle
    },
    content = function(file) {
      write.csv(filtered_data2(), file, row.names = FALSE)  # Sauvegarder les données dans un fichier CSV
    }
  )
  
  # Commentaires réactifs
  output$commentaire_graphique1 <- renderText({
    paste("Vous avez sélectionné la (les) région(s) :", paste(input$region_select, collapse = ", "))
  })
  
  output$commentaire_graphique2 <- renderText({
    paste("Vous avez sélectionné la plage d'années de", input$selected_years[1], "à", input$selected_years[2])
  })
}