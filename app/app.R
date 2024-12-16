library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(shinyWidgets)

# Laden der vorbereiteten Daten
data <- readRDS("data.rds")

# Erstellen eines Vektors mit eindeutigen Datumswerten für den Slider
datum_vektor <- unique(data$Datum)
datum_vektor <- datum_vektor[order(datum_vektor)]  # Sortieren der Datumswerte

ui <- fluidPage(
  titlePanel("Inzidenzvorhersage nach Regionen"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "model",
        "Wählen Sie das Modell:",
        choices = c("Prophet", "Random Forest"),
        selected = "Prophet"
      ),
      selectInput(
        "disease",
        "Wählen Sie die Erkrankung:",
        choices = c("Akute Atemwegserkrankung (ARE)" = "ARE", 
                    "Akute grippeähnliche Erkrankung (ILI)" = "ILI"),
        selected = "ARE"
      ),
      sliderTextInput(
        "date",
        "Wählen Sie das Datum:",
        choices = format(datum_vektor, "%Y-%m-%d"),
        selected = format(datum_vektor[1], "%Y-%m-%d"),
        grid = FALSE
      ),
      tags$div(
        tags$h4("Info:", style = "margin-left: 10px;"),
        tags$p(
          HTML(
            "Diese Anwendung stellt zwei verschiedene Modelle zur Vorhersage regionaler Inzidenzen von Atemwegserkrankungen gegenüber:<br><br>
      
      <b>Prophet-Modell:</b> Ein zeitbasiertes Vorhersagemodell, das saisonale Muster und langfristige Trends berücksichtigt.<br>
      - Es wurde separat für jede <b>Region</b> und jede <b>Erkrankung</b> trainiert.<br>
      - Das Modell nutzt historische Daten, berücksichtigt eine wöchentliche und jährliche Saisonalität und sagt Inzidenzen bis zum Jahr <b>2030</b> voraus.<br>
      - Die Berechnungen basieren auf einem linearen Wachstumsmodell mit insgesamt <b>312 Wochen</b> Vorhersagezeitraum.<br><br>
      
      <b>Random Forest:</b> Ein leistungsstarkes Machine-Learning-Modell, das komplexe Zusammenhänge zwischen mehreren Merkmalen analysiert.<br>
      - Trainiert auf historischen Daten der Merkmale: <b>Jahr, Kalenderwoche, Region</b> und <b>Erkrankung</b>.<br>
      - Das Modell besteht aus <b>500 Entscheidungsbäumen</b> und berücksichtigt die wichtigsten Merkmale zur Vorhersage der Inzidenzen.<br>
      - Die Vorhersagen erfolgen wöchentlich bis zum Jahr <b>2030</b>.<br><br>

      Wählen Sie ein Modell, eine Erkrankung und ein Datum, um die regionalen Inzidenzen anzuzeigen.<br>
      <br><b>Quelle:</b> Robert Koch-Institut (RKI)"
          ),
          style = "padding-left: 20px; padding-right: 20px;"
        )
      )
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  # Filter die Daten basierend auf den Eingaben
  filtered_data <- reactive({
    model_column <- if (input$model == "Prophet") "Inzidenz_pr" else "Inzidenz_rf"
    
    selected_date <- as.Date(input$date)
    
    # Filtern der Daten und Berechnung der Inzidenz
    data %>%
      filter(Datum == selected_date, Erkrankung == input$disease) %>%
      mutate(Inzidenz = .[[model_column]]) %>%
      st_as_sf()  # Sicherstellen, dass es ein sf-Objekt bleibt
  })
  
  # Render die Karte
  output$map <- renderLeaflet({
    map_data <- filtered_data()
    
    # Feste Farbpalette von 0 bis 15000
    pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 15000))
    
    if (nrow(map_data) == 0) {
      leaflet() %>%
        addTiles() %>%
        addPopups(lng = 10.0, lat = 51.0, popup = "Keine Daten verfügbar")
    } else {
      leaflet(map_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(Inzidenz),  # Feste Farbpalette anwenden
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = ~paste(Region, ":", round(Inzidenz, 2)),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = c(0, 15000),  # Feste Werte für die Legende
          opacity = 0.7,
          title = "Inzidenz",
          position = "bottomright"
        )
    }
  })
}

shinyApp(ui = ui, server = server)


#rsconnect::deployApp(appName = "Inzidenz_Simulation")

