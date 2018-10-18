library(shiny)
library(shinydashboard)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(readxl)
library(stringr)
library(httr)
library(jsonlite)
library(plotly)

schools <- rgdal::readOGR("https://opendata.arcgis.com/datasets/70baf6da243e40298ba9246e9a67409b_0.geojson") %>%
  spTransform(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Define UI
  header <- dashboardHeader(title = "Los Angeles County...")
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Map", tabName = "Map", icon = icon("map-marker")),
      menuItem("Chart", tabName = "Chart", icon = icon("bar-chart")),
      menuItem("Table", tabName = "Table", icon = icon("table"))
    )
  )
  
  body<- dashboardBody(
  fluidRow(
    box(
      leafletOutput("schoolmap")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)
# Define server logic
server <- function(input, output) {
  output$schoolmap <- renderLeaflet({
    leaflet(schools) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

