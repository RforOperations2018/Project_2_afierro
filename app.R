library(shiny)
library(shinydashboard)
library(rgdal)
library(RSocrata)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(readxl)
library(stringr)
library(httr)
library(jsonlite)
library(ggplot2)
library(reshape2)
library(DT)
library(plotly)

schools <- rgdal::readOGR("https://opendata.arcgis.com/datasets/70baf6da243e40298ba9246e9a67409b_0.geojson") %>%
  spTransform(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

art_grants <- read.socrata ("https://data.lacounty.gov/resource/ahzu-94ky.json")

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
    tabItems(
      tabItem("Map",
  fluidRow(
    box(
      leafletOutput("schoolmap")
    )
  )),
  tabItem("Chart",
    fluidRow(
      box(
        title = "Arts Grants by District",
        width = 12,
        (plotlyOutput("ArtGrantsPlot"))
#DT::renderDataTable("table")
      )
    )
  )
))

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  output$schoolmap <- renderLeaflet({
    leaflet(schools) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers()
  })
  output$ArtGrantsPlot <- renderPlotly({
    ggplot(data = art_grants, aes(x = district, y = award_amount)) +
      geom_bar(stat = "identity")
  })
#  output$table <- DT::renderDataTable({
#    (art_grants)
#  })
}

# Run the application 
shinyApp(ui = ui, server = server)

