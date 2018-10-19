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

schools_list <- GET("https://maps.lacity.org/lahub/rest/services/LAUSD_Schools/MapServer/0/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=FULLNAME&returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=true&resultOffset=&resultRecordCount=&f=pjson")
list <- content(schools_list)
schoolsList <- fromJSON(list)$features

districts <- rgdal::readOGR("https://opendata.arcgis.com/datasets/de0e7b572e2b4c9ba3c1f62227b88a96_8.geojson")

art_grants <- read.socrata ("https://data.lacounty.gov/resource/ahzu-94ky.json")

community <- read.socrata("https://data.lacounty.gov/resource/gut7-6rmk.json")

# Define UI
  header <- dashboardHeader(title = "Los Angeles County Arts in Schools Programs")
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("LA County Schools & Districts", tabName = "Map", icon = icon("map-marker")),
      menuItem("Arts for All Grants", tabName = "Chart1", icon = icon("bar-chart")),
      menuItem("Community Arts Partners", tabName = "Chart2", icon = icon("bar-chart")),
      menuItem("Table", tabName = "Table", icon = icon("table"))
    )
  )
  
  body<- dashboardBody(
    tabItems(
      tabItem("Map",
  fluidRow(
    box(
      selectInput("school_select",
                  "School:",
                  choices = schoolsList,
                  multiple = TRUE,
                  selectize = TRUE),
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
    )
  ),
  fluidRow(
    box(width = 12,
      leafletOutput("schoolmap", height = 700)
    )
  )),
  tabItem("Chart1",
    fluidRow(
        box(
        selectInput("DistrictSelect",
                    "District:",
                    choices = sort(unique(art_grants$district)),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("East Whittier City Elementary", "Hacienda La Puente Unified")),
        actionButton("reset", "Reset Filters", icon = icon("refresh"))
        )
      ),
    fluidRow(
      box(
        title = "Arts for All Advancement Grants",
        width = 12,
        (plotlyOutput("ArtGrantsPlot", height = 600))
      )
    )
  ),
  tabItem("Chart2",
    fluidRow(
      box(
        selectizeInput("ComSchoolSelect",
                       "School:",
                       choices = sort(unique(community$school_name)),
                       multiple = TRUE,
                       options = list(maxItems = 10)),
        actionButton("reset", "Reset Filters", icon = icon("refresh"))
          
        )
      ),
    fluidRow(
      box(
        title = "Community Arts Partners serving LA County Public Schools",
        width = 12,
        (plotlyOutput("ComArtPlot", height = 600))
      )
    )
  ),
  tabItem("Table",
    fluidRow(
      box(
        selectInput("ComSchool2Select",
                    "School:",
                    choices = sort(unique(community$school_name)),
                    multiple = TRUE,
                    selectize = TRUE),
        actionButton("reset", "Reset Filters", icon = icon("refresh"))
    )
    ),
    fluidRow(
      box(width = 12,
        DT::dataTableOutput("table")
      )
    )
    
  )
)
)
ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  schoolsInput <- reactive({
    ifelse(length(input$school_select) > 0,
           paste0("FULLNAME+IN+%28%27", paste(input$school_select, collapse = "%27,%27"),"%27)"),
           "1=1")
    url <- paste("https://maps.lacity.org/lahub/rest/services/LAUSD_Schools/MapServer/0/query?where=", filter, "&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=pjson")
})
  
  output$schoolmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(data = districts) %>%
      addMarkers(data = schools, clusterOptions = markerClusterOptions()) %>%
      addPopups(schoolmap, popup = ~TOOLTIP)
  })
  output$ArtGrantsPlot <- renderPlotly({
    ggplot(data = art_grants, aes(x = district, y = award_amount, fill = cycle)) +
      geom_bar(stat = "identity") +
      labs(x = "District", y = "Award Amount")
  })
  
  output$ComArtPlot <- renderPlotly({
    ggplot(data = community, aes(x = school_name, y = enrollment, fill = "value", na.rm = TRUE)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
      labs(x = "School", y = "Enrollment") +
      theme(legend.position="none")
  })
  
  output$table <- DT::renderDataTable({
    (data = community)
  })

  observeEvent(input$reset, {
    updateSelectInput(session, "school_select")
    updateSelectInput(session, "DistrictSelect", selected = c("East Whittier City Elementary", "Hacienda La Puente Unified"))
    updateSelectizeInput(session, "ComSchoolSelect")
    updateSelectInput(session, "ComSchool2Select")
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
