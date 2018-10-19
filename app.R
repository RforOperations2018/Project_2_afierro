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

#Load data
schools <- rgdal::readOGR("https://opendata.arcgis.com/datasets/70baf6da243e40298ba9246e9a67409b_0.geojson") %>%
  spTransform(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

schools_list <- GET("https://maps.lacity.org/lahub/rest/services/LAUSD_Schools/MapServer/0/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=FULLNAME&returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=true&resultOffset=&resultRecordCount=&f=pjson")
#You did this, I don't know what it does
list <- content(schools_list)
schoolsList <- fromJSON(list)$features

districts <- rgdal::readOGR("https://opendata.arcgis.com/datasets/de0e7b572e2b4c9ba3c1f62227b88a96_8.geojson")

art_grants <- read.socrata ("https://data.lacounty.gov/resource/ahzu-94ky.json")

community <- read.socrata("https://data.lacounty.gov/resource/gut7-6rmk.json")

# Define UI
  header <- dashboardHeader(title = "Los Angeles County Arts in Schools Programs")

#Make Sidebar  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("LA County Schools & Districts", tabName = "Map", icon = icon("map-marker")),
      menuItem("Arts for All Grants", tabName = "Chart1", icon = icon("bar-chart")),
      menuItem("Community Arts Partners", tabName = "Chart2", icon = icon("bar-chart")),
      menuItem("Table", tabName = "Table", icon = icon("table"))
    )
  )

#Make body
  body<- dashboardBody(
    tabItems(
      
#Map tab
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

#Chart of arts for all grants & input
  tabItem("Chart1",
    fluidRow(
        box(
        selectizeInput("DistrictSelect",
                    "District:",
                    choices = sort(unique(art_grants$district)),
                    multiple = TRUE,
                    selected = c("East Whittier City Elementary", "Hacienda La Puente Unified"),
                    options = list(maxItems = 15)),
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

#Chart of community partners & input
  tabItem("Chart2",
    fluidRow(
      box(
        selectizeInput("ComSchoolSelect",
                       "School:",
                       choices = sort(unique(community$school_name)),
                       multiple = TRUE,
                       selected = c("Agua Dulce Elementary", "Martha Baldwin Elementary"),
                       options = list(maxItems = 15)),
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

#Data table of the community partners
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
server <- function(input, output, session = session) {

#This is the only input called with the API that works and you did it for me...so I don't think it counts
  schoolsInput <- reactive({
    filter <- ifelse(length(input$school_select) > 0,
                     gsub(" ", "+", paste0("FULLNAME+IN+%28%27", paste(input$school_select, collapse = "%27,%27"),"%27)")), # I added a gsub since most of the school names have spaces.
                     "1=1")
    # paste0() doesn't add spaces paste() does. We don't want spaces in our URLs
    url <- paste0("https://maps.lacity.org/lahub/rest/services/LAUSD_Schools/MapServer/0/query?where=", filter, "&text=&objectIds=&time=&geometry=&geometryType=esriGeometryPoint&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=pjson")
    # For Debugging
    print(url)
    # Make API Call
    dat <- fromJSON(url)$features %>%
      flatten()
    
    # Remove "attributes." and "geometry." from rownames
    colnames(dat) <- gsub("attributes.|geometry.", "", colnames(dat))

    # Return the dataframe when the function is called
    return(dat)
})
  
  output$schoolmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
  #No fill on polygons because changing the color of them all looked quite complicated
      addPolygons(data = districts, fillOpacity = 0) %>%
      addMarkers(data = schoolsInput(), lng = ~x, lat = ~y, clusterOptions = markerClusterOptions(), popup = schoolsInput()$TOOLTIP)
  })

#This attempt at trying to figure out the API thing was wrong
  # AwardAmountInput <- reactive({
  #   ifelse(length(input$DistrictSelect) > 0,
  #          (paste0("?where=school_district+IN+%28%27", paste(input$DistrictSelect, collapse = "%27,%27"), "%27",
  #          "")))
  #   url <- paste("https://data.lacounty.gov/resource/gut7-6rmk.json", filter)
  # })

#This attempt was wrong as well. There was no clear answer on how to do a SELECT DISTINCT type query thing in SoQL so I tried this with no luck.
  # AwardAmountInput <- reactive({
  #   read.socrata("https://data.lacounty.gov/resource/ahzu-94ky.json?$query=SELECTCOUNTDISTINCT(district)FROMhttps://data.lacounty.gov/resource/ahzu-94ky.json")
  # })
  
#Selecting Districts for Chart 1  
  AwardAmountInput <- reactive({
    DF <- art_grants
    # ORG Filter
    if (length(input$DistrictSelect) > 0 ) {
      DF <- subset(DF, district %in% input$DistrictSelect)
    }
    
    return(DF)
  })

#Chart 1
  output$ArtGrantsPlot <- renderPlotly({
    ggplot(data = AwardAmountInput(), aes(x = district, y = award_amount, fill = cycle)) +
      geom_bar(stat = "identity") +
      labs(x = "District", y = "Award Amount") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                     scientific = FALSE)) +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))
  })
  
#Selecting School for Chart 2
  CommunityInput <- reactive({
    DF <- community
    # ORG Filter
    if (length(input$ComSchoolSelect) > 0 ) {
      DF <- subset(DF, school_name %in% input$ComSchoolSelect)
    }
    
    return(DF)
  })

#Chart 2  
  output$ComArtPlot <- renderPlotly({
    ggplot(data = CommunityInput(), aes(x = school_name, y = enrollment, fill = "value", na.rm = TRUE)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
      labs(x = "School", y = "Enrollment") +
      theme(legend.position="none") 
  })
  
#Selecting school for data table 
  Community2Input <- reactive({
    DF <- community
    # ORG Filter
    if (length(input$ComSchool2Select) > 0 ) {
      DF <- subset(DF, school_name %in% input$ComSchool2Select)
    }
    
    return(DF)
  })
  
#Data Table
  output$table <- DT::renderDataTable({
    (data = Community2Input())
  })

#This should reset my inputs but it doesn't work for each action button. It only resets everything if you click the action reset button on the map tab...
  observeEvent(input$reset, {
    updateSelectInput(session, "school_select")
    updateSelectizeInput(session, "DistrictSelect", selected = c("East Whittier City Elementary", "Hacienda La Puente Unified"))
    updateSelectizeInput(session, "ComSchoolSelect", selected = c("Agua Dulce Elementary", "Martha Baldwin Elementary"))
    updateSelectInput(session, "ComSchool2Select")
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
