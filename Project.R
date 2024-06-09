library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(leaflet)
library(tidygeocoder)

# Define the URL for the CSV file
filepath <- "https://raw.githubusercontent.com/sushantag9/Supermarket-Sales-Data-Analysis/master/supermarket_sales%20-%20Sheet1.csv"

# Define the header
header <- dashboardHeader(
  title = "Supermarket Sales"
)

# Define the sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard"),
    menuItem("Map", tabName = "map"),
    menuItem("Inputs", tabName = "inputs")
  )
)

# Define the body
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "inputs",
      tabBox(
        title = "Input data",
        width = 12,
        tabPanel("Data Table", DTOutput("sales_table"))
      )
    ),
    tabItem(
      tabName = "dashboard",
      tabBox(
        title = "Dashboard",
        width = 12,
        tabPanel("Fun Facts 1", "Anything you want"),
        tabPanel("Fun Fact 2", "blabla")
      )
    ),
    tabItem(
      tabName = "map",
      tabBox(
        title = "Map",
        width = 12,
        tabPanel("Map", leafletOutput("sales_map", height = 800))
      )
    )
  )
)

# Define the UI
ui <- dashboardPage(
  header,
  sidebar,
  body
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive expression to fetch the data
  sales_data <- reactive({
    read.csv(url(filepath))
  })

  # Geocode cities
  geocoded_data <- reactive({
    data <- sales_data() %>%
      distinct(City)
    geo_data <- data %>%
      geocode(City, method = "osm")
    geo_data
  })

  # Render the data table
  output$sales_table <- renderDT({
    datatable(sales_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })

  # Render the leaflet map
  output$sales_map <- renderLeaflet({
    geo_data <- geocoded_data()

    leaflet(geo_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~long, ~lat,
        label = ~City,
        radius = 5,
        color = "blue",
        fillOpacity = 0.5
      )
  })
}

# Run the application
shinyApp(ui, server)
