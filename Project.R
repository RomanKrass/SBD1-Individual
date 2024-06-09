library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

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
        tabPanel("Map", "Map goes here")
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

  # Render the data table
  output$sales_table <- renderDT({
    datatable(sales_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
}

# Run the application
shinyApp(ui, server)
