library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(leaflet)
library(tidygeocoder)
library(tidyverse)
library(viridis)

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
    menuItem("Inputs", tabName = "inputs"),
    menuItem("Charts", tabName = "charts")
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
    ),
    tabItem(
      tabName = "charts",
      tabBox(
        title = "Charts",
        width = 12,
        tabPanel(
          "By payment methode",
          inputPanel(
            selectInput("payment_methode", "Select a payment methode",
              choices = "",
              selected = "Ewallet",
              multiple = TRUE
            )
          ),
          plotOutput("vis_income")
        ),
        tabPanel(
          "By city",
          inputPanel(
            selectInput("city", "Select a city",
              choices = "",
              selected = "Yangon",
              multiple = TRUE
            )
          ),
          plotOutput("vis_city")
        ),
        tabPanel(
          "Custom",
          inputPanel(
            selectInput("city2", "Select a city",
              choices = "",
              selected = "Yangon",
              multiple = TRUE
            ),
            selectInput("payment_methode2", "Select a payment methode",
              choices = "",
              selected = "Ewallet",
              multiple = TRUE
            ),
            selectInput("gender", "Select a gender",
              choices = "",
              selected = "Male",
              multiple = TRUE
            ),
            selectInput("product_line", "Select a product line",
              choices = "",
              selected = "Health and beauty",
              multiple = TRUE
            ),
            selectInput("member_type", "Select a member type",
              choices = "",
              selected = "Normal",
              multiple = TRUE
            )
          ),
          plotOutput("custom")
        )
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
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addTiles() %>%
      addCircleMarkers(
        ~long, ~lat,
        label = ~City,
        radius = 20,
        color = "blue",
        fillOpacity = 0.8
      )
  })

  output$vis_income <- renderPlot({
    sales_data() %>%
      filter(Payment %in% input$payment_methode) %>%
      group_by(Payment) %>%
      summarise(Total = sum(Total)) %>%
      ggplot(aes(x = Payment, y = Total, fill = Payment)) +
      geom_col() +
      labs(
        title = "Amount of sales per payment method",
        x = "Payment Method",
        y = "Total Sales"
      ) +
      scale_y_continuous() +
      theme_minimal() +
      scale_color_viridis(discrete = TRUE)
  })

  output$vis_city <- renderPlot({
    sales_data() %>%
      filter(City %in% input$city) %>%
      group_by(City) %>%
      summarise(Total = sum(Total)) %>%
      ggplot(aes(x = City, y = Total, fill = City)) +
      geom_col() +
      labs(
        title = "Amount of sales per city",
        x = "City",
        y = "Total Sales"
      ) +
      scale_y_continuous() +
      theme_minimal() +
      scale_color_viridis(discrete = TRUE)
  })

  output$custom <- renderPlot({
    sales_data() %>%
      filter(
        City %in% input$city2,
        Payment %in% input$payment_methode2,
        Gender %in% input$gender,
        `Product.line` %in% input$product_line,
        `Customer.type` %in% input$member_type
      ) %>%
      group_by(City) %>%
      summarise(Total = sum(Total)) %>%
      ggplot(aes(x = City, y = Total, fill = City)) +
      geom_col() +
      labs(
        title = "Amount of sales per city",
        x = "City",
        y = "Total Sales"
      ) +
      scale_y_continuous() +
      theme_minimal()
  })

  # Observe changes in sales_data and update selectInput accordingly
  observe({
    # Ensure sales_data is available before trying to use it
    if (is.null(sales_data())) {
      return()
    }

    # Extract unique payment methods from the sales_data
    unique_payments <- unique(sales_data()$Payment)

    # Update the selectInput with the unique payment methods
    updateSelectInput(session, "payment_methode",
      choices = unique_payments,
      selected = "Ewallet"
    )

    updateSelectInput(session, "payment_methode2",
      choices = unique_payments,
      selected = "Ewallet"
    )

    updateSelectInput(session, "city",
      choices = unique(sales_data()$City),
      selected = "Yangon"
    )
    updateSelectInput(session, "city2",
      choices = unique(sales_data()$City),
      selected = "Yangon"
    )


    updateSelectInput(session, "gender",
      choices = unique(sales_data()$Gender),
      selected = "Male"
    )

    updateSelectInput(session, "product_line",
      choices = unique(sales_data()$Product.line),
      selected = "Health and beauty"
    )

    updateSelectInput(session, "member_type",
      choices = unique(sales_data()$Customer.type),
      selected = "Normal"
    )
  })
}

# Run the application
shinyApp(ui, server)
