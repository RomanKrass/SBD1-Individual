# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(leaflet)
library(leaflet)
library(leaflet.extras)
library(tidygeocoder)
library(tidyverse)
library(viridis)
library(shinycssloaders)
library(rvest)

# Define the URL for the CSV file
filepath <- "https://raw.githubusercontent.com/sushantag9/Supermarket-Sales-Data-Analysis/master/supermarket_sales%20-%20Sheet1.csv"

# URL for the exchange rate
UrlER_USD_MMK <- "https://www.xe.com/de/currencyconverter/convert/?Amount=1&From=USD&To=MMK"
UrlER_USD_CHF <- "https://www.xe.com/de/currencyconverter/convert/?Amount=1&From=USD&To=CHF"
UrlER_USD_EUR <- "https://www.xe.com/de/currencyconverter/convert/?Amount=1&From=USD&To=EUR"

# Define the header
header <- dashboardHeader(
  title = "Supermarket Sales"
)

# Define the sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Charts", tabName = "charts"),
    menuItem("Map", tabName = "map"),
    menuItem("Exchange Rate", tabName = "exchange"),
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
      tabName = "map",
      tabBox(
        title = "Map",
        width = 12,
        tabPanel(
          "Map",
          "All store locations",
          leafletOutput("sales_map", height = 800) %>% withSpinner()
        )
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
          plotOutput("vis_city"),
          plotOutput("vis_city_date")
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
        ),
        tabPanel(
          "Reviews",
          inputPanel(
            selectInput("member_type2", "Select a member type",
              choices = "",
              selected = "Normal",
              multiple = TRUE
            )
          ),
          plotOutput("vis_ratings")
        ),
      )
    ),
    tabItem(
      tabName = "exchange",
      tabBox(
        title = "Exchange Rates",
        width = 12,
        tabPanel(
          "Current Exchange Rates",
          selectInput("exchange", "Select an exchange rate", choices = c("USD to MMK", "USD to CHF", "USD to EUR"), selected = "USD to MMK"),
          textOutput("exchangeRate_Text") %>% withSpinner()
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

  # Get geocode of the cities
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
      addResetMapButton() %>%
      addCircleMarkers(
        ~long, ~lat,
        label = ~City,
        radius = 20,
        color = "blue",
        fillOpacity = 0.8
      )
  })

  # Render income plot
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
      theme_minimal(base_size = 14) +
      scale_color_viridis(discrete = TRUE)
  })

  # Render plots by city
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
      theme_minimal(base_size = 14) +
      scale_color_viridis(discrete = TRUE, option = "C")
  })

  # Render plots by city and date
  output$vis_city_date <- renderPlot({
    sales_data() %>%
      filter(City %in% input$city) %>%
      mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
      arrange(Date) %>%
      group_by(City, Date) %>%
      summarise(Total = sum(Total), .groups = "drop") %>%
      ggplot(aes(x = Date, y = Total, color = City)) +
      geom_line() +
      labs(
        title = "Amount of sales per city",
        x = "City",
        y = "Total Sales"
      ) +
      scale_y_continuous() +
      theme_minimal(base_size = 14) +
      scale_color_viridis(discrete = TRUE)
  })

  # Render customised plot
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
        title = "Amount of sales per city with filters",
        x = "City",
        y = "Total Sales"
      ) +
      scale_y_continuous() +
      theme_minimal(base_size = 14)
  })

  # Render ratings plot
  output$vis_ratings <- renderPlot({
    sales_data() %>%
      filter(`Customer.type` %in% input$member_type2) %>%
      group_by(City) %>%
      summarise(Average_Rating = mean(Rating), .groups = "drop") %>%
      ggplot(aes(x = City, y = Average_Rating, fill = City)) +
      geom_col() +
      labs(
        title = "Average rating per city",
        x = "City",
        y = "Rating"
      ) +
      scale_y_continuous() +
      theme_minimal(base_size = 14) +
      scale_color_viridis(discrete = TRUE)
  })

  # Reactive expression to fetch exchange rate
  exchangeRate <- reactive({
    # Invalidate this reactive expression every 60 seconds to update the rate
    invalidateLater(60000, session)

    # Determine the URL based on the selected currency pair
    URLExchangeRate <- switch(input$exchange,
      "USD to MMK" = UrlER_USD_MMK,
      "USD to CHF" = UrlER_USD_CHF,
      "USD to EUR" = UrlER_USD_EUR,
      NULL
    )

    req(URLExchangeRate) # Ensure a URL is selected

    page <- rvest::read_html(URLExchangeRate)
    rate <- page %>%
      rvest::html_nodes(".sc-295edd9f-1") %>%
      rvest::html_text()

    # Return the fetched rate
    rate
  })

  # Output the exchange rate
  output$exchangeRate_Text <- renderText({
    rate <- exchangeRate()
    paste("1 USD dollar is currently: ", rate)
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

    # Update the selectInput with the unique payment methods
    updateSelectInput(session, "payment_methode2",
      choices = unique_payments,
      selected = "Ewallet"
    )

    # Update the selectInput with the unique cities
    updateSelectInput(session, "city",
      choices = unique(sales_data()$City),
      selected = "Yangon"
    )

    # Update the selectInput with the unique cities
    updateSelectInput(session, "city2",
      choices = unique(sales_data()$City),
      selected = "Yangon"
    )

    # Update the selectInput with the gender values
    updateSelectInput(session, "gender",
      choices = unique(sales_data()$Gender),
      selected = "Male"
    )

    # Update the selectInput with the product line values
    updateSelectInput(session, "product_line",
      choices = unique(sales_data()$Product.line),
      selected = "Health and beauty"
    )

    # Update the selectInput with the member type values
    updateSelectInput(session, "member_type",
      choices = unique(sales_data()$Customer.type),
      selected = "Normal"
    )

    # Update the selectInput with the member type values
    updateSelectInput(session, "member_type2",
      choices = unique(sales_data()$Customer.type),
      selected = "Normal"
    )
  })
}

# Run the application
shinyApp(ui, server)
