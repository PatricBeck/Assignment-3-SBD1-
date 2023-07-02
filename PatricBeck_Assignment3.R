# Load the required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(rvest)
library(readr)
library(DT)
library(leaflet.extras)




# Scrape Data from the Web
scrape_worldPopulation <- function() {
  url <- "https://www.worldometers.info/world-population/population-by-country/#example2"
  page <- read_html(url)
  
  # Scrape the table with the world population data by country
  table <- html_table(html_nodes(page, "table#example2"), header = TRUE)[[1]]
  
  # Select all required columns
  table <- table[, c(2, 3, 10)]
  
  # Function to remove thousands separators from character strings
  removeCommas <- function(x) {
    gsub(",", "", x)
  }
  
  # Convert selected columns into numeric values
  colIndices <- c(2, 3)
  
  # Remove commas and convert to numeric values
  for (colIndex in colIndices) {
    table[[colIndex]] <- as.numeric(removeCommas(table[[colIndex]]))
  }
  
  # Change the name of the column
  colnames(table)[1] <- "country"
  colnames(table)[2] <- "Population"
  colnames(table)[3] <- "Med. Age"
  
  # Return the data
  table
}

# Function to receive the required geodata
getgeodata <- function() {
  # Source: https://www.kaggle.com/datasets/paultimothymooney/latitude-and-longitude-for-every-country-and-state
  # Import Geo data
  geodata <- read_csv("CountriesInTheWorld.csv")
  geodata <- geodata[, c("country", "latitude", "longitude")]
  # Return the data
  geodata
}





# User Interface
ui <- dashboardPage(
  dashboardHeader(title = "World Pupulation"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World map", tabName = "tab_worldmap"),
      menuItem("Evaluations", tabName = "tab_evaluations")
    )
  ),
  dashboardBody(
    tabItems(
      # Tab "World Map"
      tabItem(
        tabName = "tab_worldmap",
        fluidRow(
          box(
            title = "Selected countries",
            width = 6,
            pickerInput(
              inputId = "selected_countries",
              choices = NULL,
              options = list(`actions-box` = TRUE),
              multiple = TRUE
            )
          ),
          box(
            title = "Selected value",
            width = 6,
            selectInput(
              inputId = "selected_column",
              label = "Select column",
              choices = c("Population", "Med. Age")
            )
          )
        ),
        fluidRow(
          box(
            title = "World map",
            width = 12,
            leafletOutput("map")
          )
        ),
        fluidRow(
          box(
            title = "Number of countries",
            width = 6,
            valueBoxOutput("selected_countries_count", width = 12)
          ),
          box(
            title = "Mean value",
            width = 6,
            valueBoxOutput("average_value", width = 12)
          ),
          box(
            title = "Maximum value",
            width = 6,
            valueBoxOutput("max_value", width = 12)
          ),
          box(
            title = "Minimum value",
            width = 6,
            valueBoxOutput("min_value", width = 12)
          )
        )
      ),
    # Tab "Evaluations"
    tabItem(
      tabName = "tab_evaluations",
      fluidRow(
        box(
          title = "Selected value",
          width = 6,
          selectInput(
            inputId = "selected_column_heatmap",
            label = "Select column",
            choices = c("Population", "Med. Age")
          )
        )
      ),
      fluidRow(
        box(
          title = "Evaluations",
          width = 12,
          leafletOutput("heatmap")
        )
      ),
      fluidRow(
        box(
          title = "Top 10 values",
          width = 12,
          dataTableOutput("top_values_table")
          )
        )
      )
    )
  ),
  skin = "blue"
)





# Server
server <- function(input, output, session) {
  # Reactive function to scrape the needed data
  data <- reactive({
    table <- scrape_worldPopulation()
    geodata <- getgeodata()
    
    # Merge the tables based on the selection made in the column "country"
    merged_data <- merge(table, geodata, by = "country", all.x = TRUE)
    
    # Remove all the rows containing NA values
    merged_data <- na.omit(merged_data)
    
    # Return the merged data
    merged_data
  })
  
  # Update dropdown-menus based on the data
  observe({
    merged_data <- data()
    
    # Dropdown-menu to select countries [Tab "World Map"]
    updatePickerInput(
      session = session,
      inputId = "selected_countries",
      choices = unique(merged_data$country),
      selected = unique(merged_data$country)
    )
  })
  
  # Leaflet map [Tab "World Map"]
  output$map <- renderLeaflet({
    merged_data <- data()
    
    # Filter-Section "Selected Countries" (filter the Data beased on the selection)
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    # Section "World map" (build Leaflet map)
    leaflet() |>
      addTiles() |>
      addCircleMarkers(
        data = selected_data,
        lat = ~latitude,
        lng = ~longitude,
        label = ~paste0(input$selected_column, ": ", get(input$selected_column)),
        popup = ~paste0("<b>Country:</b> ", country, "<br>",
                        "<b>", input$selected_column, ":</b> ", get(input$selected_column)),
        color = "blueviolet",
        fillOpacity = 0.2
      )
  })
  
  # Section "Number of countries" (selected countries) [Tab "World Map"]
  output$selected_countries_count <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    valueBox(
      value = length(input$selected_countries),
      subtitle = "Selected countries",
      icon = icon("globe"),
      color = "aqua"
    )
  })
  
  # Section "Maximum Value" (incl. country name) [Tab "World Map"]
  output$max_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    max_value <- max(selected_data[[input$selected_column]], na.rm = TRUE)
    country_with_max_value <- selected_data$country[selected_data[[input$selected_column]] == max_value]
    
    valueBox(
      value = max_value,
      subtitle = paste("Maximum value (", country_with_max_value, ")"),
      icon = icon("arrow-up"),
      color = "olive"
    )
  })
  
  # Section "Mean Value" (incl. country name) [Tab "World Map"]
  output$average_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    valueBox(
      value = round(mean(selected_data[[input$selected_column]], na.rm = TRUE), 2),
      subtitle = "Mean value",
      icon = icon("calculator"),
      color = "light-blue"
    )
  })
  
  # Section "Minimum value" (incl. country name) [Tab "World Map"]
  output$min_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    
    min_value <- min(selected_data[[input$selected_column]], na.rm = TRUE)
    
    country_with_min_value <- selected_data$country[selected_data[[input$selected_column]] == min_value]
    
    valueBox(
      value = min_value,
      subtitle = paste("Minimum value (", country_with_min_value, ")"),
      icon = icon("arrow-down"),
      color = "teal"
    )
  })
  
  # Create the circles with the 2 variables (color and size) [Tab "Evaluations"]
  output$heatmap <- renderLeaflet({
    merged_data <- data()
    
    # Filter the data based on the selected column
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    # Create normalized values for color and sizes
    values <- selected_data[[input$selected_column_heatmap]]
    values <- na.omit(values)  # Remove missing values
    normalized_values <- scales::rescale(values, to = c(0.1, 4))
    
    # Set range of values for the colors
    min_value <- min(values, na.rm = TRUE)
    max_value <- max(values, na.rm = TRUE)
    
    # Define the color palette
    color_palette <- colorNumeric(
      palette = c("coral", "brown"),  # Change colors
      domain = c(min_value, max_value)
    )
    
    # Create Leaflet map
    leaflet(data = selected_data) |>
      addTiles() |>
      addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
        radius = ~sqrt(normalized_values) * 10,  # Adjust the size of the circles
        fillColor = ~color_palette(values),  # Customize the color of the circles
        color = "brown",
        fillOpacity = 0.8,
        stroke = FALSE,
        label = ~paste0(input$selected_column_heatmap, ": ", get(input$selected_column_heatmap))
      )
  })
  
  # Create a table with the top 10 values [Tab "Evaluations"]
  output$top_values_table <- DT::renderDataTable({
    merged_data <- data()
    
    # Sort selected columns in descending order.
    sorted_data <- merged_data[order(merged_data[[input$selected_column_heatmap]], decreasing = TRUE), ]
    
    # Only show the top 10 values
    top_values <- head(sorted_data, 10)
    
    # Create the object datatable 
    datatable(top_values, options = list(pageLength = 10))
  })
}





# Start shinyApp
shinyApp(ui, server)
