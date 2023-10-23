# Load necessary libraries
library(shiny)
library(leaflet)
library(dygraphs)
library(dplyr)
library(lubridate)
library(DT)
library(plotly)
library(ggplot2)

data <- read.csv('new.csv')
#colnames(data)
# Edit the date format to remove the time portion
data$ACCIDENT.DATE <- sub("T.*", "", data$ACCIDENT.DATE)

# Combine date and time into a single DateTime column with year, month, day, hour, and minute
data$DateTime <- as.POSIXct(paste(data$ACCIDENT.DATE, data$ACCIDENT.TIME), format = "%Y-%m-%d %H:%M", tz = "UTC")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #raw_data {
        font-size: 10px; /* Adjust the font size as needed */
      }
      table.dataTable {
        background-color: #f2f2f2; /* Set your desired background color */
      }
         .nav.nav-tabs > li.active > a {
        background-color: lightgreen !important; /* Active tab background color */
        color: darkblue !important; /* Active tab text color */
      }
      .nav.nav-tabs > li > a {
        background-color: #f8f9fa !important; /* Inactive tab background color */
        color: #333 !important; /* Inactive tab text color */
      }
       .sidebar {
        background-color: red; /* Sidebar background color */
        color: #fff; /* Sidebar text color */
      }
      .well {
        background-color: lightcyan; /* Sidebar well background color */
      }
      .btn-primary {
        background-color: #007BFF; /* Primary button background color */
        border-color: red; /* Primary button border color */
      }
       .slider-label {
        font-weight: bold; /* Make slider labels bold */
      }
    ")),
    tags$style(HTML("
          #streets_by_deaths {
            background-color: #F2F2F2; /* Set your desired background color */
            font-size: 14px; /* Adjust the font size as needed */
            border: 1px solid #000; /* Add a border to the table */
            border-collapse: collapse; /* Collapse borders */
          }
          #streets_by_death th {
            background-color: #FF5733; /* Set the header background color to darkred */
            color: darkred; /* Set the header text color to darkred */
          }
          #streets_by_death td {
            color: darkred; /* Set the data text color to darkred */
          }
        "))
    
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Your Own Data"),
      sliderInput("injured_people", "Number of persons injured in vehicle collisions", 0, 8, 0),
      sliderInput("hour", "Select Hour", min = 0, max = 23, value = 0),
      selectInput("affected_class", "Death Victim Type (Top Fatal Steets)", 
                  choices = c("Dead Motorists", "Dead Pedestrians", "Dead Cyclists")),
      selectInput("affected_injured_class", "Injured Victim Type (Riskiest Steets)", 
                  choices = c("Motorists Injured", "Pedestrians Injured", "Cyclists Injured"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Street Map", leafletOutput("map")),
        tabPanel("Histogram", plotlyOutput("histogram")),
        tabPanel("Riskiest Streets", dataTableOutput("dangerous_streets")),
        tabPanel("Top Fatal Streets", dataTableOutput("streets_by_deaths")),
        tabPanel("Vehicle Type In Accidents", plotlyOutput("vehicle_type_breakdown")),
        tabPanel("Causes of Accidents", plotlyOutput("contributing_factors")),
        tabPanel("Data Overview", dataTableOutput("raw_data")),
        tabPanel("Hourly Map", leafletOutput("hourly_map"),
                 uiOutput("hourly_map_title")), # In the UI part of your Shiny app
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  data <- reactive({
    if (is.null(input$file)) {
      # Load default data if no file is uploaded
      data <- read.csv("new.csv")
    } else {
      data <- read.csv(input$file$datapath)
    }
    # Process ACCIDENT.DATE and ACCIDENT.TIME columns
    data$ACCIDENT.DATE <- sub("T.*", "", data$ACCIDENT.DATE)
    # Combine date and time into a single DateTime column with year, month, day, hour, and minute
    data$DateTime <- as.POSIXct(paste(data$ACCIDENT.DATE, data$ACCIDENT.TIME), format = "%Y-%m-%d %H:%M", tz = "UTC")
    return(data)
  })
  
  output$map <- renderLeaflet({
    data_filtered <- data()
    data_filtered <- data_filtered[data_filtered$`NUMBER.OF.PERSONS.INJURED` >= input$injured_people, ]
    
    # Create a Leaflet map with a custom tileset
    leaflet(data_filtered) %>%
      addProviderTiles("CartoDB.Positron") %>%  # You can choose from various tilesets
      addCircleMarkers(
        lat = ~LATITUDE,
        lng = ~LONGITUDE,
        radius = 0.1,
        color = "red",
        fillOpacity = 0.7,
        popup = ~paste("Location: ", LOCATION)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = "red",
        labels = "Injured Persons"
      )
  })
  
  
  data_filtered <- reactive({
    if (!is.null(data())) {
      hour <- input$hour
      filtered_data <- data() %>%
        filter(hour(DateTime) == hour)
      return(filtered_data)
    }
  })
  output$hourly_map <- renderLeaflet({
    data_filtered_df <- data_filtered()
    # Create a Leaflet map for hourly data
    leaflet(data_filtered_df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lat = ~LATITUDE,
        lng = ~LONGITUDE,
        radius = 0.1,
        color = "darkorange",
        fillOpacity = 0.7,
        popup = ~paste("Location: ", LOCATION)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = "darkorange",
        labels = "Collisions At Hour Selected"
      )
  })
  output$hourly_map_title <- renderUI({
    # Create a dynamic map title based on the selected hour with HTML formatting
    hour <- input$hour
    title <- sprintf("<span style='color: darkred;'><em><strong>Collisions at Hour %02d:00 - %02d:00</strong></em></span>", hour, (hour + 1) %% 24)
    HTML(title)
  })
  
  output$histogram <- renderPlotly({
    data_filtered_df <- data_filtered()
    
    # Create a histogram plot
    hist_data <- data_filtered_df %>%
      mutate(minute = minute(DateTime))
    
    title <- sprintf("<span style='color: darkblue;'><em>Crashes by Minute for Hour %02d:00 - %02d:00</em></span>", input$hour, (input$hour + 1) %% 24)
    
    plot <- ggplot(hist_data, aes(x = minute)) +
      geom_histogram(binwidth = 1, fill = "darkred", color = "black") +
      labs(x = "Minute", y = "Crashes", title = title) +
      theme_minimal()
    
    # Convert ggplot plot to Plotly for interactivity
    ggplotly(plot)
  })
  
  
  
  output$contributing_factors <- renderPlotly({
    data_filtered <- data()
    value_counts <- table(data_filtered$`CONTRIBUTING.FACTOR.VEHICLE.1`)
    sorted_counts <- sort(value_counts, decreasing = TRUE)
    top_factors <- names(sorted_counts[1:10])
    top_data <- data_filtered[data_filtered$`CONTRIBUTING.FACTOR.VEHICLE.1` %in% top_factors, ]
    
    # Create a bar plot
    custom_colors <- c(
      "#1f77b4",  # Blue
      "#ff7f0e",  # Orange
      "#2ca02c",  # Green
      "#d62728",  # Red
      "#9467bd",  # Purple
      "#8c564b",  # Brown
      "#e377c2",  # Pink
      "#7f7f7f",  # Gray
      "#bcbd22",  # Olive
      "#17becf"   # Teal
    )
    
    p <- ggplot(top_data, aes(x = `CONTRIBUTING.FACTOR.VEHICLE.1`, fill = `CONTRIBUTING.FACTOR.VEHICLE.1`)) +
      geom_bar() +
      labs(x = "Contributing Factor", y = "Count", title = "Top Ten Contributing Factors") +
      scale_fill_manual(values = custom_colors) +  # Use the custom colors here
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert ggplot plot to Plotly for interactivity
    ggplotly(p)
  })
  
  output$vehicle_type_breakdown <- renderPlotly({
    data_filtered <- data()
    
    # Create a bar plot
    custom_colors <- c(
      "#1f77b4",   # Blue
      "#ff7f0e",   # Orange
      "#2ca02c",   # Green
      "#d62728",   # Red
      "#9467bd",   # Purple
      "#8c564b",   # Brown
      "#e377c2",   # Pink
      "#7f7f7f",   # Gray
      "#bcbd22",   # Olive
      "#17becf"    # Teal
    )
    
    top_vehicle_data <- data_filtered %>%
      group_by(`VEHICLE.TYPE.CODE.1`) %>%
      summarize(Count = n()) %>%
      arrange(desc(Count)) %>%
      slice_max(order_by = Count, n = 10)
    
    # Create the bar plot
    p <- ggplot(top_vehicle_data, aes(x = reorder(`VEHICLE.TYPE.CODE.1`, -Count), y = Count, fill = `VEHICLE.TYPE.CODE.1`)) +
      geom_bar(stat = "identity") +
      labs(x = "Vehicle Type", y = "Count", title = "Top Vehicle Type Involved In Accidents") +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert ggplot plot to Plotly for interactivity
    ggplotly(p)
  })
  output$contributing_factors <- renderPlotly({
    data_filtered <- data()
    value_counts <- table(data_filtered$`CONTRIBUTING.FACTOR.VEHICLE.1`)
    sorted_counts <- sort(value_counts, decreasing = TRUE)
    top_factors <- names(sorted_counts[1:10])
    top_data <- data_filtered[data_filtered$`CONTRIBUTING.FACTOR.VEHICLE.1` %in% top_factors, ]
    
    # Create a bar plot
    custom_colors <- c(
      "#1f77b4",  # Blue
      "#ff7f0e",  # Orange
      "#2ca02c",  # Green
      "#d62728",  # Red
      "#9467bd",  # Purple
      "#8c564b",  # Brown
      "#e377c2",  # Pink
      "#7f7f7f",  # Gray
      "#bcbd22",  # Olive
      "#17becf"   # Teal
    )
    
    p <- ggplot(top_data, aes(x = `CONTRIBUTING.FACTOR.VEHICLE.1`, fill = `CONTRIBUTING.FACTOR.VEHICLE.1`)) +
      geom_bar() +
      labs(x = "Contributing Factor", y = "Count", title = "Top Ten Contributing Factors") +
      scale_fill_manual(values = custom_colors) +  # Use the custom colors here
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert ggplot plot to Plotly for interactivity
    ggplotly(p)
  })
  
  output$streets_by_deaths <- renderDataTable({
    # Determine the selected column based on the input
    column_name <- switch(input$affected_class,
                          "Dead Motorists" = "NUMBER.OF.MOTORIST.KILLED",
                          "Dead Pedestrians" = "NUMBER.OF.PEDESTRIANS.KILLED",
                          "Dead Cyclists" = "NUMBER.OF.CYCLIST.KILLED")
    
    # Filter the data based on the selected class and death count
    filtered_data <- data() # Use data() to access the reactive data
    filtered_data <- filtered_data[filtered_data[column_name] >= 1 & !is.na(filtered_data$ON.STREET.NAME), ]
    
    # Group the data by city and calculate the sum of injuries
    city_deaths <- aggregate(filtered_data[[column_name]], by = list(Street = filtered_data$ON.STREET.NAME), FUN = sum)
    
    # Rename the 'x' column to 'Total number of Deaths'
    names(city_deaths)[2] <- "Total number of Deaths"
    
    # Sort the data by the sum of injuries in descending order
    city_deaths <- city_deaths[order(-city_deaths$`Total number of Deaths`), ]
    
    # Limit the table to the top ten cities
    top_5_cities <- head(city_deaths, 5)
    
    
    # Display the results in an interactive data table
    datatable(top_5_cities, options = list(
      paging = TRUE, # Enable pagination
      searching = TRUE, # Enable search functionality
      pageLength = 10 # Set the number of rows per page
    ), caption = "*Top Ten Cities by Total number of Deaths*")
  })
  
  
  output$dangerous_streets <- renderDataTable({
    # Determine the selected column based on the input
    column_name <- switch(input$affected_injured_class,
                          "Motorists Injured" = "NUMBER.OF.MOTORIST.INJURED",
                          "Pedestrians Injured" = "NUMBER.OF.PEDESTRIANS.INJURED",
                          "Cyclists Injured" = "NUMBER.OF.CYCLIST.INJURED"
    )
    
    # Filter the data based on the selected class and injury count
    filtered_data <- data() # Use data() to access the reactive data
    filtered_data <- filtered_data[filtered_data[column_name] >= 1 & !is.na(filtered_data$ON.STREET.NAME), ]
    
    # Group the data by city and calculate the sum of injuries
    city_injuries <- aggregate(filtered_data[[column_name]], by = list(Street = filtered_data$ON.STREET.NAME), FUN = sum)
    
    # Rename the 'x' column to 'Total number of Injuries'
    names(city_injuries)[2] <- "Total number of Injuries"
    
    # Sort the data by the sum of injuries in descending order
    city_injuries <- city_injuries[order(-city_injuries$`Total number of Injuries`), ]
    
    # Limit the table to the top ten cities
    top_10_cities <- head(city_injuries, 10)
    
    
    # Display the results in an interactive data table
    datatable(top_10_cities, options = list(
      paging = TRUE, # Enable pagination
      searching = TRUE, # Enable search functionality
      pageLength = 10 # Set the number of rows per page
    ), caption = "*Top Ten Cities by Total number of Injuries*")
  })
  
  
  
  output$raw_data <- renderDataTable({
    data() # Use data() to access the reactive data
  })
  
  # Continue with other output functions for data analysis
}

# Shiny app
shinyApp(ui = ui, server = server)

