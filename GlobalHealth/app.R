library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load and clean the dataset
globalhealth_stat <- read.csv("Global Health Statistics.csv")

# Rename columns for easier use
globalhealth_stat <- globalhealth_stat %>%
  rename(VaccineAvailability = Availability.of.Vaccines.Treatment)

# New data frame with Year, Country, and Population Affected
aggregated_data <- globalhealth_stat %>%
  group_by(Year, Country) %>%
  summarise(Population.Affected = sum(Population.Affected, na.rm = TRUE), .groups = "drop")

# Load world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Merge global health data with world map
merged_data <- world_map %>%
  left_join(aggregated_data, by = c("name" = "Country")) %>%
  mutate(Population.Affected = ifelse(is.na(Population.Affected), 0, Population.Affected)) # Handle missing data

# Define color palette
palette <- colorBin(
  palette = c("gray", "yellow", "orange", "red", "darkred"),
  domain = merged_data$Population.Affected,
  bins = c(0, 100000, 200000, 300000, 400000, 500000, 1000000),
  na.color = "gray"
)

# Define UI
ui <- fluidPage(
  titlePanel("Global Health Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("Country", "Choose a country", choices = c("All", unique(globalhealth_stat$Country))),
      sliderInput("Year", "Select Year",
                  min = min(aggregated_data$Year),
                  max = max(aggregated_data$Year),
                  value = min(aggregated_data$Year),
                  step = 1,
                  animate = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        id = "tabselected",
        tabPanel("Disease Categories per Country", 
                 value = 1,
                 plotOutput("disease_plot")),
        tabPanel("Affected Population per Country", 
                 value = 2,
                 leafletOutput("health_map", height = 600))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive data for bar chart
  filtered_disease_data <- reactive({
    if (input$Country == "All") {
      globalhealth_stat
    } else {
      globalhealth_stat %>% filter(Country == input$Country)
    }
  })
  
  # Reactive data for map (filtered by year)
  filtered_map_data <- reactive({
    merged_data %>%
      filter(Year == input$Year)
  })
  
  # Render bar chart
  output$disease_plot <- renderPlot({
    ggplot(filtered_disease_data(), aes(x = Disease.Category, fill = Disease.Category)) +
      geom_bar() +
      labs(
        title = paste("Disease Frequency for", ifelse(input$Country == "All", "All Countries", input$Country)),
        x = "Disease Category",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  # Render map
  output$health_map <- renderLeaflet({
    leaflet(data = filtered_map_data()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~palette(Population.Affected), 
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(name, "<br>Population Affected: ", Population.Affected),
        popup = ~paste(name, "<br>Population Affected: ", Population.Affected)
      ) %>%
      addLegend(
        pal = palette,
        values = ~Population.Affected,
        title = "Population Affected",
        position = "bottomright",
        opacity = 0.7
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
