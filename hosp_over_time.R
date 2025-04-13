library(shiny)
library(plotly)
library(dplyr)
library(readr)

hospital_data <- read_csv("tested_worldwide.csv") %>%
  mutate(
    Date = as.Date(Date),
    hospitalizedCurr = as.numeric(hospitalizedCurr))

hospital_data <- hospital_data %>% filter(!is.na(hospitalizedCurr))

ui <- fluidPage(
  titlePanel("Current Hospitalizations Across Countries"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select country:",
                  choices = unique(hospital_data$Country_Region),
                  selected = unique(hospital_data$Country_Region),
                  multiple = TRUE),
      dateRangeInput("date_range", "Select date range:",
                     start = min(hospital_data$Date),
                     end = max(hospital_data$Date))
    ),
    mainPanel(
      plotlyOutput("hospital_plot")
    )))

server <- function(input, output) {
  filtered_data <- reactive({
    hospital_data %>%
      filter(Country_Region %in% input$country,
             Date >= input$date_range[1],
             Date <= input$date_range[2]) %>%
      group_by(Date, Country_Region) %>%
      summarise(current_hospitalized = sum(hospitalizedCurr, na.rm = TRUE), .groups = "drop")
  })
  
  output$hospital_plot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Date, y = ~current_hospitalized, color = ~Country_Region,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Currently Hospitalized Patients Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Currently Hospitalized"),
             hovermode = "compare")
  })
}

shinyApp(ui = ui, server = server)
