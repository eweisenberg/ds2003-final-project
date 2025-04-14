library(shiny)
library(plotly)
library(dplyr)
library(readr)





death_data <- read.csv("tested_worldwide.csv") %>%
  mutate(Date = as.Date(Date),
         death = as.numeric(death)) %>%   
  filter(!is.na(death))

ui <- fluidPage(
  titlePanel("Total Deaths Across Countries"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select country:",
                  choices = unique(death_data$Country_Region),
                  selected = unique(death_data$Country_Region),
                  multiple = TRUE),
      dateRangeInput("date_range", "Select date range:",
                     start = min(death_data$Date),
                     end = max(death_data$Date))
    ),
    mainPanel(
      plotlyOutput("death_plot")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    death_data %>%
      filter(Country_Region %in% input$country,
             Date >= input$date_range[1],
             Date <= input$date_range[2]) %>%
      group_by(Date, Country_Region) %>%
      summarise(total_death = sum(death, na.rm = TRUE), .groups = "drop")
  })

  output$death_plot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Date, y = ~total_death, color = ~Country_Region,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Total Deaths Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Total Deaths"),
             hovermode = "compare")
  })
}

shinyApp(ui = ui, server = server)