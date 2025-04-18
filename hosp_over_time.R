library(shiny)
library(plotly)
library(dplyr)
library(readr)

hospital_data <- read_csv("tested_worldwide.csv") %>%
  mutate(
    Date = as.Date(Date),
    hospitalizedCurr = as.numeric(hospitalizedCurr)
  ) %>%
  filter(!is.na(hospitalizedCurr))

population_data <- read_csv("population_by_country_2020.csv") %>%
  select(`Country (or dependency)`, `Population (2020)`) %>%
  rename(
    Country_Region = `Country (or dependency)`,
    Population = `Population (2020)`
  ) %>%
  mutate(
    Country_Region = trimws(Country_Region),
    Population = as.numeric(gsub(",", "", Population))
  )

hospital_data <- left_join(hospital_data, population_data, by = "Country_Region") %>%
  filter(!is.na(Population)) %>%
  mutate(hospitalized_per_capita = hospitalizedCurr / Population)  

ui <- fluidPage(
  titlePanel("Current Hospitalizations per Capita Across Countries Over Time"),  
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
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    hospital_data %>%
      filter(Country_Region %in% input$country,
             Date >= input$date_range[1],
             Date <= input$date_range[2]) %>%
      group_by(Date, Country_Region) %>%
      summarise(hospitalized_per_capita = sum(hospitalized_per_capita, na.rm = TRUE), .groups = "drop")
  })
  
  output$hospital_plot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Date, y = ~hospitalized_per_capita, color = ~Country_Region,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Hospitalizations per Capita Over Time",  
             xaxis = list(title = "Date"),
             yaxis = list(title = "Hospitalizations per Capita"),
             hovermode = "compare")
  })
}

shinyApp(ui = ui, server = server)
