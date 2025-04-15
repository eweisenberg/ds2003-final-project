
library(plotly)
library(dplyr)
library(readr)


death_data <- read.csv("tested_worldwide.csv") %>%
  mutate(Date = as.Date(Date),
         death = as.numeric(death)) %>%
  filter(!is.na(death))


countries <- unique(death_data$Country_Region)
n <- length(countries)


colors <- setNames(rainbow(n), countries)


p <- plot_ly()


for (i in seq_along(countries)) {
  country <- countries[i]
  df <- death_data %>% filter(Country_Region == country)
  
  p <- add_trace(p,
                 data = df,
                 x = ~Date,
                 y = ~death,
                 type = 'scatter',
                 mode = 'markers',
                 name = country,
                 marker = list(color = colors[[country]]),
                 text = ~paste("Country:", Country_Region,
                               "<br>Date:", Date,
                               "<br>Deaths:", death),
                 hoverinfo = "text",
                 visible = TRUE) 
}


checkbox_buttons <- list(
  list(method = "restyle",
       args = list("visible", as.list(rep(TRUE, n))),
       label = "Select All"),
  list(method = "restyle",
       args = list("visible", as.list(rep(FALSE, n))),
       label = "Deselect All")
)


for (i in seq_along(countries)) {
  vis <- rep(FALSE, n)
  vis[i] <- TRUE
  checkbox_buttons[[length(checkbox_buttons) + 1]] <- list(
    method = "restyle",
    args = list("visible", as.list(vis)),
    label = countries[i]
  )
}


p <- layout(p,
            title = "COVID Deaths Over Time",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Cumulative_Deaths"),
           updatemenus = list(
  list(
    type = "dropdown",
    active = -1,
    buttons = checkbox_buttons,
    x = 0,
    y = 1,
    xanchor = "left",
    yanchor = "top"
  )
)
)

p



