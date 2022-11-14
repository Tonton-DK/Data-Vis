library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plotly)

q6_ui <- tabPanel(
  "Question 6",
  fluidPage(
    fluidRow(
      column(
        width = 2,
        style = "margin-top: 47px; max-width: 140px;",
        radioButtons(
          "orderby", "Order by:",
          c(
            "Capital" = "cap",
            "Non-capital" = "ncap",
            "Country")
        )
      ),
      column(
        width = 10,
         plotlyOutput(
          "pollutionPlot6", 
          width = "1200px", 
          height = "800px")
      )
    )
  )
)

q6_server <- function(input, output) {
  renderPlotly({
    ggply <- create_q6_plot(q6, input$orderby)
    ggply
  })
}

load_q6_data <- function() {
  temp <- distinct(dat, isCapital)
  q6 <- crossing(countries, temp)
  temp <- dat %>%
    select(countryName, isCapital, emissions) %>%
    group_by(countryName, isCapital) %>%
    summarise(mean_emission = mean(emissions, na.rm = TRUE))
  q6 <<- left_join(q6, temp) %>%
    mutate(
      NA_val = ifelse(!is.na(mean_emission), NA, 0),
      mean_emission = ifelse(isCapital, mean_emission * -1, mean_emission),
      order_cap = ifelse(isCapital, ifelse(is.na(mean_emission), 0, mean_emission), 0),
      order_ncap = ifelse(isCapital, 0, mean_emission),
      isCapital = ifelse(isCapital, "Capital", "Non-capital")
    )
  rm(temp)
}

create_q6_plot <- function(df, order) {
  lim = c(-400, 400)
  brk = c(-400, -300, -200, -100, 0, 100, 200, 300, 400)
  lbl = c(400, 300, 200, 100, 0, 100, 200, 300, 400)
  
  inner_plt <- ggplot(
    q6,
    aes(
      x = {
        if (order == "cap") {
          reorder(countryName, order_cap, decreasing = T)
        }
        else if (order == "ncap") {
          reorder(countryName, order_ncap, decreasing = F, sum, order = T)
        }
        else {
          countryName
        }
      },
      y = mean_emission,
      text = paste(
        "Country:\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t", countryName,
        "<br>Mean emission:\t\t\t\t\t\t", round(abs(mean_emission), 2),
        "<br>Pollution in capital:\t", isCapital
      )
  )) 
  
  plt <- ggplotly(
    inner_plt + 
    geom_bar(
      stat = "identity",
      width = 0.3,
      fill = "#d4d4d4"
    ) +
    scale_y_continuous(
      limit = c(-400, 400),
      breaks = brk,
      labels = lbl,
      expand = c(0, 0)
    ) +
    geom_point(aes(fill = isCapital), size = 3, stroke = 0) +
    geom_point(aes(x = countryName, y = NA_val, fill = "NA"), size = 3, stroke = 0) +
    labs(
      x = "Country",
      y = "Mean emission (1000x tons)",
      fill = "Pollution in capital",
      title = "Capital        Non-capital"
    ) +
    geom_hline(
      yintercept = 0,
      color = "black",
      alpha = 0.3
    ) +
    coord_flip() +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.515)
    ),
    tooltip = c("text")
  )
  
  return(plt)
}