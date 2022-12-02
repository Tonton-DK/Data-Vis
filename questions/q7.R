library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

# source('ingestion/data.R')
# source('util/create_ui.R')

q7_ui <- create_ui(
  index = 7, 
  question = "Which pollutants are mostly released?",
  conclusion = "We conclude that CO2 is mostly released",
  plot = plotlyOutput(
    "pollutionPlot7",
    width = "1200px",
    height = "800px"
  )
)

q7_server <- function(input, output) {
  output$pollutionPlot7 <- renderPlotly({
    plt1 <- create_q7_plot(q7_data)
    plt2 <- create_q7_plot(q7_data_alt)
    subplot(plt1,
            plt2,
            nrows = 2,
            shareX = TRUE,
            shareY = TRUE)
  })
}

create_q7_plot <- function(df) {
  plt <- ggplotly(
    ggplot(
      df,
      aes(
        x = reportingYear,
        y = mean_emission,
        group = pollutant,
        color = pollutant,
        text = paste(
          "Reporting year:\t\t",
          reportingYear,
          "<br>Mean emission:\t",
          round(abs(mean_emission), 2),
          "<br>Pollutant:\t\t\t\t\t\t\t\t\t\t\t" ,
          pollutant
        )
      )
    ) +
      scale_color_manual(values = c(raw_cols)) +
      ggtitle("Mean emissions for each pollutant") +
      geom_point() +
      geom_line() +
      labs(x = "Reporting year",
           y = "Mean emission (1000x tons)",
           color = "Pollutant"),
    tooltip = c("text")
  )
  return(plt)
}

# load data
top2 <- dat %>%
  group_by(pollutant) %>%
  summarise(temp = mean(emissions, na.rm = TRUE)) %>%
  arrange(temp, .by_group = TRUE) %>%
  top_n(2) %>%
  pull(pollutant)
temp <- dat %>%
  group_by(pollutant, reportingYear) %>%
  summarise(mean_emission = sum(emissions, na.rm = TRUE)) %>%
  mutate(
    reportingYear = factor(reportingYear),
    mean_emission = round(mean_emission, 0)
  )
q7_data <<- temp %>%
  filter(pollutant %in% top2)
q7_data_alt <<- temp %>%
  filter(!pollutant %in% top2)
rm(top2, temp)
