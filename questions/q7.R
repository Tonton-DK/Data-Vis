library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

source('ingestion/data.R')
source('util/create_ui.R')

q7_ui <- create_ui(
  index = 7, 
  question = "Which pollutants are mostly released?",
  conclusion = "Initially CO2 was expected to be among the top pollutants released; however, the visualisation has proven that not only is CO2 the top released pollutant, every other pollutant, excluding the additional CO2 measure, is almost negligible compared to it. While CO2 shows a decrease over the span of the data going from 2.2 billion tons in 2007 to 0.74 billion tons in 2020, the next in line is CO with 0.0012 billion tons; a mere 2000 times smaller than that of CO2.<br><br>
In spite of the fact that some countries have yet to report their emissions, it is, judging by the chart, plausible that the majority of the missing data pertains to CO2. Moreover, the missing data might be the reason behind the massive drop seen in CO2 from 2017 to 2020.",
  plot = plotlyOutput(
    "pollutionPlot7",
    width = "1000px",
    height = "100%"
  ) %>% withSpinner(color="#4363D8")
)

q7_server <- function(input, output) {
  output$pollutionPlot7 <- renderPlotly({
    plt1 <- create_q7_plot(q7_data)
    plt2 <- create_q7_plot(q7_data_alt)
    subplot(plt1,
            plt2,
            nrows = 2,
            shareX = TRUE,
            shareY = TRUE) %>%
      highlight(~pollutant, on = "plotly_click", off = "plotly_doubleclick")
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
          "<br>Pollutant:",
          pollutant,
          "Reporting year: ",
          reportingYear,
          "<br>Mean emission: ",
          round(abs(mean_emission), 2)
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
  filter(pollutant %in% top2) %>% 
  highlight_key(~pollutant)
q7_data_alt <<- temp %>%
  filter(!pollutant %in% top2) %>% 
  highlight_key(~pollutant)
rm(top2, temp)
