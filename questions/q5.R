library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)
library(treemapify)

# source('ingestion/data.R')
# source('util/create_ui.R')

q5_ui <- create_ui(
  index = 5, 
  question = "How has pollution levels developed over time pr. industry?",
  conclusion = "The time series plot visualises how the emission of air pollutants has developed for every industry sector within the data-set. When analysing the graph it becomes apparent that the main sector of pollution is the Energy sector, having pollution levels three times as high as Paper and Wood, which comes in second place.<br>
This fits very well with what was expected by the group: The biggest source of pollution would be the Energy sector, since this sector has been growing in tune with the industrialisation of the world.<br><br>

The general trends of the sectors is that everything is either stable or slightly rising, except for the Energy sector which experiences a huge decline from 2017 and onward.<br>
This is not surprising, since the general discourse of climate change has historically been based on pollution from the Energy sector. What was surprising is that some other sectors have been allowed to grow in the meantime. Sectors such as Paper and Wood is on a general rise, not seeming affected by the political climate.",
  plotlyOutput(
    "pollutionPlot5",
    width = "1000px",
    height = "100%"
  ) %>% withSpinner(color="#4363D8"),
)

q5_server <- function(input, output) {
  output$pollutionPlot5 <- renderPlotly({
    ggplotly(
      ggplot(
        q5_data,
        aes(
          x = year,
          y = emission,
          color = sector
        )
      ) +
        ggtitle("Mean emissions by industrial sector") +
        geom_point() +
        geom_line() +
        xlab("Reporting year") +
        ylab("Mean Emission (1000x tons)") +
        scale_x_continuous(breaks = 2007:2020) +
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
        scale_color_carto_d(palette = "Safe", direction = -1) +
        geom_vline(
          xintercept = 2015,
          linetype = "dotted",
          colour = "darkblue"
        )
    )
  })
}

grouped <- group_by(dat, eprtrSectorName, reportingYear)
meaned <-
  summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
q5_data <<-
  rename(meaned,
         sector = eprtrSectorName,
         year = reportingYear,
         emission = mean_emission
  )
rm(grouped, meaned)