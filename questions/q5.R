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
  conclusion = "",
  plot = plotlyOutput(
    "pollutionPlot5",
    width = "100%",
    height = "800px"
  )
)

q5_server <- function(input, output) {
  output$pollutionPlot5 <- renderPlotly({
    ggplotly(
      ggplot(
        q5_data,
        aes(
          x = reorder(eprtrSectorName, emissions),
          y = emissions / 100000000,
          text = paste(
            "Sector:",
            eprtrSectorName,
            "<br>Emission:",
            round(abs(emissions), 2)
          )
        )
      ) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(
          title = "Overall sector pollution",
          x = NULL,
          y = "Emission (1000x tons)"
        ) +
        theme(legend.position = "none") +
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)),
      tooltip = c("text")
    )
  })
}

temp <-
  dat %>% select(
    c(
      "countryName",
      "eprtrSectorName",
      "facilityName",
      "Longitude",
      "Latitude",
      "City",
      "pollutant",
      "emissions",
      "reportingYear"
    )
  )
temp <- temp %>% drop_na(eprtrSectorName)

q5_data <<-
  temp %>%
  group_by(eprtrSectorName) %>%
  summarise(emissions = sum(emissions))
rm(temp)