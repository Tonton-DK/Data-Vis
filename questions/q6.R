library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)
library(treemapify)


# source('ingestion/data.R')
# source('util/create_ui.R')

q6_ui <- create_ui(
  index = 6, 
  question = "Which industry sectors are responsible for the majority of the pollution?",
  conclusion = "",
  plot = plotOutput(
    "pollutionPlot6",
    width = "1000px",
    height = "500px"
  ) %>% withSpinner(color="#4363D8")
)

q6_server <- function(input, output) {
  output$pollutionPlot6 <- renderPlot({
    ggplot(
      q6_data,
      aes(
        area = emissions,
        fill = emissions / 100000000,
        label = eprtrSectorName
      )
    ) +
      geom_treemap() +
      geom_treemap_text(
        colour = c(
          rep("white", 2),
          1, rep("white", 6)
        ),
        place = "centre", size = 15
      ) +
      scale_fill_viridis_c() +
      labs(
        x = NULL,
        y = NULL,
        fill = "Emission (1000x tons)"
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

q6_data <<-
  temp %>%
  group_by(eprtrSectorName) %>%
  summarise(emissions = sum(emissions))
rm(temp)