library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)
library(treemapify)

q2_ui <- tabPanel(
  "Question 2",
  plotOutput("pollutionPlot1337",
             width = "100%",
             height = "800px"),
  plotlyOutput("pollutionPlot1338",
               width = "100%",
               height = "800px")
)

q2_server <- function(input, output) {
  output$pollutionPlot1337 <- renderPlot({
    dat <- read_csv("data.csv")
    dat <-
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
    dat <- dat %>% drop_na(eprtrSectorName)
    
    summarised =
      dat %>%
      group_by(eprtrSectorName) %>%
      summarise(emissions = sum(emissions))
    
    ggplot(summarised,
           aes(
             area = emissions,
             fill = eprtrSectorName,
             label = eprtrSectorName
           )) +
      geom_treemap() +
      geom_treemap_text(
        colour = "white",
        place = "centre",
        size = 15,
        grow = TRUE
      ) +
      labs(x = NULL,
           y = NULL,
           fill = "Sectors")
  })
  
  output$pollutionPlot1338 <- renderPlotly({
    dat <- read_csv("data.csv")
    dat <-
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
    dat <- dat %>% drop_na(eprtrSectorName)
    
    summarised =
      dat %>%
      group_by(eprtrSectorName) %>%
      summarise(emissions = sum(emissions))
    
    ggplotly(
      ggplot(
        summarised,
        aes(
          x = reorder(eprtrSectorName, emissions),
          y = emissions / 1000000,
          fill = eprtrSectorName
        )
      ) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Overall sector pollution", x = NULL, y = NULL) +
        theme(legend.position = "none") +
        scale_y_continuous(breaks = scales::breaks_extended(n=15))
    )
  })
}