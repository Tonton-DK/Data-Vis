library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)
library(treemapify)

q2_ui <- tabPanel(
  "Data by Sectors",
  plotOutput("pollutionPlot1337",
             width = "100%",
             height = "800px"),
  plotlyOutput("pollutionPlot1338",
               width = "100%",
               height = "800px"),
  plotlyOutput("pollutionPlot4", 
               width = "1600px", 
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

q4_server <- function(input, output){
  output$pollutionPlot4 <- renderPlotly({
    grouped <- group_by(dat, eprtrSectorName, reportingYear)
    meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
    meaned <- rename(meaned, sector = eprtrSectorName, year = reportingYear, emission = mean_emission)
    ggplotly(
      ggplot(
        meaned, 
        aes(
          x = year,
          y = emission,
          color=sector)) +
        geom_point() +
        geom_line() +
        xlab("Reporting year") +
        ylab("Mean Emission (1000x tons)") +
        scale_x_continuous(breaks=2007:2020) +
        scale_y_continuous(breaks = scales::breaks_extended(n=15)) +
        scale_color_carto_d(palette="Safe",direction=-1))
  })
}