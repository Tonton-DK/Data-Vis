library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

#source('data.R')

q4_ui <- tabPanel("Question 4", 
                  plotlyOutput("pollutionPlot4", 
                               width = "1600px", 
                               height = "800px"))

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