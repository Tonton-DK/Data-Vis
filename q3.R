library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

source('data.R')

q3_ui <- tabPanel("Question 3", 
                  selectInput("grouping",
                              label = "Choose a grouping type",
                              choices = list("Country", "EU Region"),
                              selected = "Country"),
                  plotlyOutput("pollutionPlot3")
)

q3_server <- function(input, output){
  renderPlotly({
    if(input$grouping == "Country"){
      grouped <- group_by(dat, countryName, reportingYear)
      aes <- aes(
        x = year,
        y = emission,
        color = country)
      meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
      meaned <- rename(meaned, country = countryName, year = reportingYear, emission = mean_emission)
    }
    else if (input$grouping == "EU Region"){
      grouped <- group_by(dat, region, reportingYear)
      aes <- aes(
        x = year,
        y = emission,
        color = region)
      meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
      meaned <- rename(meaned, year = reportingYear, emission = mean_emission)
    }
    
    ggplotly(
      ggplot(
        meaned, 
        aes) +
        geom_point() +
        geom_line() +
        xlab("Reporting year") +
        ylab("Mean Emission") + 
        scale_x_continuous(breaks=2007:2020) + 
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)))
  })
}