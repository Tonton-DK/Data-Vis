library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

#source('data.R')

q3_ui <- tabPanel("Question 3", 
                  selectInput("grouping",
                              label = "Choose a grouping type",
                              choices = list("Country", "EU Region", "Country by EU Region"),
                              selected = "Country"),
                  plotlyOutput("pollutionPlot3", 
                               width = "1200px", 
                               height = "800px")
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
      
      plt <- ggplot(
        meaned, 
        aes) +
        geom_point() +
        geom_line() +
        xlab("Reporting year") +
        ylab("Mean Emission (1000x tons)") + 
        scale_x_continuous(breaks=2007:2020) + 
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
        geom_vline(xintercept = 2015, linetype="dotted", colour="darkblue") 
    }
    else if (input$grouping == "EU Region"){
      grouped <- group_by(dat, region, reportingYear)
      aes <- aes(
        x = year,
        y = emission,
        color = region)
      meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
      meaned <- rename(meaned, year = reportingYear, emission = mean_emission)
      
      plt <- ggplot(
        meaned, 
        aes) +
        geom_point() +
        geom_line() +
        xlab("Reporting year") +
        ylab("Mean Emission (1000x tons)") + 
        scale_x_continuous(breaks=2007:2020) + 
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
        geom_vline(xintercept = 2015, linetype="dotted", colour="darkblue") 
    }
    else if (input$grouping == "Country by EU Region") {
      grouped <- group_by(dat, region, countryName, reportingYear)
      aes <- aes(
        x = year,
        y = emission,
        color = country)
      meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
      meaned <- rename(meaned, country = countryName, year = reportingYear, emission = mean_emission)
      
      plt <- ggplot(
        meaned, 
        aes) +
        geom_point() +
        geom_line() +
        xlab("Reporting year") +
        ylab("Mean Emission (1000x tons)") + 
        scale_x_continuous(breaks=2007:2020) + 
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
        facet_wrap(~region) +
        geom_vline(xintercept = 2015, linetype="dotted", colour="darkblue") 
    }
    
    ggplotly(plt)
  })
}