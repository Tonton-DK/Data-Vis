library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

#source('data.R')

q1_ui <- tabPanel("Question 1",
                  sliderInput("yearId",
                              "Select a year",
                              min = 2007,
                              max = 2020,
                              value = 2007,
                              sep = "",
                              animate = TRUE),
                  plotlyOutput("pollutionPlot1", 
                               width = "800px", 
                               height = "800px")
)

q1_server <- function(input, output){
  renderPlotly({
    filtered = dat %>% filter(reportingYear==input$yearId)
    
    grouped <- group_by(filtered, countryName) 
    meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE)) 
    meaned <- countries %>% left_join(meaned, by = "countryName")
    mutated <- mutate(
      meaned,
      region = ifelse(countryName == "Czechia", "Czech Republic",
                      ifelse(countryName == "United Kingdom", "UK", countryName)))
    
    mapdata <- map_data("world") %>% 
      inner_join(mutated, by = "region")
    mapdata <- rename(mapdata, country = countryName, emission = mean_emission)
    
    labels <- mapdata %>% 
      group_by(region) %>%  
      select(region, group, long, lat) %>%  
      summarise_all(mean)
    
    ggply <- ggplotly(
      ggplot(
        mapdata, 
        aes(
          x = long, 
          y = lat, 
          group = group, 
          label = country)) + 
        geom_polygon(aes(fill = emission), color = "black") +
        geom_text(data = labels, aes(label = region), colour = "blue", size = 3) + 
        scale_fill_carto_c(
          palette="Safe",
          name = "Mean emission (1000x tons)",
          na.value = "white",
          limits = c(0, 300),
          breaks = scales::breaks_extended(n = 10)))
    
    ggply$x$data[[33]]$hoverinfo <- "skip"
    ggply
  })
}