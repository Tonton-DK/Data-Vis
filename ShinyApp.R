library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)

dat <- read_csv("data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))

ui <- fluidPage(
  
  titlePanel("Pollution by year"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearId",
                  "Select a year",
                  min = 2007,
                  max = 2020,
                  value = 2007,
                  sep = "")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("pollutionPlot")
    )
  )
)

server <- function(input, output) {
  
  output$pollutionPlot <- renderPlotly({
    
    filtered = dat %>%
      filter(reportingYear==input$yearId)

    # ///////////////////////////////////
    grouped <- group_by(filtered, countryName) 
    meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE)) 
    mutated <- mutate(
      meaned,
      region = ifelse(countryName == "Czechia", "Czech Republic",
                      ifelse(countryName == "United Kingdom", "UK", countryName)))
    
    mapdata <- map_data("world") %>% 
      left_join(mutated, by = "region") %>%
      filter(!is.na(mean_emission))
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
        scale_fill_gradient(
          name = "Mean emission", 
          low = "green", 
          high = "red", 
          na.value = "white", 
          breaks = scales::breaks_extended(n = 10)) +
        geom_text(data = labels, aes(label = region), colour = "blue", size = 3))
    
    #ggply$x$data[[31]]$hoverinfo <- "skip"
    ggply
    # ///////////////////////////////////
  })
}

shinyApp(ui,server)
