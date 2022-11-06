library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

dat <- read_csv("data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))
regions <- read_csv("regions.csv")
dat <- inner_join(dat, regions, by = "countryName")
countries <- dat %>% distinct(countryName)

ui <- navbarPage(
  title="My Application",
  tabPanel("Question 1",
           sliderInput("yearId",
                       "Select a year",
                       min = 2007,
                       max = 2020,
                       value = 2007,
                       sep = "",
                       animate = TRUE),
           plotlyOutput("pollutionPlot1")
  ),
  tabPanel("Question 2"),
  tabPanel("Question 3", 
           selectInput("grouping",
                       label = "Choose a grouping type",
                       choices = list("Country", "EU Region"),
                       selected = "Country"),
           plotlyOutput("pollutionPlot3")
  ),
  tabPanel("Question 4", plotlyOutput("pollutionPlot4")),
  tabPanel("Question 5"),
  tabPanel("Question 6"),
  tabPanel("Question 7"),
  tabPanel("Question 8")
)

server <- function(input, output) {
  
  output$pollutionPlot1 <- renderPlotly({
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
          name = "Mean emission",
          na.value = "white",
          limits = c(0, 300000000),
          breaks = scales::breaks_extended(n = 10)))
    
    ggply$x$data[[33]]$hoverinfo <- "skip"
    ggply
  })
  
  output$pollutionPlot3 <- renderPlotly({
    
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
        ylab("Mean Emission in Kg") +
        scale_x_continuous(breaks=2007:2020) +
        scale_y_continuous(breaks = scales::breaks_extended(n=15)) +
        scale_color_carto_d(palette="Safe",direction=-1))
  })
}

shinyApp(ui,server)
