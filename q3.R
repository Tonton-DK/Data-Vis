library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

#source('data.R')
raw_cols <- c("#666666", "#A6761D", "#E6AB02", "#66A61E", "#E7298A", "#7570B3",
              "#D95F02", "#1B9E77", "#A6CEE3", "#B2DF8A", "#33A02C", "#A9A9A9",
              "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", 
              "#FFFF99", "#B15928", "#B3B3B3", "#E5C494", "#FFD92F", "#A6D854", 
              "#E78AC3", "#8DA0CB", "#FC8D62", "#66C2A5", "#CD7EAA", "#D5683A", 
              "#2A78B5", "#F1E54E", "#2AA179", "#62B6E6", "#E7A337", "#292929",
              "#FFE119", "#4363D8", "#F58231", "#FABEBE", "#E6BEFF", "#800000", 
              "#000075", "#FCFCFC")


q3_ui <- tabPanel("Question 3", 
                  selectInput("grouping",
                              label = "Choose a grouping type",
                              choices = list("Country", "Country by EU Region", "EU Region"),
                              selected = "Country"),
                  plotlyOutput("pollutionPlot3", 
                               width = "1200px", 
                               height = "800px")
)

q3_server <- function(input, output){
  output$pollutionPlot3 <- renderPlotly({
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
        xlab("Year") +
        ylab("Mean Emission (1000x tons)") +
        scale_x_continuous(breaks=2007:2020) + 
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
        scale_color_manual(values=c(raw_cols)) +
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
        xlab("Year") +
        ylab("Mean Emission (1000x tons)") + 
        scale_x_continuous(breaks=2007:2020) + 
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
        facet_wrap(~region) +
        scale_color_manual(values=c(raw_cols)) +
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
        xlab("Year") +
        ylab("Mean Emission (1000x tons)") + 
        scale_x_continuous(breaks=2007:2020) + 
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
        scale_color_manual(values=c(raw_cols)) +
        geom_vline(xintercept = 2015, linetype="dotted", colour="darkblue") 
    }
    
    ggplotly(plt)
  })
}

load_q3_data <- function(){
  dat <- read_csv("data.csv")
  dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))
  reg <- read_csv("regions.csv")
  # Color list from: https://github.com/DesiQuintans/Pebble-safe
  # The colors are quite bright!
  raw_cols <- c("#666666", "#A6761D", "#E6AB02", "#66A61E", "#E7298A", "#7570B3",
                "#D95F02", "#1B9E77", "#A6CEE3", "#B2DF8A", "#33A02C", "#A9A9A9",
                "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", 
                "#FFFF99", "#B15928", "#B3B3B3", "#E5C494", "#FFD92F", "#A6D854", 
                "#E78AC3", "#8DA0CB", "#FC8D62", "#66C2A5", "#CD7EAA", "#D5683A", 
                "#2A78B5", "#F1E54E", "#2AA179", "#62B6E6", "#E7A337", "#292929",
                "#FFE119", "#4363D8", "#F58231", "#FABEBE", "#E6BEFF", "#800000", 
                "#000075", "#FCFCFC")
  
  grouped <- group_by(dat, countryName, reportingYear)
  meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
  meaned <- rename(meaned, country = countryName, year = reportingYear, emission = mean_emission)
  
  regioned <- inner_join(dat, reg, by = "countryName")
  grouped <- group_by(regioned, region, reportingYear)
  meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
  meaned <- rename(meaned, year = reportingYear, emission = mean_emission)
  
  grouped <- group_by(dat, countryName, reportingYear)
  meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
  
  ranked_by_year <- meaned %>%  
    # for each year we assign a rank
    group_by(reportingYear) %>% 
    arrange(reportingYear, -mean_emission) %>%  
    # assign ranking
    mutate(rank = 1:n()) %>%  
    filter(rank <= 10) %>%  
    filter(reportingYear == 2018)
}