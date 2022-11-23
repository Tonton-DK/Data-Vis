library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

region_ui <- tabPanel(
  "Data by Regions",
  
  navlistPanel(
    "Scope",
    tabPanel(
      title = "Countries",
      plotlyOutput("pollutionPlot3c",
                   width = "1200px",
                   height = "800px")
    ),
    
    tabPanel(
      "Regions",
      plotlyOutput("pollutionPlot3r",
                   width = "1200px",
                   height = "800px")
    )
  )
)

q3_server <- function(input, output) {
  output$pollutionPlot3c <- renderPlotly({
      grouped <- group_by(dat, region, countryName, reportingYear)
      aes <- aes(x = year,
                 y = emission,
                 color = country)
      meaned <-
        summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
      meaned <-
        rename(meaned,
               country = countryName,
               year = reportingYear,
               emission = mean_emission)
      
      plt <- ggplot(meaned,
                    aes) +
        geom_point() +
        geom_line() +
        xlab("Year") +
        ylab("Mean Emission (1000x tons)") +
        scale_x_continuous(breaks = 2007:2020) +
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
        facet_wrap( ~ region) +
        scale_color_manual(values = c(raw_cols)) +
        geom_vline(xintercept = 2015,
                   linetype = "dotted",
                   colour = "darkblue")
    
    ggplotly(plt)
  })

  output$pollutionPlot3r <- renderPlotly({
    grouped <- group_by(dat, region, reportingYear)
    aes <- aes(x = year,
               y = emission,
               color = region)
    meaned <-
      summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
    meaned <-
      rename(meaned, year = reportingYear, emission = mean_emission)
    
    plt <- ggplot(meaned,
                  aes) +
      geom_point() +
      geom_line() +
      xlab("Year") +
      ylab("Mean Emission (1000x tons)") +
      scale_x_continuous(breaks = 2007:2020) +
      scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
      scale_color_manual(values = c(raw_cols)) +
      geom_vline(xintercept = 2015,
                 linetype = "dotted",
                 colour = "darkblue")
    
    ggplotly(plt)
  })
}

load_q3_data <- function() {
  dat <- read_csv("data/data.csv")
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
  reg <- read_csv("data/regions.csv")
  
  grouped <- group_by(dat, countryName, reportingYear)
  meaned <-
    summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
  meaned <-
    rename(meaned,
           country = countryName,
           year = reportingYear,
           emission = mean_emission)
  
  regioned <- inner_join(dat, reg, by = "countryName")
  grouped <- group_by(regioned, region, reportingYear)
  meaned <-
    summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
  meaned <-
    rename(meaned, year = reportingYear, emission = mean_emission)
  
  grouped <- group_by(dat, countryName, reportingYear)
  meaned <-
    summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
  
  ranked_by_year <- meaned %>%
    # for each year we assign a rank
    group_by(reportingYear) %>%
    arrange(reportingYear,-mean_emission) %>%
    # assign ranking
    mutate(rank = 1:n()) %>%
    filter(rank <= 10) %>%
    filter(reportingYear == 2018)
}