library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)
library(treemapify)

#source('data.R')

q2_ui <- tabPanel(
  "Question 2",
  plotOutput("pollutionPlot1337",
               width = "1600px",
               height = "800px"),
  plotlyOutput("pollutionPlot1338",
               width = "1600px",
               height = "800px"),
  plotlyOutput("pollutionPlot1339",
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
    
    grouped <- group_by(dat, eprtrSectorName, reportingYear)
    meaned <-
      summarise(grouped, mean_emission = mean(emissions, na.rm = TRUE))
    meaned <-
      rename(
        meaned,
        eprtrSectorName = eprtrSectorName,
        year = reportingYear,
        emission = mean_emission
      )
    
    summarised = 
      dat %>% 
      group_by(eprtrSectorName) %>% 
      summarise(emissions = sum(emissions))
    
    # Line plot
    # ggplotly(
    #   ggplot(meaned, aes(
    #     x = year,
    #     y = emission,
    #     color = eprtrSectorName
    #   )) +
    #     geom_line(size = 1) +
    #     geom_point() +
    #     scale_x_continuous(breaks = 2007:2020) +
    #     scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
    #     labs(
    #       title = "Sector pollution per year",
    #       x = "Year",
    #       y = "Emissions",
    #       color = "Sector"
    #     )
    # )
    
    ggplot(summarised, aes(area = emissions,
                       fill = eprtrSectorName,
                       label = eprtrSectorName)) +
      geom_treemap() +
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 15,
                        grow = TRUE) +
      labs(x = NULL,
           y = NULL,
           fill = "Sectors")
    
    # ggplotly(
    #   ggplot(
    #     meaned,
    #     aes(
    #       x = fct_reorder(eprtrSectorName, emission),
    #       y = emission,
    #       fill = eprtrSectorName
    #     )
    #   ) +
    #     geom_violin(trim = FALSE) +
    #     scale_y_log10() +
    #     coord_flip() +
    #     theme(legend.position = "none") +
    #     labs(title = "Industry sector pollution", x = NULL, y = "Emissions")
    # )
  })
  
  output$pollutionPlot1338 <- renderPlotly({
    # bar
    ranked_by_year <-
      meaned %>%
      group_by(year) %>%
      arrange(year) %>%
      mutate(rank = 1:n()) %>%
      filter(rank <= 10)
    
    # https://evamaerey.github.io/little_flipbooks_library/racing_bars/racing_barcharts.html#10
    meaned %>%
      # for each year we assign a rank
      group_by(year) %>%
      arrange(year, -emission) %>%
      # assign ranking
      mutate(rank = 1:n()) %>%
      filter(rank <= 10) ->
      ranked_by_year
    
    ranked_by_year %>%
      ggplot() +
      aes(xmin = 0 ,
          xmax = emission / 1000000) +
      aes(ymin = rank - .45,
          ymax = rank + .45,
          y = rank) +
      facet_wrap( ~ year) +
      geom_rect() +
      aes(fill = eprtrSectorName) +
      scale_fill_viridis_d(option = "magma",
                           direction = -1) +
      scale_y_reverse() +
      labs(x = "Sectors", y = NULL, fill = NULL) +
      theme(legend.position = "right") ->
      bar
    
    ggplotly(bar)
  })
  
  output$pollutionPlot1339 <- renderPlotly({
    # bar
    ranked_by_year <-
      meaned %>%
      group_by(year) %>%
      arrange(year) %>%
      mutate(rank = 1:n()) %>%
      filter(rank <= 10)
    
    # https://evamaerey.github.io/little_flipbooks_library/racing_bars/racing_barcharts.html#10
    meaned %>%
      # for each year we assign a rank
      group_by(year) %>%
      arrange(year, -emission) %>%
      # assign ranking
      mutate(rank = 1:n()) %>%
      filter(rank <= 10) ->
      ranked_by_year
    
    ranked_by_year %>%
      ggplot() +
      aes(xmin = 0 ,
          xmax = emission / 1000000) +
      aes(ymin = rank - .45,
          ymax = rank + .45,
          y = rank) +
      facet_wrap( ~ year) +
      geom_rect() +
      aes(fill = eprtrSectorName) +
      scale_fill_viridis_d(option = "magma",
                           direction = -1) +
      scale_y_reverse() +
      labs(x = "Sectors", y = NULL, fill = NULL) +
      theme(legend.position = "right") ->
      bar
    
    ggplotly(bar)
    
    bar_anim <-
      bar +
      facet_null() +
      scale_x_continuous(limits = c(0, 400),
                         breaks = c(0, 100, 200, 300)) +
      geom_text(
        x = 100,
        y = 5,
        aes(label = as.character(year)),
        size = 20,
        col = "grey18"
      ) +
      aes(group = eprtrSectorName) +
      transition_time(year)
    
    bar_anim
  })
}