# source('ingestion/data.R')
# source('util/create_ui.R')

q1_ui <- create_ui(
  index = 1, 
  question = "Which countries are responsible for the majority of pollution?",
  conclusion = "",
  control = sliderInput(
    "yearId",
    "Select a year",
    min = 2007,
    max = 2020,
    value = 2007,
    sep = "",
    animate = TRUE
  ),
  controlWidth = 240,
  plot = tabsetPanel(
    tabPanel(
      "Top-10",
      plotlyOutput(
        "pollutionPlot1b",
        width = "800px",
        height = "400px"
      )
    ),
    tabPanel(
      "Map",
      plotlyOutput(
        "pollutionPlot1a",
        width = "800px",
        height = "800px"
      )
    )
  )
)

q1_server <- function(input, output) {
  output$pollutionPlot1a <- renderPlotly({
    filtered = dat %>% filter(reportingYear == input$yearId)
    
    grouped <- group_by(filtered, countryName)
    meaned <-
      summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
    meaned <- countries %>% left_join(meaned, by = "countryName")
    mutated <- mutate(meaned,
                      region = ifelse(
                        countryName == "Czechia",
                        "Czech Republic",
                        ifelse(countryName == "United Kingdom", "UK", countryName)
                      ))
    
    mapdata <- map_data("world") %>%
      inner_join(mutated, by = "region")
    mapdata <-
      rename(mapdata, country = countryName, emission = mean_emission)
    
    labels <- mapdata %>%
      group_by(region) %>%
      select(region, group, long, lat) %>%
      summarise_all(mean)
    
    ggply <- ggplotly(
      ggplot(mapdata,
             aes(
               x = long,
               y = lat,
               group = group,
               label = country
             )) +
        geom_polygon(aes(fill = emission), color = "black") +
        geom_text(
          data = labels,
          aes(label = region),
          colour = "black",
          size = 3
        ) +
        scale_fill_gradient( 
          name = "Mean emission (1000x tons)",
          low = "white",  
          high = "red",
          na.value = "grey",  
          limits = c(0, 350),
          breaks = scales::breaks_extended(n = 10))
    )
    
    ggply$x$data[[33]]$hoverinfo <- "skip"
    ggply
  })
  
  output$pollutionPlot1b <- renderPlotly({
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
      filter(reportingYear == input$yearId)
    
    ggplotly(
      ggplot(ranked_by_year) +
        aes(xmin = 0 ,
            xmax = mean_emission) +
        aes(
          ymin = rank - .45,
          ymax = rank + .45,
          y = rank
        ) +
        geom_rect(alpha = .7) +
        aes(fill = countryName) +
        scale_fill_viridis_d(option = "Teal", direction = -1) +
        scale_x_continuous(
          limits = c(-150, 400),
          breaks = c(0, 100, 200, 300, 400)
        ) +
        geom_text(
          col = "gray13",
          hjust = "right",
          aes(label = countryName),
          x = -50
        ) +
        scale_y_reverse() +
        labs(fill = NULL) +
        labs(x = 'Mean Emission') +
        labs(y = "Rank") +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()
        )
    )
  })
}