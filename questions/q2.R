q2_ui <- create_ui(
  index = 2, 
  question = "How has pollution levels developed over time pr. country?",
  conclusion = "",
  plot = div(
    fluidRow(
      plotlyOutput("pollutionPlot2a",
                   width = "1200px",
                   height = "800px"),
    ),
    fluidRow(
      plotlyOutput("pollutionPlot2b",
                   width = "1200px",
                   height = "800px")
    )
  )
)

q2_server <- function(input, output) {
  output$pollutionPlot2a <- renderPlotly({
    grouped <- group_by(dat, countryName, reportingYear)
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
      ggtitle("Mean emissions for each european country") +
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
  
  output$pollutionPlot2b <- renderPlotly({
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
      ggtitle("Mean emissions for each country in each european region") +
      geom_point() +
      geom_line() +
      xlab("Year") +
      ylab("Mean Emission (1000x tons)") +
      scale_x_continuous(breaks = 2007:2020) +
      scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
      facet_wrap(~ region) +
      scale_color_manual(values = c(raw_cols)) +
      geom_vline(xintercept = 2015,
                 linetype = "dotted",
                 colour = "darkblue")
    
    ggplotly(plt)
  })
}