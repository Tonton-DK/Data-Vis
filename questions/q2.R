# library(tidyverse)
# library(ggplot2)
# library(dplyr)
# library(stringr)
# library(plotly)
# library(rcartocolor)

# source('ingestion/data.R')
# source('util/create_ui.R')


q2_ui <- create_ui(
  index = 2, 
  question = "How has pollution levels developed over time pr. country?",
  conclusion = "The time series plot visualises how the emission of air pollutants has developed for every country within the data-set. Three different views are available; All countries, Countries divided by EU region, and the EU regions themselves.<br><br>

When analysing the evolution of pollution it becomes clear that a lot of the countries are relatively stable, or have a small downward going trend. Most of these countries already had a relatively low pollution level, which could indicate that they were never a target for international pollution changes.<br>
Few countries experienced major rises and falls throughout the timeline: Estonia, Netherlands, Greece, Czechia, and Malta. Most of these were also identified in the top-10 polluting countries, indicating that these have been main targets for implementing pollution limits.<br><br>

This fits very well with what was expected by the group: The countries responsible for the majority of air pollution, would also be the countries mostly affected by global limitation politics. As can be seen these countries either experience a huge downward trend like Malta, or many up-and-down fluctuations like Estonia, struggling to limit their pollutant emissions.<br><br>

When looking only at the regions, a general downward trend becomes apparent from 2015 and onward in the Southern, Eastern, and Western regions. The Northern region on the other hand struggles to reduce air pollutants. When comparing this to the graphs of all countries divided by region, it becomes apparent that this is mainly due to Estonia driving up the mean. This makes sense seeing as Estonia was the top contender in the top-10 of polluting countries.",
  plot = div(
    fluidRow(
      plotlyOutput("pollutionPlot2a",
                   width = "1000px",
                   height = "100%") %>% withSpinner(color="#4363D8")
    ),
    fluidRow(style="margin-top:5rem;",
      plotlyOutput("pollutionPlot2b",
                   width = "1000px",
                   height = "100%") %>% withSpinner(color="#4363D8")
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
             emission = mean_emission) %>%
      highlight_key(~country)
    
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
    
    ggplotly(plt) %>%
      highlight(on = "plotly_hover", off = "plotly_doubleclick")
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
             emission = mean_emission) %>%
      highlight_key(~country)
    
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
    
    ggplotly(plt) %>%
      highlight(on = "plotly_hover", off = "plotly_doubleclick")
  })
}