library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)

emission_ui <- tabPanel("Data by Emissions",

                        navlistPanel(
                          "Scope",
                          widths = c(2, 8),
                          tabPanel(
                            title = "Over time",
                            p("Look at me, i am a question!"),
                            plotlyOutput("pollutionPlot5",
                                         width = "1200px",
                                         height = "800px"),
                            p("Look at me, i am a conclusion!")
                          )
                        ))

q5_server <- function(input, output) {
  output$pollutionPlot5 <- renderPlotly({
    plt1 <- create_q5_plot(q5)
    plt2 <- create_q5_plot(q5alt)
    subplot(plt1,
            plt2,
            nrows = 2,
            shareX = TRUE,
            shareY = TRUE)
  })
}

create_q5_plot <- function(df) {
  plt <- ggplotly(
    ggplot(
      df,
      aes(
        x = reportingYear,
        y = mean_emission,
        group = pollutant,
        color = pollutant,
        text = paste(
          "Reporting year:\t\t",
          reportingYear,
          "<br>Mean emission:\t",
          round(abs(mean_emission), 2),
          "<br>Pollutant:\t\t\t\t\t\t\t\t\t\t\t" ,
          pollutant
        )
      )
    ) +
      scale_color_manual(values = c(raw_cols)) +
      geom_point() +
      geom_line() +
      labs(x = "Reporting year",
           y = "Mean emission (1000x tons)",
           color = "Pollutant"),
    tooltip = c("text")
  ) %>%
    config(displaylogo = FALSE)
  return(plt)
}

load_q5_data <- function() {
  top2 <- dat %>%
    group_by(pollutant) %>%
    summarise(temp = mean(emissions, na.rm = TRUE)) %>%
    arrange(temp, .by_group = TRUE) %>%
    top_n(2) %>%
    pull(pollutant)
  temp <- dat %>%
    group_by(pollutant, reportingYear) %>%
    summarise(mean_emission = sum(emissions, na.rm = TRUE)) %>%
    mutate(
      reportingYear = factor(reportingYear),
      mean_emission = round(mean_emission, 0)
    )
  q5 <<- temp %>%
    filter(pollutant %in% top2)
  q5alt <<- temp %>%
    filter(!pollutant %in% top2)
}
