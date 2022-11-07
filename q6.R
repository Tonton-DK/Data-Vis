library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plotly)

q6_ui <- tabPanel(
  "Question 6",
  radioButtons("orderby", "Order by:",
               c("Capital" = "cap",
                 "Non-capital" = "ncap",
                 "Country")
  ),
  plotlyOutput("pollutionPlot6")
)

q6_server <- function(input, output) {
  renderPlotly({
    temp <- distinct(dat, isCapital)
    q6 <- crossing(countries, temp)
    temp <- dat %>%
      select(countryName, isCapital, emissions) %>%
      group_by(countryName, isCapital) %>%
      summarise(mean_emission = mean(emissions, na.rm = TRUE))
    q6 <- left_join(q6, temp) %>%
      mutate(
        mean_emission = mean_emission / 10 ^ 6,
        NA_val = ifelse(!is.na(mean_emission), NA, 0),
        mean_emission = ifelse(isCapital, mean_emission * -1, mean_emission),
        order_cap = ifelse(isCapital, ifelse(is.na(mean_emission), 0, mean_emission), 0),
        order_ncap = ifelse(isCapital, 0, mean_emission),
        isCapital = ifelse(isCapital, "Capital", "Non-capital")
      )
    rm(temp)
    
    lim = c(-400, 400)
    brk = c(-400, -300, -200, -100, 0, 100, 200, 300, 400)
    lbl = c(400, 300, 200, 100, 0, 100, 200, 300, 400)
    
    if (input$orderby == "cap") {
      plt <- ggplot(
        q6,
        aes(
          x = reorder(countryName, order_cap, decreasing = T),
          y = mean_emission,
          text = paste(
            "Country:\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t", countryName,
            "<br>Mean emission:\t\t\t\t\t\t", round(abs(mean_emission), 2),
            "<br>Pollution in capital:\t", isCapital
          )
        ))
    }
    else if (input$orderby == "ncap") {
      plt <- ggplot(
        q6,
        aes(
          x = reorder(countryName, order_ncap, decreasing = F, sum, order = T),
          y = mean_emission,
          text = paste(
            "Country:\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t", countryName,
            "<br>Mean emission:\t\t\t\t\t\t", round(abs(mean_emission), 2),
            "<br>Pollution in capital:\t", isCapital
          )
        ))
    }
    else {
      plt <- ggplot(
        q6,
        aes(
          x = countryName,
          y = mean_emission,
          text = paste(
            "Country:\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t", countryName,
            "<br>Mean emission:\t\t\t\t\t\t", round(abs(mean_emission), 2),
            "<br>Pollution in capital:\t", isCapital
          )
        ))
    }
    
    ggply <- ggplotly(
      plt +
        geom_bar(
          stat = "identity",
          width = 0.5,
          fill = "#d4d4d4"
        ) +
        scale_y_continuous(
          limit = c(-400, 400),
          breaks = brk,
          labels = lbl,
          expand = c(0, 0)
        ) +
        geom_point(aes(fill = isCapital), size = 3, stroke = 0) +
        geom_point(aes(x = countryName, y = NA_val, fill = "NA"), size = 3, stroke = 0) +
        labs(
          x = "Country",
          y = "Mean emission (thousand tons)",
          fill = "Pollution in capital",
          title = "Capital        Non-capital"
        ) +
        geom_hline(
          yintercept = 0,
          color = "black",
          alpha = 0.3
        ) +
        coord_flip() +
        theme(
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.67)
        ),
      tooltip = c("text")
    )
    ggply
  })
}