library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)

q5_ui <- tabPanel(
  "Question 5",
  plotlyOutput("pollutionPlot5", 
  width = "800px", 
  height = "800px")
)

q5_server <- function(input, output){
  renderPlotly({
    temp <- dat %>% 
      group_by(pollutant, reportingYear) %>%
      summarise(mean_emission = sum(emissions, na.rm=TRUE))
    top3 <- dat %>% 
      group_by(pollutant) %>%
      summarise(temp = mean(emissions, na.rm=TRUE)) %>%
      arrange(temp, .by_group = TRUE) %>%
      top_n(3)
    q5 <- merge(x = top3, y = temp, by = "pollutant", all.x = TRUE) %>%
      select(-temp) %>%
      mutate(reportingYear = factor(reportingYear),
             mean_emission = round(mean_emission, 0))
    rm(temp, top3)
    
    ggply <- ggplotly(ggplot(q5, aes(x = reportingYear,
                                     y = mean_emission, 
                                     group = pollutant, 
                                     color = pollutant,
                                     text = paste("Reporting year:\t\t", reportingYear,
                                                  "<br>Mean emission:\t", round(abs(mean_emission),2),
                                                  "<br>Pollutant:\t\t\t\t\t\t\t\t\t\t\t" , pollutant))) +
      geom_point() +
      geom_line() +
      labs(x = "Reporting year", 
           y = "Mean emission (1000x tons)", 
           color = "Pollutant"), tooltip = c("text")
    )
    ggply
  })
}