library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)

q6_ui <- tabPanel("Question 6",
                  plotlyOutput("pollutionPlot6")
)

q6_server <- function(input, output){
  renderPlotly({
    q6 <- dat %>% group_by(countryName, isCapital) %>%
      summarise(mean_emission = mean(emissions, na.rm=TRUE)) %>%
      mutate(mean_emission = mean_emission / 10^6,
             isCapital = ifelse(isCapital, "Capital", "Non-capital"))
    q6alt <- mutate(q6, mean_emission = ifelse(isCapital == "Capital", 
                                               mean_emission, mean_emission * -1))
    
    ggply <- ggplotly(ggplot(q6alt, aes(x = countryName, 
                                        y = mean_emission, 
                                        fill = isCapital)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(limit = c(-400, 400), 
                         breaks = c(-400, -300, -200, -100, 0, 100, 200, 300, 400),
                         labels = c(400, 300, 200, 100, 0, 100, 200, 300, 400)) +
      labs(x = "Pollution in capital", 
           y = "Mean emission (thousand tons)", 
           fill = "")
    )
    ggply
  })
}