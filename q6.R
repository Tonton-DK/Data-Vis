library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plotly)

q6_ui <- tabPanel("Question 6",
                  radioButtons("charttype", "Select chart type:",
                               c("Double axis" = "daxis",
                                 "Grouped" = "groupd")),
                  plotlyOutput("pollutionPlot6")
)

q6_server <- function(input, output){
  renderPlotly({
    q6 <- dat %>% select(countryName, isCapital, emissions) %>% group_by(countryName, isCapital) %>%
      summarise(mean_emission = mean(emissions, na.rm=TRUE)) %>%
      mutate(mean_emission = mean_emission / 10^6,
             isCapital = ifelse(isCapital, "Capital", "Non-capital"))
    
    if(input$charttype == "daxis") {
      brk = c(-400, -300, -200, -100, 0, 100, 200, 300, 400)
      lbl = c(400, 300, 200, 100, 0, 100, 200, 300, 400)
      q6alt <- mutate(q6, mean_emission = ifelse(isCapital == "Capital", mean_emission, mean_emission * -1))
      ggply <- ggplotly(ggplot(q6alt, aes(x = countryName, y = mean_emission, fill = isCapital)) +
        geom_col() +
        scale_y_continuous(limit = c(-400, 400), breaks = brk, labels = lbl, expand = c(0, 0)) +
        labs(x = "Country", y = "Mean emission (thousand tons)", fill = "Pollution in capital") +
        theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 0.5))
      )
    }
    else {
      q6 <- mutate(q6, mean_emission = ifelse(mean_emission > 0, mean_emission, 0))
      ggply <- ggplotly(ggplot(q6, aes(x = countryName, y = mean_emission, fill = isCapital)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_y_continuous(limit = c(0, 400), expand = c(0, 0)) +
        labs(x = "Country", y = "Mean emission (thousand tons)", fill = "Pollution in capital") +
        theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 0.5))
      )
    }
    
    ggply
  })
}