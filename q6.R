library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plotly)

q6_ui <- tabPanel("Question 6",
                  radioButtons("orderby", "Order by:",
                               c("Capital" = "cap",
                                 "Non-capital" = "ncap",
                                 "Country")),
                  plotlyOutput("pollutionPlot6")
)

q6_server <- function(input, output){
  renderPlotly({
    q6 <- dat %>% select(countryName, isCapital, emissions) %>% group_by(countryName, isCapital) %>%
      summarise(mean_emission = mean(emissions, na.rm=TRUE)) %>%
      mutate(mean_emission = mean_emission / 10^6,
             isCapital = ifelse(isCapital, "Capital", "Non-capital"))
    q6 <- q6 %>% mutate(mean_emission = ifelse(isCapital == "Capital", mean_emission, mean_emission * -1),
                        order_cap = ifelse(isCapital == "Capital", mean_emission, 0),
                        order_ncap = ifelse(isCapital == "Capital", 0, mean_emission))
    #q6 <- q6 %>% arrange(desc(mean_emission))
    lim = c(-400, 400)
    brk = c(-400, -300, -200, -100, 0, 100, 200, 300, 400)
    lbl = c(400, 300, 200, 100, 0, 100, 200, 300, 400)
    if(input$orderby == "cap") {
      plt <-ggplot(q6, aes(x = reorder(countryName, order_cap, decreasing = T), y = mean_emission, fill = isCapital))
    }
    else if (input$orderby == "ncap") {
      plt <- ggplot(q6, aes(x = reorder(countryName, order_ncap, decreasing = F, sum, order = T), y = mean_emission, fill = isCapital))
    }
    else {
      plt <- ggplot(q6, aes(x = countryName, y = mean_emission, fill = isCapital))
    }
    ggply <-ggplotly(plt +
                       geom_bar(stat='identity') +
                       scale_y_continuous(limit = c(-400, 400), breaks = brk, labels = lbl, expand = c(0, 0)) +
                       labs(x = "Country", y = "Mean emission (thousand tons)", fill = "Pollution in capital") +
                       theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 0.5))
    )
    ggply
  })
}