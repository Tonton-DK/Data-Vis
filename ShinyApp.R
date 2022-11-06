library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

source('data.R')
source('q1.R')
source('q3.R')
source('q4.R')

ui <- navbarPage(
  title="My Application",
  q1_ui,
  tabPanel("Question 2"),
  q3_ui,
  q4_ui,
  tabPanel("Question 5"),
  tabPanel("Question 6"),
  tabPanel("Question 7"),
  tabPanel("Question 8")
)

server <- function(input, output) {
  output$pollutionPlot1 <- q1_server(input, output)
  output$pollutionPlot3 <- q3_server(input, output)
  output$pollutionPlot4 <- q4_server(input, output)
}

shinyApp(ui,server)
