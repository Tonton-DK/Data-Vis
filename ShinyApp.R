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
source('q5.R')
source('q6.R')

# Ensures data isn't reloaded constantly
load_q6_data()
load_q5_data()

ui <- navbarPage(
  title="My Application",
  q1_ui,
  tabPanel("Question 2"),
  q3_ui,
  q4_ui,
  q5_ui,
  q6_ui,
  tabPanel("Question 7"),
  tabPanel("Question 8")
)

server <- function(input, output) {
  q1_server(input, output)
  output$pollutionPlot3 <- q3_server(input, output)
  output$pollutionPlot4 <- q4_server(input, output)
  output$pollutionPlot5 <- q5_server(input, output)
  output$pollutionPlot6 <- q6_server(input, output)
}

shinyApp(ui,server)
