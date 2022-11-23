library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

source('data.R')
source('style.R')

source('q1.R')
source('q2.R')
source('q3.R')
source('q4.R')
source('q5.R')
source('q6.R')

# Ensures data isn't reloaded constantly
load_q6_data()
load_q5_data()
load_q3_data()

ui <- navbarPage(
  title="European Environment Agency Polution Reporting",
  q1_ui,
  q2_ui,
  q3_ui,
  q4_ui,
  q5_ui,
  q6_ui
)

server <- function(input, output) {
  q1_server(input, output)
  q2_server(input, output)
  q3_server(input, output)
  q4_server(input, output)
  q5_server(input, output)
  q6_server(input, output)
}

shinyApp(ui,server)
