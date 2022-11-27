library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

source('ingestion/data.R')
source('ingestion/style.R')

source('processing/country.R')
source('processing/region.R')
source('processing/sector.R')
source('processing/emission.R')

# Ensures data isn't reloaded constantly
load_q6_data()
load_q5_data()
load_q3_data()
load_q2_data()
load_q4_data()

ui <- navbarPage(title = "European Environment Agency Polution Reporting",
                 country_ui,
                 region_ui,
                 sector_ui,
                 emission_ui)

server <- function(input, output) {
  q1_server(input, output)
  q2_server(input, output)
  q3_server(input, output)
  q4_server(input, output)
  q5_server(input, output)
  q6_server(input, output)
}

shinyApp(ui, server)
