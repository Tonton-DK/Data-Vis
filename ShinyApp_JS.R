library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)
source('ingestion/data.R')
source('ingestion/style.R')

source('util/js_util.R')
source('util/create_ui.R')

source('processing/country.R')
source('processing/region.R')
source('processing/sector.R')
source('processing/emission.R')

source('report/intro.R')


# Ensures data isn't reloaded constantly
load_q6_data()
load_q5_data()
load_q3_data()
load_q2_data()
load_q4_data()

ui = fluidPage(
  tags$head(
    tags$style(HTML(css)),
    tags$script(HTML(js))
  ),
  tags$div(id = "sidenav", class="sidenav",
           # tags$a(href="javascript:void(0)", class="closebtn", onclick="closeNav()", "&times;"),
           tags$a(href="#doc", "Documentation"),
           tags$a(href="#q1", "Question 1"),
           tags$a(href="#q2", "Question 2"),
           tags$a(href="#q3", "Question 3"),
           tags$a(href="#q4", "Question 4"),
           tags$a(href="#q5", "Question 5"),
           tags$a(href="#q6", "Question 6"),
           tags$a(href="#q7", "Question 7"),
           tags$a(href="#q8", "Question 8")
  ),
  tags$div(id = "main",
           # tags$span(style="font-size:30px;cursor:pointer", onclick="openNav()", "&#9776; Sidenav"),
           tags$section(id="doc",
                        tags$h1("Documentation"),
                        intro_ui,
           ),
           q1_ui,
           q3_ui,
           q7_ui
          
  )
)

server <- function(input, output) {
  q1_server(input, output)
  q2_server(input, output)
  q3_server(input, output)
  q4_server(input, output)
  q5_server(input, output)
  q6_server(input, output)
  intro_server(input, output)
}

shinyApp(ui, server)
