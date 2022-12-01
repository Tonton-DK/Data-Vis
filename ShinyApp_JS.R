source('ingestion/data.R')
source('ingestion/style.R')
source('util/js_util.R')
source('util/create_ui.R')
source('questions/q1.R')
source('questions/q2.R')
source('questions/q3.R')
source('questions/q4.R')
source('questions/q5.R')
source('questions/q6.R')
source('questions/q7.R')
source('report/intro.R')

ui = fluidPage(
  tags$head(tags$style(HTML(css)),
            tags$script(HTML(js))),
  tags$div(
    id = "sidenav",
    class = "sidenav",
    tags$a(href = "#doc", "Documentation"),
    tags$a(href = "#q1", "Question 1"),
    tags$a(href = "#q2", "Question 2"),
    tags$a(href = "#q3", "Question 3"),
    tags$a(href = "#q4", "Question 4"),
    tags$a(href = "#q5", "Question 5"),
    tags$a(href = "#q6", "Question 6"),
    tags$a(href = "#q7", "Question 7")
  ),
  tags$div(
    id = "main",
    tags$section(id="doc",
                 tags$h1("Documentation"),
                 intro_ui,
    ),
    q1_ui,
    q2_ui,
    q3_ui,
    q4_ui,
    q5_ui,
    q6_ui,
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
  q7_server(input, output)
  intro_server(input, output)
}

shinyApp(ui, server)
