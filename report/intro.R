library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)

intro_ui <- tabPanel("Documentation",
                     
                     navlistPanel(
                       "Documentation",
                       widths = c(2, 8),
                       tabPanel(
                         title = "Introduction",
                         p("p creates a paragraph of text."),
                         p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
                         strong("strong() makes bold text."),
                         em("em() creates italicized (i.e, emphasized) text."),
                         br(),
                         code("code displays your text similar to computer code"),
                         div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
                         br(),
                         p("span does the same thing as div, but it works with",
                           span("groups of words", style = "color:blue"),
                           "that appear inside a paragraph.")
                       ),
                       tabPanel(
                         title = "Dataset",
                         p("p creates a paragraph of text."),
                         p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
                         strong("strong() makes bold text."),
                         em("em() creates italicized (i.e, emphasized) text."),
                         br(),
                         code("code displays your text similar to computer code"),
                         div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
                         br(),
                         p("span does the same thing as div, but it works with",
                           span("groups of words", style = "color:blue"),
                           "that appear inside a paragraph.")
                         
                       ),
                       tabPanel(
                         "Report",
                         downloadButton("downloadData", "Download")
                       )
                     ))

intro_server <- function(input, output) {
  output$downloadData <- downloadHandler(
    filename = "test.txt",
    content = function(file) {
      file.copy("report/test.txt", file)
    }
  )
}
