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
                         
                       ),
                       tabPanel(
                         title = "Dataset",
                         
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
