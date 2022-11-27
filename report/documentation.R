library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)

document_ui <- tabPanel("Documentation",
                        
                        navlistPanel(
                          "Download",
                          widths = c(2, 8),
                          tabPanel(
                            title = "Report",
                            downloadButton("downloadData", "Download Report")
                          )
                        ))

document_server <- function(input, output) {
  output$downloadData <- downloadHandler(
    filename = "test.txt",
    content = function(file) {
      file.copy("report/test.txt", file)
    }
  )
}
