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
                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas non finibus dui, quis finibus massa. Proin eu elit ante. Morbi maximus nibh est, sed elementum nibh molestie consequat. Quisque scelerisque tincidunt mi ac consequat. Nullam lobortis, lacus in iaculis faucibus, dolor ante laoreet dolor, sed porttitor urna lacus vel tortor. Curabitur vitae turpis in purus tempor molestie. Donec laoreet, leo vitae maximus rutrum, lectus neque rutrum massa, ut aliquam dui libero vel felis. Vivamus lobortis, ligula at eleifend eleifend, massa turpis auctor turpis, et molestie sapien velit sed mi. Donec at sem dignissim, faucibus nibh et, sollicitudin orci."),
                         p("Praesent vitae lacinia mauris, at gravida est. Suspendisse imperdiet magna non ante maximus, nec ullamcorper leo varius. Donec pharetra eros lacus, vitae semper leo aliquam consequat. Aenean a nunc odio. Cras sit amet turpis eros. Suspendisse lacinia ipsum nec tincidunt congue. Mauris in odio leo. Donec a facilisis nisi. In in nulla diam. Sed vitae vulputate ipsum. Praesent id condimentum dui. Etiam id dolor quis urna ullamcorper vestibulum id at nibh. Donec mollis, tellus at posuere malesuada, ex dolor posuere lorem, quis vehicula neque justo quis tellus."),
                         p("Maecenas non diam ac tortor accumsan pellentesque. Suspendisse in quam lobortis, pulvinar sapien et, tempus neque. Pellentesque sem nibh, sagittis vel dui in, semper vehicula nisi. Donec placerat auctor augue quis tempor. Etiam iaculis at eros id condimentum. Donec tellus urna, tempus sit amet eros mattis, vulputate sollicitudin ipsum. Proin condimentum tincidunt arcu, vel posuere magna sodales venenatis. Ut gravida diam enim, ut dictum ante congue in. Cras vel dolor porta, elementum odio sed, eleifend nisl. Curabitur eu malesuada purus. Etiam et feugiat eros.")
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
