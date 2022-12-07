library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)

# intro_ui <- tabPanel("Documentation",
                     # 
                     # navlistPanel(
                     #   "Documentation",
                     #   widths = c(2, 8),
                     #   tabPanel(
                     #     title = "Introduction",
                     #     p("p creates a paragraph of text."),
                     #     p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
                     #     strong("strong() makes bold text."),
                     #     em("em() creates italicized (i.e, emphasized) text."),
                     #     br(),
                     #     code("code displays your text similar to computer code"),
                     #     div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
                     #     br(),
                     #     p("span does the same thing as div, but it works with",
                     #       span("groups of words", style = "color:blue"),
                     #       "that appear inside a paragraph.")
                     #   ),
                     #   tabPanel(
                     #     title = "Dataset",
                     #     p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas non finibus dui, quis finibus massa. Proin eu elit ante. Morbi maximus nibh est, sed elementum nibh molestie consequat. Quisque scelerisque tincidunt mi ac consequat. Nullam lobortis, lacus in iaculis faucibus, dolor ante laoreet dolor, sed porttitor urna lacus vel tortor. Curabitur vitae turpis in purus tempor molestie. Donec laoreet, leo vitae maximus rutrum, lectus neque rutrum massa, ut aliquam dui libero vel felis. Vivamus lobortis, ligula at eleifend eleifend, massa turpis auctor turpis, et molestie sapien velit sed mi. Donec at sem dignissim, faucibus nibh et, sollicitudin orci."),
                     #     p("Praesent vitae lacinia mauris, at gravida est. Suspendisse imperdiet magna non ante maximus, nec ullamcorper leo varius. Donec pharetra eros lacus, vitae semper leo aliquam consequat. Aenean a nunc odio. Cras sit amet turpis eros. Suspendisse lacinia ipsum nec tincidunt congue. Mauris in odio leo. Donec a facilisis nisi. In in nulla diam. Sed vitae vulputate ipsum. Praesent id condimentum dui. Etiam id dolor quis urna ullamcorper vestibulum id at nibh. Donec mollis, tellus at posuere malesuada, ex dolor posuere lorem, quis vehicula neque justo quis tellus."),
                     #     p("Maecenas non diam ac tortor accumsan pellentesque. Suspendisse in quam lobortis, pulvinar sapien et, tempus neque. Pellentesque sem nibh, sagittis vel dui in, semper vehicula nisi. Donec placerat auctor augue quis tempor. Etiam iaculis at eros id condimentum. Donec tellus urna, tempus sit amet eros mattis, vulputate sollicitudin ipsum. Proin condimentum tincidunt arcu, vel posuere magna sodales venenatis. Ut gravida diam enim, ut dictum ante congue in. Cras vel dolor porta, elementum odio sed, eleifend nisl. Curabitur eu malesuada purus. Etiam et feugiat eros.")
                     #   ),
                     #   tabPanel(
                     #     "Report",
                     #     downloadButton("downloadData", "Download")
                     #   )
                     # ))

intro_ui <- tags$div(
  tags$h1("Introduction"),
  tags$p(style="margin-top: 5rem;", HTML(
    "The environment is threatened like never before due to pollution being let out into the atmosphere. This sentiment has become more prevalent during the last decades due to environmental changes starting to affect our daily lives. Because of this, countries around the world have implemented different barriers and rules which include the introduction of solutions like CO2 quotas and other ways to limit the amounts of pollutants being released by different industrial sectors<br><br>
    Whether this has actually made an impact on the pollutants released into the environment remains to be examined. Different studies have been made in an effort to investigate the negative evolution observed in relation to the current environmental changes. This includes the collection of a wide variety of data from different industrial sectors. The data collected will be used during the execution of the following project which serves as the foundation of our investigation which will focus on different industries and their emission during the last 15 years. Based on the investigation an effort will be made to try and conclude whether or not political intervention has had any positive effects environment-wise, with the main focus being a reduction in industrial emissions.<br><br>

Using data collected on European countries and their biggest industries it will be possible to illustrate the evolution of pollutant emission for each country and sector, identifying potential positive or negative trends. Based on these observations it should be possible to conclude which countries or sectors have the biggest impact on global air pollution as well as which are actively trying to reduce their pollutant emissions. The results will consist of visual representations of selected data where only air pollution will be the main focus. Different data-sets exist in which other types of pollutants, such as water and ground pollution have been collected and documented, however these will not part of this investigation.<br><br>

The results and analysis obtained by the execution of this project could potentially serve a wide variety of users, more specifically by European politicians to advance the focus on emissions created by European countries and as an argument to make further efforts in minimising the air pollution of the aforementioned countries. To make a difference it is imperative to know the scope and severity of a given problem, which is what this project aims to illustrate with our results and report."), tags$a(href="#footnote1", "[1]"), "," ,tags$a(href="#footnote2", "[2]")
  ),
  tags$p(id="footnote1", "[1] CPCC. Global warming of 1.5C.", tags$a(href="https://www.ipcc.ch/sr15/", "https://www.ipcc.ch/sr15/"), ", 2018. Accessed: 2022-10-26."),
  tags$p(id="footnote2", "[2] Sam Meredith. ‘it’s now or never’: World’s top climate scientists issue ultimatum on critical temperature limit.", tags$a(href="https://www.cnbc.com/2022/04/04/ipcc-report-climate-scientists-issue-ultimatum-on-1point5-degrees-goal.html", "https://www.cnbc.com/2022/04/04/ipcc-report-climate-scientists-issue-ultimatum-on-1point5-degrees-goal.html"), ". Accessed: 2022-10-26."),
  tags$h4(style="margin-top: 1.5em;","Datset"),
  tags$a(href="https://www.eea.europa.eu/data-and-maps/data/industrial-reporting-under-the-industrial-6", "https://www.eea.europa.eu/data-and-maps/data/industrial-reporting-under-the-industrial-6"),
  tags$h4(style="margin-top: 1.5em;","Report"),
  tags$a(href="Download", "Download")
        
)

intro_server <- function(input, output) {
  output$downloadData <- downloadHandler(
    filename = "test.txt",
    content = function(file) {
      file.copy("report/test.txt", file)
    }
  )
}
