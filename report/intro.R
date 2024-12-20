library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)

intro_ui <- tags$div(
  tags$h1("Introduction"),
  tags$p(style="margin-top: 5rem;", HTML(
    "The environment is threatened like never before due to pollution being let out into the atmosphere. This sentiment has become more prevalent during the last several decades due to environmental changes starting to affect our day-to-day lives. Due to this, countries around the world have implemented several barriers by not only introducing CO2 quotas, but ways to limit the amount of pollutants released by different industrial sectors [<a href='#footnote1'>1</a>, <a href='#footnote2'>2</a>].<br><br>
    Whether this has actually made an impact on the pollutants released into the environment, remains to be determined. Different studies have been conducted in an effort to investigate the negative evolution observed in relation to the current environmental changes. This includes the collection of a wide variety of data from different industrial sectors. During the execution of the project, data will be collected and act as the foundation for the investigation into industrial emissions in the last 15 years. Based on the results, an effort will be made to try and conclude whether or not political intervention has had any positive effects environment-wise, with the main focus being a reduction in industrial emissions.<br><br>

Using data collected on European countries and their biggest industries, it will be possible to illustrate the evolution of pollutant emission for each country and sector, identifying potential positive or negative trends. Based on these observations, it should be possible to conclude which countries or sectors have the biggest impact on global air pollution as well as which are actively trying to reduce their pollutant emissions. The results will consist of visual representations of selected data where only air pollution will be the main focus. Different data-sets exist in which other types of pollutants, such as water and ground pollution have been collected and documented, however these will not part of this investigation.<br><br>

The results and analysis obtained by the execution of this project could potentially serve a wide variety of users; specifically, European politicians to advance their focus and research on emissions created by European countries, resulting in further efforts in order to minimise air pollution of the aforementioned countries. To make a difference, it is imperative to know the scope and severity of a given problem, which is what this project aims to illustrate with our results and report.")
  ),
  #tags$p(id="footnote1", HTML("[1] CPCC. Global warming of 1.5C. <a href='https://www.ipcc.ch/sr15/'>https://www.ipcc.ch/sr15/</a>, 2018. Accessed: 2022-10-26.")),
  #tags$p(id="footnote2", HTML("[2] Sam Meredith. ‘it’s now or never’: World’s top climate scientists issue ultimatum on critical temperature limit. <a href='https://www.cnbc.com/2022/04/04/ipcc-report-climate-scientists-issue-ultimatum-on-1point5-degrees-goal.html'>https://www.cnbc.com/2022/04/04/ipcc-report-climate-scientists-issue-ultimatum-on-1point5-degrees-goal.html</a>. Accessed: 2022-10-26.")),
  tags$p(HTML("<table style = 'margin-top: 1.5em;'>
                <tr>
                <td id = 'footnote1' style='padding-bottom: 0.3rem; padding-right:1rem; vertical-align: top; text-align: left;'>[1]</td>
                <td style='padding-bottom: 0.3rem; vertical-align: top; text-align: left;'>CPCC. Global warming of 1.5C. <a href='https://www.ipcc.ch/sr15/'>https://www.ipcc.ch/sr15/</a>, 2018. Accessed: 2022-10-26.</td>
                </tr>
                <tr>
                <td id = 'footnote2' style='padding-right:1rem; vertical-align: top; text-align: left;'>[2]</td>
                <td style='vertical-align: top; text-align: left;'>Sam Meredith. ‘it’s now or never’: World’s top climate scientists issue ultimatum on critical temperature limit. <a href='https://www.cnbc.com/2022/04/04/ipcc-report-climate-scientists-issue-ultimatum-on-1point5-degrees-goal.html'>https://www.cnbc.com/2022/04/04/ipcc-report-climate-scientists-issue-ultimatum-on-1point5-degrees-goal.html</a>. Accessed: 2022-10-26.</td>
                </tr>
                </table>")),
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
