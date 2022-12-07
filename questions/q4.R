# library(tidyverse)
# library(ggplot2)
# library(dplyr)
# library(stringr)
# library(plotly)
# library(rcartocolor)
# library(gganimate)
# library(gifski)

# source('ingestion/data.R')
# source('util/create_ui.R')

q4_ui <- create_ui(
  index = 4, 
  question = "Did the UN Paris agreement have an impact on the amount of emissions? Has the increased, decreased or stayed the same?",
  conclusion = "A general observation made is that an overall decline in pollution levels can be observed in many of the plots created especially the <i>mean emission for each European region</i>. This plot specifically displays and emphasises a clear decline in emissions after 2015 proposing that the approval and implementation of the agreement had a positive impact by minimising overall air pollution in Europe. Furthermore, a more detailed perspective can be achieved by using some of the other solutions, specifically some of the other line plots, as they display the same tendency.",
  imageOutput("pollutionPlot4",
              width = "1000px",
              height = "100%") %>% withSpinner(color="#4363D8"),
)

q4_server <- function(input, output) {
  # Lineplot of summarized emissions 
  output$pollutionPlot4 <- renderImage({
    
    if (!file.exists("outfile.gif")) {
      grouped <- group_by(dat, region, reportingYear)
      aes <- aes(x = year,
                 y = emission,
                 color = region)
      meaned <-
        summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
      meaned <-
        rename(meaned, year = reportingYear, emission = mean_emission)
      
      anim <- ggplot(meaned, aes) +
        ggtitle("Mean emissions for each european region") +
        geom_point() +
        geom_line() +
        xlab("Year") +
        ylab("Mean Emission (1000x tons)") +
        scale_x_continuous(breaks = 2007:2020) +
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
        scale_color_manual(values = c(raw_cols)) +
        geom_vline(xintercept = 2015, linetype = "dotted", colour = "darkblue") +
        gganimate::transition_reveal(year) #+ 
      #gganimate::view_follow()
      a <- animate(anim, renderer = gifski_renderer(), width=1000)
      print(a) # print first
      
      anim_save(filename = "outfile.gif", animation = a) # works
    }
    
    # Return a list containing the filename
    list(src = "outfile.gif", contentType = "image/gif")
  }, deleteFile = FALSE)
}