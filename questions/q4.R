# source('ingestion/data.R')
# source('util/create_ui.R')

q4_ui <- create_ui(
  index = 4, 
  question = "Did the UN Paris agreement have an impact on the amount of emissions? Has the increased, decreased or stayed the same?",
  conclusion = "",
  imageOutput("pollutionPlot4",
              width = "1200px",
              height = "800px"),
)

q4_server <- function(input, output) {
  # Lineplot of summarized emissions 
  output$pollutionPlot4 <- renderImage({
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
    a <- animate(anim, renderer = gifski_renderer())
    print(a) # print first
    
    anim_save(filename = "outfile.gif", animation = a) # works
    
    # Return a list containing the filename
    list(src = "outfile.gif", contentType = "image/gif")
  }, deleteFile = TRUE)
}