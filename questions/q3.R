# library(tidyverse)
# library(ggplot2)
# library(dplyr)
# library(stringr)
# library(plotly)
# library(rcartocolor)

# source('ingestion/data.R')
# source('util/create_ui.R')

q3_ui <- create_ui(
  index = 3, 
  question = "Is the capital the most polluted of the countries?",
  conclusion = "The general trend displayed in the dumbbell chart shows that for most countries the capital is the most polluted. The raw data includes every city; however, this visualisation only distinguishes between what is released in the capital and what is released outside the capital. One could think that such a grouping would result in non-capital pollution being significantly higher, although that is only the case with Estonia.<br><br>
For Belgium, Germany, and Poland it is almost a tie, although it is hard to tell; one would need to inspect the actual values (which is possible). Amsterdam, the capital of Netherlands is the most polluted city according to the data. Similarly, the plot shows that pollution outside the capital of Netherlands is greater than the majority of every other capital, which raises questions as to whether this is in fact the case or if the Netherlands simply is much better at reporting.",
  control = radioButtons(
    "orderby",
    "Order by:",
    c("Capital" = "cap",
      "Non-capital" = "ncap")
  ),
  controlWidth = 150,
  plot = plotlyOutput(
    "pollutionPlot3",
    width = "850px",
    height = "100%"
  ) %>% withSpinner(color="#4363D8")
)

q3_server <- function(input, output) {
  output$pollutionPlot3 <- renderPlotly({
    ggply <- create_q3_plot(q3_data, input$orderby)
    ggply
  })
}

create_q3_plot <- function(df, order) {
  lim = c(-400, 400)
  brk = c(-400,-300,-200,-100, 0, 100, 200, 300, 400)
  lbl = c(400, 300, 200, 100, 0, 100, 200, 300, 400)
  
  inner_plt <- ggplot(
    df,
    aes(
      x = {
        if (order == "cap") {
          reorder(countryName, order_cap, decreasing = T)
        }
        else if (order == "ncap") {
          reorder(
            countryName,
            order_ncap,
            decreasing = F,
            sum,
            order = T
          )
        }
      },
      y = mean_emission,
      text = paste(
        "Country: ",
        countryName,
        "<br>Pollution in capital: ",
        isCapital,
        "<br>Mean emission: ",
        round(abs(mean_emission), 2)
      )
    )
  )
  
  plt <- ggplotly(
    inner_plt +
      geom_bar(
        stat = "identity",
        width = 0.3,
        fill = "#989898"
      ) +
      scale_y_continuous(
        limit = c(-400, 400),
        breaks = brk,
        labels = lbl,
        expand = c(0, 0)
      ) +
      geom_point(aes(fill = isCapital), size = 3, stroke = 0) + 
      geom_point(
        aes(x = countryName, y = NA_val, fill = "NA"),
        size = 3,
        stroke = 0
      ) +
      scale_fill_manual(values = c("#2AA179", "#E6AB02", "#6A3D9A")) +
      labs(
        x = "Country",
        y = "Mean emission (1000x tons)",
        fill = "Pollution in capital",
        title = "Capital        Non-capital"
      ) +
      geom_hline(
        yintercept = 0,
        color = "black",
        alpha = 0.3
      ) +
      coord_flip() +
      theme(plot.title = element_text(hjust = 0.515)),
    tooltip = c("text")
  )
  
  return(plt)
}

# load data
temp <- distinct(dat, isCapital)
q3_data <- crossing(countries, temp)
temp <- dat %>%
  select(countryName, isCapital, emissions) %>%
  group_by(countryName, isCapital) %>%
  summarise(mean_emission = mean(emissions, na.rm = TRUE))
q3_data <<- left_join(q3_data, temp) %>%
  mutate(
    NA_val = ifelse(!is.na(mean_emission), NA, 0),
    mean_emission = ifelse(isCapital, mean_emission * -1, mean_emission),
    order_cap = ifelse(isCapital, ifelse(is.na(mean_emission), 0, mean_emission), 0),
    order_ncap = ifelse(isCapital, 0, mean_emission),
    isCapital = ifelse(isCapital, "Capital", "Non-capital")
  )
rm(temp)
