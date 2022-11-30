library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)
library(readr)
library(paletteer)
library(scales)
library(tidyr)
library(ggthemes)
#library(RColorBrewer)

country_ui <- tabPanel(
  "Data by Countries",
  navlistPanel(
    "Scope",
    widths = c(2, 8),
    tabPanel(title = "Country by year",
             p("Look at me, i am a question!"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "yearId",
                   "Select a year",
                   min = 2007,
                   max = 2020,
                   value = 2007,
                   sep = "",
                   animate = TRUE
                 )
               ),
               mainPanel(tabsetPanel(
                 tabPanel(
                   "Top-10",
                   plotlyOutput("pollutionPlot2",
                                width = "800px",
                                height = "400px")
                 ),
                 tabPanel(
                   "Map",
                   plotlyOutput("pollutionPlot1",
                                width = "800px",
                                height = "800px")
                 )
               ))
             ),
             p("Look at me, i am a conclusion!")
    ),
    tabPanel(
      title = "Country over time",
      p("Look at me, i am a question!"),
      plotlyOutput("pollutionPlot32",
                   width = "1200px",
                   height = "800px"),
      p("Look at me, i am a conclusion!")
    ),
    tabPanel("Capital pollution summarized",
             p("Look at me, i am a question!"),
             fluidPage(fluidRow(
               column(
                 width = 2,
                 style = "margin-top: 47px; max-width: 140px;",
                 radioButtons(
                   "orderby",
                   "Order by:",
                   c("Capital" = "cap",
                     "Non-capital" = "ncap",
                     "Country")
                 )
               ),
               column(
                 width = 10,
                 plotlyOutput("pollutionPlot6",
                              width = "1200px",
                              height = "800px")
               )
             )),
             p("Look at me, i am a conclusion!")
    )
  )
)

q1_ui <- create_ui(
  index = 1, 
  question = "Which countries are responsible for the majority of pollution?", 
  control = sliderInput(
    "yearId",
    "Select a year",
    min = 2007,
    max = 2020,
    value = 2007,
    sep = "",
    animate = TRUE
  ),
  controlWidth = 240,
  plot = tabsetPanel(
    tabPanel(
      "Top-10",
      plotlyOutput(
        "pollutionPlot2",
        width = "800px",
        height = "400px"
      )
    ),
    tabPanel(
      "Map",
      plotlyOutput(
        "pollutionPlot1",
        width = "800px",
        height = "800px"
      )
    )
  )
)

q3_ui <- create_ui(
  index = 3, 
  question = "Is the capital the most polluted of the countries?", 
  control = radioButtons(
    "orderby",
    "Order by:",
    c("Capital" = "cap",
      "Non-capital" = "ncap")
  ),
  controlWidth = 140,
  plot = plotlyOutput(
    "pollutionPlot6",
    width = "1200px",
    height = "800px"
  )
)


q1_server <- function(input, output) {
  output$pollutionPlot1 <- renderPlotly({
    filtered = dat %>% filter(reportingYear == input$yearId)
    
    grouped <- group_by(filtered, countryName)
    meaned <-
      summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
    meaned <- countries %>% left_join(meaned, by = "countryName")
    mutated <- mutate(meaned,
                      region = ifelse(
                        countryName == "Czechia",
                        "Czech Republic",
                        ifelse(countryName == "United Kingdom", "UK", countryName)
                      ))
    
    mapdata <- map_data("world") %>%
      inner_join(mutated, by = "region")
    mapdata <-
      rename(mapdata, country = countryName, emission = mean_emission)
    
    labels <- mapdata %>%
      group_by(region) %>%
      select(region, group, long, lat) %>%
      summarise_all(mean)
    
    ggply <- ggplotly(
      ggplot(mapdata,
             aes(
               x = long,
               y = lat,
               group = group,
               label = country
             )) +
        geom_polygon(aes(fill = emission), color = "black") +
        geom_text(
          data = labels,
          aes(label = region),
          colour = "black",
          size = 3
        ) +
        scale_fill_gradient( 
          name = "Mean emission (1000x tons)",
          low = "white",  
          high = "red",
          na.value = "grey",  
          limits = c(0, 350),
          breaks = scales::breaks_extended(n = 10))
    )
    
    ggply$x$data[[33]]$hoverinfo <- "skip"
    ggply
  })
  
  output$pollutionPlot2 <- renderPlotly({
    grouped <- group_by(dat, countryName, reportingYear)
    meaned <-
      summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
    
    ranked_by_year <- meaned %>%
      # for each year we assign a rank
      group_by(reportingYear) %>%
      arrange(reportingYear,-mean_emission) %>%
      # assign ranking
      mutate(rank = 1:n()) %>%
      filter(rank <= 10) %>%
      filter(reportingYear == input$yearId)
    
    ggplotly(
      ggplot(ranked_by_year) +
        aes(xmin = 0 ,
            xmax = mean_emission) +
        aes(
          ymin = rank - .45,
          ymax = rank + .45,
          y = rank
        ) +
        geom_rect(alpha = .7) +
        aes(fill = countryName) +
        scale_fill_viridis_d(option = "Teal", direction = -1) +
        scale_x_continuous(
          limits = c(-150, 400),
          breaks = c(0, 100, 200, 300, 400)
        ) +
        geom_text(
          col = "gray13",
          hjust = "right",
          aes(label = countryName),
          x = -50
        ) +
        scale_y_reverse() +
        labs(fill = NULL) +
        labs(x = 'Mean Emission') +
        labs(y = "Rank") +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()
        )
    )
  })
  
  output$pollutionPlot32 <- renderPlotly({
    grouped <- group_by(dat, countryName, reportingYear)
    aes <- aes(x = year,
               y = emission,
               color = country)
    meaned <-
      summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
    meaned <-
      rename(meaned,
             country = countryName,
             year = reportingYear,
             emission = mean_emission)
    
    plt <- ggplot(meaned,
                  aes) +
      ggtitle("Mean emissions for each european country") +
      geom_point() +
      geom_line() +
      xlab("Year") +
      ylab("Mean Emission (1000x tons)") +
      scale_x_continuous(breaks = 2007:2020) +
      scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
      scale_color_manual(values = c(raw_cols)) +
      geom_vline(xintercept = 2015,
                 linetype = "dotted",
                 colour = "darkblue")
    
    ggplotly(plt)
  })
}

q6_server <- function(input, output) {
  output$pollutionPlot6 <- renderPlotly({
    ggply <- create_q6_plot(q6, input$orderby)
    ggply
  })
}

load_q6_data <- function() {
  temp <- distinct(dat, isCapital)
  q6 <- crossing(countries, temp)
  temp <- dat %>%
    select(countryName, isCapital, emissions) %>%
    group_by(countryName, isCapital) %>%
    summarise(mean_emission = mean(emissions, na.rm = TRUE))
  q6 <<- left_join(q6, temp) %>%
    mutate(
      NA_val = ifelse(!is.na(mean_emission), NA, 0),
      mean_emission = ifelse(isCapital, mean_emission * -1, mean_emission),
      order_cap = ifelse(isCapital, ifelse(is.na(mean_emission), 0, mean_emission), 0),
      order_ncap = ifelse(isCapital, 0, mean_emission),
      isCapital = ifelse(isCapital, "Capital", "Non-capital")
    )
  rm(temp)
}

create_q6_plot <- function(df, order) {
  lim = c(-400, 400)
  brk = c(-400,-300,-200,-100, 0, 100, 200, 300, 400)
  lbl = c(400, 300, 200, 100, 0, 100, 200, 300, 400)
  
  inner_plt <- ggplot(q6,
                      aes(
                        x = {
                          if (order == "cap") {
                            reorder(countryName, order_cap, decreasing = T)
                          }
                          else if (order == "ncap") {
                            reorder(countryName,
                                    order_ncap,
                                    decreasing = F,
                                    sum,
                                    order = T)
                          }
                          else {
                            countryName
                          }
                        },
                        y = mean_emission,
                        text = paste(
                          "Country:\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t",
                          countryName,
                          "<br>Mean emission:\t\t\t\t\t\t",
                          round(abs(mean_emission), 2),
                          "<br>Pollution in capital:\t",
                          isCapital
                        )
                      ))
  
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
