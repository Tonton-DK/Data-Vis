library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)
library(treemapify)

sector_ui <- tabPanel("Data by Sectors",

                      navlistPanel(
                        "Scope",
                        widths = c(2, 8),
                        tabPanel(
                          title = "Over time",
                          p("Look at me, i am a question!"),
                          plotlyOutput("pollutionPlot4",
                                       width = "1600px",
                                       height = "800px"),
                          p("Look at me, i am a conclusion!")
                        ),

                        tabPanel("Summarized",
                                 p("Look at me, i am a question!"),
                                 tabsetPanel(
                                   tabPanel(
                                     "Tree",
                                     plotOutput("pollutionPlot1337",
                                                width = "100%",
                                                height = "800px")
                                   ),
                                   tabPanel(
                                     "Top-10",
                                     plotlyOutput("pollutionPlot1338",
                                                  width = "100%",
                                                  height = "800px")
                                   )
                                 ),
                                 p("Look at me, i am a conclusion!"))
                      ))

q2_server <- function(input, output) {
  output$pollutionPlot1337 <- renderPlot({
    ggplot(q2,
           aes(
             area = emissions,
             fill = eprtrSectorName,
             label = eprtrSectorName
           )) +
      geom_treemap() +
      geom_treemap_text(
        colour = "white",
        place = "centre",
        size = 15,
        grow = TRUE
      ) +
      labs(x = NULL,
           y = NULL,
           fill = "Sectors")
  })

  output$pollutionPlot1338 <- renderPlotly({
    ggplotly(
      ggplot(
        q2_alt,
        aes(
          x = reorder(eprtrSectorName, emissions),
          y = emissions / 1000000,
          fill = eprtrSectorName
        )
      ) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Overall sector pollution", x = NULL, y = NULL) +
        theme(legend.position = "none") +
        scale_y_continuous(breaks = scales::breaks_extended(n = 15))
    )
  })
}

load_q2_data <- function() {
  dat <- read_csv("data/data.csv")
    dat <-
      dat %>% select(
        c(
          "countryName",
          "eprtrSectorName",
          "facilityName",
          "Longitude",
          "Latitude",
          "City",
          "pollutant",
          "emissions",
          "reportingYear"
        )
      )
    dat <- dat %>% drop_na(eprtrSectorName)

    q2 <<-
      dat %>%
      group_by(eprtrSectorName) %>%
      summarise(emissions = sum(emissions))

      dat <- read_csv("data/data.csv")
    dat <-
      dat %>% select(
        c(
          "countryName",
          "eprtrSectorName",
          "facilityName",
          "Longitude",
          "Latitude",
          "City",
          "pollutant",
          "emissions",
          "reportingYear"
        )
      )
    dat <- dat %>% drop_na(eprtrSectorName)

    q2_alt <<-
      dat %>%
      group_by(eprtrSectorName) %>%
      summarise(emissions = sum(emissions))
}

q4_server <- function(input, output) {
  output$pollutionPlot4 <- renderPlotly({
    ggplotly(
      ggplot(q4,
             aes(
               x = year,
               y = emission,
               color = sector
             )) +
        ggtitle("Mean emissions by industrial sector") +
        geom_point() +
        geom_line() +
        xlab("Reporting year") +
        ylab("Mean Emission (1000x tons)") +
        scale_x_continuous(breaks = 2007:2020) +
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
        scale_color_carto_d(palette = "Safe", direction = -1) +
      geom_vline(xintercept = 2015,
                 linetype = "dotted",
                 colour = "darkblue")
    )
  })
}

load_q4_data <- function() {
  grouped <- group_by(dat, eprtrSectorName, reportingYear)
    meaned <-
      summarize(grouped, mean_emission = mean(emissions, na.rm = TRUE))
    q4 <<-
      rename(meaned,
             sector = eprtrSectorName,
             year = reportingYear,
             emission = mean_emission)
}
