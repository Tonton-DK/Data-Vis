library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)
library(treemapify)


# source('ingestion/data.R')
# source('util/create_ui.R')

q6_ui <- create_ui(
  index = 6, 
  question = "Which industry sectors are responsible for the majority of the pollution?",
  conclusion = "By analysing the tree-map and bar chart, the group is able to 
  determine which industry sector is responsible for the majority of the 
  pollution in the EU.<br><br>
  
  Looking at the visualisations, it becomes evident which sector is the biggest 
  polluter - the energy sector. It is the clear outlier compared to the other 
  industry sectors, and there can be several causes as to why. This fits well 
  with the narrative and does not come as a surprise to the group as, compared 
  to other industries such Paper and Wood, the needs of the people on the planet 
  are becoming bigger, thus requiring a bigger effort in maintaining that need, 
  resulting in more waste and more pollution caused.<br><br>
  
  According to the European Environment Agency, energy processes in Europe are 
  responsible for 78% of total EU emissions. These processes are tightly linked 
  to the burning of fossil fuels for heating, electricity, transport in 
  industry. Despite fossil fuel burning being the majority of the pollution 
  released, there are multiple ways the energy sector can pollute.",
  plot = div(
    fluidRow(
      plotOutput(
        "pollutionPlot6a",
        width = "1000px",
        height = "500px"
      ) %>% withSpinner(color="#4363D8")
    ),
    fluidRow(
      style="margin-top:5rem;",
      plotlyOutput(
        "pollutionPlot6b",
        width = "1000px",
        height = "100%"
      ) %>% withSpinner(color="#4363D8")
    )
  )
)

q6_server <- function(input, output) {
  output$pollutionPlot6a <- renderPlot({
    ggplot(
      q6_data_a,
      aes(
        area = emissions,
        fill = emissions / 100000000,
        label = eprtrSectorName
      )
    ) +
      geom_treemap() +
      geom_treemap_text(
        colour = c(
          rep("white", 2),
          1, rep("white", 6)
        ),
        place = "centre", size = 15
      ) +
      scale_fill_viridis_c() +
      labs(
        x = NULL,
        y = NULL,
        fill = "Emission (1000x tons)"
      )
  })
  output$pollutionPlot6b <- renderPlotly({
    ggplotly(
      ggplot(
        q6_data_b,
        aes(
          x = reorder(eprtrSectorName, emissions),
          y = emissions / 100000000,
          text = paste(
            "Sector:",
            eprtrSectorName,
            "<br>Emission:",
            round(abs(emissions), 2)
          )
        )
      ) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(
          title = "Overall sector pollution",
          x = NULL,
          y = "Emission (1000x tons)"
        ) +
        theme(legend.position = "none") +
        scale_y_continuous(breaks = scales::breaks_extended(n = 15)),
      tooltip = c("text")
    )
  })
}

temp <-
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
temp <- temp %>% drop_na(eprtrSectorName)

q6_data_a <<-
  temp %>%
  group_by(eprtrSectorName) %>%
  summarise(emissions = sum(emissions))
rm(temp)

temp <-
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
temp <- temp %>% drop_na(eprtrSectorName)

q6_data_b <<-
  temp %>%
  group_by(eprtrSectorName) %>%
  summarise(emissions = sum(emissions))
rm(temp)