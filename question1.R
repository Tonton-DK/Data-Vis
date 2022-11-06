library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

dat <- read_csv("data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))
countries <- dat %>% distinct(countryName)

# Question 1
grouped <- group_by(dat, countryName) 
meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE)) 
meaned <- countries %>% left_join(meaned, by = "countryName")
mutated <- mutate(
  meaned,
  region = ifelse(countryName == "Czechia", "Czech Republic",
                  ifelse(countryName == "United Kingdom", "UK", countryName)))

mapdata <- map_data("world") %>% 
  inner_join(mutated, by = "region")
mapdata <- rename(mapdata, country = countryName, emission = mean_emission)

labels <- mapdata %>% 
  group_by(region) %>%  
  select(region, group, long, lat) %>%  
  summarise_all(mean)

ggply <- ggplotly(
  ggplot(
    mapdata, 
    aes(
      x = long, 
      y = lat, 
      group = group, 
      label = country)) + 
    geom_polygon(aes(fill = emission), color = "black") +
    scale_fill_gradient(
      name = "Mean emission", 
      low = "green", 
      high = "red", 
      na.value = "white", 
      breaks = scales::breaks_extended(n = 10)) +
    geom_text(data = labels, aes(label = region), colour = "blue", size = 3) + 
    scale_fill_carto_c(palette="Safe"))

ggply$x$data[[33]]$hoverinfo <- "skip"
ggply