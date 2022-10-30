library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)

dat <- read_csv("data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))
cap <- read_csv("capitals.csv")
cap <- cap %>% mutate(country = countryName)

# Question 1
grouped <- group_by(dat, countryName, reportingYear) 
meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE)) 

mapdata <- map_data("world") 
q6 <- dat %>%  
  group_by(countryName) %>%  
  summarise(mean_emission = mean(emissions, na.rm=TRUE)) %>%  
  mutate( 
    region = ifelse(countryName == "Czechia", "Czech Republic", ifelse(countryName == "United Kingdom", "UK", countryName)), 
    mean_emission = mean_emission / 10^6) 

mapdata <- left_join(mapdata, q6, by = "region") %>%  
  filter(!is.na(mean_emission)) 

labels <- mapdata %>% group_by(region) %>%  
  select(region, group, long, lat) %>%  
  summarise_all(mean) 

ggplotly(ggplot(mapdata, aes(x = long, y = lat, group = group)) + 
           geom_polygon(aes(fill = mean_emission), color = "black") + 
           scale_fill_gradient(name = "Mean emission (million tons)", low = "yellow", high = "red", na.value = NA) +  
           geom_text(data = labels, aes(label = region), size=2))
