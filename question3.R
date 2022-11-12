library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(gganimate)
library(readr)
library(paletteer)
library(scales)

dat <- read_csv("data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))
reg <- read_csv("regions.csv")
# Color list from: https://github.com/DesiQuintans/Pebble-safe
# The colors are quite bright!
raw_cols <- c("#666666", "#A6761D", "#E6AB02", "#66A61E", "#E7298A", "#7570B3",
              "#D95F02", "#1B9E77", "#A6CEE3", "#B2DF8A", "#33A02C", "#A9A9A9",
              "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", 
              "#FFFF99", "#B15928", "#B3B3B3", "#E5C494", "#FFD92F", "#A6D854", 
              "#E78AC3", "#8DA0CB", "#FC8D62", "#66C2A5", "#CD7EAA", "#D5683A", 
              "#2A78B5", "#F1E54E", "#2AA179", "#62B6E6", "#E7A337", "#292929",
              "#FFE119", "#4363D8", "#F58231", "#FABEBE", "#E6BEFF", "#800000", 
              "#000075", "#FCFCFC")

# Question 3 # Obs! Not all countries have full data for 2007 - 2020
grouped <- group_by(dat, countryName, reportingYear)
meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
meaned <- rename(meaned, country = countryName, year = reportingYear, emission = mean_emission)
ggplotly(
  ggplot(
    meaned, 
    aes(
      x = year,
      y = emission,
      color=country)) +
    geom_point() +
    geom_line() +
    xlab("Reporting year") +
    ylab("Mean Emission") +
    geom_vline(xintercept = 2015, linetype="dotted", colour="darkblue") +
    scale_color_manual(values=c(raw_cols)) +
    scale_x_continuous(breaks=2007:2020) + 
    scale_y_continuous(breaks = scales::breaks_extended(n = 15) +
    geom_vline(xintercept = 2015, linetype="dotted", colour="darkblue")))
  

# Question 3 by region
regioned <- inner_join(dat, reg, by = "countryName")
grouped <- group_by(regioned, region, reportingYear)
meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
meaned <- rename(meaned, year = reportingYear, emission = mean_emission)
ggplotly(
  ggplot(
    meaned, 
    aes(
      x = year,
      y = emission,
      color=region)) +
    geom_point() +
    geom_line() +
    xlab("Reporting year") +
    ylab("Mean Emission") +
    geom_vline(xintercept = 2015, linetype="dotted", colour="darkblue") +
    scale_x_continuous(breaks=2007:2020) + 
    scale_y_continuous(breaks = scales::breaks_extended(n = 15)))

# Question 3 alternative (https://evamaerey.github.io/little_flipbooks_library/racing_bars/racing_barcharts.html)
grouped <- group_by(dat, countryName, reportingYear)
meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))

ranked_by_year <- meaned %>%  
  # for each year we assign a rank
  group_by(reportingYear) %>% 
  arrange(reportingYear, -mean_emission) %>%  
  # assign ranking
  mutate(rank = 1:n()) %>%  
  filter(rank <= 10) %>%  
  filter(reportingYear == 2018)

ggplotly(
ggplot(ranked_by_year) +  
  aes(xmin = 0 ,  
      xmax = mean_emission / 1000000) +  
  aes(ymin = rank - .45,  
      ymax = rank + .45,  
      y = rank) +  
  geom_rect(alpha = .7) +  
  aes(fill = countryName) +  
  scale_fill_viridis_d(option = "magma", direction = -1) +  
  scale_x_continuous(
    limits = c(-150, 400),  
    breaks = c(0, 100, 200, 300, 400)) +  
  geom_text(col = "gray13",  
            hjust = "right",  
            aes(label = countryName),  
            x = -50) +  
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = 'Mean Emission') +  
  labs(y = "Rank") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank())
)