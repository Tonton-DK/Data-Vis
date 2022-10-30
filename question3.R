library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)

dat <- read_csv("data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))

# Question 3 # Obs! Not all countries have full data for 2007 - 2020
grouped <- group_by(dat, countryName, reportingYear)
meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
meaned <- rename(meaned, country = countryName, year = reportingYear, emission = mean_emission)
ggplotly(ggplot(meaned, aes(
  x = year,
  y = emission,
  color=country)) +
  geom_point() +
  geom_line() +
  xlab("Reporting year") +
  ylab("Mean Emission") + 
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
  filter(rank <= 10)

my_plot <- ggplot(ranked_by_year) +  
  aes(xmin = 0 ,  
      xmax = mean_emission / 1000000) +  
  aes(ymin = rank - .45,  
      ymax = rank + .45,  
      y = rank) +  
  facet_wrap(~ reportingYear) +  
  geom_rect(alpha = .7) +  
  aes(fill = countryName) +  
  scale_fill_viridis_d(option = "magma", direction = -1) +  
  scale_x_continuous(
    limits = c(-400, 400),  
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

my_plot

my_anim_time <- my_plot +  
  facet_null() +  
  scale_x_continuous(
    limits = c(-150, 400),  
    breaks = c(0, 100, 200, 300, 400)) +  
  geom_text(x = 300, y = -10,  
            aes(label = as.character(reportingYear)),  
            size = 20, col = "grey18") +  
  aes(group = countryName) +  
  gganimate::transition_time(reportingYear)

my_anim_time
