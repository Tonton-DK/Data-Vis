library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

dat <- read_csv("data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))

reg <- read_csv("regions.csv")

# Question 4
grouped <- group_by(dat, eprtrSectorName, reportingYear)
meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
meaned <- rename(meaned, sector = eprtrSectorName, year = reportingYear, emission = mean_emission)
ggplotly(
  ggplot(
    meaned, 
    aes(
      x = year,
      y = emission,
      color=sector)) +
    geom_point() +
    geom_line() +
    xlab("Reporting year") +
    ylab("Mean Emission in Kg") +
    scale_x_continuous(breaks=2007:2020) +
    scale_y_continuous(breaks = scales::breaks_extended(n=15)) +
    scale_color_carto_d(palette="Safe",direction=-1))
