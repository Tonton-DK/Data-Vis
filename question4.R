library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)

dat <- read_csv("data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))
cap <- read_csv("capitals.csv")
cap <- cap %>% mutate(country = countryName)

# Question 4
grouped <- group_by(dat, eprtrSectorName, reportingYear)
meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
ggplotly(ggplot(meaned, aes(
  x = reportingYear,
  y = mean_emission,
  color=eprtrSectorName)) +
  geom_point() +
  geom_line() +
  xlab("Reporting year") +
  ylab("Mean Emission"))
