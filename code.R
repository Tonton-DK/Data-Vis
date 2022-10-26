library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)

dat <- read_csv("data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))
cap <- read_csv("capitals.csv")
cap <- cap %>% mutate(country = countryName)

# Flag capitals
dat <- add_column(dat, isCapital = "FALSE", .after = "City")
dat <- dat %>%
  left_join(cap) %>%
  mutate(isCapital = ((countryName == country & str_detect(City, capital)) | (countryName == country & str_detect(City, capitalInLocal)))) %>%
  select(-c(capital, capitalInLocal, country))

#capitals <- dat %>% 
#    filter(isCapital == TRUE) %>%
#    distinct(countryName, City, isCapital)

#samp <- sample_n(dat, 50)

#plot(samp$reportingYear, dat$emissions, main = "Emissions each year",
#     xlab = "Emission Year", ylab = "Kg of pollutant",
#     pch = 19)

# Question 3 # Obs! Not all countries have full data for 2007 - 2020
grouped <- group_by(dat, countryName, reportingYear)
meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))
ggplotly(ggplot(meaned, aes(
  x = reportingYear,
  y = mean_emission,
  color=countryName)) +
  geom_point() +
  geom_line() +
  xlab("Reporting year") +
  ylab("Mean Emission in tons"))

# Question 3 alternative
grouped <- group_by(dat, countryName, reportingYear)
yeared <- grouped %>% arrange(reportingYear, -emissions)
ranked <- mutate(meaned, rank = 1:n())




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
  ylab("Mean Emission in tons"))
