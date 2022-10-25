library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)

dat <- read_csv("data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))
cap <- read_csv("capitals.csv")
cap <- cap %>% mutate(country = countryName)

# Flag capitals
dat <- add_column(dat, isCapital = "FALSE", .after = "City")
dat <- dat %>%
  left_join(cap) %>%
  mutate(isCapital = (City == capital | (countryName == country & str_detect(City, capitalInLocal)))) %>%
  select(-c(capital, capitalInLocal, country))

#capitals <- dat %>% 
#    filter(isCapital == TRUE) %>%
#    distinct(countryName, City, isCapital)

#samp <- sample_n(dat, 50)

#plot(samp$reportingYear, dat$emissions, main = "Emissions each year",
#     xlab = "Emission Year", ylab = "Kg of pollutant",
#     pch = 19)

grouped <- group_by(dat, eprtrSectorName, reportingYear)
meaned <- summarize(grouped, 
          mean_emissions = mean(emissions, na.rm=TRUE))

ggplot(meaned, aes(
  x = reportingYear,
  y = mean_emissions,
  color=eprtrSectorName)) +
  geom_point() +
  geom_line()
