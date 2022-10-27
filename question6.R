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
  mutate(isCapital = 
           !is.na(City) & (
             (countryName == country & str_detect(City, capital)) 
             | (countryName == country & str_detect(City, capitalInLocal))
           )) %>%
  select(-c(capital, capitalInLocal, country))

#capitals <- dat %>% 
#    filter(isCapital == TRUE) %>%
#    distinct(countryName, City, isCapital)

#samp <- sample_n(dat, 50)

#plot(samp$reportingYear, dat$emissions, main = "Emissions each year",
#     xlab = "Emission Year", ylab = "Kg of pollutant",
#     pch = 19)S

# Question 6
############################################################################
q6 <- dat %>% group_by(countryName, isCapital) %>%
  summarise(mean_emission = mean(emissions, na.rm=TRUE)) %>%
  mutate(mean_emission = mean_emission / 10^6,
         isCapital = ifelse(isCapital, "Yes", "No"))
ggplot(q6, aes(x = isCapital, y = mean_emission)) +
  geom_bar(aes(), position = "dodge", stat="identity") +
  facet_wrap(~countryName, scales="free_x") +
  labs(x = "Pollution in capital", y = "Mean emission (million tons)")