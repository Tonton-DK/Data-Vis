library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(CGPfunctions)

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

# Question 5
############################################################################
temp <- dat %>% 
  group_by(pollutant, reportingYear) %>%
  summarise(mean_emission = mean(emissions, na.rm=TRUE))
top3 <- dat %>% 
  group_by(pollutant) %>%
  summarise(temp = mean(emissions, na.rm=TRUE)) %>%
  arrange(temp, .by_group = TRUE) %>%
  top_n(3)
q5 <- merge(x = top3, y = temp, by = "pollutant", all.x = TRUE) %>%
  select(-temp) %>%
  mutate(reportingYear = factor(reportingYear),
         mean_emission = round(mean_emission / 10^6), 0)
rm(temp, top3)

# All pollutants (messy)
#q5 <- dat %>% 
#  group_by(pollutant, reportingYear) %>%
#  summarise(mean_emission = mean(emissions, na.rm=TRUE)) %>%
#  mutate(mean_emission = round(mean_emission / 10^6), 0)

#newggslopegraph(q5, reportingYear, mean_emission, pollutant) +
#  labs(title = NULL, subtitle = NULL, caption = NULL)

# Slope in ggplot
ggplot(q5, aes(x = reportingYear,  y = mean_emission, group = pollutant, color = pollutant)) +
  geom_point() +
  geom_line() +
  labs(x = "Reporting year", y = "Mean emission (million tons)", color = "Pollutant")

# Bar chart
#ggplot(q5, aes(x = pollutant,  y = mean_emission, group = pollutant, fill = pollutant)) +
#  geom_col(show.legend = FALSE) + 
#  labs(y = "Mean emission (million tons)", x = "Pollutant") + 
#  coord_flip()
