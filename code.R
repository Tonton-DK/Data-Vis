library(tidyverse)
library(ggplot2)
library(dplyr)

dat <- read_csv("data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))

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
