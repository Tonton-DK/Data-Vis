library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(rcartocolor)

dat <- read_csv("data/data.csv")
dat <- dat %>% select(c("countryName","eprtrSectorName","facilityName","Longitude","Latitude","City","pollutant","emissions","reportingYear"))
regions <- read_csv("data/regions.csv")
dat <- inner_join(dat, regions, by = "countryName")
countries <- dat %>% distinct(countryName)
capitals <- read_csv("data/capitals.csv")
capitals <- capitals %>% mutate(country = countryName) # 

# Flag capitals
dat <- add_column(dat, isCapital = "FALSE", .after = "City")
dat <- dat %>%
  left_join(capitals) %>%
  mutate(
    isCapital = 
      !is.na(City) & (
        (countryName == country & str_detect(City, capital)) 
          | (countryName == country & str_detect(City, capitalInLocal))),
    emissions = (emissions / 10^6)
    ) %>%
  select(-c(capital, capitalInLocal, country))
capitals <- select(capitals, -c(country))