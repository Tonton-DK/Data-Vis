#if (!require("tidyverse")) install.packages("tidyverse")
#if (!require("ggplot2")) install.packages("ggplot2")
#if (!require("dplyr")) install.packages("dplyr")
#if (!require("stringr")) install.packages("stringr")
#if (!require("plotly")) install.packages("plotly")
#if (!require("patchwork")) install.packages("patchwork")
#if (!require("ggthemes")) install.packages("ggthemes")
#if (!require("devtools")) install.packages("devtools")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(patchwork)
library(ggthemes)
#devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)

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

#Question 6
############################################################################
ggthemr("flat")
q6 <- dat %>% group_by(countryName, isCapital) %>%
  summarise(mean_emission = mean(emissions, na.rm=TRUE)) %>%
  mutate(mean_emission = mean_emission / 10^6,
         isCapital = ifelse(isCapital, "Capital", "Non-capital"))
q6alt <- mutate(q6, mean_emission = ifelse(isCapital == "Capital", 
                                           mean_emission, mean_emission * -1))


# Bar chart with facets
p1 <- ggplot(q6, aes(x = isCapital, y = mean_emission, fill = isCapital)) +
  geom_bar(aes(), position = "dodge", stat="identity") +
  facet_wrap(~countryName, scales="free_x") +
  labs(x = "Pollution in capital", 
       y = "Mean emission (thousand tons)",
       fill = "")

# dumbbell'ish bar chart
p2 <- ggplot(q6alt, aes(x = countryName, y = mean_emission, fill = isCapital)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(limit = c(-400, 400), 
                     breaks = c(-400, -300, -200, -100, 0, 100, 200, 300, 400),
                     labels = c(400, 300, 200, 100, 0, 100, 200, 300, 400)) +
  labs(x = "Pollution in capital", 
       y = "Mean emission (thousand tons)", 
       fill = "")

p1 + p2