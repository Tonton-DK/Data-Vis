# install.packages("ggplot2", "dplyr", "stringr", "plotly", "readr")
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(readr)
library(tidyverse)
library(paletteer)
library(scales)


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

grouped <- group_by(dat, eprtrSectorName, reportingYear)
meaned <- summarize(grouped, 
                    mean_emissions = mean(emissions, na.rm=TRUE))

#Not all countries have full data for 2007 - 2020 (Serbia is problematic)
#scale_x/y_discrete(name, breaks(min, max, interval), labels, limits)
#The scales package could also be used instead of scale_x/y_discrete
#Maybe show emissions in tons and not kg?
grouped <- group_by(dat, countryName, reportingYear)
meaned <- summarize(grouped, mean_emission = mean(emissions, na.rm=TRUE))

#Custom color list (could be improved)
color_list <- c("#000000", "#8cfffb", "#00a8f3", "#3f48cc", "#009e73", "#FFC20A", 
                "#f0e442", "#d55e00", "#994F00", "#006CD1", "#ee6677", "#e69f00",
                "#cc79a7", "#ee6677", "#228833", "#4477aa", "#ccbb44", "#aa3377",
                "#bbbbbb", "#332288", "#999933", "#bbcc33", "#77aadd", "#44bb99",
                "#837171", "#bb8b00", "#f3b400", "#dc267f", "#a94397", "#147331",
                "#ffb102", "#9482e5", "#332288")

# Color list from: https://github.com/DesiQuintans/Pebble-safe
# The colors are quite bright!
raw_cols <- c("#666666", "#A6761D", "#E6AB02", "#66A61E", "#E7298A", "#7570B3", 
              "#D95F02", "#1B9E77", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", 
              "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", 
              "#FFFF99", "#B15928", "#B3B3B3", "#E5C494", "#FFD92F", "#A6D854", 
              "#E78AC3", "#8DA0CB", "#FC8D62", "#66C2A5", "#CD7EAA", "#D5683A", 
              "#2A78B5", "#F1E54E", "#2AA179", "#62B6E6", "#E7A337", "#292929",
              "#FFE119", "#4363D8", "#F58231", "#FABEBE", "#E6BEFF", "#800000", 
              "#000075", "#FCFCFC", "#A9A9A9")

#m_color_range <- c(color_list)
#geom_vline(): xintercept
#geom_hline(): yintercept
#geom_abline(): slope and intercept
line_plot = ggplotly(ggplot(meaned, aes(
  x = reportingYear,
  y = mean_emission,
  color=countryName)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name="Year", breaks=seq(2007,2020, 1)) +
    #ylab("Mean Emission (tons)"))
    scale_y_continuous(name="Mean Emissions (kg)", breaks=seq(0e+00, 3.5e+08, 0.5e+08)) +
    geom_vline(xintercept = 2015, linetype="dotted", colour="darkblue") + 
    #scale_color_brewer(palette = "BuPu")
    scale_color_manual(values=c(raw_cols)))

line_plot





