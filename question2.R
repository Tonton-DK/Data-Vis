library(ggplot2)
library(ggridges)
library(gganimate)
library(gifski)
library(plotly)
library(tidyverse)
library(dplyr)

dat <- read_csv("data.csv")
dat <-
  dat %>% select(
    c(
      "countryName",
      "eprtrSectorName",
      "facilityName",
      "Longitude",
      "Latitude",
      "City",
      "pollutant",
      "emissions",
      "reportingYear"
    )
  )
dat <- dat %>% drop_na(eprtrSectorName)

grouped <- group_by(dat, eprtrSectorName, reportingYear)
meaned <-
  summarise(grouped, mean_emission = mean(emissions, na.rm = TRUE))
meaned <-
  rename(
    meaned,
    eprtrSectorName = eprtrSectorName,
    year = reportingYear,
    emission = mean_emission
  )

# Line plot
ggplotly(
  ggplot(meaned, aes(
    x = year,
    y = emission,
    color = eprtrSectorName
  )) +
    geom_line(size = 1) +
    geom_point() +
    scale_x_continuous(breaks = 2007:2020) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
    labs(
      title = "Sector pollution per year",
      x = "Year",
      y = "Emissions",
      color = "Sector"
    )
)

# bar
ranked_by_year <-
  meaned %>%
  group_by(year) %>%
  arrange(year) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 10)

# bar <-
#   ggplot(ranked_by_year) +
#   aes(xmin = 0 ,
#       xmax = emission / 1000000) +
#   aes(ymin = rank - .45,
#       ymax = rank + .45,
#       y = rank) +
#   geom_rect() +
#   aes(fill = eprtrSectorName) +
#   theme_bw() +
#   theme(axis.title.y = element_blank(), axis.ticks.y = element_blank()) +
#   # theme(legend.position = "none") +
#   labs(title = 'Sector pollution',
#        x = "Mean Emission",
#        y = NULL,
#        fill = "Sectors")
#
# bar

# https://evamaerey.github.io/little_flipbooks_library/racing_bars/racing_barcharts.html#10
meaned %>%
  # for each year we assign a rank
  group_by(year) %>%
  arrange(year, -emission) %>%
  # assign ranking
  mutate(rank = 1:n()) %>%
  filter(rank <= 10) ->
  ranked_by_year

ranked_by_year %>%
  ggplot() +
  aes(xmin = 0 ,
      xmax = emission / 1000000) +
  aes(ymin = rank - .45,
      ymax = rank + .45,
      y = rank) +
  facet_wrap( ~ year) +
  geom_rect() +
  aes(fill = eprtrSectorName) +
  scale_fill_viridis_d(option = "magma",
                       direction = -1) +
  scale_y_reverse() +
  labs(x = "Sectors", y = NULL, fill = NULL) +
  theme(legend.position = "right") ->
  bar

ggplotly(bar)

bar_anim <-
  bar +
  facet_null() +
  scale_x_continuous(limits = c(0, 400),
                     breaks = c(0, 100, 200, 300)) +
  geom_text(
    x = 100,
    y = 5,
    aes(label = as.character(year)),
    size = 20,
    col = "grey18"
  ) +
  aes(group = eprtrSectorName) +
  transition_time(year)

bar_anim

# animate(plot = bar_anim, fps = 30, duration = 15, renderer = gifski_renderer())


# bar <- ggplot(
#   meaned,
#   aes(
#     x = factor(eprtrSectorName),
#     y = emission / 1000000,
#     fill = eprtrSectorName
#   )
# ) +
#   geom_bar(stat = "identity") +
#   theme(legend.position = "none") +
#   labs(title = "Sector pollution",
#        x = NULL,
#        y = "Mean emission",
#        fill = "Sectors") +
#   coord_flip()
#
# ggplotly(bar)
#
# anim_bar <-
#   bar +
#   transition_states(year, transition_length = 2, state_length = 0) +
#   ease_aes('quadratic-in-out') +
#   labs(title = "Year {closest_state}") +
#   coord_flip()
#
# animate(anim_bar,
#         width = 600,
#         height = 300,
#         fps = 30)

# ridgeline plot
# ridge <-
#   ggplot(meaned,
#          aes(x = emission, y = eprtrSectorName, fill = eprtrSectorName)) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#   labs(title = 'Sector pollution', x = "Emissions", y = "Sector") +
#   theme_ridges() +
#   theme(legend.position="none",)
#
# ridge
