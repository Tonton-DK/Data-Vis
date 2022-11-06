library(ggplot2)
library(ggridges)
library(gganimate)
library(plotly)

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
# line <-
#   ggplot(meaned, aes(x = year,
#                      y = emission,
#                      color = eprtrSectorName)) +
#   geom_line(size = 1) +
#   geom_point() +
#   scale_x_continuous(breaks = 2007:2020) +
#   scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
#   labs(
#     title = "Sector pollution per year",
#     x = "Year",
#     y = "Emissions",
#     color = "Sector"
#   )
#
# ggplotly(line)

# violin
# violin <-
#   ggplot(meaned, aes(x = eprtrSectorName, y = emission)) +
#   geom_violin() +
#   scale_y_log10() +
#   labs(
#     title = "Sector pollution",
#     x = "eprtrSectorName",
#     y = "Emissions",
#     color = "Year"
#   )
#
# ggplotly(violin)

# strip
# strip <-
#   ggplot(meaned, aes(x = eprtrSectorName, y = emission, color = year)) +
#   geom_boxplot() +
#   geom_jitter(position = position_jitter(0.1)) +
#   scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
#   scale_color_continuous(breaks = 2007:2020)
#
# ggplotly(strip)

# bar <-
#   ggplot(meaned, aes(x = eprtrSectorName, y = emission, fill = year)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   scale_y_continuous(breaks = scales::breaks_extended(n = 15)) +
#   scale_fill_continuous(breaks = 2007:2020)
# bar

# bar <-
#   ggplot(data = meaned,
#          aes(x = eprtrSectorName, y = emission, fill = eprtrSectorName)) +
#   geom_bar(stat = "identity") +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   labs(title = 'Sector pollution per year: {floor(frame_time)}', x = "Sectors", y = "Emissions") +
#   transition_time(year) +
#   coord_flip() +
#   ease_aes('linear')
#
# animate(bar, renderer = ffmpeg_renderer())

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

# normal density
density <-
  ggplot(data = meaned,
         aes(x = emission, group = eprtrSectorName, fill = eprtrSectorName)) +
  scale_x_log10() +
  geom_density(adjust = 1, alpha = .5) +
  theme_minimal() +
  labs(title = 'Density sector pollution',
       x = "Emissions",
       y = "Density",
       fill = "Sectors")

ggplotly(density)
