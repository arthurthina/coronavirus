library(nCov2019)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

y <- load_nCov2019(lang = 'en', source = 'github')
corona <- y['global']


countries <- c("Brazil", "Italy", "Iran", "China",  "United States", "Spain", "Japan", "Germany")
br <- corona %>%
  filter(country %in% countries)

head(br)
  
br_plot <- ggplot(br, aes(x = time, y = cum_confirm, col = country)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  scale_colour_brewer(palette = "Spectral") +
  theme_minimal()

br_plot