library(nCov2019)
library(dplyr)
library(ggplot2)
library(tidyr)

y <- load_nCov2019(lang = 'en', source='github')
corona <- y['global']


br <- corona %>%
  filter(country == "Brazil" | country == "Italy" | country == "Iran" | country == "China" | country == "United States"
         | country == "Spain" | country == "Japan" | country == "France")
  

t <- corona %>% 
  distinct(country, .keep_all = TRUE) %>% 
  arrange(desc(time))
t


br_plot <- ggplot(br, aes(x = time, y = log(cum_confirm), col = country)) +
  geom_point() +
  geom_line() 

br_plot
