library(readr)
library(dplyr)
library(nCov2019)
library(ggplot2)

gtrend <- read_csv("GoogleTrend_Latest.csv")
y <- load_nCov2019(lang = 'en', source = 'github')
corona <- y['global']

br_corona <- corona %>% 
  filter(country == "Brazil")

br_trend <- gtrend %>% 
  group_by(Country) %>% 
  filter(Country == "Brazil")

ggplot(br_trend, aes(x = date, y = log(coronavirus))) +
  geom_point() +
  geom_line() +
  geom_line(data = br_corona, aes(x = time, y = log(cum_confirm)), coloperfor = "red")
