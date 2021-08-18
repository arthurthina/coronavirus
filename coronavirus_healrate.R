#remotes::install_github("GuangchuangYu/nCov2019")
library(nCov2019)
library(dplyr)
library(ggplot2)
corona <- get_nCov2019(lang ='en')

plot(corona)

world_corona <- corona['global', ]
str(world_corona)
world_corona$healRate <- as.numeric(world_corona$healRate)


corona_filtered <- world_corona %>% 
  select(-showRate, -showHeal) %>% 
  filter(confirm >= 10)

print(corona_filtered)

ggplot(corona_filtered, aes(x = reorder(name, healRate), y = healRate)) +
      geom_col() +
      coord_flip() +
      xlab(NULL) + ylab("Heal Rate %") +
      labs(caption = paste("accessed date:", time(corona))) +
      theme_minimal()


