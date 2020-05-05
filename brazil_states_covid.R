library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(readr)

# Source: Secretarias de Saúde das Unidades Federativas, dados tratados por Álvaro Justen e colaboradores/Brasil.IO
brazil_covid <- read_csv("https://brasil.io/dataset/covid19/caso?format=csv")

# Fixing São Paulo input(confirmed) on 2020-03-20
brazil_covid[brazil_covid$confirmed == 306,]
brazil_covid$confirmed[brazil_covid$confirmed == 306 & brazil_covid$city == "São Paulo"] <- 358
brazil_covid[brazil_covid$confirmed == 1647,]
brazil_covid$confirmed[brazil_covid$confirmed == 1647 & brazil_covid$city == "Recife"] <- 2282
brazil_covid[brazil_covid$confirmed == 1776,]
brazil_covid$confirmed[brazil_covid$confirmed == 1776 & brazil_covid$city == "Recife"] <- 2502

# Checking top 5 most affected cities
brazil_top <- brazil_covid %>% 
  group_by(city) %>% 
  arrange(desc(confirmed)) %>% 
  distinct(city, .keep_all = TRUE) 

head(brazil_top, 9)

# Preparing dataframe
city_filtered <- c("São Paulo", "Rio de Janeiro", "Fortaleza", "Manaus", "Recife")
brazil_cities <- brazil_covid %>% 
  group_by(state) %>% 
  select(-city_ibge_code, -place_type) %>% 
  arrange(date) %>% 
  filter(city %in% city_filtered) %>% 
  mutate(diff_confirm = confirmed - lag(confirmed, default = first(confirmed)))

head(brazil_cities)

# Choosing colorblind-friendly palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#000000", "#0072B2", "#D55E00", "#CC79A7")

# Plotting the cumulative cases
ggplot(brazil_cities, aes(x = date, y = confirmed, col = city)) +
 geom_point() +
  geom_line() +
  theme_grey() +
  scale_x_date(name = "Time", date_breaks = "2 day", date_labels = "%d/%b") +
  scale_y_continuous(name = "Confirmed Cases", breaks = seq(0, 45000, 500)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_colour_manual(values = cbp1) +
  theme(legend.justification = "right", legend.position = c(0.2, 0.78)) +
  labs(title = "Brazil COVID19 Cases",
       subtitle = "Top 5 most affected cities",
       col = "Cities",
       caption = "Source: Secretarias de Saúde das Unidades Federativas, 
       dados tratados por Álvaro Justen e colaboradores/Brasil.IO") 

# Plotting new daily cases
ggplot(brazil_cities, aes(x = date, y = diff_confirm, fill = city)) +
  geom_col() +
  theme_grey() +
  scale_x_date(name = "Time", date_breaks = "2 day", date_labels = "%d/%b") +
  scale_y_continuous(name = "New Daily Cases", breaks = seq(0, 3000, 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = cbp1) +
  theme(legend.justification = "right", legend.position = c(0.2, 0.78)) +
  labs(title = "Brazil COVID19 Cases",
       subtitle = "Top 5 most affected cities - New Daily Cases Confirmed",
       fill = "Cities",
       caption = "Source: Secretarias de Saúde das Unidades Federativas, 
       dados tratados por Álvaro Justen e colaboradores/Brasil.IO")

# Preparing dataframe for boxplot - Last 30 days kept and SP removed
box_br_cities <- brazil_cities %>% 
  filter(date > "2020-03-18" & city != "São Paulo") 
head(box_br_cities)

ggplot(box_br_cities, aes(x = city, y = diff_confirm, fill = city)) + 
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 900, 25)) +
  scale_fill_brewer(palette = "BuPu") +
  labs(title = "Brazil COVID19 Cases",
       x = "Cities",
       y = "New Daily Cases",
       fill = "Cities",
       caption = "Source: Secretarias de Saúde das Unidades Federativas, 
       dados tratados por Álvaro Justen e colaboradores/Brasil.IO") 
       
       