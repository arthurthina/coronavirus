library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(readr)

#Fonte: Secretarias de Saúde das Unidades Federativas, dados tratados por Álvaro Justen e colaboradores/Brasil.IO
brazil_covid <- read_csv("covid19.csv")

# Checking and Updating a value
brazil_covid[brazil_covid$confirmed == 358,]
brazil_covid$confirmed[brazil_covid$confirmed == 306] <- 358


# Checking top 5 most affected cities
brazil_top <- brazil_covid %>% 
  group_by(city) %>% 
  arrange(desc(confirmed)) %>% 
  distinct(city, .keep_all = TRUE) 

head(brazil_top, 19)


# Preparing dataframe
city_filtered <- c("São Paulo", "Rio de Janeiro", "Fortaleza", "Manaus", "Brasília")
brazil_cities <- brazil_covid %>% 
  group_by(state) %>% 
  select(-city_ibge_code, -place_type) %>% 
  arrange(date) %>% 
  filter(city %in% city_filtered) %>% 
  mutate(diff_confirm = confirmed - lag(confirmed, default = first(confirmed)))

View(brazil_cities)

# Choosing colorblind-friendly palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#000000", "#0072B2", "#D55E00", "#CC79A7")

# Plotting the cumulative cases
ggplot(brazil_cities, aes(x = date, y = confirmed, col = city)) +
 geom_point() +
  geom_line() +
  theme_grey() +
  scale_x_date(name = "Time", date_breaks = "2 day", date_labels = "%d/%b") +
  scale_y_continuous(name = "New Confirmed Cases", breaks = seq(0, 8000, 500)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_colour_manual(values = cbp1) +
  theme(legend.justification = "right", legend.position = c(0.2, 0.78)) +
  labs(title = "Brazil COVID19 Cases",
       subtitle = "Top 5 most affected cities",
       color = "Cities",
       caption = "Fonte: Secretarias de Saúde das Unidades Federativas, 
       dados tratados por Álvaro Justen e colaboradores/Brasil.IO") 

# Plotting daily new cases confirmed
ggplot(brazil_cities, aes(x = date, y = diff_confirm, col = city)) +
 geom_point() +
  geom_line() +
  theme_grey() +
  scale_x_date(name = "Time", date_breaks = "2 day", date_labels = "%d/%b") +
  scale_y_continuous(name = "New Confirmed Daily Cases", breaks = seq(0, 700, 50)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_colour_manual(values = cbp1) +
  theme(legend.justification = "right", legend.position = c(0.2, 0.78)) +
  labs(title = "Brazil COVID19 Cases",
       subtitle = "Top 5 most affected cities - New Daily Cases Confirmed",
       color = "Cities",
       caption = "Fonte: Secretarias de Saúde das Unidades Federativas, 
       dados tratados por Álvaro Justen e colaboradores/Brasil.IO")