library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggthemr)
library(lubridate)

# Mudar o XLS

states <- read_xlsx("data_covid_state/states_august.xlsx", 
                     col_types = c("text", "text", "numeric", 
                                   "text", "text", "text", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "text")) %>% 
  hablar::retype()


states[is.na(states)] <- 0
states$age_group <- factor(states$age_group, levels = c("9-", "10-19", "20-29", "30-39", "40-49", "50-59", 
                                                           "60-69", "70-79", "80-89", "90-99", "100+", "NA"  ))

levels(states$age_group)

states %>% 
  filter(state == "SP" & age_group == "9-")


df_sp <- states %>% 
  filter(state == "SP" & date >= "2020-03-10")


ggthemr::ggthemr('earth', type = 'outer')
ggplot(df_sp, aes(x = date, y = deaths_covid19, color = age_group)) +
  geom_point(alpha = 0.8, size = 0.5) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  scale_y_continuous(breaks = seq(0, 150, 25)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "Date",
       y = "Deaths",
       color = "Age Group") +
 facet_wrap(~ age_group)


df_sp %>% 
  janitor::tabyl(place) %>% 
  ggplot(aes(x = place, y = n)) +
  geom_col()

df_sp_month <- df_sp %>% 
  group_by(month = floor_date(date, unit = "month"))

ggplot(df_sp_month, aes(x = date, y = deaths_covid19, color = age_group)) + 
  geom_bar(position = "stack",
           stat = "identity") +
  scale_x_date(date_labels = "%b %y", breaks = "month")
