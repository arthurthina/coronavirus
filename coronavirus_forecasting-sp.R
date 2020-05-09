library(ggplot2)
library(dplyr)
library(readxl)
library(prophet)
library(janitor)
library(scales)
library(readr)

#y <- load_nCov2019(lang = 'en', source = 'github')
#corona <- y['global']

arquivo_geral <- read_delim("arquivo_geral.csv", ";", escape_double = FALSE, trim_ws = TRUE)
colnames(arquivo_geral)[which(names(arquivo_geral) == "data")] <- "ds"
colnames(arquivo_geral)[which(names(arquivo_geral) == "casosAcumulados")] <- "y"

str(arquivo_geral)

corona_br <- arquivo_geral %>% 
  filter(estado == "SP") %>% 
  select(ds, y)

m <- prophet(interval.width = 0.95) %>% 
  add_country_holidays(country_name = 'BR') %>% 
  fit.prophet(corona_br)

future <- make_future_dataframe(m, periods = 25, freq = "day")
forecast <- predict(m, future)

plot(m, forecast) + 
  scale_y_continuous(labels = comma, breaks = seq(0, 130000, 5000)) +
  scale_x_datetime(date_breaks = "5 day", date_labels = "%d/%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "São Paulo Estate COVID19 Forecasting Confirmed Cases",
       x = "Date",
       y = "Cumulative Cases") 
 #prophet_plot_components(m, forecast)
sp_forecast <- forecast %>% 
  select(ds, yhat, yhat_lower, yhat_upper) %>% 
  filter(ds >= "2020-05-07" & ds <= "2020-06-01")

print(sp_forecast)

df.cv <- cross_validation(m, initial = 50, period = 30, horizon = 60, units = 'days')
head(df.cv)
df.p <- performance_metrics(df.cv)
summary(df.p)
plot_cross_validation_metric(df.cv, metric = 'rmse')
