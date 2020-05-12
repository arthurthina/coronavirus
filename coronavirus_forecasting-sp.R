library(ggplot2)
library(dplyr)
library(readxl)
library(prophet)
library(janitor)
library(scales)
library(readr)


#arquivo_geral <- read_delim("arquivo_geral.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#colnames(arquivo_geral)[which(names(arquivo_geral) == "data")] <- "ds"
#colnames(arquivo_geral)[which(names(arquivo_geral) == "casosAcumulados")] <- "y"

#arquivo_geral <- read_excel("arquivo_geral.xlsx")

brazil_covid <- read_csv("https://brasil.io/dataset/covid19/caso?format=csv")
brazil_covid[brazil_covid$confirmed == 306,]
brazil_covid$confirmed[brazil_covid$confirmed == 306 & brazil_covid$city == "São Paulo"] <- 358

corona_sp <- brazil_covid  %>% 
  filter(city == "São Paulo") %>% 
  rename(ds = "date",
         y = "confirmed") 
head(corona_sp)

m <- prophet(interval.width = 0.95) %>% 
  add_country_holidays(country_name = 'BR') %>% 
  fit.prophet(corona_sp)

future <- make_future_dataframe(m, periods = 22, freq = "day")
forecast <- predict(m, future)

plot(m, forecast) + 
  scale_y_continuous(labels = comma, breaks = seq(0, 130000, 5000)) +
  scale_x_datetime(date_breaks = "2 day", date_labels = "%d/%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "São Paulo City COVID19",
       subtitle = "Forecast Confirmed Cases",
       x = "Date",
       y = "Cumulative Confirmed Cases") 
 #prophet_plot_components(m, forecast)

sp_forecast <- forecast %>% 
  select(ds, yhat, yhat_lower, yhat_upper) %>% 
  filter(ds >= "2020-05-07" & ds <= "2020-06-01") 
print(sp_forecast, digits = 5)

df.cv <- cross_validation(m, initial = 30, period = 30, horizon = 22, units = 'days')
tail(df.cv)
df.p <- performance_metrics(df.cv)
summary(df.p)
plot_cross_validation_metric(df.cv, metric = 'rmse')

