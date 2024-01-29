# Loading libraries
library(tidyverse)
library(forecast)
library(lubridate)
library(car)
library(scales)
library(patchwork)
library(kableExtra)
library(prophet)
library(zoo)

# Adding a standard theme.
my_theme <- function(text_color) {
  theme_bw() +
    theme(text = element_text(size = 10, family = "Manjari", face = "bold", color = text_color),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5)
    )
}
data <- read.delim('ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt', comment.char = '#', header = F, sep = '', col.names = c('year','month','decimal_date','co2_average','co2_deseasonalized','days','st_dev_days', 'unc'))

# Let's have a first look at the data.
head(data)
summary(data)
# We've got data from 1958 until December 2023.

which(is.na(data))
# No Nas.

data$date <- ymd(paste0(data$year, " ", data$month, " ", "15"))
data_sel <- data %>% select(year, month, date, co2_average)

data_test <- data_sel %>% filter(year > 2020)
data_train <- data_sel %>% filter(year <= 2020)

ggplot(data_sel,aes(date, co2_average)) +
  geom_line(color='blue') +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "10 year") +
  xlab("Year-Month") +
  ylab("CO2 Concentration (ppm)") +
  my_theme(text_color = "black")
# The CO2 concentration in ppm is increasing exponentially.

# Let's check the growth rates.

# Calculate the monthly changes (comparing January with January last year, and so on)
data_sel <- data_sel %>%
  mutate(monthly_change = co2_average - lag(co2_average, n = 12))

# Calculate annual growth rate
annual_growth_rate <- data_sel %>%
  group_by(year) %>%
  summarize(avg_ppm = mean(monthly_change, na.rm = TRUE)) 

annual_growth_rate %>% 
  ggplot(aes(year, avg_ppm)) +
  geom_line() + 
  geom_hline(yintercept = 2) 
# It's increasing, with about 2.5ppm increase in 2023.
# This not very reliable though, I would still check original sources, such as
# https://www.climate.gov/news-features/understanding-climate/climate-change-atmospheric-carbon-dioxide 

# Let's model the future with ARIMA and Prophet.
# Both had been used in the acaedemic communities, such as 
# here https://journal.uii.ac.id/ENTHUSIASTIC/article/download/23131/13548/66534 

# Although, I attest the fact that the simplification of a complex problem as
# CO2 concentration is not entirely reasonable. The idea is to see whether
# the models do a good job of sensing the seasonal patterns and for myself
# to become aware of the future implications on my life.

co2_train <- ts(data_train$co2_average, start = c(1958,3), frequency = 12)
# I want to check two things: stationarity and correlations between time lags.
co2_train %>% ggtsdisplay(lag.max = 90)

# 1. The data is clearly not stationary having an increasing trend and seasonality.
# 2. The significant lag at 12 potentially suggests annual seasonality and 25 
# might hide some longer-term seasonality.

# Summary: 
# Slow decay in the ACF and significant lag at 1 in the PACF might suggest an AR(1) component.
# A significant lag at 1 in the PACF suggests a MA(1).
# And the significants lags in PACF might indicate a MA(2) in the seasonal part.
# And of course the 12 lag might indicate the seasonal part.

co2_train %>% diff() %>% diff(lag = 12) %>% ggtsdisplay(lag.max = 90)
# That looks stationary. Let's see what auto.arima says.

fit = auto.arima(co2_train)
print(fit)
# OK, so the auto.arima model actually taps into what I had just mentioned.
# Let's check if there is any trend left in the residuals.
residuals <- residuals(fit)
ljung_box_test <- Box.test(residuals, lag = 36, type = "Ljung-Box")
print(ljung_box_test)

# With a p-value > 0.05, there is not enough evidence to reject the null hypothesis. 
# Based on the Box-Ljung test, there is no significant autocorrelation in the residuals at the 
# specified lags, thus, we can say the ts is stationary.

# Forecasting ...
arima_forecast <- Co2_train %>%
  Arima(order = c(1,1,1), seasonal = list(order = c(2,1,2),period = 12),
        lambda = "auto"
  ) %>%
  forecast(h = 12 * 35) # next 35 years

arima_forecast %>%
  autoplot() +
  ylab("Co2 Concentration in PPM") + xlab("Year") +
  autolayer(Co2_test)

# ARIMA seems to drop its CO2 concentration growth rates in time.
# But I doubt that would be our case. ARIMA does not notice the strong increasing
# trend in CO2 concentration. Fair enough, it's also "a bit" far in time, so
# we cannot expect much from such a simple model.

df_arima_forecast <- data.frame(arima_forecast)

df_arima_forecast %>% 
  mutate(monthly_change = Point.Forecast - lag(Point.Forecast, n = 12)) %>% 
  summarise(rolling_avg = rollmean(monthly_change, na.rm = TRUE, k = 12)) %>% 
  ggplot(aes(rolling_avg)) +
  geom_histogram()

# Here, it's the rolling average for each year as a distribution and it's 
# mostly constant.

# With Prophet

# First adjusting the format of the data for Prophet standards.
data_ts_prophet <- data %>% 
  select(date, co2_average) %>% 
  rename(ds = date, y = co2_average)

prophet_model <- prophet()
prophet_fit <- add_seasonality(
  prophet_model,
  name = "monthly",
  period = 30.44,  # Average number of days in a month
  fourier.order = 10 # for yearly seasonality
)

prophet_fit <- fit.prophet(prophet_fit, data_ts_prophet)
future_dates <- make_future_dataframe(prophet_fit, periods = 12 * 30, freq = 'month') # next 30 years
forecast <- predict(prophet_fit, future_dates)
plot(prophet_fit, forecast)
# Very similar to ARIMA.

prophet_plot_components(prophet_fit, forecast)

forecast %>% 
  mutate(monthly_change = yhat - lag(yhat, n = 12)) %>% 
  summarise(rolling_avg = rollmean(monthly_change, na.rm = TRUE, k = 12)) %>% 
  ggplot(aes(rolling_avg)) +
  geom_histogram(bins = 50)



