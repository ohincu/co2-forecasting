# Loading libraries
library(tidyverse)
library(forecast)
library(lubridate)
library(car)
library(scales)
library(patchwork)
library(kableExtra)

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


co2_train <- ts(data_train$co2_average, start = c(1958,3), frequency = 12)
co2_train %>% ggtsdisplay()

co2_train %>% diff(lag = 12) %>% diff() %>% ggtsdisplay()
