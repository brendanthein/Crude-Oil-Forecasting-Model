
#=============================================================================#
#                                Packages                                     #
library(tidyverse)
library(janitor)
library(tsibble)
library(fpp3)
library(patchwork)
library(scales)
library(feasts)
library(fable)
#=============================================================================#

#VISUALIZATION

crudeoil_dataset <- read_csv("crudeoil_data.csv")

crudeoil_ts <- crudeoil_dataset %>% 
  clean_names() %>% 
  mutate(date = yearweek(date),price = wcoilbrenteu) %>% 
  select(date,price) %>% 
  as_tsibble(index = date)

crudeoil_ts <- crudeoil_ts %>% 
  mutate(price = as.numeric(price))

crudeoil_ts %>% autoplot() +
  labs(x = "Period (Weekly)",
       y = "Dollars per Barrel",
       title = "Crude Oil Prices: Brent-Europe",
       subtitle = "1987-2024",
       caption = "Source: Federal Reserve Economic Data (FRED)")

#Plot of raw data depicts a positive trend from 1987 to 2024. The behavior of the
#data appears to be cyclical with no seasonality. further analysis will be
#constructed through a STL decomposition

#DECOMPOSITION

crudeoil_ts %>% 
  model(stl = STL(price)) %>% 
  components(stl) %>%
  autoplot() +
  labs(x = "Period (Weekly)",
       y = "Dollars per Barrel",
       title = "Crude Oil Prices: Brent-Europe (STL Decomposition)",
       subtitle = "1987-2024",
       caption = "Source: Federal Reserve Economic Data (FRED)")
               
#From the seasonal-trend decomposition using loess we continue to see the
#the characteristics as observed in raw data plot. However when looking at the
#seasonal component we may now observe some extent of seasonality at differing time periods
#on the timeline that occurs every 52 weeks. Hence, we may state that the 
#seasonality is heteroskadastic due to variance.

#A log transformation will be used to reduce the variance in the time-series data set
#and further determine if seasonality is present
log_crudeoil_ts <- crudeoil_ts %>% mutate(price = log(price))
log_crudeoil_ts %>%
  model(stl = STL(price)) %>% 
  components(stl) %>%
  autoplot() +
  labs(x = "Period (Weekly)",
       y = "Dollars per Barrel (Percent Change)",
       title = "Log Transformed Crude Oil Prices: Brent-Europe (STL Decomposition)",
       subtitle = "1987-2024",
       caption = "Source: Federal Reserve Economic Data (FRED)") +
  scale_y_continuous(labels = percent_format(scale=100))

#There appears to be peaks in the percentage increase in price around every 10 
#years. However, we will use a seasonal plot as a final check

log_crudeoil_ts %>% gg_season(price) +
  labs(x = "Period (Weekly)",
       y = "Dollars per Barrel (Percent Change)",
       title = "Log Transformed Crude Oil Prices: Brent-Europe (STL Decomposition)",
       subtitle = "1987-2024",
       caption = "Source: Federal Reserve Economic Data (FRED)") +
  scale_y_continuous(labels = percent_format(scale=100))

#From the seasonal plot we observe no seasonality hence for the duration of the 
#forecasting process we will be using log transformed seasonally adjusted data 
#from the STL decomposition

log_crudeoil_ts_sa <- log_crudeoil_ts %>% 
  model(stl = STL(price)) %>% 
  components(stl) %>% 
  select(date,season_adjust)

log_crudeoil_ts_sa %>% autoplot() +
  labs(x = "Period (Weekly)",
       y = "Dollars per Barrel (Percent Change)",
       title = "Log Transformed Crude Oil Prices: Brent-Europe (Seasonally Adjusted)",
       subtitle = "1987-2024",
       caption = "Source: Federal Reserve Economic Data (FRED)") +
  scale_y_continuous(labels = percent_format(scale=100))

#IN-SAMPLE FORECASTING

#For the training set, we will be using log transformed seasonally adjusted data
#from 1987 W01 to 2022 W01 in order to fit a forecast model for future crude oil
#prices while reducing variance. The forecast will be back-transformed for 
#better interpretation.

#Separating Training and Test Sets
training_set <- crudeoil_ts %>% 
  filter_index(.~"2022 W01")
test_set <- crudeoil_ts %>% 
  filter_index("2022 W01"~.)

#Fitting model for log-transformed and seasonally adjusted data. The naive and
#random walk method with drift will be used. The naive method was decided on as
#crude oil prices tend to stay within a certain range in the short run unless
#an unexpected random event occurs. The random walk with drift method was decided
#on for the same reasons as the naive method except that it also accounts for the 
#increasing trend in crude oil prices over time.

rw_dcmp_model_fit <- training_set %>% 
  model(stlf_rw = decomposition_model(STL(log(price), robust = TRUE),
                                   SNAIVE(season_year),
                                   RW(season_adjust~drift())
                                   ))
naive_dcmp_model_fit <- training_set %>% 
  model(stlf_naive = decomposition_model(STL(log(price), robust = TRUE),
                                   SNAIVE(season_year),
                                   NAIVE(season_adjust)
                                   ))

#Plotting forecast
rw_plot <- rw_dcmp_model_fit %>% forecast(h=114) %>% autoplot(test_set) +
  labs(x = "Period (Weekly)",
       y = "Dollars per Barrel",
       title = "Crude Oil Prices: Brent-Europe (Random Walk Foreast)",
       subtitle = "Log Transformed and Seasonally Adjusted",
       caption = "Source: Federal Reserve Economic Data (FRED)")
  
naive_plot <- naive_dcmp_model_fit %>% forecast(h=114) %>% autoplot(test_set) +
  labs(x = "Period (Weekly)",
       y = "Dollars per Barrel",
       title = "Crude Oil Prices: Brent-Europe (Naive Forecast)",
       subtitle = "Log Transformed and Seasonally Adjusted",
       caption = "Source: Federal Reserve Economic Data (FRED)")

(rw_plot)/(naive_plot)

#ACCURACY

#After fitting the benchmark methods to the training data, accuracy measures
#will be used to determine the model with the best in-sample performance
rw_dcmp_model_fit %>% 
  forecast(h=114) %>% 
  accuracy(test_set) %>% 
  select(.model, MAE, RMSE, MAPE, MASE)

naive_dcmp_model_fit %>% 
  forecast(h=114) %>% 
  accuracy(test_set) %>% 
  select(.model, MAE, RMSE, MAPE, MASE)

#As indicated through the different accuracy measures, we find that that the
#log transformed seasonally adjusted naive method minimizes the errors in
#comparison to the random walk with drift method

#OUT-OF-SAMPLE FORECASTING

#Based on the results of the accuracy measures, the log-transformed seasonally
#adjusted method was decided upon as the best forecasting model for Brent-Europe
#crude oil prices

naive_dcmp_model_fit %>% forecast(h=156) %>% autoplot(crudeoil_ts) +
  labs(x = "Period (Weekly)",
       y = "Dollars per Barrel",
       title = "Crude Oil Prices: Brent-Europe (1 Year Forecast)",
       subtitle = "Log-Transformed Seasonally Adjusted Naive Method",
       caption = "Source: Federal Reserve Economic Data (FRED)")

naive_dcmp_model_fit %>% 
  gg_tsresiduals() +
  labs(title = "Plots for Naive Model residual analysis")

naive_dcmp_model_fit %>% 
  augment() %>% 
  features(.innov, box_pierce, lag = 10)

naive_dcmp_model_fit %>% 
  augment() %>% 
  features(.innov, ljung_box, lag = 10)
