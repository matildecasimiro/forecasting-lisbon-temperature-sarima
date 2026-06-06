# Loading the necessary packages

library(fpp3)
library(urca)
library(readr)
library(lubridate)
library(dplyr)

# Importing the data and selecting the desired columns

temp_city <- read_csv("city_temperature.csv")

temp_lisbon <- temp_city %>%
  filter(City == "Lisbon") %>%
  group_by(Year, Month) %>%
  summarise(AvgTemperature = mean(AvgTemperature, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(AvgTemperatureC = (AvgTemperature - 32) * 5 / 9) %>%  # Convert to Celsius (Celsius=(Fahrenheit−32)× 9/5)
  select(Year, Month, AvgTemperatureC)

# Convert into a tsibble

lisbon_ts <- temp_lisbon %>%
  mutate(Date = yearmonth(paste(Year, Month, sep = "-"))) %>%
  as_tsibble(index = Date) %>%
  select(Date, AvgTemperatureC)

# Visualizing the original data

lisbon_ts %>%
  ggplot(aes(y=AvgTemperatureC, x=Date))+
  geom_line(color='black') + labs(
    title = 'Average Temperature by Month in Lisbon', 
    x= 'Months [1M]', y= 'Average Temperature (ºC)') + theme_minimal()

# There is a clear seasonal variation in temperature, with peaks during the summer months and troughs during the winter months.
# The average temperature in Lisbon over these two decades has varied between approximately 5 and 25 degrees Celsius.
# There doesn’t appear to be a significant trend of increasing or decreasing temperature over the 25-year period represented in the graph.

# Splitting the data into training and test sets
# Training until December 2015
# Testing on January 2016 until December 2019

temp_train <- lisbon_ts %>% filter(year(Date)<=2017)
temp_test <- lisbon_ts %>% filter(year(Date)>2017 & year(Date) < 2020)


# Performing the Augmented Dickey-Fuller (ADF) test to check if the time series is stationary, with 5% of significance level

temp_train %>% gg_tsdisplay(AvgTemperatureC, plot_type='partial') + 
  labs(title = 'Average Temperature by Month in Lisbon', 
       x = 'Months [1M]', y='Average Temperature (ºC)') 

summary(ur.df(na.omit(temp_train$AvgTemperatureC), type=c("none"), lags=22))

# As the series is still not stationary, we will proceed by taking seasonal differences.

temp_train <- temp_train %>%
  mutate(seasonal_diff = difference(AvgTemperatureC, 12)) %>% na.omit()

temp_train %>%
  ggplot(aes(y=seasonal_diff, x=Date))+
  geom_line(color='black') + labs(
    title = 'Average Temperature by Month in Lisbon', 
    x= 'Months [1M]', y= 'Average Temperature (ºC)') + theme_minimal()

temp_train %>% gg_tsdisplay(seasonal_diff, plot_type='partial') + 
  labs(title = 'Average Temperature by Month in Lisbon', 
       subtitle = "Seasonally Differenced",
       x = 'Months [1M]', y='Average Temperature (ºC)') 

summary(ur.df(na.omit(temp_train$seasonal_diff), type=c("none"), lags = 23)) 

# The series is now stationary. We can proceed to conduct a detailed analysis of the Autocorrelation Function (ACF) and Partial Autocorrelation Function (PACF)

temp_train %>% gg_tsdisplay(seasonal_diff, plot_type='partial', lag_max=48) + 
  labs(title = 'Average Temperature by Month in Lisbon', 
       subtitle = "Seasonally Differenced",
       x = 'Months [1M]', y='Average Temperature (ºC)') 

# Fitting Candidate SARIMA Models Based on Autocorrelation Function (ACF) and Partial Autocorrelation Function (PACF) analysis

temp_fit <- temp_train %>%
  model(
    sarima101_111 = ARIMA(AvgTemperatureC ~ 0 + pdq(1,0,1) + PDQ(1,1,1)),
    sarima101_211 = ARIMA(AvgTemperatureC ~ 0 + pdq(1,0,1) + PDQ(2,1,1)),
    sarima101_212 = ARIMA(AvgTemperatureC ~ 0 + pdq(1,0,0) + PDQ(2,1,2)),
    sarima201_111 = ARIMA(AvgTemperatureC ~ 0 + pdq(2,0,1) + PDQ(1,1,1)),
    sarima102_111 = ARIMA(AvgTemperatureC ~ 0 + pdq(1,0,2) + PDQ(1,1,1)),
    sarima101_110 = ARIMA(AvgTemperatureC ~ 0 + pdq(1,0,1) + PDQ(1,1,0)),
    sarima101_210 = ARIMA(AvgTemperatureC ~ 0 + pdq(1,0,1) + PDQ(2,1,0)),
    sarima101_011 = ARIMA(AvgTemperatureC ~ 0 + pdq(1,0,1) + PDQ(0,1,1)),
    sarima100_
  )

temp_fit %>%
  glance()

# Based on the AIC, AICc, and BIC criteria, the models we have chosen to retain and subsequently perform the Ljung-Box Test on are: sarima101_111, sarima101_212, sarima101_011 and auto_arima

# sarima101_111

temp_fit %>%
  select(sarima101_111) %>%
  gg_tsresiduals() + labs(title = "Residuals of SARIMA(1,0,1)(1,1,1) model", 
                          x = 'Months [1M]') # White Noise

temp_fit %>%
  select(sarima101_111) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 24) # No Autocorrelation in the residuals

# sarima101_212

temp_fit %>%
  select(sarima101_212) %>%
  gg_tsresiduals() + labs(title = "Residuals of SARIMA(1,0,1)(2,1,2) model", 
                          x = 'Months [1M]') # White Noise


temp_fit %>%
  select(sarima101_212) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 24) # No Autocorrelation in the residuals


# sarima101_011

temp_fit %>%
  select(sarima101_011) %>%
  gg_tsresiduals() + labs(title = "Residuals of SARIMA(1,0,1)(0,1,1) model", 
                          x = 'Months [1M]') # White Noise


temp_fit %>%
  select(sarima101_011) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 24) # No Autocorrelation in the residuals


# The selected models exhibit residuals resembling white noise and show no significant autocorrelation up to a lag of 24 months, as indicated by the Ljung-Box test.

best_fit <- temp_train %>%
  model(
    sarima101_111 = ARIMA(AvgTemperatureC ~ 0 + pdq(1,0,1) + PDQ(1,1,1)),
    sarima101_212 = ARIMA(AvgTemperatureC ~ 0 + pdq(1,0,0) + PDQ(2,1,2)),
    sarima101_011 = ARIMA(AvgTemperatureC ~ 0 + pdq(1,0,1) + PDQ(0,1,1))
  )


best_fit %>% forecast(h='2 years') %>%
  accuracy(temp_test)

# Considering all three metrics, it seems like sarima101_111 performs slightly better overall, as it has the lowest MAE and MAPE, and its RMSE is very close to the lowest value

fit_fc <- best_fit %>%
  forecast(temp_test)

# Upon examining the graph, it  is clear that the sarima101_111 model demonstrates better performance in attempting to make the most accurate forecast. However, there are instances where the model fails to accurately capture intense rises and falls.

# This could be attributed to external factors such as global warming, among others, that influence the data. These factors may not be adequately accounted for by the models. Nevertheless, the sarima101_111 model is the best option we have for attempting to predict the data.

fit_fc %>%
  autoplot(temp_test, level= NULL)


# Final Model 

best_fit %>%
  select(sarima101_111) %>% 
  forecast(h= '2 years') %>%
  autoplot(temp_train)



