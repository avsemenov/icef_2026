# install.packages('fable')
install.packages("feasts")
library(tidyverse)
library(fable)
library(tsibble)
library(tsibbledata)

# 1. SETUP & SPLIT --------------------------------------------------------
# Convert to tsibble (time-series tibble)
data_ts <- as_tsibble(AirPassengers)

# Split by time index
train <- data_ts %>% filter_index(. ~ "1958 Dec")
test  <- data_ts %>% filter_index("1959 Jan" ~ .)

# 2. MODELING -------------------------------------------------------------
# We fit both models simultaneously using a formula interface
models <- train %>%
  model(
    # TSLM = Time Series Linear Model (handles trend/seasonality automatically)
    Linear_Reg = TSLM(value ~ trend() + season()),
    # ARIMA = Auto ARIMA
    ARIMA = ARIMA(value)
  )

# 3. FORECASTING ----------------------------------------------------------
# Forecast the length of the test set
forecasts <- models %>%
  forecast(test)

# 4. VISUALIZATION --------------------------------------------------------
# autoplot handles the complex binding of history + forecast automatically
forecasts %>%
  autoplot(train, level = NULL) + # level=NULL removes confidence intervals for clarity
  autolayer(test, color = "black") + # Add actual test data
  labs(title = "Fable Framework Comparison") +
  theme_minimal()

# 5. ACCURACY -------------------------------------------------------------
# Automatically computes RMSE, MAE, MAPE, etc.
accuracy_metrics <- accuracy(forecasts, data_ts) %>%
  select(.model, RMSE, MAE, MAPE) %>%
  arrange(RMSE)

print(accuracy_metrics)
