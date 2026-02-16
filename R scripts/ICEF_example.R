# Get data via SantimentR package for 4 different coins
# Calculate and visualize correlations between those coins
# Reshape the data to draw ggplot of prices
# Build a linear regression to predict next day's price
# Build an ARIMA model to predict next day's price
# Compare their performance
# Create an Rmarkdonw notebook based on the research
# Render the notebook into Word/Pdf file
# tinytex::install_tinytex()

library(tidyverse)
library(santimentR)
library(forecast) 

# Initialize client to work with Santiment API
api_key <- Sys.getenv("SANTIMENT_API_KEY")
client <- init_client(api_key = api_key) 

# Setting some parameters about the data we want to acquire
slugs <- c("bitcoin", "ethereum", "cardano", "solana")
from_date <- "2025-01-01T00:00:00Z"
to_date <- "2026-01-15T00:00:00Z"
interval <- "1d"

# Get all the data at once with map_df (tidyverse alternative to apply)
raw_data <- map_df(slugs, function(s) {
  get_metrics(client, metric = "price_usd", slug = s, 
              from_date = from_date, to_date = to_date, 
              interval = interval) %>%
    mutate(slug = s)
})

# Alternative way: getting time series for each slug separately
btc <- get_metrics(client, metric = 'price_usd',
                   slug = "bitcoin", 
                   from_date = from_date,
                   to_date = to_date,
                   interval = interval)

eth <- get_metric(client, metric = 'price_usd',
                  slug = "ethereum", 
                  from_date = from_date,
                  to_date = to_date,
                  interval = interval)

ada <- get_metric(client, metric = 'price_usd',
                  slug = "cardano", 
                  from_date = from_date,
                  to_date = to_date,
                  interval = interval)

sol <- get_metric(client, metric = 'price_usd',
                  slug = "solana", 
                  from_date = from_date,
                  to_date = to_date,
                  interval = interval)

raw_data <- bind_rows("btc"=btc, 
                      "eth"=eth, 
                      "ada"=ada, 
                      "sol"=sol, 
                      .id = "slug")

# Another way to have some overview of the data 
glimpse(raw_data)

# Separate graphs for the prices
ggplot(raw_data, aes(x = datetime, y = value, color = slug)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~ slug, scales = "free_y") + # Facet to see individual scales
  theme_minimal() +
  labs(title = "Cryptocurrency Price Trends (2025-2025)",
       x = "Date", y = "Price (USD)", color = "Asset")

# Pivot data from long to wide format for convenience
wide_data <- raw_data %>%
  pivot_wider(names_from = slug, values_from = value) %>%
  drop_na()

# Making correlation matrix the tidy way
wide_data %>% 
  select(-datetime) %>% 
  cor() -> cor_matrix

cor_matrix

# Simple correlation visualization from correlation matrix
corrplot::corrplot(cor_matrix, method = 'number', type = 'upper',
                   title = "Price Correlation Matrix")

# Fancy correlation visualization from the data in wide format
GGally::ggpairs(
  data = wide_data,
  columns = 2:5
) +
  tidyquant::scale_fill_tq() +
  tidyquant::scale_color_tq()


# Prediction --------------------------------------------------------------

# Let's use BTC as an example
# Getting the data
btc <- get_metrics(client, metric = 'price_usd',
                   slug = "bitcoin", 
                   from_date = from_date,
                   to_date = to_date,
                   interval = interval)

# Create target variable
btc_data <- btc %>% 
  arrange(datetime) %>%
  mutate(next_day_price = lead(value)) %>% # Our target variable
  drop_na()

lm_model <- lm(next_day_price ~ value, data = btc_data)
summary(lm_model)

par(mfrow = c(1, 2))
plot(lm_model, which = 1:2)

btc_ts <- ts(btc_data$value, frequency = 365)
arima_model <- auto.arima(btc_ts)
summary(arima_model)

arima_forecast <- forecast(arima_model, h = 1)
plot(arima_forecast)

lm_preds <- predict(lm_model, btc_data)
arima_preds <- fitted(arima_model)

rmse_lm <- sqrt(mean((btc_data$next_day_price - lm_preds)^2))
rmse_arima <- sqrt(mean((btc_data$value - arima_preds)^2))

performance_comparison <- tibble(
  Model = c("Linear Regression", "ARIMA"),
  RMSE = c(rmse_lm, rmse_arima)
)
print(performance_comparison)
