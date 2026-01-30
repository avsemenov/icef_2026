library(tidyverse)
library(tidyquant)

# Fetching financial data (Open-source API via tidyquant)
# We will use Apple (AAPL) as an example
df <- tq_get("AAPL", from = "2023-01-01", to = "2024-01-01") %>%
  as_tibble() # Ensure it is a tibble for improved behavior

# Tidyverse transformation: Calculate daily returns and binary direction as dependent variables for linear and logistic regressions
# Logic for log returns: log(Price / lag(Price))
df_tidy <- df %>%
  select(date, adjusted) %>%
  mutate(
    ret = log(adjusted / lag(adjusted)),
    direction = if_else(ret > 0, 1, 0), # Create a case for logistic regression: 1 if return is positive, 0 otherwise
    lag_ret = lag(ret)
  ) %>%
  drop_na() # Remove explicit missing values created by lagging

# 2. Visualizing Price Data before modelling
# Using the Grammar of Graphics, we map the date to the x-axis and the adjusted price to the y-axis using a line geom.
# Drawing ggplot line chart with prices data
ggplot(df_tidy, aes(x = date, y = adjusted)) +
  geom_line(color = "royalblue", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "AAPL Adjusted Closing Prices",
    subtitle = "Data retrieved from open-source API",
    x = "Date",
    y = "Price (USD)"
  )

# 3. Linear and Logistic Regression
# Following the guidelines from Lecture 3, we perform a linear regression for continuous outcomes and a binary logistic regression for classification.
# • Linear Regression: Modeling current returns based on lagged returns.
# • Logistic Regression: Predicting the probability of the market moving "Up" (direction = 1) based on the previous day's return.
# Linear Regression: Continuous outcome
model_linear <- lm(ret ~ lag_ret, data = df_tidy)

# Logistic Regression: Binary outcome (Classification)
model_logit <- glm(direction ~ lag_ret, data = df_tidy, 
                   family = binomial)

# Summary of models (Tidyverse style often uses 'broom' to tidy outputs)
summary(model_linear)
summary(model_logit)
graphics.off()

# 4. Visualizing Diagnostic Plots
# Diagnostic plots are essential to check assumptions like homoscedasticity and normality of residuals.
# Visualizing diagnostic plots for the Linear Model
# This generates: Residuals vs Fitted, Normal Q-Q, Scale-Location, and Residuals vs Leverage
par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(model_linear) 

# par(mar = c(2, 2, 2, 2))
