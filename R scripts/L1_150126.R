# DATA ----
# Generate sample time-series data
set.seed(123)
dates <- seq(as.Date("2023-01-01"), by = "day", 
             length.out = 365)
values <- rlnorm(365, meanlog = 5, sdlog = 2)  # Log-normal (often follows Benford)

# Create data frame with time series
ts_data <- data.frame(date = dates, value = values)


# BASE R ------------------------------------------------------------------
## One-liner ---------------------------------------------------------------
barplot(prop.table(table(floor(ts_data$value/10^floor(log10(ts_data$value)))))[1:9])

## Direct approach without function ----------------------------------------

ts_data$first_digit <- floor(ts_data$value / 10^floor(log10(ts_data$value)))

# Filter valid digits
valid_digits <- ts_data$first_digit[ts_data$first_digit %in% 1:9]

# Create summary data frame
digit_counts <- as.data.frame(table(digit = valid_digits))
digit_counts$proportion <- digit_counts$Freq / sum(digit_counts$Freq)

# Simple bar chart
barplot(digit_counts$proportion, 
        names.arg = digit_counts$digit,
        col = "lightgreen",
        main = "First Digit Distribution in Time Series",
        xlab = "First Digit",
        ylab = "Frequency")

# Show the counts
print(digit_counts)



# Short Benford function
benford_short <- function(values) {
  # Get first digits (ignore 0, negatives, NA)
  valid_vals <- values[values > 0 & !is.na(values)]
  first_digits <- floor(valid_vals / 10^floor(log10(valid_vals)))
  
  # Count occurrences of digits 1-9
  digit_counts <- table(factor(first_digits, levels = 1:9))
  
  # Create data frame
  result_df <- data.frame(
    digit = 1:9,
    count = as.numeric(digit_counts),
    proportion = as.numeric(digit_counts) / length(first_digits)
  )
  
  # Simple bar chart
  barplot(result_df$proportion, 
          names.arg = result_df$digit,
          col = "steelblue",
          main = "Benford's Law: First Digit Distribution",
          xlab = "First Digit",
          ylab = "Proportion",
          ylim = c(0, max(result_df$proportion) * 1.2))
  
  # Add expected Benford line (optional)
  expected <- log10(1 + 1/1:9)
  points(1:9 - 0.5, expected, type = "b", col = "red", lwd = 2, pch = 16)
  legend("topright", legend = c("Observed", "Expected"), 
         fill = c("steelblue", NA), col = c(NA, "red"), lty = c(NA, 1))
  
  return(result_df)
}

# Run the analysis
benford_df <- benford_short(ts_data$value)

# Show the data frame
print(benford_df)

# Optional: Compare with expected proportions
benford_df$expected <- log10(1 + 1/1:9)
print(benford_df)

## Even Shorter One ----
# Minimalist version
benford_minimal <- function(values) {
  # Get first digits
  v <- values[values > 0 & !is.na(values)]
  digits <- floor(v / 10^floor(log10(v)))
  
  # Create data frame with counts
  df <- as.data.frame(table(digit = factor(digits, levels = 1:9)))
  df$proportion <- df$Freq / sum(df$Freq)
  
  # Quick bar plot
  barplot(df$proportion, names.arg = df$digit, 
          col = "skyblue", main = "First Digit Frequencies")
  
  return(df)
}

# Use it
result <- benford_minimal(ts_data$value)
print(result)




# TIDYVERSE ---------------------------------------------------------------
library(tidyverse)

# Generate sample time-series data
set.seed(123)
ts_data <- tibble(
  date = seq(as.Date("2023-01-01"), by = "day", length.out = 365),
  value = rlnorm(365, meanlog = 5, sdlog = 2)
)

# Benford analysis in tidyverse style
benford_results <- ts_data %>%
  filter(value > 0, !is.na(value)) %>%
  mutate(
    first_digit = floor(value / 10^floor(log10(value)))
  ) %>%
  filter(first_digit %in% 1:9) %>%
  count(first_digit, name = "observed") %>%
  complete(first_digit = 1:9, fill = list(observed = 0)) %>%
  mutate(
    proportion = observed / sum(observed),
    expected = log10(1 + 1/first_digit),
    deviation = proportion - expected
  )

# Print the results
benford_results

# Simple ggplot bar chart
benford_results %>%
  ggplot(aes(x = factor(first_digit))) +
  geom_col(aes(y = proportion), fill = "steelblue", alpha = 0.7) +
  geom_point(aes(y = expected), color = "red", size = 3) +
  geom_line(aes(y = expected, group = 1), color = "red", linewidth = 1) +
  labs(
    title = "Benford's Law: First Digit Distribution",
    subtitle = "Blue bars = Observed, Red line = Expected",
    x = "First Digit",
    y = "Proportion"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


## More concise version ----------------------------------------------------
library(tidyverse)

# Generate and analyze in one pipe
benford_plot <- tibble(
  date = seq(as.Date("2023-01-01"), by = "day", length.out = 365),
  value = rlnorm(365, meanlog = 5, sdlog = 2)
) %>%
  filter(value > 0) %>%
  mutate(digit = floor(value / 10^floor(log10(value)))) %>%
  filter(digit %in% 1:9) %>%
  count(digit, name = "count") %>%
  complete(digit = 1:9, fill = list(count = 0)) %>%
  mutate(
    prop = count / sum(count),
    expected = log10(1 + 1/digit)
  ) %>%
  ggplot(aes(x = factor(digit))) +
  geom_col(aes(y = prop), fill = "darkblue", width = 0.6) +
  geom_point(aes(y = expected), color = "orange", size = 3) +
  labs(x = "First Digit", y = "Proportion") +
  theme_minimal()

print(benford_plot)


## Alternative with facets for comparison ----------------------------------

library(tidyverse)

# Create comparison data frame
benford_analysis <- tibble(
  date = seq(as.Date("2023-01-01"), by = "day", length.out = 365),
  value = rlnorm(365, meanlog = 5, sdlog = 2)
) %>%
  filter(value > 0) %>%
  mutate(
    digit = floor(value / 10^floor(log10(value))) %>% as.integer()
  ) %>%
  filter(digit %in% 1:9) %>%
  count(digit, name = "observed") %>%
  complete(digit = 1:9, fill = list(observed = 0)) %>%
  mutate(
    observed_pct = observed / sum(observed),
    expected_pct = log10(1 + 1/digit),
    type = "Observed"
  ) %>%
  bind_rows(
    tibble(
      digit = 1:9,
      observed_pct = log10(1 + 1/1:9),
      expected_pct = log10(1 + 1/1:9),
      type = "Expected"
    )
  )

# Plot side-by-side comparison
benford_analysis %>%
  ggplot(aes(x = factor(digit), y = observed_pct, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Observed" = "steelblue", "Expected" = "orange")) +
  labs(
    title = "Benford's Law Analysis",
    x = "First Digit",
    y = "Proportion",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")


## One-liner for the data frame --------------------------------------------
# Ultra-concise data frame creation
benford_df <- ts_data %>%
  filter(value > 0) %>%
  transmute(digit = floor(value/10^floor(log10(value)))) %>%
  filter(digit %in% 1:9) %>%
  count(digit) %>%
  mutate(pct = n/sum(n), expected = log10(1 + 1/digit))


## With a summary table ----------------------------------------------------
# install.packages('kableExtra')
library(kableExtra)  # Optional for pretty tables

# Analysis with formatted output
ts_data %>%
  filter(value > 0) %>%
  mutate(digit = floor(value/10^floor(log10(value)))) %>%
  filter(digit %in% 1:9) %>%
  count(digit, name = "count") %>%
  complete(digit = 1:9, fill = list(count = 0)) %>%
  mutate(
    proportion = count/sum(count),
    expected = log10(1 + 1/digit),
    diff = proportion - expected
  ) %>%
  kbl(digits = 3) %>%
  kable_styling("striped")



# DATA.TABLE ---------------------------------------------------------------
library(data.table)

# Ensure ts_data is a data.table
setDT(ts_data)

# Calculate, filter, summarize, and mutate in a chain
digit_counts <- ts_data[, first_digit := floor(value / 10^floor(log10(value)))][
  first_digit %in% 1:9,            # i: Filter valid digits
  .(Freq = .N),                    # j: Count occurrences
  keyby = .(digit = first_digit)   # by: Group and Sort
][, proportion := Freq / sum(Freq)] # j: Calculate proportion on the result

# Simple bar chart 
# (data.table columns can be accessed exactly like data.frame columns)
barplot(digit_counts$proportion, 
        names.arg = digit_counts$digit,
        col = "lightgreen",
        main = "First Digit Distribution in Time Series",
        xlab = "First Digit",
        ylab = "Frequency")

# Show the counts
digit_counts

# Linear regression intro -------------------------------------------------------
d <- datasets::cars

mean (d$speed)
d2 <- mutate(d, speed=1.67* speed, dist=0.3*dist, ratio=dist/speed)
glimpse(d2)
qplot(data=d2, dist)
qplot (data=d2, dist, xlab="длина тормозного пути (м)",
       ylab="кол-во машин",
       main="данные 1920х годов")

model <- lm(data=d2, dist~speed)
model
qplot(data=d2, speed, dist)
