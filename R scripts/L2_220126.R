# install.packages('tidyverse')
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(scales)
library(ggridges)

directory <- "./data/CV VC/"

read_csv_with_filename <- function(file) {
  df <- read_csv(file) %>%
    mutate(filename = basename(file))
  return(df)
}

file_list <- list.files(path = directory, 
                        pattern = "*.csv", 
                        full.names = TRUE)

combined_df <- file_list %>%
  map_dfr(~read_csv_with_filename(.x)) %>% 
  rename(btc_price = `BTC / USD`,
         sv = `Social Volume`,
         sd = `Social Dominance`) %>% 
  mutate(topic = str_replace(filename, ".csv", ""),
         topic = str_replace(topic, "topic_", ""),
         date = as.Date(Date),
         value = sd*0.3) 

pal = c("#E84C53", "#FF9E45", "#377EF7", "#F6C64D", "#59BA8D")

levels_new <- c("DePIN", "RWA", "AI",  "Memecoins", "ETF")
combined_df$topic <- fct_relevel(combined_df$topic, levels_new)

ggplot(combined_df, aes(x = date, y = topic, height = value, fill = topic)) +
  geom_ridgeline() +
  theme_minimal() +
  theme(
    legend.position = 'none',
    text = element_text(size=16, color = "#DFE1EC"),
    axis.text.x = element_text(color = "#7A809A"),
    axis.text.y = element_text(color = "#7A809A"), 
    panel.background = element_rect(fill = "#181B2B",
                                    colour = "#181B2B",
                                    linewidth = 0.5, 
                                    linetype = "solid"),
    panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "#505573"),
    panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                    colour = "#343A52"),
    plot.background = element_rect(fill = "#181B2B")
  ) +
  scale_x_date(limits = date, 
               date_labels = "%d.%m.%y", 
               date_breaks = "1 month", 
               expand = c(0,0)) +
  scale_fill_manual(values = pal) +
  xlab("") +
  ylab("") +
  ggtitle(label = "Some popular narratives in crypto",
          subtitle = "Based on Santiment's Social Dominance Data")

# White background
ggplot(combined_df, aes(x = date, y = topic, height = value, fill = topic)) +
  geom_ridgeline() +
  theme_minimal() +
  theme(
    legend.position = 'none',
    text = element_text(size=16)
  ) +
  scale_x_date(limits = date, 
               date_labels = "%d.%m.%y", 
               date_breaks = "1 month", 
               expand = c(0,0)) +
  scale_fill_manual(values = pal) +
  xlab("") +
  ylab("") +
  ggtitle(label = "Some popular narratives in crypto",
          subtitle = "Based on Santiment's Social Dominance Data")
