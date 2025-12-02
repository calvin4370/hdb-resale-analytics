library(tidyverse)
library(ggplot2)

df <- read_csv("data/cleaned_resale_prices.csv")

# Line Chart of Resale Price against Floor Area (sqm)
ggplot(data = df, aes(x = floor_area_sqm, y = resale_price)) +
  geom_point(alpha = 0.1, color = "navy") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(
    title = "Resale Price vs. Floor Area",
    x = "Floor Area (sqm)",
    y = "Resale Price ($)"
  )