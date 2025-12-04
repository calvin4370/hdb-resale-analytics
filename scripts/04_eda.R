# ==============================================================================
# SCRIPT:  04_eda.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-03
# PURPOSE: Perform Exploratory Data Analysis (EDA) on enriched dataset to check 
#          regression assumptions and visualize key relationships
# INPUTS:  data/processed/enriched_resale_prices.csv
# ==============================================================================

library(tidyverse)
library(ggplot2)

df <- read_csv("data/processed/cleaned_resale_prices.csv")

# Line Chart of Resale Price against Floor Area (sqm)
ggplot(data = df, aes(x = floor_area_sqm, y = resale_price)) +
  geom_point(alpha = 0.1, color = "navy") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(
    title = "Resale Price vs. Floor Area (sqm)",
    x = "Floor Area (sqm)",
    y = "Resale Price ($)"
  )


# Boxplot of Resale Price against Town
# Sort by average resale price descending
ggplot(data = df, aes(y = reorder(town, resale_price), x = resale_price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  
  theme_minimal() +
  
  labs(
    title = "Resale Price Distribution by Town",
    subtitle = "Central locations tend to have higher resale prices",
    x = "Resale Price ($)",
    y = "Town"
  )
