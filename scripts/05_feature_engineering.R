# ==============================================================================
# SCRIPT: 05_feature_engineering.R
# PURPOSE: Engineers the features: `distance_to_cbd` and `distance_to_nearest_mrt`
# ==============================================================================

library(tidyverse)

source("scripts/00_utils.R") # Import haversine_km function

resale_prices <- read_csv("data/cleaned_resale_prices.csv")
hdb_address_coords <- read_csv("data/hdb_coordinates.csv")


# -------------------- Outer join (left) the two dfs -------------------- #
# Add a common key "address" to prices df to join prices and hdb_address_coords
prices_with_address <- resale_prices %>%
  mutate(address = paste(block, street_name))

# Left join the two dataframes to add coord (lat and long) data
merged_data <- prices_with_address %>%
  left_join(hdb_address_coords, by = "address")

# Check if any rows are lost
print(paste("Missing Coordinates:", sum(is.na(merged_data$lat))))


# -------------------- Feature Engineering -------------------- #
# 1. Add `distance_from_cbd` to merged_data
# CBD coords: Downtown Core, Singapore (1° 17' 16.6308" N, 103° 51' 6.4224" E)
cbd_lat <- 1.287953
cbd_long <- 103.851784

enriched_data <- merged_data %>%
  mutate(
    distance_to_cbd = haversine_km(lat, long, cbd_lat, cbd_long)
  )

# Check summary of distance_to_cbd to visually ensure nothing went wrong
summary(enriched_data$distance_to_cbd)


# 2. Add `distance_from_nearest_mrt` to complete enriched data
# TODO


# ---------- Save the enriched data (with new features) to a new csv ---------- #
output_filepath <- "data/enriched_resale_prices.csv"
write_csv(enriched_data, output_filepath)
print(paste("Saved enriched_data to", output_filepath))