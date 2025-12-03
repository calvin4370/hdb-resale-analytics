# ==============================================================================
# SCRIPT: 05_feature_engineering.R
# PURPOSE: Engineers the features: `distance_to_cbd` and `distance_to_nearest_mrt`
# DATA LINEAGE:
# 1. 'cleaned_resale_prices.csv' -> Derived from Data.gov.sg raw files
# 2. 'hdb_coordinates.csv' -> created from calling OneMap API to obtain coords of all hdb addresses
# ==============================================================================

library(tidyverse)

source("scripts/00_utils.R") # Import haversine_km function


# -------------------- Load data -------------------- #
resale_prices <- read_csv("data/cleaned_resale_prices.csv")
hdb_address_coords <- read_csv("data/hdb_coordinates.csv")

mrt_coords <- read_csv("data/mrt_lrt_stations.csv") %>%
  rename(station_name = STATION_NAME_ENGLISH, lat = LATITUDE, long = LONGITUDE) %>%
  select(station_name, lat, long)


# ---------- Outer join (left) the resale_prices and hdb_address_coords dfs ---------- #
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
# Use vectorization to computer minimum distance from any mrt 
# (distance_from_nearest_mrt) for every address at the same time
enriched_data$distance_to_nearest_mrt <- 99999 # initialise as a large number

# Loop through each MRT station to find the minimum distance from any mrt for every hdb address at once
for (i in 1 : nrow(mrt_coords)) {
  curr_mrt_lat <- mrt_coords$lat[i]
  curr_mrt_long <- mrt_coords$long[i]
  
  dist_to_curr_mrt <- haversine_km(enriched_data$lat, enriched_data$long, curr_mrt_lat, curr_mrt_long)
  
  # Relax the entire distance_to_nearest_mrt column with min of its current value and dist_to_curr_mrt
  enriched_data$distance_to_nearest_mrt <- pmin(enriched_data$distance_to_nearest_mrt, 
                                                dist_to_curr_mrt, na.rm = TRUE)
}

# Check summary of distance_to_nearest_mrt to visually ensure nothing went wrong
summary(enriched_data$distance_to_nearest_mrt)


# ---------- Save the enriched data (with new features) to a new csv ---------- #
output_filepath <- "data/enriched_resale_prices.csv"
write_csv(enriched_data, output_filepath)
print(paste("Saved enriched_data to", output_filepath))