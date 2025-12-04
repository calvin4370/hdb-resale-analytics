# ==============================================================================
# SCRIPT: 03_feature_engineering.R
# PURPOSE: Engineers the features: `distance_to_cbd` and `distance_to_nearest_mrt`
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-03
# INPUTS:  1. data/processed/cleaned_resale_prices.csv
#          2. data/external/hdb_coordinates.csv
#          3. data/external/mrt_lrt_stations.csv
# OUTPUTS: data/processed/enriched_resale_prices.csv
# ==============================================================================

library(tidyverse)
library(geosphere) # for distHaversine function


# -------------------- Load data -------------------- #
resale_prices <- read_csv("data/processed/cleaned_resale_prices.csv")
hdb_coords <- read_csv("data/external/hdb_coordinates.csv")

mrt_coords <- read_csv("data/external/mrt_lrt_stations.csv") %>%
  rename(station_name = STATION_NAME_ENGLISH, lat = LATITUDE, long = LONGITUDE) %>%
  select(station_name, lat, long)


# ---------- Outer join (left) the resale_prices and hdb_address_coords dfs ---------- #
# Left join the two dataframes to add coord (lat and long) data
merged_data <- resale_prices %>%
  left_join(hdb_coords, by = "address")

# Check if any rows are lost
print(paste("Missing Coordinates:", sum(is.na(merged_data$lat))))


# -------------------- Feature Engineering -------------------- #
# 1. Add `distance_from_cbd` to merged_data
# CBD coords: Downtown Core, Singapore (1° 17' 16.6308" N, 103° 51' 6.4224" E)
cbd_lat <- 1.287953
cbd_long <- 103.851784
cbd_coords <- c(cbd_long, cbd_lat) # dist_haversine takes points of (long, lat) (x, y)

enriched_data <- merged_data %>%
  mutate(
    distance_to_cbd = distHaversine(cbind(long, lat), cbd_coords) / 1000 # convert meters to km
  )

# Check summary of distance_to_cbd to visually ensure nothing went wrong
summary(enriched_data$distance_to_cbd)


# 2. Add `distance_from_nearest_mrt` to complete enriched data
# Use vectorization to computer minimum distance from any mrt 
# (distance_from_nearest_mrt) for every address at the same time
enriched_data$distance_to_nearest_mrt <- 99999 # initialise the whole column with a large number

# Create a matrix of hdb addresses's coords for the distHaversine function
hdb_address_coord_matrix <- cbind(merged_data$long, merged_data$lat)

# Loop through each MRT station to find the minimum distance from any mrt for every hdb address at once
for (i in 1 : nrow(mrt_coords)) {
  curr_mrt_lat <- mrt_coords$lat[i]
  curr_mrt_long <- mrt_coords$long[i]
  curr_mrt_coords <- c(curr_mrt_long, curr_mrt_lat)
  
  # Get vector of haversine distance between each address and the curr mrt
  dists_to_curr_mrt <- distHaversine(hdb_address_coord_matrix, curr_mrt_coords) / 1000
  
  # Relax the entire distance_to_nearest_mrt column with min of its current value and dist_to_curr_mrt
  enriched_data$distance_to_nearest_mrt <- pmin(enriched_data$distance_to_nearest_mrt, 
                                                dists_to_curr_mrt, 
                                                na.rm = TRUE)
}

# Check summary of distance_to_nearest_mrt to visually ensure nothing went wrong
summary(enriched_data$distance_to_nearest_mrt)


# ---------- Save the enriched data (with new features) to a new csv ---------- #
output_filepath <- "data/processed/enriched_resale_prices.csv"
write_csv(enriched_data, output_filepath)
print(paste("Saved enriched_data to", output_filepath))