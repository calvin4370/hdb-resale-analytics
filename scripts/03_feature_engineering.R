# ==============================================================================
# SCRIPT:  03_feature_engineering.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-03
# PURPOSE: Engineer `distance_to_cbd` and `distance_to_nearest_mrt`.
#          Rail distance is evaluated as at the transaction date, so a flat is
#          only measured against stations that had opened when it was sold.
# INPUTS:  1. data/processed/cleaned_resale_prices.csv
#          2. data/external/hdb_coordinates.csv
#          3. data/external/mrt_lrt_stations.csv
# OUTPUTS: data/processed/enriched_resale_prices.csv
# ==============================================================================

library(tidyverse)
library(geosphere)

source("R/config.R")


# Load data --------------------------------------------------------------------
resale_prices <- read_csv(
  "data/processed/cleaned_resale_prices.csv",
  show_col_types = FALSE
)
hdb_coords <- read_csv(
  "data/external/hdb_coordinates.csv",
  show_col_types = FALSE
)

stations <- read_csv(
  "data/external/mrt_lrt_stations.csv",
  show_col_types = FALSE
) %>%
  select(
    station_name = STATION_NAME_ENGLISH,
    opened = OPENING_DATE,
    lat = LATITUDE,
    long = LONGITUDE
  ) %>%
  arrange(opened)

merged_data <- resale_prices %>%
  left_join(hdb_coords, by = "address")

stopifnot(
  "transactions with no geocoded coordinates" =
    !any(is.na(merged_data$lat) | is.na(merged_data$long)),
  "stations with no opening date" = !any(is.na(stations$opened))
)


# Distances are computed once per unique address, then mapped back to
# transactions, rather than recomputed for every sale at the same block.
addresses <- merged_data %>%
  distinct(address, lat, long)

address_idx <- match(merged_data$address, addresses$address)


# Feature 1: distance to CBD ---------------------------------------------------
addresses$distance_to_cbd <- distHaversine(
  cbind(addresses$long, addresses$lat),
  CBD_COORDS
) / 1000


# Feature 2: distance to nearest rail station open at the time of sale ---------
# Stations are sorted by opening date, so a running row-wise minimum over the
# address-by-station distance matrix yields, in column k, each address's nearest
# station as at the opening of the k-th station. The number of stations already
# open on a transaction date is therefore the column to read.
dist_matrix <- distm(
  cbind(addresses$long, addresses$lat),
  cbind(stations$long, stations$lat),
  fun = distHaversine
) / 1000

nearest_as_at <- t(apply(dist_matrix, 1, cummin))

stations_open <- findInterval(merged_data$resale_date, stations$opened)

stopifnot(
  "transactions predating every station opening" = all(stations_open > 0)
)

enriched_data <- merged_data %>%
  mutate(
    distance_to_cbd = addresses$distance_to_cbd[address_idx],
    distance_to_nearest_mrt = nearest_as_at[cbind(address_idx, stations_open)]
  )


# Verify the engineered features are within plausible bounds for Singapore.
stopifnot(
  "implausible CBD distance" =
    all(between(enriched_data$distance_to_cbd, 0, 30)),
  "implausible rail distance" =
    all(between(enriched_data$distance_to_nearest_mrt, 0, 5))
)

print(summary(enriched_data$distance_to_cbd))
print(summary(enriched_data$distance_to_nearest_mrt))


output_filepath <- "data/processed/enriched_resale_prices.csv"
write_csv(enriched_data, output_filepath)
message(glue::glue("Saved enriched data to {output_filepath}"))
