# ==============================================================================
# SCRIPT:  02b_validate_geocoding.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-02
# PURPOSE: Verify the coordinates produced by 02_geocoding.R before any feature
#          depends on them. Fails the run rather than reporting, so a bad
#          geocode cannot reach the model as a silently missing distance.
# INPUTS:  1. data/external/hdb_coordinates.csv
#          2. data/processed/cleaned_resale_prices.csv
# OUTPUTS: output/figures/geocoded_coordinates.png
# ==============================================================================

library(tidyverse)

# Bounding box for Singapore. A geocode outside it means OneMap matched
# something other than the intended address.
SG_LAT_RANGE <- c(1.15, 1.48)
SG_LONG_RANGE <- c(103.6, 104.1)


coords <- read_csv(
  "data/external/hdb_coordinates.csv",
  show_col_types = FALSE
)
cleaned_data <- read_csv(
  "data/processed/cleaned_resale_prices.csv",
  show_col_types = FALSE
)

# Compare on the `address` column built by 01_data_cleaning.R rather than
# rebuilding it from block and street name, so a change to how addresses are
# formed cannot make this check pass against the wrong key.
transaction_addresses <- distinct(cleaned_data, address)

missing_coords <- filter(coords, is.na(lat) | is.na(long))
ungeocoded <- anti_join(transaction_addresses, coords, by = "address")
outside_singapore <- coords %>%
  filter(
    !between(lat, SG_LAT_RANGE[1], SG_LAT_RANGE[2]) |
      !between(long, SG_LONG_RANGE[1], SG_LONG_RANGE[2])
  )

if (nrow(missing_coords) > 0) print(head(missing_coords, 20))
if (nrow(ungeocoded) > 0) print(head(ungeocoded, 20))
if (nrow(outside_singapore) > 0) print(head(outside_singapore, 20))

stopifnot(
  "geocoding returned rows with missing coordinates" =
    nrow(missing_coords) == 0,
  "transaction addresses have no geocoded coordinate" =
    nrow(ungeocoded) == 0,
  "duplicate addresses in the geocoded output" =
    !any(duplicated(coords$address)),
  "geocoded coordinates fall outside Singapore" =
    nrow(outside_singapore) == 0
)

message(glue::glue(
  "Geocoding validated: {nrow(coords)} coordinates covering all ",
  "{nrow(transaction_addresses)} transaction addresses"
))


# Plotted as a final visual check: the points should trace the shape of
# Singapore's residential areas.
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

coordinate_plot <- ggplot(coords, aes(x = long, y = lat)) +
  geom_point(alpha = 0.3, size = 0.5, color = "slateblue") +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Geocoded HDB Block Coordinates",
    subtitle = glue::glue("{nrow(coords)} unique addresses via the OneMap API"),
    x = "Longitude",
    y = "Latitude"
  )

ggsave(
  "output/figures/geocoded_coordinates.png",
  plot = coordinate_plot, width = 8, height = 6
)
