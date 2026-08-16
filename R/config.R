# ==============================================================================
# FILE:    R/config.R
# AUTHOR:  Chan Jun Jie
# PURPOSE: Analysis decisions that more than one script depends on. Operational
#          settings stay with the code that owns them — geocoding retries in
#          R/geocoding.R, the train/test years in R/prepare_model_data.R.
#
#          The Shiny app deploys standalone and cannot source this file, so its
#          constants live in app/R/global.R. Nothing here is duplicated there.
# ==============================================================================

# Predictors every model is trained on. Defined once because the modelling
# select(), the missing-value guard in 04 and the app bundle's metadata all
# need the same list, and they have drifted apart before.
MODEL_PREDICTORS <- c(
  "town",
  "flat_type",
  "floor_area_sqm",
  "storey_mid",
  "flat_model",
  "remaining_lease_numeric",
  "lease_commence_date",
  "distance_to_cbd",
  "distance_to_nearest_mrt",
  "resale_year",
  "lat",
  "long"
)

RESPONSE <- "log_resale_price"

# Downtown Core, Singapore (1 17 16.6308 N, 103 51 6.4224 E).
# Stored as (long, lat) because that is the point order distHaversine takes.
CBD_COORDS <- c(103.851784, 1.287953)

# Flats at or above this size are HDB terrace houses and outsized maisonettes.
# They are a distinct product from the flats the model is meant to price and are
# too few to learn from.
MAX_FLOOR_AREA_SQM <- 200

# Fixed so a rerun reproduces the same fits.
RANDOM_SEED <- 42
