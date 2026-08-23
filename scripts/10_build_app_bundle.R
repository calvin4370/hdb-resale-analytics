# ==============================================================================
# SCRIPT:  10_build_app_bundle.R
# AUTHOR:  Chan Jun Jie
# DATE:    2026-08-16
# PURPOSE: Build everything the Shiny app serves from the canonical artifacts.
#          The only script permitted to write into app/, so the deployed model
#          and data cannot drift from the trained ones.
# INPUTS:  1. data/processed/modelling_resale_prices.csv
#          2. output/models/*.rds
#          3. output/metrics/cv_comparison.csv
# OUTPUTS: 1. app/data/address_lookup.rds
#          2. app/data/transactions.rds
#          3. app/data/model_metadata.rds
#          4. app/models/model_deployed.rds
# ==============================================================================

library(tidyverse)
library(caret)
library(ranger)
library(xgboost)

source("R/prepare_model_data.R")
source("R/evaluate.R")
source("R/models.R")

dir.create("app/data", recursive = TRUE, showWarnings = FALSE)
dir.create("app/models", recursive = TRUE, showWarnings = FALSE)


# Deploy whichever model 09 selected on cross-validated error, rather than a
# name hardcoded here.
cv_comparison <- read_csv(
  "output/metrics/cv_comparison.csv",
  show_col_types = FALSE
)
deployed_name <- cv_comparison$Model[1]

if (!deployed_name %in% names(MODEL_FILES)) {
  stop(
    glue::glue(
      "Model selected by 09 ('{deployed_name}') has no saved artifact. ",
      "The reference model winning on CV would be a result worth ",
      "investigating before deploying anything."
    ),
    call. = FALSE
  )
}

deployed_model <- readRDS(MODEL_FILES[[deployed_name]])
message(glue::glue("Deploying: {deployed_name}"))


split <- load_split()

# Shipped with the model because the app cannot recompute it: the correction
# is derived from training residuals, and the app never sees the training set.
smear <- smearing_factor(deployed_model, split$train)

# Band multipliers for the app, from the cross-validation folds.
error_quantiles <- cv_error_quantiles(deployed_model, split$train, smear)

message(glue::glue(
  "Prediction band: global {round(error_quantiles$global[['lower']], 3)}x to ",
  "{round(error_quantiles$global[['upper']], 3)}x, with per-flat-type bands ",
  "for {nrow(error_quantiles$by_flat_type)} types"
))

# Dropped after the quantiles are computed. The saved predictions are a large
# share of the model object and the app has no use for them.
deployed_model$pred <- NULL


raw_data <- read_csv(
  "data/processed/modelling_resale_prices.csv",
  show_col_types = FALSE
)

transactions <- raw_data %>%
  mutate(
    town = as.factor(town),
    flat_type = as.factor(flat_type),
    flat_model = as.factor(flat_model)
  ) %>%
  select(
    address, town, flat_type, flat_model, storey_range, storey_mid,
    floor_area_sqm, lease_commence_date, resale_price, resale_year,
    resale_date
  ) %>%
  arrange(address, desc(resale_date))


# One row per address. Rail distance varies over time now that stations are
# only counted once open, so the most recent value is taken rather than an
# arbitrary one — the app predicts current prices, so it needs current
# accessibility.
address_lookup <- raw_data %>%
  arrange(address, desc(resale_date)) %>%
  group_by(address) %>%
  summarise(
    town = first(town),
    distance_to_cbd = first(distance_to_cbd),
    distance_to_nearest_mrt = first(distance_to_nearest_mrt),
    lat = first(lat),
    long = first(long),
    .groups = "drop"
  ) %>%
  mutate(town = factor(town, levels = levels(transactions$town)))


# Recorded so the app can state what it is serving, and so a stale bundle is
# visible rather than silent.
model_metadata <- list(
  model_name = deployed_name,
  smearing_factor = smear,
  error_quantiles = error_quantiles,
  train_years = range(split$train$resale_year),
  training_rows = nrow(split$train),
  predictors = MODEL_PREDICTORS,
  data_through = max(raw_data$resale_date),
  built_at = Sys.time()
)


saveRDS(address_lookup, "app/data/address_lookup.rds", compress = "xz")
saveRDS(transactions, "app/data/transactions.rds", compress = "xz")
saveRDS(model_metadata, "app/data/model_metadata.rds")
saveRDS(deployed_model, "app/models/model_deployed.rds")

bundle_mb <- sum(file.size(list.files(
  c("app/data", "app/models"), full.names = TRUE
))) / 1024^2

message(glue::glue(
  "App bundle built: {nrow(address_lookup)} addresses, ",
  "{nrow(transactions)} transactions, {round(bundle_mb, 1)} MB"
))
