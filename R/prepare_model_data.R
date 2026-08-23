# ==============================================================================
# FILE:    R/prepare_model_data.R
# AUTHOR:  Chan Jun Jie
# PURPOSE: Shared modelling data preparation and temporal train/test split.
#          Sourced by scripts 05-08 so every model is fit and scored on an
#          identical split.
# ==============================================================================

library(tidyverse)
library(caret)

source("R/config.R")

MODEL_DATA_PATH <- "data/processed/modelling_resale_prices.csv"

# Resale prices trend strongly across 2017-2025, so the holdout is defined by
# transaction year rather than at random. A random split would put the same
# months in both train and test and overstate out-of-sample accuracy.
TRAIN_YEARS <- 2017:2024
TEST_YEARS <- 2025:2026

# Each cross-validation fold validates on one year and trains on every year
# before it, mirroring the forward-looking task the model is actually used for.
CV_VALIDATION_YEARS <- 2020:2024


# Load the modelling dataset in the structure caret expects: log-transformed
# target, factors for categorical predictors, predictors only.
load_model_data <- function(path = MODEL_DATA_PATH) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(
      log_resale_price = log(resale_price),
      town = as.factor(town),
      flat_type = as.factor(flat_type),
      flat_model = as.factor(flat_model)
    ) %>%
    # MODEL_PREDICTORS includes lease_commence_date, which is near-collinear
    # with remaining_lease by construction; it is kept because the trees cannot
    # derive build vintage from resale_year themselves.
    select(all_of(c(RESPONSE, MODEL_PREDICTORS)))
}


# load_model_data() + split_by_year() in one call. Scripts 06-10 all need
# exactly this and previously repeated it.
load_split <- function(path = MODEL_DATA_PATH) {
  split_by_year(load_model_data(path))
}


# Split into training and test sets on transaction year.
split_by_year <- function(model_df) {
  split <- list(
    train = filter(model_df, resale_year %in% TRAIN_YEARS),
    test = filter(model_df, resale_year %in% TEST_YEARS)
  )

  message(glue::glue(
    "Train {min(TRAIN_YEARS)}-{max(TRAIN_YEARS)}: {nrow(split$train)} rows\n",
    "Test  {min(TEST_YEARS)}-{max(TEST_YEARS)}: {nrow(split$test)} rows"
  ))

  split
}


# Build expanding-window fold indices for caret. Returns row positions within
# train_df, so the folds must be used with the same data frame they were built
# from.
make_time_folds <- function(train_df, validation_years = CV_VALIDATION_YEARS) {
  fold_names <- paste0("Val", validation_years)

  index <- lapply(
    validation_years,
    function(y) which(train_df$resale_year < y)
  )
  index_out <- lapply(
    validation_years,
    function(y) which(train_df$resale_year == y)
  )

  names(index) <- fold_names
  names(index_out) <- fold_names

  list(index = index, indexOut = index_out)
}


# trainControl shared by every model script. Extra arguments (allowParallel,
# verboseIter) are passed through.
make_train_control <- function(train_df, ...) {
  folds <- make_time_folds(train_df)

  message(glue::glue(
    "CV fold {seq_along(folds$index)}: ",
    "train {lengths(folds$index)} rows -> validate {names(folds$index)} ",
    "({lengths(folds$indexOut)} rows)"
  ) %>% paste(collapse = "\n"))

  # savePredictions keeps the fold predictions caret already computes. They are
  # the only genuinely out-of-sample residuals available without touching the
  # test years, and cv_error_quantiles() builds the app's prediction band from
  # them.
  trainControl(
    method = "cv",
    index = folds$index,
    indexOut = folds$indexOut,
    savePredictions = "final",
    ...
  )
}
