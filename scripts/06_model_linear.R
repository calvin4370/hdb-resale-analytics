# ==============================================================================
# SCRIPT:  05_model_linear.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-08
# PURPOSE: Train Linear Regression models for HDB Resale Price Prediction.
#          Establishes an interpretable baseline.
# INPUTS:  data/processed/modelling_resale_prices.csv
# OUTPUTS: 1. output/models/lm_baseline.rds
#          2. output/models/lm_stepwise.rds
# ==============================================================================

library(tidyverse)
library(caret)

source("R/prepare_model_data.R")

model_df <- load_model_data()
split <- split_by_year(model_df)
train_control <- make_train_control(split$train)

dir.create("output/models", recursive = TRUE, showWarnings = FALSE)


# Model 1: Baseline OLS on all predictors --------------------------------------
set.seed(123)
model_lm_baseline <- train(
  log_resale_price ~ .,
  data = split$train,
  method = "lm",
  trControl = train_control
)

print(model_lm_baseline)
summary(model_lm_baseline$finalModel)


# Model 2: Backward stepwise AIC feature selection -----------------------------
set.seed(123)
model_lm_stepwise <- train(
  log_resale_price ~ .,
  data = split$train,
  method = "lmStepAIC",
  trControl = train_control,
  direction = "backward",
  trace = FALSE
)

print(model_lm_stepwise)
summary(model_lm_stepwise$finalModel)


saveRDS(model_lm_baseline, "output/models/lm_baseline.rds")
saveRDS(model_lm_stepwise, "output/models/lm_stepwise.rds")
