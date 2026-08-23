# ==============================================================================
# SCRIPT:  06_model_linear.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-08
# PURPOSE: Train Linear Regression models for HDB Resale Price Prediction.
#          Establishes an interpretable baseline, and produces the collinearity
#          and residual diagnostics the regularised models are motivated by.
# INPUTS:  data/processed/modelling_resale_prices.csv
# OUTPUTS: 1. output/models/lm_baseline.rds
#          2. output/models/lm_stepwise.rds
#          3. output/summaries/*.txt
#          4. output/metrics/vif_baseline.csv
#          5. output/figures/lm_baseline_residuals_*.png
# ==============================================================================

library(tidyverse)
library(caret)

source("R/prepare_model_data.R")
source("R/diagnostics.R")

split <- load_split()
train_control <- make_train_control(split$train)

for (d in c("output/models", "output/summaries", "output/metrics",
            "output/figures")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}


# Model 1: Baseline OLS on all predictors --------------------------------------
set.seed(RANDOM_SEED)
model_lm_baseline <- train(
  log_resale_price ~ .,
  data = split$train,
  method = "lm",
  trControl = train_control
)

print(model_lm_baseline)

save_text_summary(
  print(model_lm_baseline),
  print(summary(model_lm_baseline$finalModel)),
  path = "output/summaries/lm_baseline.txt"
)


# Model 2: Backward stepwise AIC feature selection -----------------------------
set.seed(RANDOM_SEED)
model_lm_stepwise <- train(
  log_resale_price ~ .,
  data = split$train,
  method = "lmStepAIC",
  trControl = train_control,
  direction = "backward",
  trace = FALSE
)

print(model_lm_stepwise)

save_text_summary(
  print(model_lm_stepwise),
  print(summary(model_lm_stepwise$finalModel)),
  path = "output/summaries/lm_stepwise.txt"
)


# Collinearity =================================================================
# The regularised models in 07 exist to handle correlated predictors. This is
# the evidence for that claim. Two dependencies are expected: lat/long against
# distance_to_cbd, and remaining_lease_numeric against resale_year, since
# remaining lease is roughly 99 - (resale_year - lease_commence_date).
vif_baseline <- compute_vif(split$train)

print(as.data.frame(vif_baseline))
write_csv(vif_baseline, "output/metrics/vif_baseline.csv")
save_text_summary(
  print(as.data.frame(vif_baseline)),
  path = "output/summaries/vif_baseline.txt"
)


# Residual diagnostics =========================================================
# Assumptions were checked on raw variables during EDA; this checks them where
# they actually apply, on the residuals of a fitted model. The by-year panel
# additionally tests the homoskedasticity that the smearing correction assumes.
residual_summary <- save_residual_diagnostics(
  model_lm_baseline$finalModel,
  split$train,
  prefix = "lm_baseline"
)

print(as.data.frame(residual_summary))
write_csv(residual_summary, "output/metrics/lm_baseline_residuals_by_year.csv")


saveRDS(model_lm_baseline, "output/models/lm_baseline.rds")
saveRDS(model_lm_stepwise, "output/models/lm_stepwise.rds")
