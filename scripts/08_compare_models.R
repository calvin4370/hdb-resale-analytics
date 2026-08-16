# ==============================================================================
# SCRIPT:  08_compare_models.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-10
# PURPOSE: Rank all trained models by cross-validated error, then score the
#          selected model once on the held-out test years.
# INPUTS:  1. data/processed/modelling_resale_prices.csv
#          2. output/models/*.rds
# OUTPUTS: output/figures/best_model_performance.png
# ==============================================================================

library(tidyverse)
library(caret)
library(ranger)
library(xgboost)

source("R/prepare_model_data.R")

model_df <- load_model_data()
split <- split_by_year(model_df)

model_map <- list(
  "OLS Baseline" = readRDS("output/models/lm_baseline.rds"),
  "Stepwise AIC" = readRDS("output/models/lm_stepwise.rds"),
  "Ridge" = readRDS("output/models/glmnet_ridge.rds"),
  "Lasso" = readRDS("output/models/glmnet_lasso.rds"),
  "Elastic Net" = readRDS("output/models/glmnet_elastic.rds"),
  "Random Forest" = readRDS("output/models/model_random_forest.rds"),
  "XGBoost" = readRDS("output/models/model_xgboost.rds")
)


# Model selection ==============================================================
# Ranked on cross-validated error only. The test years are not read until a
# single model has been chosen, so the test score stays an honest estimate of
# out-of-sample performance rather than a value that was optimised against.
# CV errors are on the log scale, being the scale the models were fit on.

cv_results <- map_dfr(names(model_map), function(model_name) {
  perf <- getTrainPerf(model_map[[model_name]])
  tibble(
    Model = model_name,
    CV_RMSE_log = round(perf$TrainRMSE, 5),
    CV_MAE_log = round(perf$TrainMAE, 5),
    CV_R2 = round(perf$TrainRsquared, 4)
  )
}) %>%
  arrange(CV_RMSE_log)

print(cv_results)

best_model_name <- cv_results$Model[1]
best_model <- model_map[[best_model_name]]
message(glue::glue("Selected on CV RMSE: {best_model_name}"))


# Test set evaluation ==========================================================

# TODO: exp() returns the geometric mean and so is biased low. Replace with a
# smearing estimator once the back-transform is corrected.
test_set <- split$test
test_set$actual_price <- exp(test_set$log_resale_price)
test_set$predicted_price <- exp(predict(best_model, test_set))

score_predictions <- function(actual, predicted) {
  tibble(
    n = length(actual),
    RMSE_SGD = round(RMSE(predicted, actual), 2),
    MAE_SGD = round(MAE(predicted, actual), 2),
    R2 = round(R2(predicted, actual), 4)
  )
}

test_overall <- score_predictions(
  test_set$actual_price,
  test_set$predicted_price
)
print(test_overall)

# Broken out by year: the gap between the two test years measures how quickly
# accuracy decays as the forecast horizon extends past the training window.
test_by_year <- test_set %>%
  group_by(resale_year) %>%
  group_modify(~ score_predictions(.x$actual_price, .x$predicted_price)) %>%
  ungroup()

print(test_by_year)


# Actual vs predicted, faceted by test year ====================================
best_model_performance_plot <- ggplot(
  test_set,
  aes(x = actual_price, y = predicted_price)
) +
  geom_point(alpha = 0.1, color = "darkblue") +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  facet_wrap(~ resale_year) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = glue::glue("{best_model_name}: Actual vs Predicted Resale Prices"),
    subtitle = "Held-out test years, never seen during training or tuning",
    x = "Actual Resale Price (SGD)",
    y = "Predicted Resale Price (SGD)"
  ) +
  theme_minimal()

print(best_model_performance_plot)
ggsave(
  "output/figures/best_model_performance.png",
  plot = best_model_performance_plot,
  width = 10,
  height = 6
)
