# ==============================================================================
# SCRIPT:  09_compare_models.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-10
# PURPOSE: Rank all trained models by cross-validated error, then score the
#          selected model once on the held-out test years.
# INPUTS:  1. data/processed/modelling_resale_prices.csv
#          2. output/models/*.rds
# OUTPUTS: 1. output/metrics/*.csv
#          2. output/figures/best_model_performance.png
# ==============================================================================

library(tidyverse)
library(caret)
library(ranger)
library(xgboost)

source("R/prepare_model_data.R")
source("R/evaluate.R")
source("R/baseline.R")

dir.create("output/metrics", recursive = TRUE, showWarnings = FALSE)

model_df <- load_model_data()
split <- split_by_year(model_df)

model_map <- list(
  # Reference model, fit here rather than loaded: it is a grouped median and
  # needs no training run.
  "Median $/sqm" = fit_median_psm(split$train),

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

cv_folds <- make_time_folds(split$train)

cv_results <- map_dfr(names(model_map), function(model_name) {
  model <- model_map[[model_name]]

  # The reference model is cross-validated over the same folds by hand; the
  # caret models carry their resampled performance already.
  perf <- if (inherits(model, "median_psm")) {
    cv_median_psm(split$train, cv_folds)
  } else {
    getTrainPerf(model)
  }

  tibble(
    Model = model_name,
    CV_RMSE_log = round(perf$TrainRMSE, 5),
    CV_MAE_log = round(perf$TrainMAE, 5),
    CV_R2 = round(perf$TrainRsquared, 4)
  )
}) %>%
  arrange(CV_RMSE_log)

print(cv_results)
write_csv(cv_results, "output/metrics/cv_comparison.csv")

best_model_name <- cv_results$Model[1]
best_model <- model_map[[best_model_name]]
message(glue::glue("Selected on CV RMSE: {best_model_name}"))


# Test set evaluation ==========================================================

smear <- smearing_factor(best_model, split$train)
message(glue::glue(
  "Smearing factor: {round(smear, 5)} ",
  "(raises predictions by {round(100 * (smear - 1), 2)}%)"
))

test_set <- split$test
test_set$actual_price <- exp(test_set$log_resale_price)
test_set$uncorrected_price <- exp(predict(best_model, test_set))
test_set$predicted_price <- test_set$uncorrected_price * smear

# Quantifies what the smearing correction changed, so the choice is evidenced
# rather than asserted.
correction_effect <- bind_rows(
  score_predictions(test_set$actual_price, test_set$uncorrected_price) %>%
    mutate(Backtransform = "exp() only", .before = 1),
  score_predictions(test_set$actual_price, test_set$predicted_price) %>%
    mutate(Backtransform = "exp() x smearing", .before = 1)
)

print(correction_effect)
write_csv(correction_effect, "output/metrics/smearing_effect.csv")


test_overall <- score_predictions(
  test_set$actual_price,
  test_set$predicted_price
) %>%
  mutate(Model = best_model_name, .before = 1)

print(test_overall)
write_csv(test_overall, "output/metrics/test_overall.csv")


# Segment breakdowns ===========================================================
# Accuracy is not uniform. The year split shows how quickly error grows once
# the forecast horizon extends past the training window; the others show which
# segments the model serves worst.
#
# R2 is expected to be negative for narrow segments such as price deciles: SST
# is computed within the segment, so a decile spanning a small price range can
# be predicted worse than its own mean even by an accurate model. Read MAPE and
# Bias_SGD for those rows, not R2.

test_by_year <- score_by_group(test_set, resale_year)
test_by_flat_type <- score_by_group(test_set, flat_type)
test_by_town <- score_by_group(test_set, town) %>% arrange(desc(MAPE_pct))
test_by_price_decile <- test_set %>%
  mutate(price_decile = ntile(actual_price, 10)) %>%
  score_by_group(price_decile)

print(test_by_year)
print(test_by_flat_type)
print(head(test_by_town, 10))
print(test_by_price_decile)

write_csv(test_by_year, "output/metrics/test_by_year.csv")
write_csv(test_by_flat_type, "output/metrics/test_by_flat_type.csv")
write_csv(test_by_town, "output/metrics/test_by_town.csv")
write_csv(test_by_price_decile, "output/metrics/test_by_price_decile.csv")


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

ggsave(
  "output/figures/best_model_performance.png",
  plot = best_model_performance_plot,
  width = 10,
  height = 6
)

message("Metrics written to output/metrics/")
