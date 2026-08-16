# ==============================================================================
# FILE:    R/evaluate.R
# AUTHOR:  Chan Jun Jie
# PURPOSE: Back-transform log-scale predictions to SGD and score them.
# ==============================================================================

library(tidyverse)


# Duan's smearing estimator. Models are fit on log(resale_price), and exp() of
# a log-scale prediction returns the geometric mean, which sits below the
# arithmetic mean. The correction factor is the mean of the exponentiated
# residuals and is computed on training data only, never on the test years.
# Assumes residuals are homoskedastic on the log scale.
smearing_factor <- function(model, train_data) {
  residuals_log <- train_data$log_resale_price - predict(model, train_data)
  mean(exp(residuals_log))
}


# Predicted resale price in SGD, with the smearing correction applied.
predict_price <- function(model, newdata, smear = 1) {
  exp(predict(model, newdata)) * smear
}


# Scores predictions on the SGD scale.
#
# R2 here is 1 - SSE/SST, not caret's default squared correlation. Squared
# correlation is invariant to scale and offset, so a model predicting a
# constant fraction of every true price would still score 1.0 — precisely the
# error the smearing correction exists to remove.
#
# Bias_SGD is the mean signed error. It is the diagnostic for both the
# retransformation bias and any failure to extrapolate past the training years.
score_predictions <- function(actual, predicted) {
  errors <- predicted - actual
  abs_pct_error <- abs(errors) / actual

  tibble(
    n = length(actual),
    RMSE_SGD = round(sqrt(mean(errors^2)), 2),
    MAE_SGD = round(mean(abs(errors)), 2),
    MAPE_pct = round(100 * mean(abs_pct_error), 3),
    MedAPE_pct = round(100 * median(abs_pct_error), 3),
    R2 = round(1 - sum(errors^2) / sum((actual - mean(actual))^2), 4),
    Bias_SGD = round(mean(errors), 2)
  )
}


# Scores within each level of a grouping column. Expects `actual_price` and
# `predicted_price` columns.
score_by_group <- function(scored_data, group_var) {
  scored_data %>%
    group_by({{ group_var }}) %>%
    group_modify(
      ~ score_predictions(.x$actual_price, .x$predicted_price)
    ) %>%
    ungroup()
}
