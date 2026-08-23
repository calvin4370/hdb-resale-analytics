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
#
# Generic because the correction is only meaningful over rows where the model
# is calibrated. A model carrying a time term fits every training year, so all
# of them qualify; a model pinned to one year does not.
#
# Assumes residuals are homoskedastic on the log scale.
smearing_factor <- function(model, train_data) {
  UseMethod("smearing_factor")
}

smearing_factor.default <- function(model, train_data) {
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


# Empirical prediction band, taken from the cross-validation folds rather than
# from training residuals or the test years. Training residuals understate the
# spread a new flat faces, and the test years may only be scored once.
#
# Returns multipliers applied to the point estimate. The point estimate already
# carries the smearing correction, so the ratios are divided by it rather than
# counting it twice.
#
# Every fold validates one year beyond its own training window, so this is a
# one-year-ahead band. The app predicts further ahead than that, which makes
# these bounds optimistic — see the note in the app disclaimer.
#
# Requires trainControl(savePredictions = "final"). UNVERIFIED: no model on disk
# was fitted with it, so this cannot run until the retrain.
cv_error_quantiles <- function(model, train_data, smear,
                               probs = c(0.1, 0.9), min_rows = 500) {
  if (is.null(model$pred)) {
    stop(
      "model carries no saved CV predictions; refit with savePredictions",
      call. = FALSE
    )
  }

  ratios <- tibble(
    flat_type = train_data$flat_type[model$pred$rowIndex],
    ratio = exp(model$pred$obs - model$pred$pred) / smear
  )

  # Thin segments get the global band rather than a quantile estimated from a
  # handful of rows.
  by_flat_type <- ratios %>%
    group_by(flat_type) %>%
    filter(n() >= min_rows) %>%
    summarise(
      lower = quantile(ratio, probs[1], names = FALSE),
      upper = quantile(ratio, probs[2], names = FALSE),
      n = n(),
      .groups = "drop"
    )

  list(
    probs = probs,
    global = c(
      lower = quantile(ratios$ratio, probs[1], names = FALSE),
      upper = quantile(ratios$ratio, probs[2], names = FALSE)
    ),
    by_flat_type = by_flat_type
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
