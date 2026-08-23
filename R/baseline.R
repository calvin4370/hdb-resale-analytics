# ==============================================================================
# FILE:    R/baseline.R
# AUTHOR:  Chan Jun Jie
# PURPOSE: Median price-per-sqm reference model. Approximates the rule of thumb
#          an agent applies by hand, and is the bar the trained models have to
#          clear to justify themselves.
# ==============================================================================

library(tidyverse)
library(caret)


# Median price per sqm by town and flat type, taken from the most recent year
# in the training data. Using the latest year rather than all years mirrors the
# constraint the trained models face: nothing may be learned from the test
# period, so the newest available price level is the best a reference model can
# do. Falls back to flat type alone, then to the global median, for
# combinations absent from training.
#
# Returns an S3 object so predict() dispatches the same way as for the caret
# models, on the log scale they are all fit on.
fit_median_psm <- function(train_data) {
  latest_year <- max(train_data$resale_year)

  recent <- train_data %>%
    filter(resale_year == latest_year) %>%
    mutate(price_per_sqm = exp(log_resale_price) / floor_area_sqm)

  structure(
    list(
      by_town_type = recent %>%
        group_by(town, flat_type) %>%
        summarise(psm_town_type = median(price_per_sqm), .groups = "drop"),
      by_type = recent %>%
        group_by(flat_type) %>%
        summarise(psm_type = median(price_per_sqm), .groups = "drop"),
      overall = median(recent$price_per_sqm),
      fitted_year = latest_year
    ),
    class = "median_psm"
  )
}


predict.median_psm <- function(object, newdata, ...) {
  newdata %>%
    left_join(object$by_town_type, by = c("town", "flat_type")) %>%
    left_join(object$by_type, by = "flat_type") %>%
    mutate(
      psm = coalesce(psm_town_type, psm_type, object$overall),
      log_price = log(psm * floor_area_sqm)
    ) %>%
    pull(log_price)
}


# This model is calibrated to a single year, so its residuals in earlier
# training years measure market drift rather than retransformation bias.
# Including them drags the correction factor below 1 and depresses predictions
# that are already low. Restrict it to the year the model was fitted on.
smearing_factor.median_psm <- function(model, train_data) {
  smearing_factor.default(
    model,
    filter(train_data, resale_year == model$fitted_year)
  )
}


# Cross-validates the reference model over the same expanding-window folds the
# caret models use, so its row in the comparison table is computed identically.
# Metrics match caret::getTrainPerf(): log scale, and R2 as squared correlation.
cv_median_psm <- function(train_data, folds) {
  map_dfr(seq_along(folds$index), function(i) {
    fold_fit <- fit_median_psm(train_data[folds$index[[i]], ])
    validation <- train_data[folds$indexOut[[i]], ]
    predicted <- predict(fold_fit, validation)

    tibble(
      TrainRMSE = RMSE(predicted, validation$log_resale_price),
      TrainMAE = MAE(predicted, validation$log_resale_price),
      TrainRsquared = R2(predicted, validation$log_resale_price)
    )
  }) %>%
    summarise(across(everything(), mean))
}
