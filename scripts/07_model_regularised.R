# ==============================================================================
# SCRIPT:  06_model_regularised.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-08
# PURPOSE: Train Regularised Regression Models
#          (Better handles multicollinearity and robust feature selection)
# INPUTS:  data/processed/modelling_resale_prices.csv
# OUTPUTS: 1. output/models/glmnet_ridge.rds
#          2. output/models/glmnet_lasso.rds
#          3. output/models/glmnet_elastic.rds
# ==============================================================================

library(tidyverse)
library(caret)
library(glmnet)

source("R/prepare_model_data.R")
source("R/tuning.R")

model_df <- load_model_data()
split <- split_by_year(model_df)
train_control <- make_train_control(split$train)

dir.create("output/models", recursive = TRUE, showWarnings = FALSE)
dir.create("output/metrics", recursive = TRUE, showWarnings = FALSE)


# Model 1: Ridge (L2) ----------------------------------------------------------
# alpha = 0 selects Ridge; lambda controls the shrinkage penalty.
ridge_grid <- expand.grid(
  alpha = 0,
  lambda = seq(0.0001, 1, length = 100)
)

set.seed(123)
model_ridge <- train(
  log_resale_price ~ .,
  data = split$train,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = ridge_grid,
  preProcess = c("center", "scale")
)

print(model_ridge)
plot(model_ridge)

ridge_coefs <- coef(model_ridge$finalModel, model_ridge$bestTune$lambda)
head(sort(abs(ridge_coefs[, 1]), decreasing = TRUE), 10)


# Model 2: Lasso (L1) ----------------------------------------------------------
# alpha = 1 selects Lasso, which drives uninformative coefficients to exactly 0.
# Lasso tends to select smaller lambda than Ridge, so the grid is narrower.
lasso_grid <- expand.grid(
  alpha = 1,
  lambda = seq(0.0001, 0.1, length = 100)
)

set.seed(123)
model_lasso <- train(
  log_resale_price ~ .,
  data = split$train,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = lasso_grid,
  preProcess = c("center", "scale")
)

print(model_lasso)
plot(model_lasso)

# Non-zero coefficients only, to show which predictors survived selection.
lasso_matrix <- as.matrix(
  coef(model_lasso$finalModel, model_lasso$bestTune$lambda)
)
print(lasso_matrix[lasso_matrix != 0, ])


# Model 3: Elastic Net ---------------------------------------------------------
# Tunes alpha (Ridge/Lasso mix) alongside lambda.
elastic_net_grid <- expand.grid(
  alpha = seq(0, 1, length = 10),
  lambda = seq(0.0001, 0.5, length = 20)
)

set.seed(123)
model_elastic_net <- train(
  log_resale_price ~ .,
  data = split$train,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = elastic_net_grid,
  preProcess = c("center", "scale")
)

print(model_elastic_net)
plot(model_elastic_net)

elastic_matrix <- as.matrix(
  coef(model_elastic_net$finalModel, model_elastic_net$bestTune$lambda)
)
print(elastic_matrix[elastic_matrix != 0, ])


# Confirm the selected lambda and alpha were bracketed by their grids rather
# than found at an edge.
report_tune_grids(
  list(
    "Ridge" = model_ridge,
    "Lasso" = model_lasso,
    "Elastic Net" = model_elastic_net
  ),
  "output/metrics/tuning_regularised.csv"
)


saveRDS(model_ridge, "output/models/glmnet_ridge.rds")
saveRDS(model_lasso, "output/models/glmnet_lasso.rds")
saveRDS(model_elastic_net, "output/models/glmnet_elastic.rds")
