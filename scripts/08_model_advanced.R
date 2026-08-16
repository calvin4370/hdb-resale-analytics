# ==============================================================================
# SCRIPT:  07_model_advanced.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-09
# PURPOSE: Train advanced non-linear regression models (Random Forest & XGBoost)
#          to capture interaction effects the linear models cannot.
# INPUTS:  data/processed/modelling_resale_prices.csv
# OUTPUTS: 1. output/models/model_random_forest.rds
#          2. output/models/model_xgboost.rds
# ==============================================================================

library(tidyverse)
library(caret)
library(ranger)
library(xgboost)
library(doParallel)

source("R/prepare_model_data.R")
source("R/tuning.R")

split <- load_split()
train_control <- make_train_control(
  split$train,
  allowParallel = TRUE,
  verboseIter = TRUE
)

dir.create("output/models", recursive = TRUE, showWarnings = FALSE)
dir.create("output/metrics", recursive = TRUE, showWarnings = FALSE)

# Leave one core for the OS.
num_cores <- detectCores() - 1
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
message(glue::glue("Parallel processing enabled: {num_cores} cores"))


# Model 1: Random Forest -------------------------------------------------------
# mtry: predictors sampled per split. splitrule: variance reduction for
# regression. min.node.size: 5 grows deeper trees, 10 shallower.
rf_grid <- expand.grid(
  mtry = c(10, 15, 20),
  splitrule = "variance",
  min.node.size = c(5, 10)
)

set.seed(RANDOM_SEED)
model_random_forest <- train(
  log_resale_price ~ .,
  data = split$train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = rf_grid,
  num.trees = 500,
  importance = "impurity"
)

print(model_random_forest)
plot(model_random_forest)
plot(varImp(model_random_forest), top = 20)


# Model 2: XGBoost -------------------------------------------------------------
# eta: learning rate. nrounds: boosting iterations. max_depth: tree depth.
# gamma: minimum loss reduction required to split. subsample /
# colsample_bytree: row and column sampling per tree. min_child_weight:
# minimum node weight before a node becomes a leaf.
xgboost_grid <- expand.grid(
  nrounds = c(500, 1000),
  max_depth = c(3, 6),
  eta = c(0.01, 0.1),
  gamma = 0,
  colsample_bytree = 0.7,
  min_child_weight = 1,
  subsample = 0.7
)

set.seed(RANDOM_SEED)
model_xgboost <- train(
  log_resale_price ~ .,
  data = split$train,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgboost_grid,
  nthread = 1
)

print(model_xgboost)
plot(model_xgboost)
plot(varImp(model_xgboost), top = 20)


stopCluster(cl)
registerDoSEQ()

# nrounds selected at 1000 or eta at 0.01 would mean boosting was still
# improving when the grid ran out.
report_tune_grids(
  list(
    "Random Forest" = model_random_forest,
    "XGBoost" = model_xgboost
  ),
  "output/metrics/tuning_advanced.csv"
)

saveRDS(model_random_forest, "output/models/model_random_forest.rds")
saveRDS(model_xgboost, "output/models/model_xgboost.rds")
