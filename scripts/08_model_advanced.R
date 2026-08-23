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

source("R/prepare_model_data.R")
source("R/tuning.R")

# Threads, not worker processes. caret's resample-level parallelism forks a
# full R session per worker, each holding its own copy of the training frame
# and its own forest — 15 of those exhausted 31 GB and killed the run. ranger
# and xgboost both thread internally over one shared copy instead, which uses
# the same cores at a fraction of the memory.
N_THREADS <- max(1, parallel::detectCores() - 2)
message(glue::glue("Using {N_THREADS} threads within a single R session"))

split <- load_split()
train_control <- make_train_control(
  split$train,
  allowParallel = FALSE,
  verboseIter = TRUE
)

dir.create("output/models", recursive = TRUE, showWarnings = FALSE)
dir.create("output/metrics", recursive = TRUE, showWarnings = FALSE)


# Model 1: Random Forest -------------------------------------------------------
# mtry: predictors sampled per split, out of the ~60 columns the factors expand
# to. splitrule: variance reduction for regression. min.node.size: smaller grows
# deeper trees.
#
# Widened after the first run selected mtry = 20 and min.node.size = 5, both at
# a grid edge. mtry is now bracketed; 20 and 5 are retained so the runs stay
# comparable.
#
# min.node.size deliberately stops at 3 rather than 1. Fully grown trees
# measured at 91s and a 3.46 GB forest per fit, and this model lost to XGBoost
# by 12% on the first run, so it is not the one being deployed. If 3 is
# selected, that lower bound is a documented cost decision, not an oversight.
rf_grid <- expand.grid(
  mtry = c(20, 30, 40),
  splitrule = "variance",
  min.node.size = c(3, 5)
)

set.seed(RANDOM_SEED)
model_random_forest <- train(
  log_resale_price ~ .,
  data = split$train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = rf_grid,
  num.trees = 500,
  importance = "impurity",
  num.threads = N_THREADS
)

print(model_random_forest)
plot(model_random_forest)
plot(varImp(model_random_forest), top = 20)


# Model 2: XGBoost -------------------------------------------------------------
# eta: learning rate. nrounds: boosting iterations. max_depth: tree depth.
# gamma: minimum loss reduction required to split. subsample /
# colsample_bytree: row and column sampling per tree. min_child_weight:
# minimum node weight before a node becomes a leaf.
#
# Widened after the first run selected nrounds = 1000, max_depth = 6 and
# eta = 0.1, all three at a grid edge. eta = 0.01 was clearly too low, so the
# range moves up and brackets 0.1 from both sides. nrounds costs almost nothing
# to extend: caret trains once to the maximum and scores the shorter runs as
# sub-models.
xgboost_grid <- expand.grid(
  nrounds = c(1000, 1500, 2000),
  max_depth = c(6, 8, 10),
  eta = c(0.05, 0.1, 0.2),
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
  nthread = N_THREADS
)

print(model_xgboost)
plot(model_xgboost)
plot(varImp(model_xgboost), top = 20)


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
