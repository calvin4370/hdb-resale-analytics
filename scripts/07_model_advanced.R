# ==============================================================================
# SCRIPT:  07_model_advanced.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-09
# PURPOSE: Train advanced non-linear regression models (Random Forest & XGBoost).
#          Allows them to capture interaction effects automatically compared to
#          linear regression models
# INPUTS:  data/processed/modelling_resale_prices.csv
# OUTPUTS: 1. output/models/model_random_forest.rds
#          2. output/models/model_xgboost.rds
# ==============================================================================

library(tidyverse)
library(caret) 
library(ranger) # for Random Forests
library(xgboost) # for XGBoost
library(doParallel) # for parallel processing

# Load data and prepare it for modelling (same as in previous scripts) ---------
df <- read_csv("data/processed/modelling_resale_prices.csv")

model_df <- df %>%
  mutate( # prepare df for caret by ensuring all categorical variables are factors
    log_resale_price = log(resale_price),
    town = as.factor(town),
    flat_type = as.factor(flat_type),
    flat_model = as.factor(flat_model)
  ) %>%
  select( # Select only logical variables
    log_resale_price, 
    town,
    flat_type,
    floor_area_sqm,
    storey_range_floored,
    flat_model,
    remaining_lease_numeric,
    distance_to_cbd,        
    distance_to_nearest_mrt,
    resale_year
  ) 
glimpse(model_df)

# Check the reference levels
print(levels(model_df$town))
print(levels(model_df$flat_type)) # verify correct order of levels for ordinal data types
print(levels(model_df$flat_model))


# Perform Stratified Sampling using caret::createDataPartition to create training
# and testing datasets ---------------------------------------------------------
# This ensures the Train and Test sets have the same distribution of resale prices

set.seed(123) # for reproducability

# Create a vector of row numbers that belong in the Training set
training_indices <- createDataPartition(
  y = model_df$log_resale_price,  # target for Stratified Sampling
  p = 0.8,              # use 80% of rows for training set
  list = FALSE          # return a vector of row numbers, not a list
)

training_set <- model_df[training_indices, ]
test_set <- model_df[-training_indices, ]

# Check dimensions for training set and testing set to ensure correctness
print(dim(training_set))
print(dim(test_set))


# Train Advanced Non-linear Regression Models ==================================

# Set up training controls for K-Fold Cross Validation -------------------------
train_control <- trainControl(
  method = "cv", # use K-Fold Cross Validation
  number = 10, # with K = 10
  allowParallel = TRUE, # for parallel processing
  verboseIter = TRUE
)
print(train_control)

# Set up Parallel processing to use all CPU cores (leave 1 for the OS)
num_cores <- detectCores() - 1
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
print(paste("Parallel processing ENABLED: using", num_cores, "cores"))


# Model 1: Random Forest -------------------------------------------------------
# Hyperparameters: - mtry: Number of variables randomly sampled as candidates at each split
#                  - ntree: Number of trees to build
#                  - min.node.size: Stopd splitting if a node has fewer than min.node.size rows.

# Define the tuneGrid for Random Forest
rf_grid <- expand.grid(
  mtry = c(10, 15, 20), # Test constraining to 10, 15, or 20 vars
  splitrule = "variance", # use variance (in log_resale_price) to measure split quality
  min.node.size = c(5, 10) # Test deep trees (5) vs shallower trees (10)
)

# Train the Random Forest model
set.seed(123)
model_random_forest <- train(
  log_resale_price ~ ., 
  data = training_set, 
  method = "ranger", 
  trControl = train_control,
  tuneGrid = rf_grid,
  num.trees = 500,
  importance = "impurity" # tracks the total variance reduction contributed by each variable
)

print(model_random_forest)
plot(model_random_forest)

# View top 20 drivers of log_resale_price
rf_imp <- varImp(model_random_forest)
print(rf_imp)
plot(rf_imp, top = 20)


# Model 2: XGBoost -------------------------------------------------------------
# Hyperparameters: - eta: learning rate (larger is faster but risks overshooting the minimum error and becomes unstable)
#                  - nrounds: Number of rounds of boosting (too few leads to underfitting, too much leads to overfitting)
#                  - max_depth: Max tree depth (XGBoost prefers more shallower trees which capture simple interactions) rather than some deep trees
#                  - gamma: Minimum loss reduction (A "pruning" threshold. A node will only split if this reduces the loss (error) by at least gamma)
#                  - subsample: fraction of training data (rows) randomly sampled to build each tree
#                  - colsample_bytree: fraction of features (columns) randomly sampled to build each tree
#                  - min_child_weight: when a node is no longer allowed to split further, and thus become a leaf

print(train_control)

# Define the tuneGrid for XGBoost
xgboost_grid <- expand.grid(
  nrounds = c(500, 1000),
  max_depth = c(3, 6),
  eta = c(0.01, 0.1),
  gamma = 0,
  colsample_bytree = c(0.7),
  min_child_weight = c(1),
  subsample = c(0.7)
)

set.seed(123)

model_xgboost <- train(
  log_resale_price ~ ., 
  data = training_set, 
  method = "xgbTree", 
  trControl = train_control,
  tuneGrid = xgboost_grid,
  nthread = 1 # restrict each XGBoost instance to 1 thread
)

print(model_xgboost)
plot(model_xgboost)

# View top 20 drivers of log_resale_price
xgb_imp <- varImp(model_xgboost)
print(xgb_imp)
plot(xgb_imp, top = 20)


# Turn off parallel processing
stopCluster(cl)
registerDoSEQ()

# Save models to output folder -------------------------------------------------
saveRDS(model_random_forest, "output/models/model_random_forest.rds")
saveRDS(model_xgboost, "output/models/model_xgboost.rds")