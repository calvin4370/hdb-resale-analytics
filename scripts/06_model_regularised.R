# ==============================================================================
# SCRIPT:  06_model_regularised.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-08
# PURPOSE: Train Regularized Regression Models
#          (Better handles multicollinearity and robust feature selection)
# INPUTS:  data/processed/modelling_resale_prices.csv
# OUTPUTS: 1. output/models/glmnet_ridge.rds
#          2. output/models/glmnet_lasso.rds
#          3. output/models/glmnet_elastic.rds
# ==============================================================================

library(tidyverse)
library(caret) 
library(glmnet) # for Generalised Linear Regression with Elastic Net

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

# Check the reference levels and verify correct order of levels for ordinal data types
print(levels(model_df$town))
print(levels(model_df$flat_type))
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


# Train Regularised Linear Regression Models ===================================

# Set up training controls for K-Fold Cross Validation ------------------------
train_control <- trainControl(
  method = "cv", # use K-Fold Cross Validation
  number = 10    # with K = 10
)
print(train_control)

# Model 1: Ridge Regression (L2 Norm) ------------------------------------------
# Hyperparameters: λ (penalty)
# Shrinks coefficients

# Define the tuneGrid for Ridge regression (seq of 100 λ values from 0.0001 to 1)
ridge_grid <- expand.grid(
  alpha = 0, # for Ridge regression only
  lambda = seq(0.0001, 1, length = 100) # Test 100 λ values from 0.0001 to 1
)

set.seed(123)
model_ridge <- train(
  log_resale_price ~ .,
  data = training_set,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = ridge_grid,
  preProcess = c("center", "scale") # standardise data by minising mean then dividing by sd
)

print(model_ridge)
plot(model_ridge) 

# Extract coefs for best lambda
ridge_coefs <- coef(model_ridge$finalModel, model_ridge$bestTune$lambda)
# Sort and print top 10
head(sort(abs(ridge_coefs[,1]), decreasing = TRUE), 10)


# Model 2: Lasso Regression (L1 Norm) ------------------------------------------
# Hyperparameters: λ (penalty)
# Feature Selection: Forces coefficients to be exactly 0 if they are considered useless

# Define the tuneGrid for Lasso regression (seq of 100 λ values from 0.0001 to 1)
lasso_grid <- expand.grid(
  alpha = 1, # Lasso regression only
  lambda = seq(0.0001, 0.1, length = 100) # Test 100 λ values from 0.0001 to 0.1 (Lasso tends to use smaller λ values)
)

set.seed(123)
model_lasso <- train(
  log_resale_price ~ .,
  data = training_set,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = lasso_grid,
  preProcess = c("center", "scale") # standardise data by minising mean then dividing by sd
)

print(model_lasso)
plot(model_lasso)

lasso_coefs <- coef(model_lasso$finalModel, model_lasso$bestTune$lambda)
# Print only non-zeroes to prove feature selection worked
lasso_matrix <- as.matrix(lasso_coefs)
print(lasso_matrix[lasso_matrix != 0, ])


# Model 3: Elastic Net ---------------------------------------------------------
# Hyperparameters: λ (penalty), alpha (how much of Ridge and Lasso to use)

elastic_net_grid <- expand.grid(
  alpha = seq(0, 1, length = 10), # Test 10 alpha values from 0 to 1
  lambda = seq(0.0001, 0.5, length = 20) # Test 20 λ values from 0.0001 to 0.5
)

set.seed(123)
model_elastic_net <- train(
  log_resale_price ~ .,
  data = training_set,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = elastic_net_grid,
  preProcess = c("center", "scale") # standardise data by minising mean then dividing by sd
)

print(model_elastic_net)
plot(model_elastic_net)

# Extract coefs for the winning lambda
elastic_coefs <- coef(model_elastic_net$finalModel, model_elastic_net$bestTune$lambda)

# Filter for non-zero coefficients
elastic_matrix <- as.matrix(elastic_coefs)
selected_vars <- elastic_matrix[elastic_matrix != 0, ]
print(selected_vars)


# Save models to output folder -------------------------------------------------
saveRDS(model_ridge, "output/models/glmnet_ridge.rds")
saveRDS(model_lasso, "output/models/glmnet_lasso.rds")
saveRDS(model_elastic_net, "output/models/glmnet_elastic.rds")