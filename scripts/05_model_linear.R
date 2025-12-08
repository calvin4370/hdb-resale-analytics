# ==============================================================================
# SCRIPT:  05_model_linear.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-08
# PURPOSE: Train Linear Regression model for HDB Resale Price Prediction.
#          Establishes a baseline.
# INPUTS:  data/processed/modelling_resale_prices.csv
# OUTPUTS: 1. outputs/models/lm_baseline.rds
#          2. outputs/models/lm_stepwise.rds
# ==============================================================================

library(tidyverse)
library(caret) # Classification And REgression Training; for stratified sampling by resale price

# Load data and prepare it for modelling ---------------------------------------
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
  p = 0.8,              # 80% for Training
  list = FALSE          # Return a vector of row numbers, not a list
)

training_set <- model_df[training_indices, ]
test_set <- model_df[-training_indices, ]

# Check dimensions for training set and testing set to ensure correctness
print(dim(training_set))
print(dim(test_set))


# Train Linear Regression Models ===============================================

# Set up training controls for K-Fold Cross Validation ------------------------
train_control <- trainControl(
  method = "cv", # use K-Fold Cross Validation
  number = 10    # with K = 10
)
print(train_control)


# Model 1: Baseline OLS (full model with all reasonable variables) -------------
set.seed(123) # for reproducability
model_lm_baseline <- train(
  log_resale_price ~ ., 
  data = training_set, 
  method = "lm", 
  trControl = train_control
)

print(model_lm_baseline)
summary(model_lm_baseline$finalModel)


# Model 2: Stepwise AIC (feature selection) ------------------------------------
# Train the LM with 10-Fold CV and use Backward Stepwise Regression Model
set.seed(123) # for reproducability
model_lm_stepwise <- train(
  log_resale_price ~ ., # Start with full model
  data = training_set,
  method = "lmStepAIC",
  trControl = train_control,
  direction = "backward", 
  trace = TRUE # set to FALSE to keep console clean by not showing every step
)

print(model_lm_stepwise)
summary(model_lm_stepwise$finalModel)


# Save models to output folder -------------------------------------------------
saveRDS(model_lm_baseline, "output/models/lm_baseline.rds")
saveRDS(model_lm_stepwise, "output/models/lm_stepwise.rds")