# ==============================================================================
# SCRIPT:  08_compare_models.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-10
# PURPOSE: Load all trained models and compare their RMSE, MAE, and R2 values
# INPUTS:  1. data/processed/modelling_resale_prices.csv
#          2. output/models/*.rds
# ==============================================================================

library(tidyverse)
library(caret) 
library(ranger)
library(xgboost)

# Load data and recreate the original test set =================================
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

# Re-create the training-test split using the same seed
set.seed(123)
training_indices <- createDataPartition(y = model_df$log_resale_price, p = 0.8, list = FALSE)

# Select only the testing set rows
test_set <- model_df[-training_indices, ]
print(paste("Test Set:", nrow(test_set), "rows"))


# Load all previously trained models ===========================================
# Hash map of (model name : model)s
model_map <- list(
  # Linear Models
  "OLS Baseline" = readRDS("output/models/lm_baseline.rds"),
  "Stepwise AIC" = readRDS("output/models/lm_stepwise.rds"),
  
  # Regularised Linear Models
  "Ridge" = readRDS("output/models/glmnet_ridge.rds"),
  "Lasso" = readRDS("output/models/glmnet_lasso.rds"),
  "Elastic Net" = readRDS("output/models/glmnet_elastic.rds"),
  
  # Advanced Models
  "Random Forest" = readRDS("output/models/model_random_forest.rds"),
  "XGBoost" = readRDS("output/models/model_xgboost.rds")
)


# Define a function to evaluate models =========================================
evaluate_model <- function(model, data, model_name) {
  # Predict log_resale_price using test data
  pred_log_price <- predict(model, data)
  
  # Convert to real SGD
  pred_real_price <- exp(pred_log_price)
  actual_real_price <- exp(data$log_resale_price)
  
  # Calculate Performance Metrics
  rmse_value <- RMSE(pred_real_price, actual_real_price)
  mae_value <- MAE(pred_real_price, actual_real_price)
  r2_value <- R2(pred_real_price, actual_real_price)
  
  # Return a data frame with one row to store the metrics of the input model
  # I intend to bind the rows of each model's df together later to form one table of results
  return(data.frame(
    Model = model_name,
    RMSE_SGD = round(rmse_value, 2),
    MAE_SGD = round(mae_value, 2),
    R_2 = round(r2_value, 4)
  ))
}


# Evaluate the trained models on the test set ==================================
results <- NULL # like an empty data.frame

for (model_name in names(model_map)) {
  curr_model <- model_map[[model_name]]
  result_row <- evaluate_model(curr_model, test_set, model_name)
  results <- bind_rows(results, result_row)
}

# Sort by increasing RMSE (lowest at the top is the best)
results <- results %>% arrange(RMSE_SGD)
print(results)


# Visualise the performance of the best model ==================================
# Actual vs Predicted Plot
# Allows us to see biases for certain ranges of resale_price
best_model_name <- results$Model[1]
best_model <- model_map[[best_model_name]]
print(paste("The best model (with lowest RMSE) is:", best_model_name))

# Generate data for plotting
test_set$pred_log_price <- predict(best_model, test_set)
test_set$pred_real_price <- exp(test_set$pred_log_price)
test_set$actual_real_price <- exp(test_set$log_resale_price)

# Plot Scatterplot of Predicted Price ($) vs Actual Price ($)
best_model_performance_plot <- ggplot(test_set, aes(x = actual_real_price, y = pred_real_price)) +
  geom_point(alpha = 0.1, color = "darkblue") + 
  
  # Plot a reference line (y = x) for perfect predictions
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  
  # Format axes
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  
  labs(title = paste0(best_model_name, ": Actual vs Predicted Resale Prices"),
       x = "Actual Resale Price (SGD)",
       y = "Predicted Resale Price (SGD)") +
  theme_minimal()

print(best_model_performance_plot)
ggsave("output/figures/best_model_performance.png", plot = best_model_performance_plot, width = 8, height = 6)