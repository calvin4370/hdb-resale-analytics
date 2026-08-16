# ==============================================================================
# FILE:    R/models.R
# AUTHOR:  Chan Jun Jie
# PURPOSE: Registry of trained model artifacts, so the comparison script and
#          the app bundle resolve the same names to the same files.
# ==============================================================================

library(tidyverse)

MODEL_FILES <- c(
  "OLS Baseline" = "output/models/lm_baseline.rds",
  "Stepwise AIC" = "output/models/lm_stepwise.rds",
  "Ridge" = "output/models/glmnet_ridge.rds",
  "Lasso" = "output/models/glmnet_lasso.rds",
  "Elastic Net" = "output/models/glmnet_elastic.rds",
  "Random Forest" = "output/models/model_random_forest.rds",
  "XGBoost" = "output/models/model_xgboost.rds"
)


load_trained_models <- function() {
  missing <- MODEL_FILES[!file.exists(MODEL_FILES)]

  if (length(missing) > 0) {
    stop(
      glue::glue(
        "Trained models not found: {paste(names(missing), collapse = ', ')}. ",
        "Run scripts 06 to 08 first."
      ),
      call. = FALSE
    )
  }

  map(MODEL_FILES, readRDS)
}
