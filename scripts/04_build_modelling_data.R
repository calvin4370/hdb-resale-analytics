# ==============================================================================
# SCRIPT:  04_build_modelling_data.R
# AUTHOR:  Chan Jun Jie
# DATE:    2026-08-16
# PURPOSE: Apply the documented exclusion rules that turn the enriched dataset
#          into the dataset the models are trained on. Every row dropped is
#          attributable to a named rule.
# INPUTS:  data/processed/enriched_resale_prices.csv
# OUTPUTS: data/processed/modelling_resale_prices.csv
# ==============================================================================

library(tidyverse)

# Flats at or above this size are HDB terrace houses and outsized maisonettes.
# They are a distinct product from the flats the model is meant to price and are
# too few to learn from.
MAX_FLOOR_AREA_SQM <- 200


# Applies one exclusion rule and reports what it removed, so the row count of
# the modelling dataset is always traceable to a specific decision.
apply_rule <- function(data, rule, f) {
  n_before <- nrow(data)
  out <- f(data)
  message(glue::glue(
    "  {rule}: -{n_before - nrow(out)} rows ({nrow(out)} remain)"
  ))
  out
}


enriched_data <- read_csv(
  "data/processed/enriched_resale_prices.csv",
  show_col_types = FALSE
)

message(glue::glue("Enriched dataset: {nrow(enriched_data)} rows"))

modelling_data <- enriched_data %>%
  # The source data carries no transaction ID, so identical rows cannot be
  # distinguished from two genuinely separate sales of matching units in the
  # same block and month. They are dropped as presumed duplicates.
  apply_rule("exact duplicate rows", distinct) %>%
  apply_rule(
    glue::glue("floor area >= {MAX_FLOOR_AREA_SQM} sqm"),
    \(x) filter(x, floor_area_sqm < MAX_FLOOR_AREA_SQM)
  )

message(glue::glue(
  "Modelling dataset: {nrow(modelling_data)} rows ",
  "({nrow(enriched_data) - nrow(modelling_data)} excluded in total)"
))


stopifnot(
  "modelling dataset is empty" = nrow(modelling_data) > 0,
  "resale_price must be positive for log transformation" =
    all(modelling_data$resale_price > 0),
  "unexpected missing values in model predictors" =
    !anyNA(modelling_data[, c(
      "town", "flat_type", "flat_model", "floor_area_sqm",
      "storey_mid", "remaining_lease_numeric", "distance_to_cbd",
      "distance_to_nearest_mrt", "resale_year", "lat", "long"
    )])
)


output_filepath <- "data/processed/modelling_resale_prices.csv"
write_csv(modelling_data, output_filepath)
message(glue::glue("Saved modelling data to {output_filepath}"))
