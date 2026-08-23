# ==============================================================================
# Shared constants and helpers for the Shiny app
# ==============================================================================

MAX_LEASE_YEARS <- 99

# Earliest lease start worth accepting. Anything older is a typo rather than a
# flat, and would produce a negative remaining lease.
MIN_LEASE_START_YEAR <- 1960


# Band multipliers for one flat type, falling back to the global band when that
# type was too thin for its own quantiles. Built by cv_error_quantiles() in
# R/evaluate.R and shipped in model_metadata.
error_band_for <- function(error_quantiles, flat_type) {
  match <- error_quantiles$by_flat_type[
    error_quantiles$by_flat_type$flat_type == flat_type,
  ]

  if (nrow(match) == 1) {
    return(c(lower = match$lower, upper = match$upper))
  }

  error_quantiles$global
}


# Most frequent value in a vector, preserving the input type. Used to pre-fill
# fields from a block's transaction history.
most_common <- function(x) {
  counts <- table(x, useNA = "no")

  if (length(counts) == 0) {
    return(NA)
  }

  value <- names(counts)[which.max(counts)]
  if (is.numeric(x)) as.numeric(value) else value
}
