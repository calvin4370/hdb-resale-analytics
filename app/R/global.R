# ==============================================================================
# Shared constants and helpers for the Shiny app
# ==============================================================================

CURRENT_YEAR <- 2025 # would need to update model and this variable every year
MAX_LEASE_YEARS <- 99

# Earliest lease start worth accepting. Anything older is a typo rather than a
# flat, and would produce a negative remaining lease.
MIN_LEASE_START_YEAR <- 1960


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
