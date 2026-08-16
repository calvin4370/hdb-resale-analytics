# ==============================================================================
# SCRIPT:  02_geocoding.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-02
# PURPOSE: Retrieve GPS coordinates for every transaction address from OneMap.
#          Resumable: the output file doubles as the cache, so a re-run only
#          queries addresses that are not already resolved.
# INPUTS:  1. data/processed/cleaned_resale_prices.csv
#          2. data/external/hdb_coordinates.csv  (optional, used as cache)
# OUTPUTS: data/external/hdb_coordinates.csv
# ==============================================================================

library(tidyverse)

source("R/geocoding.R")

COORDS_PATH <- "data/external/hdb_coordinates.csv"

# Partial results are flushed this often, so an interrupted run keeps its work.
CHECKPOINT_EVERY <- 250


df <- read_csv(
  "data/processed/cleaned_resale_prices.csv",
  show_col_types = FALSE
)

unique_addresses <- distinct(df, address)

# Only successful geocodes are ever written, so presence in the file means
# resolved. Failures are simply absent and get retried on the next run.
cached <- if (file.exists(COORDS_PATH)) {
  read_csv(COORDS_PATH, show_col_types = FALSE)
} else {
  tibble(address = character(), lat = double(), long = double())
}

pending <- unique_addresses %>%
  anti_join(cached, by = "address") %>%
  pull(address)

message(glue::glue(
  "{nrow(unique_addresses)} unique addresses, {nrow(cached)} already cached, ",
  "{length(pending)} to geocode"
))

if (length(pending) == 0) {
  message("Nothing to do.")
} else {
  results <- vector("list", length(pending))
  progress_bar <- txtProgressBar(min = 0, max = length(pending), style = 3)

  # Written out mid-run as well as at the end. Anything already collected
  # survives an interruption and is picked up as cache next time.
  flush <- function(upto) {
    resolved <- bind_rows(results[seq_len(upto)]) %>%
      filter(!is.na(lat)) %>%
      select(address, lat, long)

    # Appended, not re-sorted: a refresh should diff as the handful of new
    # addresses rather than as a rewrite of the whole file.
    write_csv(bind_rows(cached, resolved), COORDS_PATH)
  }

  for (i in seq_along(pending)) {
    results[[i]] <- geocode_address(pending[i])
    setTxtProgressBar(progress_bar, i)

    if (i %% CHECKPOINT_EVERY == 0) flush(i)
    Sys.sleep(0.1) # do not spam the OneMap API too quickly
  }

  close(progress_bar)
  flush(length(pending))

  # Reported per address and reason rather than left as an anonymous NA, so a
  # re-run can be judged instead of guessed at.
  attempted <- bind_rows(results)
  failed <- filter(attempted, is.na(lat))

  if (nrow(failed) > 0) {
    message(glue::glue("{nrow(failed)} addresses failed:"))
    print(count(failed, reason, sort = TRUE))
    print(head(select(failed, address, reason), 20))
  }

  message(glue::glue(
    "Geocoded {nrow(attempted) - nrow(failed)} of {length(pending)} ",
    "pending addresses"
  ))
}

final <- read_csv(COORDS_PATH, show_col_types = FALSE)
message(glue::glue("{nrow(final)} coordinates saved to {COORDS_PATH}"))
