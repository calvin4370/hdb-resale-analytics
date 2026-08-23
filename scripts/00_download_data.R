# ==============================================================================
# SCRIPT:  00_download_data.R
# AUTHOR:  Chan Jun Jie
# DATE:    2026-08-17
# PURPOSE: Fetch the current resale-price extract from data.gov.sg.
#          NOT part of run_all.R and never run automatically: the committed
#          snapshot is what pins the published numbers, so replacing it is a
#          deliberate act that invalidates every result until the pipeline is
#          re-run end to end.
# INPUTS:  data.gov.sg dataset d_8b84c4ee58e3cfc0ece0d773c8ca6abc
# OUTPUTS: data/raw/raw_resale_prices_<YYYY-MM-DD>.csv, or
#          data/raw/raw_resale_prices.csv with --overwrite
#
# USAGE:   Rscript scripts/00_download_data.R              # dated file, safe
#          Rscript scripts/00_download_data.R --overwrite  # replace snapshot
# ==============================================================================

library(tidyverse)
library(httr)
library(jsonlite)

DATASET_ID <- "d_8b84c4ee58e3cfc0ece0d773c8ca6abc"
API_BASE <- "https://api-open.data.gov.sg/v1/public/api/datasets"

overwrite <- "--overwrite" %in% commandArgs(trailingOnly = TRUE)
destination <- if (overwrite) {
  "data/raw/raw_resale_prices.csv"
} else {
  glue::glue("data/raw/raw_resale_prices_{Sys.Date()}.csv")
}


# data.gov.sg stages the extract to S3 and hands back a signed URL. The link is
# short-lived, so it is requested immediately before the download.
response <- RETRY(
  "GET", glue::glue("{API_BASE}/{DATASET_ID}/initiate-download"),
  times = 3, pause_base = 1, quiet = TRUE
)

stopifnot("could not reach data.gov.sg" = status_code(response) %in% c(200, 201))

download_url <- fromJSON(rawToChar(response$content))$data$url
stopifnot("API returned no download URL" = !is.null(download_url))

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
download.file(download_url, destination, mode = "wb", quiet = TRUE)

fresh <- read_csv(destination, show_col_types = FALSE)

# Guard against writing a truncated or restructured file over a good snapshot.
stopifnot(
  "downloaded file is empty" = nrow(fresh) > 0,
  "downloaded file is missing expected columns" =
    all(c("month", "town", "flat_type", "block", "street_name", "storey_range",
          "floor_area_sqm", "flat_model", "lease_commence_date",
          "remaining_lease", "resale_price") %in% names(fresh))
)

message(glue::glue(
  "Downloaded {nrow(fresh)} rows covering {min(fresh$month)} to ",
  "{max(fresh$month)} -> {destination}"
))

if (!overwrite) {
  message(glue::glue(
    "Snapshot NOT replaced. Compare against data/raw/raw_resale_prices.csv, ",
    "then re-run with --overwrite and rebuild via run_all.R."
  ))
}
