# ==============================================================================
# FILE:    R/geocoding.R
# AUTHOR:  Chan Jun Jie
# PURPOSE: OneMap lookup and the coordinate checks applied to its results.
#          Sourced by 02_geocoding.R, which validates on acceptance, and by
#          02b_validate_geocoding.R, which re-checks the saved file.
# ==============================================================================

library(tidyverse)
library(httr)
library(jsonlite)

ONEMAP_SEARCH_URL <- "https://www.onemap.gov.sg/api/common/elastic/search"

# Bounding box for Singapore. A geocode outside it means OneMap matched
# something other than the intended address.
SG_LAT_RANGE <- c(1.15, 1.48)
SG_LONG_RANGE <- c(103.6, 104.1)

# OneMap is rate limited and occasionally drops a request under load.
GEOCODE_RETRIES <- 3
GEOCODE_PAUSE_BASE <- 0.5
GEOCODE_SLEEP_SECONDS <- 0.1


in_singapore <- function(lat, long) {
  !is.na(lat) & !is.na(long) &
    between(lat, SG_LAT_RANGE[1], SG_LAT_RANGE[2]) &
    between(long, SG_LONG_RANGE[1], SG_LONG_RANGE[2])
}


# Geocode one address. Returns a one-row tibble with lat/long on success, or
# NA coordinates plus a `reason` naming the failure — a silent NA cannot be
# distinguished from a network blip, and the two need different responses.
#
# The bounding box is enforced here rather than downstream because OneMap's
# search is fuzzy and returns its best guess for a malformed query. A match
# outside Singapore is a wrong building, not a coordinate worth keeping.
geocode_address <- function(address) {
  failure <- function(reason) {
    tibble(address = address, lat = NA_real_, long = NA_real_, reason = reason)
  }

  response <- tryCatch(
    RETRY(
      "GET",
      ONEMAP_SEARCH_URL,
      query = list(
        searchVal = address,
        returnGeom = "Y",
        getAddrDetails = "Y",
        pageNum = 1
      ),
      times = GEOCODE_RETRIES,
      pause_base = GEOCODE_PAUSE_BASE,
      quiet = TRUE
    ),
    error = function(e) e
  )

  if (inherits(response, "error")) {
    return(failure(paste("request failed:", conditionMessage(response))))
  }
  if (status_code(response) != 200) {
    return(failure(paste("http", status_code(response))))
  }

  parsed <- tryCatch(
    fromJSON(rawToChar(response$content)),
    error = function(e) e
  )

  if (inherits(parsed, "error")) {
    return(failure("unparseable response"))
  }
  if (is.null(parsed$found) || parsed$found < 1) {
    return(failure("no match"))
  }

  lat <- as.numeric(parsed$results$LATITUDE[1])
  long <- as.numeric(parsed$results$LONGITUDE[1])

  if (!in_singapore(lat, long)) {
    return(failure(glue::glue("outside Singapore ({lat}, {long})")))
  }

  tibble(address = address, lat = lat, long = long, reason = NA_character_)
}
