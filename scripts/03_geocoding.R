# ==============================================================================
# SCRIPT: 03_geocoding.R
# AUTHOR: Chan Jun Jie
# DATE:   2025-12-02
# PURPOSE: Query OneMap API to retrieve GPS coordinates (lat/long) for all unique addresses
# INPUTS:  data/cleaned_resale_prices.csv
# OUTPUTS: data/hdb_coordinates.csv
# ==============================================================================

library(tidyverse)
library(httr) # for sending HTTP requests
library(jsonlite) # reads JSON to output R dataframes

# Read cleaned data from 01_cleaning.R
df <- read_csv("data/processed/cleaned_resale_prices.csv")

# Create a list of unique addresses using `block` + `street_name`
unique_addresses <- df %>%
  mutate(address = paste(block, street_name)) %>%
  distinct(address)

print(paste("Number of unique addresses to find:", nrow(unique_addresses)))


# -------------------- OneMap API call function -------------------- #
get_onemap_coords <- function(address) {
  # Calls OneMap API with address and returns a tibble of address, lat, long
  # api endpoint and query as defined in OneMap API
  api_endpoint <- "https://www.onemap.gov.sg/api/common/elastic/search"
  query <- list(
    searchVal = address, 
    returnGeom = "Y", 
    getAddrDetails = "Y", 
    pageNum = 1
  )
  
  tryCatch({
    result <- GET(api_endpoint, query = query) # OneMap API returns content in raw bytes form
    json <- rawToChar(result$content) # convert raw bytes to json text
    data <- fromJSON(json) # convert json text to R dataframe
    
    if (data$found > 0) {
      return(tibble(
        address = address,
        lat = as.numeric(data$results$LATITUDE[1]),
        long = as.numeric(data$results$LONGITUDE[1])
      ))
    } else { # If no search result found, return with NA coords
      return(tibble(address = address, lat = NA, long = NA))
    }
  }, error = function(e) { # If no search result found, return with NA coords
    return(tibble(address = address, lat = NA, long = NA))
  })
}


# -------------------- Call the API for every address -------------------- #
results <- list() # to store tibbles of (address, lat, long)
print("Geocoding of addresses in progress...")

# Show a progress bar in the console while this script is running
total_addresses <- nrow(unique_addresses)
progress_bar <- txtProgressBar(min = 0, max = total_addresses, style = 3)

for (i in 1 : total_addresses) {
  curr_address <- unique_addresses$address[i]
  results[[i]] <- get_onemap_coords(curr_address)
  setTxtProgressBar(progress_bar, i)
  Sys.sleep(0.1) # do not spam the OneMap API too quickly
}


# -------------------- Save results to a new csv -------------------- #
final_coords <- bind_rows(results) # convert list of tibbles of (address, lat, long) into one dataframe
output_filepath <- "data/external/hdb_coordinates.csv"
write_csv(final_coords, output_filepath)
print(paste("Geospatial data saved to '", output_filepath, "'", sep = ""))