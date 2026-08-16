# ==============================================================================
# SCRIPT:  01_data_cleaning.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-02
# PURPOSE: Clean raw hdb resale data to output cleaned dataset
# INPUTS:  data/raw/raw_resale_prices.csv
# OUTPUTS: data/processed/cleaned_resale_prices.csv
# ==============================================================================

library(tidyverse)
library(lubridate)

# Read raw resale data and check dataframe structure ---------------------------
raw_data <- read_csv("data/raw/raw_resale_prices.csv")
glimpse(raw_data)


# Clean data -------------------------------------------------------------------
cleaned_data <- raw_data %>%
  mutate(
    # Combine `block` and `street_name` into one column `address`
    address = paste(block, street_name),
    
    # Extract the years and months parts out of remaining_lease (in form: "XX years XX months")
    remaining_lease_years_part = as.numeric(str_extract(remaining_lease, "\\d+(?= years)")),
    remaining_lease_months_part = as.numeric(str_extract(remaining_lease, "\\d+(?= months)")),
    remaining_lease_months_part = replace_na(remaining_lease_months_part, 0), # may be NAs so replace with 0s
    
    # Convert remaining_lease to numeric form (in years)
    remaining_lease_numeric = remaining_lease_years_part + (remaining_lease_months_part / 12),
    
    # Convert <chr> month (resale date) into <date> resale date, <dbl> resale_year and <dbl> resale_month columns
    resale_date = ym(month),
    resale_year = year(resale_date),
    resale_month = month(resale_date)
  ) %>%

  # Storey is banded (e.g. "01 TO 03"). Model on the band midpoint and keep the
  # original string for display.
  separate_wider_delim(
    storey_range,
    delim = " TO ",
    names = c("storey_lower", "storey_upper"),
    cols_remove = FALSE
  ) %>%
  mutate(
    storey_mid = (as.numeric(storey_lower) + as.numeric(storey_upper)) / 2
  ) %>%

  select(
    -remaining_lease_years_part,
    -remaining_lease_months_part,
    -storey_lower,
    -storey_upper
  )


# Check cleaned_data structure
glimpse(cleaned_data)


# Save cleaned csv
write_csv(cleaned_data, "data/processed/cleaned_resale_prices.csv")