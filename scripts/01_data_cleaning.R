# ==============================================================================
# SCRIPT:  01_data_cleaning.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-02
# PURPOSE: Clean raw hdb resale data to output cleaned dataset
# INPUTS:  data/raw/raw_resale_prices.csv
# OUTPUTS: data/processed/cleaned_resale_prices.csv
# ==============================================================================

library(tidyverse)

# Read raw resale data and check dataframe structure ---------------------------
raw_data <- read_csv("data/raw/raw_resale_prices.csv")
glimpse(raw_data)


# Clean data -------------------------------------------------------------------
cleaned_data <- raw_data %>%
  mutate(
    # Combine `block` and `street_name` into one column `address`
    address = paste(block, street_name),
    
    # Extract the years and months parts out of remaining_lease. The source
    # writes a one-month remainder in the singular ("62 years 01 month"), so
    # the lookahead must not require the plural — matching only " months"
    # silently zeroes the month on 8% of rows.
    remaining_lease_years_part = as.numeric(str_extract(remaining_lease, "\\d+(?= year)")),
    remaining_lease_months_part = as.numeric(str_extract(remaining_lease, "\\d+(?= month)")),
    remaining_lease_months_part = replace_na(remaining_lease_months_part, 0), # no months part at all
    
    # Convert remaining_lease to numeric form (in years)
    remaining_lease_numeric = remaining_lease_years_part + (remaining_lease_months_part / 12),
    
    # Convert <chr> month (resale date) into <date> resale_date and <dbl>
    # resale_year
    resale_date = ym(month),
    resale_year = year(resale_date)
  ) %>%

  # Storey range column is banded (e.g. "01 TO 03"). Model on the band midpoint and keep the
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

  # Intermediates, plus the source columns that have been superseded: `month`
  # by resale_date, `remaining_lease` by its numeric form, and `block` by
  # `address`. Nothing downstream of this script reads them.
  select(
    -remaining_lease_years_part,
    -remaining_lease_months_part,
    -storey_lower,
    -storey_upper,
    -month,
    -block,
    -remaining_lease
  )


# Check cleaned_data structure
glimpse(cleaned_data)


# Save cleaned csv
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(cleaned_data, "data/processed/cleaned_resale_prices.csv")