library(tidyverse)
library(lubridate)

raw_data <- read_csv("data/raw_resale_prices.csv")

clean_data <- raw_data %>%
  mutate(
    # Extract the years and months parts out of remaining_lease (in form: "XX years XX months")
    remaining_lease_years = as.numeric(str_extract(remaining_lease, "\\d+(?= years)")),
    remaining_lease_months = as.numeric(str_extract(remaining_lease, "\\d+(?= months)")),
    
    # Replace NAs in remaining_lease_months with 0s
    remaining_lease_months = replace_na(remaining_lease_months, 0),
    
    # Combine into a single numeric column
    remaining_lease_numeric = remaining_lease_years + (remaining_lease_months / 12),
    
    # Convert storey_range into a numeric variable using only the first part (e.g. "01 TO 03" -> 1)
    storey_ranged_floored = as.numeric(str_sub(storey_range, 1, 2))
  )

# Save cleaned csv
write_csv(clean_data, "data/cleaned_resale_prices.csv")
