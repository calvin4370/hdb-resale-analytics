# ==============================================================================
# Shiny App for HDB Resale Price Prediction
# - Uses the trained XGBoost model
# ==============================================================================

library(shiny)
library(here)
library(tidyverse)

# ==============================================================================
# Setup
# ==============================================================================

# Load and prepare data --------------------------------------------------------
# Must be in the same structure (factors for categorical variables) as the
# modelling df used to train the xgboost model
df <- read_csv(here("data", "processed", "modelling_resale_prices.csv")) %>%
  mutate(
    town = as.factor(town),
    flat_type = as.factor(flat_type),
    flat_model = as.factor(flat_model),
    full_address = paste(block, street_name),
    resale_year = as.numeric(substr(month, 1, 4)),
    storey_range = paste0(sprintf("%02d", storey_range_floored), " TO ", sprintf("%02d", storey_range_floored + 2))
  )


# ==============================================================================
# Frontend
# ==============================================================================

ui <- fluidPage(
  titlePanel("HDB Resale Price Predictor"),
  h4("How much could your HDB flat be sold for on the open resale market today?"),
  
  
)


# ==============================================================================
# Backend
# ==============================================================================

server <- function(input, output, session) {
  
}

# ==============================================================================

# Run Shiny App
shinyApp(ui = ui, server = server)