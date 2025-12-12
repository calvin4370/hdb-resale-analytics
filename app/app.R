# ==============================================================================
# Shiny App for HDB Resale Price Prediction
# - Uses the trained XGBoost model
# ==============================================================================

library(shiny)
library(bslib) # for modern shiny apps (Bootstrap 5)
library(bsicons) # for Bootstrap icons
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

ui <- page_sidebar(
  
  title = "HDB Resale Price Predictor",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  # ---------- Sidebar ---------- #
  sidebar = sidebar(
    width = 350,
    title = "Flat Details",
    
    # ---------- Sidebar Content ---------- #
    # Address input
    selectizeInput(
      "sel_address", label = "Flat Address", 
      options = list(placeholder = 'Type to search'),
      choices = NULL
    ),
    card(
      class = "bg-light",
      strong("Town:"),
      textOutput("txt_town", inline = TRUE)
    ),
    
    # Flat Type input
    selectInput(
      "sel_flat_type", 
      label = "Flat Type", 
      choices = NULL
    ),
    
    # Flat Model input
    selectInput(
      "sel_flat_model", 
      label = "Flat Model", 
      choices = NULL
    ),
    
    # Floor Area input
    numericInput(
      "num_floor_area", 
      label = "Floor Area (sqm)", 
      value = NULL
    ),
    
    # Storey Level input
    sliderInput(
      "sld_storey_level", 
      label = "Storey Level", 
      value = 1, min = 1, max = 50
    ),
    
    hr(),
    
    # Predict Button
    actionButton(
      "btn_predict_price", 
      label = "Predict Resale Price", 
      class = "btn-success"
    ),
    
    # Reset Button
    actionButton(
      "btn_reset", 
      label = "Reset",
      class = "btn-secondary"
    ),
    
    # Disclaimer
    hr(),
    h4("DISCLAIMER")
    
    
  ),
  
  # ---------- Main Panel ---------- #
  h4("How much could your HDB flat be sold for on the open resale market today?"),
  
  # Display for predicted resale price and 95% CI
  layout_columns(
    # Predicted Resale Price
    value_box(
      title = "Predicted Resale Price",
      value = textOutput("txt_predicted_price"),
      showcase = bs_icon("graph-up-arrow"),
      theme = "primary",
      full_screen = FALSE
    )
    
    # 95% Condidence Interval
    # TODO
  ),
  
  # Big card with navigation tabs
  navset_card_underline(
    title = NULL,
    
    # Tab 1: Plot of Past Resale Data of Similar Flats
    nav_panel(
      title = "Historical Resale Transactions", 
      
      plotOutput("plot_past_sales"),
      card_footer(
        p(
          tags$span(style = "color: blue;;", "Blue dots = Individual sales"),
          " | ",
          tags$span(style = "color: red;", "Red line = Annual Average.")
        )
      )
    ),
    
    # Tab 2: Comparison table of Similar Flats Sold arranged by Year
    nav_panel(
      title = "Similar Flats Sold", 
      p("Recent resale transactions for similar units:"),
      DT::dataTableOutput("dt_comparable_table")
    ),
    
    # Tab 3: Flat Features input to trained model for prediction
    nav_panel(
      title = "Flat Details", 
      verbatimTextOutput("vtxt_flat_details")
    )
  ),
  
  # Footer for Credits
  tags$div(
    class = "text-center text-muted mt-3",
    
    hr(),
    p(
      "Created by Chan Jun Jie | ",
      tags$a(
        href = "https://github.com/calvin4370/hdb-resale-analytics",
        target = "_blank", # open in a new tab to not disrupt the Shiny App
        "Github"
      )
    )
  )
  
)


# ==============================================================================
# Backend
# ==============================================================================

server <- function(input, output, session) {
  
}

# ==============================================================================

# Run Shiny App
shinyApp(ui = ui, server = server)