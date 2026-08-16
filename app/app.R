# ==============================================================================
# Shiny App for HDB Resale Price Prediction
# - Uses the trained XGBoost model
# ==============================================================================

library(shiny)
library(bslib) # for modern shiny apps (Bootstrap 5)
library(bsicons) # for Bootstrap icons
library(tidyverse)
library(xgboost) # needed to run the trained xgboost model
library(caret) # needed to run caret::predict on the caret trained xgboost model
library(DT) # for datatable and formatCurrency


# ==============================================================================
# Setup
# ==============================================================================

# Load and prepare data --------------------------------------------------------
# Must be in the same structure (factors for categorical variables) as the
# modelling df used to train the xgboost model
# Built by scripts/10_build_app_bundle.R from the trained artifacts. Reading
# prepared .rds files rather than parsing the full modelling CSV keeps factor
# levels identical to the ones the model was trained on, and cuts cold start
# from a 47 MB parse to a few megabytes.
df <- readRDS("data/transactions.rds")
address_lookup <- readRDS("data/address_lookup.rds")
model_metadata <- readRDS("data/model_metadata.rds")

# Get unique values for dropdown menus
flat_type_choices <- levels(df$flat_type)
flat_model_choices <- levels(df$flat_model)

# Input bounds taken from the training data, so the app cannot ask the model
# for a prediction outside the range it was fitted on.
floor_area_bounds <- range(df$floor_area_sqm)
lease_start_bounds <- c(
  max(min(df$lease_commence_date), MIN_LEASE_START_YEAR),
  CURRENT_YEAR
)
max_storey <- max(df$storey_mid)

# Load Model from app/models
model_deployed <- readRDS("models/model_deployed.rds")


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
      choices = NULL # Initialise choides as NULL first to enable server-side selectize later
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
      choices = flat_type_choices
    ),
    
    # Flat Model input
    selectInput(
      "sel_flat_model", 
      label = "Flat Model", 
      choices = flat_model_choices
    ),
    
    # Floor Area input
    numericInput(
      "num_floor_area",
      label = "Floor Area (sqm)",
      value = NULL,
      min = floor_area_bounds[1],
      max = floor_area_bounds[2],
      step = 1
    ),

    # Storey Level input
    sliderInput(
      "sld_storey_level",
      label = "Storey Level",
      min = 1,
      max = max_storey,
      value = 1
    ),

    # Lease Start Year
    numericInput(
      "num_lease_start_year",
      label = "Lease Start Year",
      value = NULL,
      min = lease_start_bounds[1],
      max = lease_start_bounds[2],
      step = 1
    ),
    
    hr(),
    
    # Predict Button
    actionButton(
      "btn_predict", 
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
    card(
      class = "bg-light",
      style = "font-size: 0.65em;",
      strong("Note:"),
      p("This model predicts prices only for flats eligible for resale on the ", strong("Open Resale Market.")),
      p(class = "mb-0", "It does not apply to:"),
      tags$ul(
        style = "padding-left: 20px; margin-top: -15px; margin-bottom: 0px;",
        tags$li("Flats that have not met the Minimum Occupation Period (MOP) (5-10 years)."),
        tags$li("Short-lease 2-Room Flexi Flats (Must be returned to HDB)."),
        tags$li("Rental Flats.")
      )
    ),
    
    
  ),
  
  # ---------- Main Panel ---------- #
  h4("How much can your HDB flat be sold for on the Open Resale Market today?"),
  
  # Display for predicted resale price and 95% CI
  layout_columns(
    fill = FALSE, # prevent the box from filling the whole space vertically
    
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
      
      plotOutput("plot_similar_sales"),
      card_footer(
        p(
          tags$span(style = "color: blue;;", "Blue dots = Individual sales"),
          " | ",
          tags$span(style = "color: red;", "Red line = Annual Average.")
        )
      )
    ),
    
    # Tab 2: DT::datatable of Similar Flats Sold arranged by Year
    nav_panel(
      title = "Similar Flats Sold", 
      p("Recent resale transactions for similar units:"),
      DT::dataTableOutput("dt_similar_sales")
    ),
    
    # Tab 3: Flat Features input to trained model for prediction
    nav_panel(
      title = "Flat Details", 
      verbatimTextOutput("vtxt_flat_details")
    )
  ),
  
  # Footer for Credits
  tags$div(
    class = "text-center text-muted small mt-3",
    
    hr(),
    p(
      "Created by Chan Jun Jie | ",
      tags$a(
        href = "https://github.com/calvin4370/hdb-resale-analytics",
        target = "_blank", # open in a new tab to not disrupt the Shiny App
        "Github"
      )
    ),
    # Stated so a stale bundle is visible rather than silent.
    p(
      class = "mb-0",
      sprintf(
        "Model: %s, trained on %d-%d transactions. Data through %s.",
        model_metadata$model_name,
        model_metadata$train_years[1],
        model_metadata$train_years[2],
        format(model_metadata$data_through, "%b %Y")
      )
    )
  )
  
)


# ==============================================================================
# Backend
# ==============================================================================

server <- function(input, output, session) {
  
  # ---------- Initialise categorical input fields as empty ---------- #
  updateSelectizeInput(
    session, 
    "sel_address", 
    choices = address_lookup$address, 
    selected = character(0), # Set selected as BLANK
    server = TRUE # use server-side selectize for improved performance as num options is very big
  )
  updateSelectInput(session, "sel_flat_type", selected = character(0))
  updateSelectInput(session, "sel_flat_model", selected = character(0))
  
  
  # ---------- Set up reactive variables ---------- #
  # (for auto selection of other fields (UX design))
  current_location_row <- reactive({
    req(input$sel_address) # proceed if user input a valid address
    address_lookup %>% 
      filter(address == input$sel_address) %>% 
      dplyr::slice(1) # get 1 row
  }) # returns (current address, town, distance_to_cbd, distance_to_nearest_mrt)
  
  # Get list of past sales with the same address as the current address input
  past_sales_with_same_address <- reactive({
    req(input$sel_address)
    df %>% filter(address == input$sel_address)
  })
  
  
  # ---------- Event Listeners ---------- #
  
  # Auto-fill and Restrict choices for Fields ----------------------------------
  # (UX feature: Help out lazy users with fields that usually need a google search))
  # (also only leave valid choices for fields e.g. flat_type and flat_model based on input$sel_address and past transaction data)
  
  # Whenever input$sel_address is updated, auto-fill fields and restrict choices
  observeEvent(input$sel_address, {
    req(past_sales_with_same_address()) # proceed if past data available
    data <- past_sales_with_same_address()
    
    # Set input$num_lease_start_year to most common Lease Start Year from data
    updateNumericInput(
      session, "num_lease_start_year",
      value = most_common(data$lease_commence_date)
    )

    # Restrict input$sel_flat_type to seen choices in past data and set to most common available
    updateSelectInput(
      session, "sel_flat_type",
      choices = sort(unique(data$flat_type)),
      selected = most_common(data$flat_type)
    )
  })
  
  # Whenever EITHER input$sel_address OR input$sel_flat_type is updated, 
  # auto-fill other fields and restrict choices
  observeEvent(c(input$sel_address, input$sel_flat_type), {
    req(past_sales_with_same_address(), input$sel_address, input$sel_flat_type) # proceed if past data available
    data <- past_sales_with_same_address() %>% 
      filter(flat_type == input$sel_flat_type)
    
    # Pre-fill floor area and flat model with the most common values seen at
    # this address for the chosen flat type.
    if (nrow(data) > 0) {
      updateNumericInput(
        session, "num_floor_area",
        value = most_common(data$floor_area_sqm)
      )

      updateSelectInput(
        session, "sel_flat_model",
        choices = sort(unique(data$flat_model)),
        selected = most_common(data$flat_model)
      )
    }
  })
  
  # Handle Button Presses ------------------------------------------------------
  # Reset Button
  observeEvent(input$btn_reset, {
    # Clear all input fields
    updateSelectizeInput(session, "sel_address", selected = character(0))

    updateSelectInput(session, "sel_flat_type", selected = character(0))
    updateSelectInput(session, "sel_flat_model", selected = character(0))
    
    updateNumericInput(session, "num_floor_area", value = NA)
    updateNumericInput(session, "num_lease_start_year", value = NA)
    
    updateSliderInput(session, "sld_storey_level", value = 1)
  })
  
  # Predict Resale Price Button
  predicted_resale_price <- eventReactive(input$btn_predict, {
    # Only proceed if all required input fields filled
    req(input$sel_address, input$sel_flat_type, input$sel_flat_model, 
        input$num_floor_area, input$num_lease_start_year)
    
    # Retrieve location data we don't expect user to fill in, but can derive based
    # on address through our lookup table
    curr_loc_row <- current_location_row()
    
    # Browser-side min/max on numericInput is advisory only, so the same bounds
    # are enforced here. Outside them the model would be extrapolating beyond
    # the data it was fitted on.
    validate(
      need(
        is.finite(input$num_floor_area) &&
          between(input$num_floor_area, floor_area_bounds[1],
                  floor_area_bounds[2]),
        glue::glue(
          "Enter a floor area between {floor_area_bounds[1]} and ",
          "{floor_area_bounds[2]} sqm."
        )
      ),
      need(
        is.finite(input$num_lease_start_year) &&
          between(input$num_lease_start_year, lease_start_bounds[1],
                  lease_start_bounds[2]),
        glue::glue(
          "Enter a lease start year between {lease_start_bounds[1]} and ",
          "{lease_start_bounds[2]}."
        )
      )
    )

    # Calculate other model input data for user input fields
    remaining_lease_years <- MAX_LEASE_YEARS - (CURRENT_YEAR - input$num_lease_start_year)
    # Snap the chosen storey to the midpoint of its 3-storey band, matching
    # how storey is recorded in the transaction data.
    storey_band_mid <-
      (floor((as.numeric(input$sld_storey_level) - 1) / 3) * 3) + 2
    
    # Create a data.frame (1 ROW) of input hdb data
    # NOTE: MUST be in same structure as the df used to train the model
    input_data <- data.frame(
      town = factor(curr_loc_row$town, levels = levels(df$town)), 
      flat_type = factor(input$sel_flat_type, levels = levels(df$flat_type)),
      flat_model = factor(input$sel_flat_model, levels = levels(df$flat_model)),
      floor_area_sqm = as.numeric(input$num_floor_area),
      storey_mid = storey_band_mid,
      remaining_lease_numeric = as.numeric(remaining_lease_years),
      distance_to_cbd = curr_loc_row$distance_to_cbd,
      distance_to_nearest_mrt = curr_loc_row$distance_to_nearest_mrt,
      lat = curr_loc_row$lat,
      long = curr_loc_row$long,
      resale_year = CURRENT_YEAR
    )
    
    # Smearing correction from the training residuals. exp() alone returns the
    # geometric mean, which sits below the mean price the user is asking about.
    predicted_price <- tryCatch(
      exp(predict(model_deployed, input_data)) *
        model_metadata$smearing_factor,
      error = function(e) NULL
    )

    validate(need(
      length(predicted_price) == 1 && is.finite(predicted_price),
      "No prediction available for this combination of flat details."
    ))

    predicted_price
  })
  
  
  # ---------- Render Outputs ---------- #
  output$txt_town <- renderText({
    curr_loc_row <- current_location_row()
    if (nrow(curr_loc_row) == 0) return("Address not found")
    as.character(curr_loc_row$town)
  })
    
  output$vtxt_flat_details <- renderText({
    curr_loc_row <- current_location_row()
    if (nrow(curr_loc_row) == 0) return("Please select a valid address.")
    paste(
      "Address:         ", input$sel_address,
      "\nTown:            ", curr_loc_row$town,
      "\nLease Start Year:", input$num_lease_start_year,
      "\nDistance to nearest MRT/LRT: ", round(curr_loc_row$distance_to_nearest_mrt, 2), "km",
      "\nDistance to CBD: ", round(curr_loc_row$distance_to_cbd, 2), "km"
    )
  })
  
  # Get predicted price from the handle Predict button press function: eventReactive
  output$txt_predicted_price <- renderText({
    req(predicted_resale_price())
    
    price <- predicted_resale_price()
    paste0("$", format(round(price, -3), big.mark = ",")) # round to nearest $1,000
  })
  
  # DT::datatable of Past Resale Tranactions with the same address
  output$dt_similar_sales <- DT::renderDataTable({
    req(input$btn_predict) # requires Predict button to have been pressed
    
    # NOTE: In Shiny, simply reading a reactive variable creates a dependency
    # render* functions auto run when reactive variables it reads update;
    # isolate here means: read the reactive variables inside e.g. input$sel_address
    # BUT DO NOT update the dt output every time input$sel_address updates
    # so only update when the Predict button is pressed
    isolate({
      req(input$sel_address)
      
      # Return a DT::datatable of past sales sorted by year
      df %>%
        # only sales involving the SAME (address AND flat_type AND flat_model)
        filter(
          address == input$sel_address,
          flat_type == input$sel_flat_type,
          flat_model == input$sel_flat_model
        ) %>%
        # Sort by most recent transactions
        arrange(desc(resale_year)) %>% 
        select(
          resale_year, 
          address, 
          flat_type, 
          floor_area_sqm, 
          storey_range, 
          resale_price) %>%
        DT::datatable(
          options = list(pageLength = 10, dom = 'tp'),
          colnames = c("Resale Year", 
                       "Address", 
                       "Type", 
                       "Floor Area (sqm)", 
                       "Storey Range", 
                       "Resale Price ($)")
        ) %>%
        formatCurrency("resale_price", currency = "$", digits = 0)
    })
  })
  
  # Plot of recent resale transactions with same address
  output$plot_similar_sales <- renderPlot({
    req(input$btn_predict) # requires Predict button to have been pressed
    
    # NOTE: In Shiny, simply reading a reactive variable creates a dependency
    # render* functions auto run when reactive variables it reads update;
    # isolate here means: read the reactive variables inside e.g. input$sel_address
    # BUT DO NOT update the dt output every time input$sel_address updates
    # so only update when the Predict button is pressed
    isolate({
      req(input$sel_address)
      
      plot_data <- df %>%
        filter(
          address == input$sel_address,
          flat_type == input$sel_flat_type,
          flat_model == input$sel_flat_model
        )
      
      validate(
        need(
          nrow(plot_data) > 0, 
          paste0("No past resale data found for a ", input$sel_flat_type, ", ", 
                 input$sel_flat_model, " flat at ", input$sel_address)
        )
      )
      
      # Calculate Mean Resale Price by Year for the line showing mean
      mean_data <- plot_data %>%
        group_by(resale_year) %>%
        summarise(mean_price = mean(resale_price, na.rm = TRUE))
      
      # Calculate good axis limits for the plot
      y_min <- floor(min(plot_data$resale_price) / 100000) * 100000
      y_max <- ceiling(max(plot_data$resale_price) / 100000) * 100000
      
      x_min <- min(plot_data$resale_year)
      x_max <- max(plot_data$resale_year)
      
      # Return the ggplot
      ggplot() +
        # Blue point for each resale transaction
        geom_point(data = plot_data, 
                   aes(x = resale_year, y = resale_price), 
                   color = "blue", 
                   alpha = 0.5, 
                   size = 3) +
        
        # Add a line to show mean resale price by year
        geom_line(data = mean_data, 
                  aes(x = resale_year, y = mean_price), 
                  color = "red", 
                  linewidth = 1.2) +
        
        # Labels
        labs(
          title = paste("Past Resale Transactions:", input$sel_flat_type, input$sel_flat_model, "at", input$sel_address),
          x = "Resale Year", 
          y = "Resale Price (SGD)"
        ) +
        
        scale_y_continuous(limits = c(y_min, y_max), labels = scales::comma) +
        scale_x_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 1)) + 
        
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 14),
          axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(size = 12)
        )
    }) # end of isolate
  
  }) # end of output$plot_similar_sales
  
  
} # ----- End of server ----- #

# ==============================================================================

# Run Shiny App
shinyApp(ui = ui, server = server)