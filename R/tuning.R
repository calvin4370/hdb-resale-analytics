# ==============================================================================
# FILE:    R/tuning.R
# AUTHOR:  Chan Jun Jie
# PURPOSE: Detect hyperparameter searches that stopped at the edge of their own
#          grid, where the real optimum lies outside the range searched.
# ==============================================================================

library(tidyverse)


# Reports, for every tuned numeric hyperparameter, whether the selected value
# sits at the lowest or highest point of the grid. Parameters held at a single
# value were fixed by choice rather than tuned, and are skipped.
#
# An edge hit is not automatically a fault: lambda selected at the bottom of
# its range legitimately means no regularisation helps. It does mean the search
# never bracketed the optimum, so the result is a decision rather than a
# finding, and should be widened or defended.
check_tune_grid <- function(model, model_name) {
  best <- model$bestTune

  checks <- map_dfr(names(best), function(parameter) {
    values <- sort(unique(model$results[[parameter]]))

    if (!is.numeric(values) || length(values) < 2) {
      return(NULL)
    }

    chosen <- best[[parameter]]
    at_edge <- case_when(
      isTRUE(all.equal(chosen, min(values))) ~ "lower",
      isTRUE(all.equal(chosen, max(values))) ~ "upper",
      .default = NA_character_
    )

    tibble(
      model = model_name,
      parameter = parameter,
      chosen = chosen,
      grid_min = min(values),
      grid_max = max(values),
      grid_size = length(values),
      at_edge = at_edge
    )
  })

  # Models with nothing tuned (plain lm, stepwise) yield no rows at all, and an
  # empty tibble carries no columns to filter on.
  if (nrow(checks) == 0) {
    return(tibble(
      model = character(),
      parameter = character(),
      chosen = numeric(),
      grid_min = numeric(),
      grid_max = numeric(),
      grid_size = integer(),
      at_edge = character()
    ))
  }

  truncated <- filter(checks, !is.na(at_edge))

  if (nrow(truncated) > 0) {
    warning(
      glue::glue(
        "{model_name}: tuning stopped at the grid edge for ",
        "{paste(truncated$parameter, collapse = ', ')}. ",
        "Widen the grid or justify the bound."
      ),
      call. = FALSE
    )
  }

  checks
}


# Runs the check across several models and writes the combined report.
report_tune_grids <- function(models, output_path) {
  report <- imap_dfr(models, ~ check_tune_grid(.x, .y))

  print(as.data.frame(report))
  write_csv(report, output_path)

  edges <- sum(!is.na(report$at_edge))
  message(glue::glue(
    "Tuning check: {nrow(report)} tuned parameters, {edges} at a grid edge"
  ))

  report
}
