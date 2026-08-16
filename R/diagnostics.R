# ==============================================================================
# FILE:    R/diagnostics.R
# AUTHOR:  Chan Jun Jie
# PURPOSE: Post-fit checks for the linear models: collinearity among the
#          predictors, and whether the residuals behave as the model assumes.
# ==============================================================================

library(tidyverse)


# Variance inflation factor for each numeric predictor, computed directly as
# 1 / (1 - R^2) from an auxiliary regression of that predictor on every other
# predictor, factors included.
#
# Restricted to numeric predictors deliberately. A multi-level factor needs the
# generalised VIF to be interpretable, and the collinearity of interest here is
# entirely numeric: lat/long against distance_to_cbd, and remaining lease
# against resale year.
#
# Conventional reading: above 5 is worth noting, above 10 is severe.
compute_vif <- function(model_data, response = "log_resale_price") {
  predictors <- setdiff(names(model_data), response)
  numeric_predictors <- predictors[
    map_lgl(model_data[predictors], is.numeric)
  ]

  map_dfr(numeric_predictors, function(predictor) {
    others <- setdiff(predictors, predictor)
    auxiliary <- lm(
      reformulate(others, response = predictor),
      data = model_data
    )
    r_squared <- summary(auxiliary)$r.squared

    tibble(
      predictor = predictor,
      aux_r_squared = round(r_squared, 4),
      vif = round(1 / (1 - r_squared), 2)
    )
  }) %>%
    arrange(desc(vif))
}


# Residual plots for a fitted linear model. Point-heavy panels are drawn from a
# sample; the by-year panel uses every row, because it is the one that matters
# most here — the models are validated on future years, and it shows whether
# error is stable over time. It also tests the homoskedasticity that the
# smearing correction in R/evaluate.R assumes.
save_residual_diagnostics <- function(fit, train_data, prefix,
                                      sample_size = 8000) {
  diagnostics <- tibble(
    fitted = as.numeric(fitted(fit)),
    residual = as.numeric(residuals(fit)),
    resale_year = train_data$resale_year
  )

  set.seed(123)
  sampled <- slice_sample(
    diagnostics,
    n = min(sample_size, nrow(diagnostics))
  )

  residuals_vs_fitted <- ggplot(sampled, aes(x = fitted, y = residual)) +
    geom_point(alpha = 0.15, color = "slateblue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "black") +
    theme_minimal() +
    labs(
      title = "Residuals vs Fitted",
      subtitle = glue::glue("{nrow(sampled)} sampled training rows"),
      x = "Fitted log(Resale Price)",
      y = "Residual"
    )

  qq_plot <- ggplot(sampled, aes(sample = residual)) +
    stat_qq(alpha = 0.2, color = "slateblue") +
    stat_qq_line(color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(
      title = "Normal Q-Q of Residuals",
      subtitle = glue::glue("{nrow(sampled)} sampled training rows"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )

  residuals_by_year <- ggplot(
    diagnostics,
    aes(x = factor(resale_year), y = residual)
  ) +
    geom_boxplot(fill = "slateblue", alpha = 0.4, outlier.alpha = 0.1) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(
      title = "Residuals by Transaction Year",
      subtitle = "Drift across years indicates the time trend is unmodelled",
      x = "Transaction Year",
      y = "Residual"
    )

  ggsave(
    glue::glue("output/figures/{prefix}_residuals_vs_fitted.png"),
    plot = residuals_vs_fitted, width = 8, height = 6
  )
  ggsave(
    glue::glue("output/figures/{prefix}_residuals_qq.png"),
    plot = qq_plot, width = 8, height = 6
  )
  ggsave(
    glue::glue("output/figures/{prefix}_residuals_by_year.png"),
    plot = residuals_by_year, width = 8, height = 6
  )

  diagnostics %>%
    group_by(resale_year) %>%
    summarise(
      n = n(),
      mean_residual = round(mean(residual), 4),
      sd_residual = round(sd(residual), 4),
      .groups = "drop"
    )
}


# Writes printed output to a text file so everything under output/summaries/ is
# a build product rather than a pasted console transcript.
save_text_summary <- function(..., path) {
  writeLines(capture.output(...), path)
  message(glue::glue("Wrote {path}"))
}
