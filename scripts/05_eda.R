# ==============================================================================
# SCRIPT:  05_eda.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-03
# PURPOSE: Exploratory analysis: check regression assumptions and visualise the
#          relationships between resale price and its candidate predictors.
#          Produces figures only and modifies no data.
# INPUTS:  1. data/processed/enriched_resale_prices.csv  (pre-exclusion)
#          2. data/processed/modelling_resale_prices.csv (post-exclusion)
# OUTPUTS: output/figures/*.png
# ==============================================================================

library(tidyverse)
library(ggcorrplot)

# Two datasets are read deliberately. `enriched` is the state of the data before
# 04_build_modelling_data.R applies its exclusion rules, and is used only by the
# health checks and by the charts that exist to justify those rules. Every other
# chart uses `modelling`, so each figure describes exactly the data the models
# were trained on.
enriched <- read_csv(
  "data/processed/enriched_resale_prices.csv",
  show_col_types = FALSE
)
modelling <- read_csv(
  "data/processed/modelling_resale_prices.csv",
  show_col_types = FALSE
)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Section 1: Data health verification (pre-exclusion)
# ------------------------------------------------------------------------------

print(colSums(is.na(enriched)))
message(glue::glue("Duplicate rows: {sum(duplicated(enriched))}"))
message(glue::glue(
  "Rows excluded by 04_build_modelling_data.R: ",
  "{nrow(enriched) - nrow(modelling)}"
))
summary(enriched)


# ------------------------------------------------------------------------------
# Section 2: Univariate analysis
# ------------------------------------------------------------------------------

# Target: raw resale price.
# Heavily right-skewed with a long tail, motivating a log transformation.
mean_resale_price_000s <- mean(modelling$resale_price / 1000)

hist_resale_price <- ggplot(modelling, aes(x = resale_price / 1000)) +
  geom_histogram(
    fill = "slateblue", color = "darkblue",
    alpha = 0.9, binwidth = 100, boundary = 0
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 2000, by = 200)) +
  geom_vline(
    xintercept = mean_resale_price_000s,
    color = "black", linetype = "dashed", linewidth = 0.8
  ) +
  annotate(
    "text",
    x = mean_resale_price_000s + 30,
    y = 47000,
    label = paste0("Mean: $", round(mean_resale_price_000s, 0), "k"),
    color = "black",
    hjust = 0
  ) +
  theme_minimal() +
  labs(
    title = "Histogram of HDB Resale Prices ($'000s)",
    x = "Resale Price ($'000s)",
    y = "Count"
  )

ggsave(
  "output/figures/hist_resale_price.png",
  plot = hist_resale_price, width = 8, height = 6
)


# Target: log resale price.
# Much closer to normal, satisfying the linear model's assumptions.
mean_log_resale_price <- mean(log(modelling$resale_price))

hist_log_resale_price <- ggplot(modelling, aes(x = log(resale_price))) +
  geom_histogram(fill = "slateblue", color = "white", bins = 50) +
  scale_y_continuous(
    labels = scales::comma,
    breaks = seq(0, 14000, by = 2000),
    limits = c(0, 14000)
  ) +
  geom_vline(
    xintercept = mean_log_resale_price,
    linetype = "dashed", color = "black", linewidth = 1
  ) +
  annotate(
    "text",
    x = mean_log_resale_price + 0.25,
    y = Inf,
    label = paste0(
      "Mean(log price) = ", round(mean_log_resale_price, 2),
      "\nGeometric mean = $",
      scales::comma(round(exp(mean_log_resale_price), 0))
    ),
    vjust = 1.5, hjust = 0, size = 3.5
  ) +
  theme_minimal() +
  labs(
    title = "Histogram of Log-Transformed HDB Resale Prices",
    x = "log(Resale Price)",
    y = "Count",
    caption = "log(resale price) closely resembles a normal distribution"
  )

ggsave(
  "output/figures/hist_log_resale_price.png",
  plot = hist_log_resale_price, width = 8, height = 6
)


# Floor area, pre-exclusion. This is the chart that motivates the 200 sqm rule:
# the units above it are HDB terrace houses and outsized maisonettes, a
# different product from the flats being priced.
excluded_large <- enriched %>%
  distinct() %>%
  filter(floor_area_sqm >= 200)

print(count(excluded_large, flat_type, flat_model, sort = TRUE))

hist_floor_area_raw <- ggplot(enriched, aes(x = floor_area_sqm)) +
  geom_histogram(
    fill = "seagreen", color = "white", binwidth = 5, boundary = 0
  ) +
  scale_x_continuous(breaks = seq(0, 400, by = 50)) +
  theme_minimal() +
  labs(
    title = "Distribution of Floor Area, before exclusions",
    subtitle = glue::glue(
      "{nrow(excluded_large)} units at or above 200 sqm are terrace houses ",
      "and outsized maisonettes"
    ),
    x = "Floor Area (sqm)",
    y = "Count"
  )

ggsave(
  "output/figures/hist_floor_area_raw.png",
  plot = hist_floor_area_raw, width = 8, height = 6
)


# Floor area, post-exclusion. Distinct peaks at the standard flat sizes.
hist_floor_area_clean <- ggplot(modelling, aes(x = floor_area_sqm)) +
  geom_histogram(
    fill = "seagreen", color = "white", binwidth = 5, boundary = 0
  ) +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  theme_minimal() +
  labs(
    title = "Distribution of Floor Area (Cleaned)",
    subtitle = paste(
      "Distinct peaks observed at standard sizes",
      "(3-Room, 4-Room, 5-Room)"
    ),
    x = "Floor Area (sqm)",
    y = "Count",
    caption = glue::glue(
      "Note: {nrow(excluded_large)} rows (>= 200 sqm) excluded"
    )
  )

ggsave(
  "output/figures/hist_floor_area_clean.png",
  plot = hist_floor_area_clean, width = 8, height = 6
)


# Remaining lease.
hist_remaining_lease <- ggplot(modelling, aes(x = remaining_lease_numeric)) +
  geom_histogram(fill = "orange", color = "white", binwidth = 2, boundary = 0) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_minimal() +
  labs(
    title = "Distribution of Remaining Lease",
    x = "Years Left",
    y = "Count"
  )

ggsave(
  "output/figures/hist_lease.png",
  plot = hist_remaining_lease, width = 8, height = 6
)


# Storey. Very high floors are trimmed from the view only, not from the data.
n_high_storey <- sum(modelling$storey_mid > 37)

hist_storey_mid <- ggplot(
  modelling, aes(x = storey_mid)
) +
  geom_histogram(fill = "purple", color = "white", binwidth = 3, boundary = 1) +
  scale_x_continuous(breaks = seq(1, 37, by = 3), limits = c(1, 37)) +
  scale_y_continuous(breaks = seq(0, 90000, by = 10000), limits = c(0, 90000)) +
  theme_minimal() +
  labs(
    title = "Distribution of Storey Levels",
    x = "Storey (Band Midpoint)",
    y = "Count",
    caption = glue::glue(
      "Note: {n_high_storey} units above storey 37 omitted from the view ",
      "for readability."
    )
  ) +
  theme(plot.caption = element_text(
    hjust = 0, color = "darkgrey", face = "italic"
  ))

ggsave(
  "output/figures/hist_storey.png",
  plot = hist_storey_mid, width = 8, height = 6
)


# Distance to CBD.
hist_distance_to_cbd <- ggplot(modelling, aes(x = distance_to_cbd)) +
  geom_histogram(
    fill = "firebrick", color = "white", binwidth = 0.5, boundary = 0
  ) +
  scale_x_continuous(breaks = seq(0, 30, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20000, by = 5000)) +
  theme_minimal() +
  labs(
    title = "Distribution of Distance to CBD",
    x = "Distance (km)",
    y = "Count"
  )

ggsave(
  "output/figures/hist_distance_to_cbd.png",
  plot = hist_distance_to_cbd, width = 8, height = 6
)


# Distance to nearest rail station, as at the transaction date.
# The far-out cases are a genuine remote location rather than data errors, so
# they are kept.
remote_flats <- modelling %>% filter(distance_to_nearest_mrt > 3)

message(glue::glue(
  "Transactions over 3km from rail: {nrow(remote_flats)} ",
  "({n_distinct(remote_flats$address)} addresses on ",
  "{paste(unique(remote_flats$street_name), collapse = ', ')})"
))

hist_distance_to_nearest_mrt <- ggplot(
  modelling, aes(x = distance_to_nearest_mrt)
) +
  geom_histogram(
    fill = "dodgerblue", color = "white", binwidth = 0.1, boundary = 0
  ) +
  scale_x_continuous(breaks = seq(0, 4, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 30000, by = 5000)) +
  theme_minimal() +
  labs(
    title = "Distribution of Distance to nearest MRT/LRT",
    x = "Distance (km)",
    y = "Count"
  )

ggsave(
  "output/figures/hist_distance_to_nearest_mrt.png",
  plot = hist_distance_to_nearest_mrt, width = 8, height = 6
)


# Categorical predictors, checked for sparse classes.
bar_flat_type <- ggplot(modelling, aes(x = fct_infreq(flat_type))) +
  geom_bar(fill = "steelblue", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 150000, by = 20000),
    limits = c(0, 100000),
    labels = scales::comma
  ) +
  geom_text(
    stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3
  ) +
  theme_minimal() +
  labs(
    title = "Number of Resale transactions by Flat Type",
    x = "Flat Type",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "output/figures/bar_flat_type.png",
  plot = bar_flat_type, width = 8, height = 6
)

bar_town <- ggplot(modelling, aes(x = fct_infreq(town))) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Number of Resale transactions by Town",
    x = "Town",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("output/figures/bar_town.png", plot = bar_town, width = 12, height = 6)


# ------------------------------------------------------------------------------
# Section 3: Bivariate analysis
# ------------------------------------------------------------------------------

# Floor area vs price on both scales. The raw-price version shows the spread of
# price widening as area grows; the log version stabilises it.
#
# Binned rather than plotted point by point: 219,855 semi-transparent points
# saturate into a solid block that hides where the density actually sits. The
# fill is log-scaled because counts per bin span several orders of magnitude.
scatter_floor_area <- ggplot(
  modelling, aes(x = floor_area_sqm, y = resale_price / 1000)
) +
  geom_bin2d(bins = 60) +
  scale_fill_gradient(
    low = "#e8f3ed", high = "seagreen",
    transform = "log10", name = "Transactions"
  ) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 200)) +
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  theme_minimal() +
  labs(
    title = "Resale Price ($'000) vs. Floor Area (sqm)",
    x = "Floor Area (sqm)",
    y = "Resale Price ($'000)"
  )

ggsave(
  "output/figures/scatter_area.png",
  plot = scatter_floor_area, width = 8, height = 6
)

scatter_log_price_vs_floor_area <- ggplot(
  modelling, aes(x = floor_area_sqm, y = log(resale_price))
) +
  geom_bin2d(bins = 60) +
  scale_fill_gradient(
    low = "#e8f3ed", high = "seagreen",
    transform = "log10", name = "Transactions"
  ) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 200)) +
  scale_y_continuous(breaks = seq(11, 15, by = 1), limits = c(11, 15)) +
  theme_minimal() +
  labs(
    title = "log(Resale Price) vs. Floor Area (sqm)",
    x = "Floor Area (sqm)",
    y = "log(Resale Price)"
  )

ggsave(
  "output/figures/scatter_log_price_vs_floor_area.png",
  plot = scatter_log_price_vs_floor_area, width = 8, height = 6
)


# Remaining lease vs price, same comparison.
scatter_remaining_lease <- ggplot(
  modelling, aes(x = remaining_lease_numeric, y = resale_price / 1000)
) +
  geom_bin2d(bins = 60) +
  scale_fill_gradient(
    low = "#fff4e0", high = "darkorange",
    transform = "log10", name = "Transactions"
  ) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_x_continuous(breaks = seq(40, 100, by = 20), limits = c(40, 100)) +
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  theme_minimal() +
  labs(
    title = "Resale Price ($'000) vs. Remaining Lease (Years)",
    x = "Remaining Lease (Years)",
    y = "Resale Price ($'000)"
  )

ggsave(
  "output/figures/scatter_lease.png",
  plot = scatter_remaining_lease, width = 8, height = 6
)

scatter_log_price_vs_remaining_lease <- ggplot(
  modelling, aes(x = remaining_lease_numeric, y = log(resale_price))
) +
  geom_bin2d(bins = 60) +
  scale_fill_gradient(
    low = "#fff4e0", high = "darkorange",
    transform = "log10", name = "Transactions"
  ) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_x_continuous(breaks = seq(40, 100, by = 20), limits = c(40, 100)) +
  scale_y_continuous(breaks = seq(11, 15, by = 1), limits = c(11, 15)) +
  theme_minimal() +
  labs(
    title = "log(Resale Price) vs. Remaining Lease (Years)",
    x = "Remaining Lease (Years)",
    y = "log(Resale Price)"
  )

ggsave(
  "output/figures/scatter_log_price_vs_remaining_lease.png",
  plot = scatter_log_price_vs_remaining_lease, width = 8, height = 6
)


# Negative relationship between price and both distance measures.
scatter_distance_to_cbd <- ggplot(
  modelling, aes(x = distance_to_cbd, y = resale_price / 1000)
) +
  geom_bin2d(bins = 60) +
  scale_fill_gradient(
    low = "#f9e9e9", high = "firebrick",
    transform = "log10", name = "Transactions"
  ) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20)) +
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  theme_minimal() +
  labs(
    title = "Resale Price ($'000) vs. Distance to CBD (km)",
    x = "Distance to CBD (km)",
    y = "Resale Price ($'000)"
  )

ggsave(
  "output/figures/scatter_cbd.png",
  plot = scatter_distance_to_cbd, width = 8, height = 6
)

scatter_distance_to_nearest_mrt <- ggplot(
  modelling, aes(x = distance_to_nearest_mrt, y = resale_price / 1000)
) +
  geom_bin2d(bins = 60) +
  scale_fill_gradient(
    low = "#e6f2ff", high = "dodgerblue4",
    transform = "log10", name = "Transactions"
  ) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) +
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  theme_minimal() +
  labs(
    title = "Resale Price ($'000) vs. Distance to nearest MRT/LRT (km)",
    x = "Distance to MRT (km)",
    y = "Resale Price ($'000)",
    caption = glue::glue(
      "Note: {nrow(remote_flats)} transactions over 3km from rail ",
      "omitted from the view for readability."
    )
  ) +
  theme(plot.caption = element_text(
    hjust = 0, color = "darkgrey", face = "italic"
  ))

ggsave(
  "output/figures/scatter_distance_to_nearest_mrt.png",
  plot = scatter_distance_to_nearest_mrt, width = 8, height = 6
)


# storey_mid is discrete in steps of 3, so a boxplot per level reads better
# than a scatter.
box_storey_mid <- ggplot(
  modelling, aes(x = storey_mid, y = resale_price / 1000)
) +
  geom_boxplot(aes(group = storey_mid), fill = "purple", alpha = 0.3) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  scale_x_continuous(breaks = seq(1, 50, by = 3)) +
  theme_minimal() +
  labs(
    title = "Boxplot of Resale Price ($'000) vs. Storey",
    x = "Storey (Band Midpoint)",
    y = "Resale Price ($'000)"
  )

ggsave(
  "output/figures/box_storey_mid.png",
  plot = box_storey_mid, width = 8, height = 6
)


# Categories ordered by median price rather than alphabetically.
box_flat_type <- ggplot(
  modelling,
  aes(
    x = reorder(flat_type, resale_price, FUN = median),
    y = resale_price / 1000
  )
) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  theme_minimal() +
  labs(
    title = "Resale Price vs. Flat Type",
    x = "Flat Type",
    y = "Resale Price ($'000)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "output/figures/box_flat_type.png",
  plot = box_flat_type, width = 8, height = 6
)

box_town <- ggplot(
  modelling,
  aes(x = reorder(town, resale_price, FUN = median), y = resale_price / 1000)
) +
  geom_boxplot(fill = "seagreen", alpha = 0.6) +
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  theme_minimal() +
  labs(
    title = "Resale Price vs. Town",
    subtitle = "Sorted from lowest median price (Left) to highest (Right)",
    x = "Town",
    y = "Resale Price ($'000)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("output/figures/box_town.png", plot = box_town, width = 12, height = 6)


# Prices rise across the window, with a dip around the onset of COVID-19.
# This trend is the reason the models use a temporal rather than random split.
scatter_time <- ggplot(
  modelling, aes(x = resale_date, y = resale_price / 1000)
) +
  geom_bin2d(bins = 60) +
  scale_fill_gradient(
    low = "#ececf5", high = "slateblue4",
    transform = "log10", name = "Transactions"
  ) +
  geom_smooth(color = "red", linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    limits = as.Date(c("2017-01-01", "2025-12-31"))
  ) +
  theme_minimal() +
  labs(
    title = glue::glue(
      "Resale Price Over Time ",
      "({format(min(modelling$resale_date), '%b %Y')} - ",
      "{format(max(modelling$resale_date), '%b %Y')})"
    ),
    x = "Date",
    y = "Resale Price ($'000)"
  )

ggsave(
  "output/figures/scatter_time.png",
  plot = scatter_time, width = 8, height = 6
)


# ------------------------------------------------------------------------------
# Section 4: Multivariate analysis
# ------------------------------------------------------------------------------

# Pairwise correlation between the numeric predictors. Note this cannot reveal
# the near-dependency between remaining_lease_numeric, resale_year and
# lease_commence_date, which needs a variance inflation factor to detect.
numeric_vars <- modelling %>%
  transmute(
    log_resale_price = log(resale_price),
    floor_area_sqm,
    storey_mid,
    remaining_lease_numeric,
    distance_to_cbd,
    distance_to_nearest_mrt
  )

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(round(cor_matrix, 2))

heatmap <- ggcorrplot(
  cor_matrix,
  method = "square",
  type = "lower",
  lab = TRUE,
  lab_size = 4,
  title = "Heatmap of Key Drivers of Resale Price",
  colors = c("blue", "white", "red"),
  tl.cex = 12,
  ggtheme = ggplot2::theme_minimal()
)

ggsave("output/figures/heatmap.png", plot = heatmap, width = 8, height = 6)

message("EDA figures written to output/figures/")
