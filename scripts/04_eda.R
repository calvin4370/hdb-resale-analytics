# ==============================================================================
# SCRIPT:  04_eda.R
# AUTHOR:  Chan Jun Jie
# DATE:    2025-12-03
# PURPOSE: Perform Exploratory Data Analysis (EDA) on enriched dataset to check 
#          regression assumptions and visualize key relationships
# INPUTS:  data/processed/enriched_resale_prices.csv
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(ggcorrplot)

df <- read_csv("data/processed/enriched_resale_prices.csv")

# ------------------------------------------------------------------------------
# Section 1: Data Health Verification
# ------------------------------------------------------------------------------

# Check for NA
print(colSums(is.na(df)))

# Check for duplicate rows
duplicates_count <- sum(duplicated(df))
print(paste("Duplicate Rows:", duplicates_count))

df <- df %>% distinct() # remove duplicate rows (307)

# Verify correct types and no impossible values
summary(df)


# ------------------------------------------------------------------------------
# Section 2: Univariate Analysis
# ------------------------------------------------------------------------------

# Verify Normality assumption for target variable of interest (resale_price)
# Histogram of Resale Price ($)
mean_resale_price_000s <- mean(df$resale_price / 1000)
hist_resale_price <- ggplot(data = df, aes(x = resale_price / 1000)) +
  geom_histogram(fill = "slateblue", color = "darkblue", alpha = 0.9, binwidth = 100, boundary = 0) +
  
  # Scale the x and y axis labels
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 2000, by = 200)) +
  
  # Add vertical line for the mean and label it
  geom_vline(aes(xintercept = mean_resale_price_000s), 
             color = "black", linetype = "dashed", linewidth = 0.8) +
  annotate("text", 
           x = mean_resale_price_000s + 30,   # Shift x slightly right of the line
           y = 47000,             # Set y height of label
           label = paste0("Mean: $", round(mean_resale_price_000s, 0), "k"), 
           color = "black", 
           hjust = 0) +           # left-align text

  theme_minimal() +
  labs(
    title = "Histogram of HDB Resale Prices ($'000s)",
    x = "Resale Price ($'000s)",
    y = "Count"
  )

print(hist_resale_price)
ggsave("output/figures/hist_resale_price.png", plot = hist_resale_price, width = 8, height = 6)


# Check numerical independent variables for outliers and multimodality ---------
# Histograms and Boxplots of numerical independent variables -------------------
# 1. Floor Area (sqm) ----------------------------------------------------------
hist_floor_area <- ggplot(data = df, aes(x = floor_area_sqm)) +
  geom_histogram(fill = "seagreen", color = "white", binwidth = 5, boundary = 0) +
  scale_x_continuous(breaks = seq(0, 400, by = 50)) +
  theme_minimal() +
  labs(title = "Distribution of Floor Area (sqm)", x = "Floor Area (sqm)", y = "Count")

boxplot_floor_area <- ggplot(data = df, aes(x = floor_area_sqm)) +
  geom_boxplot(fill = "seagreen", color = "black", alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 400, by = 50)) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + # remove y axis labels
  labs(title = "Boxplot of Floor Area", x = "Floor Area (sqm)")

print(hist_floor_area)
print(boxplot_floor_area)

# Outliers > 300
outlier_floor_area <- df %>% 
  filter(floor_area_sqm > 300) %>% 
  select(month, town, flat_type, floor_area_sqm, resale_price, address)
print(outlier_floor_area)

# Check the flats between 200 and 300 sqm
large_flats <- df %>% 
  filter(floor_area_sqm > 200 & floor_area_sqm < 300) %>% 
  select(town, flat_type, floor_area_sqm, resale_price, street_name) %>%
  arrange(desc(floor_area_sqm))
print(large_flats)

# remove all outliers with floor_area_sqm >= 200, (HDB Terrace Houses)
df <- df %>% filter(floor_area_sqm < 200) 

# Replot histogram and save
hist_floor_area_clean <- ggplot(data = df, aes(x = floor_area_sqm)) +
  geom_histogram(fill = "seagreen", color = "white", binwidth = 5, boundary = 0) +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) + 
  theme_minimal() +
  labs(
    title = "Distribution of Floor Area (Cleaned)", 
    subtitle = "Distinct peaks observed at standard sizes (3-Room, 4-Room, 5-Room)",
    x = "Floor Area (sqm)", 
    y = "Count",
    caption = "Note: 11 outlier rows (> 200 sqm) removed"
  )

print(hist_floor_area_clean)
ggsave("output/figures/hist_floor_area_clean.png", plot = hist_floor_area_clean, width = 8, height = 6)


# 2. Remaining Lease Numeric (Years) -------------------------------------------
hist_remaining_lease <- ggplot(data = df, aes(x = remaining_lease_numeric)) +
  geom_histogram(fill = "orange", color = "white", binwidth = 2, boundary = 0) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_minimal() +
  labs(title = "Distribution of Remaining Lease", x = "Years Left", y = "Count")

boxplot_remaining_lease <- ggplot(data = df, aes(x = remaining_lease_numeric)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.6) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + # remove unnecessary y-axis labels
  labs(title = "Boxplot of Remaining Lease", x = "Years Left")

print(hist_remaining_lease)
print(boxplot_remaining_lease)
ggsave("output/figures/hist_lease.png", plot = hist_remaining_lease, width = 8, height = 6)


# 3. storey_range_floored ------------------------------------------------------
hist_storey_range_floored <- ggplot(data = df, aes(x = storey_range_floored)) +
  geom_histogram(fill = "purple", color = "white", binwidth = 3, boundary = 1) +
  scale_x_continuous(breaks = seq(1, 37, by = 3), limits = c(1, 37)) + 
  scale_y_continuous(breaks = seq(0, 90000, by = 10000), limits = c(0, 90000)) +
  theme_minimal() +
  labs(
    title = "Distribution of Storey Levels",
    x = "Storey Range (Lower Bound)",
    y = "Count",
    caption = "Note: 366 ultra-high-rise units (>37 floors) were excluded from the visual for readability."
  ) +
  theme(plot.caption = element_text(hjust = 0, color = "darkgrey", face = "italic"))

print(hist_storey_range_floored)
ggsave("output/figures/hist_storey.png", plot = hist_storey_range_floored, width = 8, height = 6)


# 4. Distance to CBD (km) ------------------------------------------------------
hist_distance_to_cbd <- ggplot(data = df, aes(x = distance_to_cbd)) +
  geom_histogram(fill = "firebrick", color = "white", binwidth = 0.5, boundary = 0) +
  scale_x_continuous(breaks = seq(0, 30, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20000, by = 5000)) +
  theme_minimal() +
  labs(title = "Distribution of Distance to CBD", x = "Distance (km)", y = "Count")

boxplot_distance_to_cbd <- ggplot(data = df, aes(x = distance_to_cbd)) +
  geom_boxplot(fill = "firebrick", color = "black", alpha = 0.6) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + # remove unnecessary y-axis labels
  labs(title = "Boxplot of Distance to CBD", x = "Distance (km)")

print(hist_distance_to_cbd)
print(boxplot_distance_to_cbd)
ggsave("output/figures/hist_distance_to_cbd.png", plot = hist_distance_to_cbd, width = 8, height = 6)


# 5. Distance to nearest MRT/LRT (km) ------------------------------------------
hist_distance_to_nearest_mrt <- ggplot(data = df, aes(x = distance_to_nearest_mrt)) +
  geom_histogram(fill = "dodgerblue", color = "white", binwidth = 0.1, boundary = 0) +
  scale_x_continuous(breaks = seq(0, 4, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 30000, by = 5000)) +
  theme_minimal() +
  labs(title = "Distribution of Distance to nearest MRT/LRT", x = "Distance (km)", y = "Count")

boxplot_distance_to_nearest_mrt <- ggplot(data = df, aes(x = distance_to_nearest_mrt)) +
  geom_boxplot(fill = "dodgerblue", color = "black", alpha = 0.6) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + # remove unnecessary y-axis labels
  labs(title = "Boxplot of Distance to nearest MRT/LRT", x = "Distance (km)")

print(hist_distance_to_nearest_mrt)
print(boxplot_distance_to_nearest_mrt)

# Check outliers with distance_to_nearest_mrt > 3km
outlier_dist_to_mrt <- df %>% 
  filter(distance_to_nearest_mrt > 3) %>% 
  select(month, town, flat_type, floor_area_sqm, resale_price, address, distance_to_nearest_mrt)
print(outlier_dist_to_mrt, n = 100)
# Since all 46 of them are CHANGI VILLAGE RD hdbs just in a remote location and not data errors, I will keep them 

ggsave("output/figures/hist_distance_to_nearest_mrt.png", plot = hist_distance_to_nearest_mrt, width = 8, height = 6)


# Check categorical independent variables for sparse classes (too few observations)
# Bar plots to check for number of observations for each category --------------
# 1. Flat type -----------------------------------------------------------------
bar_flat_type <- ggplot(data = df, aes(x = fct_infreq(flat_type))) +
  geom_bar(fill = "steelblue", color = "black") +
  scale_y_continuous(breaks = seq(0, 150000, by = 20000), 
                     limits = c(0, 100000), 
                     labels = scales::comma) +
  
  # Add count labels above each bar
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3) +
  
  theme_minimal() +
  labs(
    title = "Number of Resale transactions by Flat Type",
    x = "Flat Type",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate labels

print(bar_flat_type)
ggsave("output/figures/bar_flat_type.png", plot = bar_flat_type, width = 8, height = 6)


# 2. Town ----------------------------------------------------------------------
bar_town <- ggplot(data = df, aes(x = fct_infreq(town))) + # Sort by count decreasing
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Number of Resale transactions by Town",
    x = "Town",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

print(bar_town)
ggsave("output/figures/bar_town.png", plot = bar_town, width = 12, height = 6)


# ------------------------------------------------------------------------------
# Section 3: Bivariate Analysis
# ------------------------------------------------------------------------------

# Scatter plots of Resale Price vs Numeric Variables ---------------------------
# 1. Scatter plot of Resale Price ($'000) vs. Floor Area -----------------------
scatter_floor_area <- ggplot(data = df, aes(x = floor_area_sqm, y = resale_price / 1000)) +
  geom_point(alpha = 0.1, color = "seagreen") + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + # draw line of best fit
  scale_x_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 200)) +
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  theme_minimal() +
  labs(
    title = "Resale Price ($'000) vs. Floor Area (sqm)",
    x = "Floor Area (sqm)",
    y = "Resale Price ($'000)"
  )

# Strong positive linear relationship between Floor Area and Resale price,
# suggests Floor Area is a key driver of Resale Price
# However, heteroskedasticity may be present based on the increasing spread of
# Resale Price ($'000) as Floor Area (sqm) increases
# May need to log transform floor area in the linear regression model
print(scatter_floor_area)
ggsave("output/figures/scatter_area.png", plot = scatter_floor_area, width = 8, height = 6)


# 2. Scatter plot of Resale Price ($'000) vs. Remaining Lease (Years) ----------
scatter_remaining_lease <- ggplot(data = df, aes(x = remaining_lease_numeric, y = resale_price / 1000)) +
  geom_point(alpha = 0.01, color = "orange") + 
  geom_smooth(method = "lm", color = "black", se = FALSE) + # draw best-fit line
  scale_x_continuous(breaks = seq(20, 100, by = 20), limits = c(20, 100)) +
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  theme_minimal() +
  labs(
    title = "Resale Price ($'000) vs. Remaining Lease (Years)",
    x = "Remaining Lease (Years)",
    y = "Resale Price ($'000)"
  )

# Positive correlation between Remaining Lease (years) and Resale Price ($'000)
# However, hetero skedasticity may be present
print(scatter_remaining_lease)
ggsave("output/figures/scatter_lease.png", plot = scatter_remaining_lease, width = 8, height = 6)


# NOTE: storey_range_floored is a DISCRETE numeric data type (ordinal), with values 1, 4, 7... etc.
# so better to plot boxplot

# 3. Scatter plot of Resale Price ($'000) vs. Distance to CBD (km) -------------
scatter_distance_to_cbd <- ggplot(data = df, aes(x = distance_to_cbd, y = resale_price / 1000)) +
  geom_point(alpha = 0.05, color = "firebrick") + 
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20)) +
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  theme_minimal() +
  labs(
    title = "Resale Price ($'000) vs. Distance to CBD (km)",
    x = "Distance to CBD (km)",
    y = "Resale Price ($'000)"
  )

# Negative linear relationship between Distance to CBD and Resale Price
print(scatter_distance_to_cbd)
ggsave("output/figures/scatter_cbd.png", plot = scatter_distance_to_cbd, width = 8, height = 6)


# 4. Scatter plot of Resale Price ($'000) vs. Distance to nearest MRT/LRT (km) -
scatter_distance_to_nearest_mrt <- ggplot(data = df, aes(x = distance_to_nearest_mrt, y = resale_price / 1000)) +
  geom_point(alpha = 0.05, color = "dodgerblue") + 
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) + # zoom in on the main cluster < 3km
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  theme_minimal() +
  labs(
    title = "Resale Price ($'000) vs. Distance to nearest MRT/LRT (km)",
    x = "Distance to MRT (km)",
    y = "Resale Price ($'000)",
    caption = "Note: 46 HDBS (at CHANGI VILLAGE RD) with distance_from_nearest_mrt > 3km were excluded from the scatterplot for readability."
  ) +
  theme(plot.caption = element_text(hjust = 0, color = "darkgrey", face = "italic"))

# Negative linear relationship between Distance to nearest MRT/LRT and Resale Price
# Possible heteroskedasticity present
print(scatter_distance_to_nearest_mrt)
ggsave("output/figures/scatter_distance_to_nearest_mrt.png", plot = scatter_distance_to_nearest_mrt, width = 8, height = 6)


# Boxplots of Resale Price vs Discrete/Categorical Variables -------------------
# 5. Boxplot of Resale Price ($'000) vs. Storey Range --------------------------
# Note: used a boxplot because storey_range_floored is discrete with steps of 3 
# We group by 'storey_range_floored' so ggplot draws a box for each level.
box_storey_range_floored <- ggplot(data = df, aes(x = storey_range_floored, y = resale_price / 1000)) +
  geom_boxplot(aes(group = storey_range_floored), fill = "purple", alpha = 0.3) +
  geom_smooth(method = "lm", color = "black", se = FALSE) + # line of best fit
  
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  scale_x_continuous(breaks = seq(1, 50, by = 3)) +
  theme_minimal() +
  labs(
    title = "Boxplot of Resale Price ($'000) vs. Storey Range (Lower Bound)",
    x = "Storey Floor (Lower Bound)",
    y = "Resale Price ($'000)"
  )

# Positive linear relationship between Storey Range and Resale Price
print(box_storey_range_floored)
ggsave("output/figures/box_storey_range_floored.png", plot = box_storey_range_floored, width = 8, height = 6)


# 6. Boxplot of Resale Price ($'000) vs. Flat Type -----------------------------
# Note: reorder(flat_type, resale_price, FUN = median) arranges the flat_type boxes in increasing MEDIAN resale_price; note that default FUN is mean
box_flat_type <- ggplot(data = df, aes(x = reorder(flat_type, resale_price), FUN = median), y = resale_price / 1000)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  theme_minimal() +
  labs(
    title = "Resale Price vs. Flat Type",
    x = "Flat Type",
    y = "Resale Price ($'000)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # angle the x-axis labels to fit them better

print(box_flat_type)
ggsave("output/figures/box_flat_type.png", plot = box_flat_type, width = 8, height = 6)


# 7. Boxplot of Resale Price ($'000) vs. Town ----------------------------------
# Note: reorder(town, resale_price, FUN = median) arranges the town boxes in increasing MEDIAN resale_price
box_town <- ggplot(data = df, aes(x = reorder(town, resale_price, FUN = median), y = resale_price / 1000)) +
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

print(box_town)
ggsave("output/figures/box_town.png", plot = box_town, width = 12, height = 6)


# Temporal Analysis ------------------------------------------------------------
# 8. Scatterplot of Resale Price vs Time ---------------------------------------
summary(df$resale_date)
scatter_time <- ggplot(data = df, aes(x = resale_date, y = resale_price / 1000)) +
  geom_point(alpha = 0.05, color = "darkgrey") + 
  
  geom_smooth(color = "blue", linewidth = 1) + # use default LOESS curve to see trend
  
  scale_y_continuous(breaks = seq(0, 1750, by = 250), limits = c(0, 1750)) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y", # format label as just the year "2017", "2018"
    limits = as.Date(c("2017-01-01", "2025-12-31"))
  ) +
  
  theme_minimal() +
  labs(
    title = "Resale Price Over Time (Jan 2017 - Dec 2025)",
    x = "Date",
    y = "Resale Price ($'000)"
  )

# There is a rising trend in Resale Prices over time from Jan 2017 to Dec 2025
# with a period of decreasing resale prices between 2019 and 2o20 (likely due
# to poor economic conditions during the COVID-19 pandemic)
print(scatter_time)
ggsave("output/figures/scatter_time.png", plot = scatter_time, width = 8, height = 6)


# ------------------------------------------------------------------------------
# Section 4: Multivariate Analysis
# ------------------------------------------------------------------------------

# Correlation Matrix between all numeric variables to detect multicollinearity
# Select all the relevant numeric variables
numeric_vars <- df %>%
  select(
    resale_price, 
    floor_area_sqm, 
    storey_range_floored,
    remaining_lease_numeric, 
    distance_to_cbd, 
    distance_to_nearest_mrt
  )

print(colnames(numeric_vars))

cor_matrix <- cor(numeric_vars, use = "complete.obs") # use = "complete.obs" just ignores NAs
print(round(cor_matrix, 2))


# Heatmap to better visualise the above results
# RED = positive linear relationship, BLUE = negative linear relationship
# WHITE = no linear relationship
heatmap <- ggcorrplot(cor_matrix,
                      method = "square",       # shape of visualisation elements
                      type = "lower",          # only show the lower triangle
                      lab = TRUE,              # show correlation coefficient labels
                      lab_size = 4,            
                      title = "Heatmap of Key Drivers of Resale Price",
                      colors = c("blue", "white", "red"), # neg, nothing, pos linear relationship
                      tl.cex = 12,
                      ggtheme = ggplot2::theme_minimal()
)

print(heatmap)
ggsave("output/figures/heatmap.png", plot = heatmap, width = 8, height = 6)

