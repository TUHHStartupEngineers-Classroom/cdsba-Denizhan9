# Load necessary libraries
library(dplyr)

# Load data for the coupon campaign
df_coupon <- readRDS("coupon.rds")

# Display information about the data
str(df_coupon)

# Get the range of days_since_last
range_days <- range(df_coupon$days_since_last)

# Define cut-off
c0 <- 60

# [1] Check sensitivity to changing the bandwidth
# Specify different bandwidths based on the range of days_since_last
bandwidth_original <- c(range_days[1], range_days[2])
bandwidth_half <- c((c0 + range_days[1])/2, (c0 + range_days[2])/2)
bandwidth_double <- c((c0 + range_days[1])*2, (c0 + range_days[2])*2)

# Function to compute LATE for a given bandwidth
compute_LATE <- function(data, bw) {
  df_bw_below <- data %>% filter(days_since_last >= bw[1] & days_since_last < c0)
  df_bw_above <- data %>% filter(days_since_last >= c0 & days_since_last <= bw[2])
  
  print("Below bandwidth:")
  print(summary(df_bw_below$days_since_last))
  
  print("Above bandwidth:")
  print(summary(df_bw_above$days_since_last))
  
  if (nrow(df_bw_below) == 0 || nrow(df_bw_above) == 0) {
    stop("No data in one or more bandwidth segments. Please check the data and bandwidth values.")
  }
  
  model_bw_below <- lm(purchase_after ~ days_since_last, data = df_bw_below)
  model_bw_above <- lm(purchase_after ~ days_since_last, data = df_bw_above)
  
  y0 <- predict(model_bw_below, tibble(days_since_last = c0))
  y1 <- predict(model_bw_above, tibble(days_since_last = c0))
  
  late <- y1 - y0
  return(late)
}

# Compute LATE for different bandwidths
late_original <- compute_LATE(df_coupon, bandwidth_original)
late_half <- compute_LATE(df_coupon, bandwidth_half)
late_double <- compute_LATE(df_coupon, bandwidth_double)

# Print LATE for different bandwidths
cat("Original Bandwidth LATE: ", late_original, "\n")
cat("Half Bandwidth LATE: ", late_half, "\n")
cat("Double Bandwidth LATE: ", late_double, "\n")

