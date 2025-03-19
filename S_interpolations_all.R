# Load necessary libraries
library(zoo)
library(imputeTS)


# Save the cubic spline interpolated data to a CSV file (optional)
#write.csv(interpolated_df_cubic, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_NDVI_Planet_cubic_int.csv", row.names = FALSE)

rm(list = ls())

# Interpolation for Sentinel NDVI

# Load your dataset
D_NDVI_to_interpolate <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_NDVI_sentinel_lake.csv")

# Convert the date column to Date format
names(D_NDVI_to_interpolate)[names(D_NDVI_to_interpolate) == 'system.time_start'] <- 'date'

D_NDVI_to_interpolate$date <- as.Date(D_NDVI_to_interpolate$date, format = "%b %d, %Y")

# Calculate the average NDVI for each date
D_NDVI_to_interpolate <- D_NDVI_to_interpolate %>%
  group_by(date) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

# Create a sequence of all dates from the minimum to the maximum date
alldates <- seq.Date(from = min(D_NDVI_to_interpolate$date), to = max(D_NDVI_to_interpolate$date), by = 1)

# Create a data frame with all dates and NA values
NAframes <- data.frame(date = alldates)

# Function to calculate Mean Absolute Error (MAE)
calculate_mae <- function(true_values, predicted_values) {
  mean(abs(true_values - predicted_values), na.rm = TRUE)
}

# Initialize vectors to store the error for each interpolation method
mae_linear <- numeric(length(colnames(D_NDVI_to_interpolate)[-1]))
mae_cubic <- numeric(length(colnames(D_NDVI_to_interpolate)[-1]))

# Loop through each Watercontactsite column (except the date column)
for (i in seq_along(colnames(D_NDVI_to_interpolate)[-1])) {
  
  site <- colnames(D_NDVI_to_interpolate)[i + 1]
  
  # Extract data for the current site
  site_data <- data.frame(date = D_NDVI_to_interpolate$date, NDVI = D_NDVI_to_interpolate[[site]])
  
  # Remove 50% of the data randomly
  set.seed(123)  # For reproducibility
  missing_indices <- sample(1:nrow(site_data), size = floor(0.5 * nrow(site_data)), replace = FALSE)
  
  # Create a copy of site_data with 50% of the data removed
  site_data_missing <- site_data
  site_data_missing$NDVI[missing_indices] <- NA
  
  # Merge with NAframes to ensure all dates are present
  merged_data <- merge(NAframes, site_data_missing, by = "date", all.x = TRUE)
  
  # Perform linear interpolation
  interpolated_ts_linear <- na_interpolation(merged_data$NDVI, option = "linear")
  
  # Perform cubic spline interpolation
  interpolated_ts_cubic <- na.spline(merged_data$NDVI)
  
  # Compare the interpolated values with the original values
  mae_linear[i] <- calculate_mae(site_data$NDVI[missing_indices], interpolated_ts_linear[missing_indices])
  mae_cubic[i] <- calculate_mae(site_data$NDVI[missing_indices], interpolated_ts_cubic[missing_indices])
  
  # Optionally, print or store the results for each site
  cat("Site:", site, "\n")
  cat("MAE Linear:", mae_linear[i], "\n")
  cat("MAE Cubic:", mae_cubic[i], "\n\n")
}

# Summary of the results
results <- data.frame(
  Watercontactsite = colnames(D_NDVI_to_interpolate)[-1],
  MAE_Linear = mae_linear,
  MAE_Cubic = mae_cubic
)

print(results)

# Determine which method is better for each site
results$Better_Method <- ifelse(results$MAE_Linear < results$MAE_Cubic, "Linear", "Cubic Spline")

print(results)

# Initialize empty data frames to store the interpolated results
interpolated_df_linear <- data.frame(date = alldates)
interpolated_df_cubic <- data.frame(date = alldates)

# Loop through each Watercontactsite column (except the date column)
for (site in colnames(D_NDVI_to_interpolate)[-1]) {
  
  # Extract data for the current site
  site_data <- data.frame(date = D_NDVI_to_interpolate$date, NDVI = D_NDVI_to_interpolate[[site]])
  
  # Merge with NAframes to ensure all dates are present
  merged_data <- merge(NAframes, site_data, by = "date", all.x = TRUE)
  
  # Perform linear interpolation
  interpolated_ts_linear <- na_interpolation(merged_data$NDVI, option = "linear")
  
  # Perform cubic spline interpolation
  interpolated_ts_cubic <- na.spline(merged_data$NDVI)
  
  # Add the interpolated data to the respective data frames
  interpolated_df_linear[[site]] <- interpolated_ts_linear
  interpolated_df_cubic[[site]] <- interpolated_ts_cubic
}

# Save the linear interpolated data to a CSV file
write.csv(interpolated_df_linear, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_NDVI_Sentinel_linear_int.csv", row.names = FALSE)

# Save the cubic spline interpolated data to a CSV file (optional)
#write.csv(interpolated_df_cubic, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_NDVI_Planet_cubic_int.csv", row.names = FALSE)


rm(list = ls())

# Interpolation for Sentinel NDWI 

library(lubridate)

# Load your dataset
D_NDVI_to_interpolate <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_NDWI_sentinel_lake.csv")

# Convert the date column to Date format
names(D_NDVI_to_interpolate)[names(D_NDVI_to_interpolate) == 'system.time_start'] <- 'date'

D_NDVI_to_interpolate$date <- as.Date(D_NDVI_to_interpolate$date, format = "%b %d, %Y")

# Calculate the average NDVI for each date
D_NDVI_to_interpolate <- D_NDVI_to_interpolate %>%
  group_by(date) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

# Create a sequence of all dates from the minimum to the maximum date
alldates <- seq.Date(from = min(D_NDVI_to_interpolate$date), to = max(D_NDVI_to_interpolate$date), by = 1)

# Create a data frame with all dates and NA values
NAframes <- data.frame(date = alldates)

# Function to calculate Mean Absolute Error (MAE)
calculate_mae <- function(true_values, predicted_values) {
  mean(abs(true_values - predicted_values), na.rm = TRUE)
}

# Initialize vectors to store the error for each interpolation method
mae_linear <- numeric(length(colnames(D_NDVI_to_interpolate)[-1]))
mae_cubic <- numeric(length(colnames(D_NDVI_to_interpolate)[-1]))

# Loop through each Watercontactsite column (except the date column)
for (i in seq_along(colnames(D_NDVI_to_interpolate)[-1])) {
  
  site <- colnames(D_NDVI_to_interpolate)[i + 1]
  
  # Extract data for the current site
  site_data <- data.frame(date = D_NDVI_to_interpolate$date, NDVI = D_NDVI_to_interpolate[[site]])
  
  # Remove 50% of the data randomly
  set.seed(123)  # For reproducibility
  missing_indices <- sample(1:nrow(site_data), size = floor(0.5 * nrow(site_data)), replace = FALSE)
  
  # Create a copy of site_data with 50% of the data removed
  site_data_missing <- site_data
  site_data_missing$NDVI[missing_indices] <- NA
  
  # Merge with NAframes to ensure all dates are present
  merged_data <- merge(NAframes, site_data_missing, by = "date", all.x = TRUE)
  
  # Perform linear interpolation
  interpolated_ts_linear <- na_interpolation(merged_data$NDVI, option = "linear")
  
  # Perform cubic spline interpolation
  interpolated_ts_cubic <- na.spline(merged_data$NDVI)
  
  # Compare the interpolated values with the original values
  mae_linear[i] <- calculate_mae(site_data$NDVI[missing_indices], interpolated_ts_linear[missing_indices])
  mae_cubic[i] <- calculate_mae(site_data$NDVI[missing_indices], interpolated_ts_cubic[missing_indices])
  
  # Optionally, print or store the results for each site
  cat("Site:", site, "\n")
  cat("MAE Linear:", mae_linear[i], "\n")
  cat("MAE Cubic:", mae_cubic[i], "\n\n")
}

# Summary of the results
results <- data.frame(
  Watercontactsite = colnames(D_NDVI_to_interpolate)[-1],
  MAE_Linear = mae_linear,
  MAE_Cubic = mae_cubic
)

print(results)

# Determine which method is better for each site
results$Better_Method <- ifelse(results$MAE_Linear < results$MAE_Cubic, "Linear", "Cubic Spline")

print(results)

# Initialize empty data frames to store the interpolated results
interpolated_df_linear <- data.frame(date = alldates)
interpolated_df_cubic <- data.frame(date = alldates)

# Loop through each Watercontactsite column (except the date column)
for (site in colnames(D_NDVI_to_interpolate)[-1]) {
  
  # Extract data for the current site
  site_data <- data.frame(date = D_NDVI_to_interpolate$date, NDVI = D_NDVI_to_interpolate[[site]])
  
  # Merge with NAframes to ensure all dates are present
  merged_data <- merge(NAframes, site_data, by = "date", all.x = TRUE)
  
  # Perform linear interpolation
  interpolated_ts_linear <- na_interpolation(merged_data$NDVI, option = "linear")
  
  # Perform cubic spline interpolation
  interpolated_ts_cubic <- na.spline(merged_data$NDVI)
  
  # Add the interpolated data to the respective data frames
  interpolated_df_linear[[site]] <- interpolated_ts_linear
  interpolated_df_cubic[[site]] <- interpolated_ts_cubic
}

# Save the linear interpolated data to a CSV file
write.csv(interpolated_df_linear, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_NDWI_Sentinel_linear_int.csv", row.names = FALSE)

# Save the cubic spline interpolated data to a CSV file (optional)
#write.csv(interpolated_df_cubic, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_NDVI_Planet_cubic_int.csv", row.names = FALSE)


rm(list = ls())

# Interpolation for Sentinel NDWI 

library(lubridate)

# Load your dataset
D_NDVI_to_interpolate <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_Turbidity_sentinel_lake.csv")

# Convert the date column to Date format
names(D_NDVI_to_interpolate)[names(D_NDVI_to_interpolate) == 'system.time_start'] <- 'date'

D_NDVI_to_interpolate$date <- as.Date(D_NDVI_to_interpolate$date, format = "%b %d, %Y")

# Calculate the average NDVI for each date
D_NDVI_to_interpolate <- D_NDVI_to_interpolate %>%
  group_by(date) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

# Create a sequence of all dates from the minimum to the maximum date
alldates <- seq.Date(from = min(D_NDVI_to_interpolate$date), to = max(D_NDVI_to_interpolate$date), by = 1)

# Create a data frame with all dates and NA values
NAframes <- data.frame(date = alldates)

# Function to calculate Mean Absolute Error (MAE)
calculate_mae <- function(true_values, predicted_values) {
  mean(abs(true_values - predicted_values), na.rm = TRUE)
}

# Initialize vectors to store the error for each interpolation method
mae_linear <- numeric(length(colnames(D_NDVI_to_interpolate)[-1]))
mae_cubic <- numeric(length(colnames(D_NDVI_to_interpolate)[-1]))

# Loop through each Watercontactsite column (except the date column)
for (i in seq_along(colnames(D_NDVI_to_interpolate)[-1])) {
  
  site <- colnames(D_NDVI_to_interpolate)[i + 1]
  
  # Extract data for the current site
  site_data <- data.frame(date = D_NDVI_to_interpolate$date, NDVI = D_NDVI_to_interpolate[[site]])
  
  # Remove 50% of the data randomly
  set.seed(123)  # For reproducibility
  missing_indices <- sample(1:nrow(site_data), size = floor(0.5 * nrow(site_data)), replace = FALSE)
  
  # Create a copy of site_data with 50% of the data removed
  site_data_missing <- site_data
  site_data_missing$NDVI[missing_indices] <- NA
  
  # Merge with NAframes to ensure all dates are present
  merged_data <- merge(NAframes, site_data_missing, by = "date", all.x = TRUE)
  
  # Perform linear interpolation
  interpolated_ts_linear <- na_interpolation(merged_data$NDVI, option = "linear")
  
  # Perform cubic spline interpolation
  interpolated_ts_cubic <- na.spline(merged_data$NDVI)
  
  # Compare the interpolated values with the original values
  mae_linear[i] <- calculate_mae(site_data$NDVI[missing_indices], interpolated_ts_linear[missing_indices])
  mae_cubic[i] <- calculate_mae(site_data$NDVI[missing_indices], interpolated_ts_cubic[missing_indices])
  
  # Optionally, print or store the results for each site
  cat("Site:", site, "\n")
  cat("MAE Linear:", mae_linear[i], "\n")
  cat("MAE Cubic:", mae_cubic[i], "\n\n")
}

# Summary of the results
results <- data.frame(
  Watercontactsite = colnames(D_NDVI_to_interpolate)[-1],
  MAE_Linear = mae_linear,
  MAE_Cubic = mae_cubic
)

print(results)

# Determine which method is better for each site
results$Better_Method <- ifelse(results$MAE_Linear < results$MAE_Cubic, "Linear", "Cubic Spline")

print(results)

# Initialize empty data frames to store the interpolated results
interpolated_df_linear <- data.frame(date = alldates)
interpolated_df_cubic <- data.frame(date = alldates)

# Loop through each Watercontactsite column (except the date column)
for (site in colnames(D_NDVI_to_interpolate)[-1]) {
  
  # Extract data for the current site
  site_data <- data.frame(date = D_NDVI_to_interpolate$date, NDVI = D_NDVI_to_interpolate[[site]])
  
  # Merge with NAframes to ensure all dates are present
  merged_data <- merge(NAframes, site_data, by = "date", all.x = TRUE)
  
  # Perform linear interpolation
  interpolated_ts_linear <- na_interpolation(merged_data$NDVI, option = "linear")
  
  # Perform cubic spline interpolation
  interpolated_ts_cubic <- na.spline(merged_data$NDVI)
  
  # Add the interpolated data to the respective data frames
  interpolated_df_linear[[site]] <- interpolated_ts_linear
  interpolated_df_cubic[[site]] <- interpolated_ts_cubic
}

# Save the linear interpolated data to a CSV file
write.csv(interpolated_df_linear, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_Turbidity_Sentinel_linear_int.csv", row.names = FALSE)

# Save the cubic spline interpolated data to a CSV file (optional)
#write.csv(interpolated_df_cubic, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_NDVI_Planet_cubic_int.csv", row.names = FALSE)

