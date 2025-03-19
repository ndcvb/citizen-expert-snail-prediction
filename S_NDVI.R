
# NDVI 

# First create the convex hulls 
# Before doing this, please calculate the UTM coordinates for your sites (using the python script called S_utm_conv.py)
rm(list=ls())

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_pp_temp_utm.csv", sep = ",", header = TRUE) 
ex_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_pp_temp.csv", sep = ",", header = TRUE) 

# Create a spatial points data frame
points_sf <- cs_data %>%
  st_as_sf(coords = c("UTM_Easting", "UTM_Northing"), crs = 32636) # Use appropriate CRS

# Group by Watercontactsite and create convex hulls
polygons_sf <- points_sf %>%
  group_by(Watercontactsite) %>%
  summarize(geometry = st_combine(geometry)) %>%
  st_convex_hull()  # Create the convex hull

#saveRDS(polygons_sf,"~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/polygons.RDS")

# 56 polygons :D 

# If you want to visualize the polygons
plot(st_geometry(polygons_sf), col = 'lightblue', border = 'darkblue')
plot(st_geometry(points_sf), add = TRUE, col = 'red')

# Example (jump if not needed it)
# Select a specific Watercontactsite to plot (e.g., "Site1")
selected_site <- "kooga"  # Replace with the actual site you want to plot

# Filter the points for the selected site
points_selected <- points_sf %>%
  filter(Watercontactsite == selected_site)

# Create a convex hull for the selected site
polygon_selected <- points_selected %>%
  summarize(geometry = st_combine(geometry)) %>%
  st_convex_hull()

# Plot the convex hull polygon
plot(st_geometry(polygon_selected), col = 'lightblue', border = 'darkblue', main = selected_site)
plot(st_geometry(points_selected), add = TRUE, col = 'red', pch = 19)

rm(list = setdiff(ls(), c('cs_data','ex_data','polygons_sf')))

# Terrestrial NDVI - we look at q90 and median 

## Polygons already created, then they need to extract the ndvi values 
# Load NDVI raster directory 
ndvi_dir <- "/Volumes/T7 Shield/NDVI_mosaic_output"

# List all NDVI images
ndvi_files <- list.files(ndvi_dir, pattern = "\\.tif$", full.names = TRUE)

# Custom function to extract the 90th percentile [q90 for upland]
q90 <- function(x, na.rm = TRUE) {
  quantile(x, probs = 0.90, na.rm = na.rm)
}

# Function to extract Q90 NDVI value for each polygon
extract_ndvi <- function(ndvi_raster, polygon_sp) {
  # Extract Q90 values
  ndvi_value <- tryCatch({
    raster::extract(ndvi_raster, polygon_sp, fun = q90, na.rm = TRUE, df = TRUE)[, 2]
  }, error = function(e) {
    NA
  })
  return(ndvi_value)
}

# Initialize an empty data frame to store results
ndvi_q90_t <- data.frame()
# Loop through each NDVI image
for (ndvi_file in ndvi_files) {
  # Extract the date from the file name
  date_str <- sub("new_mosaic_by_date_(\\d{8})_NDVI\\.tif", "\\1", basename(ndvi_file))
  
  # Load the NDVI raster
  ndvi_raster <- raster(ndvi_file)
  
  # Loop through each polygon
  for (i in 1:nrow(polygons_sf)) {
    # Extract the current polygon
    polygon <- polygons_sf[i, ]
    
    # Convert the current polygon to Spatial object
    polygon_sp <- as(polygon, "Spatial")
    
    # Extract the location from the polygon object
    location <- polygon$Watercontactsite
    
    # Extract NDVI values for the current polygon
    ndvi_value <- extract_ndvi(ndvi_raster, polygon_sp)
    
    # Create a data frame for the current result
    ndvi_data <- data.frame(
      date = date_str,
      location = location,
      ndvi_value = ndvi_value
    )
    
    # Append the result to the all_ndvi_data data frame
    ndvi_q90_t <- rbind(ndvi_q90_t, ndvi_data)
  }
}
# Display combined NDVI data
print(ndvi_q90_t)


median_value <- function(x, na.rm = TRUE) {
  median(x, na.rm = na.rm) }
  
# Function to extract median NDVI value for each polygon
extract_ndvi_m <- function(ndvi_raster, polygon_sp) {
  # Extract mediam values
  ndvi_value <- tryCatch({
    raster::extract(ndvi_raster, polygon_sp, fun = median_value, na.rm = TRUE, df = TRUE)[, 2]
  }, error = function(e) {
    NA
  })
  return(ndvi_value)
}

# Initialize an empty data frame to store results
ndvi_med_t <- data.frame()
# Loop through each NDVI image
for (ndvi_file in ndvi_files) {
  # Extract the date from the file name
  date_str <- sub("new_mosaic_by_date_(\\d{8})_NDVI\\.tif", "\\1", basename(ndvi_file))
  
  # Load the NDVI raster
  ndvi_raster <- raster(ndvi_file)
  
  # Loop through each polygon
  for (i in 1:nrow(polygons_sf)) {
    # Extract the current polygon
    polygon <- polygons_sf[i, ]
    
    # Convert the current polygon to Spatial object
    polygon_sp <- as(polygon, "Spatial")
    
    # Extract the location from the polygon object
    location <- polygon$Watercontactsite
    
    # Extract NDVI values for the current polygon
    ndvi_value <- extract_ndvi_m(ndvi_raster, polygon_sp)
    
    # Create a data frame for the current result
    ndvi_data <- data.frame(
      date = date_str,
      location = location,
      ndvi_value = ndvi_value
    )
    
    # Append the result to the all_ndvi_data data frame
    ndvi_med_t <- rbind(ndvi_med_t, ndvi_data)
  }
}
# Display combined NDVI data
print(ndvi_med_t)

plot(ndvi_med_t$ndvi_value, ndvi_q90_t$ndvi_value)

ndvi_med_t_w = ndvi_med_t %>% pivot_wider(names_from = location,  values_from = ndvi_value)
ndvi_q90_t_w = ndvi_q90_t %>% pivot_wider(names_from = location,  values_from = ndvi_value)

write.csv(ndvi_med_t_w, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_NDVI_t_med.csv", row.names = FALSE )
write.csv(ndvi_q90_t_w, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_NDVI_t_q90.csv", row.names = FALSE )

rm(list = setdiff(ls(), c('cs_data','ex_data','ndvi_med_t_w','ndvi_q90_t_w')))

###### Interpolation 

# Convert the date column to Date format
ndvi_med_t_w$date <- as.Date(as.character(ndvi_med_t_w$date), format = "%Y%m%d")

# Create a sequence of all dates from the minimum to the maximum date
alldates <- seq.Date(from = min(ndvi_med_t_w$date), to = max(ndvi_med_t_w$date), by = 1)

# Create a data frame with all dates and NA values
NAframes <- data.frame(date = alldates)

# Function to calculate Mean Absolute Error (MAE)
calculate_mae <- function(true_values, predicted_values) {
  mean(abs(true_values - predicted_values), na.rm = TRUE)
}

# Initialize vectors to store the error for each interpolation method
mae_linear <- numeric(length(colnames(ndvi_med_t_w)[-1]))
mae_cubic <- numeric(length(colnames(ndvi_med_t_w)[-1]))

# Loop through each Watercontactsite column (except the date column)
for (i in seq_along(colnames(ndvi_med_t_w)[-1])) {
  
  site <- colnames(ndvi_med_t_w)[i + 1]
  
  # Extract data for the current site
  site_data <- data.frame(date = ndvi_med_t_w$date, NDVI = ndvi_med_t_w[[site]])
  
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
  Watercontactsite = colnames(ndvi_med_t_w)[-1],
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
for (site in colnames(ndvi_med_t_w)[-1]) {
  
  # Extract data for the current site
  site_data <- data.frame(date = ndvi_med_t_w$date, NDVI = ndvi_med_t_w[[site]])
  
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
write.csv(interpolated_df_linear, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_NDVI_med_t.csv", row.names = FALSE)

NDVI_med_t = interpolated_df_linear

rm(list = setdiff(ls(), c('cs_data','ex_data','NDVI_med_t','ndvi_q90_t_w')))

# Interpolation

# Convert the date column to Date format
ndvi_q90_t_w$date <- as.Date(as.character(ndvi_q90_t_w$date), format = "%Y%m%d")

# Create a sequence of all dates from the minimum to the maximum date
alldates <- seq.Date(from = min(ndvi_q90_t_w$date), to = max(ndvi_q90_t_w$date), by = 1)

# Create a data frame with all dates and NA values
NAframes <- data.frame(date = alldates)

# Function to calculate Mean Absolute Error (MAE)
calculate_mae <- function(true_values, predicted_values) {
  mean(abs(true_values - predicted_values), na.rm = TRUE)
}

# Initialize vectors to store the error for each interpolation method
mae_linear <- numeric(length(colnames(ndvi_q90_t_w)[-1]))
mae_cubic <- numeric(length(colnames(ndvi_q90_t_w)[-1]))

# Loop through each Watercontactsite column (except the date column)
for (i in seq_along(colnames(ndvi_q90_t_w)[-1])) {
  
  site <- colnames(ndvi_q90_t_w)[i + 1]
  
  # Extract data for the current site
  site_data <- data.frame(date = ndvi_q90_t_w$date, NDVI = ndvi_q90_t_w[[site]])
  
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
  Watercontactsite = colnames(ndvi_q90_t_w)[-1],
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
for (site in colnames(ndvi_q90_t_w)[-1]) {
  
  # Extract data for the current site
  site_data <- data.frame(date = ndvi_q90_t_w$date, NDVI = ndvi_q90_t_w[[site]])
  
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
write.csv(interpolated_df_linear, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_NDVI_q90_t.csv", row.names = FALSE)

NDVI_q90_t = interpolated_df_linear

rm(list = setdiff(ls(), c('cs_data','ex_data','NDVI_med_t','NDVI_q90_t')))

## Add terrestrial NDVI to the datasets 

# Function to add source data to df_excel
add_source_data <- function(main_df, source_df, source_prefix) {
  # Initialize the new column
  new_column <- paste0(source_prefix)
  main_df[[new_column]] <- NA
  
  # Iterate through each row of main_df
  for (i in seq_len(nrow(main_df))) {
    current_date <- main_df$date[i]
    current_site <- main_df$Watercontactsite[i]
    
    # Match the date in source_df
    matched_row <- source_df %>%
      filter(date == current_date)
    
    if (nrow(matched_row) > 0) {
      # Extract the column name from the current site
      col_name <- as.character(current_site)
      
      # Check if the column exists in the matched row
      if (col_name %in% colnames(matched_row)) {
        # Assign the value from the column
        main_df[[new_column]][i] <- matched_row[[col_name]]
      } else {
        # If the column doesn't exist, assign NA
        main_df[[new_column]][i] <- NA
      }
    }
  }
  
  return(main_df)
}

# Add columns for each source dataset
cs_data <- add_source_data(cs_data, NDVI_med_t, "NDVI_med_t")
cs_data <- add_source_data(cs_data, NDVI_q90_t, "NDVI_q90_t")
ex_data <- add_source_data(ex_data, NDVI_med_t, "NDVI_med_t")
ex_data <- add_source_data(ex_data, NDVI_q90_t, "NDVI_q90_t")

rm(list = setdiff(ls(), c('cs_data','ex_data')))

# Save the terrestrial NDVI interpolated data to a CSV file
write.csv(ex_data, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_pp_temp_Tndvi.csv", row.names = FALSE)
write.csv(cs_data, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_pp_temp_Tndvi.csv", row.names = FALSE)

# Aquatic NDVI 

# This is from Sentinel (GEE) - first run S_interpolations_all.R (to have NDVI, NDWI and Turbidity)

# Function to add source data to df_excel
add_source_data <- function(main_df, source_df, source_prefix) {
  # Initialize the new column
  new_column <- paste0(source_prefix)
  main_df[[new_column]] <- NA
  
  # Iterate through each row of main_df
  for (i in seq_len(nrow(main_df))) {
    current_date <- main_df$date[i]
    current_site <- main_df$Watercontactsite[i]
    
    # Match the date in source_df
    matched_row <- source_df %>%
      filter(date == current_date)
    
    if (nrow(matched_row) > 0) {
      # Extract the column name from the current site
      col_name <- as.character(current_site)
      
      # Check if the column exists in the matched row
      if (col_name %in% colnames(matched_row)) {
        # Assign the value from the column
        main_df[[new_column]][i] <- matched_row[[col_name]]
      } else {
        # If the column doesn't exist, assign NA
        main_df[[new_column]][i] <- NA
      }
    }
  }
  
  return(main_df)
}

# Read the files 

# This is q10
NDVI_sentinel = read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_NDVI_Sentinel_linear_int.csv", sep = ",", header = TRUE)

# This is q10
NDWI_sentinel = read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_NDWI_Sentinel_linear_int.csv", sep = ",", header = TRUE)

# This is q90
Turbidity_sentinel = read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_Turbidity_Sentinel_linear_int.csv", sep = ",", header = TRUE)

# Add columns for each source dataset
NDVI_sentinel$date <- as.Date(NDVI_sentinel$date)
NDWI_sentinel$date <- as.Date(NDWI_sentinel$date)
Turbidity_sentinel$date <- as.Date(Turbidity_sentinel$date)

# Add columns for each source dataset
cs_data <- add_source_data(cs_data, NDVI_sentinel, "NDVI_aqua")
cs_data <- add_source_data(cs_data, NDWI_sentinel, "NDWI_aqua")
cs_data <- add_source_data(cs_data, Turbidity_sentinel, "Turbidity_aqua")

ex_data <- add_source_data(ex_data, NDVI_sentinel, "NDVI_aqua")
ex_data <- add_source_data(ex_data, NDWI_sentinel, "NDWI_aqua")
ex_data <- add_source_data(ex_data, Turbidity_sentinel, "Turbidity_aqua")

# Save the terrestrial NDVI interpolated data to a CSV file
write.csv(ex_data, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_env.csv", row.names = FALSE)
write.csv(cs_data, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_env.csv", row.names = FALSE)

rm(list = setdiff(ls(), c('cs_data','ex_data')))




