
# Environmental variables calculation (Precipitation and Temperature)

# libraries 

library(dplyr)
library(lubridate)
library(ggplot2)

# Before create a simple csv with Watercontactsite and latitude and longitude (this is to use in the GEE scripts)

#cs_data = read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_filtered.csv", sep = ",", header = TRUE)

#simple = unique(cs_data %>% select('Watercontactsite'))

#original_points = read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/Watercontact_sites.csv", sep = ",", header = TRUE)

#simple_coord = left_join(simple, original_points, by = 'Watercontactsite')

#simple_coord = simple_coord %>% select('Watercontactsite','longitude','latitude')

#write.csv(simple_coord, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/Watercontact_sites_simple.csv", row.names = FALSE )

rm(list=ls())

# Precipitation 

# Read data files 

# Read the output of the GEE Precipitation script 

df_pp <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/CHIRPS_output_EE.csv", sep = ",", header = TRUE)

# Fix format for the GEE output
values <- df_pp[,2:(ncol(df_pp)-2)]
new_df <- data.frame(Watercontactsite = df_pp$Watercontactsite, values)

# Clean column names in new_df to match date format
names(new_df) <- gsub("^X", "", names(new_df))
names(new_df) <- gsub("_", "", names(new_df))
names(new_df) <- gsub("precipitation", "", names(new_df))

# CS data
cs_data = read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_filtered.csv", sep = ",", header = TRUE)

cs_data = cs_data %>% dplyr::select('Watercontactsite',latitude='X_Take.a.GPS.point_latitude',longitude='X_Take.a.GPS.point_longitude',date='today','ID',bio = 'What.is.the.number.of.Biomphalaria.specimens...example.shown.below.',time='Enter.the.ending.time')

# Expert data

ex_data = read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_filtered.csv", sep = ",", header = TRUE)

ex_data = ex_data %>% dplyr::select('Watercontactsite',Site.type, date='Date_J',bio = 'Biomphalaria_J',time='S.time_J')

# Ensure proper date formats
cs_data$date <- as.Date(cs_data$date)
ex_data$date = as.Date(ex_data$date)
new_df_dates <- as.Date(names(new_df)[-1], format = "%Y%m%d")

head(new_df_dates)

# Create a new column in cs_data for precipitation

cs_data$oneweek = cs_data$date - 7

cs_data$pp_oneweek <- NA

for (i in 1:nrow(cs_data)) {
  site <- cs_data$Watercontactsite[i]
  sta_date = cs_data$oneweek[i]
  end_date = cs_data$date[i]
  
  # Filter new_df for the matching Watercontactsite
  site_temps <- new_df %>% filter(Watercontactsite == site)
  
  period =   seq(sta_date,end_date, by='days')
  period = gsub("-", "", period)
  
  matching_period = colnames(site_temps) %in% period
  
  cs_data$pp_oneweek[i] <- rowSums(site_temps[1, matching_period], na.rm = TRUE)
  
}

summary(cs_data$pp_oneweek)

# Create a new column in ex_data for precipitation

ex_data$oneweek = ex_data$date - 7

ex_data$pp_oneweek <- NA

for (i in 1:nrow(ex_data)) {
  site <- ex_data$Watercontactsite[i]
  sta_date = ex_data$oneweek[i]
  end_date = ex_data$date[i]
  
  # Filter new_df for the matching Watercontactsite
  site_temps <- new_df %>% filter(Watercontactsite == site)
  
  period =   seq(sta_date,end_date, by='days')
  period = gsub("-", "", period)
  
  matching_period = colnames(site_temps) %in% period
  
  ex_data$pp_oneweek[i] <- rowSums(site_temps[1, matching_period], na.rm = TRUE)
  
}

summary(ex_data$pp_oneweek)

# Create density plot of precipitation
ggplot(cs_data, aes(x = pp_oneweek)) +
  geom_density(fill = "blue", color = "darkblue", alpha = 0.5) +
  labs(
    x = "Precipitation",
    y = "Density") + theme_bw()

# Temperature 

rm(list = setdiff(ls(), c('cs_data','ex_data')))

df_st <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/MODIS_output_EE.csv", sep = ",", header = TRUE)

# Fix format for the GEE output and convert to Celsius
values <- df_st[,2:(ncol(df_st)-2)]* 0.02 - 273.15
new_df <- data.frame(Watercontactsite = df_st$Watercontactsite, values)

# Clean column names in new_df to match date format
names(new_df) <- gsub("^X", "", names(new_df))
names(new_df) <- gsub("_", "", names(new_df))
names(new_df) <- gsub("LSTDay1km", "", names(new_df))

# Transpose data to start with the interpolation 

df_transposed <- as.data.frame(t(new_df))
colnames(df_transposed) <- df_transposed[1, ]
df_transposed <- df_transposed[-1, ]
  # Convert in numeric
df_transposed[] <- lapply(df_transposed, function(x) as.numeric(as.character(x)))
df_transposed$date <- rownames(df_transposed)
df_transposed <- df_transposed[, c("date", setdiff(names(df_transposed), "date"))]
rownames(df_transposed) <- NULL

# Interpolation 
library(imputeTS)
library(zoo)

# Convert the date column to Date format
df_transposed$date <- as.Date(as.character(df_transposed$date), format = "%Y%m%d")

summary(df_transposed)

# Create a sequence of all dates from the minimum to the maximum date
alldates <- seq.Date(from = min(df_transposed$date), to = max(df_transposed$date), by = 1)

# Create a data frame with all dates and NA values
NAframes <- data.frame(date = alldates)

# Function to calculate Mean Absolute Error (MAE)
calculate_mae <- function(true_values, predicted_values) {
  mean(abs(true_values - predicted_values), na.rm = TRUE)
}

# Initialize vectors to store the error for each interpolation method
mae_linear <- numeric(length(colnames(df_transposed)[-1]))
mae_cubic <- numeric(length(colnames(df_transposed)[-1]))

# Loop through each Watercontactsite column (except the date column)
for (i in seq_along(colnames(df_transposed)[-1])) {
  
  site <- colnames(df_transposed)[i + 1]
  
  # Extract data for the current site
  site_data <- data.frame(date = df_transposed$date, temp = df_transposed[[site]])
  
  # Remove 50% of the data randomly
  set.seed(123)  # For reproducibility
  missing_indices <- sample(1:nrow(site_data), size = floor(0.5 * nrow(site_data)), replace = FALSE)
  
  # Create a copy of site_data with 50% of the data removed
  site_data_missing <- site_data
  site_data_missing$temp[missing_indices] <- NA
  
  # Merge with NAframes to ensure all dates are present
  merged_data <- merge(NAframes, site_data_missing, by = "date", all.x = TRUE)
  
  # Perform linear interpolation
  interpolated_ts_linear <- na_interpolation(merged_data$temp, option = "linear")
  
  # Perform cubic spline interpolation
  interpolated_ts_cubic <- na.spline(merged_data$temp)
  
  # Compare the interpolated values with the original values
  mae_linear[i] <- calculate_mae(site_data$temp[missing_indices], interpolated_ts_linear[missing_indices])
  mae_cubic[i] <- calculate_mae(site_data$temp[missing_indices], interpolated_ts_cubic[missing_indices])
  
  # Optionally, print or store the results for each site
  cat("Site:", site, "\n")
  cat("MAE Linear:", mae_linear[i], "\n")
  cat("MAE Cubic:", mae_cubic[i], "\n\n")
}

# Summary of the results
results <- data.frame(
  Watercontactsite = colnames(df_transposed)[-1],
  MAE_Linear = mae_linear,
  MAE_Cubic = mae_cubic
)

print(results)

# Determine which method is better for each site
results$Better_Method <- ifelse(results$MAE_Linear < results$MAE_Cubic, "Linear", "Cubic Spline")

print(results)

# Proceed with linear interpolation 

# Initialize empty data frames to store the interpolated results
interpolated_df_linear <- data.frame(date = alldates)
interpolated_df_cubic <- data.frame(date = alldates)

# Loop through each Watercontactsite column (except the date column)
for (site in colnames(df_transposed)[-1]) {
  
  # Extract data for the current site
  site_data <- data.frame(date = df_transposed$date, temp = df_transposed[[site]])
  
  # Merge with NAframes to ensure all dates are present
  merged_data <- merge(NAframes, site_data, by = "date", all.x = TRUE)
  
  # Perform linear interpolation
  interpolated_ts_linear <- na_interpolation(merged_data$temp, option = "linear")
  
  # Perform cubic spline interpolation
  interpolated_ts_cubic <- na.spline(merged_data$temp)
  
  # Add the interpolated data to the respective data frames
  interpolated_df_linear[[site]] <- interpolated_ts_linear
  interpolated_df_cubic[[site]] <- interpolated_ts_cubic
}

# Save the linear interpolated data to a CSV file
#write.csv(interpolated_df_linear, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_temperature_int.csv", row.names = FALSE)

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

cs_data <- add_source_data(cs_data, interpolated_df_linear, "temp_modis")
ex_data <- add_source_data(ex_data, interpolated_df_linear, "temp_modis")

# Create density plot of temperature
ggplot(cs_data, aes(x = temp_modis)) +
  geom_density(fill = "red", color = "darkred", alpha = 0.5) +
  labs(
    x = "Temperature",
    y = "Density") + theme_bw()

rm(list = setdiff(ls(), c('cs_data','ex_data')))

cs_data <- cs_data %>% dplyr::select(-oneweek)
ex_data <- ex_data %>% dplyr::select(-oneweek)

write.csv(cs_data, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_pp_temp.csv", row.names = FALSE)
write.csv(ex_data, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_pp_temp.csv", row.names = FALSE)





