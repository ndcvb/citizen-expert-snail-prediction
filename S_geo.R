# Libraries
library(sf)
library(raster)
library(lubridate)
library(ggplot2)
library(terra)

# Extract DEM

# Nothing of this will change for every Watercontactsite so let's calculate for the simple dataframe 

# But first run it with the UTM script for python S_utm_convert.py

simple_df <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/Watercontact_sites_simple_UTM.csv", sep = ",", header = TRUE) 

#coordinates_csv <- data.frame(lon = simple_df$UTM_Easting, lat = simple_df$UTM_Northing)

# Before using them, reproject polygons in UTM

polygons = readRDS("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/polygons.RDS")

#st_crs(polygons)

#polygons_latlon <- st_transform(polygons, crs = 4326)

# Elevation 

image_file <- "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_Copernicus_tiles_reproj_clip_upd.tif"

image_raster <- raster(image_file)  

# Function to compute Q90
q90 <- function(x, na.rm = TRUE) {
  quantile(x, probs = 0.90, na.rm = na.rm)
}

# Function to extract Q90 value for each polygon
extract_value <- function(raster, polygon_sp) {
  value <- tryCatch({
    raster::extract(raster, polygon_sp, fun = q90, na.rm = TRUE, df = TRUE)[, 2]
  }, error = function(e) {
    NA
  })
  return(value)
}


# Create an empty column 'elev' in the Excel file

simple_df$elev <- NA

# Loop through each polygon in polygons_latlon
for (j in 1:nrow(polygons)) {
  # Extract the current polygon
  polygon <- polygons[j, ]
  
  # Convert the current polygon to Spatial object
  polygon_sp <- as(polygon, "Spatial")
  
  # Extract the location from the polygon object
  location <- polygon$Watercontactsite
  
  # Extract Q90 values for the current polygon
  elev_value <- extract_value(image_raster, polygon_sp)
  
  # Find the corresponding row in simple_df and update the 'elev' column
  simple_df[simple_df$Watercontactsite == location, "elev"] <- elev_value
}

# Flow accumulation 

image_file <- "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_flow_acc_up.tif"

image_raster <- raster(image_file)  

# Create an empty column 'elev' in the Excel file

simple_df$flow_acc <- NA

# Loop through each polygon in polygons_latlon
for (j in 1:nrow(polygons)) {
  # Extract the current polygon
  polygon <- polygons[j, ]
  
  # Convert the current polygon to Spatial object
  polygon_sp <- as(polygon, "Spatial")
  
  # Extract the location from the polygon object
  location <- polygon$Watercontactsite
  
  # Extract Q90 values for the current polygon
  flow_value <- extract_value(image_raster, polygon_sp)
  
  # Find the corresponding row in simple_df and update the 'flow_acc' column
  simple_df[simple_df$Watercontactsite == location, "flow_acc"] <- flow_value
}

# TPI 

image_file <- "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_TPI_up.tif"

image_raster <- raster(image_file)  

# Create an empty column 'TPI' in the Excel file

simple_df$TPI <- NA

# Loop through each polygon in polygons_latlon
for (j in 1:nrow(polygons)) {
  # Extract the current polygon
  polygon <- polygons[j, ]
  
  # Convert the current polygon to Spatial object
  polygon_sp <- as(polygon, "Spatial")
  
  # Extract the location from the polygon object
  location <- polygon$Watercontactsite
  
  # Extract Q90 values for the current polygon
  TPI_value <- extract_value(image_raster, polygon_sp)
  
  # Find the corresponding row in simple_df and update the 'flow_acc' column
  simple_df[simple_df$Watercontactsite == location, "TPI"] <- TPI_value
}

# SS Copernicus

image_file <- "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_SS_up.tif"

image_raster <- raster(image_file)  

# Create an empty column 'TPI' in the Excel file

simple_df$SS <- NA

# Loop through each polygon in polygons_latlon
for (j in 1:nrow(polygons)) {
  # Extract the current polygon
  polygon <- polygons[j, ]
  
  # Convert the current polygon to Spatial object
  polygon_sp <- as(polygon, "Spatial")
  
  # Extract the location from the polygon object
  location <- polygon$Watercontactsite
  
  # Extract Q90 values for the current polygon
  SS_value <- extract_value(image_raster, polygon_sp)
  
  # Find the corresponding row in simple_df and update the 'flow_acc' column
  simple_df[simple_df$Watercontactsite == location, "SS"] <- SS_value
}

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_env.csv", sep = ",", header = TRUE) 

ex_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_env.csv", sep = ",", header = TRUE) 

rm(list = setdiff(ls(), c('cs_data','ex_data','simple_df')))

head(simple_df)

# Perform the merge
cs_data <- merge(cs_data, simple_df[, c("Watercontactsite", "elev", "flow_acc", "TPI", "SS")],
                 by = "Watercontactsite", 
                 all.x = TRUE)

ex_data <- merge(ex_data, simple_df[, c("Watercontactsite", "elev", "flow_acc", "TPI", "SS")],
                 by = "Watercontactsite", 
                 all.x = TRUE)

write.csv(ex_data, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_env_geo.csv", row.names = FALSE)
write.csv(cs_data, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_env_geo.csv", row.names = FALSE)

