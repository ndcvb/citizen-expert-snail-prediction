import pandas as pd
import utm

# Function to convert sexagesimal coordinates to UTM using Zone 36
def convert_to_utm(lat, lon):
    utm_coords = utm.from_latlon(lat, lon, force_zone_number=36)
    return utm_coords[0], utm_coords[1]

# Updated CSV path
csv_path = "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/Watercontact_sites_simple.csv"
output_csv_path = "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/Watercontact_sites_simple_UTM.csv"

# Read CSV file with pandas
df = pd.read_csv(csv_path)

# Add new columns for UTM coordinates using UTM Zone 36
df['UTM_Easting'], df['UTM_Northing'] = zip(*df.apply(lambda row: convert_to_utm(row['latitude'], row['longitude']), axis=1))

# Save the updated DataFrame with UTM coordinates to a new CSV file
df.to_csv(output_csv_path, index=False)

print("UTM conversion completed. Updated data with UTM coordinates saved to:", output_csv_path)
