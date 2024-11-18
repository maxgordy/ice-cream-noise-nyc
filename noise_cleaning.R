library(dplyr)
library(tidyverse)
library(janitor)
library(sf)

# Import and clean 311 noise complaints
noise_data <- read.csv("311_Noise_Complaints_20241111.csv") %>%
  mutate(
    created_date = as.POSIXct(trimws(as.character(Created.Date)), format = "%m/%d/%Y %I:%M:%S %p")
  ) %>%
  clean_names()

unique(noise_data$descriptor)

# Filter and select relevant columns for ice cream truck complaints
ice_cream_sf <- noise_data %>%
  filter(descriptor == "Noise, Ice Cream Truck (NR4)") %>%
  select(
    complaint_id = unique_key,
    created_date,
    complaint_type,
    descriptor,
    city,
    community_board,
    borough,
    latitude,
    longitude
  ) %>%
  filter(!is.na(longitude))  %>% # Remove rows without location data
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = TRUE)  # Convert to sf

# Load NTA map and set CRS to match ice_cream_sf
nta_map <- read_sf("https://data.cityofnewyork.us/resource/93vf-i5bz.geojson") %>%
  st_set_crs(st_crs(ice_cream_sf))

# Spatial join to assign NTAs to complaints
ice_cream_with_nta <- st_join(ice_cream_sf, nta_map, join = st_within) %>%
  filter(!is.na(ntacode))  # Remove rows without an NTA assignment

# Summarize complaints by NTA (aggregating the points into a single feature per NTA)
nta_complaints <- ice_cream_with_nta %>%
  group_by(ntacode) %>%
  summarize(
    total_complaints = n(),
    geometry = st_union(geometry)  # Aggregate points into a single feature per NTA
  )

head(nta_complaints)

# Load population data and join with complaints
population_data <- read.csv("https://data.cityofnewyork.us/resource/swpk-hqdp.csv") %>%
  filter(year == "2010") %>% #take only most recent population estimates
  rename(ntacode = nta_code)  # Align column names for joining 

# Join complaints data with population and calculate complaints per 1,000 people
nta_complaints <- nta_complaints %>%
  left_join(population_data, by = "ntacode") %>%
  mutate(
    complaints_per_1000 = ifelse(population > 0, (total_complaints / population) * 1000, 0)
  )

head(nta_complaints)

# Spatially join the summarized complaints with the NTA polygons
nta_complaints_polygons <- st_join(nta_map, nta_complaints, join = st_intersects) %>%
  mutate(
    total_complaints = replace_na(total_complaints, 0),
    complaints_per_1000 = replace_na(complaints_per_1000, 0)
  )

# Plot the updated choropleth map for complaints per 1,000 people
ggplot(data = nta_complaints_polygons) +
  geom_sf(aes(fill = complaints_per_1000), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_void() +
  labs(
    title = "Ice Cream Truck Noise Complaints",
    subtitle = "Per 1,000 People by NTA",
    fill = "Complaints/1,000"
  )


# Write to GeoJSON for QGIS
st_write(ice_cream_sf, "ice_cream_complaints_points.geojson", delete_dsn = TRUE)
st_write(nta_complaints_polygons, "nta_complaints_polygons.geojson", delete_dsn = TRUE)


### PARKS

#load in parks shapefile
greenspace_data <- read_sf("Parks Properties_20241115.geojson") %>%
  st_set_crs(st_crs(ice_cream_sf))

head(greenspace_data)

#select greenspace types with expected ice cream presence - parks and recreational facilities
unique(greenspace_data$typecategory)

park_greenspace <- greenspace_data %>% 
  filter(
    typecategory %in% c(
      "Neighborhood Park",
      "Playground",
      "Jointly Operated Playground",
      "Community Park",
      "Recreational Field/Courts",
      "Flagship Park"
    )
  ) %>%
  clean_names() 

#validate geoms
invalid_geometries <- park_greenspace %>% filter(!st_is_valid(geometry))
print(invalid_geometries)

park_greenspace <- park_greenspace %>% 
  mutate(geometry = st_make_valid(geometry))

#write updated parks data
st_write(park_greenspace, "park_space_edited.geojson", delete_dsn = TRUE)

#check CRS
st_crs(park_greenspace)
st_crs(ice_cream_with_nta)

#check geometry
ggplot() +
  geom_sf(data = park_greenspace, fill = "green", alpha = 0.5) +
  geom_sf(data = ice_cream_sf, color = "red", size = 1) +
  theme_minimal()

st_write(park_greenspace, "park_space.geojson", delete_dsn = TRUE)

#calculate distances from complaints to the nearest greenspace
ice_cream_with_nta <- ice_cream_with_nta %>%
  mutate(
    distance_to_greenspace = st_distance(geometry, st_geometry(park_greenspace)) %>% 
      apply(1, min) %>%  # find the minimum distance to any greenspace polygon
      as.numeric()
  )

#histogram of distances
ggplot(data = ice_cream_with_nta) +
  geom_histogram(aes(x = distance_to_greenspace), bins = 30, fill = "lightgreen", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Number of Ice Cream Truck Noise Complaints by Distance to the Nearest Park",
    x = "Distance to Nearest Park (meters)",
    y = "Count of Complaints"
  )

#summarize distance data by NTA
nta_with_distances <- ice_cream_with_nta %>%
  group_by(ntacode) %>%
  summarize(avg_distance_to_greenspace = mean(distance_to_greenspace, na.rm = TRUE),
            total_complaints = n())

#scatter plot of distances per NTA
ggplot(data = nta_with_distances) +
  geom_point(aes(x = avg_distance_to_greenspace, y = total_complaints), color = "blue") +
  theme_minimal() +
  labs(
    title = "Average Distance to Greenspace vs Total Complaints by NTA",
    x = "Average Distance to Greenspace (meters)",
    y = "Total Complaints"
  )