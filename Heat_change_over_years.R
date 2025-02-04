rm(list = ls())

#install.packages("ecmwfr")
#install.packages("ncdf4")
#install.packages("raster")
#install.packages("rgdal")
#install.packages("RNetCDF")

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(ggplot2) # package for plotting
library(ecmwfr)
library(tidyverse)
library(lubridate)
library(abind)
library(dplyr)
library(reshape2)
library(sf)
library(dplyr)
library(raster)
library(data.table)
library(plm)
library(chillR)



setwd("C:/Users/saptashya.ghosh/Dropbox/agmarket_spillover/6. figures/heat change/")


###############################################################################

# Read the HDD latitude/longitude data
hdd_lat_lon <- read.csv("data/HDD_census.csv")

# Read the district shapefile as an sf object
district_shape <- st_read("C:/Users/saptashya.ghosh/Dropbox/CIBIL_Nirupama/1_raw/03_shapefiles/in_district.shp")

district_shape <- district_shape %>%
  arrange(stcode11, dtcode11) 

# Transform the district shapefile to WGS84 (assuming your lat/lon data is in WGS84)
district_shape <- st_transform(district_shape, crs = 4326)

# Convert stcode11 and dtcode11 to character
hdd_lat_lon <- hdd_lat_lon %>%
  mutate(
    stcode11 = as.character(stcode11),
    dtcode11 = as.character(dtcode11)
  )

# Remove leading zero from stcode11 in district_shape
district_shape$stcode11 <- sub("^0", "", district_shape$stcode11)

# For District
district_shape$dtcode11 <- sub("^0+", "", district_shape$dtcode11)

# Perform a non-spatial merge using the 'merge' function
#district_shape_hdd <- merge(district_shape, hdd_lat_lon, by = c("stcode11", "dtcode11"), all.x = TRUE)

# Perform a non-spatial merge on lonx and latx
district_shape_hdd <- district_shape %>%
  left_join(hdd_lat_lon, by = c("stcode11", "dtcode11"))

# Convert to data frame, remove duplicates
district_shape_hdd_df <- as.data.frame(district_shape_hdd)
distinct_df <- district_shape_hdd_df %>%
  distinct(dtcode11, year, .keep_all = TRUE)

# Convert back to sf
district_shape_hdd <- st_as_sf(distinct_df, sf_column_name = "geometry")

# create a duplicate data 
hdd_dist <- district_shape_hdd

# List of years to create heat maps for
years <- c(2012, 2013
, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

# Directory to save the plots
output_dir <- "C:/Users/saptashya.ghosh/Dropbox/agmarket_spillover/6. figures/heat change"

# Loop over each year, create the plot, and save it
for (year in years) {
  # Filter data for the specific year
  hdd_dist_year <- hdd_dist[hdd_dist$year == year, ]
  print(paste("Processing year:", year))
  
  # Create the plot
  hdd_plot <- ggplot() + 
    geom_sf(data = hdd_dist, aes(geometry = geometry), color = "black", fill = NA) +
    geom_sf(data = hdd_dist_year, aes(geometry = geometry, fill = HDD)) +
    scale_fill_viridis_c(name = "HDD") +
    labs(title = paste("Heat Days in", year)) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),   # White background
      plot.background = element_rect(fill = "white", color = NA),    # White outer background
      panel.grid = element_blank(),                                  # Remove grid lines
      axis.text = element_text(color = "black"),                     # Black axis text
      axis.title = element_text(color = "black"),                    # Black axis titles
      axis.ticks = element_line(color = "black"),                    # Black axis ticks
      plot.title = element_text(hjust = 0.5, size = 14), 
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  
  # Define file path to save the plot
  file_path <- file.path(output_dir, paste0("heat_days_ch", year, ".png"))
  
  # Save the plot as a PNG file
  ggsave(file_path, plot = hdd_plot, width = 8, height = 6)
}


##Calculate the lags for heat change
hdd_dist_lag <- hdd_dist %>%
  select(dtcode11, year, HDD) %>%   # Select only necessary columns
  mutate(year = year + 1) %>%           # Shift the year forward by 1
  rename(HDD_lag = HDD)             # Rename HDD_sum to HDD_lag

hdd_dist_lag$geometry <- NULL

# Step 2: Join the shifted dataset with the original to calculate heat change
hdd_dist <- hdd_dist %>%
  left_join(hdd_dist_lag, by = c("dtcode11", "year")) %>%  # Join on district and year
  mutate(
    heat_change = HDD - HDD_lag  # Calculate heat change
  )

# Find the overall range of heat_change across all years
heat_change_min <- min(hdd_dist$heat_change, na.rm = TRUE)
heat_change_max <- max(hdd_dist$heat_change, na.rm = TRUE)

# List of years to create heat maps for
years <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)


for (year in years) {
  # Filter data for the specific year
  hdd_dist_year <- hdd_dist[hdd_dist$year == year, ]
  print(paste("Processing year:", year))
  
  
  # Create the plot with fixed color scale
  hdd_plot <- ggplot() + 
    # Plot the outline of all Indian districts (in black) without filling
    geom_sf(data = hdd_dist, aes(geometry = geometry), color = "black", fill = NA) +
    # Plot only the regions with heat_change values for the given year
    geom_sf(data = hdd_dist_year, aes(geometry = geometry, fill = heat_change)) +
    # Define color scale for heat_change values
    scale_fill_viridis_c(name = "HDD", limits = c(heat_change_min, heat_change_max)) +
    labs(title = paste("Heat Change in", year)) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),   # White background
      plot.background = element_rect(fill = "white", color = NA),    # White outer background
      panel.grid = element_blank(),                                  # Remove grid lines
      axis.text = element_text(color = "black"),                     # Black axis text
      axis.title = element_text(color = "black"),                    # Black axis titles
      axis.ticks = element_line(color = "black"),                    # Black axis ticks
      plot.title = element_text(hjust = 0.5, size = 14), 
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  
  # Define file path to save the plot
  file_path <- file.path(output_dir, paste0("ch_heat_change_", year, ".png"))
  
  # Save the plot as a PNG file
  ggsave(file_path, plot = hdd_plot, width = 8, height = 6)
}


# Loop over each year, create the plot, and save it (for heat change)
for (year in years) {
  # Filter data for the specific year
  hdd_dist_year <- hdd_dist[hdd_dist$year == year, ]
  
  # Create the plot with fixed color scale
  hdd_plot <- ggplot() + 
    geom_sf(data = hdd_dist, aes(geometry = geometry), color = "black", fill = NA) +
    geom_sf(data = hdd_dist_year, aes(geometry = geometry, fill = heat_change)) +
    scale_fill_viridis_c(name = "HDD", limits = c(heat_change_min, heat_change_max)) +
    labs(title = paste("Heat Change in", year)) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),   # White background
      plot.background = element_rect(fill = "white", color = NA),    # White outer background
      panel.grid = element_blank(),                                  # Remove grid lines
      axis.text = element_text(color = "black"),                     # Black axis text
      axis.title = element_text(color = "black"),                    # Black axis titles
      axis.ticks = element_line(color = "black"),                    # Black axis ticks
      plot.title = element_text(hjust = 0.5, size = 14), 
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  
  # Define file path to save the plot
  file_path <- file.path(output_dir, paste0("heat_change_", year, ".png"))
  
  # Save the plot as a PNG file
  ggsave(file_path, plot = hdd_plot, width = 8, height = 6)
}




##Calculate the heat change based on 2013
# Calculate HDD_2013
hdd_dist <- hdd_dist %>%
  group_by(dtcode11) %>% # Group by district
  mutate(HDD_2013 = HDD - HDD[year == 2013]) %>% # Calculate HDD difference
  ungroup() # Remove grouping

# Create the 2013 HDD column and calculate HDD_2013 as the difference
hdd_dist <- hdd_dist %>%
  group_by(dtcode11) %>%
  mutate(HDD_2013_ref = HDD[year == 2013],       # Create reference column with 2013 HDD values
         HDD_2013 = HDD - HDD_2013_ref) %>%      # Subtract 2013 HDD from each year's HDD
  ungroup()  


# Find the overall range of heat_change across all years
heat_change_min <- min(hdd_dist$HDD_2013, na.rm = TRUE)
heat_change_max <- max(hdd_dist$HDD_2013, na.rm = TRUE)

# List of years to create heat maps for
years <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)

# Loop over each year, create the plot, and save it (for heat change)
for (year in years) {
  # Filter data for the specific year
  hdd_dist_year <- hdd_dist[hdd_dist$year == year, ]
  
  # Create the plot with fixed color scale
  hdd_plot <- ggplot() + 
    geom_sf(data = hdd_dist_year, aes(geometry = geometry, fill = HDD_2013)) +
    scale_fill_viridis_c(name = "HDD", limits = c(heat_change_min, heat_change_max)) +
    labs(title = paste("Heat Change in", year)) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),   # White background
      plot.background = element_rect(fill = "white", color = NA),    # White outer background
      panel.grid = element_blank(),                                  # Remove grid lines
      axis.text = element_text(color = "black"),                     # Black axis text
      axis.title = element_text(color = "black"),                    # Black axis titles
      axis.ticks = element_line(color = "black"),                    # Black axis ticks
      plot.title = element_text(hjust = 0.5, size = 14), 
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  
  # Define file path to save the plot
  file_path <- file.path(output_dir, paste0("heat_change_13_", year, ".png"))
  
  # Save the plot as a PNG file
  ggsave(file_path, plot = hdd_plot, width = 8, height = 6)
}



