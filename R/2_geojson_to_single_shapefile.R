# This R script is used to read in all individual .geojson files
# and turn them into a single shapefile and .geojson and save that out as a single file
# and also calculate area for each urban greenspace

# packages
library(tidyverse)
library(sf)

sf_use_s2(FALSE)


# First thing is to read in all the files
# a quick function to do this
read_in_files <- function(file_name){
  
  file <- st_read(paste0("Data/mapped_wetlands_geojson/", file_name)) %>%
    mutate(LOCALITY_ID=gsub(".geojson", "", file_name))
  
}

files <- list.files("Data/mapped_wetlands_geojson/")

combined <- bind_rows(lapply(files, read_in_files))


target_file <- "Data/combined_shp_geojson/combined_polygons.shp"

# Check if the target file already exists
if (file.exists(target_file)) {
  file.remove(target_file)  # Delete the existing file
}

# This code runs the combined polygons into a shapefile
write_sf(combined, "Data/combined_shp_geojson/combined_polygons.shp")


target_file <- "Data/combined_shp_geojson/combined_polygons.geojson"

# Check if the target file already exists
if (file.exists(target_file)) {
  file.remove(target_file)  # Delete the existing file
}

# Now a geojson
write_sf(combined, "Data/combined_shp_geojson/combined_polygons.geojson")
