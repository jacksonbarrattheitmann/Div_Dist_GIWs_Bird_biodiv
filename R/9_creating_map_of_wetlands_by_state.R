# Creating Map of wetlands by state

library(readr)
library(auk)
library(sp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(basemaps)
library(raster)
library(ggspatial)
library(sf)
library(ggrepel)
library(gridExtra)
library(mapdata)
library(tidyverse)
library(tigris)

# read in the data
file_list <- list.files(path = "Data/eBird_local_alpha_level/", pattern = "\\.RDS", full.names = TRUE)
r <- lapply(file_list, readRDS)
s <- bind_rows(r, .id = "column_label")
s <- as.data.frame(s)

# adding the state aberviations excel sheet
state_abev <- read_csv("Data/earth_engine_env_data/abrev_state.csv") 

# filtering to unique LOCALITY_ID

wetlands <- s %>%
  dplyr::select(LOCALITY_ID, STATE_CODE) %>%
  dplyr::group_by(LOCALITY_ID) %>%
  unique()

# removing the US- from the state code
wetlands$STATE_CODE <- gsub("US-","", as.character(wetlands$STATE_CODE))

wetlands <- rename(wetlands, state = STATE_CODE)
# inner join to get the full name 

data_count <- inner_join(wetlands, state_abev, by = "state") %>%
  group_by(full_name) %>%
  summarise(count = n())

# get US map
usa <- map_data('state')
usa <- left_join(usa, data_count, by=c("region"="full_name"))

# map
ggplot() + geom_polygon(data=usa, aes(long, lat, group=group, fill=count), color="grey30") +
  
  scale_fill_gradient(  
    low = "lightblue",  # Specify the low color
    high = "darkblue",
    na.value = "#F5FFF5" # Specify the high color
  ) +
  theme(legend.position = "bottom", legend.title=element_text(hjust=0.5),
        axis.title = element_blank(), panel.background = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank())

ggsave("wetlands_by_State.jpeg", height=20, width=30, units="in", dpi = 300)



### Futile attempt to re-project map for Div and Dis

library(ggplot2)
library(dplyr)
library(readr)

# Assuming `usa` is a data frame with columns: long, lat, group, count
# Summing the 'count' by state (group)

# read in centroids of states
centroids <- read_csv("Data/state_centroids/centroids_us_states.csv")

# Calculate the centroids of each state (average longitudes and latitudes of each polygon)
state_centroids <- usa %>%
  group_by(region) %>%
  summarise(
    centroid_long = mean(long),  # Averaging longitudes for centroid
    centroid_lat = mean(lat)     # Averaging latitudes for centroid
  ) %>%
  left_join(data_count, by = c("region" = "full_name")) %>%  # Join with the total count data
  mutate(count=ifelse(is.na(count), 0, count))

state_centroids <- usa %>%
  dplyr::select(-long, -lat) %>%
  left_join(centroids, by = c("region" = "State")) %>%  # Join with the total count data
  group_by(region) %>%
  summarise(Latitude=first(Latitude), Longitude=first(Longitude), count=first(count), color=first(Color)) %>%
  filter(region!="district of columbia") %>%
  mutate(count=ifelse(is.na(count), 0, count))

# Plot the map with the total 'count' labels at the centroids
ggplot() + 
  # Plotting the polygons (states)
  geom_polygon(data = usa, aes(x = long, y = lat, group = group, fill = count), color = "grey30") + 
  
  # Adding the text labels with the total count at the centroids
  geom_text(data = state_centroids, 
            aes(x = Longitude, y = Latitude, label = count, color = color), 
            size = 10, fontface = "bold", hjust = 0.75, vjust = 0.5) + 
  scale_color_identity() +
  
  # Color scale for the fill
  scale_fill_gradient(
    low = "lightblue",  # Specify the low color
    high = "darkblue",  # Specify the high color
    na.value = "#F5FFF5" # Color for missing data
  ) +
  
  # Customize theme
  theme(
    legend.position = "bottom", 
    legend.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) + 
  # Optionally set the coordinate limits if you want a specific region
  coord_cartesian(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE)

ggplot() + 
  # Plotting the polygons (states)
  geom_polygon(data = usa, aes(x = long, y = lat, group = group, fill = count), color = "grey30") + 
  
  # Adding the text labels with the total count at the centroids
  geom_text(data = state_centroids, 
            aes(x = Longitude, y = Latitude, label = count, color = color), 
            size = 13, fontface = "bold", hjust = 0.75, vjust = 0.5) + 
  scale_color_identity() +
  
  # Color scale for the fill with modified legend title
  scale_fill_gradient(
    low = "lightblue",  # Specify the low color
    high = "darkblue",  # Specify the high color
    na.value = "#F5FFF5", # Color for missing data
    name = "Count"        # Set legend title to capital "Count"
  ) +
  
  # Customize theme
  theme(
    legend.position = "none", 
    legend.title = element_text(size = 50),  # Increase title size
    legend.text = element_text(size = 40),  # Increase text size for legend entries
    legend.key.size = unit(3, "cm"),  # Make the legend key (color gradient) larger
    legend.key.height = unit(1, "cm"),  # Adjust the height of the legend bar
    axis.title = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) + 
  # Optionally set the coordinate limits if you want a specific region
  coord_cartesian(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE)


ggsave("Figures/wetlands_by_state_w_labels.jpeg", height=20, width=35, units="in", dpi = 300)
