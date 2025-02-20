
# packages
library(readr)
library(dplyr)
library(tidyverse)
library(data.table)
library(lattice)
library(vegan)
library(lme4)
library(performance)
library(easystats)
library(see)
library(ggplot2)
library(patchwork)
library(marginaleffects)
library(margins)
library(GGally)
library(patchwork)
library(ggpubr)
library(arm)
library(broom.mixed)


env_matrix <- readRDS("Data/earth_engine_env_data/env_matrix.RDS")

# Now all the functional tables
ins <- readRDS("Data/species_matrix/insessorial_matrix.RDS")

aqu <- readRDS("Data/species_matrix/aquatic_matrix.RDS")

gen <- readRDS("Data/species_matrix/generalist_matrix.RDS")

aer <- readRDS("Data/species_matrix/aerial_matrix.RDS")

ter <- readRDS("Data/species_matrix/terrestrial_matrix.RDS")


#### Now breaking them back into each of their separate lifestyle groups

# Generalist
Generalist <- gen %>%
  left_join(env_matrix, by = "LOCALITY_ID")

# Insessorial
Insessorial <- ins %>%
  left_join(env_matrix, by = "LOCALITY_ID")

# Aquatic
Aquatic<- aqu %>%
  left_join(env_matrix, by = "LOCALITY_ID")

# Aerial
Aerial <- aer %>%
  left_join(env_matrix, by = "LOCALITY_ID")

# Terrestrial
Terrestrial <-ter %>%
  left_join(env_matrix, by = "LOCALITY_ID")




# Function to run the model and plot results for 25, 10, 5km buffers 
run_land_cover_models <- function(data) {
  # Get the dataset name as a string
  data_name <- deparse(substitute(data))
  
  # Function for 10km model
  model_10km <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_10km) +
                        rescale(grass_wet) + rescale(grass_10km) + rescale(trees_wet) + rescale(trees_10km) +
                        rescale(water_wet) + rescale(water_10km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_10km) +
                        rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_10km) + rescale(shan_wet) +
                        rescale(shan_gamma_10) + (1| NA_L1NAME), family = poisson, data = data)
  
  summary_10km <- summary(model_10km)
  check_model(model_10km)
  
  # Get summary and plot for 10km
  summary_10km_df <- broom.mixed::tidy(model_10km, conf.int = TRUE) %>%
    dplyr::filter(effect == "fixed") %>%
    mutate(predictor = c("Intercept", "Total checklists", "Built local", "Built 10km",
                         "Grassland local", "Grassland 10km", "Trees local", "Trees 10km", "Water local", "Water 10km",
                         "Flooded Veg local", "Flooded Veg 10km", "Shrub local", "Shrub 10km", "Heterogeneity local", "Heterogeneity 10km")) %>%
    mutate(Scale = c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", 
                     "Local", "Landscape", "Local", "Landscape", "Local", "Landscape")) %>%
    mutate(model = paste("Model 10km -", data_name))
  
  plot_10km <- summary_10km_df %>%
    dplyr::filter(!predictor %in% c("Intercept", "Total checklists")) %>%
    ggplot(aes(x = predictor, y = estimate, color = Scale)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
    coord_flip() +
    theme_bw() +
    theme(axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = c("black", "black", "black", "black", "black", "black", "black", "black", 
                                               "black", "black", "black", "black", "black", "black"))) +
    ylab("Effect size") +
    xlab("") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    scale_color_brewer(palette = "Dark2") +
    ggtitle(paste(data_name, "10km buffer"))
  
  
  # Function for 25km model
  model_25km <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_25km) +
                        rescale(grass_wet) + rescale(grass_25km) + rescale(trees_wet) + rescale(trees_25km) +
                        rescale(water_wet) + rescale(water_25km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_25km) +
                        rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_25km) + rescale(shan_wet) +
                        rescale(shan_gamma_25) + (1| NA_L1NAME), family = poisson, data = data)
  
  summary_25km <- summary(model_25km)
  check_model(model_25km)
  
  # Get summary and plot for 25km
  summary_25km_df <- broom.mixed::tidy(model_25km, conf.int = TRUE) %>%
    dplyr::filter(effect == "fixed") %>%
    mutate(predictor = c("Intercept", "Total checklists", "Built local", "Built 25km",
                         "Grassland local", "Grassland 25km", "Trees local", "Trees 25km", "Water local", "Water 25km",
                         "Flooded Veg local", "Flooded Veg 25km", "Shrub local", "Shrub 25km", "Heterogeneity local", "Heterogeneity 25km")) %>%
    mutate(Scale = c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", 
                     "Local", "Landscape", "Local", "Landscape", "Local", "Landscape")) %>%
    mutate(model = paste("Model 25km -", data_name))
  
  plot_25km <- summary_25km_df %>%
    dplyr::filter(!predictor %in% c("Intercept", "Total checklists")) %>%
    ggplot(aes(x = predictor, y = estimate, color = Scale)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
    coord_flip() +
    theme_bw() +
    theme(axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = c("black", "black", "black", "black", "black", "black", "black", "black", 
                                               "black", "black", "black", "black", "black", "black"))) +
    ylab("Effect size") +
    xlab("") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    scale_color_brewer(palette = "Dark2") +
    ggtitle(paste(data_name, "25km buffer"))
  
  
  # Function for 5km model
  model_5km <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_5km) +
                       rescale(grass_wet) + rescale(grass_5km) + rescale(trees_wet) + rescale(trees_5km) +
                       rescale(water_wet) + rescale(water_5km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_5km) +
                       rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_5km) + rescale(shan_wet) +
                       rescale(shan_gamma_5) + (1| NA_L1NAME), family = poisson, data = data)
  
  summary_5km <- summary(model_5km)
  check_model(model_5km)
  
  # Get summary and plot for 5km
  summary_5km_df <- broom.mixed::tidy(model_5km, conf.int = TRUE) %>%
    dplyr::filter(effect == "fixed") %>%
    mutate(predictor = c("Intercept", "Total checklists", "Built local", "Built 5km",
                         "Grassland local", "Grassland 5km", "Trees local", "Trees 5km", "Water local", "Water 5km",
                         "Flooded Veg local", "Flooded Veg 5km", "Shrub local", "Shrub 5km", "Heterogeneity local", "Heterogeneity 5km")) %>%
    mutate(Scale = c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", 
                     "Local", "Landscape", "Local", "Landscape", "Local", "Landscape")) %>%
    mutate(model = paste("Model 5km -", data_name))
  
  plot_5km <- summary_5km_df %>%
    dplyr::filter(!predictor %in% c("Intercept", "Total checklists")) %>%
    ggplot(aes(x = predictor, y = estimate, color = Scale)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
    coord_flip() +
    theme_bw() +
    theme(axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = c("black", "black", "black", "black", "black", "black", "black", "black", 
                                               "black", "black", "black", "black", "black", "black"))) +
    ylab("Effect size") +
    xlab("") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    scale_color_brewer(palette = "Dark2") +
    ggtitle(paste(data_name, "5km buffer"))
  
  ggarrange(plot_25km, plot_10km, plot_5km,
            labels = c("A", "B", "C"),
            ncol = 2, nrow = 2, common.legend = TRUE)
}

run_land_cover_models(Generalist)
run_land_cover_models(Aquatic)
run_land_cover_models(Insessorial)
run_land_cover_models(Terrestrial)
run_land_cover_models(Aerial)

