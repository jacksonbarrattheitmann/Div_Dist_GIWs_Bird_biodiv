#re-sampled richness analysis

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
library(scales)
library(broom.mixed)
library(arm)

# read in data
### Read in the env_matrix and the s_matrix to do some preliminary analysis

s_matrix <- readRDS("Data/species_matrix/s_matrix.RDS")

env_matrix <- readRDS("Data/earth_engine_env_data/env_matrix.RDS")

# Combine them to do analysis
wetland_bird_matrix <- s_matrix %>%
  full_join(env_matrix, by = "LOCALITY_ID")

# Histogram of resampled_richness values
histogram(wetland_bird_matrix$resampled_mean_richness)

# Quick plot of resampled richness and total richness
ggplot(wetland_bird_matrix, aes(x=total_richness, y=resampled_mean_richness)) +
  geom_point()+
  geom_smooth(method = 'lm') + theme_bw()

cor(wetland_bird_matrix$total_richness, wetland_bird_matrix$resampled_mean_richness)

#Figure of resampled richness and area
ggplot(wetland_bird_matrix, aes(x=area_sqkm, y=resampled_mean_richness))+
  geom_point()+
  theme_bw()+
  scale_x_log10()+
  geom_smooth(method = "lm") +
  xlab("log Area (km2)") +
  ylab("Mean Resampled Richness")


#### Checking Species-Area relationship for resampled richness
mod1_lm <- lm(resampled_mean_richness ~ log10(area_sqkm), data=wetland_bird_matrix)
summary(mod1_lm)

#data looks very normal, and model performed well
check_model(mod1_lm)

# could go for the Poisson family, but not necessary like we did for total_richness
#mod1_glm <- glm(resampled_mean_richness ~ log10(area_sqkm), family=poisson, data=wetland_bird_matrix)
#summary(mod1_glm)

#check_model(mod1_glm)


####### Landcover
####### Using the same random effect model we used for total richness

big_mod_lm_re <- lmer(resampled_mean_richness ~ rescale(built_wet) + rescale(built_25km) +
                          rescale(grass_wet) + rescale(grass_25km) + rescale(trees_wet) + rescale(trees_25km) +
                          rescale(water_wet) + rescale(water_25km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_25km)
                        + rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_25km) + rescale(shan_wet) + rescale(shan_gamma_25) + 
                          (1| NA_L1NAME), data=wetland_bird_matrix)

summary(big_mod_lm_re)
anova(big_mod_lm_re)

# Random effects: The term (1| NA_L1NAME) specifies that the model includes a random intercept for each level of NA_L1NAME. 
# This means that the model allows the baseline level of total_richness to vary by NA_L1NAME. In other words, 
# it is assuming that there's some grouping structure in your data specified by NA_L1NAME that 
# could cause observations within the same group to be more similar to each other than to observations in different groups. 
# This random intercept captures that variability.

# make a plot...
big_mod_summary_lm_re <- broom.mixed::tidy(big_mod_lm_re, conf.int=TRUE) %>%
  dplyr::filter(effect=="fixed") %>%
  mutate(predictor=c("Intercept", 
                     "Built local", "Built landscape",
                     "Grassland local", "Grassland landscape", 
                     "Trees local", "Trees landscape",
                     "Water local", "Water landscape",
                     "Flooded Veg local", "Flooded Veg landscape",
                     "Shrub local", "Shrub landscape",
                     "Heterogeneity local", "Heterogeneity landscape")) %>%
  mutate(Scale=c(NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape",
                 "Local", "Landscape", "Local", "Landscape", "Local", "Landscape"))

big_mod_summary_lm_re %>%
  dplyr::filter(! predictor %in% c("Intercept")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x =element_text(color="black"), 
        axis.text.y = element_text(color = c("black", "red", "black", "blue",
                                             "black", "black", "black", "black",
                                             "black", "black", "red", "black",
                                             "black")))+
  ylab("Effect size")+
  xlab("")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("Resampled Mean Richness")

check_model(big_mod_lm_re)
qqmath(big_mod_lm_re)
